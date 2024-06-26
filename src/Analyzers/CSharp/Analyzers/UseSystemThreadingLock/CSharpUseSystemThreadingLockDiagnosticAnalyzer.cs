﻿// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.
// See the LICENSE file in the project root for more information.

using System.Linq;
using Microsoft.CodeAnalysis.CodeStyle;
using Microsoft.CodeAnalysis.CSharp.CodeStyle;
using Microsoft.CodeAnalysis.CSharp.Extensions;
using Microsoft.CodeAnalysis.CSharp.Shared.Extensions;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using Microsoft.CodeAnalysis.Diagnostics;
using Microsoft.CodeAnalysis.Operations;
using Microsoft.CodeAnalysis.Shared.Collections;
using Roslyn.Utilities;

namespace Microsoft.CodeAnalysis.CSharp.UseSystemThreadingLock;

/// <summary>
/// Looks for code of the form:
/// 
///     object _gate = new object();
///     ...
///     lock (_gate)
///     {
///     }
///     
/// and converts it to:
/// 
///     Lock _gate = new Lock();
/// </summary>
[DiagnosticAnalyzer(LanguageNames.CSharp)]
internal class CSharpUseSystemThreadingLockDiagnosticAnalyzer : AbstractBuiltInCodeStyleDiagnosticAnalyzer
{
    public CSharpUseSystemThreadingLockDiagnosticAnalyzer()
        : base(IDEDiagnosticIds.UseSystemThreadingLockDiagnosticId,
               EnforceOnBuildValues.UseSystemThreadingLock,
               CSharpCodeStyleOptions.PreferSystemThreadingLock,
               new LocalizableResourceString(
                   nameof(CSharpAnalyzersResources.Use_System_Threading_Lock), CSharpAnalyzersResources.ResourceManager, typeof(CSharpAnalyzersResources)))
    {
    }

    public override DiagnosticAnalyzerCategory GetAnalyzerCategory()
        => DiagnosticAnalyzerCategory.SemanticSpanAnalysis;

    protected override void InitializeWorker(AnalysisContext context)
    {
        context.RegisterCompilationStartAction(compilationContext =>
        {
            var compilation = compilationContext.Compilation;

            // The new 'Lock' feature is only supported in C# 13 and above, and only if we actually have a definition of
            // System.Threading.Lock available.
            if (!compilation.LanguageVersion().IsCSharp13OrAbove())
                return;

            var lockType = compilation.GetTypeByMetadataName("System.Threading.Lock");
            if (lockType is null)
                return;

            context.RegisterSymbolStartAction(AnalyzeNamedType, SymbolKind.NamedType);
        });
    }

    private void AnalyzeNamedType(SymbolStartAnalysisContext context)
    {
        var cancellationToken = context.CancellationToken;
        var namedType = (INamedTypeSymbol)context.Symbol;
        if (namedType is not
            {
                TypeKind: TypeKind.Class or TypeKind.Struct,
                DeclaringSyntaxReferences: [var reference, ..]
            })
        {
            return;
        }

        var syntaxTree = reference.GetSyntax(cancellationToken).SyntaxTree;
        var option = context.GetCSharpAnalyzerOptions(syntaxTree).PreferSystemThreadingLock;

        // Bail immediately if the user has disabled this feature.
        if (!option.Value || ShouldSkipAnalysis(syntaxTree, context.Options, context.Compilation.Options, option.Notification, cancellationToken))
            return;

        // Needs to have a private field that is exactly typed as 'object'.  This way we can analyze all usages of it to
        // be sure it's completely safe to move to the new lock type.
        using var fieldsArray = TemporaryArray<IFieldSymbol>.Empty;

        foreach (var member in namedType.GetMembers())
        {
            if (member is not IFieldSymbol
                {
                    Type.SpecialType: SpecialType.System_Object,
                    DeclaredAccessibility: Accessibility.Private,
                    DeclaringSyntaxReferences: [var fieldSyntaxReference],
                } field)
            {
                continue;
            }

            if (fieldSyntaxReference.GetSyntax(cancellationToken) is not VariableDeclaratorSyntax fieldSyntax)
                continue;

            // For simplicity, only offer this for fields with a single declarator.
            if (fieldSyntax.Parent is not VariableDeclarationSyntax { Parent: FieldDeclarationSyntax, Variables.Count: 1 })
                return;

            // If we have an initializer at the declaration site, it needs to be initialized with either `new object()`
            // or `new()`.
            if (fieldSyntax.Initializer != null && !IsSystemObjectCreationExpression(fieldSyntax.Initializer.Value))
                continue;

            // Looks like something that could be converted to a System.Threading.Lock if we see that this field is used
            // in a compatible fashion.
            fieldsArray.Add(field);
        }

        if (fieldsArray.Count == 0)
            return;

        var potentialLockFields = new ConcurrentSet<IFieldSymbol>();
        var wasLockedSet = new ConcurrentSet<IFieldSymbol>();
        foreach (var field in fieldsArray)
            potentialLockFields.Add(field);

        context.RegisterOperationAction(context =>
        {
            var cancellationToken = context.CancellationToken;
            var fieldReferenceOperation = (IFieldReferenceOperation)context.Operation;
            var fieldReference = fieldReferenceOperation.Field.OriginalDefinition;

            if (!potentialLockFields.Contains(fieldReference))
                return;

            if (fieldReferenceOperation.Parent is ILockOperation lockOperation)
            {
                // Locking on the the new lock type disallows yielding inside the lock.  So if we see that, immediately
                // consider this not applicable.
                if (lockOperation.Syntax.DescendantNodesAndSelf().Any(n => n is YieldStatementSyntax))
                {
                    potentialLockFields.Remove(fieldReference);
                    return;
                }

                // We did lock on this field, mark as such as its now something we'd def like to convert to a Lock if possible.
                wasLockedSet.Add(fieldReference);
                return;
            }

            // it's ok to assign to the field, as long as we're assigning a new lock object to it.
            if (fieldReferenceOperation.Parent is IAssignmentOperation { Syntax: AssignmentExpressionSyntax assignmentSyntax } assignment &&
                assignment.Target == fieldReferenceOperation &&
                IsSystemObjectCreationExpression(assignmentSyntax.Right))
            {
                var operand = assignment.Value is IConversionOperation { Conversion: { Exists: true, IsImplicit: true } } conversion
                    ? conversion.Operand
                    : assignment.Value;

                if (operand is IObjectCreationOperation { Arguments.Length: 0, Constructor.ContainingType.SpecialType: SpecialType.System_Object })
                    return;
            }

            // Fine to use `nameof(someLock)` as that's not actually using the lock.
            if (fieldReferenceOperation.Parent is INameOfOperation)
                return;

            // Add more supported patterns here as needed.

            // This wasn't a supported case.
            potentialLockFields.Remove(fieldReference);
        }, OperationKind.FieldReference);

        context.RegisterSymbolEndAction(context =>
        {
            var cancellationToken = context.CancellationToken;

            foreach (var lockField in potentialLockFields)
            {
                // Has to at least see this field locked on to offer to convert it to a Lock.
                if (!wasLockedSet.Contains(lockField))
                    continue;

                // .Single is safe here as we confirmed there was only one DeclaringSyntaxReference in the field at the beginning of analysis.
                var declarator = (VariableDeclaratorSyntax)lockField.DeclaringSyntaxReferences.Single().GetSyntax(cancellationToken);

                context.ReportDiagnostic(DiagnosticHelper.Create(
                    Descriptor,
                    declarator.Identifier.GetLocation(),
                    option.Notification,
                    context.Options,
                    additionalLocations: null,
                    properties: null));
            }
        });
    }

    private static bool IsSystemObjectCreationExpression(ExpressionSyntax expression)
    {
        return expression
            is ImplicitObjectCreationExpressionSyntax { ArgumentList.Arguments.Count: 0 }
            or ObjectCreationExpressionSyntax { ArgumentList.Arguments.Count: 0, Type: PredefinedTypeSyntax { Keyword.RawKind: (int)SyntaxKind.ObjectKeyword } };
    }
}
