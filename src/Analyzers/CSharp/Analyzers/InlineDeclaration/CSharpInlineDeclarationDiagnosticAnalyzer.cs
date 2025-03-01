﻿// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.
// See the LICENSE file in the project root for more information.

using System.Collections.Immutable;
using System.Linq;
using System.Linq.Expressions;
using System.Threading;
using Microsoft.CodeAnalysis.CodeStyle;
using Microsoft.CodeAnalysis.CSharp.CodeStyle;
using Microsoft.CodeAnalysis.CSharp.Extensions;
using Microsoft.CodeAnalysis.CSharp.LanguageService;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using Microsoft.CodeAnalysis.Diagnostics;
using Microsoft.CodeAnalysis.Shared.Extensions;
using Roslyn.Utilities;

namespace Microsoft.CodeAnalysis.CSharp.InlineDeclaration;

/// <summary>
/// Looks for code of the form:
/// 
///     int i;
///     if (int.TryParse(s, out i)) { }
///     
/// And offers to convert it to:
/// 
///     if (int.TryParse(s, out var i)) { }   or
///     if (int.TryParse(s, out int i)) { }   or
/// 
/// </summary>
[DiagnosticAnalyzer(LanguageNames.CSharp)]
internal sealed class CSharpInlineDeclarationDiagnosticAnalyzer()
    : AbstractBuiltInCodeStyleDiagnosticAnalyzer(IDEDiagnosticIds.InlineDeclarationDiagnosticId,
        EnforceOnBuildValues.InlineDeclaration,
        CSharpCodeStyleOptions.PreferInlinedVariableDeclaration,
        new LocalizableResourceString(nameof(CSharpAnalyzersResources.Inline_variable_declaration), CSharpAnalyzersResources.ResourceManager, typeof(CSharpAnalyzersResources)),
        new LocalizableResourceString(nameof(CSharpAnalyzersResources.Variable_declaration_can_be_inlined), CSharpAnalyzersResources.ResourceManager, typeof(CSharpAnalyzersResources)))
{
    private const string CS0165 = nameof(CS0165); // Use of unassigned local variable 's'

    public override DiagnosticAnalyzerCategory GetAnalyzerCategory()
        => DiagnosticAnalyzerCategory.SemanticSpanAnalysis;

    protected override void InitializeWorker(AnalysisContext context)
    {
        context.RegisterCompilationStartAction(compilationContext =>
        {
            var compilation = compilationContext.Compilation;
            var expressionType = compilation.GetTypeByMetadataName(typeof(Expression<>).FullName!);

            // We wrap the SyntaxNodeAction within a CodeBlockStartAction, which allows us to
            // get callbacks for Argument nodes, but analyze nodes across the entire code block
            // and eventually report a diagnostic on the local declaration node.
            // Without the containing CodeBlockStartAction, our reported diagnostic would be classified
            // as a non-local diagnostic and would not participate in lightbulb for computing code fixes.
            compilationContext.RegisterCodeBlockStartAction<SyntaxKind>(blockStartContext =>
                blockStartContext.RegisterSyntaxNodeAction(
                    syntaxContext => AnalyzeSyntaxNode(syntaxContext, expressionType), SyntaxKind.Argument));
        });
    }

    private void AnalyzeSyntaxNode(SyntaxNodeAnalysisContext context, INamedTypeSymbol? expressionType)
    {
        var syntaxTree = context.Node.SyntaxTree;
        var csOptions = (CSharpParseOptions)syntaxTree.Options;
        if (csOptions.LanguageVersion < LanguageVersion.CSharp7)
        {
            // out-vars are not supported prior to C# 7.0.
            return;
        }

        var option = context.GetCSharpAnalyzerOptions().PreferInlinedVariableDeclaration;
        if (!option.Value || ShouldSkipAnalysis(context, option.Notification))
        {
            // Don't bother doing any work if the user doesn't even have this preference set.
            return;
        }

        var argumentNode = (ArgumentSyntax)context.Node;
        if (argumentNode.RefOrOutKeyword.Kind() != SyntaxKind.OutKeyword)
        {
            // Immediately bail if this is not an out-argument.  If it's not an out-argument
            // we clearly can't convert it to be an out-variable-declaration.
            return;
        }

        var argumentExpression = argumentNode.Expression;
        if (argumentExpression is not IdentifierNameSyntax identifierName)
        {
            // has to be exactly the form "out i".  i.e. "out this.i" or "out v[i]" are legal
            // cases for out-arguments, but could not be converted to an out-variable-declaration.
            return;
        }

        if (argumentNode.Parent is not ArgumentListSyntax argumentList)
        {
            return;
        }

        var invocationOrCreation = argumentList.Parent;
        if (invocationOrCreation?.Kind() is not SyntaxKind.InvocationExpression and not SyntaxKind.ObjectCreationExpression)
        {
            // Out-variables are only legal with invocations and object creations.
            // If we don't have one of those bail.  Note: we need hte parent to be
            // one of these forms so we can accurately verify that inlining the 
            // variable doesn't change semantics.
            return;
        }

        // Don't offer to inline variables named "_".  It can cause is to create a discard symbol
        // which would cause a break.
        if (identifierName.Identifier.ValueText == "_")
        {
            return;
        }

        var containingStatement = argumentExpression.FirstAncestorOrSelf<StatementSyntax>();
        if (containingStatement == null)
        {
            return;
        }

        var cancellationToken = context.CancellationToken;

        var semanticModel = context.SemanticModel;
        if (semanticModel.GetSymbolInfo(argumentExpression, cancellationToken).Symbol is not ILocalSymbol outLocalSymbol)
        {
            // The out-argument wasn't referencing a local.  So we don't have an local
            // declaration that we can attempt to inline here.
            return;
        }

        // Ensure that the local-symbol actually belongs to LocalDeclarationStatement.
        // Trying to do things like inline a var-decl in a for-statement is just too 
        // esoteric and would make us have to write a lot more complex code to support
        // that scenario.
        var localReference = outLocalSymbol.DeclaringSyntaxReferences.FirstOrDefault();
        if (localReference?.GetSyntax(cancellationToken) is not VariableDeclaratorSyntax localDeclarator)
        {
            return;
        }

        var localDeclaration = localDeclarator.Parent as VariableDeclarationSyntax;
        if (localDeclaration?.Parent is not LocalDeclarationStatementSyntax localStatement)
        {
            return;
        }

        // Bail out early if the localDeclaration is outside the context's analysis span.
        if (!context.ShouldAnalyzeSpan(localDeclaration.Span))
            return;

        if (localDeclarator.SpanStart >= argumentNode.SpanStart)
        {
            // We have an error situation where the local was declared after the out-var.  
            // Don't even bother offering anything here.
            return;
        }

        // If the local has an initializer, only allow the refactoring if it is initialized
        // with a simple literal or 'default' expression.  i.e. it's ok to inline "var v = 0"
        // since there are no side-effects of the initialization.  However something like
        // "var v = M()" should not be inlined as that could break program semantics.
        if (localDeclarator.Initializer != null)
        {
            if (localDeclarator.Initializer.Value is not LiteralExpressionSyntax and
                not DefaultExpressionSyntax)
            {
                return;
            }
        }

        // Get the block that the local is scoped inside of.  We'll search that block
        // for references to the local to make sure that no reads/writes happen before
        // the out-argument.  If there are any reads/writes we can't inline as those
        // accesses will become invalid.
        var enclosingBlockOfLocalStatement = CSharpBlockFacts.Instance.GetImmediateParentExecutableBlockForStatement(localStatement);
        if (enclosingBlockOfLocalStatement is null)
            return;

        if (argumentExpression.IsInExpressionTree(semanticModel, expressionType, cancellationToken))
        {
            // out-vars are not allowed inside expression-trees.  So don't offer to
            // fix if we're inside one.
            return;
        }

        // Find the scope that the out-declaration variable will live in after we
        // rewrite things.
        var outArgumentScope = GetOutArgumentScope(argumentExpression);
        if (outArgumentScope == null)
            return;

        if (!outLocalSymbol.CanSafelyMoveLocalToBlock(enclosingBlockOfLocalStatement, outArgumentScope))
        {
            return;
        }

        // Make sure that variable is not accessed outside of that scope.
        var dataFlow = semanticModel.AnalyzeDataFlow(outArgumentScope);
        if (dataFlow.ReadOutside.Contains(outLocalSymbol) || dataFlow.WrittenOutside.Contains(outLocalSymbol))
        {
            // The variable is read or written from outside the block that the new variable
            // would be scoped in.  This would cause a break.
            //
            // Note(cyrusn): We could still offer the refactoring, but just show an error in the
            // preview in this case.
            return;
        }

        // Make sure the variable isn't ever accessed before the usage in this out-var.
        if (IsAccessed(semanticModel, outLocalSymbol, enclosingBlockOfLocalStatement,
                       localStatement, argumentNode, cancellationToken))
        {
            return;
        }

        // See if inlining this variable would make it so that some variables were no
        // longer definitely assigned.
        if (WouldCauseDefiniteAssignmentErrors(
                semanticModel, localStatement, enclosingBlockOfLocalStatement, outLocalSymbol))
        {
            return;
        }

        // Collect some useful nodes for the fix provider to use so it doesn't have to
        // find them again.
        var allLocations = ImmutableArray.Create(
            localDeclarator.GetLocation(),
            identifierName.GetLocation(),
            invocationOrCreation.GetLocation());

        // If the local variable only has one declarator, then report the suggestion on the whole
        // declaration.  Otherwise, return the suggestion only on the single declarator.
        var reportNode = localDeclaration.Variables.Count == 1
            ? (SyntaxNode)localDeclaration
            : localDeclarator;

        context.ReportDiagnostic(DiagnosticHelper.Create(
            Descriptor,
            reportNode.GetLocation(),
            option.Notification,
            context.Options,
            additionalLocations: allLocations,
            properties: null));
    }

    private static bool WouldCauseDefiniteAssignmentErrors(
        SemanticModel semanticModel,
        LocalDeclarationStatementSyntax localStatement,
        SyntaxNode enclosingBlock,
        ILocalSymbol outLocalSymbol)
    {
        // See if we have something like:
        //
        //      int i = 0;
        //      if (Goo() || Bar(out i))
        //      {
        //          Console.WriteLine(i);
        //      }
        //
        // In this case, inlining the 'i' would cause it to longer be definitely
        // assigned in the WriteLine invocation.
        var nextStatement = localStatement.GetNextStatement();
        Contract.ThrowIfNull(nextStatement);

        var dataFlow = semanticModel.AnalyzeDataFlow(
            nextStatement,
            CSharpBlockFacts.Instance.GetExecutableBlockStatements(enclosingBlock).Last());
        Contract.ThrowIfNull(dataFlow);
        return dataFlow.DataFlowsIn.Contains(outLocalSymbol);
    }

    private static SyntaxNode? GetOutArgumentScope(SyntaxNode argumentExpression)
    {
        for (var current = argumentExpression; current != null; current = current.Parent)
        {
            // We were in a lambda.  The lambda body will be the new scope of the out var.
            if (current.Parent is LambdaExpressionSyntax lambda && current == lambda.Body)
                return current;

            // The arm of a switch expression is its own isolated scope.
            if (current.Parent is SwitchExpressionArmSyntax switchArm && current == switchArm.Expression)
                return current;

            // Any loop construct defines a scope for out-variables, as well as each of the following:
            // * Using statements
            // * Fixed statements
            // * Try statements (specifically for exception filters)
            switch (current.Kind())
            {
                case SyntaxKind.WhileStatement:
                case SyntaxKind.DoStatement:
                case SyntaxKind.ForStatement:
                case SyntaxKind.ForEachStatement:
                case SyntaxKind.UsingStatement:
                case SyntaxKind.FixedStatement:
                case SyntaxKind.TryStatement:
                    return current;
            }

            if (current is StatementSyntax)
            {
                // We hit a statement containing the out-argument.  Statements can have one of 
                // two forms.  They're either parented by a block, or by another statement 
                // (i.e. they're an embedded statement).  If we're parented by a block, then
                // that block will be the scope of the new out-var.
                //
                // However, if our containing statement is not parented by a block, then that
                // means we have something like:
                //
                //      if (x)
                //          if (Try(out y))
                //
                // In this case, there is a 'virtual' block scope surrounding the embedded 'if'
                // statement, and that will be the scope the out-var goes into.
                return current.IsParentKind(SyntaxKind.Block)
                    ? current.Parent
                    : current;
            }
        }

        return null;
    }

    private static bool IsAccessed(
        SemanticModel semanticModel,
        ISymbol outSymbol,
        SyntaxNode enclosingBlockOfLocalStatement,
        LocalDeclarationStatementSyntax localStatement,
        ArgumentSyntax argumentNode,
        CancellationToken cancellationToken)
    {
        var localStatementStart = localStatement.Span.Start;
        var argumentNodeStart = argumentNode.Span.Start;
        var variableName = outSymbol.Name;

        // Walk the block that the local is declared in looking for accesses.
        // We can ignore anything prior to the actual local declaration point,
        // and we only need to check up until we reach the out-argument.
        foreach (var descendentNode in enclosingBlockOfLocalStatement.DescendantNodes())
        {
            var descendentStart = descendentNode.Span.Start;
            if (descendentStart <= localStatementStart)
            {
                // This node is before the local declaration.  Can ignore it entirely as it could
                // not be an access to the local.
                continue;
            }

            if (descendentStart >= argumentNodeStart)
            {
                // We reached the out-var.  We can stop searching entirely.
                break;
            }

            if (descendentNode is IdentifierNameSyntax identifierName)
            {
                // See if this looks like an accessor to the local variable syntactically.
                if (identifierName.Identifier.ValueText == variableName)
                {
                    // Confirm that it is a access of the local.
                    var symbol = semanticModel.GetSymbolInfo(identifierName, cancellationToken).Symbol;
                    if (outSymbol.Equals(symbol))
                    {
                        // We definitely accessed the local before the out-argument.  We 
                        // can't inline this local.
                        return true;
                    }
                }
            }
        }

        // No accesses detected
        return false;
    }
}
