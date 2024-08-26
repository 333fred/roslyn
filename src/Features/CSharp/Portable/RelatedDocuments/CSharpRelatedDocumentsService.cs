﻿// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.
// See the LICENSE file in the project root for more information.

using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Composition;
using System.Linq;
using System.Linq.Expressions;
using System.Threading;
using System.Threading.Tasks;
using Microsoft.CodeAnalysis.CSharp.Extensions;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using Microsoft.CodeAnalysis.Host.Mef;
using Microsoft.CodeAnalysis.PooledObjects;
using Microsoft.CodeAnalysis.RelatedDocuments;
using Microsoft.CodeAnalysis.Shared.Extensions;
using Microsoft.CodeAnalysis.Shared.Utilities;
using Roslyn.Utilities;

namespace Microsoft.CodeAnalysis.CSharp.RelatedDocuments;

[ExportLanguageService(typeof(IRelatedDocumentsService), LanguageNames.CSharp), Shared]
[method: ImportingConstructor]
[method: Obsolete(MefConstruction.ImportingConstructorMessage, error: true)]
internal sealed class CSharpRelatedDocumentsService() : AbstractRelatedDocumentsService
{
    protected override async ValueTask GetRelatedDocumentIdsInCurrentProcessAsync(
        Document document,
        int position,
        Func<ImmutableArray<DocumentId>, CancellationToken, ValueTask> callbackAsync,
        CancellationToken cancellationToken)
    {
        var solution = document.Project.Solution;

        // Don't need nullable analysis, and we're going to walk a lot of the tree, so speed things up by not doing
        // excess semantic work.
        var semanticModel = await document.GetRequiredNullableDisabledSemanticModelAsync(cancellationToken).ConfigureAwait(false);
        var root = await document.GetRequiredSyntaxRootAsync(cancellationToken).ConfigureAwait(false);

        var seenDocumentIds = new ConcurrentSet<DocumentId>();
        var seenTypeNames = new ConcurrentSet<string>();

        // Bind as much as we can in parallel to get the results as quickly as possible.  We call into the
        // ProducerConsumer overload that will batch up values into arrays, and call into the callback with them.  While
        // that callback is executing, the ProducerConsumer continues to run, computing more results and getting them
        // ready for when that call returns.  This approach ensures that we're not pausing while we're reporting the
        // results to whatever client is calling into us.
        await ProducerConsumer<DocumentId>.RunParallelAsync(
            // Order the nodes by the distance from the requested position.
            IteratePotentialTypeNodes().OrderBy(t => t.expression.SpanStart - position),
            produceItems: static (tuple, callback, args, cancellationToken) =>
            {
                var (solution, semanticModel, seenDocumentIds, seenTypeNames, callbackAsync) = args;
                ProductItems(
                    solution, semanticModel, tuple.expression, tuple.nameToken, callback, seenDocumentIds, seenTypeNames, cancellationToken);

                return Task.CompletedTask;
            },
            consumeItems: static async (array, args, cancellationToken) =>
            {
                var (solution, semanticModel, seenDocumentIds, seenTypeNames, callbackAsync) = args;
                await callbackAsync(array, cancellationToken).ConfigureAwait(false);
            },
            args: (solution, semanticModel, seenDocumentIds, seenTypeNames, callbackAsync),
            cancellationToken).ConfigureAwait(false);

        return;

        IEnumerable<(ExpressionSyntax expression, SyntaxToken nameToken)> IteratePotentialTypeNodes()
        {
            using var _ = ArrayBuilder<SyntaxNode>.GetInstance(out var stack);
            stack.Push(root);

            while (stack.TryPop(out var current))
            {
                if (current is MemberAccessExpressionSyntax memberAccess)
                {
                    // Could be a static member access off of a type name.  Check the left side, and if it's just a
                    // dotted name, return that.

                    if (IsPossibleTypeName(memberAccess.Expression, out var nameToken))
                    {
                        // Something like `X.Y.Z` where `X.Y` is a type name.  Bind X.Y
                        yield return (memberAccess.Expression, nameToken);
                    }
                    else
                    {
                        // Something like `(...).Y`.  Recurse down the left side of the member access to see if there
                        // are types in there. We don't want to recurse down the name portion as it will never be a
                        // type.
                        stack.Push(memberAccess.Expression);
                    }

                    continue;
                }
                else if (current is NameSyntax name)
                {
                    yield return (name, name.GetNameToken());

                    // Intentionally continue to recurse down the name so that if we have things like `X<Y>` we'll bind
                    // the inner `Y` as well.
                }

                // Don't need to recurse in order as our caller is ordering results anyways.
                foreach (var child in current.ChildNodesAndTokens())
                {
                    if (child.AsNode(out var childNode))
                        stack.Push(childNode);
                }
            }
        }

        static bool IsPossibleTypeName(ExpressionSyntax expression, out SyntaxToken nameToken)
        {
            while (expression is MemberAccessExpressionSyntax memberAccessExpression)
                expression = memberAccessExpression.Expression;

            if (expression is not NameSyntax name)
            {
                nameToken = default;
                return false;
            }

            nameToken = name.GetNameToken();
            return true;
        }

        static void ProductItems(
            Solution solution,
            SemanticModel semanticModel,
            ExpressionSyntax expression,
            SyntaxToken nameToken,
            Action<DocumentId> callback,
            ConcurrentSet<DocumentId> seenDocumentIds,
            ConcurrentSet<string> seenTypeNames,
            CancellationToken cancellationToken)
        {
            if (nameToken.Kind() != SyntaxKind.IdentifierName)
                return;

            if (nameToken.ValueText == "")
                return;
            // Don't rebind a type name we've already seen.  Note: this is a conservative/inaccurate check.
            // Specifically, there could be different types with the same last name portion (from different
            // namespaces). In that case, we'll miss the one that is further away.  We can revisit this in the
            // future if we think it's necessary.
            if (!seenTypeNames.Add(nameToken.ValueText))
                return;

            var symbol = semanticModel.GetSymbolInfo(expression, cancellationToken).GetAnySymbol();
            if (symbol is not ITypeSymbol)
                return;

            foreach (var syntaxReference in symbol.DeclaringSyntaxReferences)
            {
                var documentId = solution.GetDocument(syntaxReference.SyntaxTree)?.Id;
                if (documentId != null && seenDocumentIds.Add(documentId))
                    callback(documentId);
            }
        }
    }
}
