﻿// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.
// See the LICENSE file in the project root for more information.

using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;
using System.Runtime.CompilerServices;
using System.Threading;
using System.Threading.Tasks;
using Microsoft.CodeAnalysis.CodeActions;
using Microsoft.CodeAnalysis.CodeFixes;
using Microsoft.CodeAnalysis.ImplementType;
using Microsoft.CodeAnalysis.PooledObjects;
using Microsoft.CodeAnalysis.Shared.Extensions;
using static Microsoft.CodeAnalysis.ImplementInterface.AbstractImplementInterfaceService;

namespace Microsoft.CodeAnalysis.ImplementInterface;

using static ImplementHelpers;

internal abstract class AbstractImplementInterfaceCodeFixProvider<TTypeSyntax> : CodeFixProvider
    where TTypeSyntax : SyntaxNode
{
    protected abstract bool IsTypeInInterfaceBaseList(TTypeSyntax type);

    public sealed override FixAllProvider GetFixAllProvider()
        => WellKnownFixAllProviders.BatchFixer;

    public sealed override async Task RegisterCodeFixesAsync(CodeFixContext context)
    {
        var document = context.Document;
        var span = context.Span;
        var cancellationToken = context.CancellationToken;

        var root = await document.GetRequiredSyntaxRootAsync(cancellationToken).ConfigureAwait(false);

        var token = root.FindToken(span.Start);
        if (!token.Span.IntersectsWith(span))
            return;

        foreach (var type in token.Parent.GetAncestorsOrThis<TTypeSyntax>())
        {
            if (this.IsTypeInInterfaceBaseList(type))
            {
                var service = document.GetRequiredLanguageService<IImplementInterfaceService>();
                var generationOptions = context.Options.GetImplementTypeGenerationOptions(document.Project.Services);

                var info = await service.AnalyzeAsync(
                    document, type, cancellationToken).ConfigureAwait(false);
                if (info is not null)
                {
                    using var _ = ArrayBuilder<CodeAction>.GetInstance(out var codeActions);
                    await foreach (var implementOptions in GetImplementOptionsAsync(document, info, cancellationToken))
                    {
                        var title = GetTitle(implementOptions);
                        var equivalenceKey = GetEquivalenceKey(implementOptions);
                        codeActions.Add(CodeAction.Create(
                            title,
                            cancellationToken => service.ImplementInterfaceAsync(
                                document, info, implementOptions, generationOptions, cancellationToken),
                            equivalenceKey));
                    }

                    if (codeActions.Count > 0)
                        context.RegisterFixes(codeActions, context.Diagnostics);
                }

                break;
            }
        }
    }

    private static async IAsyncEnumerable<ImplementInterfaceOptions> GetImplementOptionsAsync(
        Document document, IImplementInterfaceInfo state, [EnumeratorCancellation] CancellationToken cancellationToken)
    {
        var compilation = await document.Project.GetRequiredCompilationAsync(cancellationToken).ConfigureAwait(false);

        if (state.MembersWithoutExplicitOrImplicitImplementationWhichCanBeImplicitlyImplemented.Length > 0)
        {
            var totalMemberCount = 0;
            var inaccessibleMemberCount = 0;

            foreach (var (_, members) in state.MembersWithoutExplicitOrImplicitImplementationWhichCanBeImplicitlyImplemented)
            {
                foreach (var member in members)
                {
                    totalMemberCount++;

                    if (IsLessAccessibleThan(member, state.ClassOrStructType))
                        inaccessibleMemberCount++;
                }
            }

            // If all members to implement are inaccessible, then "Implement interface" codeaction
            // will be the same as "Implement interface explicitly", so there is no point in having both of them
            if (totalMemberCount != inaccessibleMemberCount)
                yield return new() { OnlyRemaining = true };

            if (ShouldImplementDisposePattern(compilation, state, explicitly: false))
                yield return new() { ImplementDisposePattern = true };

            var delegatableMembers = GetDelegatableMembers(document, state, cancellationToken);
            foreach (var member in delegatableMembers)
                yield return new() { ThroughMember = member };

            if (state.ClassOrStructType.IsAbstract)
                yield return new() { OnlyRemaining = true, Abstractly = true };
        }

        if (state.MembersWithoutExplicitImplementation.Length > 0)
        {
            yield return new() { Explicitly = true };

            if (ShouldImplementDisposePattern(compilation, state, explicitly: true))
                yield return new() { ImplementDisposePattern = true, Explicitly = true };
        }

        if (AnyImplementedImplicitly(state))
            yield return new() { Explicitly = true, OnlyRemaining = true };
    }

    private static bool AnyImplementedImplicitly(IImplementInterfaceInfo state)
    {
        if (state.MembersWithoutExplicitOrImplicitImplementation.Length != state.MembersWithoutExplicitImplementation.Length)
        {
            return true;
        }

        for (var i = 0; i < state.MembersWithoutExplicitOrImplicitImplementation.Length; i++)
        {
            var (typeA, membersA) = state.MembersWithoutExplicitOrImplicitImplementation[i];
            var (typeB, membersB) = state.MembersWithoutExplicitImplementation[i];
            if (!typeA.Equals(typeB))
            {
                return true;
            }

            if (!membersA.SequenceEqual(membersB))
            {
                return true;
            }
        }

        return false;
    }

    private static ImmutableArray<ISymbol> GetDelegatableMembers(
        Document document, IImplementInterfaceInfo state, CancellationToken cancellationToken)
    {
        var firstInterfaceType = state.InterfaceTypes.First();

        return ImplementHelpers.GetDelegatableMembers(
            document,
            state.ClassOrStructType,
            t => t.GetAllInterfacesIncludingThis().Contains(firstInterfaceType),
            cancellationToken);
    }
}
