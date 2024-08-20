﻿// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.
// See the LICENSE file in the project root for more information.

using System;
using System.Collections.Immutable;
using System.Runtime.CompilerServices;
using Microsoft.CodeAnalysis.Diagnostics;

#if NET

namespace Microsoft.CodeAnalysis.Serialization;

/// <summary>
/// Wrapper around a real <see cref="AnalyzerReference"/>.  An "isolated" analyzer reference is an analyzer reference
/// associated with an AssemblyLoadContext that is connected to a set of other "isolated" analyzer references.  This
/// allows for loading the analyzers and generators from it in a way that is associated with that load context, keeping
/// them separate from other analyzers and generators loaded in other load contexts, while also allowing all of those
/// instances to be collected when no longer needed.  Being isolated means that if any of the underlying assembly
/// references change, that they can be loaded side by side with the prior references.  This enables functionality like
/// live reloading of analyzers and generators when they change on disk.  Note: this is only supported on .Net Core, and
/// not .Net Framework, as only the former has AssemblyLoadContexts.
/// </summary>
internal sealed class IsolatedAnalyzerReference(
    IsolatedAssemblyReferenceSet isolatedAssemblyReferenceSet,
    AnalyzerReference underlyingAnalyzerReference) : AnalyzerReference
{
    private static readonly ConditionalWeakTable<DiagnosticAnalyzer, IsolatedAssemblyReferenceSet> s_analyzerToPinnedReferenceSet = new();
    private static readonly ConditionalWeakTable<ISourceGenerator, IsolatedAssemblyReferenceSet> s_generatorToPinnedReferenceSet = new();

    /// <summary>
    /// We keep a strong reference here.  As long as the IsolatedAnalyzerFileReference is passed out and held
    /// onto (say by a Project instance), it should keep the IsolatedAssemblyReferenceSet (and its ALC) alive.
    /// </summary>
    private readonly IsolatedAssemblyReferenceSet _isolatedAssemblyReferenceSet = isolatedAssemblyReferenceSet;

    /// <summary>
    /// The actual real <see cref="AnalyzerReference"/> we defer our operations to.
    /// </summary>
    public readonly AnalyzerReference UnderlyingAnalyzerReference = underlyingAnalyzerReference;

    public override string Display => UnderlyingAnalyzerReference.Display;
    public override string? FullPath => UnderlyingAnalyzerReference.FullPath;
    public override object Id => UnderlyingAnalyzerReference.Id;

    public override ImmutableArray<DiagnosticAnalyzer> GetAnalyzers(string language)
        => PinAnalyzers(UnderlyingAnalyzerReference.GetAnalyzers(language));

    public override ImmutableArray<DiagnosticAnalyzer> GetAnalyzersForAllLanguages()
        => PinAnalyzers(UnderlyingAnalyzerReference.GetAnalyzersForAllLanguages());

    [Obsolete]
    public override ImmutableArray<ISourceGenerator> GetGenerators()
        => PinGenerators(UnderlyingAnalyzerReference.GetGenerators());

    public override ImmutableArray<ISourceGenerator> GetGenerators(string language)
        => PinGenerators(UnderlyingAnalyzerReference.GetGenerators(language));

    public override ImmutableArray<ISourceGenerator> GetGeneratorsForAllLanguages()
        => PinGenerators(UnderlyingAnalyzerReference.GetGeneratorsForAllLanguages());

    private ImmutableArray<ISourceGenerator> PinGenerators(ImmutableArray<ISourceGenerator> generators)
    {
        // Keep a reference from each generator to the IsolatedAssemblyReferenceSet.  This will ensure it (and
        // the ALC it points at) stays alive as long as the generator instance stays alive.
        foreach (var generator in generators)
            s_generatorToPinnedReferenceSet.TryAdd(generator, _isolatedAssemblyReferenceSet);

        return generators;
    }

    private ImmutableArray<DiagnosticAnalyzer> PinAnalyzers(ImmutableArray<DiagnosticAnalyzer> analyzers)
    {
        // Keep a reference from each analyzer to the IsolatedAssemblyReferenceSet.  This will ensure it (and
        // the ALC it points at) stays alive as long as the generator instance stays alive.
        foreach (var analyzer in analyzers)
            s_analyzerToPinnedReferenceSet.TryAdd(analyzer, _isolatedAssemblyReferenceSet);

        return analyzers;
    }

    public override bool Equals(object? obj)
        => ReferenceEquals(this, obj);

    public override int GetHashCode()
        => RuntimeHelpers.GetHashCode(this);

    public override string ToString()
        => $"{nameof(IsolatedAnalyzerFileReference)}({UnderlyingAnalyzerReference})";
}

#endif
