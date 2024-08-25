﻿// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.
// See the LICENSE file in the project root for more information.

using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Composition;
using System.IO;
using Microsoft.CodeAnalysis.Host.Mef;
using Microsoft.CodeAnalysis.Diagnostics;

#if NET
using System.Runtime.Loader;
#endif

namespace Microsoft.CodeAnalysis.Host;

internal interface IAnalyzerAssemblyLoaderProvider : IWorkspaceService
{
    IAnalyzerAssemblyLoaderInternal SharedShadowCopyLoader { get; }

#if NET
    /// <summary>
    /// Creates a fresh shadow copying loader that will load all <see cref="AnalyzerReference"/>s in a fresh <see
    /// cref="AssemblyLoadContext"/>.
    /// </summary>
    IAnalyzerAssemblyLoaderInternal CreateNewShadowCopyLoader();
#endif
}

/// <summary>
/// Abstract implementation of an analyzer assembly loader that can be used by VS/VSCode to provide a <see
/// cref="IAnalyzerAssemblyLoader"/> with an appropriate path.
/// </summary>
internal abstract class AbstractAnalyzerAssemblyLoaderProviderFactory(
    IEnumerable<IAnalyzerAssemblyResolver> externalResolvers) : IWorkspaceServiceFactory
{
    private readonly ImmutableArray<IAnalyzerAssemblyResolver> _externalResolvers = externalResolvers.ToImmutableArray();

    public IWorkspaceService CreateService(HostWorkspaceServices workspaceServices)
        => new DefaultAnalyzerAssemblyLoaderProvider(this, workspaceServices.Workspace.Kind ?? "Default");

    protected virtual IAnalyzerAssemblyLoaderInternal WrapLoader(IAnalyzerAssemblyLoaderInternal loader)
        => loader;

    private sealed class DefaultAnalyzerAssemblyLoaderProvider : IAnalyzerAssemblyLoaderProvider
    {
        private readonly AbstractAnalyzerAssemblyLoaderProviderFactory _factory;
        private readonly string _workspaceKind;
        private readonly Lazy<IAnalyzerAssemblyLoaderInternal> _shadowCopyLoader;

        public DefaultAnalyzerAssemblyLoaderProvider(
            AbstractAnalyzerAssemblyLoaderProviderFactory factory,
            string workspaceKind)
        {
            _factory = factory;
            _workspaceKind = workspaceKind;

            // We use a lazy here in case creating the loader requires MEF imports in the derived constructor.
            _shadowCopyLoader = new(CreateShadowCopyLoader);
        }

        public IAnalyzerAssemblyLoaderInternal SharedShadowCopyLoader
            => _shadowCopyLoader.Value;

        public IAnalyzerAssemblyLoaderInternal CreateNewShadowCopyLoader()
            => CreateShadowCopyLoader();

        private IAnalyzerAssemblyLoaderInternal CreateShadowCopyLoader()
            => _factory.WrapLoader(DefaultAnalyzerAssemblyLoader.CreateNonLockingLoader(
                    Path.Combine(Path.GetTempPath(), nameof(Roslyn), "AnalyzerAssemblyLoader", _workspaceKind),
                    _factory._externalResolvers));
    }
}

[ExportWorkspaceServiceFactory(typeof(IAnalyzerAssemblyLoaderProvider)), Shared]
[method: ImportingConstructor]
[method: Obsolete(MefConstruction.ImportingConstructorMessage, error: true)]
internal sealed class DefaultAnalyzerAssemblyLoaderService(
    [ImportMany] IEnumerable<IAnalyzerAssemblyResolver> externalResolvers)
    : AbstractAnalyzerAssemblyLoaderProviderFactory(externalResolvers);
