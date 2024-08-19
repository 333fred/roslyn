﻿// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.
// See the LICENSE file in the project root for more information.

using System;
using System.Collections;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.ComponentModel;
using System.Linq;
using System.Threading;
using System.Threading.Tasks;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.Diagnostics;
using Microsoft.CodeAnalysis.Editor.Shared.Utilities;
using Microsoft.CodeAnalysis.Internal.Log;
using Microsoft.CodeAnalysis.PooledObjects;
using Microsoft.CodeAnalysis.Remote;
using Microsoft.CodeAnalysis.Shared.TestHooks;
using Microsoft.CodeAnalysis.SourceGeneration;
using Microsoft.VisualStudio.Language.Intellisense;
using Microsoft.VisualStudio.Shell;
using Roslyn.Utilities;
using VSLangProj140;

namespace Microsoft.VisualStudio.LanguageServices.Implementation.SolutionExplorer;

internal sealed class AnalyzerItemSource : IAttachedCollectionSource, INotifyPropertyChanged
{
    private readonly AnalyzersFolderItem _analyzersFolder;
    private readonly IAnalyzersCommandHandler _commandHandler;

    private readonly BulkObservableCollection<AnalyzerItem> _items = [];

    private readonly CancellationTokenSource _cancellationTokenSource = new();
    private readonly AsyncBatchingWorkQueue _workQueue;
    private readonly IThreadingContext _threadingContext;

    private IReadOnlyCollection<AnalyzerReference> _analyzerReferences;

    public event PropertyChangedEventHandler PropertyChanged;

    private Workspace Workspace => _analyzersFolder.Workspace;
    private ProjectId ProjectId => _analyzersFolder.ProjectId;

    public AnalyzerItemSource(
        AnalyzersFolderItem analyzersFolder,
        IAnalyzersCommandHandler commandHandler,
        IAsynchronousOperationListenerProvider listenerProvider)
    {
        _analyzersFolder = analyzersFolder;
        _commandHandler = commandHandler;

        _workQueue = new AsyncBatchingWorkQueue(
            DelayTimeSpan.Idle,
            ProcessQueueAsync,
            listenerProvider.GetListener(FeatureAttribute.SourceGenerators),
            _cancellationTokenSource.Token);

        this.Workspace.WorkspaceChanged += OnWorkspaceChanged;
    }

    public object SourceItem => _analyzersFolder;

    // Defer actual determination and computation of the items until later.
    public bool HasItems => !_cancellationTokenSource.IsCancellationRequested;

    public IEnumerable Items => _items;

    private void NotifyPropertyChanged(string propertyName)
        => PropertyChanged?.Invoke(this, new PropertyChangedEventArgs(propertyName));

    private void OnWorkspaceChanged(object sender, WorkspaceChangeEventArgs e)
    {
        switch (e.Kind)
        {
            case WorkspaceChangeKind.SolutionAdded:
            case WorkspaceChangeKind.SolutionChanged:
            case WorkspaceChangeKind.SolutionReloaded:
            case WorkspaceChangeKind.SolutionRemoved:
            case WorkspaceChangeKind.SolutionCleared:
                _workQueue.AddWork();
                break;

            case WorkspaceChangeKind.ProjectAdded:
            case WorkspaceChangeKind.ProjectReloaded:
            case WorkspaceChangeKind.ProjectChanged:
            case WorkspaceChangeKind.ProjectRemoved:
                if (e.ProjectId == this.ProjectId)
                    _workQueue.AddWork();

                break;
        }
    }

    private async ValueTask ProcessQueueAsync(CancellationToken cancellationToken)
    {
        // If the project went away, then shut ourselves down.
        var project = this.Workspace.CurrentSolution.GetProject(this.ProjectId);
        if (project is null)
        {
            this.Workspace.WorkspaceChanged -= OnWorkspaceChanged;

            _cancellationTokenSource.Cancel();
            _items.Clear();
            NotifyPropertyChanged(nameof(HasItems));
            NotifyPropertyChanged(nameof(Items));
            return;
        }

        // If nothing changed wrt analyzer references, then there's nothing we need to do.
        if (project.AnalyzerReferences == _analyzerReferences)
            return;

        // Set the new set of analyzer references we're going to have AnalyzerItems for.
        _analyzerReferences = project.AnalyzerReferences;

        var computedItems = await GetItemsWithAnalyzersOrGeneratorsAsync(
            project, cancellationToken).ConfigureAwait(false);

        try
        {
            _items.BeginBulkOperation();

            _items.Clear();
            _items.AddRange(computedItems);

            return;
        }
        finally
        {
            _items.EndBulkOperation();
            NotifyPropertyChanged(nameof(Items));
        }

        async Task<ImmutableArray<AnalyzerItem>> GetItemsWithAnalyzersOrGeneratorsAsync(
            Project project,
            CancellationToken cancellationToken)
        {
            // Can only remote AnalyzerFileReferences over to the oop side.  Ignore all other kinds in the VS process.
            var analyzerFileReferences = project.AnalyzerReferences.OfType<AnalyzerFileReference>().ToImmutableArray();
            var analyzerFileReferencePaths = analyzerReferences
                .OfType<AnalyzerFileReference>()
                .SelectAsArray(r => r.FullPath);

            var client = await RemoteHostClient.TryGetClientAsync(this.Workspace, cancellationToken).ConfigureAwait(false);
            if (client is not null)
            {
                var result = await client.TryInvokeAsync<IRemoteSourceGenerationService, ImmutableArray<SourceGeneratorIdentity>>(
                    project,
                    (service, solutionChecksum, cancellationToken) => service.GetSourceGeneratorIdentitiesAsync(
                        solutionChecksum, project.Id, analyzerFileReference.FullPath, cancellationToken),
                    cancellationToken).ConfigureAwait(false);

                // If the call fails, the OOP substrate will have already reported an error
                if (!result.HasValue)
                    return [];

                return result.Value;
            }


            _analyzerItems.Add(new AnalyzerItem(_analyzersFolder, reference, _commandHandler.AnalyzerContextMenuController));
        }
    }

    private ImmutableArray<AnalyzerReference> GetFilteredAnalyzers(IEnumerable<AnalyzerReference> analyzerReferences, Project project)
    {
        // Filter out analyzer dependencies which have no diagnostic analyzers, but still retain the unresolved analyzers and analyzers with load errors.
        var builder = ArrayBuilder<AnalyzerReference>.GetInstance();
        foreach (var analyzerReference in analyzerReferences)
        {
            // Analyzer dependency:
            // 1. Must be an Analyzer file reference (we don't understand other analyzer dependencies).
            // 2. Mush have no diagnostic analyzers.
            // 3. Must have no source generators.
            // 4. Must have non-null full path.
            // 5. Must not have any assembly or analyzer load failures.
            if (analyzerReference is AnalyzerFileReference &&
                analyzerReference.GetAnalyzers(project.Language).IsDefaultOrEmpty &&
                analyzerReference.GetGenerators(project.Language).IsDefaultOrEmpty &&
                analyzerReference.FullPath != null)
            {
                continue;
            }

            builder.Add(analyzerReference);
        }

        return builder.ToImmutableAndFree();
    }
}
