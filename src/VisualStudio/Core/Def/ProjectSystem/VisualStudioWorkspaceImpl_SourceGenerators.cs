﻿// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.
// See the LICENSE file in the project root for more information.

using System;
using System.ComponentModel.Composition;
using Microsoft.CodeAnalysis.Editor;
using Microsoft.CodeAnalysis.Editor.Shared.Extensions;
using Microsoft.CodeAnalysis.Host;
using Microsoft.CodeAnalysis.Host.Mef;
using Microsoft.CodeAnalysis.Text;
using Microsoft.VisualStudio.Commanding;
using Microsoft.VisualStudio.Shell;
using Microsoft.VisualStudio.Shell.Interop;
using Microsoft.VisualStudio.Text.Editor.Commanding.Commands;
using Microsoft.VisualStudio.Utilities;

namespace Microsoft.VisualStudio.LanguageServices.Implementation.ProjectSystem;

internal abstract partial class VisualStudioWorkspaceImpl
{
    private bool _isSubscribedToSourceGeneratorImpactingEvents;

    public void SubscribeToSourceGeneratorImpactingEvents()
    {
        _threadingContext.ThrowIfNotOnUIThread();
        if (_isSubscribedToSourceGeneratorImpactingEvents)
            return;

        // UIContextImpl requires IVsMonitorSelection service:
        if (ServiceProvider.GlobalProvider.GetService(typeof(IVsMonitorSelection)) == null)
            return;

        _isSubscribedToSourceGeneratorImpactingEvents = true;

        // This pattern ensures that we are called whenever the build starts/completes even if it is already in progress.
        KnownUIContexts.SolutionBuildingContext.WhenActivated(() =>
        {
            KnownUIContexts.SolutionBuildingContext.UIContextChanged += (_, e) =>
            {
                if (!e.Activated)
                {
                    // After a build occurs, transition the solution to a new source generator version.  This will
                    // ensure that any cached SG documents will be re-generated.
                    this.EnqueueUpdateSourceGeneratorVersion(projectId: null, forceRegeneration: false);
                }
            };
        });

        KnownUIContexts.SolutionExistsAndFullyLoadedContext.WhenActivated(() =>
        {
            KnownUIContexts.SolutionExistsAndFullyLoadedContext.UIContextChanged += (_, e) =>
            {
                if (e.Activated)
                {
                    // After the solution fully loads, transition the solution to a new source generator version.  This
                    // will ensure that we'll now produce correct SG docs with fully knowledge of all the user's state.
                    this.EnqueueUpdateSourceGeneratorVersion(projectId: null, forceRegeneration: false);
                }
            };
        });

        // Whenever the workspace status changes, go attempt to update generators.
        var workspaceStatusService = this.Services.GetRequiredService<IWorkspaceStatusService>();
        workspaceStatusService.StatusChanged += (_, _) => EnqueueUpdateSourceGeneratorVersion(projectId: null, forceRegeneration: false);

        // Now kick off at least the initial work to run generators.
        this.EnqueueUpdateSourceGeneratorVersion(projectId: null, forceRegeneration: false);
    }

    [Export(typeof(ICommandHandler))]
    [Name(PredefinedCommandHandlerNames.SourceGeneratorSave)]
    [ContentType(StandardContentTypeNames.Text)]
    [method: ImportingConstructor]
    [method: Obsolete(MefConstruction.ImportingConstructorMessage, error: true)]
    internal sealed class SaveCommandHandler() : IChainedCommandHandler<SaveCommandArgs>
    {
        public string DisplayName => ServicesVSResources.Roslyn_save_command_handler;

        public CommandState GetCommandState(SaveCommandArgs args, Func<CommandState> nextCommandHandler)
            => nextCommandHandler();

        public void ExecuteCommand(SaveCommandArgs args, Action nextCommandHandler, CommandExecutionContext executionContext)
        {
            nextCommandHandler();

            // Try to find a roslyn workspace and document corresponding to the text buffer.
            var container = args.SubjectBuffer.AsTextContainer();
            if (TryGetWorkspace(container, out var workspace) &&
                workspace is VisualStudioWorkspaceImpl visualStudioWorkspace)
            {
                // Note: this will work, even if the text buffer is a Document/AdditionalDoc/AnalyzerConfigDoc. We want
                // that so that saving things like an additional file will still rerun generators for the projects the
                // additional file is in.
                var documentId = workspace.GetDocumentIdInCurrentContext(container);
                if (documentId != null)
                {
                    // Enqueue for at least the project this document is from.
                    visualStudioWorkspace.EnqueueUpdateSourceGeneratorVersion(documentId.ProjectId, forceRegeneration: false);

                    // And any links as well.
                    foreach (var relatedDocument in args.SubjectBuffer.GetRelatedDocuments())
                    {
                        var relatedProjectId = relatedDocument.Project.Id;
                        if (relatedProjectId != documentId.ProjectId)
                            visualStudioWorkspace.EnqueueUpdateSourceGeneratorVersion(relatedProjectId, forceRegeneration: false);
                    }
                }
            }
        }
    }
}
