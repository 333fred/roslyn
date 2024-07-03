﻿// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.
// See the LICENSE file in the project root for more information.

using System;
using System.Collections.Immutable;
using System.ComponentModel.Composition;
using System.ComponentModel.Design;
using System.Diagnostics.CodeAnalysis;
using System.Threading;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CodeActions;
using Microsoft.CodeAnalysis.CodeFixes.Configuration;
using Microsoft.CodeAnalysis.Diagnostics;
using Microsoft.CodeAnalysis.Editor.Shared.Utilities;
using Microsoft.CodeAnalysis.ErrorReporting;
using Microsoft.CodeAnalysis.Host.Mef;
using Microsoft.CodeAnalysis.Progress;
using Microsoft.CodeAnalysis.Shared.Extensions;
using Microsoft.CodeAnalysis.Shared.TestHooks;
using Microsoft.VisualStudio.LanguageServices.Implementation.Suppression;
using Microsoft.VisualStudio.Shell;
using Microsoft.VisualStudio.Shell.Interop;
using Microsoft.VisualStudio.Shell.TableControl;
using Microsoft.VisualStudio.Utilities;
using Task = System.Threading.Tasks.Task;

namespace Microsoft.VisualStudio.LanguageServices.Implementation.TableDataSource;

[Export(typeof(VisualStudioDiagnosticListTableCommandHandler))]
internal partial class VisualStudioDiagnosticListTableCommandHandler
{
    private readonly IThreadingContext _threadingContext;
    private readonly VisualStudioWorkspace _workspace;
    private readonly VisualStudioSuppressionFixService _suppressionFixService;
    private readonly VisualStudioDiagnosticListSuppressionStateService _suppressionStateService;
    private readonly IUIThreadOperationExecutor _uiThreadOperationExecutor;
    private readonly IDiagnosticAnalyzerService _diagnosticService;
    private readonly ICodeActionEditHandlerService _editHandlerService;
    private readonly IAsynchronousOperationListener _listener;

    private IWpfTableControl? _tableControl;

    [ImportingConstructor]
    [Obsolete(MefConstruction.ImportingConstructorMessage, error: true)]
    public VisualStudioDiagnosticListTableCommandHandler(
        IThreadingContext threadingContext,
        SVsServiceProvider serviceProvider,
        VisualStudioWorkspace workspace,
        IVisualStudioSuppressionFixService suppressionFixService,
        VisualStudioDiagnosticListSuppressionStateService suppressionStateService,
        IUIThreadOperationExecutor uiThreadOperationExecutor,
        IDiagnosticAnalyzerService diagnosticService,
        ICodeActionEditHandlerService editHandlerService,
        IAsynchronousOperationListenerProvider listenerProvider)
    {
        _threadingContext = threadingContext;
        _workspace = workspace;
        _suppressionFixService = (VisualStudioSuppressionFixService)suppressionFixService;
        _suppressionStateService = suppressionStateService;
        _uiThreadOperationExecutor = uiThreadOperationExecutor;
        _diagnosticService = diagnosticService;
        _editHandlerService = editHandlerService;
        _listener = listenerProvider.GetListener(FeatureAttribute.ErrorList);
    }

    public async Task InitializeAsync(IAsyncServiceProvider serviceProvider)
    {
        var errorList = await serviceProvider.GetServiceAsync<SVsErrorList, IErrorList>(_threadingContext.JoinableTaskFactory, throwOnFailure: false).ConfigureAwait(false);
        _tableControl = errorList?.TableControl;
    }

    /// <summary>
    /// Add a command handler and status query handler for a menu item
    /// </summary>
    private static OleMenuCommand AddCommand(
        IMenuCommandService menuCommandService,
        int commandId,
        EventHandler invokeHandler,
        EventHandler beforeQueryStatus)
    {
        var commandIdWithGroupId = new CommandID(Guids.RoslynGroupId, commandId);
        var command = new OleMenuCommand(invokeHandler, delegate { }, beforeQueryStatus, commandIdWithGroupId);
        menuCommandService.AddCommand(command);
        return command;
    }

    private void OnAddSuppressionsStatus(object sender, EventArgs e)
    {
        var command = (MenuCommand)sender;
        command.Visible = _suppressionStateService.CanSuppressSelectedEntries;
        command.Enabled = command.Visible && !KnownUIContexts.SolutionBuildingContext.IsActive;
    }

    private void OnRemoveSuppressionsStatus(object sender, EventArgs e)
    {
        var command = (MenuCommand)sender;
        command.Visible = _suppressionStateService.CanRemoveSuppressionsSelectedEntries;
        command.Enabled = command.Visible && !KnownUIContexts.SolutionBuildingContext.IsActive;
    }

    private void OnAddSuppressionsInSourceStatus(object sender, EventArgs e)
    {
        var command = (MenuCommand)sender;
        command.Visible = _suppressionStateService.CanSuppressSelectedEntriesInSource;
        command.Enabled = command.Visible && !KnownUIContexts.SolutionBuildingContext.IsActive;
    }

    private void OnAddSuppressionsInSuppressionFileStatus(object sender, EventArgs e)
    {
        var command = (MenuCommand)sender;
        command.Visible = _suppressionStateService.CanSuppressSelectedEntriesInSuppressionFiles;
        command.Enabled = command.Visible && !KnownUIContexts.SolutionBuildingContext.IsActive;
    }

    private void OnAddSuppressionsInSource(object sender, EventArgs e)
        => _suppressionFixService.AddSuppressions(selectedErrorListEntriesOnly: true, suppressInSource: true, projectHierarchy: null);

    private void OnAddSuppressionsInSuppressionFile(object sender, EventArgs e)
        => _suppressionFixService.AddSuppressions(selectedErrorListEntriesOnly: true, suppressInSource: false, projectHierarchy: null);

    private void OnRemoveSuppressions(object sender, EventArgs e)
        => _suppressionFixService.RemoveSuppressions(selectedErrorListEntriesOnly: true, projectHierarchy: null);

    private async Task SetSeverityHandlerAsync(ReportDiagnostic reportDiagnostic, DiagnosticData selectedDiagnostic, Project project)
    {
        try
        {
            using var token = _listener.BeginAsyncOperation(nameof(SetSeverityHandlerAsync));
            using var context = _uiThreadOperationExecutor.BeginExecute(
                title: ServicesVSResources.Updating_severity,
                defaultDescription: ServicesVSResources.Updating_severity,
                allowCancellation: true,
                showProgress: true);

            var newSolution = await ConfigureSeverityAsync(context.UserCancellationToken).ConfigureAwait(false);
            var operations = ImmutableArray.Create<CodeActionOperation>(new ApplyChangesOperation(newSolution));
            using var scope = context.AddScope(allowCancellation: true, ServicesVSResources.Updating_severity);
            await _editHandlerService.ApplyAsync(
                _workspace,
                project.Solution,
                fromDocument: null,
                operations,
                title: ServicesVSResources.Updating_severity,
                scope.GetCodeAnalysisProgress(),
                context.UserCancellationToken).ConfigureAwait(false);

            // Kick off diagnostic re-analysis for affected document so that the configured diagnostic gets refreshed.
            if (selectedDiagnostic.DocumentId != null)
                _diagnosticService.RequestDiagnosticRefresh();
        }
        catch (OperationCanceledException)
        {
        }
        catch (Exception ex) when (FatalError.ReportAndCatch(ex))
        {
        }

        return;

        // Local functions.
        async System.Threading.Tasks.Task<Solution> ConfigureSeverityAsync(CancellationToken cancellationToken)
        {
            var diagnostic = await selectedDiagnostic.ToDiagnosticAsync(project, cancellationToken).ConfigureAwait(false);
            return await ConfigurationUpdater.ConfigureSeverityAsync(reportDiagnostic, diagnostic, project, cancellationToken).ConfigureAwait(false);
        }
    }

    private bool TryGetPathToAnalyzerConfigDoc(DiagnosticData selectedDiagnostic, [NotNullWhen(true)] out Project? project, [NotNullWhen(true)] out string? pathToAnalyzerConfigDoc)
    {
        project = _workspace.CurrentSolution.GetProject(selectedDiagnostic.ProjectId);
        pathToAnalyzerConfigDoc = project?.TryGetAnalyzerConfigPathForProjectConfiguration();
        return pathToAnalyzerConfigDoc is not null;
    }
}
