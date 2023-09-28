﻿// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.
// See the LICENSE file in the project root for more information.

using System.Collections.Immutable;
using LSP = Microsoft.VisualStudio.LanguageServer.Protocol;

namespace Microsoft.CodeAnalysis.LanguageServer.Handler.CodeActions
{
    /// <summary>
    /// This class provides the intermediate data passed between CodeActionsHandler, CodeActionResolveHandler,
    /// and RunCodeActionsHandler. The class provides enough information for each handler to identify the code
    /// action that it is dealing with. The information is passed along via the Data property in LSP.VSCodeAction. 
    /// </summary>
    /// <param name="UniqueIdentifier"> The unique identifier of a code action. No two code actions should have the same unique identifier. </param>
    /// <param name="CustomTags"></param>
    /// <param name="Range"></param>
    /// <param name="TextDocument"></param>
    internal record CodeActionResolveData(string UniqueIdentifier, ImmutableArray<string> CustomTags, LSP.Range Range, LSP.TextDocumentIdentifier TextDocument) : DocumentResolveData(TextDocument);
}
