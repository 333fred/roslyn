// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.
// See the LICENSE file in the project root for more information.

using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.IO;
using System.Linq;
using System.Text.RegularExpressions;
using System.Xml.Linq;
using Newtonsoft.Json;

namespace RunTests
{
    internal sealed partial class AgentOutputReporter
    {
        private readonly Options _options;

        internal AgentOutputReporter(Options options)
        {
            _options = options;
        }

        internal void WriteSummary(ImmutableArray<TestResult> testResults)
        {
            var summary = CreateAgentOutputSummary(testResults);
            var json = JsonConvert.SerializeObject(summary, Formatting.None, new JsonSerializerSettings
            {
                NullValueHandling = NullValueHandling.Ignore
            });

            Console.WriteLine(json);
        }

        private AgentOutputSummary CreateAgentOutputSummary(ImmutableArray<TestResult> testResults)
        {
            var groups = new Dictionary<string, AgentFailureGroup>(StringComparer.Ordinal);
            var failedTestCount = 0;
            var failedWorkItemCount = 0;

            foreach (var testResult in testResults)
            {
                if (testResult.Succeeded)
                {
                    continue;
                }

                failedWorkItemCount++;
                var failureLogPath = WriteFailureLogFile(testResult);
                var failedTests = GetFailedTests(testResult);

                if (failedTests.Count == 0)
                {
                    var group = GetOrCreateGroup(groups, filePath: null, fullyQualifiedName: null, className: null, methodName: null);
                    group.Instances.Add(new AgentFailureInstance
                    {
                        AssemblyPath = testResult.WorkItemInfo.Filters.Keys.FirstOrDefault().AssemblyPath,
                        WorkItemDisplayName = testResult.DisplayName,
                        ResultsFilePath = testResult.TestResultInfo.ResultsFilePath,
                        HtmlResultsFilePath = testResult.TestResultInfo.HtmlResultsFilePath,
                        FailureLogPath = failureLogPath,
                        Line = null,
                        Note = "No per-test failures were found in the results file."
                    });

                    continue;
                }

                foreach (var failedTest in failedTests)
                {
                    failedTestCount++;
                    var group = GetOrCreateGroup(groups, failedTest.FilePath, failedTest.FullyQualifiedName, failedTest.ClassName, failedTest.MethodName);
                    group.Instances.Add(new AgentFailureInstance
                    {
                        AssemblyPath = testResult.WorkItemInfo.Filters.Keys.FirstOrDefault().AssemblyPath,
                        WorkItemDisplayName = testResult.DisplayName,
                        ResultsFilePath = testResult.TestResultInfo.ResultsFilePath,
                        HtmlResultsFilePath = testResult.TestResultInfo.HtmlResultsFilePath,
                        FailureLogPath = failureLogPath,
                        Line = failedTest.Line
                    });
                }
            }

            var summary = new AgentOutputSummary
            {
                Version = 1,
                Run = new AgentRunInfo
                {
                    Configuration = _options.Configuration,
                    Architecture = _options.Architecture,
                    TestResultsDirectory = _options.TestResultsDirectory,
                    LogFilesDirectory = _options.LogFilesDirectory,
                    TimestampUtc = DateTime.UtcNow.ToString("O")
                },
                Summary = new AgentSummaryCounts
                {
                    FailedTestCount = failedTestCount,
                    FailedWorkItemCount = failedWorkItemCount,
                    GroupCount = groups.Count
                },
                Groups = groups.Values
                    .OrderBy(group => group.FilePath, StringComparer.Ordinal)
                    .ThenBy(group => group.FullyQualifiedName, StringComparer.Ordinal)
                    .ToList()
            };

            return summary;
        }

        private string WriteFailureLogFile(TestResult testResult)
        {
            var outputLogPath = Path.Combine(_options.LogFilesDirectory, $"xUnitFailure-{testResult.DisplayName}.log");
            Directory.CreateDirectory(_options.LogFilesDirectory);
            File.WriteAllText(outputLogPath, testResult.StandardOutput ?? "");
            return outputLogPath;
        }

        private static AgentFailureGroup GetOrCreateGroup(Dictionary<string, AgentFailureGroup> groups, string? filePath, string? fullyQualifiedName, string? className, string? methodName)
        {
            var key = BuildGroupKey(filePath, fullyQualifiedName);
            if (!groups.TryGetValue(key, out var group))
            {
                group = new AgentFailureGroup
                {
                    FilePath = filePath,
                    FullyQualifiedName = fullyQualifiedName,
                    ClassName = className,
                    MethodName = methodName,
                    Instances = new List<AgentFailureInstance>()
                };

                groups.Add(key, group);
            }

            return group;
        }

        private static string BuildGroupKey(string? filePath, string? fullyQualifiedName)
        {
            return $"{filePath ?? string.Empty}\0{fullyQualifiedName ?? string.Empty}";
        }

        private static List<FailedTestInfo> GetFailedTests(TestResult testResult)
        {
            var resultsFilePath = testResult.TestResultInfo.ResultsFilePath;
            if (string.IsNullOrEmpty(resultsFilePath) || !File.Exists(resultsFilePath))
            {
                return new List<FailedTestInfo>();
            }

            XDocument document;
            try
            {
                document = XDocument.Load(resultsFilePath);
            }
            catch
            {
                return new List<FailedTestInfo>();
            }

            var failedTests = new List<FailedTestInfo>();
            foreach (var element in document.Descendants())
            {
                if (!string.Equals(element.Name.LocalName, "test", StringComparison.OrdinalIgnoreCase))
                {
                    continue;
                }

                if (!IsFailure(element))
                {
                    continue;
                }

                var className = (string?)element.Attribute("type");
                var methodName = (string?)element.Attribute("method");
                var name = (string?)element.Attribute("name");
                var fullyQualifiedName = name ?? BuildFullyQualifiedName(className, methodName);
                var stackTrace = GetElementValue(element, "stack-trace");
                var (filePath, line) = TryParseFileAndLine(stackTrace);

                failedTests.Add(new FailedTestInfo
                {
                    ClassName = className,
                    MethodName = methodName,
                    FullyQualifiedName = fullyQualifiedName,
                    FilePath = filePath,
                    Line = line
                });
            }

            return failedTests;
        }

        private static string? BuildFullyQualifiedName(string? className, string? methodName)
        {
            if (string.IsNullOrEmpty(className) || string.IsNullOrEmpty(methodName))
            {
                return null;
            }

            return $"{className}.{methodName}";
        }

        private static bool IsFailure(XElement element)
        {
            var result = (string?)element.Attribute("result") ?? (string?)element.Attribute("outcome");
            return string.Equals(result, "Fail", StringComparison.OrdinalIgnoreCase)
                || string.Equals(result, "Failed", StringComparison.OrdinalIgnoreCase);
        }

        private static string? GetElementValue(XElement element, string localName)
        {
            foreach (var descendant in element.Descendants())
            {
                if (string.Equals(descendant.Name.LocalName, localName, StringComparison.OrdinalIgnoreCase))
                {
                    return descendant.Value;
                }
            }

            return null;
        }

        private static (string? FilePath, int? Line) TryParseFileAndLine(string? stackTrace)
        {
            if (string.IsNullOrWhiteSpace(stackTrace))
            {
                return (null, null);
            }

            var match = StackTraceRegex.Match(stackTrace);
            if (!match.Success)
            {
                return (null, null);
            }

            var path = match.Groups["path"].Value;
            if (string.IsNullOrWhiteSpace(path))
            {
                return (null, null);
            }

            var lineValue = match.Groups["line"].Value;
            if (!int.TryParse(lineValue, out var line))
            {
                return (path, null);
            }

            return (path, line);
        }

        private sealed class AgentOutputSummary
        {
            public int Version { get; set; }
            public AgentRunInfo Run { get; set; } = new AgentRunInfo();
            public AgentSummaryCounts Summary { get; set; } = new AgentSummaryCounts();
            public List<AgentFailureGroup> Groups { get; set; } = new List<AgentFailureGroup>();
        }

        private sealed class AgentRunInfo
        {
            public string Configuration { get; set; } = string.Empty;
            public string Architecture { get; set; } = string.Empty;
            public string TestResultsDirectory { get; set; } = string.Empty;
            public string LogFilesDirectory { get; set; } = string.Empty;
            public string TimestampUtc { get; set; } = string.Empty;
        }

        private sealed class AgentSummaryCounts
        {
            public int FailedTestCount { get; set; }
            public int FailedWorkItemCount { get; set; }
            public int GroupCount { get; set; }
        }

        private sealed class AgentFailureGroup
        {
            public string? FilePath { get; set; }
            public string? FullyQualifiedName { get; set; }
            public string? ClassName { get; set; }
            public string? MethodName { get; set; }
            public List<AgentFailureInstance> Instances { get; set; } = new List<AgentFailureInstance>();
        }

        private sealed class AgentFailureInstance
        {
            public string? AssemblyPath { get; set; }
            public string WorkItemDisplayName { get; set; } = string.Empty;
            public string? ResultsFilePath { get; set; }
            public string? HtmlResultsFilePath { get; set; }
            public string FailureLogPath { get; set; } = string.Empty;
            public int? Line { get; set; }
            public string? Note { get; set; }
        }

        private sealed class FailedTestInfo
        {
            public string? ClassName { get; set; }
            public string? MethodName { get; set; }
            public string? FullyQualifiedName { get; set; }
            public string? FilePath { get; set; }
            public int? Line { get; set; }
        }

        [GeneratedRegex(@"\s+in\s+(?<path>.*?):line\s+(?<line>\d+)", RegexOptions.CultureInvariant)]
        private static partial Regex StackTraceRegex { get; }
    }
}
