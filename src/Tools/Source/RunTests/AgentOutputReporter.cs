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
            var failureRecords = CreateFailureRecords(testResults);
            var outputDirectory = Path.Combine(_options.LogFilesDirectory, "agent-output");
            Directory.CreateDirectory(outputDirectory);

            var jsonSettings = new JsonSerializerSettings
            {
                NullValueHandling = NullValueHandling.Ignore
            };

            var recordIndex = 0;
            foreach (var record in failureRecords)
            {
                recordIndex++;
                var outputPath = Path.Combine(outputDirectory, $"{recordIndex:D6}.json");
                var recordJson = JsonConvert.SerializeObject(record, Formatting.None, jsonSettings);
                File.WriteAllText(outputPath, recordJson);
            }

            var summary = CreateAgentOutputIndex(testResults, failureRecords.Count, outputDirectory);
            var summaryJson = JsonConvert.SerializeObject(summary, Formatting.None, jsonSettings);
            Console.WriteLine(summaryJson);
        }

        private List<AgentFailureRecord> CreateFailureRecords(ImmutableArray<TestResult> testResults)
        {
            var records = new List<AgentFailureRecord>();

            foreach (var testResult in testResults)
            {
                if (testResult.Succeeded)
                {
                    continue;
                }

                var failedTests = GetFailedTests(testResult);

                if (failedTests.Count == 0)
                {
                    records.Add(CreateFailureRecord(
                        testResult,
                        failedTest: null,
                        note: "No per-test failures were found in the results file."));

                    continue;
                }

                foreach (var failedTest in failedTests)
                {
                    records.Add(CreateFailureRecord(testResult, failedTest, note: null));
                }
            }

            return records
                .OrderByDescending(record => record.FilePath, StringComparer.Ordinal)
                .ThenByDescending(record => record.FullyQualifiedName, StringComparer.Ordinal)
                .ThenBy(record => record.WorkItemDisplayName, StringComparer.Ordinal)
                .ToList();
        }

        private AgentOutputIndex CreateAgentOutputIndex(ImmutableArray<TestResult> testResults, int failureRecordCount, string outputDirectory)
        {
            var failedTestCount = 0;
            var failedWorkItemCount = 0;

            foreach (var testResult in testResults)
            {
                if (testResult.Succeeded)
                {
                    continue;
                }

                failedWorkItemCount++;
                var failedTests = GetFailedTests(testResult);
                failedTestCount += failedTests.Count;
            }

            return new AgentOutputIndex
            {
                Version = 1,
                Run = CreateRunInfo(),
                Summary = new AgentSummaryCounts
                {
                    FailedTestCount = failedTestCount,
                    FailedWorkItemCount = failedWorkItemCount,
                    FailureRecordCount = failureRecordCount
                },
                OutputDirectory = outputDirectory
            };
        }

        private AgentFailureRecord CreateFailureRecord(TestResult testResult, FailedTestInfo? failedTest, string? note)
        {
            return new AgentFailureRecord
            {
                FilePath = failedTest?.FilePath,
                FullyQualifiedName = failedTest?.FullyQualifiedName,
                ClassName = failedTest?.ClassName,
                MethodName = failedTest?.MethodName,
                Line = failedTest?.Line,
                TestOutput = failedTest?.Output,
                WorkItemDisplayName = testResult.DisplayName,
                Note = note
            };
        }

        private AgentRunInfo CreateRunInfo()
        {
            return new AgentRunInfo
            {
                Configuration = _options.Configuration,
                Architecture = _options.Architecture,
                TestResultsDirectory = _options.TestResultsDirectory,
                LogFilesDirectory = _options.LogFilesDirectory,
                TimestampUtc = DateTime.UtcNow.ToString("O")
            };
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
                var message = GetElementValue(element, "message");
                var stackTrace = GetElementValue(element, "stack-trace");
                var output = GetElementValue(element, "output");
                var testOutput = BuildTestOutput(message, stackTrace, output);
                var (filePath, line) = TryParseFileAndLine(stackTrace);

                failedTests.Add(new FailedTestInfo
                {
                    ClassName = className,
                    MethodName = methodName,
                    FullyQualifiedName = fullyQualifiedName,
                    FilePath = filePath,
                    Line = line,
                    Output = testOutput
                });
            }

            return failedTests;
        }

        private static string? BuildTestOutput(string? message, string? stackTrace, string? output)
        {
            var parts = new List<string>();

            if (!string.IsNullOrWhiteSpace(message))
            {
                parts.Add(message);
            }

            if (!string.IsNullOrWhiteSpace(stackTrace))
            {
                parts.Add(stackTrace);
            }

            if (!string.IsNullOrWhiteSpace(output))
            {
                parts.Add(output);
            }

            if (parts.Count == 0)
            {
                return null;
            }

            return string.Join(Environment.NewLine + Environment.NewLine, parts);
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

        private sealed class AgentOutputIndex
        {
            public int Version { get; set; }
            public AgentRunInfo Run { get; set; } = new AgentRunInfo();
            public AgentSummaryCounts Summary { get; set; } = new AgentSummaryCounts();
            public string OutputDirectory { get; set; } = string.Empty;
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
            public int FailureRecordCount { get; set; }
        }

        private sealed class AgentFailureRecord
        {
            public string? FilePath { get; set; }
            public string? FullyQualifiedName { get; set; }
            public string? ClassName { get; set; }
            public string? MethodName { get; set; }
            public int? Line { get; set; }
            public string? TestOutput { get; set; }
            public string WorkItemDisplayName { get; set; } = string.Empty;
            public string? Note { get; set; }
        }

        private sealed class FailedTestInfo
        {
            public string? ClassName { get; set; }
            public string? MethodName { get; set; }
            public string? FullyQualifiedName { get; set; }
            public string? FilePath { get; set; }
            public int? Line { get; set; }
            public string? Output { get; set; }
        }

        [GeneratedRegex(@"\s+in\s+(?<path>.*?):line\s+(?<line>\d+)", RegexOptions.CultureInvariant)]
        private static partial Regex StackTraceRegex { get; }
    }
}
