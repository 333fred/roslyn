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
            var fileSummariesDirectory = Path.Combine(outputDirectory, "file-summaries");
            Directory.CreateDirectory(fileSummariesDirectory);

            var jsonSettings = new JsonSerializerSettings
            {
                NullValueHandling = NullValueHandling.Ignore
            };

            var recordIndex = 0;
            foreach (var record in failureRecords)
            {
                recordIndex++;
                var outputFileName = $"{recordIndex:D6}.json";
                var outputPath = Path.Combine(outputDirectory, outputFileName);
                record.OutputFileName = outputFileName;
                var recordJson = JsonConvert.SerializeObject(record, Formatting.None, jsonSettings);
                File.WriteAllText(outputPath, recordJson);
            }

            var fileTasks = WriteFileSummaries(fileSummariesDirectory, failureRecords);
            var summaryFilePath = Path.Combine(outputDirectory, "tasks.txt");
            WriteTaskSummaryFile(summaryFilePath, fileTasks);
            Console.WriteLine(MakeRelativePath(summaryFilePath) ?? summaryFilePath);
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

        private static List<AgentFileTask> WriteFileSummaries(string fileSummariesDirectory, List<AgentFailureRecord> failureRecords)
        {
            var map = new Dictionary<string, List<AgentFailureRecord>>(StringComparer.Ordinal);
            foreach (var record in failureRecords)
            {
                if (string.IsNullOrEmpty(record.FilePath) || string.IsNullOrEmpty(record.FullyQualifiedName))
                {
                    continue;
                }

                if (!map.TryGetValue(record.FilePath, out var list))
                {
                    list = new List<AgentFailureRecord>();
                    map.Add(record.FilePath, list);
                }

                list.Add(record);
            }

            var filePaths = map.Keys.ToList();
            filePaths.Sort(StringComparer.Ordinal);
            filePaths.Reverse();

            var tasks = new List<AgentFileTask>();
            var index = 0;
            foreach (var filePath in filePaths)
            {
                index++;
                var summaryFileName = $"{index:D6}.txt";
                var summaryFilePath = Path.Combine(fileSummariesDirectory, summaryFileName);
                var records = map[filePath];
                records.Sort((left, right) => StringComparer.Ordinal.Compare(left.FullyQualifiedName, right.FullyQualifiedName));

                var lines = new List<string>();
                foreach (var record in records)
                {
                    lines.Add($"{record.FullyQualifiedName} | {record.OutputFileName}");
                }

                File.WriteAllLines(summaryFilePath, lines);
                tasks.Add(new AgentFileTask
                {
                    FilePath = filePath,
                    SummaryFilePath = MakeRelativePath(summaryFilePath) ?? summaryFilePath
                });
            }

            return tasks;
        }

        private static void WriteTaskSummaryFile(string summaryFilePath, List<AgentFileTask> tasks)
        {
            var lines = new List<string>();

            foreach (var task in tasks)
            {
                lines.Add($"{task.FilePath} | {task.SummaryFilePath}");
            }

            File.WriteAllLines(summaryFilePath, lines);
        }

        private AgentFailureRecord CreateFailureRecord(TestResult testResult, FailedTestInfo? failedTest, string? note)
        {
            return new AgentFailureRecord
            {
                FilePath = MakeRelativePath(failedTest?.FilePath),
                FullyQualifiedName = failedTest?.FullyQualifiedName,
                ClassName = failedTest?.ClassName,
                MethodName = failedTest?.MethodName,
                Line = failedTest?.Line,
                TestOutput = failedTest?.Output,
                WorkItemDisplayName = testResult.DisplayName,
                Note = note
            };
        }

        private static string? MakeRelativePath(string? path)
        {
            if (string.IsNullOrWhiteSpace(path))
            {
                return path;
            }

            if (!Path.IsPathRooted(path))
            {
                return path;
            }

            try
            {
                var root = Directory.GetCurrentDirectory();
                return Path.GetRelativePath(root, path);
            }
            catch
            {
                return path;
            }
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

            var matches = StackTraceRegex.Matches(stackTrace);
            if (matches.Count == 0)
            {
                return (null, null);
            }

            var match = matches[^1];
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
            public string OutputFileName { get; set; } = string.Empty;
        }

        private sealed class AgentFileTask
        {
            public string FilePath { get; set; } = string.Empty;
            public string SummaryFilePath { get; set; } = string.Empty;
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
