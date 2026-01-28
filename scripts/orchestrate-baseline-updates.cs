#!/usr/bin/env dotnet
#:package GitHub.Copilot.SDK
// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.
// See the LICENSE file in the project root for more information.

using System.Diagnostics;
using System.Runtime.CompilerServices;
using System.Text;
using System.Threading;
using GitHub.Copilot.SDK;

var repoRoot = GetRepoRoot();
const string RunTestsCommand = "./build.sh";
var runTestsArgs = "--testCoreClr --testCompilerOnly --restore --agentOutput";
var skipFirstRun = false;
var maxParallel = Environment.ProcessorCount;

for (var i = 0; i < args.Length; i++)
{
    var arg = args[i];
    switch (arg)
    {
        case "--runTestsArgs":
            runTestsArgs = RequireValue(args, ref i, arg);
            break;
        case "--skipFirstRun":
            skipFirstRun = true;
            break;
        case "--maxParallel":
            maxParallel = int.Parse(RequireValue(args, ref i, arg));
            break;
        case "--help":
        case "-h":
            PrintHelp();
            return;
        default:
            Console.Error.WriteLine($"Unknown argument: {arg}");
            PrintHelp();
            return;
    }
}

if (maxParallel < 1)
{
    maxParallel = 1;
}

await using var copilotClient = CreateCopilotClient(repoRoot);
await copilotClient.StartAsync(CancellationToken.None);

var firstRunSkipped = false;
while (true)
{
    string? summaryPath;
    if (skipFirstRun && !firstRunSkipped)
    {
        firstRunSkipped = true;
        summaryPath = ResolveSummaryPath(repoRoot, output: null);
    }
    else
    {
        EnsureAgentOutputArg(ref runTestsArgs);
        var runResult = RunCommand(RunTestsCommand, runTestsArgs, repoRoot);
        if (runResult.ExitCode == 0)
        {
            Console.WriteLine("All tests passed. Baselines are up to date.");
            return;
        }

        summaryPath = ResolveSummaryPath(repoRoot, runResult.Output);
        if (string.IsNullOrEmpty(summaryPath) || !File.Exists(summaryPath))
        {
            Console.Error.WriteLine("summary.txt was not found. Aborting.");
            Console.Error.WriteLine(runResult.Output);
            return;
        }
    }

    if (string.IsNullOrEmpty(summaryPath) || !File.Exists(summaryPath))
    {
        Console.Error.WriteLine("summary.txt was not found. Aborting.");
        return;
    }

    var fileSummaryPaths = File.ReadAllLines(summaryPath)
        .Select(line => line.Trim())
        .Where(line => !string.IsNullOrEmpty(line))
        .ToList();

    if (fileSummaryPaths.Count == 0)
    {
        Console.WriteLine("No failing files found. Baselines are up to date.");
        return;
    }

    fileSummaryPaths.Sort(StringComparer.Ordinal);

    using var semaphore = new SemaphoreSlim(initialCount: maxParallel, maxCount: maxParallel);
    var tasks = new List<Task>();

    foreach (var fileSummaryPath in fileSummaryPaths)
    {
        tasks.Add(Task.Run(async () =>
        {
            await semaphore.WaitAsync().ConfigureAwait(false);
            try
            {
                await ProcessFileSummaryAsync(repoRoot, fileSummaryPath, copilotClient).ConfigureAwait(false);
            }
            finally
            {
                semaphore.Release();
            }
        }));
    }

    await Task.WhenAll(tasks).ConfigureAwait(false);
}

static async Task ProcessFileSummaryAsync(string repoRoot, string fileSummaryPath, CopilotClient copilotClient)
{
    var summaryFullPath = Path.IsPathRooted(fileSummaryPath)
        ? fileSummaryPath
        : Path.Combine(repoRoot, fileSummaryPath);

    if (!File.Exists(summaryFullPath))
    {
        Console.Error.WriteLine($"Missing file summary: {summaryFullPath}");
        return;
    }

    var folderPath = Path.GetDirectoryName(summaryFullPath) ?? repoRoot;
    var folderName = new DirectoryInfo(folderPath).Name;
    var fileNamePart = ExtractFileNamePart(folderName);
    var testFilePath = ResolveTestFilePath(repoRoot, fileNamePart);
    if (string.IsNullOrEmpty(testFilePath))
    {
        Console.Error.WriteLine($"Could not resolve test file for folder '{folderName}'.");
        return;
    }

    var tasks = new List<(int Line, string OutputPath)>();
    foreach (var line in File.ReadAllLines(summaryFullPath))
    {
        if (string.IsNullOrWhiteSpace(line))
        {
            continue;
        }

        var parts = line.Split(' ', 2, StringSplitOptions.RemoveEmptyEntries);
        if (parts.Length != 2 || !int.TryParse(parts[0], out var lineNumber))
        {
            continue;
        }

        var outputPath = Path.Combine(folderPath, parts[1]);
        tasks.Add((lineNumber, outputPath));
    }

    tasks.Sort((left, right) => right.Line.CompareTo(left.Line));

    foreach (var task in tasks)
    {
        if (!File.Exists(task.OutputPath))
        {
            Console.Error.WriteLine($"Missing test output: {task.OutputPath}");
            continue;
        }

        var prompt = BuildPrompt(testFilePath, task.OutputPath);
        try
        {
            var response = await SendCopilotPromptAsync(copilotClient, repoRoot, prompt).ConfigureAwait(false);
            if (!ContainsDone(response))
            {
                Console.Error.WriteLine("Copilot response did not include 'done'. Output:");
                Console.Error.WriteLine(response);
                WaitForUser(testFilePath, task.OutputPath, prompt);
            }
        }
        catch (Exception ex)
        {
            Console.Error.WriteLine("Copilot SDK request failed:");
            Console.Error.WriteLine(ex.ToString());
            WaitForUser(testFilePath, task.OutputPath, prompt);
        }
    }
}

static string BuildPrompt(string testFilePath, string outputPath)
{
    return $"Update baselines in {testFilePath} using output from {outputPath}. Use the compiler-baseline-update-skill. Do not ask questions. Do not use web or terminal tools. Respond only with 'done' or 'done (errors: ...)'.";
}

static string ExtractFileNamePart(string folderName)
{
    var underscore = folderName.IndexOf('_');
    if (underscore <= 0 || underscore == folderName.Length - 1)
    {
        return folderName;
    }

    return folderName.Substring(underscore + 1);
}

static string? ResolveTestFilePath(string repoRoot, string fileNamePart)
{
    var searchRoot = Path.Combine(repoRoot, "src");
    if (!Directory.Exists(searchRoot))
    {
        return null;
    }

    var matches = new List<string>();
    matches.AddRange(Directory.EnumerateFiles(searchRoot, fileNamePart + ".cs", SearchOption.AllDirectories));
    matches.AddRange(Directory.EnumerateFiles(searchRoot, fileNamePart + ".vb", SearchOption.AllDirectories));

    if (matches.Count == 1)
    {
        return matches[0];
    }

    return null;
}

static void EnsureAgentOutputArg(ref string runTestsArgs)
{
    if (!runTestsArgs.Contains("--agentOutput", StringComparison.OrdinalIgnoreCase)
        && !runTestsArgs.Contains("--agent-output", StringComparison.OrdinalIgnoreCase))
    {
        runTestsArgs += " --agentOutput";
    }
}

static string? ResolveSummaryPath(string repoRoot, string? output)
{
    if (!string.IsNullOrEmpty(output))
    {
        var lines = output.Split(new[] { '\r', '\n' }, StringSplitOptions.RemoveEmptyEntries);
        for (var i = lines.Length - 1; i >= 0; i--)
        {
            var line = lines[i].Trim();
            if (line.EndsWith("summary.txt", StringComparison.OrdinalIgnoreCase))
            {
                var candidate = Path.IsPathRooted(line) ? line : Path.Combine(repoRoot, line);
                if (File.Exists(candidate))
                {
                    return candidate;
                }
            }
        }
    }

    var defaultPath = Path.Combine(repoRoot, "artifacts", "log", "Debug", "agent-output", "summary.txt");
    return File.Exists(defaultPath) ? defaultPath : null;
}

static (int ExitCode, string Output) RunCommand(string fileName, string arguments, string workingDirectory)
{
    var startInfo = new ProcessStartInfo
    {
        FileName = fileName,
        Arguments = arguments,
        WorkingDirectory = workingDirectory,
        RedirectStandardOutput = true,
        RedirectStandardError = true,
        UseShellExecute = false
    };

    using var process = new Process { StartInfo = startInfo };
    var output = new StringBuilder();
    process.OutputDataReceived += (_, e) =>
    {
        if (e.Data != null)
        {
            output.AppendLine(e.Data);
        }
    };
    process.ErrorDataReceived += (_, e) =>
    {
        if (e.Data != null)
        {
            output.AppendLine(e.Data);
        }
    };

    process.Start();
    process.BeginOutputReadLine();
    process.BeginErrorReadLine();
    process.WaitForExit();

    return (process.ExitCode, output.ToString());
}

static bool ContainsDone(string output)
{
    return output.IndexOf("done", StringComparison.OrdinalIgnoreCase) >= 0;
}

static void WaitForUser(string testFilePath, string outputPath, string prompt)
{
    Console.WriteLine("Agent failed to complete an update.");
    Console.WriteLine("Inputs to the agent:");
    Console.WriteLine($"  Test file: {testFilePath}");
    Console.WriteLine($"  Test output: {outputPath}");
    Console.WriteLine($"  Prompt: {prompt}");
    Console.WriteLine("Please review the output above, fix the issue manually, then press Enter to continue.");
    Console.WriteLine("If you want to abort, press Ctrl+C.");
    Console.ReadLine();
}

static string RequireValue(string[] args, ref int index, string arg)
{
    if (index + 1 >= args.Length)
    {
        throw new ArgumentException($"Missing value for {arg}");
    }

    index++;
    return args[index];
}

static void PrintHelp()
{
    Console.WriteLine("Baseline updater orchestrator (file-based app)");
    Console.WriteLine("Usage:");
    Console.WriteLine("  dotnet run scripts/orchestrate-baseline-updates.cs [options]");
    Console.WriteLine();
    Console.WriteLine("Options:");
    Console.WriteLine("  --runTestsArgs <args>         Test args (default: --testCoreClr --testCompilerOnly --restore --agentOutput)");
    Console.WriteLine("  --skipFirstRun                Skip test run on first loop and use existing summary.txt");
    Console.WriteLine("  --maxParallel <n>             Max parallel file updates (default: CPU count)");
}

static string GetRepoRoot([CallerFilePath] string sourceFilePath = "")
{
    if (Path.GetDirectoryName(sourceFilePath) is not string scriptsDir
        || Path.GetDirectoryName(scriptsDir) is not string repoRoot
        || !File.Exists(Path.Join(repoRoot, "scripts", Path.GetFileName(sourceFilePath))))
    {
        Console.WriteLine("Could not determine source file path. This script must be located in the 'scripts' directory of the Roslyn repo.");
        Environment.Exit(1);
        throw null!;
    }

    return repoRoot;
}

static CopilotClient CreateCopilotClient(string repoRoot)
{
    return new CopilotClient(new CopilotClientOptions
    {
        Cwd = repoRoot,
        AutoStart = false,
        AutoRestart = true,
        LogLevel = "warn"
    });
}

static async Task<string> SendCopilotPromptAsync(CopilotClient copilotClient, string repoRoot, string prompt)
{
    var sessionConfig = new SessionConfig
    {
        Model = "gpt-5",
        SkillDirectories = new List<string>
        {
            Path.Combine(repoRoot, ".github", "skills")
        },
        InfiniteSessions = new InfiniteSessionConfig
        {
            Enabled = false
        }
    };

    await using var session = await copilotClient.CreateSessionAsync(sessionConfig, CancellationToken.None).ConfigureAwait(false);
    var responseEvent = await session.SendAndWaitAsync(new MessageOptions { Prompt = prompt }, timeout: null, CancellationToken.None).ConfigureAwait(false);
    return responseEvent.Data.Content ?? string.Empty;
}
