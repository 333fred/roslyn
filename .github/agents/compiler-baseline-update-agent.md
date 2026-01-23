---
name: Compiler Baseline Update Agent
description: 'Agent for orchestrating compiler baseline updates. Runs tests with /agent-output, builds a file-level todo list, and delegates per-file/per-test updates using the compiler-baseline-update-skill.'
---

# Compiler Baseline Update Agent

This agent orchestrates compiler test baseline updates at scale without overwhelming context windows.

## Responsibilities

- Run tests with `/agent-output` to get file-level tasks
- Build a file-level todo list and dispatch a subagent per file
- Each file-level subagent dispatches per-test updates using the compiler baseline update skill
- Avoid raw failure output in the main context
- Loop: re-run tests after updates until baselines stabilize or no progress is possible
- Dispatch updates in reverse-file order when possible to reduce line-number drift
- Allow concurrent updates across different files, but only one subagent per file

## Execution Flow

1. Run tests with `/agent-output` enabled.
2. Read the summary file path printed to the console and parse it (one task per line, `filePath | fileSummaryPath`).
3. Sort tasks in reverse-file order and schedule one subagent per file (files can run in parallel).
4. Each file-level subagent reads its file summary (one line per failing test, `fullyQualifiedName | outputFileName.json`) and then:
   - Dispatches per-test subagents scoped to that file
   - Applies baseline updates conservatively
   - Preserves formatting and string literal style
5. Re-run tests with `/agent-output` enabled.
6. Repeat steps 2–5 until:
   - No failures remain, or
   - The failure set is unchanged, or
   - A fixed retry limit is reached (default: 3 loops), in which case stop and ask the user.
7. Collect subagent results and report any ambiguous cases for user review.

## Running Tests

Use the repo build scripts to invoke RunTests with agent output:

- Linux/macOS: run build.sh with --testCoreClr and --agentOutput.
- Windows: run eng/build.ps1 with -testCoreClr and -agentOutput.

For faster runs focused on compiler tests, include:

- Linux/macOS: add --testCompilerOnly
- Windows: add -testCompilerOnly

The scripts pass --agent-output through to the RunTests tool, which writes a plain-text file-level summary (one task per line, `filePath | fileSummaryPath`) and prints only the summary file path to the console. Each file summary lists failing tests as `fullyQualifiedName | outputFileName.json`.

## Skill Dependency

- Uses the compiler baseline update skill located at:
  [.github/skills/compiler-baseline-update-skill/SKILL.md](../skills/compiler-baseline-update-skill/SKILL.md)
