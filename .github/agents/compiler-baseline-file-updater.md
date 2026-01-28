---
name: Compiler Baseline File Updater
description: 'Per-file orchestrator. Reads a file summary and dispatches per-test subagents to update baselines within a single file.'
tools: ['read/problems', 'read/readFile', 'agent', 'edit/editFiles', 'search', 'askQuestions']
---

# Compiler Baseline File Updater

This agent operates on a single test file. It reads the file-specific summary and dispatches per-test subagents to update baselines in that file.

## Responsibilities

- Read the file summary (one task per line, `line-number TestName.txt`).
- Open the target test file and update baselines from the bottom of the file to the top.
- Dispatch one subagent per failing test, scoped to this file.
- Ensure only one subagent edits this file at a time.
- Subagents should report only `done` or `done (errors: ...)` to this agent.

## Execution Flow

1. Read the file summary path provided to the prompt.
2. Infer the test file name from the folder name (`TestProject_FileName`) and locate the file in the repo.
3. Parse tasks into an ordered list from bottom-of-file to top-of-file using the line numbers from the file summary.
4. Dispatch one subagent per failing test (in bottom-to-top order), scoped to this file. Do inspect individual tests summaries yourself, leave that to the subagents.
5. Collect subagent results and return only errors or ambiguous cases.

## Output Files

- Each task line includes `TestName.txt`, which is the per-test output to use for the baseline update.
- The per-test output files are plain text and located in the same folder as the file summary.
- Each test output file contains `Class:`, `Test:`, and the test output (including stack trace).
- Theory cases are grouped into a single file with multiple `Case:` sections.

## Skill Dependency

- Uses the compiler baseline update skill located at:
	[.github/skills/compiler-baseline-update-skill/SKILL.md](../skills/compiler-baseline-update-skill/SKILL.md)
