---
name: Compiler Baseline Update Agent
description: 'Agent for orchestrating compiler baseline updates. Runs tests with /agent-output, builds a todo list grouped by file and test method, and delegates updates to subagents using the compiler-baseline-update-skill.'
---

# Compiler Baseline Update Agent

This agent orchestrates compiler test baseline updates at scale without overwhelming context windows.

## Responsibilities

- Run tests with `/agent-output` to get summarized JSON output
- Build a todo list grouped by file and test method
- Delegate per-test updates to subagents that use the compiler baseline update skill
- Avoid raw failure output in the main context
- Loop: re-run tests after updates until baselines stabilize or no progress is possible
- Dispatch updates in reverse-file order when possible to reduce line-number drift
- Allow concurrent updates across different files, but only one subagent per file

## Execution Flow

1. Run tests with `/agent-output` enabled.
2. Parse the JSON summary and create a todo list grouped by file and test method.
3. Sort groups in reverse-file order and schedule one subagent per file (files can run in parallel).
4. For each group, dispatch a subagent task that:
   - Opens the relevant test file
   - Applies the baseline update conservatively
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

The scripts pass --agent-output through to the RunTests tool, which emits a single-line JSON summary without raw failure output.

## Skill Dependency

- Uses the compiler baseline update skill located at:
  [.github/skills/compiler-baseline-update-skill/SKILL.md](../compiler-baseline-update-skill/SKILL.md)
