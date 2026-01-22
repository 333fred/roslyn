---
name: compiler-baseline-update-skill
description: 'Conservative baseline updater for compiler tests. Use when updating expected diagnostics, IL, or runtime outputs in Roslyn compiler tests, especially after large changes. Preserves formatting, string literal style, and conditional baselines; stops and asks when output does not match requested update scope.'
---

# Compiler Baseline Update Skill

A conservative, formatting-preserving baseline updater for Roslyn compiler tests. Intended to be used by agents or directly when updating expected diagnostics, IL, or runtime output baselines.

## When to Use This Skill

- Large compiler changes cause many test baselines to change
- Updating expected diagnostics, IL output, or runtime output in tests
- Tests use conditional baselines for theories or inputs
- Need to keep formatting and string literal style intact

## Core Principles

- Preserve existing indentation, alignment, and spacing
- Preserve string literal style (verbatim, raw, interpolated) and quoting
- Preserve conditional baseline logic (`if`/`switch`/interpolated paths)
- Only update within the user-specified scope
- If output changes outside the requested scope, stop and ask for direction

## Workflow

1. Identify the failing test method and locate its baseline in the test file.
2. Read enough surrounding code to understand the baseline structure and any branching logic.
3. Update only the requested baseline portion, preserving:
   - Indentation and spacing
   - Literal type (raw vs verbatim vs interpolated)
   - Existing conditional logic and data flow
4. For theories, ensure per-input baselines remain distinct and properly keyed.
5. If the new output does not fit the existing baseline pattern, or extra unexpected output appears, stop and ask for guidance.

## Baseline Output Format

Test output typically contains:

- Expected:
- Actual:
- Diff: (optional)

The update source is the **Actual** section. Copy the exact contents of the Actual section into the test baseline.
Ignore Diff and any other diagnostic output.

### Escaping Rules

- If the existing baseline uses a verbatim string literal, escape internal quotes by doubling them.
- If the existing baseline uses a regular string literal, escape backslashes and quotes as needed.
- If the existing baseline uses a raw string literal, preserve raw string formatting and indentation.
- If the existing baseline uses interpolated strings, keep interpolation intact and update only fixed text.

Always preserve the original string literal style unless explicitly instructed otherwise.

## Locating the Failing Baseline

- Use stack traces to find candidate file and method, but line numbers may drift if earlier tests were updated.
- Prefer searching by test method name and nearby baseline markers in the same file.
- Read enough context to confirm you are editing the correct test and baseline within the test.

### Order and Concurrency Guidance

- Process updates in reverse-file order when possible to reduce line-number drift.
- Avoid concurrent updates to the same file. Only one agent should modify a file at a time to prevent conflicts.

## Guidance for Theories and Conditional Baselines

- If the test uses `if`/`switch` to select baselines, keep that structure intact.
- If the test uses interpolated strings to inject test inputs, preserve the interpolation and update only the fixed parts.
- Do not collapse multiple baselines into a single string unless explicitly requested.

## Safety Checks

- Do not reformat unrelated code.
- Do not change test logic or control flow.
- Do not update baselines for other tests.
- If no baseline is found, or if the requested update is ambiguous, stop and ask.
