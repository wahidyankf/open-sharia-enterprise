# rhino-cli Coverage Improvements

## Status

**Status**: In Progress
**Created**: 2026-03-19
**Scope**: `apps/rhino-cli/` (v0.12.0 → v0.13.0)

## Overview

Enhance rhino-cli's `test-coverage` command family with additional format support, per-file
reporting, coverage merging, diff coverage, and file exclusion. Also fix `spec-coverage validate`
to support demo-be backend naming conventions.

### Current State

rhino-cli v0.12.0 supports 3 coverage formats (Go cover.out, LCOV, JaCoCo XML) with single-file
threshold validation. The tool outputs aggregate coverage percentage and pass/fail status.

### Goals

1. **Cobertura XML format** -- Add the #2 most widely used coverage format (Python `coverage xml`
   default, .NET Coverlet default, GitLab CI standard)
2. **Per-file reporting** -- Show file-level coverage breakdown to identify weak spots
3. **Coverage merging** -- Combine multiple coverage files into a unified report
4. **Diff coverage** -- Report coverage only for changed lines (git diff), enabling PR quality gates
5. **File exclusion patterns** -- Exclude generated code and test utilities from coverage calculation
6. **spec-coverage demo-be support** -- Fix naming convention mismatch that prevents demo-be
   backends from using `spec-coverage validate`

### Non-Goals

- Coverage trend tracking (better served by Codecov/CI dashboard)
- Istanbul JSON format (JS/TS ecosystem already uses LCOV via Vitest v8)
- Clover XML format (niche, mostly PHP)
- HTML report generation (out of scope for CLI tool)

## Plan Files

- [Requirements](./requirements.md) -- Detailed requirements with Gherkin acceptance criteria
- [Technical Documentation](./tech-docs.md) -- Architecture, design decisions, implementation approach
- [Delivery](./delivery.md) -- Phased delivery checklist with validation steps

## Impact

### Projects Affected

| Component                    | Change                              |
| ---------------------------- | ----------------------------------- |
| `apps/rhino-cli/`            | New parsers, commands, flags, tests |
| `apps/rhino-cli/README.md`   | Document new features               |
| All `demo-be-*/project.json` | Potential spec-coverage integration |

### Version Bump

v0.12.0 → v0.13.0 (minor version: new features, backward compatible)
