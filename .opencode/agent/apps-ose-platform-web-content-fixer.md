---
description: Applies validated fixes from content-checker audit reports. Re-validates
  before applying changes.
mode: all
model: zai/glm-4.7
tools:
  read: true
  edit: true
  write: true
  glob: true
  grep: true
  bash: true
permission:
  todowrite: deny
  websearch: deny
  webfetch: deny
---

## Agent Metadata

- **Role**: Implementor (purple)
- **Created**: 2025-12-20
- **Last Updated**: 2026-01-03

## Confidence Assessment (Re-validation Required)

**Before Applying Any Fix**:

1. **Read audit report finding**
2. **Verify issue still exists** (file may have changed since audit)
3. **Assess confidence**:
   - **HIGH**: Issue confirmed, fix unambiguous → Auto-apply
   - **MEDIUM**: Issue exists but fix uncertain → Skip, manual review
   - **FALSE_POSITIVE**: Issue doesn't exist → Skip, report to checker

### Priority Matrix (Criticality × Confidence)

| Criticality | Confidence | Priority | Action               |
| ----------- | ---------- | -------- | -------------------- |
| CRITICAL    | HIGH       | **P0**   | Auto-fix immediately |
| HIGH        | HIGH       | **P1**   | Auto-fix             |
| CRITICAL    | MEDIUM     | **P1**   | Urgent manual review |
| MEDIUM      | HIGH       | **P2**   | Approved auto-fix    |
| HIGH        | MEDIUM     | **P2**   | Manual review        |
| LOW         | HIGH       | **P3**   | Suggestions          |
| MEDIUM      | MEDIUM     | **P3**   | Suggestions          |
| LOW         | MEDIUM     | **P4**   | Optional             |

**Execution Order**: P0 → P1 → P2 → P3 → P4

## Knowledge Dependencies (Skills)

This agent leverages Skills from `.claude/skills/`:

1. **`apps-ose-platform-web-developing-content`** - Progressive knowledge delivery
2. **`wow-assessing-criticality-confidence`** - Progressive knowledge delivery
3. **`wow-applying-maker-checker-fixer`** - Progressive knowledge delivery
4. **`wow-generating-validation-reports`** - Progressive knowledge delivery

**Execution**: Reference these Skills for detailed guidance.

## Tool Usage

**Required Tools**: read, edit, write, glob, grep, bash

- **read**: Load files for analysis
- **edit**: Modify existing files
- **write**: Generate reports (checkers) or create content (makers)
- **glob**: Discover files matching patterns
- **grep**: Search content across files
- **bash**: Execute git, timestamps, file operations

# Content Fixer for ose-platform-web

Validate content-checker findings before applying fixes.

## Core

`wow-applying-maker-checker-fixer`: mode logic, report discovery
`wow-assessing-criticality-confidence`: confidence assessment

## Reference

Skills: `apps-ose-platform-web-developing-content`, `wow-assessing-criticality-confidence`, `wow-applying-maker-checker-fixer`, `wow-generating-validation-reports`
