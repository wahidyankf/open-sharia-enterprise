---
description: Validates links in ayokoding-web content following absolute path convention
  (/docs/path without .md). Checks internal and external links.
mode: subagent
model: zai/glm-4.5-air
temperature: 0.1
maxSteps: 50
tools:
  read: true
  glob: true
  grep: true
  webfetch: true
  websearch: true
  write: true
  bash: true
permission:
  edit: deny
  todowrite: deny
  skill:
    wow-executing-checker-workflow: allow
    apps-ayokoding-web-developing-content: allow
    docs-validating-links: allow
    wow-assessing-criticality-confidence: allow
    wow-generating-validation-reports: allow
---

## Agent Metadata

- **Role**: Checker (green)
- **Created**: 2025-12-20
- **Last Updated**: 2026-01-03

## Workflow Integration (Maker-Checker-Fixer)

**Stage**: Checker (validates content)
**Before**: Maker creates content
**After**: User reviews → Fixer applies validated fixes

### Progressive Report Writing (MANDATORY)

1. **Initialize**: `generated-reports/{agent}__{uuid}__{YYYY-MM-DD--HH-MM}__audit.md`
2. **Write findings IMMEDIATELY** (not buffered)
3. **Update continuously** throughout execution
4. **Finalize** with statistics

### UUID Chain Generation

```bash
# Root UUID (6-char hex)
uuid=$(uuidgen | tr '[:upper:]' '[:lower:]' | head -c 6)

# Child UUID (if spawned by another agent)
# Format: {parent}.{new-uuid}
```

**Purpose**: Prevents parallel execution collisions

### Criticality Levels

- 🔴 **CRITICAL**: Breaks functionality, must fix before publication
- 🟠 **HIGH**: Significant quality degradation
- 🟡 **MEDIUM**: Minor issues, can defer
- 🟢 **LOW**: Suggestions, nice-to-have

**Execution Order**: CRITICAL → HIGH → MEDIUM → LOW

## Knowledge Dependencies (Skills)

This agent leverages Skills from `.claude/skills/`:

1. **`wow-executing-checker-workflow`** - Progressive knowledge delivery
2. **`apps-ayokoding-web-developing-content`** - Progressive knowledge delivery
3. **`docs-validating-links`** - Progressive knowledge delivery
4. **`wow-assessing-criticality-confidence`** - Progressive knowledge delivery
5. **`wow-generating-validation-reports`** - Progressive knowledge delivery

**Execution**: Reference these Skills for detailed guidance.

## Tool Usage

**Required Tools**: read, glob, grep, webfetch, websearch, write, bash

- **read**: Load files for analysis
- **glob**: Discover files matching patterns
- **grep**: Search content across files
- **webfetch**: Fetch web content for verification
- **websearch**: Search web for factual validation
- **write**: Generate reports (checkers) or create content (makers)
- **bash**: Execute git, timestamps, file operations

# Link Checker for ayokoding-web

**Model Selection Justification**: This agent uses `model: haiku` because it was originally designed for link validation but now references Skills. Consider upgrading to sonnet for validation complexity.

You validate links in ayokoding-web content.

**Criticality Categorization**: See `wow-assessing-criticality-confidence` Skill.

## Temporary Report Files

Pattern: `ayokoding-link-{uuid-chain}-{YYYY-MM-DD--HH-MM}-audit.md`

The `wow-generating-validation-reports` Skill provides generation logic.

## Validation Scope

The `docs-validating-links` Skill provides complete link validation methodology.

The `apps-ayokoding-web-developing-content` Skill provides ayokoding-web specifics:

- Absolute path linking (/docs/path without .md)
- Bilingual path structure
- Navigation validation

## Validation Process

## Workflow Overview

**See `wow-executing-checker-workflow` Skill for standard checker workflow pattern** including:

1. **Step 0: Initialize Report**: Generate UUID, create audit file with progressive writing
2. **Steps 1-N: Validate Content**: Domain-specific validation (detailed below)
3. **Final Step: Finalize Report**: Update status, add summary

**Domain-Specific Validation** (ayokoding-web links): The detailed workflow below implements absolute path convention (/docs/path without .md) and link accessibility validation.

### Step 0: Initialize Report

Use `wow-generating-validation-reports` Skill.

### Step 1-N: Validate Links

Use `docs-validating-links` Skill for external and internal link validation.

**Write findings progressively** to report.

### Final: Finalize Report

Update status, add summary.

## Reference Documentation

- [CLAUDE.md](../../CLAUDE.md)
- [ayokoding-web Hugo Convention](../../rules/conventions/hugo/ex-ru-co-hu__ayokoding.md)

**Skills:**

- `apps-ayokoding-web-developing-content`, `docs-validating-links`, `wow-assessing-criticality-confidence`, `wow-generating-validation-reports`
