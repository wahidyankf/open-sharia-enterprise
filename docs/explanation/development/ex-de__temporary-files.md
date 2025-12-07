---
title: "Temporary Files Convention"
description: Guidelines for AI agents creating temporary uncommitted files and folders
category: development
tags:
  - temporary-files
  - ai-agents
  - file-organization
  - best-practices
created: 2025-12-01
updated: 2025-12-07
---

# Temporary Files Convention

Guidelines for AI agents when creating temporary uncommitted files and folders in the open-sharia-enterprise repository.

## 📋 Overview

This convention establishes designated directories for temporary files created by AI agents during validation, auditing, checking, and other automated tasks. It prevents repository clutter and provides clear organization for ephemeral outputs.

## 🎯 The Rule

**AI agents creating temporary uncommitted file(s) or folder(s) MUST use one of these directories:**

- `generated-reports/` - For validation, audit, and check reports
- `local-temp/` - For miscellaneous temporary files and scratch work

**Exception**: Unless specified otherwise by other existing rules/conventions in the repository.

## 📂 Directory Purposes

### `generated-reports/`

**Use for**: Structured reports and analysis outputs

**Examples**:

- Validation reports (docs-checker, plan-checker, etc.)
- Audit reports (repo-rules-checker)
- Execution verification reports (plan-execution-checker)
- Todo lists and progress tracking

**Note**: `docs-link-checker` does NOT create report files (outputs in conversation only)

**Naming pattern**: `YYYY-MM-DD__[report-type].md`

**Example files**:

```
generated-reports/2025-12-01__repository-audit.md
generated-reports/2025-12-01__docs-validation.md
generated-reports/2025-12-01__plan-execution-validation.md
```

### `local-temp/`

**Use for**: Miscellaneous temporary files and scratch work

**Examples**:

- Draft files before finalizing
- Temporary data processing files
- Scratch notes and calculations
- Intermediate build artifacts
- Any temporary files that don't fit the "report" category

**Naming pattern**: No strict pattern required (use descriptive names)

**Example files**:

```
local-temp/draft-convention.md
local-temp/temp-analysis.json
local-temp/scratch-notes.txt
```

## ✅ When This Applies

Use these directories when:

- Creating validation or audit reports
- Generating temporary checklists or todo lists
- Writing intermediate analysis files
- Creating scratch files for processing
- Any file that is **not meant to be committed** to version control
- Files intended for immediate review/use only

## ❌ When NOT to Use These Directories

Do NOT use these directories for:

- **Permanent documentation** - Use `docs/` directory with proper naming convention
- **Operational metadata** - Use `docs/metadata/` directory (e.g., `external-links-status.yaml` for link verification cache)
- **Project planning** - Use `plans/` directory with proper structure
- **Source code** - Use `apps/` or `libs/` directories
- **Configuration files** - Place in repository root or appropriate subdirectories
- **Files explicitly required by other conventions** - Follow the specific convention's guidelines

## 🔧 Implementation for AI Agents

### For Report-Generating Agents

Agents that create validation/audit reports (docs-checker, plan-checker, repo-rules-checker, etc.) should:

1. Use `generated-reports/` directory
2. Follow naming pattern: `YYYY-MM-DD__[report-type].md`
3. Include timestamp in filename for traceability
4. Use descriptive report type in filename

**Example implementation**:

```markdown
When generating a validation report:

- Path: `generated-reports/2025-12-01__docs-validation.md`
- Include: Timestamp, agent name, summary, detailed findings
```

### For General-Purpose Agents

Agents creating miscellaneous temporary files should:

1. Use `local-temp/` directory
2. Use descriptive filenames
3. Clean up files after use (when appropriate)
4. Document the purpose of temporary files if they're long-lived

## 🗂️ Directory Status

Both directories are **gitignored** (not tracked by version control):

Under the "Temporary files" section (line 70):

- `local-temp/`

Under the "Generated reports" section (line 73):

- `generated-reports/`

Files in these directories will not be committed to the repository.

## 🔄 Exception Handling

The rule includes "unless specified otherwise by other rules/conventions":

- If a specific convention already defines where certain files should go, **follow that convention instead**
- This rule serves as the **default/fallback** for temporary files
- When in doubt, use these directories rather than creating files in the repository root

**Example exceptions**:

- **Operational metadata files** - Use `docs/metadata/` instead (e.g., `external-links-status.yaml` is committed to git, not temporary)
- **docs-link-checker agent** - Outputs results in conversation only (no report files created)
- Agent-specific conventions may override this rule
- Task-specific requirements may specify different locations
- User instructions may explicitly request different locations

## 📚 Related Conventions

- [File Naming Convention](../conventions/ex-co__file-naming-convention.md) - For permanent documentation files
- [AI Agents Convention](./ex-de__ai-agents.md) - For agent design and tool access
- [Diátaxis Framework](../conventions/ex-co__diataxis-framework.md) - For documentation organization

## 🎯 Benefits

This convention provides:

1. **Clear Organization** - Temporary files are isolated from permanent content
2. **Prevent Clutter** - No temporary files scattered across the repository
3. **Easy Cleanup** - Both directories can be safely cleared when needed
4. **Traceability** - Generated reports include dates for tracking
5. **Consistent Behavior** - All agents follow the same pattern

## ⚠️ Important Notes

- Always use one of these directories for temporary files (never the repository root)
- Choose `generated-reports/` for structured reports, `local-temp/` for everything else
- Include dates in report filenames for traceability
- Remember these files are gitignored and won't be committed
- Clean up old files periodically to prevent accumulation
