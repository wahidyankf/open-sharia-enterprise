---
title: "Temporary Files Convention"
description: Guidelines for AI agents creating temporary uncommitted files and folders
category: development
subcategory: development
tags:
  - temporary-files
  - ai-agents
  - file-organization
  - best-practices
created: 2025-12-01
updated: 2025-12-15
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

### Report File Naming Standard

**CRITICAL REQUIREMENT**: All checker/fixer agents use standardized report naming pattern.

**Pattern**: `{agent-family}-report-{YYYY-MM-DD-HH-MM}-{suffix}.md`

**Components**:

- `{agent-family}`: Agent name WITHOUT checker/fixer/maker suffix (e.g., `repo-rules`, `ayokoding-content`, `docs`, `plan`, `plan-execution`)
- `-report-`: Literal string with single dash separators (makes purpose immediately clear)
- `{YYYY-MM-DD-HH-MM}`: Timestamp in UTC+7 (year-month-day-hour-minute, single dashes throughout)
- `-{suffix}`: Optional suffix with single dash separator (`audit`, `fix`, `validation`, `summary`)

**Separator Rules**:

- Single dash (`-`) separates ALL components consistently
- "report" keyword is ALWAYS included in filename
- Timestamp uses single dashes throughout (YYYY-MM-DD-HH-MM)

**Why this pattern**:

- **Explicit purpose**: "report" keyword makes file purpose immediately clear
- **Simplicity**: Single dash separator is simpler and more readable than mixed separators
- **Consistency**: Same structure across all agents (easier discovery, programmatic processing)
- **Clarity**: Agent family, report keyword, timestamp, and suffix all immediately visible
- **Sortability**: Chronological sorting works naturally (YYYY-MM-DD first)
- **Flexibility**: Optional suffix supports different report types from same agent family

**Example files**:

```
generated-reports/repo-rules-report-2025-12-14-20-45-audit.md
generated-reports/repo-rules-report-2025-12-14-20-45-fix.md
generated-reports/ayokoding-content-report-2025-12-14-15-30-audit.md
generated-reports/ose-platform-web-content-report-2025-12-14-15-30-audit.md
generated-reports/docs-report-2025-12-15-10-00-validation.md
generated-reports/plan-report-2025-12-15-11-30-validation.md
generated-reports/plan-execution-report-2025-12-15-14-00-validation.md
```

**Pattern Rules**:

- Use single dash (`-`) to separate all components
- Include "report" keyword after agent-family name
- Timestamp MUST be UTC+7 (YYYY-MM-DD-HH-MM format)
- Zero-pad all timestamp components (01 not 1, 09 not 9)
- Agent family is lowercase with dashes (multi-word: `ose-platform-web-content`, `plan-execution`)
- Suffix is lowercase, no plurals (`audit` not `audits`)
- Suffix is optional (may be omitted if report type is obvious from context)

#### Repository Audit Reports

**Agent**: repo-rules-checker
**Pattern**: `repo-rules-report-{YYYY-MM-DD-HH-MM}-audit.md`
**Example**: `repo-rules-report-2025-12-14-20-45-audit.md`

**Content**: Comprehensive consistency audit covering:

- CLAUDE.md vs convention documents
- Agent definitions vs conventions
- Cross-references and links
- Duplication and contradictions
- Frontmatter consistency
- File naming compliance

**Timestamp**: Audit start time in UTC+7 (YYYY-MM-DD-HH-MM format)

**Retention**: Keep for historical tracking and comparison. Review/archive older reports periodically.

#### Repository Fix Reports

**Agent**: repo-rules-fixer
**Pattern**: `repo-rules-report-{YYYY-MM-DD-HH-MM}-fix.md`
**Example**: `repo-rules-report-2025-12-14-20-45-fix.md`

**Content**: Fix application report with validation results covering:

- Validation summary (total findings, fixes applied, false positives, manual review items)
- Detailed list of fixes applied with confidence levels
- False positives detected with recommendations for checker improvement
- Items needing manual review with reasoning
- Files modified during fix application
- Recommendations for improving repo-rules-checker accuracy

**Naming**: Replaces `-audit` suffix with `-fix` suffix (same timestamp)

**Example Pairing**:

- Audit report: `repo-rules-report-2025-12-14-20-45-audit.md`
- Fix report: `repo-rules-report-2025-12-14-20-45-fix.md`

**Workflow**: Generated after repo-rules-fixer processes a repo-rules-checker audit report

**Retention**: Keep alongside audit reports for complete audit trail. Provides transparency on what was automatically fixed vs. skipped.

#### Content Validation Reports

**Agents**: ayokoding-content-checker, ose-platform-web-content-checker
**Pattern**: `{site}-content-report-{YYYY-MM-DD-HH-MM}-audit.md`

**Examples**:

- `ayokoding-content-report-2025-12-14-15-30-audit.md`
- `ose-platform-web-content-report-2025-12-14-15-30-audit.md`

**Content**: Hugo content validation results (frontmatter, structure, quality)

#### Documentation Validation Reports

**Agent**: docs-checker
**Pattern**: `docs-report-{YYYY-MM-DD-HH-MM}-validation.md`
**Example**: `docs-report-2025-12-15-10-00-validation.md`

**Content**: Documentation factual accuracy and consistency validation

#### Plan Validation Reports

**Agent**: plan-checker
**Pattern**: `plan-report-{YYYY-MM-DD-HH-MM}-validation.md`
**Example**: `plan-report-2025-12-15-11-30-validation.md`

**Content**: Plan readiness validation (completeness, accuracy, implementability)

#### Plan Execution Validation Reports

**Agent**: plan-execution-checker
**Pattern**: `plan-execution-report-{YYYY-MM-DD-HH-MM}-validation.md`
**Example**: `plan-execution-report-2025-12-15-14-00-validation.md`

**Content**: Implementation validation against requirements

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
