---
title: "Temporary Files Convention"
description: Guidelines for AI agents creating temporary uncommitted files and folders
category: explanation
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

## Principles Respected

This practice respects the following core principles:

- **[Explicit Over Implicit](../principles/software-engineering/ex-pr-se__explicit-over-implicit.md)**: Designated directories (`generated-reports/`, `local-temp/`) with explicit purposes. Report naming pattern clearly encodes agent family, timestamp, and type. No hidden temporary files scattered throughout the repository.

- **[Simplicity Over Complexity](../principles/general/ex-pr-ge__simplicity-over-complexity.md)**: Two directories for all temporary files - one for reports, one for scratch work. Simple, flat structure with clear naming conventions. No complex hierarchies or categorization schemes.

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

**CRITICAL REQUIREMENT**: All checker/fixer agents use standardized report naming pattern aligned with repository file naming convention.

**Pattern**: `{agent-family}__{YYYY-MM-DD--HH-MM}__{suffix}.md`

**Components**:

- `{agent-family}`: Agent name WITHOUT checker/fixer/maker suffix (e.g., `repo-rules`, `ayokoding-content`, `docs`, `plan`, `plan-execution`)
- `__`: Double underscore separator (aligns with repository file naming convention)
- `{YYYY-MM-DD--HH-MM}`: Timestamp in UTC+7 with double dash between date and time
- `__`: Double underscore separator
- `{suffix}`: Optional suffix (`audit`, `fix`, `validation`, `summary`)

**Separator Rules**:

- Double underscore (`__`) separates major components (agent-family, timestamp, suffix)
- Double dash (`--`) separates date from time within timestamp
- Single dash (`-`) separates components within date (YYYY-MM-DD) and time (HH-MM)
- NO "report" keyword in filename (redundant - location in `generated-reports/` makes purpose clear)

**Why this pattern**:

- **Alignment**: Follows repository file naming convention (`[prefix]__[content-identifier].md`)
- **Consistency**: Same separator style as documentation files (double underscore for major segments)
- **Clarity**: Agent family, timestamp, and suffix all clearly separated
- **Sortability**: Chronological sorting works naturally (YYYY-MM-DD first)
- **Flexibility**: Optional suffix supports different report types from same agent family
- **No Redundancy**: "report" keyword omitted (directory name already indicates purpose)

**Example files**:

```
generated-reports/repo-rules__2025-12-14--20-45__audit.md
generated-reports/repo-rules__2025-12-14--20-45__fix.md
generated-reports/ayokoding-content__2025-12-14--15-30__audit.md
generated-reports/ose-platform-web-content__2025-12-14--15-30__audit.md
generated-reports/docs__2025-12-15--10-00__validation.md
generated-reports/plan__2025-12-15--11-30__validation.md
generated-reports/plan-execution__2025-12-15--14-00__validation.md
```

**Pattern Rules**:

- Use double underscore (`__`) to separate agent-family, timestamp, and suffix
- Use double dash (`--`) to separate date from time in timestamp
- Timestamp MUST be UTC+7 (YYYY-MM-DD--HH-MM format)
- Zero-pad all timestamp components (01 not 1, 09 not 9)
- Agent family is lowercase with single dashes (multi-word: `ose-platform-web-content`, `plan-execution`)
- Suffix is lowercase, no plurals (`audit` not `audits`)
- Suffix is optional (may be omitted if report type is obvious from context)

**CRITICAL - Timestamp Generation:**

**❌ WRONG - Using placeholder timestamps:**

```bash
# DO NOT use placeholder values
filename="repo-rules__2025-12-14--00-00__audit.md"  # WRONG!
```

**✅ CORRECT - Execute bash command for actual current time:**

```bash
# MUST execute bash command to get real time
timestamp=$(TZ='Asia/Jakarta' date +"%Y-%m-%d--%H-%M")
filename="repo-rules__${timestamp}__audit.md"
# Example: repo-rules__2025-12-14--16-43__audit.md (actual time!)
```

**Why this is critical:** Placeholder timestamps like "00-00" defeat the entire purpose of timestamping. Reports must have accurate creation times for audit trails, chronological sorting, and debugging. See [Timestamp Format Convention](../conventions/ex-co__timestamp-format.md) for complete details.

#### Repository Audit Reports

**Agent**: repo-rules-checker
**Pattern**: `repo-rules__{YYYY-MM-DD--HH-MM}__audit.md`
**Example**: `repo-rules__2025-12-14--20-45__audit.md`

**Content**: Comprehensive consistency audit covering:

- CLAUDE.md vs convention documents
- Agent definitions vs conventions
- Cross-references and links
- Duplication and contradictions
- Frontmatter consistency
- File naming compliance

**Timestamp**: Audit start time in UTC+7 (YYYY-MM-DD-HH-MM format)

**Retention**: Keep for historical tracking and comparison. Review/archive older reports periodically.

#### Fixer Reports (Universal Pattern)

**Agents**: All fixer agents (repo-rules-fixer, ayokoding-content-fixer, docs-tutorial-fixer, ose-platform-web-content-fixer, readme-fixer, docs-fixer, plan-fixer)

**Pattern**: `{agent-family}__{YYYY-MM-DD--HH-MM}__fix.md`

**Universal Structure**: All fixer agents follow the same report structure:

**Naming Convention**:

- Replaces `__audit` suffix with `__fix` suffix
- **CRITICAL**: Uses SAME timestamp as source audit report
- This creates clear audit-fix report pairing for traceability

**Report Pairing Examples**:

| Agent Family             | Audit Report                                            | Fix Report                                            |
| ------------------------ | ------------------------------------------------------- | ----------------------------------------------------- |
| repo-rules               | `repo-rules__2025-12-14--20-45__audit.md`               | `repo-rules__2025-12-14--20-45__fix.md`               |
| ayokoding-content        | `ayokoding-content__2025-12-14--15-30__audit.md`        | `ayokoding-content__2025-12-14--15-30__fix.md`        |
| ose-platform-web-content | `ose-platform-web-content__2025-12-14--16-00__audit.md` | `ose-platform-web-content__2025-12-14--16-00__fix.md` |
| docs-tutorial            | `docs-tutorial__2025-12-14--10-15__audit.md`            | `docs-tutorial__2025-12-14--10-15__fix.md`            |
| readme                   | `readme__2025-12-14--09-45__audit.md`                   | `readme__2025-12-14--09-45__fix.md`                   |
| docs                     | `docs__2025-12-15--10-00__validation.md`                | `docs__2025-12-15--10-00__fix.md`                     |
| plan                     | `plan__2025-12-15--11-30__validation.md`                | `plan__2025-12-15--11-30__fix.md`                     |

**Why Same Timestamp?**

- Enables matching audit report with corresponding fix report
- Chronological sorting keeps related reports together
- Audit trail shows what was detected vs what was fixed
- Supports debugging (compare checker findings vs fixer actions)

**Universal Content Structure**:

All fixer reports include these sections:

1. **Validation Summary**:
   - Total findings processed from audit report
   - Fixes applied (HIGH confidence count)
   - False positives detected (count)
   - Needs manual review (MEDIUM confidence count)

2. **Fixes Applied**:
   - Detailed list of HIGH confidence fixes
   - What was changed in each file
   - Re-validation results confirming issue
   - Confidence level reasoning

3. **False Positives Detected**:
   - Checker findings that re-validation disproved
   - Why checker was wrong (detection logic flaw)
   - Actionable recommendations to improve checker
   - Example code showing correct validation approach

4. **Needs Manual Review**:
   - MEDIUM confidence items requiring human judgment
   - Why automated fix was skipped (subjective/ambiguous/risky)
   - Action required from user

5. **Recommendations for Checker**:
   - Improvements based on false positives
   - Concrete suggestions with example code
   - Impact assessment

6. **Files Modified**:
   - Complete list of files changed during fix application
   - Total count for summary

**Confidence Levels**: All fixers use universal three-level system (HIGH/MEDIUM/FALSE_POSITIVE). See [Fixer Confidence Levels Convention](./ex-de__fixer-confidence-levels.md) for complete criteria.

**Workflow**:

1. Checker generates audit report
2. User reviews audit report
3. User invokes fixer
4. Fixer reads audit report, re-validates findings
5. Fixer applies HIGH confidence fixes automatically
6. Fixer generates fix report with same timestamp as audit

**Retention**: Keep alongside audit reports for complete audit trail. Provides transparency on automated fixes vs manual review items vs false positives.

#### Content Validation Reports

**Agents**: ayokoding-content-checker, ose-platform-web-content-checker
**Pattern**: `{site}-content__{YYYY-MM-DD--HH-MM}__audit.md`

**Examples**:

- `ayokoding-content__2025-12-14--15-30__audit.md`
- `ose-platform-web-content__2025-12-14--15-30__audit.md`

**Content**: Hugo content validation results (frontmatter, structure, quality)

#### Documentation Validation Reports

**Agent**: docs-checker
**Pattern**: `docs__{YYYY-MM-DD--HH-MM}__validation.md`
**Example**: `docs__2025-12-15--10-00__validation.md`

**Content**: Documentation factual accuracy and consistency validation

#### Plan Validation Reports

**Agent**: plan-checker
**Pattern**: `plan__{YYYY-MM-DD--HH-MM}__validation.md`
**Example**: `plan__2025-12-15--11-30__validation.md`

**Content**: Plan readiness validation (completeness, accuracy, implementability)

#### Plan Execution Validation Reports

**Agent**: plan-execution-checker
**Pattern**: `plan-execution__{YYYY-MM-DD--HH-MM}__validation.md`
**Example**: `plan-execution__2025-12-15--14-00__validation.md`

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
5. **MUST have both Write and Bash tools** in their frontmatter

**Tool Requirements**:

Any agent writing to `generated-reports/` MUST have:

- **Write tool**: Required for creating report files
- **Bash tool**: Required for generating UTC+7 timestamps using `TZ='Asia/Jakarta' date +"%Y-%m-%d--%H-%M"`

**Example frontmatter**:

```yaml
---
name: repo-rules-checker
description: Validates consistency between agents, CLAUDE.md, conventions, and documentation.
tools: Read, Glob, Grep, Write, Bash
model: sonnet
color: green
---
```

**Rationale**: Write tool creates the file, Bash tool generates accurate timestamps. Both are mandatory for report-generating agents.

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
