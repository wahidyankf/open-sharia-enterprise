---
title: "Maker-Checker-Fixer Pattern Convention"
description: Three-stage content quality workflow pattern used across multiple agent families for systematic content creation, validation, and remediation
category: explanation
subcategory: development
tags:
  - maker-checker-fixer
  - workflow
  - content-quality
  - agent-patterns
  - validation
  - automation
created: 2025-12-14
updated: 2025-12-24
---

# Maker-Checker-Fixer Pattern Convention

This document defines the **maker-checker-fixer pattern**, a three-stage content quality workflow used across multiple agent families in this repository. The pattern ensures high-quality content through systematic creation, validation, and remediation cycles.

## Principles Implemented/Respected

**REQUIRED SECTION**: All development practice documents MUST include this section to ensure traceability from practices back to foundational values.

This practice respects the following core principles:

- **[Automation Over Manual](../../principles/software-engineering/automation-over-manual.md)**: Checker agents automatically validate content against conventions. Fixer agents apply validated fixes without manual intervention. Human effort focuses on content creation and subjective improvements, not mechanical validation.

- **[Simplicity Over Complexity](../../principles/general/simplicity-over-complexity.md)**: Three clear stages (make, check, fix) instead of complex, multi-phase workflows. Each agent has single, well-defined responsibility. Separation of concerns keeps the workflow simple and predictable.

## Conventions Implemented/Respected

**REQUIRED SECTION**: All development practice documents MUST include this section to ensure traceability from practices to documentation standards.

This practice implements/respects the following conventions:

- **[Criticality Levels Convention](../quality/criticality-levels.md)**: Checker agents categorize findings by criticality (CRITICAL/HIGH/MEDIUM/LOW) to indicate importance/urgency. Fixer agents combine criticality with confidence to determine fix priority (P0-P4).

- **[Fixer Confidence Levels Convention](../quality/fixer-confidence-levels.md)**: Fixer agents assess confidence (HIGH/MEDIUM/FALSE_POSITIVE) for each finding. Only HIGH confidence fixes applied automatically. Criticality and confidence work orthogonally to determine priority.

- **[Temporary Files Convention](../infra/temporary-files.md)**: All checker agents MUST write validation/audit reports to `generated-reports/` directory using pattern `{agent-family}__{YYYY-MM-DD--HH-MM}__audit.md`. Fixer agents write fix reports to same directory with `__fix.md` suffix. Progressive writing requirement ensures audit history survives context compaction.

- **[Timestamp Format Convention](../../conventions/formatting/timestamp.md)**: Report filenames use UTC+7 timestamps in format `YYYY-MM-DD--HH-MM` (hyphen-separated for filesystem compatibility).

- **[Content Quality Principles](../../conventions/writing/quality.md)**: Checker agents validate content against quality standards (active voice, heading hierarchy, alt text, WCAG compliance). Fixer agents apply quality improvements when findings have HIGH confidence.

## Overview

### What is the Maker-Checker-Fixer Pattern?

The maker-checker-fixer pattern is a **quality control workflow** consisting of three specialized agent roles:

1. **Maker** - Creates or updates content comprehensively
2. **Checker** - Validates content against conventions and standards
3. **Fixer** - Applies validated fixes from checker audit reports

Each role is implemented as a separate agent with specific responsibilities and tool permissions, enabling a robust separation of concerns for content quality management.

### Why This Pattern Exists

**Without this pattern:**

- FAIL: Quality issues discovered after content creation
- FAIL: Manual validation is time-consuming and error-prone
- FAIL: No systematic remediation process
- FAIL: Inconsistent content quality across the repository

**With this pattern:**

- PASS: Systematic validation of all content
- PASS: Automated detection of convention violations
- PASS: Safe, validated fix application
- PASS: Iterative quality improvement
- PASS: Audit trail for all changes

### Scope

This pattern is used across **seven agent families**:

1. **repo-rules-\*** - Repository-wide consistency
2. **ayokoding-web-\*** - Hugo content for ayokoding-web
3. **docs-tutorial-\*** - Tutorial quality validation
4. **ose-platform-web-content-\*** - Hugo content for ose-platform-web
5. **readme-\*** - README quality standards
6. **docs-\*** - Documentation factual accuracy
7. **plan-\*** - Plan completeness and structure

## The Three Stages

### Stage 1: Maker (Comprehensive Content Management)

**Role**: Creates NEW content and updates EXISTING content with all dependencies

**Characteristics**:

- **User-driven operation** - Responds to user requests for content creation/modification
- **Comprehensive scope** - Creates target content AND updates all related files
- **Cascading changes** - Adjusts indices, cross-references, and dependencies
- **Proactive management** - Anticipates what needs updating beyond the immediate request

**Tool Pattern**: `Write`, `Edit` (content modification tools)

**Color**: 🟦 Blue (Writer agents) or 🟨 Yellow (wow\_\_rules-maker uses bash)

**Examples**:

| Agent                          | Creates/Updates                                    | Also Manages                                      | Tools Used            |
| ------------------------------ | -------------------------------------------------- | ------------------------------------------------- | --------------------- |
| wow\_\_rules-maker             | Convention docs, AGENTS.md sections, agent prompts | Cross-references, indices, related documentation  | Bash (not Edit/Write) |
| ayokoding-web-general-maker    | General Hugo learning content, blog posts          | Navigation files, overview pages, indices         | Write, Edit           |
| ayokoding-web-by-example-maker | By-example tutorials with annotated code           | 75-90 examples, diagrams, educational annotations | Write, Edit           |
| docs\_\_tutorial-maker         | Tutorial content with narrative flow               | Learning objectives, diagrams, code examples      | Write, Edit           |
| ose-platform-web-content-maker | Platform update posts, about pages                 | Navigation, asset references                      | Write, Edit           |
| readme\_\_maker                | README sections with engaging content              | Links to detailed docs, cross-references          | Write, Edit           |

**Note**: `repo-governance-maker` is a special case that uses bash commands (cat, sed, awk) instead of Edit/Write tools for file operations.

**Key Responsibilities**:

- PASS: Create new content from scratch
- PASS: Update existing content when requested
- PASS: Adjust ALL dependencies (indices, cross-refs, navigation)
- PASS: Follow all conventions during creation
- PASS: Provide complete, production-ready content

**When to Use**: User wants to **create or update content** (not validate or fix)

**Example Workflow**:

```markdown
User: "Add a new Hugo tutorial to ayokoding-web about TypeScript generics"

Maker Agent (ayokoding-web-general-maker):

1. Creates content/en/learn/swe/programming-languages/typescript/generics.md
2. Creates content/id/belajar/swe/programming-languages/typescript/generics.md (bilingual)
3. Updates content/en/learn/swe/programming-languages/typescript/\_index.md (navigation)
4. Updates content/id/belajar/swe/programming-languages/typescript/\_index.md (navigation)
5. Ensures overview.md/ikhtisar.md links are correct
6. Follows weight ordering convention (level-based)
7. Uses accessible colors in diagrams
8. Validates all internal links
9. Delivers complete, ready-to-publish content
```

### Stage 2: Checker (Validation)

**Role**: Validates content against conventions and generates audit reports

**Characteristics**:

- **Validation-driven** - Analyzes existing content for compliance
- **Non-destructive** - Does NOT modify files being checked
- **Comprehensive reporting** - Generates detailed audit reports in `generated-reports/`
- **Evidence-based** - Re-validates findings to prevent false positives (in fixer stage)

**Tool Pattern**: `Read`, `Glob`, `Grep`, `Write`, `Bash` (read-only + report generation)

- `Write` needed for audit report files
- `Bash` needed for UTC+7 timestamps in report filenames

**Color**: 🟩 Green (Checker agents)

**Examples**:

| Agent                            | Validates                                       | Generates Report                                                |
| -------------------------------- | ----------------------------------------------- | --------------------------------------------------------------- |
| wow\_\_rules-checker             | AGENTS.md, agents, conventions, documentation   | `repo-rules__{timestamp}__audit.md`                             |
| ayokoding-web-general-checker    | General Hugo content (frontmatter, links)       | `ayokoding-web__{timestamp}__audit.md`                          |
| ayokoding-web-by-example-checker | By-example tutorials (coverage, annotations)    | `ayokoding-web-by-example__{uuid-chain}__{timestamp}__audit.md` |
| docs\_\_tutorial-checker         | Tutorial pedagogy, narrative flow, visual aids  | `docs-tutorial__{timestamp}__audit.md`                          |
| ose-platform-web-content-checker | Platform content (structure, formatting, links) | `ose-platform-web__{timestamp}__audit.md`                       |
| readme\_\_checker                | README engagement, accessibility, jargon        | `readme__{timestamp}__audit.md`                                 |

**Key Responsibilities**:

- PASS: Validate content against conventions
- PASS: Generate audit reports with specific line numbers
- PASS: Categorize issues by criticality (CRITICAL/HIGH/MEDIUM/LOW)
- PASS: Provide actionable recommendations
- PASS: Do NOT modify files being checked

**Criticality Categorization** (see [Criticality Levels Convention](../quality/criticality-levels.md)):

Checkers categorize findings by **importance/urgency**:

- **CRITICAL** - Breaks functionality, blocks users (must fix before publication)
- **HIGH** - Significant quality degradation, convention violations (should fix)
- **MEDIUM** - Minor quality issues, style inconsistencies (fix when convenient)
- **LOW** - Suggestions, optional improvements (consider for future)

**Report Format**: Findings grouped by criticality in standardized sections with emoji indicators for accessibility.

**When to Use**: Need to **validate content quality** before publication or after maker changes

**Example Workflow**:

```markdown
User: "Check the new TypeScript tutorial for quality issues"

Checker Agent (ayokoding-web-general-checker):

1. Reads content/en/learn/swe/programming-languages/typescript/generics.md
2. Validates frontmatter (date format, required fields, weight ordering)
3. Checks content structure (heading hierarchy, link format)
4. Validates Hugo conventions (absolute paths, no .md extension)
5. Checks content quality (alt text, accessible colors, etc.)
6. Generates audit report: generated-reports/ayokoding-web**2025-12-14--20-45**audit.md
7. Reports findings in conversation (summary only)
8. Does NOT modify the tutorial file
```

### Stage 3: Fixer (Remediation)

**Role**: Applies validated fixes from checker audit reports

**Characteristics**:

- **Validation-driven** - Works from checker audit reports (not user requests)
- **Re-validation before fixing** - Confirms issues still exist (prevents false positives)
- **Confidence-based** - Only applies HIGH confidence fixes automatically
- **Safe application** - Skips MEDIUM (manual review) and FALSE_POSITIVE findings
- **Audit trail** - Generates fix reports for transparency

**Tool Pattern**: `Read`, `Edit`, `Glob`, `Grep`, `Write`, `Bash` (modification + report generation)

- `Edit` for applying fixes (NOT `Write` which creates new files)
- `Write` for fix report generation
- `Bash` for timestamps

**Color**: 🟪 Purple (Fixer agents) - Applies validated fixes

**Examples**:

| Agent                          | Fixes                                              | Generates Report                                              | Tools Used            |
| ------------------------------ | -------------------------------------------------- | ------------------------------------------------------------- | --------------------- |
| wow\_\_rules-fixer             | Convention violations from wow\_\_rules-checker    | `repo-rules__{timestamp}__fix.md`                             | Bash (not Edit/Write) |
| ayokoding-web-general-fixer    | General Hugo content issues from general-checker   | `ayokoding-web__{timestamp}__fix.md`                          | Edit, Write, Bash     |
| ayokoding-web-by-example-fixer | By-example tutorial issues from by-example-checker | `ayokoding-web-by-example__{uuid-chain}__{timestamp}__fix.md` | Edit, Write, Bash     |
| readme\_\_fixer                | README quality issues from readme\_\_checker       | `readme__{timestamp}__fix.md`                                 | Edit, Write, Bash     |

**Note**: `repo-governance-fixer` is a special case that uses bash commands (sed, awk, cat) instead of Edit/Write tools for file modifications. It still needs bash for report generation and timestamps.

**Key Responsibilities**:

- PASS: Read audit reports from checker agents
- PASS: Re-validate each finding before applying fix
- PASS: Apply HIGH confidence fixes automatically (priority-based)
- PASS: Skip MEDIUM confidence (needs manual review)
- PASS: Report FALSE_POSITIVE findings for checker improvement
- PASS: Generate comprehensive fix reports

**Priority-Based Execution** (see [Fixer Confidence Levels Convention - Integration](../quality/fixer-confidence-levels.md#integration-with-criticality-levels)):

Fixers combine **criticality** (importance) with **confidence** (certainty) to determine priority:

| Priority         | Criticality × Confidence         | Action                               |
| ---------------- | -------------------------------- | ------------------------------------ |
| **P0** (Blocker) | CRITICAL + HIGH                  | Auto-fix immediately, block if fails |
| **P1** (Urgent)  | HIGH + HIGH OR CRITICAL + MEDIUM | Auto-fix or urgent review            |
| **P2** (Normal)  | MEDIUM + HIGH OR HIGH + MEDIUM   | Auto-fix (if approved) or review     |
| **P3-P4** (Low)  | LOW combinations                 | Suggestions only                     |

**Execution Order**: P0 → P1 → P2 → P3-P4 ensures critical issues fixed before deployment proceeds.

**When to Use**: After checker identifies issues and user approves fixing them

**Example Workflow**:

```markdown
User: "Apply fixes from the latest ayokoding-web audit report"

Fixer Agent (ayokoding-web-general-fixer):

1. Auto-detects latest: generated-reports/ayokoding-web**2025-12-14--20-45**audit.md
2. Parses findings (25 issues found)
3. Re-validates each finding:
   - 18 findings → HIGH confidence (apply fixes)
   - 4 findings → MEDIUM confidence (skip, flag for manual review)
   - 3 findings → FALSE_POSITIVE (skip, report to improve checker)
4. Applies 18 fixes (missing fields, wrong values, format errors)
5. Generates fix report: generated-reports/ayokoding-web**2025-12-14--20-45**fix.md
6. Reports summary: 18 fixed, 4 manual review needed, 3 false positives detected
```

## Common Workflows

### Basic Workflow: Create → Validate → Fix

**Scenario**: Creating new content from scratch

```
1. User Request: "Create new tutorial about X"
   ↓
2. Maker: Creates content + all dependencies
   ↓
3. User: Reviews content, looks good
   ↓
4. Checker: Validates content, finds minor issues
   ↓
5. User: Reviews audit report, approves fixes
   ↓
6. Fixer: Applies validated fixes
   ↓
7. Done: Content is production-ready
```

**Example**:

```bash
# Step 1: Create content
User: "Create TypeScript generics tutorial for ayokoding-web"
Agent: ayokoding-web-general-maker (creates tutorial + navigation updates)

# Step 2: Validate
User: "Check the new tutorial"
Agent: ayokoding-web-general-checker (generates audit report)

# Step 3: Fix
User: "Apply the fixes"
Agent: ayokoding-web-general-fixer (applies validated fixes from audit)
```

### Iterative Workflow: Maker → Checker → Fixer → Checker

**Scenario**: Major content update requiring validation of fixes

```
1. User Request: "Update existing content X"
   ↓
2. Maker: Updates content + dependencies
   ↓
3. Checker: Validates, finds issues
   ↓
4. User: Reviews audit, approves fixes
   ↓
5. Fixer: Applies fixes
   ↓
6. Checker: Re-validates to confirm fixes worked
   ↓
7. Done: Content verified clean
```

**When to use**: Critical content, major refactoring, or when fixer confidence is uncertain

### Update Workflow: Maker (update mode) → Checker

**Scenario**: Updating existing content that's already high quality

```
1. User Request: "Add section Y to existing content X"
   ↓
2. Maker: Updates content (already validated during creation)
   ↓
3. Checker: Quick validation (optional, for confirmation)
   ↓
4. Done: High-quality content remains high-quality
```

**When to use**: Minor updates to well-maintained content

## Agent Categorization by Color

The maker-checker-fixer pattern aligns with the agent color categorization system:

| Color         | Role     | Stage   | Tool Pattern                    | Examples                                                                           |
| ------------- | -------- | ------- | ------------------------------- | ---------------------------------------------------------------------------------- |
| 🟦 **Blue**   | Writers  | Maker   | Has `Write` (creates new files) | ayokoding-web-general-maker, ayokoding-web-by-example-maker, readme\_\_maker       |
| 🟩 **Green**  | Checkers | Checker | Has `Write`, `Bash` (no `Edit`) | ayokoding-web-general-checker, ayokoding-web-by-example-checker, readme\_\_checker |
| 🟨 **Yellow** | Updaters | Fixer   | Has `Edit` (not `Write`)        | wow\_\_rules-fixer                                                                 |

**Note**: Purple (🟪 Implementors) agents execute plans and use all tools, falling outside the maker-checker-fixer pattern.

See [AI Agents Convention - Agent Color Categorization](../agents/ai-agents.md#agent-color-categorization) for complete details.

## The Five Agent Families

### 1. repo-rules-\* (Repository Consistency)

**Domain**: Repository-wide consistency across agents, conventions, AGENTS.md, and documentation

**Agents**:

- **wow\_\_rules-maker** (🟦 Maker) - Propagates rule changes across multiple files
- **wow\_\_rules-checker** (🟩 Checker) - Validates consistency, generates audit reports
- **wow\_\_rules-fixer** (🟨 Fixer) - Applies validated fixes from audit reports

**Use Case**: Maintaining consistency when adding/modifying conventions or standards

**Example**:

```
1. repo-governance-maker: Add new emoji usage rule to convention doc + update AGENTS.md + update agents
2. repo-governance-checker: Validate all files comply with new rule
3. repo-governance-fixer: Fix non-compliant files found in audit
```

### 2. ayokoding-web-\* (Hugo Content for ayokoding-web)

**Domain**: Hugo content for ayokoding-web (Hextra theme) - learning content, blog posts, by-example tutorials

**Agents**:

- **ayokoding-web-general-maker** (🟦 Maker) - Creates general Hugo content following conventions
- **ayokoding-web-by-example-maker** (🟦 Maker) - Creates by-example tutorials with annotated code
- **ayokoding-web-general-checker** (🟩 Checker) - Validates general Hugo frontmatter, links, quality
- **ayokoding-web-by-example-checker** (🟩 Checker) - Validates by-example tutorial quality (coverage, annotations)
- **ayokoding-web-general-fixer** (🟨 Fixer) - Fixes general Hugo content issues
- **ayokoding-web-by-example-fixer** (🟨 Fixer) - Fixes by-example tutorial issues

**Use Case**: Creating and validating educational content for ayokoding-web

**Example (General Content)**:

```
1. ayokoding-web-general-maker: Create TypeScript tutorial with bilingual content
2. ayokoding-web-general-checker: Validate frontmatter, links, navigation, weight ordering
3. ayokoding-web-general-fixer: Apply validated fixes from audit
```

**Example (By-Example Tutorial)**:

```
1. ayokoding-web-by-example-maker: Create Golang by-example with 75-90 annotated examples
2. ayokoding-web-by-example-checker: Validate 95% coverage, annotations, self-containment
3. ayokoding-web-by-example-fixer: Apply validated fixes from audit
```

### 3. docs-tutorial-\* (Tutorial Quality)

**Domain**: Tutorial pedagogy, narrative flow, visual completeness, hands-on elements

**Agents**:

- **docs\_\_tutorial-maker** (🟦 Maker) - Creates tutorials with narrative flow and scaffolding
- **docs\_\_tutorial-checker** (🟩 Checker) - Validates tutorial quality (pedagogy, visuals, exercises)

**Use Case**: Creating high-quality learning-oriented tutorials

**Example**:

```
1. docs__tutorial-maker: Create RAG tutorial with progressive scaffolding, diagrams, code examples
2. docs__tutorial-checker: Validate narrative flow, visual completeness, hands-on elements
```

**Note**: Tutorial fixes are typically manual (narrative quality requires human judgment)

### 4. ose-platform-web-content-\* (Hugo Content for ose-platform-web)

**Domain**: Hugo content for ose-platform-web (PaperMod theme) - platform updates, about pages

**Agents**:

- **ose-platform-web-content-maker** (🟦 Maker) - Creates platform content (updates, about)
- **ose-platform-web-content-checker** (🟩 Checker) - Validates content structure, formatting

**Use Case**: Creating and validating professional English content for platform landing page

**Example**:

```
1. ose-platform-web-content-maker: Create beta release announcement post
2. ose-platform-web-content-checker: Validate frontmatter, links, cover images
```

**Future**: ose-platform-web-content-fixer (🟨 Fixer)

### 5. readme-\* (README Quality)

**Domain**: README engagement, accessibility, scannability, jargon elimination

**Agents**:

- **readme\_\_maker** (🟦 Maker) - Creates README content following quality standards
- **readme\_\_checker** (🟩 Checker) - Validates engagement, accessibility, paragraph length

**Use Case**: Maintaining high-quality, welcoming README files

**Example**:

```
1. readme__maker: Add Security section with problem-solution hook
2. readme__checker: Validate paragraph length, jargon, acronym context
```

**Future**: readme\_\_fixer (🟨 Fixer)

### 6. docs-\* (Documentation Factual Accuracy)

**Domain**: Documentation factual correctness, technical accuracy, code examples, contradictions

**Agents**:

- **docs\_\_maker** (🟦 Maker) - Creates and edits documentation following conventions
- **docs\_\_checker** (🟩 Checker) - Validates factual accuracy using WebSearch/WebFetch
- **docs\_\_fixer** (🟨 Fixer) - Applies validated factual accuracy fixes

**Use Case**: Ensuring documentation is technically accurate and current

**Example**:

```
1. docs__maker: Create API documentation with code examples
2. docs__checker: Validate command syntax, version numbers, API methods against authoritative sources
3. docs__fixer: Fix incorrect command flags, update outdated versions, correct broken links
```

**Note**: docs\_\_fixer distinguishes objective factual errors (command syntax, version numbers - apply automatically) from subjective improvements (narrative quality, terminology - manual review)

### 7. plan-\* (Plan Completeness and Structure)

**Domain**: Project plan structure, completeness, codebase alignment, technical accuracy

**Agents**:

- **plan\_\_maker** (🟦 Maker) - Creates project planning documents
- **plan\_\_checker** (🟩 Checker) - Validates plan readiness for implementation
- **plan\_\_fixer** (🟨 Fixer) - Applies validated structural/format fixes

**Use Case**: Ensuring plans are complete and accurate before implementation

**Example**:

```
1. plan__maker: Create project plan with requirements, tech-docs, delivery checklist
2. plan__checker: Validate required sections exist, verify codebase assumptions, check technology choices
3. plan__fixer: Add missing sections, fix broken file references, correct format violations
```

**Note**: plan\_\_fixer distinguishes structural/format issues (missing sections, broken links - apply automatically) from strategic decisions (technology choices, scope, architecture - manual review)

## When to Use Each Stage

### When to Use Maker vs Fixer

**Use Maker when:**

- PASS: User explicitly requests content creation or updates
- PASS: Creating NEW content from scratch
- PASS: Making significant changes to EXISTING content
- PASS: Need comprehensive dependency management (indices, cross-refs)
- PASS: **User-driven workflow** (user says "create" or "update")

**Use Fixer when:**

- PASS: Checker has generated an audit report
- PASS: Issues are convention violations (not content gaps)
- PASS: Fixes are mechanical (field values, formatting, etc.)
- PASS: **Validation-driven workflow** (checker found issues)

**Example Distinction**:

```markdown
User: "Add a new tutorial about Docker" → Use MAKER (user-driven creation)
User: "Fix issues from the latest audit report" → Use FIXER (validation-driven fixes)
```

### When Checker is Optional vs Required

**Checker is OPTIONAL when:**

- Small, trivial updates (fixing typo, adding sentence)
- Content created by experienced maker (high confidence in quality)
- Time-sensitive changes (can validate later)

**Checker is REQUIRED when:**

- PASS: New content created from scratch
- PASS: Major refactoring or updates
- PASS: Before publishing to production
- PASS: Complex content (tutorials, Hugo content)
- PASS: Critical files (AGENTS.md, convention docs)

**Best Practice**: When in doubt, run the checker. Validation is fast and prevents issues.

### When to Skip Fixer (Manual Fixes Preferred)

**Skip Fixer when:**

- FAIL: Issues require human judgment (narrative quality, engagement)
- FAIL: Fixes are context-dependent (different solutions for different cases)
- FAIL: Checker reports are unclear or ambiguous
- FAIL: User prefers manual control over changes

**Use Fixer when:**

- PASS: Issues are mechanical (missing fields, wrong values)
- PASS: Fixes are unambiguous (clear right answer)
- PASS: Many repetitive fixes needed (efficiency gain)
- PASS: Audit report has HIGH confidence findings

**Example**:

```markdown
# Use Fixer (mechanical fixes)

- Missing frontmatter fields → Fixer
- Wrong date format → Fixer
- Broken internal links → Fixer

# Manual fixes (human judgment required)

- Paragraph too long → Manual (needs content restructuring)
- Engaging hook missing → Manual (creative writing)
- Jargon detected → Manual (context-dependent rewording)
```

## Benefits of the Pattern

### 1. Separation of Concerns

Each agent has a **single, clear responsibility**:

- Maker focuses on **content creation** (not validation)
- Checker focuses on **validation** (not fixing)
- Fixer focuses on **remediation** (not detection)

**Result**: Agents are simpler, more maintainable, and less error-prone.

### 2. Safety Through Validation

**Problem without pattern**: Automated fixes might introduce new issues or break existing content.

**Solution with pattern**: Fixer re-validates findings before applying changes, categorizes by confidence level, and skips uncertain fixes.

**Result**: Safe, reliable automated remediation.

### 3. Audit Trail

Every validation and fix is **documented in generated-reports/**:

- Audit reports show what was checked and what was found
- Fix reports show what was changed and why
- Users can review history and understand changes

**Result**: Transparency and accountability.

### 4. Iterative Improvement

**False Positive Feedback Loop**:

```
Checker: Flags issue (potential false positive)
   ↓
Fixer: Re-validates, detects FALSE_POSITIVE
   ↓
Fixer: Reports false positive with suggestion for checker improvement
   ↓
User: Updates checker logic based on feedback
   ↓
Checker: Improved accuracy in future runs
```

**Result**: Pattern enables continuous improvement of validation logic.

### 5. Scalability

Pattern scales across **multiple domains** without reinventing the workflow:

- Same pattern for repo rules, Hugo content, tutorials, READMEs
- Consistent user experience across all content types
- New families can adopt pattern easily

**Result**: Standardized quality control across entire repository.

## Integration with Conventions

The maker-checker-fixer pattern integrates with repository conventions:

| Convention                                                                  | How Pattern Uses It                                             |
| --------------------------------------------------------------------------- | --------------------------------------------------------------- |
| [AI Agents Convention](../agents/ai-agents.md)                              | Defines agent structure, tool permissions, color coding         |
| [Criticality Levels Convention](../quality/criticality-levels.md)           | Checkers categorize by criticality, fixers use for priority     |
| [Fixer Confidence Levels Convention](../quality/fixer-confidence-levels.md) | Fixers assess confidence, combine with criticality for priority |
| [Repository Validation Methodology](../quality/repository-validation.md)    | Standard validation patterns used by checker/fixer              |
| [Content Quality Principles](../../conventions/writing/quality.md)          | What checkers validate (quality standards)                      |
| [Hugo Content Convention](../../conventions/hugo/shared.md)                 | What ayokoding/ose-platform makers/checkers enforce             |
| [Tutorial Convention](../../conventions/tutorials/general.md)               | What docs\_\_tutorial-maker/checker enforce                     |
| [README Quality Convention](../../conventions/writing/readme-quality.md)    | What readme\_\_maker/checker enforce                            |
| [Temporary Files Convention](../infra/temporary-files.md)                   | Where checker/fixer reports are stored                          |

**Key Point**: The pattern is a **workflow framework**. The conventions define **what** to validate/enforce.

## Related Documentation

**Pattern Implementation**:

- [AI Agents Convention](../agents/ai-agents.md) - Agent structure and tool permissions
- [Repository Validation Methodology](../quality/repository-validation.md) - Validation patterns and techniques
- [Temporary Files Convention](../infra/temporary-files.md) - Report storage and naming

**Workflow Orchestration**:

- [Workflow Pattern Convention](../../workflows/meta/workflow-identifier.md) - How workflows orchestrate agents

**Domain-Specific Standards**:

- [Content Quality Principles](../../conventions/writing/quality.md) - Universal content standards
- [Hugo Content Convention - Shared](../../conventions/hugo/shared.md) - Hugo content standards
- [Hugo Content Convention - ayokoding](../../conventions/hugo/ayokoding.md) - ayokoding-web specifics
- [Hugo Content Convention - OSE Platform](../../conventions/hugo/ose-platform.md) - ose-platform-web specifics
- [Tutorial Convention](../../conventions/tutorials/general.md) - Tutorial quality standards
- [README Quality Convention](../../conventions/writing/readme-quality.md) - README standards

**Agent Examples**:

- `.opencode/agent/repo-governance-maker.md` - Example maker agent
- `.opencode/agent/repo-governance-checker.md` - Example checker agent
- `.opencode/agent/repo-governance-fixer.md` - Example fixer agent
- `.opencode/agent/apps-ayokoding-web-general-maker.md` - General Hugo content maker
- `.opencode/agent/ayokoding-web-by-example-maker.md` - By-example tutorial maker
- `.opencode/agent/apps-ayokoding-web-general-checker.md` - General Hugo content checker
- `.opencode/agent/ayokoding-web-by-example-checker.md` - By-example tutorial checker
- `.opencode/agent/ayokoding-web-general-fixer.md` - General Hugo content fixer
- `.opencode/agent/ayokoding-web-by-example-fixer.md` - By-example tutorial fixer

---

This pattern provides a **systematic, scalable, and safe approach** to content quality management across multiple domains. By separating creation, validation, and remediation into distinct stages, we achieve high-quality content through iterative improvement and automated safeguards.
