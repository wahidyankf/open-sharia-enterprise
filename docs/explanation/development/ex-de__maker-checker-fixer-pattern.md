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

## Principles Respected

**REQUIRED SECTION**: All development practice documents MUST include this section to ensure traceability from practices back to foundational values.

This practice respects the following core principles:

- **[Automation Over Manual](../principles/software-engineering/ex-pr-se__automation-over-manual.md)**: Checker agents automatically validate content against conventions. Fixer agents apply validated fixes without manual intervention. Human effort focuses on content creation and subjective improvements, not mechanical validation.

- **[Simplicity Over Complexity](../principles/general/ex-pr-ge__simplicity-over-complexity.md)**: Three clear stages (make, check, fix) instead of complex, multi-phase workflows. Each agent has single, well-defined responsibility. Separation of concerns keeps the workflow simple and predictable.

## Conventions Implemented/Respected

**REQUIRED SECTION**: All development practice documents MUST include this section to ensure traceability from practices to documentation standards.

This practice implements/respects the following conventions:

- **[Temporary Files Convention](./ex-de__temporary-files.md)**: All checker agents MUST write validation/audit reports to `generated-reports/` directory using pattern `{agent-family}__{YYYY-MM-DD--HH-MM}__audit.md`. Fixer agents write fix reports to same directory with `__fix.md` suffix. Progressive writing requirement ensures audit history survives context compaction.

- **[Timestamp Format Convention](../conventions/ex-co__timestamp-format.md)**: Report filenames use UTC+7 timestamps in format `YYYY-MM-DD--HH-MM` (hyphen-separated for filesystem compatibility).

- **[Content Quality Principles](../conventions/ex-co__content-quality.md)**: Checker agents validate content against quality standards (active voice, heading hierarchy, alt text, WCAG compliance). Fixer agents apply quality improvements when findings have HIGH confidence.

## 📋 Overview

### What is the Maker-Checker-Fixer Pattern?

The maker-checker-fixer pattern is a **quality control workflow** consisting of three specialized agent roles:

1. **Maker** - Creates or updates content comprehensively
2. **Checker** - Validates content against conventions and standards
3. **Fixer** - Applies validated fixes from checker audit reports

Each role is implemented as a separate agent with specific responsibilities and tool permissions, enabling a robust separation of concerns for content quality management.

### Why This Pattern Exists

**Without this pattern:**

- ❌ Quality issues discovered after content creation
- ❌ Manual validation is time-consuming and error-prone
- ❌ No systematic remediation process
- ❌ Inconsistent content quality across the repository

**With this pattern:**

- ✅ Systematic validation of all content
- ✅ Automated detection of convention violations
- ✅ Safe, validated fix application
- ✅ Iterative quality improvement
- ✅ Audit trail for all changes

### Scope

This pattern is used across **seven agent families**:

1. **repo-rules-\*** - Repository-wide consistency
2. **ayokoding-content-\*** - Hugo content for ayokoding-web
3. **docs-tutorial-\*** - Tutorial quality validation
4. **ose-platform-web-content-\*** - Hugo content for ose-platform-web
5. **readme-\*** - README quality standards
6. **docs-\*** - Documentation factual accuracy
7. **plan-\*** - Plan completeness and structure

## 🎯 The Three Stages

### Stage 1: Maker (Comprehensive Content Management)

**Role**: Creates NEW content and updates EXISTING content with all dependencies

**Characteristics**:

- **User-driven operation** - Responds to user requests for content creation/modification
- **Comprehensive scope** - Creates target content AND updates all related files
- **Cascading changes** - Adjusts indices, cross-references, and dependencies
- **Proactive management** - Anticipates what needs updating beyond the immediate request

**Tool Pattern**: `Write`, `Edit` (content modification tools)

**Color**: 🟦 Blue (Writer agents)

**Examples**:

| Agent                              | Creates/Updates                                    | Also Manages                                      |
| ---------------------------------- | -------------------------------------------------- | ------------------------------------------------- |
| repo-rules-maker                   | Convention docs, CLAUDE.md sections, agent prompts | Cross-references, indices, related documentation  |
| ayokoding-content-general-maker    | General Hugo learning content, blog posts          | Navigation files, overview pages, indices         |
| ayokoding-content-by-example-maker | By-example tutorials with annotated code           | 75-90 examples, diagrams, educational annotations |
| docs-tutorial-maker                | Tutorial content with narrative flow               | Learning objectives, diagrams, code examples      |
| ose-platform-web-content-maker     | Platform update posts, about pages                 | Navigation, asset references                      |
| readme-maker                       | README sections with engaging content              | Links to detailed docs, cross-references          |

**Key Responsibilities**:

- ✅ Create new content from scratch
- ✅ Update existing content when requested
- ✅ Adjust ALL dependencies (indices, cross-refs, navigation)
- ✅ Follow all conventions during creation
- ✅ Provide complete, production-ready content

**When to Use**: User wants to **create or update content** (not validate or fix)

**Example Workflow**:

```markdown
User: "Add a new Hugo tutorial to ayokoding-web about TypeScript generics"

Maker Agent (ayokoding-content-general-maker):

1. Creates content/en/learn/swe/prog-lang/typescript/generics.md
2. Creates content/id/belajar/swe/prog-lang/typescript/generics.md (bilingual)
3. Updates content/en/learn/swe/prog-lang/typescript/\_index.md (navigation)
4. Updates content/id/belajar/swe/prog-lang/typescript/\_index.md (navigation)
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

| Agent                                | Validates                                       | Generates Report                            |
| ------------------------------------ | ----------------------------------------------- | ------------------------------------------- |
| repo-rules-checker                   | CLAUDE.md, agents, conventions, documentation   | `repo-rules__{timestamp}__audit.md`         |
| ayokoding-content-general-checker    | General Hugo content (frontmatter, links)       | `ayokoding-content__{timestamp}__audit.md`  |
| ayokoding-content-by-example-checker | By-example tutorials (coverage, annotations)    | `by-example-checker__{timestamp}__audit.md` |
| docs-tutorial-checker                | Tutorial pedagogy, narrative flow, visual aids  | `docs-tutorial__{timestamp}__audit.md`      |
| ose-platform-web-content-checker     | Platform content (structure, formatting, links) | `ose-platform-web__{timestamp}__audit.md`   |
| readme-checker                       | README engagement, accessibility, jargon        | `readme__{timestamp}__audit.md`             |

**Key Responsibilities**:

- ✅ Validate content against conventions
- ✅ Generate audit reports with specific line numbers
- ✅ Categorize issues (Critical/Important/Minor)
- ✅ Provide actionable recommendations
- ✅ Do NOT modify files being checked

**When to Use**: Need to **validate content quality** before publication or after maker changes

**Example Workflow**:

```markdown
User: "Check the new TypeScript tutorial for quality issues"

Checker Agent (ayokoding-content-general-checker):

1. Reads content/en/learn/swe/prog-lang/typescript/generics.md
2. Validates frontmatter (date format, required fields, weight ordering)
3. Checks content structure (heading hierarchy, link format)
4. Validates Hugo conventions (absolute paths, no .md extension)
5. Checks content quality (alt text, accessible colors, etc.)
6. Generates audit report: generated-reports/ayokoding-content**2025-12-14--20-45**audit.md
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

**Color**: 🟨 Yellow (Updater agents) - Modifies existing content

**Examples**:

| Agent                              | Fixes                                              | Generates Report                         |
| ---------------------------------- | -------------------------------------------------- | ---------------------------------------- |
| repo-rules-fixer                   | Convention violations from repo-rules-checker      | `repo-rules__{timestamp}__fix.md`        |
| ayokoding-content-general-fixer    | General Hugo content issues from general-checker   | `ayokoding-content__{timestamp}__fix.md` |
| ayokoding-content-by-example-fixer | By-example tutorial issues from by-example-checker | `by-example-fixer__{timestamp}__fix.md`  |
| readme-fixer                       | README quality issues from readme-checker          | `readme__{timestamp}__fix.md`            |

**Key Responsibilities**:

- ✅ Read audit reports from checker agents
- ✅ Re-validate each finding before applying fix
- ✅ Apply HIGH confidence fixes automatically
- ✅ Skip MEDIUM confidence (needs manual review)
- ✅ Report FALSE_POSITIVE findings for checker improvement
- ✅ Generate comprehensive fix reports

**When to Use**: After checker identifies issues and user approves fixing them

**Example Workflow**:

```markdown
User: "Apply fixes from the latest ayokoding-content audit report"

Fixer Agent (ayokoding-content-general-fixer):

1. Auto-detects latest: generated-reports/ayokoding-content**2025-12-14--20-45**audit.md
2. Parses findings (25 issues found)
3. Re-validates each finding:
   - 18 findings → HIGH confidence (apply fixes)
   - 4 findings → MEDIUM confidence (skip, flag for manual review)
   - 3 findings → FALSE_POSITIVE (skip, report to improve checker)
4. Applies 18 fixes (missing fields, wrong values, format errors)
5. Generates fix report: generated-reports/ayokoding-content**2025-12-14--20-45**fix.md
6. Reports summary: 18 fixed, 4 manual review needed, 3 false positives detected
```

## 🔄 Common Workflows

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
Agent: ayokoding-content-general-maker (creates tutorial + navigation updates)

# Step 2: Validate
User: "Check the new tutorial"
Agent: ayokoding-content-general-checker (generates audit report)

# Step 3: Fix
User: "Apply the fixes"
Agent: ayokoding-content-general-fixer (applies validated fixes from audit)
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

## 🎨 Agent Categorization by Color

The maker-checker-fixer pattern aligns with the agent color categorization system:

| Color         | Role     | Stage   | Tool Pattern                    | Examples                                                                                |
| ------------- | -------- | ------- | ------------------------------- | --------------------------------------------------------------------------------------- |
| 🟦 **Blue**   | Writers  | Maker   | Has `Write` (creates new files) | ayokoding-content-general-maker, ayokoding-content-by-example-maker, readme-maker       |
| 🟩 **Green**  | Checkers | Checker | Has `Write`, `Bash` (no `Edit`) | ayokoding-content-general-checker, ayokoding-content-by-example-checker, readme-checker |
| 🟨 **Yellow** | Updaters | Fixer   | Has `Edit` (not `Write`)        | repo-rules-fixer                                                                        |

**Note**: Purple (🟪 Implementors) agents execute plans and use all tools, falling outside the maker-checker-fixer pattern.

See [AI Agents Convention - Agent Color Categorization](./ex-de__ai-agents.md#agent-color-categorization) for complete details.

## 📊 The Five Agent Families

### 1. repo-rules-\* (Repository Consistency)

**Domain**: Repository-wide consistency across agents, conventions, CLAUDE.md, and documentation

**Agents**:

- **repo-rules-maker** (🟦 Maker) - Propagates rule changes across multiple files
- **repo-rules-checker** (🟩 Checker) - Validates consistency, generates audit reports
- **repo-rules-fixer** (🟨 Fixer) - Applies validated fixes from audit reports

**Use Case**: Maintaining consistency when adding/modifying conventions or standards

**Example**:

```
1. repo-rules-maker: Add new emoji usage rule to convention doc + update CLAUDE.md + update agents
2. repo-rules-checker: Validate all files comply with new rule
3. repo-rules-fixer: Fix non-compliant files found in audit
```

### 2. ayokoding-content-\* (Hugo Content for ayokoding-web)

**Domain**: Hugo content for ayokoding-web (Hextra theme) - learning content, blog posts, by-example tutorials

**Agents**:

- **ayokoding-content-general-maker** (🟦 Maker) - Creates general Hugo content following conventions
- **ayokoding-content-by-example-maker** (🟦 Maker) - Creates by-example tutorials with annotated code
- **ayokoding-content-general-checker** (🟩 Checker) - Validates general Hugo frontmatter, links, quality
- **ayokoding-content-by-example-checker** (🟩 Checker) - Validates by-example tutorial quality (coverage, annotations)
- **ayokoding-content-general-fixer** (🟨 Fixer) - Fixes general Hugo content issues
- **ayokoding-content-by-example-fixer** (🟨 Fixer) - Fixes by-example tutorial issues

**Use Case**: Creating and validating educational content for ayokoding-web

**Example (General Content)**:

```
1. ayokoding-content-general-maker: Create TypeScript tutorial with bilingual content
2. ayokoding-content-general-checker: Validate frontmatter, links, navigation, weight ordering
3. ayokoding-content-general-fixer: Apply validated fixes from audit
```

**Example (By-Example Tutorial)**:

```
1. ayokoding-content-by-example-maker: Create Golang by-example with 75-90 annotated examples
2. ayokoding-content-by-example-checker: Validate 95% coverage, annotations, self-containment
3. ayokoding-content-by-example-fixer: Apply validated fixes from audit
```

### 3. docs-tutorial-\* (Tutorial Quality)

**Domain**: Tutorial pedagogy, narrative flow, visual completeness, hands-on elements

**Agents**:

- **docs-tutorial-maker** (🟦 Maker) - Creates tutorials with narrative flow and scaffolding
- **docs-tutorial-checker** (🟩 Checker) - Validates tutorial quality (pedagogy, visuals, exercises)

**Use Case**: Creating high-quality learning-oriented tutorials

**Example**:

```
1. docs-tutorial-maker: Create RAG tutorial with progressive scaffolding, diagrams, code examples
2. docs-tutorial-checker: Validate narrative flow, visual completeness, hands-on elements
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

- **readme-maker** (🟦 Maker) - Creates README content following quality standards
- **readme-checker** (🟩 Checker) - Validates engagement, accessibility, paragraph length

**Use Case**: Maintaining high-quality, welcoming README files

**Example**:

```
1. readme-maker: Add Security section with problem-solution hook
2. readme-checker: Validate paragraph length, jargon, acronym context
```

**Future**: readme-fixer (🟨 Fixer)

### 6. docs-\* (Documentation Factual Accuracy)

**Domain**: Documentation factual correctness, technical accuracy, code examples, contradictions

**Agents**:

- **docs-maker** (🟦 Maker) - Creates and edits documentation following conventions
- **docs-checker** (🟩 Checker) - Validates factual accuracy using WebSearch/WebFetch
- **docs-fixer** (🟨 Fixer) - Applies validated factual accuracy fixes

**Use Case**: Ensuring documentation is technically accurate and current

**Example**:

```
1. docs-maker: Create API documentation with code examples
2. docs-checker: Validate command syntax, version numbers, API methods against authoritative sources
3. docs-fixer: Fix incorrect command flags, update outdated versions, correct broken links
```

**Note**: docs-fixer distinguishes objective factual errors (command syntax, version numbers - apply automatically) from subjective improvements (narrative quality, terminology - manual review)

### 7. plan-\* (Plan Completeness and Structure)

**Domain**: Project plan structure, completeness, codebase alignment, technical accuracy

**Agents**:

- **plan-maker** (🟦 Maker) - Creates project planning documents
- **plan-checker** (🟩 Checker) - Validates plan readiness for implementation
- **plan-fixer** (🟨 Fixer) - Applies validated structural/format fixes

**Use Case**: Ensuring plans are complete and accurate before implementation

**Example**:

```
1. plan-maker: Create project plan with requirements, tech-docs, delivery checklist
2. plan-checker: Validate required sections exist, verify codebase assumptions, check technology choices
3. plan-fixer: Add missing sections, fix broken file references, correct format violations
```

**Note**: plan-fixer distinguishes structural/format issues (missing sections, broken links - apply automatically) from strategic decisions (technology choices, scope, architecture - manual review)

## 🔍 When to Use Each Stage

### When to Use Maker vs Fixer

**Use Maker when:**

- ✅ User explicitly requests content creation or updates
- ✅ Creating NEW content from scratch
- ✅ Making significant changes to EXISTING content
- ✅ Need comprehensive dependency management (indices, cross-refs)
- ✅ **User-driven workflow** (user says "create" or "update")

**Use Fixer when:**

- ✅ Checker has generated an audit report
- ✅ Issues are convention violations (not content gaps)
- ✅ Fixes are mechanical (field values, formatting, etc.)
- ✅ **Validation-driven workflow** (checker found issues)

**Example Distinction**:

```markdown
User: "Add a new tutorial about Docker" → Use MAKER (user-driven creation)
User: "Fix issues from the latest audit report" → Use FIXER (validation-driven fixes)
```

### When Checker is Optional vs Required

**Checker is OPTIONAL when:**

- ⚠️ Small, trivial updates (fixing typo, adding sentence)
- ⚠️ Content created by experienced maker (high confidence in quality)
- ⚠️ Time-sensitive changes (can validate later)

**Checker is REQUIRED when:**

- ✅ New content created from scratch
- ✅ Major refactoring or updates
- ✅ Before publishing to production
- ✅ Complex content (tutorials, Hugo content)
- ✅ Critical files (CLAUDE.md, convention docs)

**Best Practice**: When in doubt, run the checker. Validation is fast and prevents issues.

### When to Skip Fixer (Manual Fixes Preferred)

**Skip Fixer when:**

- ❌ Issues require human judgment (narrative quality, engagement)
- ❌ Fixes are context-dependent (different solutions for different cases)
- ❌ Checker reports are unclear or ambiguous
- ❌ User prefers manual control over changes

**Use Fixer when:**

- ✅ Issues are mechanical (missing fields, wrong values)
- ✅ Fixes are unambiguous (clear right answer)
- ✅ Many repetitive fixes needed (efficiency gain)
- ✅ Audit report has HIGH confidence findings

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

## 💡 Benefits of the Pattern

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

## 🔗 Integration with Conventions

The maker-checker-fixer pattern integrates with repository conventions:

| Convention                                                              | How Pattern Uses It                                     |
| ----------------------------------------------------------------------- | ------------------------------------------------------- |
| [AI Agents Convention](./ex-de__ai-agents.md)                           | Defines agent structure, tool permissions, color coding |
| [Repository Validation Methodology](./ex-de__repository-validation.md)  | Standard validation patterns used by checker/fixer      |
| [Content Quality Principles](../conventions/ex-co__content-quality.md)  | What checkers validate (quality standards)              |
| [Hugo Content Convention](../conventions/ex-co__hugo-content-shared.md) | What ayokoding/ose-platform makers/checkers enforce     |
| [Tutorial Convention](../conventions/ex-co__tutorials.md)               | What docs-tutorial-maker/checker enforce                |
| [README Quality Convention](../conventions/ex-co__readme-quality.md)    | What readme-maker/checker enforce                       |
| [Temporary Files Convention](./ex-de__temporary-files.md)               | Where checker/fixer reports are stored                  |

**Key Point**: The pattern is a **workflow framework**. The conventions define **what** to validate/enforce.

## 📚 Related Documentation

**Pattern Implementation**:

- [AI Agents Convention](./ex-de__ai-agents.md) - Agent structure and tool permissions
- [Repository Validation Methodology](./ex-de__repository-validation.md) - Validation patterns and techniques
- [Temporary Files Convention](./ex-de__temporary-files.md) - Report storage and naming

**Workflow Orchestration**:

- [Workflow Pattern Convention](../workflows/ex-wf__workflow-pattern.md) - How workflows orchestrate agents

**Domain-Specific Standards**:

- [Content Quality Principles](../conventions/ex-co__content-quality.md) - Universal content standards
- [Hugo Content Convention - Shared](../conventions/ex-co__hugo-content-shared.md) - Hugo content standards
- [Hugo Content Convention - ayokoding](../conventions/ex-co__hugo-content-ayokoding.md) - ayokoding-web specifics
- [Hugo Content Convention - OSE Platform](../conventions/ex-co__hugo-content-ose-platform.md) - ose-platform-web specifics
- [Tutorial Convention](../conventions/ex-co__tutorials.md) - Tutorial quality standards
- [README Quality Convention](../conventions/ex-co__readme-quality.md) - README standards

**Agent Examples**:

- `.claude/agents/repo-rules-maker.md` - Example maker agent
- `.claude/agents/repo-rules-checker.md` - Example checker agent
- `.claude/agents/repo-rules-fixer.md` - Example fixer agent
- `.claude/agents/ayokoding-content-general-maker.md` - General Hugo content maker
- `.claude/agents/ayokoding-content-by-example-maker.md` - By-example tutorial maker
- `.claude/agents/ayokoding-content-general-checker.md` - General Hugo content checker
- `.claude/agents/ayokoding-content-by-example-checker.md` - By-example tutorial checker
- `.claude/agents/ayokoding-content-general-fixer.md` - General Hugo content fixer
- `.claude/agents/ayokoding-content-by-example-fixer.md` - By-example tutorial fixer

---

This pattern provides a **systematic, scalable, and safe approach** to content quality management across multiple domains. By separating creation, validation, and remediation into distinct stages, we achieve high-quality content through iterative improvement and automated safeguards.
