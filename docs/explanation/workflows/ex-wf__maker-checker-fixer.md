---
name: maker-checker-fixer
goal: Create or update content, validate quality, and apply validated fixes
termination: Content passes all quality checks or user approves partial state
inputs:
  - name: family
    type: enum
    values:
      [
        docs,
        ayokoding-content,
        ayokoding-facts,
        ayokoding-structure,
        ose-platform-web-content,
        readme,
        plan,
        repo-rules,
      ]
    description: Which content family to process
    required: true
  - name: scope
    type: string
    description: What to create/update (e.g., "all", "path/to/file.md", "changed")
    required: true
  - name: maker-args
    type: object
    description: Additional arguments for the maker agent
    required: false
outputs:
  - name: created-files
    type: file-list
    description: Files created or modified by the maker
  - name: audit-report
    type: file
    pattern: generated-reports/{family}__*__audit.md
    description: Quality validation report from checker
  - name: fixes-applied
    type: boolean
    description: Whether fixes were applied
  - name: final-status
    type: enum
    values: [pass, partial, fail]
    description: Final workflow status
---

# Maker-Checker-Fixer Workflow

**Purpose**: The canonical three-stage workflow for creating high-quality content with automated validation and fixing.

**When to use**: Anytime you create or update documentation, Hugo content, README files, plans, or repository rules where quality validation and automated fixes are beneficial.

## Background

This workflow implements the [Maker-Checker-Fixer Pattern](../development/ex-de__maker-checker-fixer-pattern.md) used across seven content families in this repository:

1. **docs** - Project documentation
2. **ayokoding-content** - ayokoding-web Hugo content
3. **ayokoding-facts** - Factual accuracy for ayokoding-web
4. **ayokoding-structure** - Navigation structure for ayokoding-web
5. **ose-platform-web-content** - ose-platform-web Hugo content
6. **readme** - README.md files
7. **plan** - Project planning documents
8. **repo-rules** - Repository governance (principles, conventions, development, agents)

Each family has dedicated agents: `{family}-maker`, `{family}-checker`, `{family}-fixer`.

## Steps

### 1. Create or Update Content (Sequential)

The maker agent creates new content or updates existing content based on the scope.

**Agent**: `{input.family}-maker`

- **Args**: `scope: {input.scope}, ...{input.maker-args}`
- **Output**: `{created-files}` - List of files created or modified

**Success criteria**: Content is created/updated successfully and follows basic structural requirements.

**On failure**: Terminate workflow with status `fail`. Report error to user with guidance on fixing the issue.

**Notes**:

- Makers focus on content creation, not perfection
- Validation happens in next step
- Different families have different maker behaviors (see family-specific documentation)

### 2. Validate Content Quality (Sequential)

The checker agent validates content against quality standards and generates an audit report.

**Agent**: `{input.family}-checker`

- **Args**: `scope: {step1.outputs.created-files}`
- **Output**: `{audit-report}` - Detailed audit in `generated-reports/`
- **Depends on**: Step 1 completion

**Success criteria**: Checker completes validation and generates audit report (even if issues found).

**On failure**: Terminate workflow with status `fail`. Checker should not fail unless technical errors occur (file access, parsing errors).

**Notes**:

- **MANDATORY**: All checkers MUST write reports to `generated-reports/` (no conversation-only output)
- **PROGRESSIVE WRITING**: Checkers must initialize report files at start and write findings progressively
- Reports follow pattern: `{family}__{YYYY-MM-DD--HH-MM}__audit.md`
- Checkers validate against conventions, principles, and family-specific standards
- All issues are categorized by confidence level (HIGH/MEDIUM/FALSE_POSITIVE)

### 3. User Review (Human Checkpoint)

User reviews the audit report and decides whether to proceed with fixes.

**Prompt**: "Checker found {issue-count} issues in {step2.outputs.audit-report}. Review the report. Should I apply fixes?"

**Options**:

- **Approve all fixes** → Proceed to step 4 with all fixes
- **Approve HIGH confidence only** → Proceed to step 4 with HIGH confidence fixes
- **Approve selective fixes** → User specifies which findings to fix
- **Reject all fixes** → Skip to step 5 (verification only)
- **Terminate workflow** → End with status `partial` (content created but not validated)

**Timeout**: None (workflow waits for user decision)

**Notes**:

- User should review audit report before approving fixes
- HIGH confidence fixes are objective (typos, broken links, missing required fields)
- MEDIUM confidence fixes may require judgment (style improvements, reorganization)
- FALSE_POSITIVE findings should not be fixed (checker error or subjective judgment)

### 4. Apply Validated Fixes (Conditional, Sequential)

The fixer agent applies approved fixes from the audit report.

**Agent**: `{input.family}-fixer`

- **Args**: `report: {step2.outputs.audit-report}, approved: {step3.user-selection}`
- **Output**: `{fixes-applied}` - Boolean indicating if fixes were applied
- **Condition**: `{step3.user-approved} == true` (any approval option except "Reject all")
- **Depends on**: Step 3 completion

**Success criteria**: Fixer successfully applies approved fixes without errors.

**On failure**: Report which fixes failed and why. Set `{fixes-applied} = false`. Continue to step 5 for verification.

**Notes**:

- Fixers use [Fixer Confidence Levels](../development/ex-de__fixer-confidence-levels.md) to assess fixes
- Fixers re-validate findings before applying (prevent false positives)
- HIGH confidence: Objective fixes (typos, formatting, broken links)
- MEDIUM confidence: Subjective improvements (rewording, reorganization)
- Fixers preserve original meaning and style (see [Content Preservation](../development/ex-de__content-preservation.md))
- Fixers skip FALSE_POSITIVE findings automatically

### 5. Verify Final Quality (Sequential)

The checker agent re-validates content to confirm all issues are resolved.

**Agent**: `{input.family}-checker`

- **Args**: `scope: {step1.outputs.created-files}, expect: zero-issues`
- **Output**: `{final-audit}` - Verification report
- **Depends on**: Step 4 completion (if fixes applied) or Step 3 (if fixes rejected)

**Success criteria**: Checker reports zero HIGH confidence issues.

**On failure**: Set status to `partial`. Report remaining issues to user.

**Notes**:

- Verification uses same checker as step 2 for consistency
- MEDIUM confidence issues remaining are acceptable (not blocking)
- If new issues appear, workflow status = `partial` (investigate why)
- User can manually fix remaining issues or re-run workflow

## Termination Criteria

- ✅ **Success** (`pass`): Step 5 verification reports zero HIGH confidence issues
- ⚠️ **Partial** (`partial`): Content created/updated but issues remain, OR user terminated at step 3, OR fixes applied but verification found new issues
- ❌ **Failure** (`fail`): Step 1 (maker) or step 2 (checker) failed with technical errors

## Example Usage

### Documentation Creation

```bash
# Create new tutorial and validate
workflow run maker-checker-fixer \
  --family=docs \
  --scope=docs/tutorials/tu__new-tutorial.md \
  --maker-args='{"topic": "Getting Started", "type": "beginner"}'
```

### Hugo Content Update

```bash
# Update ayokoding content and validate
workflow run maker-checker-fixer \
  --family=ayokoding-content \
  --scope=ayokoding-web/content/en/programming/
```

### README Quality Check

```bash
# Validate and fix README
workflow run maker-checker-fixer \
  --family=readme \
  --scope=README.md
```

### Full Repository Validation

```bash
# Validate all docs
workflow run maker-checker-fixer \
  --family=docs \
  --scope=all
```

## Workflow Variants

### Quick Validation (Skip Maker)

For existing content, skip step 1:

```bash
workflow run checker-fixer \
  --family=docs \
  --scope=docs/explanation/workflows/
```

This variant starts at step 2 (validation) for already-created content.

### Maker-Only (Skip Validation)

For rapid prototyping, create content without validation:

```bash
workflow run {family}-maker \
  --scope={target}
```

This is NOT a workflow - just direct agent invocation. Use for drafts only.

## Family-Specific Behaviors

### docs

- **Maker**: Creates tutorials, how-tos, references, explanations
- **Checker**: Validates factual correctness, links, content quality
- **Fixer**: Applies content quality fixes, corrects broken links

### ayokoding-content

- **Maker**: Creates Hugo content for ayokoding-web (Hextra theme)
- **Checker**: Validates Hugo conventions, bilingual consistency, navigation
- **Fixer**: Fixes frontmatter, weight system, navigation links

### ayokoding-facts

- **Maker**: N/A (no maker agent - validation only)
- **Checker**: Validates factual accuracy using web verification
- **Fixer**: Corrects factual errors, updates outdated information

### ayokoding-structure

- **Maker**: Adjusts weights and ordering in ayokoding-web
- **Checker**: Validates navigation structure, weight conventions, coverage
- **Fixer**: Corrects weight ordering, fixes structural issues

### ose-platform-web-content

- **Maker**: Creates Hugo content for ose-platform-web (PaperMod theme)
- **Checker**: Validates Hugo conventions, landing page standards
- **Fixer**: Fixes frontmatter, content structure

### readme

- **Maker**: Creates or updates README.md files
- **Checker**: Validates engagement, accessibility, scannability
- **Fixer**: Removes jargon, improves scannability, adds context

### plan

- **Maker**: Creates project planning documents
- **Checker**: Validates plan completeness, technical accuracy
- **Fixer**: Fixes plan structure, corrects technical errors

### repo-rules

- **Maker**: Creates or updates principles, conventions, development practices
- **Checker**: Validates consistency across CLAUDE.md, conventions, agents
- **Fixer**: Fixes contradictions, updates indices, syncs documentation

## Related Workflows

- [`ex-wf__full-docs-validation`](./ex-wf__full-docs-validation.md) - Comprehensive docs validation across all families
- [`ex-wf__content-creation`](./EXAMPLE-FORMAT.md) - Extended workflow including planning and deployment

## Success Metrics

Track these metrics across workflow executions:

- **Issue detection rate**: How many issues caught by checker
- **Fix success rate**: Percentage of fixes applied without errors
- **Verification pass rate**: How often step 5 reports zero issues
- **Confidence accuracy**: How often HIGH confidence fixes are correct
- **False positive rate**: How often FALSE_POSITIVE findings appear

## Notes

- This workflow is the **foundation** for quality assurance in this repository
- All seven families follow this exact pattern (consistency)
- Workflow respects all principles, conventions, and development practices
- Human approval (step 3) ensures quality and prevents unwanted changes
- Progressive writing in checkers ensures audit history survives context compaction
- Fixer confidence levels prevent false positives and preserve content intent

## Principles Respected

- ✅ **Explicit Over Implicit**: All steps, criteria, and decisions are explicit
- ✅ **Automation Over Manual**: Automates validation and fixing
- ✅ **Simplicity Over Complexity**: Three clear stages, linear flow
- ✅ **Accessibility First**: Human-readable reports, clear prompts
- ✅ **Progressive Disclosure**: Can run full workflow or individual steps
- ✅ **No Time Estimates**: Focus on quality outcomes, not duration
