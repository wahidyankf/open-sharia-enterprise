---
name: ayokoding-web-content-check-fix
goal: Validate all ayokoding-web content quality, apply fixes iteratively, then regenerate titles and navigation
termination: Zero findings across all validators and navigation regenerated
inputs:
  - name: scope
    type: string
    description: Content to validate (e.g., "all", "ayokoding-web/content/en/", "specific-file.md")
    required: false
    default: all
  - name: max-iterations
    type: number
    description: Maximum number of check-fix cycles to prevent infinite loops
    required: false
    default: 5
outputs:
  - name: final-status
    type: enum
    values: [pass, partial, fail]
    description: Final validation status
  - name: iterations-completed
    type: number
    description: Number of check-fix cycles executed
  - name: content-report
    type: file
    pattern: generated-reports/ayokoding-content__*__audit.md
    description: Final content validation report
  - name: facts-report
    type: file
    pattern: generated-reports/ayokoding-facts__*__audit.md
    description: Final facts validation report
  - name: structure-report
    type: file
    pattern: generated-reports/ayokoding-structure__*__audit.md
    description: Final structure validation report
  - name: links-report
    type: file
    pattern: generated-reports/ayokoding-link__*__audit.md
    description: Final links validation report
---

# AyoKoding Web Content Check-Fix Workflow

**Purpose**: Comprehensively validate all ayokoding-web content (Hugo conventions, factual accuracy, structure, links), apply fixes iteratively until all issues are resolved, then regenerate titles and navigation.

**When to use**:

- After creating or updating ayokoding-web content
- Before deploying ayokoding-web to production
- Periodically to ensure content quality and accuracy
- After bulk content changes or restructuring

## Steps

### 1. Parallel Validation (Parallel)

Run all ayokoding validators concurrently to identify all issues across different quality dimensions.

**Agent 1a**: `ayokoding-content-checker`

- **Args**: `scope: {input.scope}`
- **Output**: `{content-report-N}` - Hugo conventions, bilingual consistency, navigation validation

**Agent 1b**: `ayokoding-facts-checker`

- **Args**: `scope: {input.scope}`
- **Output**: `{facts-report-N}` - Factual accuracy, code examples, tutorial sequences

**Agent 1c**: `ayokoding-structure-checker`

- **Args**: `scope: {input.scope}`
- **Output**: `{structure-report-N}` - Weight conventions, ordering, navigation structure

**Agent 1d**: `ayokoding-link-checker`

- **Args**: `scope: {input.scope}`
- **Output**: `{links-report-N}` - Internal/external link validation, Hugo link format

**Success criteria**: All four checkers complete and generate audit reports.

**On failure**: Terminate workflow with status `fail`.

**Notes**:

- All checkers run in parallel for efficiency
- Each generates independent audit report in `generated-reports/`
- Reports use progressive writing to survive context compaction

### 2. Aggregate Findings (Sequential)

Analyze all audit reports to determine if fixes are needed.

**Condition Check**: Count ALL findings (HIGH, MEDIUM, and MINOR) across all four reports

- If total findings > 0: Proceed to step 3
- If total findings = 0: Skip to step 7 (Finalization)

**Depends on**: Step 1 completion

**Notes**:

- Considers ALL findings from all four validation dimensions
- Fixes everything: HIGH (objective), MEDIUM (structural), MINOR (style/formatting)
- Tracks findings by category for observability
- Achieves perfect content quality state

### 3. Apply Content Fixes (Sequential, Conditional)

Fix Hugo convention violations, frontmatter issues, and content quality problems.

**Agent**: `ayokoding-content-fixer`

- **Args**: `report: {step1.outputs.content-report-N}, approved: all`
- **Output**: `{content-fixes-applied}`
- **Condition**: Content findings exist from step 2
- **Depends on**: Step 2 completion

**Success criteria**: Fixer successfully applies content fixes without errors.

**On failure**: Log errors, continue to next fixer.

### 4. Apply Facts Fixes (Sequential, Conditional)

Fix factual errors, outdated information, and incorrect code examples.

**Agent**: `ayokoding-facts-fixer`

- **Args**: `report: {step1.outputs.facts-report-N}, approved: all`
- **Output**: `{facts-fixes-applied}`
- **Condition**: Facts findings exist from step 2
- **Depends on**: Step 3 completion

**Success criteria**: Fixer successfully applies factual fixes without errors.

**On failure**: Log errors, continue to next fixer.

**Notes**:

- Uses web verification to ensure accuracy
- Re-validates findings before applying
- Preserves educational content intent

### 5. Apply Structure Fixes (Sequential, Conditional)

Fix weight ordering, navigation structure, and coverage issues.

**Agent**: `ayokoding-structure-fixer`

- **Args**: `report: {step1.outputs.structure-report-N}, approved: all`
- **Output**: `{structure-fixes-applied}`
- **Condition**: Structure findings exist from step 2
- **Depends on**: Step 4 completion

**Success criteria**: Fixer successfully applies structure fixes without errors.

**On failure**: Log errors, proceed to re-validation.

**Notes**:

- Adjusts weights to maintain proper ordering
- Fixes navigation structure issues
- Does NOT regenerate navigation (that happens in step 8)

### 6. Iteration Control (Sequential)

Determine whether to continue fixing or move to finalization.

**Logic**:

- Re-run all checkers (step 1) to get fresh reports
- Count ALL findings (HIGH, MEDIUM, MINOR) across all new reports
- If findings = 0: Proceed to step 7 (Finalization)
- If findings > 0 AND iterations < max-iterations: Loop back to step 3 with new reports
- If findings > 0 AND iterations >= max-iterations: Proceed to step 7 with status `partial`

**Depends on**: Step 5 completion

**Notes**:

- Prevents infinite loops with max-iterations limit
- Continues until ZERO findings of any confidence level across all validators
- Each iteration gets fresh validation reports
- Tracks iteration count and finding trends

### 7. Regenerate Titles (Sequential)

Update title fields in all ayokoding-web markdown files based on filenames and configuration.

**Agent**: `ayokoding-title-maker`

- **Args**: `scope: {input.scope}`
- **Output**: `{titles-updated}`
- **Depends on**: Zero findings or max-iterations reached

**Success criteria**: All titles regenerated successfully.

**On failure**: Log errors, continue to navigation regeneration.

**Notes**:

- Runs after all fixes applied
- Updates titles based on filename conventions
- Handles language-specific title overrides

### 8. Regenerate Navigation (Sequential)

Regenerate 2-layer navigation listings in all \_index.md files from file structure.

**Agent**: `ayokoding-navigation-maker`

- **Args**: `scope: {input.scope}`
- **Output**: `{navigation-updated}`
- **Depends on**: Step 7 completion

**Success criteria**: All navigation listings regenerated successfully.

**On failure**: Terminate workflow with status `fail`.

**Notes**:

- Runs AFTER title regeneration (titles affect navigation)
- Automatically generates 2-layer navigation from file structure
- Ensures complete navigation coverage

### 9. Final Validation (Sequential)

Run all checkers one final time to confirm zero issues remain.

**Agents**: All four checkers in parallel

- ayokoding-content-checker
- ayokoding-facts-checker
- ayokoding-structure-checker
- ayokoding-link-checker

**Args**: `scope: {input.scope}, expect: zero-issues`

**Output**: Final audit reports for all dimensions

**Success criteria**: All checkers report zero issues of ANY confidence level.

**On failure**: Set status to `partial`.

**Depends on**: Step 8 completion

### 10. Finalization (Sequential)

Report final status and summary.

**Output**: `{final-status}`, `{iterations-completed}`, all final reports

**Status determination**:

- **Success** (`pass`): Zero findings after final validation
- **Partial** (`partial`): Findings remain after max-iterations OR final validation failed
- **Failure** (`fail`): Technical errors during check, fix, or finalization

**Depends on**: Step 9 completion

## Termination Criteria

- ✅ **Success** (`pass`): Zero findings of ANY confidence level (HIGH, MEDIUM, MINOR) across all validators after finalization
- ⚠️ **Partial** (`partial`): Any findings remain after max-iterations OR final validation found issues
- ❌ **Failure** (`fail`): Checkers, fixers, or finalization agents encountered technical errors

## Example Usage

### Full Content Check-Fix

```bash
# Run complete ayokoding-web content validation and fixing
workflow run ayokoding-web-content-check-fix
```

### Validate Specific Language

```bash
# Validate only English content
workflow run ayokoding-web-content-check-fix --scope=ayokoding-web/content/en/
```

### Validate Specific Section

```bash
# Validate only programming section
workflow run ayokoding-web-content-check-fix --scope=ayokoding-web/content/en/programming/
```

### Extended Validation

```bash
# Allow up to 10 iterations for complex content issues
workflow run ayokoding-web-content-check-fix --scope=all --max-iterations=10
```

### Quick Validation Only

```bash
# Check only, don't fix or regenerate (set max-iterations=0)
workflow run ayokoding-web-content-check-fix --scope=all --max-iterations=0
```

## Iteration Example

Typical execution flow:

```
Iteration 1:
  Parallel Check (4 validators) → 25 total findings
    - Content: 10 findings
    - Facts: 8 findings
    - Structure: 5 findings
    - Links: 2 findings
  Sequential Fix → Content → Facts → Structure
  Re-check → 8 findings remain

Iteration 2:
  Parallel Check → 8 findings
  Sequential Fix → Content → Facts → Structure
  Re-check → 0 findings

Finalization:
  Regenerate Titles → Success
  Regenerate Navigation → Success
  Final Validation → Zero issues

Result: SUCCESS (2 iterations)
```

## Safety Features

**Infinite Loop Prevention**:

- Max-iterations parameter (default: 5)
- Workflow terminates with `partial` if limit reached
- Tracks iteration count and finding trends

**False Positive Protection**:

- All fixers re-validate findings before applying
- Skips FALSE_POSITIVE findings automatically
- Progressive writing ensures audit history survives

**Error Recovery**:

- Continues to next fixer even if one fails
- Continues to finalization even if fixes partially fail
- Reports which fixes succeeded/failed
- Generates final reports regardless of status

**Comprehensive Coverage**:

- Four validation dimensions (content, facts, structure, links)
- Parallel validation for efficiency
- Sequential fixing for dependency management
- Post-fix regeneration for consistency

## Validation Dimensions

### Content Validation (ayokoding-content-checker)

- Hugo conventions (frontmatter, theme-specific)
- Bilingual consistency
- Navigation structure
- Content quality principles

### Facts Validation (ayokoding-facts-checker)

- Technical accuracy using web verification
- Code examples correctness
- Tutorial sequences validity
- Bilingual factual consistency

### Structure Validation (ayokoding-structure-checker)

- Weight conventions and ordering
- Navigation structure completeness
- Coverage gaps
- Pedagogical progression

### Links Validation (ayokoding-link-checker)

- Internal link validity
- External link accessibility
- Hugo link format compliance (absolute paths, no .md)
- Broken link detection

## Related Workflows

This workflow can be composed with:

- Deployment workflows (validate before deploying ayokoding-web)
- Content creation workflows (validate after bulk content creation)
- Translation workflows (validate bilingual consistency)

## Success Metrics

Track across executions:

- **Average iterations to completion**: How many cycles typically needed
- **Success rate**: Percentage reaching zero findings
- **Findings by dimension**: Which validators find most issues
- **Fix success rate**: Percentage of fixes applied without errors
- **Common issue categories**: What problems appear most frequently

## Notes

- **Fully automated**: No human checkpoints, runs to completion
- **Comprehensive**: Validates all quality dimensions
- **Parallel validation**: Efficient checking across dimensions
- **Sequential fixing**: Manages dependencies between fixers
- **Post-fix regeneration**: Ensures titles and navigation are current
- **Idempotent**: Safe to run multiple times
- **Observable**: Generates detailed audit reports for each dimension
- **Bounded**: Max-iterations prevents runaway execution

This workflow ensures comprehensive ayokoding-web content quality through multi-dimensional validation, iterative fixing, and automated regeneration of titles and navigation.

## Principles Respected

- ✅ **Explicit Over Implicit**: All steps, validators, fixers, and finalization are explicit
- ✅ **Automation Over Manual**: Fully automated validation, fixing, and regeneration
- ✅ **Simplicity Over Complexity**: Clear flow despite multiple validators
- ✅ **Accessibility First**: Generates human-readable audit reports
- ✅ **Progressive Disclosure**: Can run with different scopes and iteration limits
- ✅ **No Time Estimates**: Focus on quality outcomes, not duration
