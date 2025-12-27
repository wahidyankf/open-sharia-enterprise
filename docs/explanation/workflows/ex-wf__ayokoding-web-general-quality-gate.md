---
name: ayokoding-web-general-quality-gate
goal: Validate all ayokoding-web content quality, apply fixes iteratively until zero findings, then regenerate titles and navigation
termination: Zero findings across all validators and navigation regenerated (runs indefinitely until achieved unless max-iterations provided)
inputs:
  - name: scope
    type: string
    description: Content to validate (e.g., "all", "ayokoding-web/content/en/", "specific-file.md")
    required: false
    default: all
  - name: mode
    type: enum
    values: [lax, normal, strict, ultra]
    description: Quality threshold (lax: CRITICAL only, normal: CRITICAL/HIGH, strict: +MEDIUM, ultra: all levels)
    required: false
    default: normal
  - name: min-iterations
    type: number
    description: Minimum check-fix cycles before allowing zero-finding termination (prevents premature success)
    required: false
  - name: max-iterations
    type: number
    description: Maximum check-fix cycles to prevent infinite loops (if not provided, runs until zero findings)
    required: false
  - name: max-concurrency
    type: number
    description: Maximum number of agents/tasks that can run concurrently during workflow execution
    required: false
    default: 2
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
    pattern: generated-reports/ayokoding-web-general__*__audit.md
    description: Final content validation report
  - name: facts-report
    type: file
    pattern: generated-reports/ayokoding-web-facts__*__audit.md
    description: Final facts validation report
  - name: structure-report
    type: file
    pattern: generated-reports/ayokoding-web-structure__*__audit.md
    description: Final structure validation report
  - name: links-report
    type: file
    pattern: generated-reports/ayokoding-web-link__*__audit.md
    description: Final links validation report
---

# AyoKoding Content General Quality Gate Workflow

**Purpose**: Comprehensively validate all ayokoding-web content (Hugo conventions, factual accuracy, structure, links), apply fixes iteratively until all issues are resolved, then regenerate titles and navigation.

**When to use**:

- After creating or updating ayokoding-web content
- Before deploying ayokoding-web to production
- Periodically to ensure content quality and accuracy
- After bulk content changes or restructuring

## Steps

### 1. Parallel Validation (Parallel)

Run all ayokoding validators concurrently to identify all issues across different quality dimensions.

**Agent 1a**: `apps__ayokoding-web__general-checker`

- **Args**: `scope: {input.scope}`
- **Output**: `{content-report-N}` - Hugo conventions, bilingual consistency, navigation validation

**Agent 1b**: `apps__ayokoding-web__facts-checker`

- **Args**: `scope: {input.scope}`
- **Output**: `{facts-report-N}` - Factual accuracy, code examples, tutorial sequences

**Agent 1c**: `apps__ayokoding-web__structure-checker`

- **Args**: `scope: {input.scope}`
- **Output**: `{structure-report-N}` - Weight conventions, ordering, navigation structure

**Agent 1d**: `apps__ayokoding-web__link-checker`

- **Args**: `scope: {input.scope}`
- **Output**: `{links-report-N}` - Internal/external link validation, Hugo link format

**Success criteria**: All four checkers complete and generate audit reports.

**On failure**: Terminate workflow with status `fail`.

**Notes**:

- All checkers run in parallel for efficiency (max concurrency: {input.max-concurrency})
- Default parallelization: 2 concurrent agents, can be increased for faster validation
- Recommend setting to 4 for this workflow (matches number of checkers)
- Each generates independent audit report in `generated-reports/`
- Reports use progressive writing to survive context compaction

### 2. Aggregate Findings (Sequential)

Analyze all audit reports to determine if fixes are needed.

**Condition Check**: Count findings based on mode level across all four reports

- **lax**: Count CRITICAL only (aggregate across all 4 validators)
- **normal**: Count CRITICAL + HIGH (aggregate across all 4 validators)
- **strict**: Count CRITICAL + HIGH + MEDIUM (aggregate across all 4 validators)
- **ultra**: Count all levels (CRITICAL, HIGH, MEDIUM, LOW) (aggregate across all 4 validators)

**Below-threshold findings**: Report but don't block success

- **lax**: HIGH/MEDIUM/LOW reported, not counted
- **normal**: MEDIUM/LOW reported, not counted
- **strict**: LOW reported, not counted
- **ultra**: All findings counted

**Decision**:

- If threshold-level findings > 0: Proceed to step 3
- If threshold-level findings = 0: Skip to step 7 (Finalization)

**Depends on**: Step 1 completion

**Notes**:

- Aggregates findings across all four validation dimensions (content, facts, structure, links)
- Fix scope determined by mode level
- Below-threshold findings remain visible in audit reports
- Enables progressive quality improvement

### 3. Apply Content Fixes (Sequential, Conditional)

Fix Hugo convention violations, frontmatter issues, and content quality problems.

**Agent**: `apps__ayokoding-web__general-fixer`

- **Args**: `report: {step1.outputs.content-report-N}, approved: all, mode: {input.mode}`
- **Output**: `{content-fixes-applied}`
- **Condition**: Content findings exist from step 2
- **Depends on**: Step 2 completion

**Success criteria**: Fixer successfully applies content fixes without errors.

**On failure**: Log errors, continue to next fixer.

**Notes**:

- **Fix scope based on mode**:
  - **lax**: Fix CRITICAL only (skip HIGH/MEDIUM/LOW)
  - **normal**: Fix CRITICAL + HIGH (skip MEDIUM/LOW)
  - **strict**: Fix CRITICAL + HIGH + MEDIUM (skip LOW)
  - **ultra**: Fix all levels (CRITICAL, HIGH, MEDIUM, LOW)
- Below-threshold findings remain untouched

### 4. Apply Facts Fixes (Sequential, Conditional)

Fix factual errors, outdated information, and incorrect code examples.

**Agent**: `apps__ayokoding-web__facts-fixer`

- **Args**: `report: {step1.outputs.facts-report-N}, approved: all, mode: {input.mode}`
- **Output**: `{facts-fixes-applied}`
- **Condition**: Facts findings exist from step 2
- **Depends on**: Step 3 completion

**Success criteria**: Fixer successfully applies factual fixes without errors.

**On failure**: Log errors, continue to next fixer.

**Notes**:

- Uses web verification to ensure accuracy
- Re-validates findings before applying
- Preserves educational content intent
- Fix scope respects mode level (same as step 3)

### 5. Apply Structure Fixes (Sequential, Conditional)

Fix weight ordering, navigation structure, and coverage issues.

**Agent**: `apps__ayokoding-web__structure-fixer`

- **Args**: `report: {step1.outputs.structure-report-N}, approved: all, mode: {input.mode}`
- **Output**: `{structure-fixes-applied}`
- **Condition**: Structure findings exist from step 2
- **Depends on**: Step 4 completion

**Success criteria**: Fixer successfully applies structure fixes without errors.

**On failure**: Log errors, proceed to re-validation.

**Notes**:

- Adjusts weights to maintain proper ordering
- Fixes navigation structure issues
- Does NOT regenerate navigation (that happens in step 8)
- Fix scope respects mode level (same as step 3)

### 6. Iteration Control (Sequential)

Determine whether to continue fixing or move to finalization.

**Logic**:

- Re-run all checkers (step 1) to get fresh reports (respects max-concurrency limit)
- Count findings based on mode level across all new reports (same as Step 2):
  - **lax**: Count CRITICAL only (aggregate across all 4 validators)
  - **normal**: Count CRITICAL + HIGH (aggregate across all 4 validators)
  - **strict**: Count CRITICAL + HIGH + MEDIUM (aggregate across all 4 validators)
  - **ultra**: Count all levels (aggregate across all 4 validators)
- If threshold-level findings = 0 AND iterations >= min-iterations (or min not provided): Proceed to step 7 (Finalization)
- If threshold-level findings = 0 AND iterations < min-iterations: Loop back to step 3 (need more iterations)
- If threshold-level findings > 0 AND max-iterations provided AND iterations >= max-iterations: Proceed to step 7 with status `partial`
- If threshold-level findings > 0 AND (max-iterations not provided OR iterations < max-iterations): Loop back to step 3

**Below-threshold findings**: Continue to be reported in audit but don't affect iteration logic

**Depends on**: Step 5 completion

**Notes**:

- **Default behavior**: Runs indefinitely until zero threshold-level findings (no max-iterations limit)
- **Optional min-iterations**: Prevents premature termination before sufficient iterations
- **Optional max-iterations**: Prevents infinite loops when explicitly provided
- Each iteration gets fresh validation reports across all four validators
- Tracks iteration count and finding trends
- Below-threshold findings remain visible but don't block convergence

### 7. Regenerate Titles (Sequential)

Update title fields in all ayokoding-web markdown files based on filenames and configuration.

**Agent**: `apps__ayokoding-web__title-maker`

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

**Agent**: `apps__ayokoding-web__navigation-maker`

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

- apps**ayokoding-web**general-checker
- apps**ayokoding-web**facts-checker
- apps**ayokoding-web**structure-checker
- apps**ayokoding-web**link-checker

**Args**: `scope: {input.scope}, expect: zero-issues`

**Output**: Final audit reports for all dimensions

**Success criteria**: All checkers report zero issues of ANY confidence level.

**On failure**: Set status to `partial`.

**Depends on**: Step 8 completion

**Notes**: Concurrency controlled by max-concurrency parameter (default: 2, recommend: 4 for this workflow).

### 10. Finalization (Sequential)

Report final status and summary.

**Output**: `{final-status}`, `{iterations-completed}`, all final reports

**Status determination**:

- **Success** (`pass`): Zero findings after final validation
- **Partial** (`partial`): Findings remain after max-iterations OR final validation failed
- **Failure** (`fail`): Technical errors during check, fix, or finalization

**Depends on**: Step 9 completion

## Termination Criteria

**Success** (`pass`):

- **lax**: Zero CRITICAL findings across all validators (HIGH/MEDIUM/LOW may exist)
- **normal**: Zero CRITICAL/HIGH findings across all validators (MEDIUM/LOW may exist)
- **strict**: Zero CRITICAL/HIGH/MEDIUM findings across all validators (LOW may exist)
- **ultra**: Zero findings at all levels across all validators

**Partial** (`partial`):

- Threshold-level findings remain after max-iterations OR final validation found issues

**Failure** (`fail`):

- Technical errors during check, fix, or finalization

**Note**: Below-threshold findings are reported in final audit but don't prevent success status.

## Example Usage

### Standard Check-Fix (Normal Strictness)

```bash
# Run complete ayokoding-web content validation with default settings
# Fixes CRITICAL/HIGH only, reports MEDIUM/LOW
workflow run ayokoding-web-general-quality-gate

# Equivalent explicit form
workflow run ayokoding-web-general-quality-gate --mode=normal
```

### Pre-Deployment Validation (Strict)

```bash
# Fixes CRITICAL/HIGH/MEDIUM, reports LOW
workflow run ayokoding-web-general-quality-gate --mode=strict

# Success criteria: Zero CRITICAL/HIGH/MEDIUM findings across all 4 validators
# LOW findings reported but don't block
```

### Comprehensive Audit (Ultra)

```bash
# Fixes all levels, zero tolerance
workflow run ayokoding-web-general-quality-gate --mode=ultra

# Success criteria: Zero findings at all levels across all 4 validators
# Equivalent to pre-mode parameter behavior
```

### Validate Specific Language

```bash
# Validate only English content with normal mode
workflow run ayokoding-web-general-quality-gate \
  --scope=ayokoding-web/content/en/ \
  --mode=normal
```

### Validate Specific Section

```bash
# Validate only programming section with strict mode
workflow run ayokoding-web-general-quality-gate \
  --scope=ayokoding-web/content/en/programming/ \
  --mode=strict
```

### With Iteration Bounds

```bash
# Require at least 2 iterations, cap at 10 maximum
workflow run ayokoding-web-general-quality-gate \
  --mode=normal \
  --min-iterations=2 \
  --max-iterations=10
```

### Strict Mode with Safety Limits

```bash
# Pre-deployment check with iteration bounds
workflow run ayokoding-web-general-quality-gate \
  --mode=strict \
  --max-iterations=10
```

### Fast Validation with Increased Concurrency

```bash
# Run all 4 checkers concurrently for faster validation
workflow run ayokoding-web-general-quality-gate \
  --max-concurrency=4

# Pre-deployment with strict mode and full concurrency
workflow run ayokoding-web-general-quality-gate \
  --mode=strict \
  --max-concurrency=4

# Conservative concurrency for resource-limited environments
workflow run ayokoding-web-general-quality-gate \
  --max-concurrency=1  # Sequential execution
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

- Optional max-iterations parameter (no default - runs until zero findings)
- When provided, workflow terminates with `partial` if limit reached
- Tracks iteration count and finding trends
- Use max-iterations when fix convergence is uncertain

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

### Content Validation (apps**ayokoding-web**general-checker)

- Hugo conventions (frontmatter, theme-specific)
- Bilingual consistency
- Navigation structure
- Content quality principles

### Facts Validation (apps**ayokoding-web**facts-checker)

- Technical accuracy using web verification
- Code examples correctness
- Tutorial sequences validity
- Bilingual factual consistency

### Structure Validation (apps**ayokoding-web**structure-checker)

- Weight conventions and ordering
- Navigation structure completeness
- Coverage gaps
- Pedagogical progression

### Links Validation (apps**ayokoding-web**link-checker)

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
