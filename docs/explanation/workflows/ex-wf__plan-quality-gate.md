---
name: plan-quality-gate
goal: Validate plan completeness and technical accuracy, apply fixes iteratively until zero findings achieved
termination: Zero findings remain after validation (runs indefinitely until achieved unless max-iterations provided)
inputs:
  - name: scope
    type: string
    description: Plan files to validate (e.g., "all", "plans/in-progress/", "specific-plan.md")
    required: false
    default: all
  - name: mode
    type: enum
    values: [lax, normal, strict, ocd]
    description: Quality threshold (lax: CRITICAL only, normal: CRITICAL/HIGH, strict: +MEDIUM, ocd: all levels)
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
  - name: final-report
    type: file
    pattern: generated-reports/plan__*__audit.md
    description: Final audit report
---

# Plan Quality Gate Workflow

**Purpose**: Automatically validate plan completeness, technical accuracy, and implementation readiness, then apply fixes iteratively until all issues are resolved.

**When to use**:

- After creating new project plans
- Before starting plan execution
- When updating existing plans with new requirements
- Periodically to ensure plan quality and accuracy

## Steps

### 1. Initial Validation (Sequential)

Run plan validation to identify completeness and accuracy issues.

**Agent**: `plan__checker`

- **Args**: `scope: {input.scope}`
- **Output**: `{audit-report-1}` - Initial audit report in `generated-reports/`

**Success criteria**: Checker completes and generates audit report.

**On failure**: Terminate workflow with status `fail`.

### 2. Check for Findings (Sequential)

Analyze audit report to determine if fixes are needed.

**Condition Check**: Count findings based on mode level in `{step1.outputs.audit-report-1}`

- **lax**: Count CRITICAL only
- **normal**: Count CRITICAL + HIGH
- **strict**: Count CRITICAL + HIGH + MEDIUM
- **ocd**: Count all levels (CRITICAL, HIGH, MEDIUM, LOW)

**Below-threshold findings**: Report but don't block success

- **lax**: HIGH/MEDIUM/LOW reported, not counted
- **normal**: MEDIUM/LOW reported, not counted
- **strict**: LOW reported, not counted
- **ocd**: All findings counted

**Decision**:

- If threshold-level findings > 0: Proceed to step 3
- If threshold-level findings = 0: Skip to step 6 (Success)

**Depends on**: Step 1 completion

**Notes**:

- Fix scope determined by mode level
- Below-threshold findings remain visible in audit reports
- Enables progressive quality improvement

### 3. Apply Fixes (Sequential, Conditional)

Apply all validated fixes from the audit report.

**Agent**: `plan__fixer`

- **Args**: `report: {step1.outputs.audit-report-1}, approved: all, mode: {input.mode}`
- **Output**: `{fixes-applied}`
- **Condition**: Findings exist from step 2
- **Depends on**: Step 2 completion

**Success criteria**: Fixer successfully applies all fixes without errors.

**On failure**: Log errors, proceed to step 4 for verification.

**Notes**:

- Fixer re-validates findings before applying (prevents false positives)
- **Fix scope based on mode parameter**:
  - **lax**: Fixes CRITICAL only (skips HIGH/MEDIUM/LOW)
  - **normal**: Fixes CRITICAL/HIGH (skips MEDIUM/LOW)
  - **strict**: Fixes CRITICAL/HIGH/MEDIUM (skips LOW)
  - **ocd**: Fixes all levels (CRITICAL, HIGH, MEDIUM, LOW)
- Within scope, applies HIGH confidence fixes automatically
- Flags MEDIUM confidence for manual review

### 4. Re-validate (Sequential)

Run checker again to verify fixes resolved issues and no new issues introduced.

**Agent**: `plan__checker`

- **Args**: `scope: {input.scope}`
- **Output**: `{audit-report-N}` - Verification audit report
- **Depends on**: Step 3 completion

**Success criteria**: Checker completes validation.

**On failure**: Terminate workflow with status `fail`.

### 5. Iteration Control (Sequential)

Determine whether to continue fixing or terminate.

**Logic**:

- Count findings based on mode level in `{step4.outputs.audit-report-N}` (same as Step 2):
  - **lax**: Count CRITICAL only
  - **normal**: Count CRITICAL + HIGH
  - **strict**: Count CRITICAL + HIGH + MEDIUM
  - **ocd**: Count all levels
- If threshold-level findings = 0 AND iterations >= min-iterations (or min not provided): Proceed to step 6 (Success)
- If threshold-level findings = 0 AND iterations < min-iterations: Loop back to step 3 (need more iterations)
- If threshold-level findings > 0 AND max-iterations provided AND iterations >= max-iterations: Proceed to step 6 (Partial)
- If threshold-level findings > 0 AND (max-iterations not provided OR iterations < max-iterations): Loop back to step 3

**Depends on**: Step 4 completion

**Notes**:

- **Default behavior**: Runs indefinitely until zero findings (no max-iterations limit)
- **Optional min-iterations**: Prevents premature termination before sufficient iterations
- **Optional max-iterations**: Prevents infinite loops when explicitly provided
- Each iteration uses the latest audit report
- Tracks iteration count for observability

### 6. Finalization (Sequential)

Report final status and summary.

**Output**: `{final-status}`, `{iterations-completed}`, `{final-report}`

**Status determination**:

- **Success** (`pass`): Zero findings after validation
- **Partial** (`partial`): Findings remain after max-iterations
- **Failure** (`fail`): Technical errors during check or fix

**Depends on**: Reaching this step from step 2, 4, or 5

## Termination Criteria

**Success** (`pass`): Zero threshold-level findings in final validation

- **lax**: Zero CRITICAL findings (HIGH/MEDIUM/LOW may exist)
- **normal**: Zero CRITICAL/HIGH findings (MEDIUM/LOW may exist)
- **strict**: Zero CRITICAL/HIGH/MEDIUM findings (LOW may exist)
- **ocd**: Zero findings at all levels

**Partial** (`partial`): Threshold-level findings remain after max-iterations

**Failure** (`fail`): Checker or fixer encountered technical errors

**Note**: Below-threshold findings reported in final audit but don't prevent success.

## Example Usage

### Validate All Plans

```bash
# Run full plan validation with default settings
workflow run plan-quality-gate
```

### Validate Specific Plan Folder

```bash
# Validate only in-progress plans
workflow run plan-quality-gate --scope=plans/in-progress/
```

### Validate Single Plan

```bash
# Validate specific plan file
workflow run plan-quality-gate --scope=plans/in-progress/2025-01-15__new-feature/plan.md
```

### With Iteration Bounds

```bash
# Require at least 2 iterations, cap at 10 maximum
workflow run plan-quality-gate --scope=all --min-iterations=2 --max-iterations=10
```

### Prevent Infinite Loops

```bash
# Set maximum iterations when unsure about fix convergence
workflow run plan-quality-gate --scope=all --max-iterations=10
```

### Require Minimum Iterations

```bash
# Ensure at least 3 check-fix cycles before accepting zero findings
workflow run plan-quality-gate --scope=all --min-iterations=3
```

### Standard Check-Fix (Normal Strictness)

```bash
# Run full plan validation with default settings
# Fixes CRITICAL/HIGH only, reports MEDIUM/LOW
workflow run plan-quality-gate --scope=all

# Equivalent explicit form
workflow run plan-quality-gate --scope=all --mode=normal
```

### Pre-Release Validation (Strict)

```bash
# Fixes CRITICAL/HIGH/MEDIUM, reports LOW
workflow run plan-quality-gate --scope=all --mode=strict

# Success criteria: Zero CRITICAL/HIGH/MEDIUM findings
# LOW findings reported but don't block
```

### Comprehensive Audit (Very Strict)

```bash
# Fixes all levels, zero tolerance
workflow run plan-quality-gate --scope=all --mode=ocd

# Success criteria: Zero findings at all levels
# Equivalent to pre-mode parameter behavior
```

### Strict Mode with Safety Limits

```bash
# Pre-release check with iteration bounds
workflow run plan-quality-gate \
  --scope=all \
  --mode=strict \
  --max-iterations=10
```

## Iteration Example

Typical execution flow:

```
Iteration 1:
  Check → 12 findings (missing requirements, incomplete checklists) → Fix → Re-check → 5 findings

Iteration 2:
  Check (reuse) → 5 findings (technical inaccuracies) → Fix → Re-check → 1 finding

Iteration 3:
  Check (reuse) → 1 finding (formatting) → Fix → Re-check → 0 findings

Result: SUCCESS (3 iterations)
```

## Safety Features

**Infinite Loop Prevention**:

- Optional max-iterations parameter (no default - runs until zero findings)
- When provided, workflow terminates with `partial` if limit reached
- Tracks iteration count for monitoring
- Use max-iterations when fix convergence is uncertain

**False Positive Protection**:

- Fixer re-validates each finding before applying
- Skips FALSE_POSITIVE findings automatically
- Progressive writing ensures audit history survives

**Error Recovery**:

- Continues to verification even if some fixes fail
- Reports which fixes succeeded/failed
- Generates final report regardless of status

## Plan-Specific Validation

The plan\_\_checker validates:

- **Completeness**: All required sections present (requirements, deliverables, checklists)
- **Technical Accuracy**: Commands, versions, tool names verified via web search
- **Implementation Readiness**: Plans are actionable and executable
- **Codebase Alignment**: References to existing files, patterns, and conventions
- **Clarity**: Clear problem statements, well-defined scope, unambiguous requirements

## Related Workflows

This workflow can be composed with:

- Content creation workflows (validate plans before creating content)
- Execution workflows (validate before starting implementation)
- Release workflows (validate plan completeness before release planning)

## Success Metrics

Track across executions:

- **Average iterations to completion**: How many cycles typically needed
- **Success rate**: Percentage reaching zero findings
- **Common finding categories**: What issues appear most often in plans
- **Fix success rate**: Percentage of fixes applied without errors

## Notes

- **Fully automated**: No human checkpoints, runs to completion
- **Idempotent**: Safe to run multiple times, won't break working plans
- **Conservative**: Fixer skips uncertain changes (preserves plan intent)
- **Observable**: Generates audit reports for every iteration
- **Bounded**: Max-iterations prevents runaway execution
- **Scope-aware**: Can validate all plans or specific subsets

**Parallelization**: Currently validates and fixes sequentially. The `max-concurrency` parameter is reserved for future enhancements where different plan validation aspects (completeness, technical accuracy, implementation readiness) could run concurrently.

This workflow ensures plan quality and implementation readiness through iterative validation and fixing, making it ideal for maintaining high-quality project planning.

## Principles Respected

- ✅ **Explicit Over Implicit**: All steps, conditions, and termination criteria are explicit
- ✅ **Automation Over Manual**: Fully automated validation and fixing without human intervention
- ✅ **Simplicity Over Complexity**: Clear linear flow with loop control
- ✅ **Accessibility First**: Generates human-readable audit reports
- ✅ **Progressive Disclosure**: Can run with different scopes and iteration limits
- ✅ **No Time Estimates**: Focus on quality outcomes, not duration
