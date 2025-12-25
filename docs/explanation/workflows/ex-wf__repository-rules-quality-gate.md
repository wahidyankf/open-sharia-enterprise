---
name: repository-rules-quality-gate
goal: Validate repository consistency across all layers, apply fixes iteratively until zero findings achieved
termination: Zero findings remain after validation (runs indefinitely until achieved unless max-iterations provided)
inputs:
  - name: min-iterations
    type: number
    description: Minimum check-fix cycles before allowing zero-finding termination (prevents premature success)
    required: false
  - name: max-iterations
    type: number
    description: Maximum check-fix cycles to prevent infinite loops (if not provided, runs until zero findings)
    required: false
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
    pattern: generated-reports/repo-rules__*__audit.md
    description: Final audit report
---

# Repository Rules Quality Gate Workflow

**Purpose**: Automatically validate repository consistency across principles, conventions, development practices, agents, and CLAUDE.md, then apply fixes iteratively until all issues are resolved.

**When to use**:

- After making changes to conventions, principles, or development practices
- Before major releases or deployments
- Periodically to ensure repository health
- After adding or modifying agents

## Steps

### 1. Initial Validation (Sequential)

Run repository-wide consistency check to identify all issues.

**Agent**: `repo-rules-checker`

- **Args**: `scope: all`
- **Output**: `{audit-report-1}` - Initial audit report in `generated-reports/`

**Success criteria**: Checker completes and generates audit report.

**On failure**: Terminate workflow with status `fail`.

### 2. Check for Findings (Sequential)

Analyze audit report to determine if fixes are needed.

**Condition Check**: Count ALL findings (HIGH, MEDIUM, and MINOR) in `{step1.outputs.audit-report-1}`

- If findings > 0: Proceed to step 3
- If findings = 0: Skip to step 6 (Success)

**Depends on**: Step 1 completion

**Notes**:

- Fixes ALL findings, not just critical ones
- Includes minor issues like formatting, style improvements
- Ensures repository achieves perfect quality state

### 3. Apply Fixes (Sequential, Conditional)

Apply all validated fixes from the audit report.

**Agent**: `repo-rules-fixer`

- **Args**: `report: {step1.outputs.audit-report-1}, approved: all`
- **Output**: `{fixes-applied}`
- **Condition**: Findings exist from step 2
- **Depends on**: Step 2 completion

**Success criteria**: Fixer successfully applies all fixes without errors.

**On failure**: Log errors, proceed to step 4 for verification.

**Notes**:

- Fixer re-validates findings before applying (prevents false positives)
- Fixes ALL confidence levels: HIGH (objective), MEDIUM (structural), MINOR (style/formatting)
- Achieves perfect repository state with zero findings

### 4. Re-validate (Sequential)

Run checker again to verify fixes resolved issues and no new issues introduced.

**Agent**: `repo-rules-checker`

- **Args**: `scope: all`
- **Output**: `{audit-report-N}` - Verification audit report
- **Depends on**: Step 3 completion

**Success criteria**: Checker completes validation.

**On failure**: Terminate workflow with status `fail`.

### 5. Iteration Control (Sequential)

Determine whether to continue fixing or terminate.

**Logic**:

- Count ALL findings in `{step4.outputs.audit-report-N}` (HIGH, MEDIUM, MINOR)
- If findings = 0 AND iterations >= min-iterations (or min not provided): Proceed to step 6 (Success)
- If findings = 0 AND iterations < min-iterations: Loop back to step 3 (need more iterations)
- If findings > 0 AND max-iterations provided AND iterations >= max-iterations: Proceed to step 6 (Partial)
- If findings > 0 AND (max-iterations not provided OR iterations < max-iterations): Loop back to step 3

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

- ✅ **Success** (`pass`): Zero findings of ANY confidence level (HIGH, MEDIUM, MINOR) in final validation
- ⚠️ **Partial** (`partial`): Any findings remain after max-iterations cycles
- ❌ **Failure** (`fail`): Checker or fixer encountered technical errors

## Example Usage

### Standard Check-Fix

```bash
# Run full repository rules check-fix with default settings
workflow run repository-rules-quality-gate
```

### With Iteration Bounds

```bash
# Require at least 2 iterations, cap at 10 maximum
workflow run repository-rules-quality-gate --min-iterations=2 --max-iterations=10
```

### Prevent Infinite Loops

```bash
# Set maximum iterations when unsure about fix convergence
workflow run repository-rules-quality-gate --max-iterations=10
```

### Require Minimum Iterations

```bash
# Ensure at least 3 check-fix cycles before accepting zero findings
workflow run repository-rules-quality-gate --min-iterations=3
```

## Iteration Example

Typical execution flow:

```
Iteration 1:
  Check → 15 findings → Fix → Re-check → 8 findings

Iteration 2:
  Check (reuse) → 8 findings → Fix → Re-check → 2 findings

Iteration 3:
  Check (reuse) → 2 findings → Fix → Re-check → 0 findings

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

## Related Workflows

This workflow can be composed with:

- Deployment workflows (validate before deploy)
- Release workflows (audit before version bump)
- Content creation workflows (validate after bulk changes)

## Success Metrics

Track across executions:

- **Average iterations to completion**: How many cycles typically needed
- **Success rate**: Percentage reaching zero findings
- **Common finding categories**: What issues appear most often
- **Fix success rate**: Percentage of fixes applied without errors

## Notes

- **Fully automated**: No human checkpoints, runs to completion
- **Idempotent**: Safe to run multiple times, won't break working state
- **Conservative**: Fixer skips uncertain changes (preserves correctness)
- **Observable**: Generates audit reports for every iteration
- **Bounded**: Max-iterations prevents runaway execution

This workflow ensures repository consistency through iterative validation and fixing, making it ideal for maintenance and quality assurance.

## Principles Respected

- ✅ **Explicit Over Implicit**: All steps, conditions, and termination criteria are explicit
- ✅ **Automation Over Manual**: Fully automated validation and fixing without human intervention
- ✅ **Simplicity Over Complexity**: Clear linear flow with loop control
- ✅ **Accessibility First**: Generates human-readable audit reports
- ✅ **Progressive Disclosure**: Can run with different iteration limits
- ✅ **No Time Estimates**: Focus on quality outcomes, not duration
