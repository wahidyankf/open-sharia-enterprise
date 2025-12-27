---
name: repository-rules-quality-gate
goal: Validate repository consistency across all layers, apply fixes iteratively until zero findings achieved
termination: Zero findings remain after validation (runs indefinitely until achieved unless max-iterations provided)
inputs:
  - name: strictness
    type: enum
    values: [normal, strict, very-strict]
    description: Quality threshold (normal: CRITICAL/HIGH only, strict: +MEDIUM, very-strict: all levels)
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
  - name: max-parallelization
    type: number
    description: Maximum number of agents/tasks that can run in parallel during workflow execution
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
    pattern: generated-reports/repo-rules__*__*__audit.md
    description: Final audit report (4-part format with UUID chain)
  - name: execution-scope
    type: string
    description: Scope identifier for UUID chain tracking (default "repo-rules")
    required: false
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

- **Args**: `scope: all, EXECUTION_SCOPE: repo-rules`
- **Output**: `{audit-report-1}` - Initial audit report in `generated-reports/` (4-part format: `repo-rules__{uuid-chain}__{timestamp}__audit.md`)

**UUID Chain Tracking**: Checker generates 6-char UUID and writes to `generated-reports/.execution-chain-repo-rules` before spawning any child agents. See [Temporary Files Convention](../development/ex-de__temporary-files.md#uuid-chain-generation) for details.

**Success criteria**: Checker completes and generates audit report.

**On failure**: Terminate workflow with status `fail`.

### 2. Check for Findings (Sequential)

Analyze audit report to determine if fixes are needed.

**Condition Check**: Count findings based on strictness level in `{step1.outputs.audit-report-1}`

- **normal**: Count CRITICAL + HIGH only
- **strict**: Count CRITICAL + HIGH + MEDIUM
- **very-strict**: Count all levels (CRITICAL, HIGH, MEDIUM, LOW)

**Below-threshold findings**: Report but don't block success

- **normal**: MEDIUM/LOW reported, not counted
- **strict**: LOW reported, not counted
- **very-strict**: All findings counted

**Decision**:

- If threshold-level findings > 0: Proceed to step 3
- If threshold-level findings = 0: Skip to step 6 (Success)

**Depends on**: Step 1 completion

**Notes**:

- Fix scope determined by strictness level
- Below-threshold findings remain visible in audit reports
- Enables progressive quality improvement

### 3. Apply Fixes (Sequential, Conditional)

Apply validated fixes from the audit report based on strictness level.

**Agent**: `repo-rules-fixer`

- **Args**: `report: {step1.outputs.audit-report-1}, approved: all, strictness: {input.strictness}, EXECUTION_SCOPE: repo-rules`
- **Output**: `{fixes-applied}` - Fix report with same UUID chain as source audit
- **Condition**: Threshold-level findings exist from step 2
- **Depends on**: Step 2 completion

**Success criteria**: Fixer successfully applies all threshold-level fixes without errors.

**On failure**: Log errors, proceed to step 4 for verification.

**Notes**:

- Fixer re-validates findings before applying (prevents false positives)
- **Fix scope based on strictness**:
  - **normal**: Fix CRITICAL + HIGH only (skip MEDIUM/LOW)
  - **strict**: Fix CRITICAL + HIGH + MEDIUM (skip LOW)
  - **very-strict**: Fix all levels (CRITICAL, HIGH, MEDIUM, LOW)
- Below-threshold findings remain untouched

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

- Count findings based on strictness level in {step4.outputs.audit-report-N} (same as Step 2):
  - **normal**: Count CRITICAL + HIGH
  - **strict**: Count CRITICAL + HIGH + MEDIUM
  - **very-strict**: Count all levels
- If threshold-level findings = 0 AND iterations >= min-iterations (or min not provided): Proceed to step 6 (Success)
- If threshold-level findings = 0 AND iterations < min-iterations: Loop back to step 3 (need more iterations)
- If threshold-level findings > 0 AND max-iterations provided AND iterations >= max-iterations: Proceed to step 6 (Partial)
- If threshold-level findings > 0 AND (max-iterations not provided OR iterations < max-iterations): Loop back to step 3

**Below-threshold findings**: Continue to be reported in audit but don't affect iteration logic

**Depends on**: Step 4 completion

**Notes**:

- **Default behavior**: Runs indefinitely until zero threshold-level findings (no max-iterations limit)
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

**Success** (`pass`):

- **normal**: Zero CRITICAL/HIGH findings (MEDIUM/LOW may exist)
- **strict**: Zero CRITICAL/HIGH/MEDIUM findings (LOW may exist)
- **very-strict**: Zero findings at all levels

**Partial** (`partial`):

- Threshold-level findings remain after max-iterations safety limit

**Failure** (`fail`):

- Technical errors during check or fix

**Note**: Below-threshold findings are reported in final audit but don't prevent success status.

## Example Usage

### Standard Check-Fix (Normal Strictness)

```bash
# Run full repository rules check-fix with default settings
# Fixes CRITICAL/HIGH only, reports MEDIUM/LOW
workflow run repository-rules-quality-gate

# Equivalent explicit form
workflow run repository-rules-quality-gate --strictness=normal
```

### Pre-Release Validation (Strict)

```bash
# Fixes CRITICAL/HIGH/MEDIUM, reports LOW
workflow run repository-rules-quality-gate --strictness=strict

# Success criteria: Zero CRITICAL/HIGH/MEDIUM findings
# LOW findings reported but don't block
```

### Comprehensive Audit (Very Strict)

```bash
# Fixes all levels, zero tolerance
workflow run repository-rules-quality-gate --strictness=very-strict

# Success criteria: Zero findings at all levels
# Equivalent to pre-strictness parameter behavior
```

### With Iteration Bounds

```bash
# Require at least 2 iterations, cap at 10 maximum
workflow run repository-rules-quality-gate \
  --strictness=normal \
  --min-iterations=2 \
  --max-iterations=10
```

### Strict Mode with Safety Limits

```bash
# Pre-release check with iteration bounds
workflow run repository-rules-quality-gate \
  --strictness=strict \
  --max-iterations=10
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

**Parallelization**: Currently validates and fixes sequentially. The `max-parallelization` parameter is reserved for future enhancements where multiple validation dimensions (principles, conventions, development, agents, CLAUDE.md) could run concurrently.

This workflow ensures repository consistency through iterative validation and fixing, making it ideal for maintenance and quality assurance.

## Principles Respected

- ✅ **Explicit Over Implicit**: All steps, conditions, and termination criteria are explicit
- ✅ **Automation Over Manual**: Fully automated validation and fixing without human intervention
- ✅ **Simplicity Over Complexity**: Clear linear flow with loop control
- ✅ **Accessibility First**: Generates human-readable audit reports
- ✅ **Progressive Disclosure**: Can run with different iteration limits
- ✅ **No Time Estimates**: Focus on quality outcomes, not duration
