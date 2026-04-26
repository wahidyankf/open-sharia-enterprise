---
name: ci-quality-gate
goal: Validate all projects conform to CI/CD standards and fix non-compliance iteratively
termination: "Zero findings on two consecutive validations (max-iterations defaults to 7, escalation warning at 5)"
inputs:
  - name: scope
    type: string
    description: Scope of validation - "all" for all projects, or specific project name
    required: false
    default: all
  - name: mode
    type: enum
    values: [lax, normal, strict, ocd]
    description: "Quality threshold (lax: CRITICAL only, normal: CRITICAL/HIGH, strict: +MEDIUM, ocd: all levels)"
    required: false
    default: strict
  - name: max-iterations
    type: number
    description: Maximum number of check-fix cycles
    required: false
    default: 7
outputs:
  - name: final-status
    type: enum
    values: [pass, partial, fail]
    description: Final validation status
  - name: iterations-completed
    type: number
    description: Number of check-fix cycles performed
  - name: final-report
    type: file
    pattern: generated-reports/ci__*__audit.md
    description: Final audit report from ci-checker
---

# CI Quality Gate Workflow

**Purpose**: Automatically validate all projects in the repository conform to CI/CD standards
defined in `governance/development/infra/ci-conventions.md`, then iteratively fix non-compliance
until zero findings are achieved.

**When to use**:

- After adding a new app to the repository
- After modifying CI/CD infrastructure (workflows, composite actions, Docker files)
- As a periodic compliance check
- Before major releases to ensure CI consistency

## Execution Mode

**Preferred Mode**: Agent Delegation — invoke `ci-checker` and `ci-fixer` via the Agent tool
with `subagent_type` (see [Workflow Execution Modes Convention](../meta/execution-modes.md)).

**Fallback Mode**: Manual Orchestration — execute workflow logic directly using
Read/Write/Edit tools when Agent Delegation is unavailable.

## Steps

### 1. Initial Check (Sequential)

Run `ci-checker` to validate all projects against CI standards.

**Agent**: `ci-checker`

- **Args**: `scope: {input.scope}`
- **Output**: Audit report in `generated-reports/`

**Success criteria**: Checker completes and generates audit report.

**On failure**: Terminate workflow with status `fail`.

### 2. Analyze Findings (Sequential)

Count findings by criticality level.

**Condition Check**: Count ALL findings in audit report.

- If findings > 0: Proceed to step 3 (reset `consecutive_zero_count` to 0)
- If findings = 0: Initialize `consecutive_zero_count` to 1 (first zero),
  proceed to step 4 for confirmation re-check (consecutive pass requirement)

**Depends on**: Step 1 completion

### 3. Apply Fixes (Sequential)

Run `ci-fixer` to address findings from the latest audit report.

**Agent**: `ci-fixer`

- **Args**: `report: {step1.outputs.audit-report}, approved: all`
- **Output**: Fixed files, updated configurations
- **Condition**: Findings exist from step 2
- **Depends on**: Step 2 completion

**Success criteria**: Fixer successfully applies fixes without errors.

**On failure**: Log errors, proceed to step 4 for verification.

### 4. Re-check and Iterate (Sequential)

Run `ci-checker` again to verify fixes and check for new issues.

**Agent**: `ci-checker`

- **Args**: `scope: {input.scope}`
- **Output**: Verification audit report
- **Depends on**: Step 3 completion

**Logic**:

- Count ALL findings in verification report
- Track `consecutive_zero_count` across iterations (resets to 0 when findings > 0, increments when findings = 0)
- If consecutive_zero_count >= 2: Proceed to step 5 (Success — double-zero confirmed)
- If consecutive_zero_count < 2 AND findings = 0: Loop back to step 4 (confirmation check)
- If findings > 0 AND iterations < max-iterations: Loop back to step 3
- If findings > 0 AND iterations >= max-iterations: Proceed to step 5 (Partial)
- **Escalation**: If findings count is not decreasing after iteration 5, log a warning: "Convergence not achieved — likely non-deterministic findings or scope expansion"

### 5. Finalization (Sequential)

Report final status.

- **pass**: Zero findings confirmed on two consecutive validations
- **partial**: Findings remain after max-iterations
- **fail**: Technical errors during checking or fixing

## Related Workflows

- [Plan Execution](../../workflows/plan/plan-execution.md) -- Uses similar iterative check-fix pattern
- [Plan Quality Gate](../../workflows/plan/plan-quality-gate.md) -- Analogous quality gate for plans

## Principles Implemented/Respected

- **Explicit Over Implicit**: All CI standards are documented in governance docs, not implicit conventions
- **Automation Over Manual**: Automated checking and fixing reduces manual compliance burden
- **Simplicity Over Complexity**: Simple iterative check-fix loop with bounded iterations

## Conventions Implemented/Respected

- **[CI/CD Conventions](../../development/infra/ci-conventions.md)**: The standards being validated
- **[Workflow Identifier Convention](../meta/workflow-identifier.md)**: Follows standard workflow structure
