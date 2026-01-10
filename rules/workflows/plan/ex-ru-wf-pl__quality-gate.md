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

## Execution Mode

**Current Mode**: Manual Orchestration (see [Workflow Execution Modes Convention](../meta/ex-ru-wf-me__execution-modes.md))

This workflow is currently executed through **manual orchestration** where the AI assistant (Claude Code or OpenCode) follows workflow steps directly using Read/Write/Edit tools. File changes persist to the actual filesystem.

**How to Execute**:

```
User: "Run plan quality gate workflow for plans/backlog/my-plan/ in manual mode"
```

The AI will:

1. Execute plan\_\_checker logic directly (read, validate, write audit)
2. Execute plan\_\_fixer logic directly (read audit, apply fixes, write fix report)
3. Iterate until zero findings achieved
4. Show git status with modified files
5. Wait for user commit approval

**Why Manual Mode?**: Task tool (Claude Code) or agent spawning (OpenCode) runs agents in isolated contexts where file changes don't persist. Manual orchestration ensures audit reports and fixes are actually written to the filesystem.

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

**Condition Check**: Count ALL findings (HIGH, MEDIUM, and MINOR) in `{step1.outputs.audit-report-1}`

- If findings > 0: Proceed to step 3
- If findings = 0: Skip to step 6 (Success)

**Depends on**: Step 1 completion

**Notes**:

- Fixes ALL findings, not just critical ones
- Includes minor issues like formatting, style improvements
- Ensures plans achieve perfect quality state

### 3. Apply Fixes (Sequential, Conditional)

Apply all validated fixes from the audit report.

**Agent**: `plan__fixer`

- **Args**: `report: {step1.outputs.audit-report-1}, approved: all`
- **Output**: `{fixes-applied}`
- **Condition**: Findings exist from step 2
- **Depends on**: Step 2 completion

**Success criteria**: Fixer successfully applies all fixes without errors.

**On failure**: Log errors, proceed to step 4 for verification.

**Notes**:

- Fixer re-validates findings before applying (prevents false positives)
- Fixes ALL confidence levels: HIGH (objective), MEDIUM (structural), MINOR (style/formatting)
- Achieves perfect plan quality with zero findings

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

### Validate All Plans

```
User: "Run plan quality gate workflow for all plans in manual mode"
```

The AI will execute the workflow directly:

- Validate all plan files (plan\_\_checker logic)
- Apply all fixes (plan\_\_fixer logic)
- Iterate until zero findings achieved

### Validate Specific Plan Folder

```
User: "Run plan quality gate workflow for plans/in-progress/ in manual mode"
```

The AI will execute with scoped validation:

- Validate only in-progress plans
- Fix issues in those plans only
- Iterate until zero findings in scope

### Validate Single Plan

```
User: "Run plan quality gate workflow for plans/in-progress/2025-01-15__new-feature/plan.md in manual mode"
```

The AI will execute with single-file scope:

- Validate specific plan file only
- Fix issues in that file
- Iterate until plan is clean

### With Iteration Bounds

```
User: "Run plan quality gate workflow for all plans with min-iterations=2 and max-iterations=10"
```

The AI will execute with iteration controls:

- Require at least 2 check-fix cycles
- Cap at maximum 10 iterations
- Report final status after completion

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

This workflow ensures plan quality and implementation readiness through iterative validation and fixing, making it ideal for maintaining high-quality project planning.

## Principles Respected

- ✅ **Explicit Over Implicit**: All steps, conditions, and termination criteria are explicit
- ✅ **Automation Over Manual**: Fully automated validation and fixing without human intervention
- ✅ **Simplicity Over Complexity**: Clear linear flow with loop control
- ✅ **Accessibility First**: Generates human-readable audit reports
- ✅ **Progressive Disclosure**: Can run with different scopes and iteration limits
- ✅ **No Time Estimates**: Focus on quality outcomes, not duration
