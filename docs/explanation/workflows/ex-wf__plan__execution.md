---
name: plan-execution
goal: Execute a project plan, validate its completion and quality, then iteratively continue until all requirements are met and archive to plans/done/
termination: Zero findings remain after validation and plan moved to done/
inputs:
  - name: plan-path
    type: string
    description: Path to the plan file to execute (e.g., "plans/in-progress/2025-01-15__new-feature/plan.md")
    required: true
  - name: max-iterations
    type: number
    description: Maximum number of execute-check cycles to prevent infinite loops
    required: false
    default: 10
outputs:
  - name: final-status
    type: enum
    values: [pass, partial, fail]
    description: Final execution and validation status
  - name: iterations-completed
    type: number
    description: Number of execute-check cycles performed
  - name: final-report
    type: file
    pattern: generated-reports/plan-execution__*__validation.md
    description: Final validation report from plan-execution-checker
---

# Plan Execution Workflow

**Purpose**: Automatically execute a project plan, validate its completion and quality, then iteratively continue execution until all requirements are met. Upon success, move the plan to `plans/done/`.

**When to use**:

- When you want to execute a plan from start to finish with automated quality validation
- After creating a new plan and want immediate implementation
- For plans that require iterative refinement to meet all requirements
- When you need automated archival of completed plans to done/ folder
- For systematic plan completion with zero-findings quality standard

## Steps

### 1. Initial Execution (Sequential)

Execute the project plan using the plan-executor agent.

**Agent**: `plan-executor`

- **Args**: `plan: {input.plan-path}`
- **Output**: `{execution-started}` - Implementation begins, delivery checklist items progressively updated

**Success criteria**: Executor completes current iteration without technical errors.

**On failure**: Terminate workflow with status `fail`.

**Notes**:

- Executor reads plan, implements requirements, updates checklist items as completed
- May request user input for critical decisions during execution
- Progressively updates delivery checklist throughout implementation

### 2. Validation (Sequential)

Validate the implementation against plan requirements.

**Agent**: `plan-execution-checker`

- **Args**: `plan: {input.plan-path}`
- **Output**: `{audit-report-1}` - Initial validation report in `generated-reports/`
- **Depends on**: Step 1 completion

**Success criteria**: Checker completes and generates validation report.

**On failure**: Terminate workflow with status `fail`.

**Notes**:

- Validates implementation against plan requirements
- Checks all deliverables meet quality standards
- Verifies delivery checklist completion
- Generates progressive report with all findings (HIGH, MEDIUM, MINOR)

### 3. Check for Findings (Sequential)

Analyze validation report to determine if further execution is needed.

**Condition Check**: Count ALL findings (HIGH, MEDIUM, and MINOR) in `{step2.outputs.audit-report-1}`

- If findings > 0: Proceed to step 4 (Continue Execution)
- If findings = 0: Skip to step 7 (Finalization - Success)

**Depends on**: Step 2 completion

**Notes**:

- Includes all finding levels - missing requirements, incomplete deliverables, quality issues
- Zero findings required for success (perfect quality standard)
- Reports which requirements still need work

### 4. Continue Execution (Sequential, Conditional)

Address findings and continue implementation.

**Agent**: `plan-executor`

- **Args**: `plan: {input.plan-path}, focus: {findings-from-latest-report}`
- **Output**: `{additional-work-completed}` - More checklist items completed, findings addressed
- **Condition**: Findings exist from step 3 or step 6
- **Depends on**: Step 3 completion (first iteration) or Step 6 completion (subsequent iterations)

**Success criteria**: Executor addresses findings and continues implementation.

**On failure**: Log errors, proceed to step 5 for verification.

**Notes**:

- Executor focuses on addressing specific findings while continuing overall plan execution
- Updates delivery checklist with resolved items
- May implement new requirements or fix quality issues
- Continues from previous work, doesn't restart from scratch

### 5. Re-validate (Sequential)

Run validation again to verify findings resolved and no new issues introduced.

**Agent**: `plan-execution-checker`

- **Args**: `plan: {input.plan-path}`
- **Output**: `{audit-report-N}` - Verification validation report
- **Depends on**: Step 4 completion

**Success criteria**: Checker completes validation.

**On failure**: Terminate workflow with status `fail`.

**Notes**:

- Verifies all findings from previous report are resolved
- Checks no new issues were introduced during fixes
- Generates fresh validation report with current status

### 6. Iteration Control (Sequential)

Determine whether to continue execution or terminate.

**Logic**:

- Count ALL findings in `{step5.outputs.audit-report-N}` (HIGH, MEDIUM, MINOR)
- If findings = 0: Proceed to step 7 (Finalization - Success)
- If findings > 0 AND iterations < max-iterations: Loop back to step 4 with new report
- If findings > 0 AND iterations >= max-iterations: Proceed to step 7 (Finalization - Partial)

**Depends on**: Step 5 completion

**Notes**:

- Prevents infinite loops with max-iterations limit
- Continues until ZERO findings of any confidence level
- Each iteration uses the latest validation report
- Tracks iteration count for observability

### 7. Finalization (Sequential)

Report final status and archive plan if successful.

**Logic**:

- If status is `pass` (zero findings):
  - Move entire plan folder from current location to `plans/done/`
  - Preserve folder name and structure (e.g., `plans/in-progress/2025-01-15__new-feature/` → `plans/done/2025-01-15__new-feature/`)
  - Use `git mv` to preserve git history
- If status is `partial` or `fail`: Leave plan in current location

**Output**: `{final-status}`, `{iterations-completed}`, `{final-report}`

**Status determination**:

- ✅ **Success** (`pass`): Zero findings after validation, all requirements met, plan moved to `plans/done/`
- ⚠️ **Partial** (`partial`): Findings remain after max-iterations, plan stays in current location
- ❌ **Failure** (`fail`): Technical errors during execution or checking, plan stays in current location

**Depends on**: Reaching this step from step 3, 5, or 6

## Termination Criteria

- ✅ **Success** (`pass`): Zero findings of ANY confidence level (HIGH, MEDIUM, MINOR) in final validation, all deliverables complete, plan archived to `plans/done/`
- ⚠️ **Partial** (`partial`): Findings remain after max-iterations cycles, plan requires manual intervention
- ❌ **Failure** (`fail`): Executor or checker encountered technical errors preventing completion

## Example Usage

### Execute Plan with Default Settings

```bash
# Run full plan execution with default settings (max 10 iterations)
workflow run plan-execution --plan-path=plans/in-progress/2025-01-15__new-feature/plan.md
```

### Execute with Extended Iterations

```bash
# Allow up to 15 iterations for complex plans
workflow run plan-execution --plan-path=plans/in-progress/2025-01-15__complex-migration/plan.md --max-iterations=15
```

### Execute Plan from Backlog

```bash
# Execute plan from backlog (won't move to done until complete)
workflow run plan-execution --plan-path=plans/backlog/2025-02-01__future-feature/plan.md
```

### Quick Validation Only

```bash
# Execute once, don't retry (set max-iterations=1)
workflow run plan-execution --plan-path=plans/in-progress/2025-01-15__new-feature/plan.md --max-iterations=1
```

## Iteration Example

Typical execution flow:

```
Iteration 1:
  Execute → Implement 60% of requirements → Check → 8 findings (missing deliverables, incomplete features)

Iteration 2:
  Execute (focus on findings) → Complete remaining requirements → Check → 3 findings (quality issues)

Iteration 3:
  Execute (focus on findings) → Address quality issues → Check → 0 findings

Result: SUCCESS (3 iterations) → Plan moved to plans/done/
```

## Safety Features

**Infinite Loop Prevention**:

- Max-iterations parameter (default: 10)
- Workflow terminates with `partial` if limit reached
- Tracks iteration count for monitoring

**Progressive Updates**:

- Delivery checklist items updated throughout execution
- Each iteration builds on previous work
- Validation history preserved in generated-reports/

**Error Recovery**:

- Continues to verification even if some execution steps encounter issues
- Reports which requirements succeeded/failed
- Generates final report regardless of status

**Plan Preservation**:

- Only moves plan to done/ on complete success (zero findings)
- Partial completion keeps plan in current location for manual review
- Uses git mv to preserve commit history when archiving

## Plan-Specific Validation

The plan-execution-checker validates:

- **Requirements Coverage**: All requirements from plan implemented
- **Deliverables Completeness**: All deliverables created and meet quality standards
- **Checklist Completion**: All delivery checklist items marked as completed
- **Quality Standards**: Implementation follows repository conventions and best practices
- **Testing Requirements**: Tests written and passing as specified in plan
- **Documentation**: Required documentation created and accurate

## Related Workflows

This workflow can be composed with:

- **plan\_\_quality-gate**: Validate plan quality before executing (recommended pre-step)
- Content creation workflows: Execute content-focused plans
- Release workflows: Execute release plans with deployment
- **wow**rules**quality-gate**: Validate repository consistency after plan execution

**Recommended Workflow Sequence**:

```
1. plan__quality-gate → Validate plan completeness and accuracy
2. plan-execution → Execute validated plan
3. wow__rules__quality-gate → Ensure repository consistency
```

## Success Metrics

Track across executions:

- **Average iterations to completion**: How many cycles typically needed for different plan types
- **Success rate**: Percentage of plans reaching zero findings and moving to done/
- **Common finding categories**: What issues appear most often during execution
- **Execution success rate**: Percentage of requirements implemented without errors
- **Archival rate**: Percentage of plans successfully moved to done/

## Notes

- **Semi-automated**: plan-executor may request user input for critical decisions, but execution continues autonomously
- **Idempotent**: Safe to re-run on partially completed plans, won't duplicate work
- **Progressive**: Each iteration builds on previous work, continuously updating checklists
- **Observable**: Generates validation reports for every validation cycle
- **Bounded**: Max-iterations prevents runaway execution
- **Archival**: Automatically moves successfully completed plans to done/ folder
- **History-preserving**: Uses git mv to maintain commit history when archiving

**Key Differences from plan-quality-gate**:

1. **Execution-focused**: Uses plan-executor (implements code) instead of plan-fixer (fixes plan documents)
2. **End-to-end**: Covers full plan lifecycle from execution through validation to archival
3. **Progressive delivery**: Continuously updates checklist items throughout execution
4. **Archival automation**: Moves completed plans to plans/done/ automatically
5. **Higher default iterations**: Default 10 (vs 5) since implementation is more complex than document fixes

This workflow ensures complete plan execution with validated quality, making it ideal for systematically implementing project plans from start to archive.

## Principles Respected

- ✅ **Explicit Over Implicit**: All steps, conditions, and termination criteria clearly defined
- ✅ **Automation Over Manual**: Fully automated execution, validation, and archival
- ✅ **Simplicity Over Complexity**: Clear linear flow with loop control and bounded iterations
- ✅ **Accessibility First**: Generates human-readable validation reports for transparency
- ✅ **Progressive Disclosure**: Configurable iterations and plan paths for different use cases
- ✅ **No Time Estimates**: Focus on quality outcomes and completion criteria, not duration
