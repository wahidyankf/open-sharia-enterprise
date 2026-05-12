---
name: plan-quality-gate
title: "plan-quality-gate"
goal: Validate plan completeness and technical accuracy, apply fixes iteratively until zero findings achieved
termination: "Zero findings on two consecutive validations (max-iterations defaults to 7, escalation warning at 5)"
inputs:
  - name: scope
    type: string
    description: Plan files to validate (e.g., "all", "plans/in-progress/", "specific-plan.md")
    required: false
    default: all
  - name: mode
    type: enum
    values: [lax, normal, strict, ocd]
    description: "Quality threshold (lax: CRITICAL only, normal: CRITICAL/HIGH, strict: +MEDIUM, ocd: all levels)"
    required: false
    default: strict
  - name: min-iterations
    type: number
    description: Minimum check-fix cycles before allowing zero-finding termination (prevents premature success)
    required: false
  - name: max-iterations
    type: number
    description: Maximum check-fix cycles to prevent infinite loops
    required: false
    default: 7
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

**Preferred Mode**: Agent Delegation — invoke `plan-checker` and `plan-fixer` via the Agent
tool with `subagent_type` (see [Workflow Execution Modes Convention](../meta/execution-modes.md)).

**Fallback Mode**: Manual Orchestration — execute workflow logic directly using
Read/Write/Edit tools when Agent Delegation is unavailable.

The Agent tool runs delegated agents that persist file changes to the actual filesystem, making it
the preferred approach when these agents exist as defined delegated agent types.

**How to Execute**:

```
User: "Run plan quality gate workflow for plans/backlog/my-plan/"
```

The AI will:

1. Invoke `plan-checker` via the Agent tool (reads plan files, writes audit report)
2. Invoke `plan-fixer` via the Agent tool (reads audit, applies fixes, writes fix report)
3. Iterate until zero findings achieved
4. Show git status with modified files
5. Wait for user commit approval

**Fallback (Manual Mode)**:

```
User: "Run plan quality gate workflow for plans/backlog/my-plan/ in manual mode"
```

The AI executes checker and fixer logic directly using Read/Write/Edit tools in the main
context — use this when agent delegation is unavailable.

**When to use**:

- After creating new project plans
- Before starting plan execution
- When updating existing plans with new requirements
- Periodically to ensure plan quality and accuracy

## Research Delegation

The `plan-checker` agent delegates multi-page web research to the
[`web-research-maker`](../../../.claude/agents/web-research-maker.md) delegated agent when verifying a single
technical claim requires more than one or two searches, or more than two fetches. This keeps the
plan audit context lean — `plan-checker` receives a cited, synthesised summary and translates it
into dual-labelled findings, rather than burning its own context on multi-page research. Checkers
retain in-context `WebSearch` and `WebFetch` for single-shot verification against known
authoritative URLs. No workflow-level configuration is required; the delegation is encoded in the
`plan-checker` prompt.

Multi-page research delegation keeps plan-checker context lean — externalizing 2+ search or 3+ fetch operations into `web-research-maker` reduces the checker's per-claim context spend. Tracked under Observability Metrics as 'web-research delegation rate'.

## Steps

### 1. Initial Validation (Sequential)

Run plan validation to identify completeness, accuracy, and hallucination issues.

**Agent**: `plan-checker`

- **Args**: `scope: {input.scope}`
- **Output**: `{audit-report-1}` - Initial audit report in `generated-reports/`

**Validation scope** (per `plan-checker` Steps 0-7 + 5b/5c/5d/5e/5f):

- Structure (folder name, file layout, mandatory sections)
- Requirements (BRD + PRD content placement, Gherkin)
- Technical documentation (architecture, design decisions, diagrams)
- Delivery checklist (granularity, TDD shape, execution-grade clarity)
- Operational readiness (Step 5b — quality gates, CI verification, env setup)
- Manual behavioral assertions (Step 5c — Playwright MCP / curl)
- Worktree specification (Step 5d — declared `## Worktree` section + path format)
- Execution-grade clarity (Step 5e — file paths, commands, acceptance criteria per checkbox)
- **Anti-hallucination scan** (Step 5f — confidence labels, Anti-Pattern Catalog AP-1 through AP-10, suggested-executor annotation validity, web-citation completeness) per the [Plan Anti-Hallucination Convention](../../development/quality/plan-anti-hallucination.md)

For external claims that are not already documented in the repo and require more than a single-shot URL fetch, `plan-checker` delegates research to [`web-research-maker`](../../../.claude/agents/web-research-maker.md) per the lower plan-content threshold (any non-grep'd external claim → delegate). See [Plan Anti-Hallucination Convention §Web-Research Delegation](../../development/quality/plan-anti-hallucination.md#web-research-delegation-lower-threshold-for-plans).

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

- If threshold-level findings > 0: Proceed to step 3 (reset `consecutive_zero_count` to 0)
- If threshold-level findings = 0: Initialize `consecutive_zero_count` to 1 (this check is the first zero),
  proceed to step 4 for confirmation re-check (consecutive pass requirement)

**Depends on**: Step 1 completion

**Notes**:

- Fix scope determined by mode level
- Below-threshold findings remain visible in audit reports
- Enables progressive quality improvement

### 3. Apply Fixes (Sequential, Conditional)

Apply all validated fixes from the audit report.

**Agent**: `plan-fixer`

- **Args**: `report: {step1.outputs.audit-report-1}, approved: all`
- **Output**: `{fixes-applied}`
- **Condition**: Findings exist from step 2
- **Depends on**: Step 2 completion

**Success criteria**: Fixer successfully applies all fixes without errors.

**On failure**: Log errors, proceed to step 4 for verification.

**Notes**:

- Fixer re-validates findings before applying (prevents false positives)
- Fixes ALL criticality levels: CRITICAL (blocking), HIGH (objective), MEDIUM (structural), LOW (style/formatting)
- Achieves perfect plan quality with zero findings

### 4. Re-validate (Sequential)

Run checker again to verify fixes resolved issues and no new issues introduced.

**Agent**: `plan-checker`

- **Args**: `scope: {input.scope}, uuid-chain: {previous-uuid-chain}, fix-report: {step3.outputs.fix-report}`
- **Output**: `{audit-report-N}` - Verification audit report
- **Depends on**: Step 3 completion

**Re-validation mode**: The UUID chain signals re-validation mode to the checker. The fix report provides the changed files list for scoped re-validation. The checker validates only changed plan files and reuses iteration 1's codebase inspection scope.

**Success criteria**: Checker completes validation.

**On failure**: Terminate workflow with status `fail`.

### 5. Iteration Control (Sequential)

Determine whether to continue fixing or terminate.

**Logic**:

- Count findings based on mode level in `{step4.outputs.audit-report-N}` (same as Step 2):
  - **lax**: Count CRITICAL only
  - **normal**: Count CRITICAL + HIGH
  - **strict**: Count CRITICAL + HIGH + MEDIUM
  - **ocd**: Count all levels (CRITICAL, HIGH, MEDIUM, LOW)
- Track `consecutive_zero_count` across iterations (resets to 0 when threshold-level findings > 0, increments when = 0)
- If consecutive_zero_count >= 2 AND iterations >= min-iterations (or min not provided): Proceed to step 6 (Success — double-zero confirmed)
- If consecutive_zero_count >= 2 AND iterations < min-iterations: Loop back to step 4 (re-validate)
- If consecutive_zero_count < 2 AND threshold-level findings = 0: Loop back to step 4 (confirmation check — no fix needed, just re-verify)
- If threshold-level findings > 0 AND max-iterations provided AND iterations >= max-iterations: Proceed to step 6 (Partial)
- If threshold-level findings > 0 AND (max-iterations not provided OR iterations < max-iterations): Loop back to step 3

**Depends on**: Step 4 completion

**Notes**:

- **Default behavior**: Runs up to 7 iterations (default max-iterations). Override with higher value for more attempts
- **Consecutive pass requirement**: Zero findings must be confirmed by a second independent check before declaring success
- **Convergence target**: Workflow should stabilize in 3-5 iterations with convergence safeguards (scoped re-validation, cached verification, false positive tracking)
- **Escalation threshold**: If findings count is not monotonically decreasing after iteration 5, log a warning: "Convergence not achieved — likely non-deterministic findings or scope expansion"
- **Optional min-iterations**: Prevents premature termination before sufficient iterations
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

- **lax**: Zero CRITICAL findings on 2 consecutive checks (HIGH/MEDIUM/LOW may exist)
- **normal**: Zero CRITICAL/HIGH findings on 2 consecutive checks (MEDIUM/LOW may exist)
- **strict**: Zero CRITICAL/HIGH/MEDIUM findings on 2 consecutive checks (LOW may exist)
- **ocd**: Zero findings at all levels on 2 consecutive checks

**Partial** (`partial`):

- Threshold-level findings remain after max-iterations safety limit

**Failure** (`fail`):

- Checker or fixer encountered technical errors

**Note**: Below-threshold findings are reported in final audit but don't prevent success status. Success requires two consecutive zero-finding validations (consecutive pass requirement).

## Example Usage

### Validate All Plans

```
User: "Run plan quality gate workflow for all plans"
```

The AI will invoke `plan-checker` and `plan-fixer` via the Agent tool:

- Validate all plan files (`plan-checker` delegated agent)
- Apply all fixes (`plan-fixer` delegated agent)
- Iterate until zero findings achieved

### Validate Specific Plan Folder

```
User: "Run plan quality gate workflow for plans/in-progress/"
```

The AI will invoke agents with scoped validation:

- Validate only in-progress plans
- Fix issues in those plans only
- Iterate until zero findings in scope

### Validate Single Plan

```
User: "Run plan quality gate workflow for plans/in-progress/new-feature/plan.md"
```

The AI will invoke agents with single-file scope:

- Validate specific plan file only
- Fix issues in that file
- Iterate until plan is clean

### With Iteration Bounds

```
User: "Run plan quality gate workflow for all plans with min-iterations=2 and max-iterations=10"
```

The AI will invoke agents with iteration controls:

- Require at least 2 check-fix cycles
- Cap at maximum 10 iterations
- Report final status after completion

## Iteration Example

Typical execution flow:

```
Iteration 1:
  Check (full scan + comprehensive codebase inspection) → 12 findings → Fix → captures changed files

Iteration 2:
  Check (scoped to changed files, cached verification) → 3 findings → Fix → captures changed files

Iteration 3:
  Check (scoped) → 0 findings (consecutive_zero: 1)

Iteration 4 (confirmation):
  Re-check (scoped) → 0 findings (consecutive_zero: 2 — double-zero confirmed)

Result: SUCCESS (4 iterations)
```

## Safety Features

**Infinite Loop Prevention**:

- max-iterations defaults to 7 (override with higher value for more attempts)
- When provided, workflow terminates with `partial` if limit reached
- Tracks iteration count for monitoring
- Escalation warning at iteration 5 if not converging

**Convergence Safeguards**:

- Checker loads `.known-false-positives.md` skip list at start of each iteration
- Fixer persists new FALSE_POSITIVEs to skip list after each run
- Re-validation uses scoped scan (changed files only) to prevent scope expansion
- Comprehensive codebase inspection on iteration 1 with locked scope on iterations 2+
- Factual claims verified in iteration 1 are cached, not re-verified with WebSearch
- Escalation after repeated checker-fixer disagreements on the same finding

**False Positive Protection**:

- Fixer re-validates each finding before applying
- Skips FALSE_POSITIVE findings automatically
- Progressive writing ensures audit history survives

**Error Recovery**:

- Continues to verification even if some fixes fail
- Reports which fixes succeeded/failed
- Generates final report regardless of status

## Plan-Specific Validation

The plan-checker validates:

- **Completeness**: All five canonical documents present in multi-file plans — `README.md`, `brd.md`, `prd.md`, `tech-docs.md`, `delivery.md`. Required sections populated in each file per the [Content-Placement Rules](../../conventions/structure/plans.md#content-placement-rules-brdmd-vs-prdmd). Single-file exception is allowed when the plan is trivially small (≤1000 lines) and a single `README.md` covers the nine mandatory sections: Context, Scope, Business Rationale (condensed BRD), Product Requirements (condensed PRD), Technical Approach, **Worktree**, Delivery Checklist, Quality Gates, Verification.
- **Technical Accuracy**: Commands, versions, tool names, API signatures verified via repo `Grep` first (free, fast, accurate); external claims verified via `web-research-maker` per the lower plan-content delegation threshold
- **Anti-Hallucination Scan**: Every non-trivial factual claim carries an inline confidence label (`[Repo-grounded]` / `[Web-cited]` / `[Judgment call]` / `[Unverified]`); zero violations of Anti-Pattern Catalog AP-1 through AP-10; every cited file path / Nx target / agent / skill resolves on the current commit. See [Plan Anti-Hallucination Convention](../../development/quality/plan-anti-hallucination.md).
- **Worktree Specification**: Plan contains a `## Worktree` section declaring the worktree path (`worktrees/<plan-identifier>/`) and provisioning command. See [Plans Organization Convention §Worktree Specification](../../conventions/structure/plans.md#worktree-specification).
- **Execution-Grade Clarity**: Every delivery checkbox names explicit file path(s), verbatim shell command(s), and a concrete acceptance criterion. See [Plans Organization Convention §Execution-Grade Clarity](../../conventions/structure/plans.md#execution-grade-clarity-hard-rule).
- **Implementation Readiness**: Plans are actionable and executable
- **Codebase Alignment**: References to existing files, patterns, and conventions
- **Clarity**: Clear problem statements, well-defined scope, unambiguous requirements
- **Operational Readiness** (CRITICAL): Plans must include all of the following:
  - **Local quality gates**: Steps to run affected tests, linting, typecheck locally before pushing (`nx affected -t typecheck lint test:quick spec-coverage`)
  - **Post-push CI verification**: Steps to monitor and verify GitHub Actions/workflows pass after pushing to main, with instructions to fix failures immediately
  - **Development environment setup**: Steps to set up the dev environment for the features being built (dependencies, env vars, DB, dev server)
  - **Fix-all-issues instruction**: Explicit instruction to fix ALL failures found during quality gates — including preexisting issues not caused by the current changes (root cause orientation principle)
  - **Thematic commit guidance**: Instruction to commit changes thematically with Conventional Commits format, splitting different domains/concerns into separate commits
  - **Manual behavioral assertions**: Steps to use Playwright MCP for web UI verification (navigate, snapshot, click, check console errors) and curl for API verification (hit endpoints, check responses, test error cases) — applicable when the plan touches UI or API code

## Final Audit Report Structure

The audit report emitted by `plan-checker` follows this structure:

1. **Report metadata** — report ID (UUID chain), date, plan path, mode, iteration number
2. **Scope** — which plan documents were checked (README, brd, prd, tech-docs, delivery)
3. **Findings by criticality** — CRITICAL → HIGH → MEDIUM → LOW, each with:
   - Finding ID
   - Category (structure, requirements, anti-hallucination, acceptance-criteria, etc.)
   - Confidence level (HIGH / MEDIUM / FALSE_POSITIVE)
   - Description and suggested fix
4. **Executive summary** — findings count by criticality, consecutive-zero count, pass/fail verdict
5. **Links to related reports** — previous iteration report (if any), plan quality gate report

## Observability Metrics

Track across executions:

- **Iterations-to-convergence per mode** — how many check-fix cycles needed per mode level
- **Anti-hallucination violations by category** — AP-1 through AP-10 breakdown (from plan-checker Step 5f output)
- **Web-research delegation rate** — count of `web-research-maker` invocations per audit; higher rate indicates more external fact-checking
- **AI tokens spent on validation** — measure cost per plan audit

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

## Principles Implemented/Respected

- PASS: **Explicit Over Implicit**: All steps, conditions, and termination criteria are explicit
- PASS: **Automation Over Manual**: Fully automated validation and fixing without human intervention
- PASS: **Simplicity Over Complexity**: Clear linear flow with loop control
- PASS: **Accessibility First**: Generates human-readable audit reports
- PASS: **Progressive Disclosure**: Can run with different scopes and iteration limits
- PASS: **No Time Estimates**: Focus on quality outcomes, not duration

## Conventions Implemented/Respected

- **[File Naming Convention](../../conventions/structure/file-naming.md)**: Workflow file follows plain name convention for workflows
- **[Linking Convention](../../conventions/formatting/linking.md)**: All cross-references use GitHub-compatible markdown with `.md` extensions
- **[Content Quality Principles](../../conventions/writing/quality.md)**: Active voice, proper heading hierarchy, single H1
- **[Plans Organization Convention](../../conventions/structure/plans.md)**: Workflow validates the five-document structure and worktree section per the convention
- **[Plan Anti-Hallucination Convention](../../development/quality/plan-anti-hallucination.md)**: plan-checker's Step 5f enforces this convention's recipes, confidence labels, and Anti-Pattern Catalog
