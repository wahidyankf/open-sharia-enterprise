---
name: repo-cross-vendor-parity-quality-gate
goal: Validate cross-vendor behavioral-parity invariants and apply fixes iteratively until zero findings achieved
termination: "Zero findings on two consecutive validations (max-iterations defaults to 7, escalation warning at 5)"
inputs:
  - name: scope
    type: string
    description: 'Subset of invariants to validate (e.g., "all", "governance", "sync", "counts", "maps"). Defaults to all five invariants.'
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
    pattern: generated-reports/parity__*__audit.md
    description: Final audit report
---

# Repository Cross-Vendor Parity Quality Gate Workflow

**Purpose**: Automatically validate cross-vendor behavioral-parity invariants between
the canonical primary platform binding (`.claude/`), the secondary platform binding
(`.opencode/`), the canonical root instruction surfaces (`AGENTS.md`, `CLAUDE.md`),
and `governance/` prose, then apply fixes iteratively until all findings resolve.

This is the structural parallel to the
[`plan-quality-gate` workflow](../plan/plan-quality-gate.md) but scoped to repository
cross-vendor parity rather than plan completeness.

## Execution Mode

**Preferred Mode**: Agent Delegation — invoke `repo-parity-checker` and `repo-parity-fixer`
via the Agent tool with `subagent_type` (see
[Workflow Execution Modes Convention](../meta/execution-modes.md)).

**Fallback Mode**: Manual Orchestration — execute workflow logic directly using
Bash/Read/Edit tools when Agent Delegation is unavailable.

**How to Execute**:

```
User: "Run repo cross-vendor parity quality gate workflow"
```

The orchestrator will:

1. Invoke `repo-parity-checker` via the Agent tool (runs the five invariant validations,
   writes audit report)
2. Invoke `repo-parity-fixer` via the Agent tool (reads audit, auto-fixes sync drift only,
   flags other findings for human resolution)
3. Iterate until zero findings achieved on two consecutive validations
4. Show git status with modified files
5. Wait for user commit approval

**Fallback (Manual Mode)**:

```
User: "Run repo cross-vendor parity quality gate workflow in manual mode"
```

The orchestrator executes the validation script directly via
`bash apps/rhino-cli/scripts/validate-cross-vendor-parity.sh` (or equivalently
`nx run rhino-cli:validate:cross-vendor-parity`) and addresses any findings inline.

**When to use**:

- After creating or modifying agents in `.claude/agents/`
- After modifying governance prose, `AGENTS.md`, or `CLAUDE.md`
- After modifying the binding-sync logic in `apps/rhino-cli/internal/agents/`
- Periodically as a parity audit (the same target runs in `.husky/pre-push` for any
  push that touches one of these surfaces)

## Steps

### 1. Initial Validation (Sequential)

Run cross-vendor parity validation to identify findings across the five invariants.

**Agent**: `repo-parity-checker`

- **Args**: `scope: {input.scope}, mode: {input.mode}`
- **Output**: `{audit-report-1}` — Initial audit report in `generated-reports/parity__<uuid>__<timestamp>__audit.md`

**Success criteria**: Checker completes and generates audit report.

**On failure**: Terminate workflow with status `fail`.

### 2. Check for Findings (Sequential)

Analyze the audit report to determine if fixes are needed.

**Condition check**: Count findings in `{audit-report-1}` according to the configured
`mode`:

- `lax` — count CRITICAL only
- `normal` — count CRITICAL + HIGH
- `strict` (default) — count CRITICAL + HIGH + MEDIUM
- `ocd` — count CRITICAL + HIGH + MEDIUM + LOW

If 0 findings → proceed to step 6 (require a second consecutive zero before terminating).

If &gt;0 findings → proceed to step 3.

### 3. Apply Fixes (Sequential, Conditional)

Apply auto-remediable fixes from the audit report.

**Agent**: `repo-parity-fixer`

- **Args**: `audit-report: {audit-report-N}`
- **Output**: `{fix-report-N}` — Fixer report listing what was auto-fixed and what was
  flagged for human resolution

**Auto-fixable scope** (per `repo-parity-fixer` definition):

- Invariant 3 — binding sync drift (re-run `npm run sync:claude-to-opencode` and stage)

**Out-of-scope** (fixer flags and exits non-zero — orchestrator escalates to human):

- Invariants 1, 2, 4, 5 (governance prose, AGENTS/CLAUDE, count divergence, map gaps)
- Invariant 6 advisory (platform-bindings catalog drift)

**On out-of-scope findings**: surface in the orchestrator's user-visible status with the
finding details from the audit report; do not loop further until the human resolves.

### 4. Re-Validate (Sequential)

Run validation again to confirm the fixer's auto-fixes resolved findings without
introducing new ones.

**Agent**: `repo-parity-checker`

- **Args**: `scope: {input.scope}, mode: {input.mode}`
- **Output**: `{audit-report-N+1}`

### 5. Iteration Control (Sequential)

Determine whether to continue or terminate.

**Logic**:

- If findings &gt; 0 AND iterations &lt; max-iterations → loop back to step 3
- If findings &gt; 0 AND iterations ≥ max-iterations → terminate with status `partial`
- If findings = 0 → require a second consecutive zero (proceed to step 6)
- If iteration count reaches 5 → emit an escalation warning to the orchestrator's
  user-visible output (matches the plan-quality-gate convergence safeguard)

### 6. Double-Zero Termination (Sequential)

Re-run validation one more time to confirm the zero-finding state is stable (mirrors
plan-quality-gate's double-zero termination — guards against transient
false-positive-skip cycles where the checker's first zero was an artifact of a fixer
edit that the next checker run would re-flag).

**Agent**: `repo-parity-checker`

- **Args**: `scope: {input.scope}, mode: {input.mode}`
- **Output**: `{audit-report-final}`

**Success criteria**:

- Two consecutive zero-finding validations
- Fixer flagged no out-of-scope findings remaining

**On non-zero second pass**: loop back to step 3 (counts as a regular iteration; do not
double-count the previous zero).

## Termination Criteria

- **Success** (`pass`): Two consecutive zero-finding validations, all auto-fixable
  drift remediated, fixer reported no remaining out-of-scope findings.
- **Partial** (`partial`): `max-iterations` reached without double-zero, or fixer
  reports out-of-scope findings that require human resolution.
- **Failure** (`fail`): Checker or fixer encountered a tooling error
  (e.g., `rhino-cli` build failure, missing `npm` dependency).

## Convergence Safeguards (mirror plan-quality-gate)

- **False-positive skip list**: when the orchestrator decides a checker finding is a
  false-positive (e.g., regex collision on `Llama` / `Devin` / `Grok`), it records the
  decision in the audit report so subsequent iterations don't re-flag the same line.
- **Scoped re-validation**: if the fixer only touched `.opencode/agents/`, the
  re-validation may scope to invariants 3 and 4; full-scope re-validation runs at the
  final double-zero check.
- **Escalation warning at iteration 5**: emit a one-line warning so the human knows the
  loop is approaching `max-iterations`; surface the most-frequent finding category as
  context.

## Limited Auto-Fix Scope (Documented Constraint)

This workflow's fixer is intentionally narrow. Only **binding sync drift** is
auto-remediable, because it has a deterministic, idempotent fix
(`npm run sync:claude-to-opencode`). The other invariants require human judgment:

- Governance prose violations require choosing between rewrite, fence, or heading
  allowlist per the convention's Migration Guidance.
- Count divergence requires deciding between deleting an orphan and authoring a missing
  agent counterpart — both have product implications.
- Translation-map gaps require deciding what role / capability tier the new value
  represents.
- platform-bindings catalog drift requires reading current vendor documentation and writing a
  catalog update.

When the fixer flags out-of-scope findings, the workflow surfaces them with full
context and exits, rather than guessing.

## Iteration Example

Typical execution flow when the only outstanding issue is sync drift:

```
Step 1: Initial validation
  Iteration 1 → 1 finding (Invariant 3, sync drift)

Step 3: Apply fixes
  Fixer runs npm run sync:claude-to-opencode → 70 agents converted
  Stages .opencode/agents/<changed>.md

Step 4: Re-validate
  Iteration 2 → 0 findings

Step 6: Double-zero check
  Iteration 3 → 0 findings → CONFIRMED

Result: SUCCESS, 3 iterations
```

Typical flow when count divergence is detected:

```
Step 1: Initial validation
  Iteration 1 → 1 finding (Invariant 4, count mismatch)

Step 3: Apply fixes
  Fixer flags out-of-scope; emits "human action required: investigate orphan
  in .opencode/agents/<name>.md OR author .claude counterpart"

Result: PARTIAL after 1 iteration; user must resolve before re-running.
```

## Safety Features

- **Bounded iterations**: `max-iterations` (default 7) prevents runaway loops.
- **Idempotent auto-fix**: the only auto-fix (sync) is provably idempotent — running it
  twice produces the same output.
- **Read-only checker**: `repo-parity-checker` has no `Edit` tool; it can only Read,
  Glob, Grep, Bash (for the validation script and rhino-cli invocations), WebFetch,
  and Write (for the audit report).
- **Validation history**: every iteration's audit report is preserved in
  `generated-reports/` for post-mortem and regression analysis.

## Related Conventions

- [Governance Vendor-Independence Convention](../../conventions/structure/governance-vendor-independence.md)
- [Workflow Naming Convention](../../conventions/structure/workflow-naming.md) — this
  workflow's filename `repo-cross-vendor-parity-quality-gate` follows the
  `<scope>(-<qualifier>)*-<type>` rule (scope `repo` matches parent dir
  `governance/workflows/repo/`).
- [Maker-Checker-Fixer Pattern](../../development/pattern/maker-checker-fixer.md)
- [Plan Quality Gate Workflow](../plan/plan-quality-gate.md) — structural parallel for
  plan-completeness validation

## Conventions Implemented/Respected

- **[File Naming Convention](../../conventions/structure/file-naming.md)**: file uses
  kebab-case.
- **[Linking Convention](../../conventions/formatting/linking.md)**: cross-references
  use GitHub-compatible markdown with `.md` extensions.
- **[Content Quality Principles](../../conventions/writing/quality.md)**: active voice,
  proper heading hierarchy, single H1.
