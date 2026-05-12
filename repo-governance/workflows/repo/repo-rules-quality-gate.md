---
name: repo-rules-quality-gate
goal: Validate repository consistency across all layers, apply fixes iteratively until zero findings achieved
termination: "Zero findings on two consecutive validations (max-iterations defaults to 7, escalation warning at 5)"
inputs:
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
    pattern: generated-reports/repo-rules__*__*__audit.md
    description: Final audit report (4-part format with UUID chain)
  - name: execution-scope
    type: string
    description: Scope identifier for UUID chain tracking (default "repo-rules")
    required: false
---

# Repository Rules Validation Workflow

**Purpose**: Automatically validate repository consistency across principles, conventions, development practices, agent and skill source definitions, and subdirectory README files, then apply fixes iteratively until all issues are resolved.

**IMPORTANT - Scope Clarification**:

This workflow validates **source definitions only**. Source includes governance docs, primary agent definitions, and primary skill packages — all of which live under version control and are authored by hand. It does NOT validate generated directories:

- PASS: **Validates**: `repo-governance/` (principles, conventions, development practices, workflows, vision)
- PASS: **Validates**: `.claude/agents/` (primary agent source definitions — agent-to-agent duplication, agent-Skill duplication, frontmatter compliance)
- PASS: **Validates**: `.claude/skills/` (primary agent-skill source — agent-skill-to-agent-skill consolidation opportunities, agent-skill content quality). Agent skills are NOT mirrored to secondary bindings — primary binding skill packages are read natively by all supporting coding-agent platforms, so `.claude/skills/` IS the source of truth and IS in scope.
- PASS: **Validates (partial)**: `docs/explanation/` (Diátaxis tree — preflight frontmatter audit covers tutorial / how-to / reference / explanation per the Diátaxis schema; software-engineering subtree validated by Step 8 in the AI checker for principle alignment, README index accuracy, and version documentation) and `docs/explanation/README.md` (Diátaxis explanation index — Step 1 Rules Governance scope) and `docs/explanation/software-engineering/` (~265 files / 345k lines — Step 8 dedicated validation: governance-principle alignment, cross-reference completeness, file naming, document structure, template completeness, diagram accessibility, README index accuracy, version documentation).
- FAIL: **Skips**: the rest of `docs/` (`docs/tutorials/`, `docs/how-to/`, `docs/reference/`, `docs/explanation/` non-software-engineering subtrees, `docs/metadata/`) — out of scope for this workflow today; validated by the specialized `docs/` agent family (`docs-checker`, `docs-tutorial-checker`, `docs-link-checker`, `docs-software-engineering-separation-checker`). Extending coverage to all of `docs/` is a backlog item — see Backlog below.
- FAIL: **Skips**: secondary platform binding agent directories (e.g., `.opencode/agents/`) — auto-generated from `.claude/agents/` via `npm run sync:claude-to-opencode`. Validate via the sync script + `validate:cross-vendor-parity` Nx target, not this workflow.

**Generated Output Validation**: Use CLI validation commands for validating generated content. This workflow ensures SOURCE is correct, then sync commands validate output generation.

**When to use**:

- After making changes to conventions, principles, or development practices
- Before major releases or deployments
- Periodically to ensure repository health
- After adding or modifying agents

## Execution Mode

**Preferred Mode**: Agent Delegation — invoke `repo-rules-checker` and
`repo-rules-fixer` via the Agent tool with `subagent_type`
(see [Workflow Execution Modes Convention](../meta/execution-modes.md)).

**Fallback Mode**: Manual Orchestration — execute workflow logic directly using
Read/Write/Edit tools when Agent Delegation is unavailable.

The Agent tool runs delegated agents that persist file changes to the actual filesystem, making it
the preferred approach when these agents exist as defined delegated agent types.

**How to Execute**:

```
User: "Run repository rules quality gate workflow in normal mode"
```

The AI will:

0. Build the rhino-cli binary if missing (`nx build rhino-cli`), then run deterministic preflight (Step 0.5) capturing the JSON envelope to `generated-reports/`.
1. Invoke `repo-rules-checker` via the Agent tool (reads governance files, writes audit)
2. Invoke `repo-rules-fixer` via the Agent tool (reads audit, applies fixes, writes fix report)
3. Iterate until zero findings achieved
4. Show git status with modified files
5. Wait for user commit approval

**Fallback (Manual Mode)**:

```
User: "Run repository rules quality gate workflow in manual mode"
```

The AI executes checker and fixer logic directly using Read/Write/Edit tools in the main
context — use this when agent delegation is unavailable.

## Steps

### 0.5. Deterministic Preflight (Sequential)

Run the `rhino-cli` orchestrator to harvest all deterministic governance findings before invoking the AI checker. Deterministic categories (file naming, frontmatter shape, license presence, README index integrity, emoji codepoints, layer-coherence, doc heading hierarchy, agent-skill verbatim duplication) execute in milliseconds and cache via Nx; the AI checker then runs only the AI-only categories (paraphrased duplication, semantic contradictions, terminology alignment, principle-appropriateness judgement).

**Why Step 0.5 (and not Step 1, renumbering everything down)**: This step was inserted between the pre-existing Step 1 (Initial Validation) and the workflow start. Decimal numbering preserves the existing Step 1-6 references in the checker/fixer prompts that pre-date the preflight. The [Workflow Identifier Convention](../../../repo-governance/workflows/meta/workflow-identifier.md) explicitly allows sub-step decimals for non-disruptive insertions.

**Command**:

```bash
mkdir -p generated-reports
./apps/rhino-cli/dist/rhino-cli repo-governance audit -o json > generated-reports/repo-governance-audit__{uuid}__{timestamp}.json
```

The binary must be built first via `nx build rhino-cli`; the prebuilt path is `apps/rhino-cli/dist/rhino-cli`.

> **Recommendation**: Pin `RHINO_AUDIT_NOW=<RFC3339>` per workflow run to enable the SHA-256 hash-reuse optimization (the `ran_at` field is derived from this env var; without it the timestamp defaults to `time.Now()` and the hash always changes). See [`apps/rhino-cli/README.md`](../../../apps/rhino-cli/README.md#environment-variables) for details.

- **Output**: `{preflight-report}` — JSON envelope at the captured path; schema `rhino-cli/repo-governance-audit/v1`
- **Exit handling**:
  - Exit 0 (clean): All deterministic categories pass; pass JSON path to checker.
  - Exit 1 (findings): Deterministic findings present; pass JSON path to checker (the checker incorporates the deterministic findings verbatim into the final audit's "Deterministic Findings (rhino-cli preflight)" section).
  - Exit 2 (invocation error): Terminate workflow with `fail` status. **Debugging hint**: Re-run with `./apps/rhino-cli/dist/rhino-cli repo-governance audit -o text` for human-readable diagnostic. Common causes: missing binary (rebuild via `nx build rhino-cli`); broken category function (run individual `dist/rhino-cli repo-governance <category>` to isolate).

> **Operator hatch**: If the calibrated emoji-audit (rhino-cli v0.16.1 expanded skip-dirs) still reports legacy-tree findings the operator needs to bypass, pass `--skip emoji-audit` to the orchestrator. This is a backup hatch — the primary remedy is to add the missing dir to `emojiSkipDirs` or to pass `--exclude <glob>`.

**Success criteria**: Preflight completes; JSON file exists at expected path; JSON parses as valid `AuditEnvelope` with `schema` field set to `rhino-cli/repo-governance-audit/v1`.

**Depends on**: None (first step in each iteration). Runs again before every re-validation iteration; if the JSON SHA-256 is unchanged from the prior iteration, the checker reuses the deterministic findings section unchanged and only re-evaluates AI-only categories.

### 1. Initial Validation (Sequential)

Run repository-wide consistency check to identify all issues.

**Agent**: `repo-rules-checker`

- **Args**: `scope: all, EXECUTION_SCOPE: repo-rules, preflight-report: {step0_5.outputs.preflight-report}`
- **Output**: `{audit-report-1}` - Initial audit report in `generated-reports/` (4-part format: `repo-rules__{uuid-chain}__{timestamp}__audit.md`)

**UUID Chain Tracking**: Checker generates 6-char UUID and writes to `generated-reports/.execution-chain-repo-rules` before spawning any child agents. See [Temporary Files Convention](../../development/infra/temporary-files.md#uuid-generation) for details.

**Note on preflight unavailability**: If the `preflight-report` argument is missing, the file does not exist, or the JSON fails schema validation, the AI checker falls back to full Steps 1-8 evaluation per its own Step 0.5 graceful-degradation rule (`.claude/agents/repo-rules-checker.md`). This is NOT a workflow failure — the checker logs a `[WARN]` in the audit report and the workflow proceeds. Only an Exit 2 from rhino-cli itself (broken binary, missing dependency) terminates the workflow with `fail`.

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

These below-threshold rules apply to AI-only findings; deterministic findings from preflight follow the separate visibility-only rule defined above in the Step 2 Condition Check (deterministic findings are reported in the `## Deterministic Findings (rhino-cli preflight)` section of the audit but NEVER count toward the mode threshold regardless of their criticality).

- **lax**: HIGH/MEDIUM/LOW reported, not counted
- **normal**: MEDIUM/LOW reported, not counted
- **strict**: LOW reported, not counted
- **ocd**: All findings counted

Deterministic findings (those from the rhino-cli preflight) are reported in the audit but do NOT count toward the mode threshold. They are managed via the `generated-reports/.known-false-positives.md` skip-list outside the iteration loop. Only AI-only findings count toward the mode threshold.

**Decision**:

- If threshold-level findings > 0: Proceed to step 3 (reset `consecutive_zero_count` to 0)
- If threshold-level findings = 0: Initialize `consecutive_zero_count` to 1 (this check is the
  first zero), proceed to step 4 for confirmation re-check (consecutive pass requirement)

**Depends on**: Step 1 completion

**Notes**:

- Fix scope determined by mode level
- Below-threshold findings remain visible in audit reports
- Enables progressive quality improvement

### 3. Apply Fixes (Sequential, Conditional)

Apply validated fixes from the audit report based on mode level.

**Agent**: `repo-rules-fixer`

- **Args**: `report: {step1.outputs.audit-report-1}, approved: all, mode: {input.mode}, EXECUTION_SCOPE: repo-rules`
- **Output**: `{fixes-applied}` - Fix report with same UUID chain as source audit
- **Condition**: Threshold-level findings exist from step 2
- **Depends on**: Step 2 completion

**Success criteria**: Fixer successfully applies all threshold-level fixes without errors.

**On failure**: Log errors, proceed to step 4 for verification.

**Notes**:

- Fixer re-validates findings before applying (prevents false positives)
- **Fix scope based on mode**:
  - **lax**: Fix CRITICAL only (skip HIGH/MEDIUM/LOW)
  - **normal**: Fix CRITICAL + HIGH (skip MEDIUM/LOW)
  - **strict**: Fix CRITICAL + HIGH + MEDIUM (skip LOW)
  - **ocd**: Fix all levels (CRITICAL, HIGH, MEDIUM, LOW)
- Below-threshold findings remain untouched

### 4. Re-validate (Sequential)

Re-run the deterministic preflight (Step 0.5) first, then invoke the AI checker. If the preflight JSON SHA-256 is unchanged from the prior iteration, the checker reuses the deterministic findings section unchanged and only re-evaluates AI-only categories.

**Preflight re-run**:

```bash
mkdir -p generated-reports
./apps/rhino-cli/dist/rhino-cli repo-governance audit -o json > generated-reports/repo-governance-audit__{uuid}__{timestamp}.json
```

The binary must be built first via `nx build rhino-cli`; the prebuilt path is `apps/rhino-cli/dist/rhino-cli`.

**Agent**: `repo-rules-checker`

- **Args**: `scope: all, preflight-report: {step4.preflight.outputs.preflight-report}`
- **Output**: `{audit-report-N}` - Verification audit report
- **Depends on**: Step 3 completion

**Note on preflight unavailability**: If the `preflight-report` argument is missing, the file does not exist, or the JSON fails schema validation, the AI checker falls back to full Steps 1-8 evaluation per its own Step 0.5 graceful-degradation rule (`.claude/agents/repo-rules-checker.md`). This is NOT a workflow failure — the checker logs a `[WARN]` in the audit report and the workflow proceeds. Only an Exit 2 from rhino-cli itself (broken binary, missing dependency) terminates the workflow with `fail`.

**Success criteria**: Checker completes validation.

**On failure**: Terminate workflow with status `fail`.

### 5. Iteration Control (Sequential)

Determine whether to continue fixing or terminate.

**Logic**:

- Count findings based on mode level in {step4.checker.outputs.audit-report-N} (same as Step 2):
  - **lax**: Count CRITICAL only
  - **normal**: Count CRITICAL + HIGH
  - **strict**: Count CRITICAL + HIGH + MEDIUM
  - **ocd**: Count all levels
- Track `consecutive_zero_count` across iterations (resets to 0 when threshold-level findings > 0, increments when = 0)
- If consecutive_zero_count >= 2 AND iterations >= min-iterations (or min not provided): Proceed to step 6 (Success — double-zero confirmed)
- If consecutive_zero_count >= 2 AND iterations < min-iterations: Loop back to step 4 (re-validate)
- If consecutive_zero_count < 2 AND threshold-level findings = 0: Loop back to step 4 (confirmation check — no fix needed, just re-verify)
- If threshold-level findings > 0 AND max-iterations provided AND iterations >= max-iterations: Proceed to step 6 (Partial)
- If threshold-level findings > 0 AND (max-iterations not provided OR iterations < max-iterations): Loop back to step 3

**Below-threshold findings**: Continue to be reported in audit but don't affect iteration logic

Deterministic findings (those from the rhino-cli preflight) are reported in the audit but do NOT count toward the mode threshold. They are managed via the `generated-reports/.known-false-positives.md` skip-list outside the iteration loop. Only AI-only findings count toward the mode threshold.

**Depends on**: Step 4 completion

**Notes**:

- **Default behavior**: Runs up to 7 iterations (default max-iterations). Override with higher value for more attempts
- **Consecutive pass requirement**: Zero findings must be confirmed by a second independent check before declaring success
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

- Technical errors during check or fix

**Note on deterministic findings**: Deterministic findings from preflight are reported in the audit's `## Deterministic Findings (rhino-cli preflight)` section but do NOT count toward any mode threshold per Step 2's visibility-only rule. Two consecutive zero-finding validations refers to AI-only findings only.

**Note**: Below-threshold findings are reported in final audit but don't prevent success status. Success requires two consecutive zero-finding validations (consecutive pass requirement).

## Example Usage

### Standard Invocation (Normal Strictness)

```
User: "Run repository rules quality gate workflow in normal mode"
```

The AI will invoke specialized agents via the Agent tool:

- Validate repository consistency (`repo-rules-checker` delegated agent)
- Apply fixes for CRITICAL/HIGH findings (`repo-rules-fixer` delegated agent)
- Iterate until zero CRITICAL/HIGH findings achieved
- Report MEDIUM/LOW findings without fixing them

### Pre-Release Validation (Strict Mode)

```
User: "Run repository rules quality gate workflow in strict mode"
```

The AI will invoke agents with stricter criteria:

- Fix CRITICAL/HIGH/MEDIUM findings
- Report LOW findings without fixing them
- Iterate until zero CRITICAL/HIGH/MEDIUM findings achieved

### Comprehensive Audit (OCD Mode)

```
User: "Run repository rules quality gate workflow in ocd mode"
```

The AI will invoke agents with zero-tolerance criteria:

- Fix ALL findings (CRITICAL, HIGH, MEDIUM, LOW)
- Iterate until zero findings at all levels
- Equivalent to pre-mode parameter behavior

### With Iteration Bounds

```
User: "Run repository rules quality gate workflow in normal mode with min-iterations=2 and max-iterations=10"
```

The AI will invoke agents with iteration controls:

- Require at least 2 check-fix cycles
- Cap at maximum 10 iterations to prevent infinite loops
- Report final status (pass/partial) after completion

## Iteration Example

Typical execution flow:

```
Step 0.5: Preflight (cold) → 934 deterministic findings emitted to generated-reports/ (visibility only)

Iteration 1:
  Step 0.5: Preflight (cached, RHINO_AUDIT_NOW=...) → same 934 findings (hash match, skip re-eval)
  Step 1: AI checks → 5 AI-only findings
  Steps 2-5: Fixer addresses 3 findings

Iteration 2:
  Step 0.5: Preflight (cached) → same 934 findings
  Step 1: AI checks → 2 AI-only findings remaining
  Steps 2-5: Fixer addresses 2 findings

Iteration 3:
  Step 0.5: Preflight (cached) → same 934 findings
  Step 1: AI checks → 0 AI-only findings (consecutive_zero=1)

Iteration 4:
  Step 0.5: Preflight (cached) → same 934 findings
  Step 1: AI checks → 0 AI-only findings (consecutive_zero=2)

Result: PASS (double-zero AI-only; deterministic findings documented in skip-list)
```

## Safety Features

**Infinite Loop Prevention**:

- max-iterations defaults to 7 (override with higher value for more attempts)
- When provided, workflow terminates with `partial` if limit reached
- Tracks iteration count for monitoring
- Escalation warning at iteration 5 if not converging

**Convergence Safeguards**:

- Preflight SHA-256 hash reuse: when `RHINO_AUDIT_NOW` is pinned per the Step 0.5 recommendation, identical repo state across iterations produces identical preflight JSON. The checker detects this and skips re-evaluating deterministic categories (reuses prior iteration's `## Deterministic Findings` section verbatim) — concentrates AI-token spend on AI-only categories.
- Checker loads `.known-false-positives.md` skip list at start of each iteration
- Fixer persists new FALSE_POSITIVEs to skip list after each run
- Re-validation uses scoped scan (changed files only) to prevent scope expansion
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

## Skip-list Curation Rules

The skip-list at `generated-reports/.known-false-positives.md` filters out known intentional findings from the preflight deterministic categories.

**Who maintains it**: The repository maintainer who runs the quality gate.

**When to add vs fix**: Add a skip-list entry only when a finding is genuinely intentional — test fixtures, archived legacy content, third-party vendored content. Fix every other finding at the source.

**Per-entry schema**:

- `key`: category | path | finding signature (matches the `key` field in the preflight JSON)
- `rationale`: why this is intentional
- `date accepted`: ISO 8601 date
- `approver`: GitHub handle

**Per-category triage priority**:

1. CRITICAL findings first — fix or escalate, never skip
2. Vendor-audit findings second — fix at source or skip with explicit rationale
3. Everything else last — curate by category

### Deterministic findings → skip-list pipeline

On each iteration, every preflight finding NOT already in `generated-reports/.known-false-positives.md` lands in the audit's `## Deterministic Findings (rhino-cli preflight)` section. The maintainer reviews each entry between workflow runs and either (a) fixes the underlying issue (one-time, removes the finding for future runs) OR (b) appends an explicit skip-list entry with rationale + date + approver. Findings never auto-migrate to the skip-list — every entry requires explicit operator approval.

## Related Workflows

This workflow can be composed with:

- Deployment workflows (validate before deploy)
- Release workflows (audit before version bump)
- Content creation workflows (validate after bulk changes)

## Observability Metrics

Track across executions:

- **Preflight cold-run latency**: Target < 120 seconds for initial run
- **Preflight cached-run latency**: Target < 5 seconds with `RHINO_AUDIT_NOW` pin (SHA-256 hash match avoids re-evaluation)
- **AI tokens spent on Step 1+ vs deterministic findings count**: Ratio of AI token cost to mechanical findings already caught by preflight
- **AI-only-to-deterministic finding ratio**: Persistent AI-only majority signals candidate categories to refactor from AI to deterministic
- **Iterations-to-convergence per mode**: How many check-fix cycles needed per mode level
- **Average iterations to completion**: How many cycles typically needed
- **Success rate**: Percentage reaching zero findings
- **Common finding categories**: What issues appear most often
- **Fix success rate**: Percentage of fixes applied without errors

**Target ratio**: ≥80% of findings should be DETERMINISTIC (mechanical / encoded predicates) after Phase 3 stabilizes; <20% AI-only findings indicates the deterministic preflight is catching the bulk of issues. A persistent AI-only majority signals candidate categories to refactor from AI to deterministic per the [Deterministic vs AI Validation Split Convention](../../../repo-governance/conventions/structure/deterministic-vs-ai-validation-split.md) §'When to refactor from AI to deterministic'.

## Notes

- **Fully automated**: No human checkpoints, runs to completion
- **Idempotent**: Safe to run multiple times, won't break working state. Byte-deterministic output across runs only when `RHINO_AUDIT_NOW=<RFC3339>` is pinned; without the pin, `ran_at` in the preflight JSON varies per run (logical findings are still identical).
- **Conservative**: Fixer skips uncertain changes (preserves correctness)
- **Observable**: Generates audit reports for every iteration
- **Bounded**: Max-iterations prevents runaway execution

**Concurrency**: The preflight (Step 0.5) is a single binary invocation and is intrinsically parallel-safe — multiple consumers can run preflight against the same repo state without contention. Validation and fixing (Steps 1-5) are sequential. The `max-concurrency` parameter is reserved for future enhancements where multiple AI-checker validation dimensions could run concurrently against a shared preflight JSON.

**Note**: "agents" in this context refers to agent SOURCE definitions in the primary binding directory (e.g., `.claude/agents/`) — secondary directories (e.g., `.opencode/agents/`) are auto-generated.

This workflow ensures repository consistency through iterative validation and fixing, making it ideal for maintenance and quality assurance.

## Backlog

- Extend `repo-rules-checker` scope to all of `docs/` (tutorials, how-to, reference, explanation non-software-engineering subtrees, metadata) — currently delegated to the specialized `docs/` agent family (docs-checker, docs-tutorial-checker, docs-link-checker, docs-software-engineering-separation-checker); consolidation would simplify gate orchestration.

## Principles Implemented/Respected

- PASS: **Explicit Over Implicit**: All steps, conditions, and termination criteria are explicit
- PASS: **Automation Over Manual**: Fully automated validation and fixing without human intervention
- PASS: **Simplicity Over Complexity**: Clear linear flow with loop control
- PASS: **Accessibility First**: Generates human-readable audit reports
- PASS: **Progressive Disclosure**: Can run with different iteration limits
- PASS: **No Time Estimates**: Focus on quality outcomes, not duration

## Conventions Implemented/Respected

- **[Deterministic vs AI Validation Split Convention](../../conventions/structure/deterministic-vs-ai-validation-split.md)**: Step 0.5 preflight enforces the deterministic-vs-AI category split and the JSON envelope contract this workflow consumes; Step 0.5 implements the deterministic validation tier; AI checker (Steps 1-8) implements the AI-only tier
- **[File Naming Convention](../../conventions/structure/file-naming.md)**: Workflow file follows plain name convention for workflows
- **[Linking Convention](../../conventions/formatting/linking.md)**: All cross-references use GitHub-compatible markdown with `.md` extensions
- **[Content Quality Principles](../../conventions/writing/quality.md)**: Active voice, proper heading hierarchy, single H1

## What changed

Step 0.5 added 2026-05-12 referencing the archived `2026-05-12__optimize-repo-rules-quality-gate-with-rhino-cli` plan. Hardening edits (broken-command fix, visibility-only codification, hash-reuse documentation, arg-name unification, exit-2 recovery, Skip-list Curation Rules section, Observability Metrics section, Step-0.5 numbering rationale, emoji-audit operator hatch) added by this plan: `plans/in-progress/complete-repo-rules-zero-findings/`.
