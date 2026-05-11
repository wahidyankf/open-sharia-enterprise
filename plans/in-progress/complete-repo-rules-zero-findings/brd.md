# BRD — Complete Repo-Rules Zero Findings

## Business Goal

After landing the deterministic preflight in the prior plan (`2026-05-12__optimize-repo-rules-quality-gate-with-rhino-cli`, archived) [Repo-grounded — visible in recent commits `2508d5908` and `49f7df154`], the `repo-rules-quality-gate` workflow is now factually superior — preflight runs in milliseconds, caches via Nx, and frees the AI checker to focus on AI-only categories. But the gate has never actually converged to zero in production. The first strict-mode run after the preflight shipped produced ~4479 findings, of which most are noise rooted in three audit-side bugs and one outstanding refactor (agent dedup clusters). The business goal is **make `repo-rules-quality-gate strict` actually pass double-zero — both deterministic preflight AND AI-only findings — and harden both quality-gate workflow docs against the bugs observed in production use, so the next operator can rely on a green gate as a meaningful release signal**.

## Business Impact

### Pain Points Today

- **The strict gate cries wolf.** ~3772 of ~4479 findings [Judgment call — per-category split sourced from the user brief; Phase 0 re-confirms] come from three audit-side bugs (legacy emoji scans of `archived/` and `.next/`, frontmatter schema mismatching the actual Diátaxis values, fenced-code-block N-fence misparsing). Operators stop trusting the gate when 84%+ of findings are noise.
- **The workflow is hard to invoke correctly.** The `repo-rules-quality-gate.md` Step 0.5 + Step 4 command (`npx nx run rhino-cli:validate:repo-governance-audit -o json > out.json`) is broken — `-o json` parses as a positional Nx arg, not a forwarded flag, and nx wrapper output bleeds into stdout, corrupting the JSON envelope the checker consumes. This breaks the preflight contract silently.
- **The hash-reuse optimization never fires.** Step 0.5 claims a SHA-256 hash check skips redundant re-evaluation, but the default `time.Now()` for the envelope's `ran_at` field changes every run, so the hash always differs. The `RHINO_AUDIT_NOW` env var that pins this is undocumented in both the workflow and the rhino-cli README.
- **Step-2 finding counting is ambiguous.** The current text "Count findings based on mode level" applied literally to the deterministic preflight findings would make every iteration count thousands of HIGH findings against the threshold and the gate would never converge. The convention's actual intent is that deterministic findings are visibility-only — managed via skip-list curation, not iteration count — but this is not codified anywhere.
- **`plan-quality-gate.md` has a mode bug.** Inputs declare `mode: lax|normal|strict|ocd` but Step 2 says "Count ALL findings (CRITICAL, HIGH, MEDIUM, and LOW)". The mode parameter is ignored. Either honor the mode (consistent with the sibling workflow) or remove the mode input. The brief mandates honoring it.
- **368 agent dedup clusters block double-zero.** `agents-detect-duplication` flags 368 verbatim-or-near-verbatim duplicated patterns across maker/checker/fixer agents [Judgment call — count from the user brief; Phase 0 re-confirms]. Past attempts at normalization were vetoed because they erased agent-specific phrasing; the conservative parameterized-skill approach has not been tried.
- **Observability is anecdotal.** Neither workflow tracks preflight latency, AI-token spend, AI-vs-deterministic finding ratio, or iterations-to-convergence as standard metrics. Operators have no data to compare runs across modes or before/after calibrations.

### Expected Benefits

- **Trust in the gate.** When `repo-rules-quality-gate strict` exits clean, the operator knows it means clean — not "clean after subtracting 84% noise."
- **Reliable contract.** The Step 0.5 command produces a valid JSON envelope the checker can parse; hash-reuse triggers correctly; exit-2 recovery has a debugging hint.
- **Codified intent.** Deterministic-findings-as-visibility-only is written down, not folklore. Skip-list curation is governed by explicit rules.
- **Consistent quality-gate semantics.** `plan-quality-gate.md` and `repo-rules-quality-gate.md` honor `mode` the same way, so operators don't have to remember two different sets of semantics.
- **Honest agent dedup.** Conservative skill extraction collapses true boilerplate into parameterized skills without normalizing away any agent's intentional variation — every existing per-agent phrasing survives.
- **Provable observability.** Both workflows track measurable signals (preflight latency, finding ratios, iterations-to-convergence) so future calibrations can be data-driven.

## Affected Roles

This repo is solo-maintainer. The same person wears multiple hats; the maintainer cares about the work landing cleanly. The agents listed below are the actual consumers of the work, in the order they will touch it:

- **The maintainer (planning hat)** — uses this plan as a contract before kicking off execution.
- **The maintainer (execution hat)** — runs `plan-quality-gate` then `plan-execution` against this plan.
- **`plan-checker`** — validates the plan at authoring time; emits findings for any unmet structural rule, anti-hallucination violation, or operational-readiness gap.
- **`plan-fixer`** — applies fixes from `plan-checker` findings.
- **`plan-executor` / `plan-execution`** — runs delivery.md sequentially.
- **`repo-rules-checker`** — consumes the calibrated deterministic envelope plus its own AI-only categories during Phase 6 convergence.
- **`repo-rules-fixer`** — applies AI-side fixes after Phase 5's skill extraction reshapes the agent corpus.
- **`web-research-maker`** — invoked only if an external claim emerges during execution that is not already documented in the repo.
- **`swe-golang-dev`** — picks up Phase 1 edits (rhino-cli is Go). Mentioned as a suggested executor on Phase 1 checkboxes.
- **`agent-maker`** — picks up Phase 5 skill extraction (touches `.claude/agents/` and `.claude/skills/`).

## Business-Level Success Metrics

- **`repo-rules-quality-gate strict` pass status flips to `pass` with double-zero confirmation** [Repo-grounded — `pass` defined in `repo-rules-quality-gate.md` Termination Criteria as zero findings on two consecutive validations].
- **Baseline finding count drops from ~4479 to 0** at end of Phase 6. (Intermediate: Phase 1 target ≤1000; Phase 4 target ~residual genuine governance findings only; Phase 5 target zero `agents-detect-duplication` clusters.) [Judgment call: the 4479 → ≤1000 → 0 step pattern is a planning estimate based on per-category attribution in the user brief.]
- **`plan-quality-gate` and `repo-rules-quality-gate` mode handling is consistent** — both workflows honor `mode: lax|normal|strict|ocd` with the same threshold semantics, verifiable by reading both docs side-by-side after the plan lands. [Repo-grounded — current state inspected in both workflow files.]
- **The two workflows ship with observability sections** that name explicit metrics: preflight latency (cold + cached), AI-vs-deterministic finding ratio, iterations-to-convergence, AI-tokens spent.
- **Conservative-extraction safety: zero behavioral drift across all migrated agents**, proven by golden tests comparing rendered pre/post agent bodies after each batch.

The repo has no production-monitoring infrastructure for these metrics today; "track" here means documenting the metric names + how to derive them from existing audit reports, not building dashboards. Future plans may invest in dashboards if needed.

## Business-Scope Non-Goals

- **Building a metrics dashboard.** The plan only adds Observability Metrics _sections_ to the two workflows. Operators interpret them by reading audit reports.
- **Migrating the rest of `docs/` into `repo-rules-quality-gate`'s scope.** Tutorials, how-to, reference, and non-software-engineering explanation are validated by the dedicated `docs-*` agent family [Repo-grounded — see `repo-rules-quality-gate.md` lines 56-58].
- **Touching the parent `ose-projects` repo or its sibling `ose-infra` / `ose-primer`.** This plan operates entirely inside `ose-public`.
- **Building new agents or workflows.** Phase 5 extracts shared skills; it does not introduce new agents or workflows.
- **Defining "perfect" agent dedup as zero shared phrases.** Conservative parameterization keeps two-agent dup intact when phrasing is intentionally non-identical.
- **Re-running every governance audit category from scratch.** Audits that already pass cleanly stay untouched.

## Business Risks and Mitigations

| Risk                                                                                                              | Likelihood | Impact | Mitigation                                                                                                                                                                                                                                                                                             |
| ----------------------------------------------------------------------------------------------------------------- | ---------- | ------ | ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------ |
| Phase 5 skill extraction quietly normalizes per-agent phrasing                                                    | Medium     | High   | Conservative parameterization rule is encoded as the Phase 5 design constraint; behavioral-equivalence golden tests for 3 canonical agents gate every batch; batches are ≤5 agents and revertable per checkpoint commit.                                                                               |
| Phase 1 emoji-skip-dirs expansion masks real emoji violations in legitimate code                                  | Low        | Medium | Each new skip-dir is justified inline in the source comment + the rhino-cli README v0.16.1 entry; `archived/`, `.next/`, etc. are universally generated/legacy.                                                                                                                                        |
| Phase 1 Diátaxis frontmatter schema change rejects governance docs that previously passed                         | Low        | Medium | New schema is additive — accepts old `category: software` with a deprecation finding (warn, not fail) and the four Diátaxis values as fail-on-mismatch. Unit + integration tests cover both shapes.                                                                                                    |
| Phase 2 workflow doc changes break the `repo-rules-checker` agent's prompt expectations                           | Low        | High   | Arg-name unification picks the dominant existing form (`{step0_5.outputs.preflight-report}`) so the checker prompt does not need to change. No semantic change to the checker contract.                                                                                                                |
| Phase 3 mode-bug fix changes `plan-quality-gate` behavior for in-flight plans                                     | Low        | Medium | Default is preserved: `mode` defaults to `strict`, which previously counted CRITICAL+HIGH+MEDIUM+LOW (since "all" was counted). After the fix, strict still counts CRITICAL+HIGH+MEDIUM but stops counting LOW. The plan documents this delta explicitly in the workflow file's "What changed" footer. |
| Baseline counts in the brief (4479 total, etc.) are stale by Phase 0                                              | Medium     | Low    | Phase 0 re-captures the baseline with `RHINO_AUDIT_NOW` pinned and records the actual per-category breakdown into delivery notes; downstream phases respond to the actual numbers, not the brief estimate.                                                                                             |
| The `RHINO_AUDIT_NOW` pin is forgotten in production, hash-reuse never fires                                      | Medium     | Low    | Phase 2.3 documents the recommendation explicitly in the workflow + Phase 1.5 documents it in `apps/rhino-cli/README.md` Command section and Version History v0.16.1 entry; operators see it in both places.                                                                                           |
| Phase 4 sweep accidentally deletes legitimate footer markers in places where they belong (e.g., third-party docs) | Low        | Low    | The sweep is limited to `repo-governance/` — no third-party content lives there. Each deletion is reviewed in the git diff before the checkpoint commit.                                                                                                                                               |
| The `--exclude` flag added in Phase 1.4 conflicts with the existing `--skip` flag semantics                       | Low        | Low    | `--skip` skips an entire audit category; `--exclude` excludes a path glob from category scanning. The two are orthogonal and documented as such in `governance_audit.go` and the rhino-cli README v0.16.1 entry.                                                                                       |

## Alternatives Considered

- **Defer agent dedup (Phase 5) to a follow-up plan.** Rejected — the brief explicitly forbids deferral, and the user's conservative-parameterization mandate gives the design constraint needed to execute safely now.
- **Aggressively normalize agent boilerplate (the opposite of conservative).** Rejected — past normalizations have erased intentional per-agent variation; user has explicitly chosen conservative parameterization.
- **Add `--exclude` to every individual audit category command instead of just the orchestrator.** Rejected — the orchestrator already propagates options through `AuditOptions`; only the orchestrator needs the new flag.
- **Replace `mode` in `plan-quality-gate.md` with a fixed all-levels behavior.** Rejected — sibling workflow consistency wins; both quality gates honor `mode` the same way after this plan.
- **Add per-category convergence dashboards.** Out of scope — observability is documentation-only this plan.
