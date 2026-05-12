# PRD — Complete Repo-Rules Zero Findings

## Product Overview

This plan ships three discrete product changes that compose into a single executable workflow improvement:

1. **A calibrated rhino-cli release (v0.16.1)** that produces deterministic-governance signal rather than legacy-tree noise — emoji-audit honors generated/legacy skip-dirs, docs-validate-frontmatter accepts Diátaxis category values, docs-validate-heading-hierarchy parses N-fence code blocks correctly, and the orchestrator accepts a `--exclude` path-glob flag.
2. **Two hardened quality-gate workflow docs** — `repo-rules-quality-gate.md` (11 production-observed fixes) and `plan-quality-gate.md` (mode-bug fix plus observability) that operators can invoke confidently.
3. **A conservative agent-skill extraction pass** that collapses verbatim/near-verbatim duplication across maker/checker/fixer agents into parameterized shared skills, preserving every existing per-agent phrasing variation. Output: zero `agents-detect-duplication` clusters with zero behavioral drift.

The combined result: `repo-rules-quality-gate strict` reaches double-zero on both deterministic preflight AND AI-only findings.

## Personas

Solo-maintainer repo — same person wears every hat. Listing them as "agents that touch this product" rather than as headcount:

- **Maintainer (planning hat)** — reads this PRD to confirm intent before authorizing execution.
- **Maintainer (execution hat)** — runs the delivery checklist.
- **`plan-checker`** — validates this plan's compliance with the Plans Organization Convention and the Plan Anti-Hallucination Convention. Emits dual-label findings (criticality + confidence).
- **`plan-fixer`** — applies fixes from `plan-checker`'s audit report.
- **`plan-executor` / `plan-execution`** — sequentially executes delivery.md, respecting suggested-executor annotations.
- **`repo-rules-checker`** — Phase 6 consumer; reads the Phase 1 calibrated envelope plus runs its own AI-only categories.
- **`repo-rules-fixer`** — Phase 4/6 fixer for residuals.
- **`swe-golang-dev`** — Phase 1 suggested executor for rhino-cli Go edits.
- **`agent-maker`** — Phase 5 suggested executor for agent + skill edits.
- **`web-research-maker`** — only invoked if an external claim emerges during execution that is not already documented in the repo.

## User Stories

### US-1 — Trustworthy strict gate

As the **maintainer (planning hat)**, I want `repo-rules-quality-gate strict` to converge to a trustworthy double-zero, so that a clean gate is a meaningful release signal rather than "clean after subtracting noise."

### US-2 — Working preflight command

As the **`repo-rules-checker` agent**, I want the Step 0.5 + Step 4 preflight command to produce a valid JSON envelope I can parse, so that I don't fail at preflight ingestion before the AI evaluation even starts.

### US-3 — Cache-hit hash reuse

As the **maintainer (execution hat)** running successive preflight iterations, I want the deterministic envelope's SHA-256 to be stable when the repo contents are stable, so that the documented "reuse deterministic findings unchanged" optimization actually fires.

### US-4 — Codified visibility-only semantics

As the **`repo-rules-checker` agent**, I want the workflow to explicitly state that deterministic findings are visibility-only (managed via the skip-list) and do not count against the mode threshold, so that thousands of HIGH preflight findings don't prevent any strict-mode run from ever converging.

### US-5 — Skip-list curation rules

As the **maintainer (execution hat)**, I want explicit rules for what goes into `generated-reports/.known-false-positives.md` versus what gets fixed, so that the skip-list doesn't become a graveyard of avoidance.

### US-6 — Consistent quality-gate mode semantics

As the **maintainer (execution hat)**, I want `plan-quality-gate` and `repo-rules-quality-gate` to honor the `mode` parameter the same way, so that I don't have to remember two divergent sets of semantics.

### US-7 — Observability footer

As the **maintainer (execution hat)**, I want both workflows to declare which observable signals to track (preflight latency, AI-vs-deterministic finding ratio, iterations-to-convergence, AI-tokens-spent), so that future calibrations can be data-driven.

### US-8 — Calibrated emoji audit

As the **`repo-rules-checker` agent**, I want `emoji-audit` to skip legacy and generated directories (`archived/`, `test-results/`, `playwright-report/`, `coverage/`, `.venv/`, `.dart_tool/`, `out/`, `.cache/`, `__pycache__/`, `.pytest_cache/`, plus `generated*` variants) by default (`.next/` is already skipped [Repo-grounded]), so that emoji findings reflect current source code rather than vendored or legacy content.

### US-9 — Diátaxis-aware frontmatter audit

As a **content maintainer working in `docs/explanation/software-engineering/`**, I want frontmatter validation to accept the actual Diátaxis `category: explanation` value, so that my files don't fail validation when they're conformant to the documented Diátaxis convention.

### US-10 — N-fence-aware heading audit

As a **content maintainer writing nested code samples** in documentation, I want `docs-validate-heading-hierarchy` to handle 4-backtick and 5-backtick fences (Markdown-spec-compliant), so that headings inside nested code samples are not misreported as document headings.

### US-11 — Path-glob exclusion

As the **maintainer (execution hat)**, I want a `--exclude <glob>` flag on the `repo-governance audit` orchestrator, so that I can transiently exclude a path during exploration without editing the source skip-dirs lists.

### US-12 — Conservative skill extraction

As the **`agent-maker` agent**, I want shared skills to accept per-agent variation as parameters rather than normalize to a canonical phrasing, so that every existing agent's intentional voice survives the dedup pass.

### US-13 — Behavioral-equivalence guarantees

As the **maintainer (execution hat)**, I want golden tests that compare pre/post agent rendered bodies after each extraction batch, so that I get an early failure signal if skill extraction silently changes any agent's behavior.

### US-14 — Plan archival to done

As the **maintainer (execution hat)**, I want a final archival checklist that moves the plan from `plans/in-progress/` to `plans/done/YYYY-MM-DD__complete-repo-rules-zero-findings/`, updates the in-progress and done READMEs, and commits the move, so that the plans index stays accurate.

### US-15 — Worktree declaration with runtime waiver

As the **`plan-checker`** validating this plan, I want the worktree section declared per convention with an explicit N/A note that the user has waived runtime provisioning, so that the Step-0 hard gate is satisfied without forcing an unnecessary worktree.

## Functional Requirements

### FR-1 — emoji-audit skip-dirs expansion

`apps/rhino-cli/internal/repo-governance/emoji_audit.go` `emojiSkipDirs` map adds (at minimum): `archived`, `test-results`, `playwright-report`, `coverage`, `.venv`, `.dart_tool`, `out`, `.cache`, `__pycache__`, `.pytest_cache` (10 new entries; `.next` is already present at line 59 — not added again [Repo-grounded]). Additional `generated*` variants (e.g., `generated-output`, `generated-build`) added if present anywhere in the repo via `Glob` discovery during execution.

### FR-2 — Diátaxis frontmatter schema

`apps/rhino-cli/internal/docs/frontmatter.go` `validateSoftwareSchema` accepts `category` values: `tutorial`, `how-to`, `reference`, `explanation`. Files under `docs/explanation/software-engineering/` continue to require `title`, `description`, `category`, `subcategory`, and `tags`. The pre-existing `category=software` value emits a warn-severity deprecation finding (not fail) pointing operators at the corrected schema.

### FR-3 — N-fence support in heading hierarchy

`apps/rhino-cli/internal/docs/heading_hierarchy.go` `isFenceLine` + `fenceMarkerOf` track fence length, not just kind. An opening fence of length N (N >= 3) is closed only by a same-language fence of length >= N. Lines inside a 4-backtick fence containing 3-backtick code samples are skipped wholesale.

### FR-4 — `--exclude` flag on orchestrator

`apps/rhino-cli/cmd/governance_audit.go` `governanceAuditCmd` accepts repeatable `--exclude <glob>` flags. The orchestrator forwards them to each category that supports path exclusion via `AuditOptions.ExcludeGlobs []string`.

### FR-5 — `RHINO_AUDIT_NOW` documentation

`apps/rhino-cli/README.md` documents the `RHINO_AUDIT_NOW` env var in the Command section for `repo-governance audit` AND in the v0.16.1 Version History entry.

### FR-6 — Version bump to v0.16.1

`apps/rhino-cli/cmd/root.go` `Version` constant bumps from `0.16.0` to `0.16.1`. The version-history block in `apps/rhino-cli/README.md` gains a v0.16.1 entry summarizing FR-1 through FR-5.

### FR-7 — Workflow command fix (direct binary)

`repo-governance/workflows/repo/repo-rules-quality-gate.md` Step 0.5 + Step 4 use `./apps/rhino-cli/dist/rhino-cli repo-governance audit -o json > <path>` (direct binary form) instead of the broken nx-wrapped form. Both steps note the binary must be built first via `nx build rhino-cli`.

### FR-8 — Deterministic-findings-as-visibility-only codification

`repo-rules-quality-gate.md` Step 2 + Step 5 explicit rule: deterministic findings are reported but do NOT count toward the mode threshold. They are managed via `generated-reports/.known-false-positives.md` skip-list curation outside the iteration loop.

### FR-9 — Hash-reuse pinning recommendation

`repo-rules-quality-gate.md` Step 0.5 documents `RHINO_AUDIT_NOW=<RFC3339>` as the recommended pin for production runs, with cross-link to the rhino-cli README.

### FR-10 — Arg-name consistency

`repo-rules-quality-gate.md` Step 1 + Step 4 use `{step0_5.outputs.preflight-report}` (Step 1) and `{step4.preflight.outputs.preflight-report}` (Step 4) consistently — Step 4's `{step4_preflight.outputs.preflight-report}` form is corrected to the dot-namespaced sub-step convention.

### FR-11 — Exit-2 recovery section

`repo-rules-quality-gate.md` Step 0.5 contains a 2-line debugging hint: run `dist/rhino-cli repo-governance audit -o text` for human-readable diagnostic; common causes (missing binary — rebuild; broken category function — run individual `rhino-cli repo-governance <category>` to isolate).

### FR-12 — Skip-list curation rules section

`repo-rules-quality-gate.md` has a new H2 "Skip-list Curation Rules" defining: who maintains `.known-false-positives.md`, when to add entries vs fix findings, what each entry must contain (key + rationale + date + approver), per-category triage priority.

### FR-13 — Observability metrics section

Both `repo-rules-quality-gate.md` AND `plan-quality-gate.md` have an H2 "Observability Metrics" replacing or augmenting the generic Success Metrics section. Tracked metrics (at minimum):

- Preflight cold-run latency target (gut-based estimate)
- Preflight cached-run latency target (gut-based estimate)
- AI tokens spent on Step 1+ vs deterministic findings count
- AI-only finding-to-deterministic-finding ratio
- Iterations-to-convergence per mode

### FR-14 — "What changed" footer

`repo-rules-quality-gate.md` notes Step 0.5 added 2026-05-12 referencing the prior plan; notes this hardening commit referencing this plan.

### FR-15 — Step 0.5 numbering rationale

`repo-rules-quality-gate.md` Step 0.5 has a one-paragraph note explaining why the step is numbered 0.5 (preserves Step 1-6 numbering from the pre-preflight era) rather than renumbering Steps 1-6 to 1-7. Cites the [Workflow Identifier Convention](../../../repo-governance/workflows/meta/workflow-identifier.md)'s flexibility for sub-step decimals.

### FR-16 — Emoji-audit operator hatch

`repo-rules-quality-gate.md` Step 0.5 has a callout noting operators may pass `--skip emoji-audit` if they need to bypass legacy/archived tree scans (backup hatch — primary fix is the expanded skip-dirs list from FR-1).

### FR-17 — Conventions Implemented entry confirmed

`repo-rules-quality-gate.md` `Conventions Implemented/Respected` section contains the `Deterministic vs AI Validation Split Convention` reference. (Already present per commit `efe87aba2`; this plan verifies and leaves untouched if so.)

### FR-18 — `plan-quality-gate` mode honored

`plan-quality-gate.md` Step 2 + Step 5 count findings by criticality threshold per mode (matching `repo-rules-quality-gate.md` semantics):

- `lax` — CRITICAL only
- `normal` — CRITICAL + HIGH
- `strict` — CRITICAL + HIGH + MEDIUM
- `ocd` — all levels

### FR-19 — `plan-quality-gate` Conventions Implemented entries

`plan-quality-gate.md` `Conventions Implemented/Respected` adds entries for [Plans Organization Convention](../../../repo-governance/conventions/structure/plans.md) and [Plan Anti-Hallucination Convention](../../../repo-governance/development/quality/plan-anti-hallucination.md).

### FR-20 — Research-delegation cost note

`plan-quality-gate.md` Research Delegation section documents the qualitative context-budget benefit of delegating multi-page research to `web-research-maker` and cross-references the Observability Metrics section.

### FR-21 — Final Audit Report Structure section

`plan-quality-gate.md` has a new H2 documenting the plan-audit-report structure the operator can expect (mirrors the existing structure in `repo-rules-checker.md`).

### FR-22 — Phase 4 governance-finding fixes

Footer markers (`**Last Updated**` blocks) removed from `repo-governance/`. Workflow agent refs added or exempted as appropriate. README index gaps reconciled. Frontmatter and heading-hierarchy residuals after Phase 1 calibration fixed. Skip-list `.known-false-positives.md` curated per FR-12 rules.

### FR-23 — Conservative skill extraction

`agents-detect-duplication` clusters with ≥3 agents involved are extracted into parameterized shared skills under `.claude/skills/<skill-name>/SKILL.md`. Two-agent dup is NOT extracted unless bodies are genuinely identical. Skills accept variable portions as front-matter args or in-body placeholders.

### FR-24 — Behavioral-equivalence golden tests

For 3 canonical agents (1 maker, 1 checker, 1 fixer), pre-extraction full rendered body is captured and compared against post-extraction rendered body. Halt-on-drift; rollback the batch if drift is detected.

### FR-25 — Per-batch validation

After each extraction batch (≤5 agents per batch):

1. Sync to opencode via `npm run sync:claude-to-opencode`
2. `agents detect-duplication` cluster count drops monotonically (verified by re-running the command)
3. `agents validate-claude --agents-only` remains PASS
4. `agents validate-sync` remains PASS
5. `nx run rhino-cli:validate:cross-vendor-parity` remains PASS
6. `nx run rhino-cli:test:quick` coverage threshold preserved

### FR-26 — Plan archival

Plan moves from `plans/in-progress/complete-repo-rules-zero-findings/` to `plans/done/YYYY-MM-DD__complete-repo-rules-zero-findings/` via `git mv`, where `YYYY-MM-DD` is the completion date. `plans/in-progress/README.md` removes the plan; `plans/done/README.md` adds it. Final commit: `chore(plans): archive complete-repo-rules-zero-findings`.

## Non-Functional Requirements

### NFR-1 — Idempotency

Phases are designed to be safely re-runnable. If Phase 1 lands but Phase 2 fails partway, re-running Phase 2 from the start is safe (workflow doc edits are idempotent text changes; no side effects).

### NFR-2 — Per-phase checkpoint commits

Every phase ends with a checkpoint commit + push to `origin/main` per Trunk Based Development [Repo-grounded — `repo-governance/development/workflow/trunk-based-development.md`]. Allows revert at phase granularity.

### NFR-3 — Per-batch checkpoint commits in Phase 5

Phase 5 commits per batch (≤5 agents per batch). A regression in batch N+1 is revertable without losing batches 1..N.

### NFR-4 — CI green after every push

`origin/main` CI must be green after each push. If any GitHub Actions check fails, fix immediately and push a follow-up commit before the next phase begins. [Repo-grounded — `repo-governance/development/workflow/ci-post-push-verification.md`]

### NFR-5 — Tests pass at every checkpoint

`nx affected -t typecheck`, `nx affected -t lint`, `nx affected -t test:quick`, and `nx affected -t spec-coverage` exit zero before every push. [Repo-grounded — `apps/rhino-cli/project.json` targets exist.]

### NFR-6 — Cross-vendor parity preserved

`nx run rhino-cli:validate:cross-vendor-parity` exits zero after every Phase 5 batch. The `.opencode/agents/` mirror is auto-generated from `.claude/agents/` via `npm run sync:claude-to-opencode`.

### NFR-7 — Anti-hallucination compliance

Every non-trivial factual claim in the five plan documents carries an inline confidence label (`[Repo-grounded]`, `[Web-cited]`, `[Judgment call]`, or `[Unverified]`) per the Plan Anti-Hallucination Convention. Zero AP-1 through AP-10 violations as defined in the convention.

### NFR-8 — Vendor-neutrality in governance prose

Any new governance prose added to the workflow docs (e.g., Skip-list Curation Rules section) uses vendor-neutral terminology — the workflow files live under `repo-governance/`, in scope for the Governance Vendor-Independence Convention.

### NFR-9 — Conservative-parameterization invariant

Phase 5 skill extraction NEVER normalizes per-agent phrasing. Skills accept variation as args, never pick a canonical wording. Two-agent duplications are preserved unless genuinely byte-identical.

### NFR-10 — Final-state determinism

The final `repo-governance audit -o json` envelope at the end of Phase 6 (after baseline re-capture) reports `total_findings = 0` AND has the same SHA-256 across two consecutive runs (proving determinism). The byte-determinism gate is already part of the audit envelope contract [Repo-grounded — `apps/rhino-cli/README.md` v0.16.0 entry: "Verified byte-deterministic via 10-run SHA-256 gate"].

## Acceptance Criteria (Gherkin)

```gherkin
Feature: Calibrated rhino-cli governance audits

Scenario: emoji-audit skips legacy and generated dirs by default
  Given the rhino-cli source has expanded emojiSkipDirs per FR-1
  When the developer runs `nx run rhino-cli:test:quick`
  Then unit tests assert each new skip-dir is skipped during a walk
  And integration tests assert no findings emerge from a synthetic archived/ fixture

Scenario: docs-validate-frontmatter accepts Diátaxis explanation
  Given a file under docs/explanation/software-engineering/ with frontmatter category=explanation
  When the developer runs `dist/rhino-cli docs validate-frontmatter`
  Then the file passes with zero fail-severity findings

Scenario: docs-validate-frontmatter warns on deprecated category=software
  Given a file under docs/explanation/software-engineering/ with frontmatter category=software
  When the developer runs `dist/rhino-cli docs validate-frontmatter`
  Then a warn-severity deprecation finding is emitted
  And the command exit code does not flip to 1 because of the warning alone

Scenario: docs-validate-heading-hierarchy handles 4-fence nesting
  Given a markdown file with a 4-backtick code block containing a 3-backtick sample
  And the sample contains a `## ` line that is not a real heading
  When the developer runs `dist/rhino-cli docs validate-heading-hierarchy`
  Then no heading-hierarchy finding is emitted for the inner sample line

Scenario: orchestrator accepts --exclude
  Given the orchestrator is invoked as `rhino-cli repo-governance audit --exclude 'archived/**'`
  When the audit runs
  Then no finding references a path matching `archived/**`

Scenario: RHINO_AUDIT_NOW enables hash reuse
  Given two consecutive preflight runs with `RHINO_AUDIT_NOW=2026-05-12T12:00:00Z` pinned
  And no repo file has changed between runs
  When the developer compares the SHA-256 of both JSON envelopes
  Then both envelopes have identical SHA-256

Scenario: rhino-cli version bumped to 0.16.1
  Given the rhino-cli binary is rebuilt after the edits
  When the developer runs `dist/rhino-cli --version`
  Then the output contains "0.16.1"

Feature: Hardened repo-rules-quality-gate workflow

Scenario: workflow command produces valid JSON envelope
  Given the rhino-cli binary at `apps/rhino-cli/dist/rhino-cli` exists
  When the operator runs the Step 0.5 command exactly as written in the workflow
  Then the output file at the captured path parses as `rhino-cli/repo-governance-audit/v1` JSON

Scenario: deterministic findings are visibility-only
  Given a preflight envelope with 700 deterministic HIGH findings
  And mode=strict
  When the workflow Step 2 evaluates threshold-level findings
  Then deterministic findings are not counted against the threshold
  And the workflow is permitted to proceed to a zero AI-only-finding confirmation

Scenario: Exit 2 recovery hint visible
  Given the operator reads `repo-rules-quality-gate.md` Step 0.5
  When they encounter the exit-2 description
  Then a debugging hint pointing at `dist/rhino-cli repo-governance audit -o text` is present

Scenario: Skip-list curation rules section present
  Given an operator opens `repo-rules-quality-gate.md`
  When they search for "Skip-list Curation Rules"
  Then an H2 section by that name exists
  And the section names per-entry requirements (key + rationale + date + approver)

Scenario: Observability metrics section present
  Given an operator opens `repo-rules-quality-gate.md`
  When they search for "Observability Metrics"
  Then an H2 section by that name exists
  And the section names at least: preflight cold-run latency, preflight cached-run latency, AI tokens spent, AI-vs-deterministic finding ratio, iterations-to-convergence

Feature: Plan-quality-gate mode bug fix

Scenario: plan-quality-gate honors strict mode threshold
  Given a plan audit with 0 CRITICAL, 0 HIGH, 0 MEDIUM, and 3 LOW findings
  And mode=strict
  When the workflow Step 2 counts threshold-level findings
  Then the count returns 0 (LOW is below strict threshold)
  And the workflow proceeds to consecutive-zero confirmation

Scenario: plan-quality-gate honors ocd mode threshold
  Given a plan audit with 0 CRITICAL, 0 HIGH, 0 MEDIUM, and 3 LOW findings
  And mode=ocd
  When the workflow Step 2 counts threshold-level findings
  Then the count returns 3 (LOW is at-threshold for ocd)
  And the workflow proceeds to Step 3 fixing

Scenario: plan-quality-gate observability section present
  Given an operator opens `plan-quality-gate.md`
  When they search for "Observability Metrics"
  Then an H2 section by that name exists

Scenario: plan-quality-gate Conventions Implemented entries present
  Given an operator opens `plan-quality-gate.md`
  When they read the `Conventions Implemented/Respected` section
  Then it cites the Plans Organization Convention and the Plan Anti-Hallucination Convention

Feature: Phase 4 governance findings cleared

Scenario: no Last Updated footer markers remain in repo-governance
  Given Phase 4 sweep has run
  When the developer runs `grep -rn "\*\*Last Updated\*\*" repo-governance/`
  Then zero matches are returned

Scenario: README index reconciliation
  Given the affected dirs (`repo-governance/workflows/`, `.claude/agents/`, `.claude/skills/`)
  When `dist/rhino-cli repo-governance readme-index-audit` runs
  Then the category returns zero findings for those dirs

Feature: Conservative agent-skill extraction

Scenario: agents-detect-duplication exits clean
  Given Phase 5 extraction has completed
  When the developer runs `dist/rhino-cli agents detect-duplication`
  Then the exit code is 0 and the category reports zero clusters

Scenario: per-agent phrasing preserved
  Given a maker agent had unique phrasing in its Validation Process structure pre-extraction
  When the agent is rendered with the extracted skill inlined
  Then the rendered body retains the agent's specific phrasing (proven by the golden-test diff being empty modulo extracted skill content)

Scenario: per-batch validation gates intact
  Given a Phase 5 batch has been applied
  When the developer runs the post-batch validation suite
  Then `agents validate-claude --agents-only` exits 0
  And `agents validate-sync` exits 0
  And `nx run rhino-cli:validate:cross-vendor-parity` exits 0
  And `nx run rhino-cli:test:quick` exits 0

Feature: Final double-zero convergence

Scenario: strict mode reaches pass status
  Given Phases 1-5 have completed
  When the operator invokes `repo-rules-quality-gate` in strict mode
  Then the workflow terminates with `final-status=pass`
  And `iterations-completed` is recorded
  And the final audit report shows total_findings=0 across deterministic preflight AND AI-only categories

Scenario: byte-deterministic envelope at convergence
  Given the final state from the previous scenario
  When the operator runs `dist/rhino-cli repo-governance audit -o json` twice with `RHINO_AUDIT_NOW` pinned
  Then both envelope SHA-256 hashes are identical

Feature: Plan archival

Scenario: plan archived to done with completion date
  Given all delivery checklist items are ticked
  And all quality gates pass (local + CI)
  When the maintainer runs `git mv plans/in-progress/complete-repo-rules-zero-findings plans/done/<today>__complete-repo-rules-zero-findings`
  Then `plans/in-progress/README.md` no longer lists this plan
  And `plans/done/README.md` lists it with the completion date
  And the final commit subject is `chore(plans): archive complete-repo-rules-zero-findings`
```

## Product Scope

### In Scope

- All FR-1 through FR-26 deliverables above
- All NFR-1 through NFR-10 quality constraints above
- All 22 Gherkin scenarios above
- Updates to `apps/rhino-cli/README.md` Version History (v0.16.1) and the Command section for `repo-governance audit`
- Curation of `generated-reports/.known-false-positives.md` per FR-12 rules
- Auto-sync of `.opencode/agents/` after every Phase 5 batch
- Final plan archival per FR-26

### Out of Scope

- Touching `ose-infra/`, `ose-primer/`, or parent `ose-projects` content
- Building a real-time metrics dashboard (observability sections document metric definitions only)
- Migrating tutorials, how-to, reference, or non-software-engineering explanation under `repo-rules-quality-gate` scope
- Refactoring beyond what Phase 5 conservative extraction delivers (no aggressive normalization)
- Replacing the `mode` enum with a different parameter shape

## Product Risks

| Risk                                                                                               | Mitigation                                                                                                                                                                                                                                           |
| -------------------------------------------------------------------------------------------------- | ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| Phase 1 frontmatter schema change rejects governance docs that previously passed                   | Schema is additive; old `category=software` value emits warn (not fail), preserving current pass/fail status during transition. Unit + integration tests cover both shapes.                                                                          |
| Phase 1 emoji skip-dirs hide a legitimate emoji violation in newly added code                      | Each new skip-dir is documented in the v0.16.1 release notes with rationale; `--exclude` provides a fine-grained alternative for one-off cases; the audit can still find violations in non-skipped directories.                                      |
| Phase 5 batches accidentally break the rendered behavior of an extracted agent                     | Behavioral-equivalence golden tests for 3 canonical agents detect drift; per-batch revert keeps the rest of the work safe.                                                                                                                           |
| Phase 3 mode-bug fix surprises operators expecting all-levels behavior on plan-quality-gate strict | The "What changed" footer in the workflow file documents the delta explicitly. The default mode remains `strict`, so a developer who passes no mode argument continues to get the strict semantics (now CRITICAL+HIGH+MEDIUM instead of all levels). |
| Phase 4 README index reconciliation deletes a needed cross-link                                    | Reconciliation is mechanical (link references actual sibling .md files); each deletion is visible in the git diff before commit. Re-running `dist/rhino-cli repo-governance readme-index-audit` confirms zero findings post-sweep.                   |
| The Step 0.5 direct-binary command requires a pre-built binary that may not exist on a fresh clone | Phase 2.1 explicitly notes the prerequisite (`nx build rhino-cli` first); the workflow text adds the same note. Operators who hit the missing-binary failure see the Phase 2.5 exit-2 recovery hint pointing at the same fix.                        |
| Phase 2.4 arg-name rename breaks an existing checker prompt                                        | The rename targets only Step 4's `step4_preflight` → `step4.preflight` (Step 1 already uses the dominant form). The checker prompt does not reference Step 4's arg name; the workflow doc is the only consumer of the arg.                           |
