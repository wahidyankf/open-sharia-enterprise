# BRD — BDD + DDD Tooling Gap-Fill

## Why this work matters

`rhino-cli` ships strong BDD + DDD validators. The audit on 2026-05-09 confirmed the inner logic is real and well-tested. But the **wiring** between those validators and the pre-push gate is uneven:

- 4 of 7 `specs *` validators are dead code (no Nx target invokes them). The other 3 are placeholder `drift-*` stubs.
- Of the apps that have specs (`organiclever`, `wahidyankf`, `oseplatform`, `ayokoding`, plus the three CLIs), only `organiclever-web` has `ddd bc/ul` in `test:quick`. Plans 1-3 fix three of the remaining web apps; this plan fixes `organiclever-be` (which shares organiclever's registry but doesn't validate it).
- Two `ddd` validators silently mis-cover multi-surface bounded contexts: `ddd ul` greps only TS/TSX, `ddd bc` walks only the first context's parent for orphans.
- Even after wiring `validate:specs-*` into `.husky/pre-push`, four CI surfaces (`pr-quality-gate.yml` and three `test-and-deploy-*.yml` workflows that drive the main-branch deploy pipeline) still do not call those validators. Pre-push is bypassable (`--no-verify`); without parallel coverage on PR gate + main CI, structural spec drift can land on `main`.
- `spec-coverage validate` only validates **forward** direction: every Gherkin step has at least one matching step impl. The **reverse** direction is unenforced — a scenario can be renamed or deleted while its step impl(s) sit orphaned in test files; nothing in the toolchain reports them. Over time orphan step impls accumulate, get copy-pasted, or contradict refactored scenarios. Across all 15 spec-coverage-wired projects, this blind spot scales linearly with the project count.

The gap is not in the validators. It is in the layer above them.

## Business value

1. **Enforced governance, not aspirational governance.** Today's "BDD adoption is mandatory" is a sentence in `governance/`; tomorrow's is a `nx affected -t validate:specs-adoption validate:specs-tree validate:specs-counts validate:specs-links` exit code on **every** gating surface — pre-push (catches at push time), PR quality gate (catches at merge time), and the four main-CI deploy workflows (catches at deploy time). The first kind drifts; the second kind doesn't. **Net result**: zero dead specs/BDD/DDD scripts in `rhino-cli` — every `specs *`, `ddd *`, and `spec-coverage *` command is gated on every relevant surface or deleted.
2. **Multi-surface DDD becomes honest.** Plans 2 and 3 introduce bounded contexts that span web + api perspectives. Without fix #4 (per-BC `code_lang:`), `ddd ul` would silently fail to validate F# / TS / both for any cross-perspective BC. With fix #4, every code identifier in every glossary is grep-checked in the right files.
3. **Drift-command housekeeping.** Three placeholder commands that appear functional in `--help` are a long-standing source of false security. Deleting or hiding them removes a lurking trap.
4. **Future-app onboarding cost drops.** A new web app adopts DDD by adding to the allowlist + creating its `ddd/bounded-contexts.yaml`. No more per-project.json copy-paste of `ddd bc/ul` invocations once fix #1 + #2 wire the centralized gate.

## Why now

- Plans 1-3 are the prerequisite, so this plan slots in immediately after their merge.
- Each gap costs nothing to fix individually but costs significantly more if left to compound: every new app added without these gates inherits the unenforced shape and amplifies the wiring debt.
- Several fixes (#5, #6, #7) are <50-line code changes with high signal-to-noise ratio.

## Cost

- ~10 phased TDD code changes inside `apps/rhino-cli/`, each with a Red→Green→Refactor cycle.
- 4 `project.json` files updated (`organiclever-be`, plus tightening for the 3 web apps that plans 1-3 already wire).
- 1 `.husky/pre-push` change adding the four new validate-specs targets to the existing `nx affected` line.
- 3 `.github/workflows/*.yml` files updated for Fix #14: a new `specs-gate` job in `pr-quality-gate.yml` (added to the `quality-gate` aggregator's `needs`), a new `specs-gate` job in `_reusable-test-and-deploy.yml` (added to the `deploy` job's `needs`, blocking deploys on validate-specs failures), and a new `specs-gate` job in `test-and-deploy-organiclever-web-development.yml` (added to the `deploy` job's `needs`).
- Fix #15: ~80 LOC across `internal/speccoverage/checker.go` + `types.go` + `cmd/spec_coverage_validate.go` (extend `stepMatcher` with origin tracking, add `OrphanStepImpl` finding type, add reverse-direction pass run after the forward pass on the same matcher pool, exit non-zero on any orphan). Plus a pre-flight orphan audit phase that runs the new check against all 15 spec-coverage-wired projects in the worktree and either deletes orphan step impls or moves them under live scenarios in the same plan; conservative estimate 10-30 orphan cleanup LOC across affected `*-e2e/`, `*-cli/`, and `*-web/` step files (exact count surfaces only at audit time).
- 1 `.claude/agents/specs-checker.md` and `specs-fixer.md` update to reflect the dropped `drift-*` commands.
- ~20 lines of governance update (`governance/conventions/structure/specs-directory-structure.md`) clarifying allowlist policy and listing all gating surfaces (pre-push + PR gate + main-CI workflows).
- New env var name `OSE_RHINO_DDD_SEVERITY`; legacy `ORGANICLEVER_RHINO_DDD_SEVERITY` deprecated for one minor rev with a stderr warning.

## Risk

**Low to medium**, depending on fix:

- **Low**: fixes #1, #2, #3, #7, #8, #9, #10, #14 are mechanical — wiring, severity strings, whitelist expansion. Each lands behind a TDD red→green and a manual test. Fix #14 yml edits add three new `specs-gate` jobs (PR + reusable-deploy + organiclever-development) that run `nx run-many -t validate:specs-* --projects=rhino-cli`; the steps are mechanical and do not change any validator code.
- **Medium**: fixes #4 (per-BC `code_lang:`) and #5 (multi-parent orphan walk) edit the validator core (`bcregistry/validator.go`, `glossary/validator.go`). New tests cover positive + negative cases per the existing 90% coverage rule. Risk: test fixture drift across the existing 30+ rhino-cli test files. Mitigated by `(cd apps/rhino-cli && go test ./...)` after each phase.
- **Medium**: fix #6 (multi-file scenario matching) changes spec-coverage's behavior in non-shared-steps mode. Today only `--shared-steps` mode is in production use; non-shared mode change is theoretically observable but practically zero-impact since no project uses it.
- **Medium**: fix #15 (reverse-direction step orphan check) edits `internal/speccoverage/checker.go` core + extends `stepMatcher` with origin tracking. Default-on with no escape hatch by deliberate scope choice — the philosophical alignment with "zero dead specs/BDD/DDD scripts" extends to "zero dead step impls". Risk: the pre-flight orphan audit (Phase 5B.4) may surface unexpected orphans across the 15 spec-coverage-wired projects (Go CLI step packages, TS step files, Playwright step modules), all of which must be cleaned up in the same plan before merge. If the audit surfaces a large orphan count (>30) that suggests a different cleanup pattern, the plan halts at Phase 5B.4 for re-scoping rather than proceeding to merge with shortcuts. Mitigated by running the audit early (in Phase 5B.4, before any pre-push wiring lands) so cleanup happens before downstream gates fire.

## Success Metrics

- `[Judgment call]` `nx run rhino-cli:validate:specs-adoption` exits 0 for all four allowlisted web apps after this plan lands on `main`.
- `[Judgment call]` `nx run rhino-cli:validate:specs-tree` exits 0 for all four allowlisted web apps after this plan lands on `main`.
- `[Judgment call]` `nx run rhino-cli:validate:specs-counts` exits 0 for all four allowlisted web apps after this plan lands on `main`.
- `[Judgment call]` `nx run rhino-cli:validate:specs-links` exits 0 for all four allowlisted web apps after this plan lands on `main`.
- `[Judgment call]` `rhino-cli specs --help` lists exactly 4 subcommands (validate-tree, validate-counts, validate-links, validate-adoption) — no drift-\* placeholders.
- `[Judgment call]` `OSE_RHINO_DDD_SEVERITY=warn rhino-cli ddd bc organiclever` emits a stderr audit line and exits 0 even when findings exist.
- `[Judgment call]` `nx run rhino-cli:test:quick` reports coverage ≥90% after all 15 fixes.
- `[Judgment call]` The pre-push gate blocks a push that introduces a structural spec violation, missing required folder, or broken markdown link in any allowlisted app.
- `[Judgment call]` The `pr-quality-gate.yml` workflow's `specs-gate` job runs `nx run-many -t validate:specs-adoption validate:specs-tree validate:specs-counts validate:specs-links --projects=rhino-cli` on every PR and is in the `quality-gate` aggregator's `needs:` list (Fix #14).
- `[Judgment call]` The reusable test-and-deploy workflow (`_reusable-test-and-deploy.yml`) and the OrganicLever development deploy workflow (`test-and-deploy-organiclever-web-development.yml`) both contain a `specs-gate` job blocking deploys; the three thin caller workflows for ayokoding-web, oseplatform-web, and wahidyankf-web inherit the gate at runtime via the reusable workflow with no per-file edit needed (Fix #14 — three file edits: PR gate + reusable deploy + organiclever-development; four runtime surfaces covered).
- `[Judgment call]` **Zero dead specs/BDD/DDD scripts** — `git grep` over `apps/rhino-cli/cmd/specs_*.go` shows every command is invoked by either an Nx target wired to `.husky/pre-push`, the PR quality gate, or a main-CI deploy workflow; the three `specs_drift_*.go` files are deleted.
- `[Judgment call]` **Zero orphan step implementations** (Fix #15) — `rhino-cli spec-coverage validate` exits non-zero if any extracted step impl across the project's source files lacks at least one matching Gherkin step. Reverse-direction enforcement is default-on with no escape hatch; a scenario rename/delete that orphans its step impl(s) blocks the next push.

## Affected Roles

Single maintainer wearing multiple hats — no separate org roles exist, but the following workflow changes affect each hat distinctly:

- **Developer hat** — pre-push gate fires more often (four new `validate:specs-*` targets appended to the existing `nx affected` line); a structural spec violation or orphan step impl now aborts a push that previously succeeded. Net workflow change: spec regressions are caught before remote CI, not after.
- **CI operator hat** — three workflow files (`pr-quality-gate.yml`, `_reusable-test-and-deploy.yml`, `test-and-deploy-organiclever-web-development.yml`) each gain a new `specs-gate` job visible in the GitHub Actions run log; the `quality-gate` and `deploy` aggregator jobs each have one additional `needs:` entry. No new manual intervention required; behavior is automatic.
- **Downstream ose-primer consumer hat** — allowlist-driven allowlist constant, validator improvements, and agent-binding updates propagate via `repo-ose-primer-propagation-maker` post-merge; no immediate action required from downstream consumers, but the template gains these tooling improvements for teams bootstrapping from it.

## Stakeholders

Single maintainer + downstream consumers:

- All four web apps (post plans 1-3) depend on the new allowlist gates.
- `ose-primer` (downstream MIT template) inherits these tooling improvements via the propagation maker; no immediate action there but worth flagging post-merge.

## Out of scope (deferred)

- Implementing the three drift commands. Fix #7 only deletes/hides them; implementation is a separate plan when the need arises.
- AST-based step extraction (LOW priority from audit). Tracked as backlog research item.
- Validator unification with `lint:md` / `docs validate-links`. Tracked as separate plan.
- DDD-aware `nx affected` graph. Future research.
