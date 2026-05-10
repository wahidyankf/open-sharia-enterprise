# Delivery Checklist — BDD + DDD Tooling Gap-Fill

All steps follow Red → Green → Refactor (TDD). After each phase, run `(cd apps/rhino-cli && go build ./... && go test ./...)` plus `nx run rhino-cli:test:quick`. Do not advance phases out of order.

> **Manual behavioral acceptance gate**: Per-phase steps implement the code change. The manual CLI smoke verification for each fix (expected inputs → expected stdout/stderr/exit-code) is consolidated in **Phase 12.10**. After each phase completes its TDD cycle, proceed; full behavioral assertion runs in Phase 12.

---

## Worktree

Worktree path: `worktrees/bdd-ddd-tooling-gap-fill/`

Provision before execution (run from repo root):

```bash
claude --worktree bdd-ddd-tooling-gap-fill
```

See [Worktree Path Convention](../../../governance/conventions/structure/worktree-path.md) and [Plans Organization Convention §Worktree Specification](../../../governance/conventions/structure/plans.md#worktree-specification).

---

## Environment Setup

- [ ] Provision worktree: `claude --worktree bdd-ddd-tooling-gap-fill` (creates `worktrees/bdd-ddd-tooling-gap-fill/` in repo root; see [Worktree Path Convention](../../../governance/conventions/structure/worktree-path.md)).
- [ ] Initialize toolchain in the root worktree: `npm install && npm run doctor -- --fix` (see [Worktree Toolchain Initialization](../../../governance/development/workflow/worktree-setup.md)).
- [ ] Verify existing tests pass before making changes: `nx run rhino-cli:test:quick`.

---

## Phase 0 — Pre-flight gate

- [ ] **0.1** Confirm plans 1, 2, 3 are merged to `origin/main` and CI is green for all three. The allowlist gates (wired in Phases 1.4 + 7B.4, validated in Phase 12) will fail if any of them is incomplete.
  - **Note (2026-05-10)**: Plan 3 (`oseplatform-web-ddd-and-specs-format`) merged to `origin/main`. `specs/apps/oseplatform/ddd/bounded-contexts.yaml` v2, 7 BCs. `oseplatform-web:test:quick` now runs `ddd bc/ul`. oseplatform is allowlist-eligible.
  - **Note (2026-05-10)**: Plan 2 (`ayokoding-web-ddd-and-specs-format`) merged to `origin/main` (SHA 232b07c2e). `specs/apps/ayokoding/ddd/bounded-contexts.yaml` v2, 6 BCs. `ayokoding-web:test:quick` now runs `ddd bc/ul`. ayokoding is allowlist-eligible. CI run 25615323712 green.
  - **Note (2026-05-10)**: Plan 1 (`wahidyankf-web-ddd-and-specs-format`) merged to `origin/main` (SHA 77ad0771e). `specs/apps/wahidyankf/ddd/bounded-contexts.yaml` v2, 5 BCs. `wahidyankf-web:test:quick` now runs `ddd bc/ul`. wahidyankf is allowlist-eligible. CI run 25616084542 green. All three plans now done — Phase 0 fully unblocked.
- [ ] **0.2** Inspect `specs/apps/{wahidyankf,oseplatform,ayokoding}/ddd/bounded-contexts.yaml` — confirm each has `version: 2`, ≥1 context, valid layers. If anything is broken, the dependency is not actually satisfied; halt and re-validate plans 1-3 first.
- [ ] **0.3** Inspect `apps/{wahidyankf-web,oseplatform-web,ayokoding-web,organiclever-web}/project.json` — confirm each runs `ddd bc/ul` in `test:quick`. (organiclever-be is NOT yet wired; that's Phase 3.)
- [ ] **0.4** Create worktree `worktrees/bdd-ddd-tooling-gap-fill/`.

---

## Phase 1 — Allowlist constant + Nx target wiring (Fixes #1, #2)

### 1.1 Allowlist package

- [ ] **1.1.1 RED** Create `apps/rhino-cli/internal/allowlist/allowlist_test.go` asserting `AppsWithDDD` contains exactly the four expected apps (no CLIs, no extras). Test fails: package doesn't exist.
- [ ] **1.1.2 GREEN** Create `apps/rhino-cli/internal/allowlist/allowlist.go` per `tech-docs.md` § "Allowlist constant". Test passes.

### 1.2 `--apps` flag on `validate-adoption` and `validate-tree`

- [ ] **1.2.1 RED** In `cmd/specs_validate_adoption_test.go` add scenarios: "no positional, no flag → defaults to allowlist"; "explicit positional preserved"; "--apps flag overrides defaults". Tests fail.
- [ ] **1.2.2 GREEN** Add `--apps` StringSlice flag; if positional empty AND flag empty, use `allowlist.AppsWithDDD`; if positional set, single-app behavior preserved. Same change in `cmd/specs_validate_tree.go`.
- [ ] **1.2.3 GREEN** Run unit + integration tests. Coverage ≥90%.

### 1.3 Nx targets

- [ ] **1.3.1** Edit `apps/rhino-cli/project.json`:
  - Add `validate:specs-adoption` target per `tech-docs.md` Fix #1+#2.
  - Add `validate:specs-tree` target.
- [ ] **1.3.2 GREEN** Run `nx run rhino-cli:validate:specs-adoption` — exits 0 (assumes plans 1-3 merged).
- [ ] **1.3.3 GREEN** Run `nx run rhino-cli:validate:specs-tree` — exits 0.

### 1.4 Pre-push wiring

- [ ] **1.4.1** Edit `.husky/pre-push` to add `validate:specs-adoption validate:specs-tree` to the existing `nx affected -t typecheck lint test:quick spec-coverage` invocation, so the pre-push line becomes `nx affected -t typecheck lint test:quick spec-coverage validate:specs-adoption validate:specs-tree --parallel="$PARALLEL"`. No conditional regex — the allowlist lives only in `apps/rhino-cli/internal/allowlist/allowlist.go`.
- [ ] **1.4.2 RED** Stage a no-op edit to `specs/apps/wahidyankf/ddd/bounded-contexts.yaml` (whitespace) and run `git push --dry-run`. Confirm both new gates fire (cache miss because input changed).
- [ ] **1.4.3 GREEN** Confirm push succeeds (gates pass with valid registry).
- [ ] **1.4.4** Manually break `specs/apps/wahidyankf/behavior/web/gherkin/` (rename a folder) and confirm gate aborts the push with a HIGH finding. Then revert.
- [ ] **1.4.5 GREEN** Push without spec changes — confirm both new targets are cache-hit (near-zero cost).

---

## Phase 2 — `organiclever-be:test:quick` DDD wiring (Fix #3)

- [ ] **2.1 RED** From the existing `apps/organiclever-be/project.json` `test:quick.options.commands`, the two `ddd bc/ul` invocations are absent. Capture this state.
- [ ] **2.2 GREEN** Edit `apps/organiclever-be/project.json`:
  - Prepend the two `ddd bc/ul` commands.
  - Add the two new `inputs` paths.
- [ ] **2.3 GREEN** `nx run organiclever-be:test:quick` — DDD validators run before dotnet test; both green.
- [ ] **2.4 GREEN** Touch `specs/apps/organiclever/ddd/bounded-contexts.yaml` (whitespace edit), reset to original. Re-run `nx run organiclever-be:test:quick`. Confirm cache miss (DDD re-runs).

---

## Phase 3 — Per-BC `code_lang:` field (Fix #4)

### 3.1 Schema bump

- [ ] **3.1.1 RED** Add unit test in `internal/bcregistry/bcregistry_test.go`:
  - Scenario: registry without `code_lang` defaults to `[ts, tsx]`.
  - Scenario: registry with `code_lang: [fs]` is decoded.
  - Scenario: unsupported `code_lang: [cobol]` errors clearly.
    Tests fail.
- [ ] **3.1.2 GREEN** Edit `internal/bcregistry/types.go` adding `CodeLang []string` to `Context`. Add `SupportedLangGlobs` map. Add `validateCodeLang()` helper.
- [ ] **3.1.3 GREEN** Edit `internal/bcregistry/loader.go`: post-decode loop sets default; calls `validateCodeLang`.
- [ ] **3.1.4 GREEN** All unit tests pass.

### 3.2 Validator change

- [ ] **3.2.1 RED** Add unit tests in `internal/glossary/validator_test.go`:
  - F#-only BC with `code_lang: [fs]` validates against .fs grep.
  - TS+F# BC with `code_lang: [ts, fs]` validates union.
  - TS-only BC (default) preserves today's behavior.
    Tests fail.
- [ ] **3.2.2 GREEN** Edit `internal/glossary/validator.go` `checkTerms()` and `checkForbiddenSynonyms()`: replace hardcoded `[]string{"*.ts", "*.tsx"}` with per-BC computed glob list.
- [ ] **3.2.3 GREEN** All unit tests pass. Integration tests in `cmd/ddd_ul.integration_test.go` updated for new fixtures.

### 3.3 Update existing registries

- [ ] **3.3.1** `specs/apps/organiclever/ddd/bounded-contexts.yaml` — leave `code_lang` absent (defaults to `[ts, tsx]`, correct for current FE-only contexts).
- [ ] **3.3.2** `specs/apps/{wahidyankf,oseplatform,ayokoding}/ddd/bounded-contexts.yaml` — same; default `[ts, tsx]` is correct since plans 1-3 only created TS bounded contexts.
- [ ] **3.3.3 GREEN** Run `rhino-cli ddd ul organiclever wahidyankf oseplatform ayokoding` — all green (no behavior change today; foundation laid for future polyglot BCs).

---

## Phase 4 — Multi-parent orphan-root walks (Fix #5)

- [ ] **4.1 RED** Add unit test in `internal/bcregistry/bcregistry_test.go`:
  - Multi-parent registry: contexts with gherkin under `behavior/web/gherkin/` AND `behavior/api/gherkin/`. Plant an orphan dir under each. Validator should report **both** orphans.
    Test fails because today only first parent is walked.
- [ ] **4.2 GREEN** Edit `internal/bcregistry/validator.go:208, 212` per `tech-docs.md` Fix #5: build `glossaryRoots` and `gherkinRoots` maps; iterate sorted slices.
- [ ] **4.3 GREEN** All existing tests pass; new test passes.
- [ ] **4.4 GREEN** Manual smoke: plant a real orphan under `specs/apps/oseplatform/behavior/api/gherkin/` (e.g. mkdir `bogus/`, touch `bogus.feature`), run `rhino-cli ddd bc oseplatform` — confirm orphan reported. Revert.

---

## Phase 5 — Multi-file scenario matching (Fix #6)

- [ ] **5.1 RED** Add unit test in `internal/speccoverage/checker_test.go`: feature with 2 scenarios, two test files (`feature.test.tsx` with scenario A, `feature.extra.test.tsx` with scenario B). Without fix, scenario B reported as gap. Test fails (today scenario B is reported).
- [ ] **5.2 GREEN** Refactor `findMatchingTestFile` → `findAllMatchingTestFiles`. Update `checkOneToOne` to union scenario titles across all matches before checking.
- [ ] **5.3 GREEN** All existing tests pass; new test passes.
- [ ] **5.4 GREEN** `--shared-steps` mode unchanged — confirm via existing tests.

---

## Phase 6 — Delete drift-\* placeholders (Fix #7)

- [ ] **6.1 RED** Add unit test in `cmd/specs_drift_routes_test.go` (or new file): `rhino-cli specs --help` does not list `drift-routes`. Today the test fails (drift-routes is registered).
- [ ] **6.2 GREEN** Delete files:
  - `apps/rhino-cli/cmd/specs_drift_routes.go`
  - `apps/rhino-cli/cmd/specs_drift_endpoints.go`
  - `apps/rhino-cli/cmd/specs_drift_contracts.go`
  - Their `_test.go` companions if any.
- [ ] **6.3 GREEN** `go build ./...` succeeds. `rhino-cli specs --help` lists 4 subcommands (validate-tree, validate-counts, validate-links, validate-adoption).
- [ ] **6.4** Update `governance/conventions/structure/specs-directory-structure.md`: add a "Drift detection" subsection noting these commands are not currently implemented; track via the tooling backlog.

---

## Phase 7 — Severity reconciliation (Fix #8)

- [ ] **7.1 RED** Update unit test in `cmd/specs_validate_counts_test.go` to assert HIGH for missing folder, MEDIUM for empty folder. Test fails (today missing reports MEDIUM).
- [ ] **7.2 GREEN** Edit `cmd/specs_validate_counts.go` — two changes required:
  1. In `validateSpecCounts()`, change the missing-folder finding's `Criticality` from `"MEDIUM"` to `"HIGH"` in the struct literal at the missing-folder branch (the empty-folder branch keeps `"MEDIUM"`).
  2. Replace the hardcoded `MEDIUM` in the printf format on line 48 (`%s: MEDIUM: %s`) with `%s: %s: %s` and pass `f.Criticality` so the runtime severity string matches the struct value.
- [ ] **7.3 GREEN** All tests pass. Manual smoke: create a missing/empty folder pair under a test fixture; confirm severity strings.

---

## Phase 7B — Wire validate-counts + validate-links per allowlist (Fix #12, #13)

> **Why this phase exists**: Plan goal is **zero dead specs/BDD/DDD scripts** after this plan ships. Fixes #1 and #2 wire `validate-adoption` + `validate-tree`; this phase wires the remaining two (`validate-counts` and `validate-links`) using the same allowlist pattern. Phase ordering: must run after Phase 7 (Fix #8) so the new `validate:specs-counts` gate inherits HIGH severity for missing folders.

### 7B.1 `--apps` flag on validate-counts (Fix #12)

- [ ] **7B.1.1 RED** Add unit tests in `cmd/specs_validate_counts_test.go` mirroring Fix #1+#2 pattern: "no positional, no flag → defaults to allowlist"; "explicit positional folder preserved"; "--apps flag overrides defaults". Tests fail.
- [ ] **7B.1.2 GREEN** Edit `cmd/specs_validate_counts.go`: add `--apps` StringSlice flag; if positional empty AND flag empty, use `allowlist.AppsWithDDD` (same import path as Phase 1.1). Today's positional folder behavior preserved.
- [ ] **7B.1.3 GREEN** Run unit tests. Coverage ≥90%.

### 7B.2 `--apps` flag on validate-links (Fix #13)

- [ ] **7B.2.1 RED** Add unit tests in `cmd/specs_validate_links_test.go` (same scenarios as 7B.1.1, scoped to validate-links). Tests fail.
- [ ] **7B.2.2 GREEN** Edit `cmd/specs_validate_links.go`: same pattern — add `--apps` StringSlice flag with allowlist default, preserve positional behavior.
- [ ] **7B.2.3 GREEN** Run unit tests. Coverage ≥90%.

### 7B.3 Nx targets

- [ ] **7B.3.1** Edit `apps/rhino-cli/project.json`:
  - Add `validate:specs-counts` target per `tech-docs.md` Fix #12 (cacheable, `inputs` mirror `validate:specs-tree`).
  - Add `validate:specs-links` target per `tech-docs.md` Fix #13 (cacheable, `inputs` cover `specs/apps/**/*.md`).
- [ ] **7B.3.2 GREEN** Run `nx run rhino-cli:validate:specs-counts` — exits 0 (assumes plans 1-3 merged).
- [ ] **7B.3.3 GREEN** Run `nx run rhino-cli:validate:specs-links` — exits 0.

### 7B.4 Pre-push wiring

- [ ] **7B.4.1** Edit `.husky/pre-push` to extend the existing nx affected line with `validate:specs-counts validate:specs-links`. Final form: `nx affected -t typecheck lint test:quick spec-coverage validate:specs-adoption validate:specs-tree validate:specs-counts validate:specs-links --parallel="$PARALLEL"`.
- [ ] **7B.4.2 RED** Stage a no-op edit to `specs/apps/wahidyankf/ddd/bounded-contexts.yaml` (whitespace) and run `git push --dry-run`. Confirm both new gates fire (cache miss because input changed).
- [ ] **7B.4.3 GREEN** Confirm push succeeds (gates pass with valid registry + intact links).
- [ ] **7B.4.4** Manually rename `specs/apps/wahidyankf/containers/` → `containers-bogus/` and confirm the validate:specs-counts gate aborts the push with a HIGH finding. Then revert.
- [ ] **7B.4.5** Manually introduce a broken markdown link in `specs/apps/wahidyankf/system-context/context.md` (e.g. `[broken](./does-not-exist.md)`) and confirm the validate:specs-links gate aborts the push. Then revert.
- [ ] **7B.4.6 GREEN** Push without spec changes — confirm both new targets are cache-hit (near-zero cost).

---

## Phase 8 — Severity audit log + env var rename (Fix #9)

- [ ] **8.1 RED** Add unit tests in `cmd/ddd_bc_test.go` and `cmd/ddd_ul_test.go`:
  - Scenario A: `OSE_RHINO_DDD_SEVERITY=warn` + invocation → stderr line emitted; exit 0 with warnings.
  - Scenario B: `ORGANICLEVER_RHINO_DDD_SEVERITY=warn` + invocation → deprecation stderr line + audit stderr line; exit 0.
  - Scenario C: both env vars set → `OSE_RHINO_DDD_SEVERITY` wins; legacy ignored without deprecation warning.
  - Scenario D: `--severity=error` + env var `warn` → flag wins; no audit line.
    Tests fail.
- [ ] **8.2 GREEN** Edit `cmd/ddd_bc.go:84-91` and `cmd/ddd_ul.go:82-89` per `tech-docs.md` Fix #9.
- [ ] **8.3 GREEN** All tests pass. Confirm `(cd apps/rhino-cli && OSE_RHINO_DDD_SEVERITY=warn go run main.go ddd bc organiclever)` emits the audit line to stderr and exits 0 even when findings exist.

---

## Phase 9 — Symmetry whitelist expansion (Fix #10)

- [ ] **9.1 RED** Add unit tests in `internal/bcregistry/bcregistry_test.go`:
  - `partnership` asymmetric (one-side declared) → finding.
  - `shared-kernel` asymmetric → finding.
  - `anticorruption-layer` one-side declared → no finding (one-way intentionally).
  - `open-host-service` one-side declared → no finding.
  - Unknown kind `made-up-kind` → "unknown relationship kind" finding.
    Tests fail.
- [ ] **9.2 GREEN** Edit `internal/bcregistry/validator.go`:
  - Expand `asymmetricKinds` to include `partnership`, `shared-kernel`.
  - Add `knownKinds` set + new validator pass that flags unknown kinds.
- [ ] **9.3 GREEN** All tests pass. Existing organiclever registry (uses only `customer-supplier` and `conformist`) continues to pass without change.

---

## Phase 10 — `gherkin: []string` schema extension (Fix #11)

### 10.1 Schema bump with auto-conversion

- [ ] **10.1.1 RED** Add unit tests in `internal/bcregistry/bcregistry_test.go`:
  - Single-string form decodes to one-element slice (backward compat with organiclever, plans 1, 2, 3).
  - List form `[behavior/web/gherkin/x, behavior/api/gherkin/x]` decodes intact.
  - Empty list errors clearly (`empty gherkin list`).
  - Tests fail.
- [ ] **10.1.2 GREEN** Edit `internal/bcregistry/types.go` per `tech-docs.md` Fix #11:
  - Replace `Gherkin string` with `Gherkin GherkinPaths`.
  - Add `GherkinPaths` named type with `UnmarshalYAML` that auto-converts scalar to single-element list.
- [ ] **10.1.3 GREEN** Edit `internal/bcregistry/loader.go`: post-decode loop validates `len(ctx.Gherkin) > 0`.
- [ ] **10.1.4 GREEN** All unit tests pass.

### 10.2 `checkGherkin()` per-path

- [ ] **10.2.1 RED** Add unit tests in `internal/bcregistry/bcregistry_test.go`:
  - List with one missing path → finding for missing path; passing path validated.
  - List with one path lacking .feature → "no feature files" finding for that path only.
  - Tests fail.
- [ ] **10.2.2 GREEN** Edit `internal/bcregistry/validator.go` `checkGherkin()`: loop `ctx.Gherkin` iterating each path independently per `tech-docs.md`.
- [ ] **10.2.3 GREEN** All tests pass.

### 10.3 `registeredGherkin` map population

- [ ] **10.3.1 RED** Combined fix #5 + #11 test: orphan under api/gherkin/ when registry declares both perspectives → reported. Today's code (single-parent walk + single-path) doesn't catch it; with both fixes it does.
- [ ] **10.3.2 GREEN** Edit `internal/bcregistry/validator.go` `validate()`: `registeredGherkin` loop iterates `ctx.Gherkin`.
- [ ] **10.3.3 GREEN** All tests pass.

### 10.4 Glossary validator

- [ ] **10.4.1 RED** Test in `internal/glossary/validator_test.go`: glossary feature reference resolvable under either of a BC's two declared gherkin paths is "found".
- [ ] **10.4.2 GREEN** Edit `internal/glossary/validator.go` `checkTerms`: iterate `ctx.Gherkin` paths; first match wins.
- [ ] **10.4.3 GREEN** All tests pass.

### 10.5 Backward-compat smoke

- [ ] **10.5 GREEN** Run `rhino-cli ddd bc organiclever` — exits 0 (single-string `gherkin:` form auto-converts; behavior unchanged).
- [ ] **10.6 GREEN** Run `rhino-cli ddd bc <each plan-1-3 app>` — exits 0 (same single-string form, no migration required).

---

## Phase 11 — Documentation + agent-binding updates

- [ ] **11.1** Update `governance/conventions/structure/specs-directory-structure.md`:
  - Document the allowlist policy (Phase 1).
  - Document `code_lang:` schema field (Phase 3).
  - Document drift-\* removal (Phase 6).
  - Document the four allowlist-driven pre-push gates: `validate:specs-adoption`, `validate:specs-tree`, `validate:specs-counts`, `validate:specs-links` (Phases 1, 7B). State explicitly that **zero `specs *` commands ship dead** after this plan.
  - Document severity audit + env var rename (Phase 8).
  - Document expanded symmetry whitelist (Phase 9).
  - Document `gherkin: []string` extension (Phase 10).
- [ ] **11.2** Update `.claude/agents/specs-checker.md` and `.claude/agents/specs-fixer.md`:
  - Drop references to `drift-routes`, `drift-endpoints`, `drift-contracts`.
  - Reference the new `validate:specs-adoption` and `validate:specs-tree` Nx targets.
- [ ] **11.3** Run `npm run sync:claude-to-opencode` — confirm `.opencode/agents/` mirror updated.
- [ ] **11.4** Run `npm run validate:sync` — parity confirmed.
- [ ] **11.5** `npm run lint:md` — fix violations.

---

## Phase 12 — Final validation gate

> **Important**: Fix ALL failures found during quality gates, not just those caused by your changes. This follows the root cause orientation principle — proactively fix preexisting errors encountered during work.

- [ ] **12.1** `(cd apps/rhino-cli && go build ./... && go test ./...)` — all pass.
- [ ] **12.2** `nx run rhino-cli:test:quick` — coverage ≥90%.
- [ ] **12.3** `nx run rhino-cli:validate:specs-adoption` — 0 findings (4 web apps).
- [ ] **12.4** `nx run rhino-cli:validate:specs-tree` — 0 findings.
- [ ] **12.4b** `nx run rhino-cli:validate:specs-counts` — 0 findings.
- [ ] **12.4c** `nx run rhino-cli:validate:specs-links` — 0 findings.
- [ ] **12.4d** `git grep -l "cobra.Command" apps/rhino-cli/cmd/specs_*.go apps/rhino-cli/cmd/ddd_*.go apps/rhino-cli/cmd/spec_coverage*.go` cross-checked against `apps/rhino-cli/project.json` + `.husky/pre-push` + each app's `project.json` — every command file is referenced by at least one invocation; the three `specs_drift_*.go` files no longer exist. **Zero dead specs/BDD/DDD scripts confirmed.**
- [ ] **12.5** For each of the 4 web apps: `nx run <app>-web:test:quick` — DDD passes.
- [ ] **12.6** `nx run organiclever-be:test:quick` — DDD passes.
- [ ] **12.7** `nx affected -t typecheck lint test:quick spec-coverage validate:specs-adoption validate:specs-tree validate:specs-counts validate:specs-links --base=HEAD~1` — full pre-push gate green.
- [ ] **12.8** `npm run lint:md` — 0 violations.
- [ ] **12.9** `npm run validate:sync` — `.claude/` ↔ `.opencode/` parity.
- [ ] **12.10** Manual smoke per fix:
  - **#1**: edit `specs/apps/wahidyankf/ddd/bounded-contexts.yaml` whitespace; `git push --dry-run` — both new gates fire.
  - **#3**: `nx run organiclever-be:test:quick` cold cache → DDD validators run.
  - **#4**: temporarily add `code_lang: [fs]` to one organiclever BC + a stale F# identifier in its glossary → `ddd ul` reports stale identifier. Revert.
  - **#5**: plant orphan under `specs/apps/oseplatform/behavior/api/gherkin/bogus/` → `ddd bc oseplatform` reports orphan. Revert.
  - **#7**: `rhino-cli specs --help` shows 4 subcommands.
  - **#8**: missing folder + empty folder → distinct severities.
  - **#9**: `OSE_RHINO_DDD_SEVERITY=warn rhino-cli ddd bc organiclever` emits stderr audit line.
  - **#11**: edit `specs/apps/ayokoding/ddd/bounded-contexts.yaml` to declare `gherkin: [behavior/web/gherkin/content, behavior/api/gherkin/content]` for content BC; `rhino-cli ddd bc ayokoding` exits 0. Revert.
  - **#12**: rename `specs/apps/wahidyankf/containers/` → `containers-bogus/`; `git push --dry-run` aborts via validate:specs-counts with HIGH severity. Revert.
  - **#13**: introduce a broken markdown link in `specs/apps/wahidyankf/system-context/context.md` (`[broken](./does-not-exist.md)`); `git push --dry-run` aborts via validate:specs-links. Revert.

---

## Phase 13 — Commit, push, archive

### Commit Guidelines

- [ ] Commit changes thematically — group related changes into logically cohesive commits.
- [ ] Follow Conventional Commits format: `<type>(<scope>): <description>`.
- [ ] Split different domains/concerns into separate commits (e.g., allowlist wiring separate from schema changes separate from governance doc updates).
- [ ] Do NOT bundle unrelated fixes into a single commit.

- [ ] **13.1** Commit per phase OR single atomic. Recommended: **single atomic commit** since fixes are governance-shaped and tightly coupled.
  - Message: `feat(rhino-cli): close BDD+DDD tooling enforcement gaps`
  - Body lists 13 fixes by number, ending with the headline outcome: "zero dead specs/BDD/DDD scripts in rhino-cli".
- [ ] **13.2** Push via Trunk Based Development (default) or draft PR (optional).
- [ ] **13.3** Wait for `main` CI green — specifically monitor the `CI` workflow at `https://github.com/wahidyankf/ose-public/actions` for the push commit. Per `governance/development/workflow/ci-monitoring.md`.
- [ ] **13.4** Move plan folder to `plans/done/YYYY-MM-DD__bdd-ddd-tooling-gap-fill/`.
- [ ] **13.5** Update `plans/in-progress/README.md` and `plans/done/README.md`.
- [ ] **13.6** Surface for downstream: confirm `repo-ose-primer-propagation-maker` has the new constants, agent definitions, and validator changes on its propagation list. The maker runs in dry-run by default; an actual primer PR is a separate decision.
- [ ] **13.7** Optional follow-up commit: edit `specs/apps/ayokoding/ddd/bounded-contexts.yaml` to migrate the four multi-perspective BCs (`content`, `search`, `i18n`, `navigation`) from single-string `gherkin:` to list form `[behavior/web/gherkin/<bc>, behavior/api/gherkin/<bc>]`. Single yaml edit; out of scope of this plan but unblocked by it.

---

## Notes on dependency timing

- **If plans 1-3 ship one-by-one and plan 4 ships immediately after each completes**: rework Phase 0.1 to gate only on "the relevant subset of plans 1-3" and gate Phases 1.4 (pre-push wiring) on all-three-done. The allowlist gate must be deferred until every allowlisted app actually has the new shape.
- **If plan 4 ships before any of plans 1-3 (not recommended)**: the Phase 1.4 pre-push wiring would fail. In that case, deliver Phases 1.1 + 1.2 + 1.3 (validator + Nx targets) without 1.4 (pre-push), let 1.4 land in a second commit after plans 1-3 are all green.
