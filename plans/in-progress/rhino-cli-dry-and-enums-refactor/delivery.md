# Delivery Checklist — rhino-cli DRY + Enum Refactor Pass

All steps follow Red → Green → Refactor (TDD) where applicable. Where a
change is pure mechanical replacement that breaks no test, RED is
implicit (the existing test suite is the contract; it must stay green
after each commit). Run `npx nx run rhino-cli:test:quick` at the end of
every phase. Run `npx nx affected -t typecheck lint test:quick spec-coverage`
before push.

## Worktree

Worktree path: `worktrees/async-rolling-gizmo/` [Judgment call — reuses the
existing worktree provisioned at plan authoring time. Convention requires
`worktrees/rhino-cli-dry-and-enums-refactor/` but the existing worktree is
already populated and on branch `worktree/async-rolling-gizmo`.]

See [Worktree Path Convention](../../../governance/conventions/structure/worktree-path.md)
and [Plans Organization Convention §Worktree Specification](../../../governance/conventions/structure/plans.md#worktree-specification).

If starting fresh, provision via:

```bash
claude --worktree async-rolling-gizmo
```

Toolchain initialization (run once in the root worktree before Phase 0
if not already done):

```bash
npm install
npm run doctor -- --fix
```

---

## Commit Guidelines

- Follow Conventional Commits: `<type>(<scope>): <description>`.
  Type is `refactor` for all phases in this plan.
- Commit one phase per commit as shown in each phase's COMMIT step.
- Do NOT bundle multiple phases into a single commit.
- Fix-forward commits for quality gate failures use the same scope as
  the phase that introduced the failure.

---

## Phase 0 — Baseline capture

Pre-refactor capture. No code changes yet. Output stored in
`local-temp/refactor-baseline/` (gitignored).

- [ ] **0.1** — Create capture directory:
      `mkdir -p local-temp/refactor-baseline`.

- [ ] **0.2** — Capture pre-refactor LOC for the 7 per-language extractor
      files:
      `wc -l apps/rhino-cli/internal/speccoverage/{rust,dart,java,python,elixir,clojure,dotnet}_steps.go > local-temp/refactor-baseline/extractor-loc-before.txt`.

- [ ] **0.3** — Capture pre-refactor stdout for representative
      subcommands against committed fixtures (use existing tests'
      fixture data; do NOT alter committed files):
  - `cd apps/rhino-cli && go run . agents validate-claude > /tmp/baseline-validate-claude-text.txt 2>&1; echo "exit=$?" >> /tmp/baseline-validate-claude-text.txt`
  - `cd apps/rhino-cli && go run . agents validate-claude -o json > /tmp/baseline-validate-claude-json.txt 2>&1; echo "exit=$?" >> /tmp/baseline-validate-claude-json.txt`
  - `cd apps/rhino-cli && go run . doctor --scope minimal > /tmp/baseline-doctor-min.txt 2>&1; echo "exit=$?" >> /tmp/baseline-doctor-min.txt`
  - `cd apps/rhino-cli && go run . specs validate-tree organiclever > /tmp/baseline-specs-tree.txt 2>&1; echo "exit=$?" >> /tmp/baseline-specs-tree.txt`
  - Move all `/tmp/baseline-*.txt` to `local-temp/refactor-baseline/`.

- [ ] **0.4** — Run `npx nx run rhino-cli:test:quick` and capture coverage
      number to `local-temp/refactor-baseline/coverage-before.txt`. This
      is the floor; coverage must not drop below this minus 0.5%.

- [ ] **0.5** — Audit `stepMatcher.exact` / `.patterns` test usage:
      `grep -rn '\.exact\[\|\.patterns\b' apps/rhino-cli/internal/speccoverage/*_test.go > local-temp/refactor-baseline/legacy-field-usage.txt`.
      If the file lists more than 5 distinct test files touching these
      fields, item 14 (Phase 9) is split to a follow-up plan; mark
      item 14 deferred in `done/` move when archiving.

- [ ] **0.6** — Commit baseline capture:
      `git add local-temp/.gitignore || true; echo "phase 0 — baseline captured (no code changes)"` —
      this is the only no-commit phase; no code changed, no git commit
      created.

---

## Phase 1 — `agents.CheckStatus` sealed interface + `ValidationResult.Add`

Foundation phase. Broadest reach, lowest risk. ~232 string-literal sites.

Pattern: sealed interface (`//sumtype:decl`) per the
`velvety-herding-ullman` baseline — see tech-docs.md Item 2.

- [ ] **1.1 GREEN (sealed interface)** — Edit
      `apps/rhino-cli/internal/agents/types.go`: add `//sumtype:decl`
      `CheckStatus` interface with marker `isCheckStatus()` and `Code() string`,
      plus variants `StatusPassed{}`, `StatusWarning{}`, `StatusFailed{}`
      and a `ParseCheckStatus(s string) (CheckStatus, bool)` helper.
      Change `ValidationCheck.Status` field type from `string` to
      `CheckStatus`. Every new exported identifier carries a doc comment
      ending in a period (godot + revive).

- [ ] **1.2 GREEN (callsite migration — internal/agents)** — Replace every
      string literal `Status: "passed"` / `"warning"` / `"failed"` in
      `apps/rhino-cli/internal/agents/*.go` (non-test) with the
      corresponding zero-value variant (e.g., `StatusPassed{}`). Files
      touched: `agent_validator.go`, `claude_validator.go`,
      `skill_validator.go`, `sync_validator.go`, `yaml_formatting.go`.
      Verify with
      `grep -rn 'Status:\s*"passed"\|Status:\s*"warning"\|Status:\s*"failed"' apps/rhino-cli/internal/agents/*.go | grep -v _test.go` —
      must return 0.

- [ ] **1.3 GREEN (JSON wire-format)** — Add a wire-format struct in the
      reporter (or marshalling helper) that exposes `Status string`
      populated via `check.Status.Code()`, mirroring
      `internal/doctor/reporter.go:105`. Existing JSON output
      (`"status": "passed"`) is preserved byte-for-byte.

- [ ] **1.4 GREEN (test migration)** — Update `*_test.go` files in
      `apps/rhino-cli/internal/agents/` that compare `check.Status` to
      a literal string. Replace with type assertion
      (`_, ok := check.Status.(StatusPassed)`) or compare via
      `check.Status.Code() == "passed"`. JSON-output assertions keep
      the literal `"passed"` since strings are the wire format.

- [ ] **1.5 GREEN (Add method)** — Add `func (r *ValidationResult) Add(check ValidationCheck)`
      method in `types.go` per tech-docs.md item 10. Use exhaustive type
      switch on `check.Status.(type)`; `gochecksumtype` enforces every
      variant is handled.

- [ ] **1.6 GREEN (sync_validator migration)** — Edit
      `apps/rhino-cli/internal/agents/sync_validator.go`: replace 4×
      duplicated `result.Checks = append(...) ; result.PassedChecks++ / FailedChecks++ ; result.TotalChecks++`
      blocks with `result.Add(check)`. Lines affected approximately
      22-67. Tests must continue to pass with no edits.

- [ ] **1.7 PHASE GATE** — Run
      `cd apps/rhino-cli && golangci-lint run ./...` — exits 0 (no new
      gochecksumtype, godot, revive, or errorlint violations introduced).
      Then run `npx nx run rhino-cli:test:quick`. Exit 0, coverage ≥
      baseline − 0.5%.

- [ ] **1.8 COMMIT** — `refactor(rhino-cli/agents): introduce CheckStatus sealed interface and ValidationResult.Add`.

---

## Phase 2 — `cmd/helpers.go` `mustFindGitRoot`

Foundation phase. 24-file mechanical replace.

- [ ] **2.1 GREEN (helper)** — Edit
      `apps/rhino-cli/cmd/helpers.go`: add `mustFindGitRoot(cmd *cobra.Command) (string, error)`
      per tech-docs.md item 16.

- [ ] **2.2 GREEN (callsite migration)** — Replace the 3-line
      `findGitRoot()` + error-wrap preamble in 24 cmd files with
      `repoRoot, err := mustFindGitRoot(cmd); if err != nil { return err }`.
      Files affected: every `cmd/*.go` runE function that calls
      `findGitRoot()`. Use a single `sed` invocation followed by manual
      review:
      `cd apps/rhino-cli && grep -l 'findGitRoot()' cmd/*.go | grep -v _test.go`.

- [ ] **2.3 PHASE GATE** — `npx nx run rhino-cli:test:quick` exits 0.

- [ ] **2.4 COMMIT** — `refactor(rhino-cli/cmd): consolidate findGitRoot calls into mustFindGitRoot helper`.

---

## Phase 3 — `speccoverage.matcherKind` sealed interface

Closed-universe phase. ~12 sites across `checker.go`, `types.go`, and
`rust_steps.go` (non-test). `reporter.go` requires no changes — it uses
`o.MatcherKind` as a wire-format string field already.

Pattern: sealed interface (`//sumtype:decl`) per tech-docs.md item 1.

- [ ] **3.1 GREEN (sealed interface)** — Edit
      `apps/rhino-cli/internal/speccoverage/checker.go`: add
      `//sumtype:decl matcherKind` interface with marker `isMatcherKind()`
      and `Code() string`, plus variants `kindExact{}` and `kindPattern{}`.
      Change `stepMatcherEntry.Kind` field type from `string` to
      `matcherKind`. Doc comments end in periods (godot).

- [ ] **3.2 GREEN (callsite migration)** — Replace every
      `Kind: "exact"` / `Kind: "pattern"` write with `Kind: kindExact{}`
      / `Kind: kindPattern{}`. Replace `case "exact":` / `case "pattern":`
      string switches with type switches: `case kindExact:` /
      `case kindPattern:`. Replace `e.Kind == "pattern"` with type
      assertion: `_, isPattern := e.Kind.(kindPattern)`.
      `gochecksumtype` enforces exhaustiveness — no `default:` arm.

- [ ] **3.3 GREEN (JSON wire bridge)** — `OrphanStepImpl.MatcherKind`
      stays `string` (wire format). At the orphan collection site
      (`checkOrphanStepImpls`), populate via `e.Kind.Code()`. Verify
      JSON output unchanged: rerun the Phase 0 spec-coverage capture
      and diff against baseline.

- [ ] **3.4 PHASE GATE** —
      `cd apps/rhino-cli && golangci-lint run ./internal/speccoverage/...` —
      exits 0 (gochecksumtype reports zero exhaustiveness violations).
      Then `npx nx run rhino-cli:test:quick` exits 0.
      `grep -n '"exact"\|"pattern"' apps/rhino-cli/internal/speccoverage/*.go | grep -v _test.go` —
      should only show the `Code()` method bodies and the JSON-wire
      population.

- [ ] **3.5 COMMIT** — `refactor(rhino-cli/speccoverage): introduce sealed matcherKind interface`.

---

## Phase 4 — `bcregistry.Severity` + `glossary.Severity` sealed interface + ddd consolidation

Closed-universe + DRY in one phase (small enough). Item 4 + Item 8.

Pattern: each package defines its own `//sumtype:decl` `Severity` sealed
interface per tech-docs.md item 4.

- [ ] **4.1 GREEN (bcregistry Severity)** — Edit
      `apps/rhino-cli/internal/bcregistry/types.go`: add a
      `//sumtype:decl Severity` interface with marker `isSeverity()` and
      `Code() string`, plus variants `SeverityError{}`, `SeverityWarn{}`,
      and a `ParseSeverity(s string) (Severity, bool)` helper. Change
      `Finding.Severity` and `ValidateOptions.Severity` field types
      from `string` to `Severity`. Doc comments end in periods.

- [ ] **4.2 GREEN (glossary Severity)** — Same pattern in
      `apps/rhino-cli/internal/glossary/types.go`.

- [ ] **4.3 GREEN (internal callsites)** — Replace string literals
      `Severity: "error"` / `Severity: "warn"` in `bcregistry/*.go` and
      `glossary/*.go` non-test files with the corresponding zero-value
      variant (e.g., `Severity: SeverityError{}`). Severity comparisons
      in switches use type-switch on `.(type)`.

- [ ] **4.4 GREEN (cmd/severity.go)** — Create
      `apps/rhino-cli/cmd/severity.go` (_New file_) with shared
      `resolveSeverity(flagVal string) string` (returns the wire string
      `"error"` or `"warn"`) and `normaliseSeverity` per tech-docs.md
      item 8. The cmd-layer keeps the wire-string contract for backwards
      compatibility with `--severity` flag and `OSE_RHINO_DDD_SEVERITY`
      env var; sealed-interface conversion happens at the
      bcregistry/glossary boundary via `ParseSeverity`.

- [ ] **4.5 GREEN (ddd_bc.go migration)** — Edit
      `apps/rhino-cli/cmd/ddd_bc.go`: delete `resolveBcSeverity`,
      `normaliseSeverity`. Call site becomes
      `sev := resolveSeverity(bcSeverity)`. Pass `sev` through to
      `bcregistry.ValidateAll` which calls `ParseSeverity` internally.

- [ ] **4.6 GREEN (ddd_ul.go migration)** — Edit
      `apps/rhino-cli/cmd/ddd_ul.go`: delete `resolveUlSeverity`,
      `normaliseUlSeverity`. Call site becomes
      `sev := resolveSeverity(ulSeverity)`.

- [ ] **4.6b GREEN (delete old severity test file)** — Delete
      `apps/rhino-cli/cmd/ddd_severity_test.go` (tests the now-deleted
      `resolveBcSeverity` / `resolveUlSeverity` functions; will not compile
      after steps 4.5 and 4.6). Acceptance criterion:
      `go build ./apps/rhino-cli/cmd/...` exits 0.

- [ ] **4.7 GREEN (test for severity precedence)** — Add
      `apps/rhino-cli/cmd/severity_test.go` (_New file_) covering: flag wins,
      env wins when no flag, default when neither, env "warn" emits
      stderr warning. RED on file creation, GREEN once impl works.

- [ ] **4.8 PHASE GATE** —
      `cd apps/rhino-cli && golangci-lint run ./internal/bcregistry/... ./internal/glossary/... ./cmd/...` —
      exits 0. Then `npx nx run rhino-cli:test:quick` exits 0.
      Re-run Phase 0 ddd_bc + ddd_ul output captures with both flag and
      env permutations; diff against baseline (output byte-identical).

- [ ] **4.9 COMMIT** — `refactor(rhino-cli): sealed Severity interface + shared resolveSeverity helper`.

---

## Phase 5 — Mermaid sealed-enum verification (no-op phase)

Per `velvety-herding-ullman` rebase: `Direction`, `ViolationKind`,
`WarningKind` are already sealed interfaces in main, and
`gochecksumtype` enforces exhaustiveness at lint time. This phase
contains no source edits; it exists to record that the mermaid
exhaustivity item (originally Item 5) is satisfied by the upstream
work and verified once at plan execution.

- [ ] **5.1 VERIFY** — Run
      `cd apps/rhino-cli && golangci-lint run ./internal/mermaid/...` —
      exit 0 with zero `gochecksumtype` violations. If violations
      surface (e.g., a future variant added without updating switches),
      fix in place: add the missing case to each affected type switch.
      No code changes are expected at plan start.

- [ ] **5.2 NO COMMIT** — This phase produces no diff under the
      expected baseline. If 5.1 surfaced a violation requiring a fix,
      commit as
      `refactor(rhino-cli/mermaid): exhaust sealed-enum switch for <variant>`.

---

## Phase 6 — `cmd.Criticality` sealed interface + specs subcommand consolidation

Type-safety + DRY. Item 3 + Item 9 paired (Criticality is consumed by
the new specs driver).

Pattern: sealed interface (`//sumtype:decl`) per tech-docs.md item 3.

- [ ] **6.1 GREEN (Criticality sealed interface)** — Edit
      `apps/rhino-cli/cmd/specs_validate_tree.go` (where SpecFinding is
      defined): add a `//sumtype:decl Criticality` interface with marker
      `isCriticality()` and `Code() string`, plus variants
      `CriticalityHigh{}`, `CriticalityMedium{}`, `CriticalityLow{}`.
      Change `SpecFinding.Criticality` field type from `string` to
      `Criticality`. JSON wire-format preservation: pick the smaller-diff
      approach (separate string field OR `MarshalJSON` method) — verify
      via 6.3 golden output diff.

- [ ] **6.2 GREEN (callsite migration)** — Replace `Criticality: "HIGH"`,
      `"MEDIUM"`, `"LOW"` writes in
      `apps/rhino-cli/cmd/specs_validate_*.go` with the corresponding
      variant (e.g., `Criticality: CriticalityHigh{}`). Comparisons use
      type assertion or `f.Criticality.Code()`.

- [ ] **6.3 GREEN (golden output capture)** — Pre-driver-merge: capture
      the current output for each of the 4 specs subcommands against
      `local-temp/refactor-baseline/` fixtures (or use a real spec
      tree that has known findings). Save to
      `local-temp/refactor-baseline/specs-{tree,counts,links,adoption}-before.txt`.

- [ ] **6.4 GREEN (resolvers)** — Create
      `apps/rhino-cli/cmd/specs_apps.go` with `resolveSpecsAppNames`
      and `resolveSpecsAppFolders` per tech-docs.md item 9. Add unit
      tests for both.

- [ ] **6.5 GREEN (driver)** — Create
      `apps/rhino-cli/cmd/specs_driver.go` with `runSpecsValidator`
      per tech-docs.md item 9. Add a unit test against an in-memory
      validator stub.

- [ ] **6.6 GREEN (validate-tree migration)** — Edit
      `apps/rhino-cli/cmd/specs_validate_tree.go`: replace
      `runSpecsValidateTree` body with `runSpecsValidator(cmd, "validate-tree", apps, validateSpecTree)`.
      Delete `resolveTreeApps` (replaced by `resolveSpecsAppNames`).

- [ ] **6.7 GREEN (validate-adoption migration)** — Same pattern in
      `specs_validate_adoption.go`.

- [ ] **6.8 GREEN (validate-counts migration)** — Same pattern in
      `specs_validate_counts.go`. The `validate-counts` finding output
      uses `f.Criticality` while the others hardcode `HIGH`; the
      driver uses `f.Criticality` uniformly. Verify byte-identical
      output via 6.3 baseline diff.

- [ ] **6.9 GREEN (validate-links migration)** — Same pattern in
      `specs_validate_links.go`. Note: validate-links resolves to
      folder paths via `resolveSpecsAppFolders`, not to app names.

- [ ] **6.10 PHASE GATE** — `npx nx run rhino-cli:test:quick` exits 0.
      Re-run 6.3 captures and diff against the pre-driver baseline:
      output must be byte-identical.

- [ ] **6.11 COMMIT** — `refactor(rhino-cli/cmd/specs): typed Criticality + shared specs driver`.

---

## Phase 7 — Naming validator runE consolidation

DRY. Item 15.

- [ ] **7.1 GREEN (driver)** — Create
      `apps/rhino-cli/cmd/naming_driver.go` with
      `runNamingValidator(cmd, label, kind, fn)` per tech-docs.md
      item 15.

- [ ] **7.2 GREEN (agents migration)** — Edit
      `apps/rhino-cli/cmd/agents_validate_naming.go`: replace
      `runValidateAgentsNaming` body with one-line call to
      `runNamingValidator`. Keep `agentsValidateNamingFn` injection
      point untouched (tests rely on it).

- [ ] **7.3 GREEN (workflows migration)** — Same pattern in
      `apps/rhino-cli/cmd/workflows_validate_naming.go`.

- [ ] **7.4 PHASE GATE** — `npx nx run rhino-cli:test:quick` exits 0.

- [ ] **7.5 COMMIT** — `refactor(rhino-cli/cmd): shared runNamingValidator driver`.

---

## Phase 8 — Doctor `withEmptyOK` decorator + `minimal` field

Items 12 and 13 paired (both touch `internal/doctor/`).

`Scope` is already a sealed interface in main per `velvety-herding-ullman`
(`ScopeFull{}`, `ScopeMinimal{}`); this phase removes the parallel
`MinimalTools` map and migrates the scope branch to a sealed-interface
type switch (gochecksumtype enforced).

- [ ] **8.1 GREEN (decorator)** — Edit
      `apps/rhino-cli/internal/doctor/checker.go`: add
      `withEmptyOK(f compareFn) compareFn` per tech-docs.md item 12.
      Decorator returns `StatusOK{}` (sealed-interface zero-value
      variant) when `required == ""`.

- [ ] **8.2 GREEN (compareXxx migration)** — Strip the
      `if required == "" { return StatusOK{}, "no version requirement" }`
      preamble from `compareExact`, `compareMajor`, `compareMajorGTE`,
      `compareGTE`. Wrap each at the `buildToolDefs` site with
      `withEmptyOK(...)`.

- [ ] **8.3 GREEN (toolDef.minimal field)** — Edit
      `apps/rhino-cli/internal/doctor/tools.go`: add
      `minimal bool` field on `toolDef`. Mark each entry currently in
      `MinimalTools` with `minimal: true` (git, volta, node, npm,
      golang, docker, jq). Doc comment on the new field ends in a
      period (godot).

- [ ] **8.4 GREEN (CheckAll filter)** — Edit `checker.go` `CheckAll`:
      replace the `MinimalTools[def.name]` map lookup with `def.minimal`.
      The scope branch becomes an exhaustive type switch on the sealed
      `Scope` interface per tech-docs.md item 13 — a `switch opts.Scope.(type)`
      with `case nil, ScopeFull:` (all defs) and `case ScopeMinimal:`
      (filter on `def.minimal`). `gochecksumtype` enforces every variant
      is handled; no `default` arm.

- [ ] **8.5 GREEN (delete MinimalTools)** — Delete the `MinimalTools`
      var from `apps/rhino-cli/internal/doctor/types.go` and remove its
      doc comment.

- [ ] **8.6 PHASE GATE** —
      `cd apps/rhino-cli && golangci-lint run ./internal/doctor/...` —
      exits 0 (gochecksumtype + godot + revive + errorlint clean).
      Then `npx nx run rhino-cli:test:quick` exits 0. Re-run Phase 0
      doctor capture; diff against baseline (output byte-identical).

- [ ] **8.7 COMMIT** — `refactor(rhino-cli/doctor): drop MinimalTools map for toolDef.minimal + withEmptyOK decorator`.

---

## Phase 9 — `stepMatcher` legacy field removal (gated)

Code-deletion phase. Item 14. Gated by Phase 0.5 audit.

- [ ] **9.1 GATE** — Re-read
      `local-temp/refactor-baseline/legacy-field-usage.txt`. If more
      than 5 distinct test files touch `.exact[` or `.patterns` field
      directly, **STOP**. Mark item 14 deferred. Skip remainder of
      Phase 9. Document deferral in done/ archival commit.

- [ ] **9.2 GREEN (matches migration)** — Edit
      `apps/rhino-cli/internal/speccoverage/checker.go` `matches()`:
      switch from `sm.exact[normalized]` to
      `_, ok := sm.exactIndex[normalized]; ok`. Iterate `sm.entries`
      filtered by `Kind == kindPattern` instead of `sm.patterns`.

- [ ] **9.3 GREEN (test migration)** — For each test file listed in
      9.1: replace direct `sm.exact[k] = true` and
      `sm.patterns = append(sm.patterns, re)` writes with
      `sm.addExactWithOrigin(k, "")` and `sm.addPatternWithOrigin(re, "", "")`.

- [ ] **9.4 GREEN (delete fields)** — Edit `checker.go`: delete
      `exact` and `patterns` fields from `stepMatcher` struct. Delete
      writes to them in `addExactWithOrigin`, `addPatternWithOrigin`,
      `newStepMatcher`.

- [ ] **9.5 PHASE GATE** — `npx nx run rhino-cli:test:quick` exits 0.

- [ ] **9.6 COMMIT** — `refactor(rhino-cli/speccoverage): drop legacy stepMatcher.exact/patterns fields`.

---

## Phase 10 — Per-language step extractor consolidation

Largest LOC reduction. Item 6 + Item 7.

- [ ] **10.1 GREEN (scan_helpers.go)** — Create
      `apps/rhino-cli/internal/speccoverage/scan_helpers.go` with
      `extractStrategy` enum, `extractRule` struct, `scanLines`,
      `scanFull`, `applyRules` per tech-docs.md item 6. Include
      `default: panic` in the strategy switch.

- [ ] **10.2 GREEN (helpers unit tests)** — Add
      `apps/rhino-cli/internal/speccoverage/scan_helpers_test.go`
      covering: `scanLines` against a fixture, `scanFull` against a
      fixture, each strategy variant.

- [ ] **10.3 GREEN (clojure migration)** — Edit
      `apps/rhino-cli/internal/speccoverage/clojure_steps.go`: replace
      `extractClojureStepTexts` body with a `cljRules` declaration +
      `scanLines(path, sm, cljRules)` call. Verify:
      `go build ./apps/rhino-cli/...` exits 0.

- [ ] **10.4 GREEN (java migration)** — Edit
      `apps/rhino-cli/internal/speccoverage/java_steps.go`: same pattern for
      `extractJVMStepTexts`. Verify: `go build ./apps/rhino-cli/...` exits 0.

- [ ] **10.5 GREEN (elixir migration)** — Edit
      `apps/rhino-cli/internal/speccoverage/elixir_steps.go`: replace
      `extractElixirStepTexts` body with an `elixirRules` declaration +
      `scanLines(path, sm, elixirRules)` call. Verify:
      `go build ./apps/rhino-cli/...` exits 0.

- [ ] **10.6 GREEN (rust migration)** — Edit
      `apps/rhino-cli/internal/speccoverage/rust_steps.go`: replace
      `extractRustStepTexts` body with a `rustRules` declaration +
      `scanLines` call. Three rules in priority order: regex, expr, literal.
      Verify: `go build ./apps/rhino-cli/...` exits 0.

- [ ] **10.7 GREEN (dart migration)** — Edit
      `apps/rhino-cli/internal/speccoverage/dart_steps.go`: replace
      `extractDartStepTexts` body using `scanFull`. Verify:
      `go build ./apps/rhino-cli/...` exits 0.

- [ ] **10.8 GREEN (python migration)** — Edit
      `apps/rhino-cli/internal/speccoverage/python_steps.go`: replace
      `extractPythonStepTexts` body using `scanFull` and the
      `strategyAddPython` strategy. Add the `{{ → {` transform to the rule.
      Verify: `go build ./apps/rhino-cli/...` exits 0.

- [ ] **10.9 GREEN (csharp migration)** — Edit
      `apps/rhino-cli/internal/speccoverage/dotnet_steps.go`: replace
      `extractCSharpStepTexts` body using `scanFull` with two rules
      (verbatim string with `"" → "` transform; regular string). Verify:
      `go build ./apps/rhino-cli/...` exits 0.

- [ ] **10.10 GREEN (fsharp migration)** — Edit
      `apps/rhino-cli/internal/speccoverage/dotnet_steps.go` (both C# and
      F# extractors live in this file): update `extractFSharpStepTexts`.
      It anchors with `^...$`. Either add `strategyAddAnchoredPattern`
      or keep its own loop (decide by reviewing the helper signature
      complexity). If kept bespoke, document why with a comment
      pointing at the anchoring requirement. Verify:
      `go build ./apps/rhino-cli/...` exits 0.

- [ ] **10.11 GREEN (TS migration)** — Edit
      `apps/rhino-cli/internal/speccoverage/checker.go`: migrate
      `extractTSStepTexts` to two rules (string-style + regex-literal)
      using `scanFull`. Comment-stripping via `stripJSComments` stays as-is.
      Verify: `go build ./apps/rhino-cli/...` exits 0.

- [ ] **10.12 GREEN (Go migration)** — Edit
      `apps/rhino-cli/internal/speccoverage/checker.go`: migrate
      `extractGoStepTexts` to one rule using `scanLines` +
      `strategyAddPattern`. Verify: `go build ./apps/rhino-cli/...` exits 0.

- [ ] **10.13 GREEN (extractor registry)** — Edit
      `apps/rhino-cli/internal/speccoverage/checker.go`: replace switch in
      `extractAllStepTexts` (~line 723) with
      `var stepExtractorsByExt = map[string]func(string, *stepMatcher) error{...}` lookup.
      Verify: `go build ./apps/rhino-cli/...` exits 0.

- [ ] **10.14 GREEN (scenario extractor registry)** — Edit
      `apps/rhino-cli/internal/speccoverage/checker.go`: replace switch in
      `extractScenarioTitles` (~line 634) with
      `var scenarioExtractorsByExt = map[string]func(string) (map[string]bool, error){...}` lookup.
      Default (unmapped extension → TS extractor) preserved by an
      explicit fallback after the lookup. Verify:
      `go build ./apps/rhino-cli/...` exits 0.

- [ ] **10.15 PHASE GATE** — `npx nx run rhino-cli:test:quick` exits 0.
      Run `wc -l apps/rhino-cli/internal/speccoverage/{rust,dart,java,python,elixir,clojure,dotnet}_steps.go > /tmp/extractor-loc-after.txt` and verify ≥30%
      reduction vs `local-temp/refactor-baseline/extractor-loc-before.txt`.

- [ ] **10.16 COMMIT** — `refactor(rhino-cli/speccoverage): consolidate per-language step extractors via scan helpers + ext registry`.

---

## Phase 11 — Agents `passed`/`failed` constructor helpers

DRY. Item 11.

- [ ] **11.1 GREEN (helpers)** — Create
      `apps/rhino-cli/internal/agents/check_helpers.go` (_New file_) with
      `passed`, `failed`, `warning` constructors per tech-docs.md
      item 11.

- [ ] **11.2 GREEN (agent_validator migration)** — In
      `apps/rhino-cli/internal/agents/agent_validator.go`: replace
      every `ValidationCheck{Name: ..., Status: StatusPassed{}, Message: ...}`
      literal with `passed(name, message)`. Same for `StatusFailed{}`
      → `failed(...)` and `StatusWarning{}` → `warning(...)`. Where the
      literal includes a non-helper-shape field (e.g., a custom Name
      format), keep the literal.

- [ ] **11.3 GREEN (skill_validator migration)** — Same pattern for
      `apps/rhino-cli/internal/agents/skill_validator.go`.

- [ ] **11.4 PHASE GATE** — `npx nx run rhino-cli:test:quick` exits 0.

- [ ] **11.5 COMMIT** — `refactor(rhino-cli/agents): pass/fail constructor helpers`.

---

## Phase 12 — Final verification

- [ ] **12.1** — Run `npx nx run rhino-cli:test:quick`. Exit 0.

- [ ] **12.2** — Run `npx nx run rhino-cli:typecheck`. Exit 0.

- [ ] **12.3** — Run `npx nx run rhino-cli:lint`. Exit 0. Then run the
      full golangci-lint set explicitly to confirm zero violations across
      the new linters introduced by `velvety-herding-ullman`:
      `cd apps/rhino-cli && golangci-lint run ./...` — exit 0 with zero
      `gochecksumtype`, `errorlint`, `godot`, `revive (exported)`,
      `revive (package-comments)`, `exhaustive`, or `iotamixing`
      violations.

- [ ] **12.4** — Run coverage validation:
      `cd apps/rhino-cli && go run . test-coverage validate cover.out 90`. Exit 0.

- [ ] **12.5** — Re-run all Phase 0 captures and diff against
      baselines:
  - `agents validate-claude` (text + json)
  - `doctor --scope minimal`
  - `specs validate-tree organiclever`
  - `specs validate-counts`, `validate-links`, `validate-adoption`
  - `ddd bc`, `ddd ul` with various flag/env permutations.

  Each diff must be empty.

- [ ] **12.6** — Run repo-wide affected target:
      `npx nx affected -t typecheck lint test:quick spec-coverage`. Exit 0.

- [ ] **12.7** — Verify enum residue is gone — these greps must each
      return 0 hits in non-test source (the `Code()` method bodies and
      JSON wire-format population are the only allowed locations for
      the literal strings):
  - `grep -rn 'Status:\s*"passed"\|Status:\s*"warning"\|Status:\s*"failed"' apps/rhino-cli/internal/agents/*.go | grep -v _test.go`
  - `grep -rn 'Kind:\s*"exact"\|Kind:\s*"pattern"' apps/rhino-cli/internal/speccoverage/*.go | grep -v _test.go`
  - `grep -rn 'Severity:\s*"error"\|Severity:\s*"warn"' apps/rhino-cli/internal/bcregistry/*.go apps/rhino-cli/internal/glossary/*.go | grep -v _test.go`
  - `grep -rn 'Criticality:\s*"HIGH"\|Criticality:\s*"MEDIUM"\|Criticality:\s*"LOW"' apps/rhino-cli/cmd/*.go | grep -v _test.go`

- [ ] **12.7b** — Verify every new sealed-interface declaration is
      annotated and exhaustive. Run:
  - `grep -rn '//sumtype:decl' apps/rhino-cli/internal/agents/types.go apps/rhino-cli/internal/speccoverage/checker.go apps/rhino-cli/internal/bcregistry/types.go apps/rhino-cli/internal/glossary/types.go apps/rhino-cli/cmd/specs_validate_tree.go` —
    must return at least one match per file (the new declarations from
    Items 1, 2, 3, 4).
  - `cd apps/rhino-cli && golangci-lint run --enable-only=gochecksumtype ./...` — exits 0.

- [ ] **12.8** — Verify legacy field removal (gated by Phase 9):
  - `grep -rn '\.exact\[\|sm\.patterns\b\|stepMatcher.exact\|stepMatcher.patterns' apps/rhino-cli/internal/speccoverage/*.go | grep -v _test.go` —
    must return 0 hits if Phase 9 ran.

- [ ] **12.9** — Verify MinimalTools removal:
  - `grep -rn 'MinimalTools' apps/rhino-cli/internal/doctor/*.go | grep -v _test.go` —
    must return 0 hits.

- [ ] **12.10** — Open the `local-temp/refactor-baseline/extractor-loc-before.txt`
      and verify the LOC reduction in the 7 extractor files is ≥ 30%
      via:
      `wc -l apps/rhino-cli/internal/speccoverage/{rust,dart,java,python,elixir,clojure,dotnet}_steps.go`.

- [ ] **12.11** — Push all phase commits to `origin/main` (Trunk Based
      Development; commits already on main via fast-forward of the
      worktree branch). Use `git push origin main`.

- [ ] **12.12** — Monitor CI: schedule a wake-up, then check
      `gh run list --branch main --limit 3` after CI duration. Verify
      the rhino-cli workflows pass. Do not declare done until CI is
      green.

- [ ] **12.13** — Move plan folder to `done/`:
  - `git mv plans/in-progress/rhino-cli-dry-and-enums-refactor plans/done/$(date +%Y-%m-%d)__rhino-cli-dry-and-enums-refactor`
  - Update `plans/in-progress/README.md` (remove entry).
  - Update `plans/done/README.md` (add entry).
  - Commit: `chore(plans): archive rhino-cli-dry-and-enums-refactor to done`.

---

## Quality Gates Summary

> **Important**: Fix ALL failures found during quality gates — not just
> those caused by your changes. Root cause orientation: proactively fix
> preexisting errors encountered during work.

Local gates (run during execution):

- `npx nx run rhino-cli:test:quick` — exits 0 between every phase.
- `npx nx run rhino-cli:typecheck` — exits 0 between every phase.
- `npx nx run rhino-cli:lint` — exits 0 between every phase.
- Phase-end byte-for-byte diffs against Phase 0 captures.

CI gates (post-push):

- `rhino-cli` GitHub Actions workflow — green.
- Pre-push hooks in dependent workspaces (organiclever-be,
  organiclever-web, etc.) — green.

## Verification

Plan is done when all items 12.1 through 12.13 are checked AND CI is
green for the latest pushed commit.
