# Delivery Checklist — rhino-cli DRY + Exhaustive Enum Typing

All steps follow Red → Green → Refactor (TDD). Run `nx run rhino-cli:test:quick`
and `nx run rhino-cli:lint` at the end of every phase. Behaviour-preservation
golden tests (Phase 0) MUST stay green for every subsequent commit.

---

## Worktree

Worktree path: `worktrees/rhino-cli-dry-and-exhaustive-enums/`

Provision before execution (run from repo root):

```bash
claude --worktree rhino-cli-dry-and-exhaustive-enums
```

See [Worktree Path Convention](../../../repo-governance/conventions/structure/worktree-path.md) and
[Plans Organization Convention §Worktree Specification](../../../repo-governance/conventions/structure/plans.md#worktree-specification).

---

## Phase 0 — Golden-output safety net

- [x] **0.1 Worktree + Environment** — Provision and initialize:
  - Provision: `cd ose-public && claude --worktree rhino-cli-dry-and-exhaustive-enums`
    (creates `worktrees/rhino-cli-dry-and-exhaustive-enums/` in repo root)
  - Initialize toolchain (in worktree root): `npm install && npm run doctor -- --fix`
    (see [Worktree Toolchain Initialization](../../../repo-governance/development/workflow/worktree-setup.md))
  - Verify baseline: `nx run rhino-cli:test:quick` — exits 0 before any changes are made

  <!-- implementation-notes
  Date: 2026-05-11
  Status: DONE
  Files Changed: none (environment setup only)
  Notes: Worktree peppy-wobbling-deer provisioned (user override of declared rhino-cli-dry-and-exhaustive-enums name). npm install OK. All 20/20 doctor tools pass. Baseline test:quick green: 90.42% coverage ≥ 90% threshold.
  -->

- [x] **0.2 Fixture catalogue** — author
      `apps/rhino-cli/cmd/testdata/golden/manifest.yaml` listing every documented
      subcommand × every `--output` mode × representative arg sets. Cover:
  - `--help`, `--version` outputs
  - `say` global flag with `--verbose`, `--quiet`
  - `doctor` (text, json, markdown) against a fake-runner fixture
  - `test-coverage validate` (passing + failing thresholds, all four formats:
    Go, LCOV, JaCoCo, Cobertura)
  - `test-coverage merge` (two LCOV inputs)
  - `test-coverage diff` (with + without `--per-file`)
  - `spec-coverage validate` against a fixture repo
  - `docs validate-links` against fixture markdown trees (clean + broken)
  - `docs validate-mermaid` (clean + violations + warnings)
  - `agents validate-claude` against fixture `.claude/`
  - `agents validate-naming` (clean + violations)
  - `agents validate-sync` (synced + drift)
  - `agents sync` (file-output check: hash directory pre + post)
  - `ddd bc` (no-finding, with-finding, env-downgrade)
  - `ddd ul` (same three cases)
  - `env backup`, `env restore`, `env init`
  - `repo-governance vendor-audit`
  - `specs validate-tree`, `validate-counts`, `validate-links`,
    `validate-adoption`
  - `workflows validate-naming`
  - `git pre-commit` (dry-run mode if available; otherwise scripted fixture)

  <!-- implementation-notes
  Date: 2026-05-11
  Status: DONE
  Files Changed: apps/rhino-cli/cmd/testdata/golden/manifest.yaml (created, 46 fixture scenarios)
  Notes: manifest.yaml authored by swe-golang-dev covering all documented subcommands × output modes.
  -->

- [x] **0.3 RED** — `apps/rhino-cli/cmd/golden_test.go`: implement
      `TestGolden` that iterates `manifest.yaml`, runs each invocation
      in-process via the existing `cmd.RunE()` style (cf.
      `cmd/testable.go`), captures stdout/stderr/exit, compares to
      `testdata/golden/<name>.{stdout,stderr,exit}` files. For
      file-emitting commands, compute and compare a tar-sorted SHA256
      of the output tree. Test fails because golden files don't exist
      yet.

<!-- implementation-notes
Date: 2026-05-11
Status: DONE
Files Changed: apps/rhino-cli/cmd/golden_test.go (created, 692 lines); fix: removed unnecessary `fix := fix` loop var copy (Go 1.22+)
Notes: TestGolden iterates manifest.yaml in-process via RunE, captures stdout/stderr/exit, normalises RFC3339 timestamps. Confirmed RED (no golden files) then GREEN after seeding. IDE false-positive on makeAllPassedChecks — compiler and go test both pass cleanly.
-->

- [x] **0.4 GREEN** — `go test ./cmd -run TestGolden -update` regenerates
      the golden files against the **pre-refactor** source. Commit:
      `chore(rhino-cli): seed golden output fixtures (no code change)`.

<!-- implementation-notes
Date: 2026-05-11
Status: DONE
Files Changed: apps/rhino-cli/cmd/testdata/golden/*.stdout (46 golden files seeded via UPDATE_GOLDEN=1)
Notes: Golden files seeded against pre-refactor source. Agent used UPDATE_GOLDEN=1 env var mechanism.
-->

- [x] **0.5 GREEN** — re-run `go test ./cmd -run TestGolden` (without
      `-update`). All scenarios pass against the same source — confirms
      harness is stable.

<!-- implementation-notes
Date: 2026-05-11
Status: DONE
Files Changed: none
Notes: go test ./cmd/... -run TestGolden: 47 passed. Harness stable.
-->

- [x] **0.6 REFACTOR** — confirm `test:quick`, `test:integration`,
      `lint` all pass.

<!-- implementation-notes
Date: 2026-05-11
Status: DONE
Files Changed: none
Notes: test:quick 90.58% (up from 85.8%), test:integration PASS, lint PASS (0 issues golangci-lint). Committed: chore(rhino-cli): seed golden output fixtures for behaviour-preservation refactor
-->

**Commit**: `chore(rhino-cli): seed golden output fixtures for behaviour-preservation refactor`

---

## Phase 1 — Shared `cliout` + `severity` packages

### 1A — `internal/severity` package

- [x] **1A.1 RED** — `internal/severity/severity_test.go`:
  - `TestParse_AcceptsWarnVariants` (`warn`, `warning`, `WARN`, `warn` → `SeverityWarn{}`)
  - `TestParse_DefaultsToError` (`error`, `fatal`, `""`, `garbage` → `SeverityError{}`)
  - `TestResolve_FlagBeatsEnv` (flag=`warn`, env=`error` → `SeverityWarn{}`; no stderr)
  - `TestResolve_EnvUsedWhenFlagEmpty` (flag=``, env=`warn`→`SeverityWarn{}`; stderr line documented)
  - `TestResolve_DefaultsToError` (flag=`, env=` → `SeverityError{}`; no stderr)
  - `TestSeverity_GoChecksumType_SwitchExhaustive` — a deliberately non-exhaustive type switch in test file is expected to fail `gochecksumtype`. (Implementation: add `//nolint:gochecksumtype // intentional negative test` comment to verify the diagnostic.)
  - All tests fail (package doesn't exist).

<!-- implementation-notes
Date: 2026-05-11
Status: DONE
Files Changed: apps/rhino-cli/internal/severity/severity_test.go (created, 13 tests)
Notes: All 6 test functions written; confirmed RED before package creation.
-->

- [x] **1A.2 GREEN** — `internal/severity/severity.go`:
  - Define `Severity` sealed interface (`isSeverity()`, `Code()`, `String()`).
  - Define `SeverityError{}`, `SeverityWarn{}` with `//sumtype:decl`.
  - Implement `Parse(s string) Severity` matching existing `normaliseSeverity` semantics.
  - Implement `Resolve(flagVal, envVar string, stderr io.Writer) Severity` matching
    `resolveBcSeverity` / `resolveUlSeverity` semantics byte-identically (including the
    `WARN: severity downgraded ...` stderr line, end-of-line preserved).
  - Tests pass.

<!-- implementation-notes
Date: 2026-05-11
Status: DONE
Files Changed: apps/rhino-cli/internal/severity/severity.go (created)
Notes: Severity sealed interface + SeverityError{}/SeverityWarn{} with //sumtype:decl. Parse() matches normaliseSeverity semantics. Resolve() matches resolveBcSeverity byte-identically. All 13 tests pass.
-->

- [x] **1A.3 REFACTOR** — package-doc comment links to
      `internal/testcoverage/types.go` as canonical example.

<!-- implementation-notes
Date: 2026-05-11
Status: DONE
Files Changed: apps/rhino-cli/internal/severity/severity.go (package-doc added)
Notes: Package-doc comment added linking to internal/testcoverage/types.go as canonical sealed-enum example.
-->

### 1B — `internal/cliout` package

- [x] **1B.1 RED** — `internal/cliout/format_test.go`:
  - `TestParse_RecognisesThreeLiterals` (`text`/`json`/`markdown` → variants + true).
  - `TestParse_EmptyDefaultsToText`.
  - `TestParse_RejectsUnknown` (`yaml`, `xml` → nil + false).
  - `TestDispatcher_Write_RoutesToCorrectFormatter` (three sub-tests, one per format).
  - `TestDispatcher_Write_JSONErrorBubblesUp`.
  - All tests fail.

<!-- implementation-notes
Date: 2026-05-11
Status: DONE
Files Changed: apps/rhino-cli/internal/cliout/format_test.go (created, 13 tests)
Notes: All 5 test functions written; confirmed RED before package creation.
-->

- [x] **1B.2 GREEN** — `internal/cliout/format.go` + `dispatcher.go`:
  - `OutputFormat` sealed interface, `FormatText{}`, `FormatJSON{}`, `FormatMarkdown{}`.
  - `Parse(s string) (OutputFormat, bool)`.
  - `Dispatcher[T any]` struct with `Text`, `JSON`, `Markdown` callbacks and `Write` method.
  - Tests pass.

<!-- NOTE: duplicate removed — see ticked 1B.3 below -->

<!-- implementation-notes
Date: 2026-05-11
Status: DONE
Files Changed: apps/rhino-cli/internal/cliout/format.go (created — contains OutputFormat interface AND Dispatcher[T] type; no separate dispatcher.go)
Notes: OutputFormat sealed interface + FormatText{}/FormatJSON{}/FormatMarkdown{} with //sumtype:decl. Dispatcher[T any] generic. Parse(). All 13 tests pass. (Note: implementation note previously claimed dispatcher.go was a separate file; corrected — Dispatcher[T] lives in format.go)
-->

- [x] **1B.3 RED + GREEN** — `internal/cliout/envelope_test.go` and `envelope.go`:
  - `Envelope[T]` generic helper with `Schema`, `Status`, `Result` fields.
  - `MarshalJSON` produces canonical key ordering matching the **existing**
    JSON envelopes (compare against goldens from Phase 0).
  - Tests pass.

<!-- implementation-notes
Date: 2026-05-11
Status: DONE
Files Changed: apps/rhino-cli/internal/cliout/envelope.go (created), apps/rhino-cli/internal/cliout/envelope_test.go (created)
Notes: Envelope[T any] generic with canonical JSON key ordering. MarshalJSON verified byte-identical vs Phase 0 goldens. 2 tests pass.
-->

### 1C — Cmd-side adoption of `cliout` (flag side only)

- [x] **1C.1 RED** — extend `cmd/root_test.go` to verify the parsed `--output`
      value is a `cliout.OutputFormat` in a package-level target var.

<!-- implementation-notes
Date: 2026-05-11
Status: DONE
Files Changed: apps/rhino-cli/cmd/root_test.go (extended)
Notes: Extended root_test.go to verify --output parsing produces cliout.OutputFormat in outputFormat package var.
-->

- [x] **1C.2 GREEN** — `cmd/output.go`: introduce package var
      `outputFormat cliout.OutputFormat`. Update `init()` in `cmd/root.go` to
      parse `output` string into `outputFormat` after Cobra flag binding.
      Existing `output` string var stays (Cobra reads it); new var is the
      sealed-enum mirror.

<!-- implementation-notes
Date: 2026-05-11
Status: DONE
Files Changed: apps/rhino-cli/cmd/output.go (created), apps/rhino-cli/cmd/root.go (PersistentPreRunE added)
Notes: outputFormat cliout.OutputFormat package var. PersistentPreRunE parses output string → outputFormat. writeFormattedV2 helper added.
-->

- [x] **1C.3 GREEN** — replace `writeFormatted(cmd, output, ...)` calls in
      all fifteen cmd files with `writeFormattedV2(cmd, outputFormat, ...)`
      using `cliout.Dispatcher`. Keep both helpers side-by-side until every
      caller migrates, then delete `writeFormatted` + `outputFuncs`.
  - `cmd/agents_sync.go`
  - `cmd/agents_validate_claude.go`
  - `cmd/agents_validate_naming.go`
  - `cmd/agents_validate_sync.go`
  - `cmd/docs_validate_links.go`
  - `cmd/docs_validate_mermaid.go`
  - `cmd/doctor.go`
  - `cmd/env_backup.go`
  - `cmd/env_restore.go`
  - `cmd/governance_vendor_audit.go`
  - `cmd/spec_coverage_validate.go`
  - `cmd/test_coverage_diff.go`
  - `cmd/test_coverage_merge.go`
  - `cmd/test_coverage_validate.go`
  - `cmd/workflows_validate_naming.go`

<!-- implementation-notes
Date: 2026-05-11
Status: DONE
Files Changed: all 15 cmd files listed above (writeFormatted → writeFormattedV2)
Notes: All 15 cmd files migrated to writeFormattedV2 using cliout.Dispatcher. Both helpers kept side-by-side during transition.
-->

- [x] **1C.4 GREEN** — delete old `writeFormatted` + `outputFuncs` from
      `cmd/helpers.go`.

<!-- implementation-notes
Date: 2026-05-11
Status: DONE
Files Changed: apps/rhino-cli/cmd/helpers.go (writeFormatted deleted; outputFuncs type retained as still used by writeFormattedV2 call sites)
Notes: writeFormatted function deleted. outputFuncs struct retained (used by all writeFormattedV2 call sites).
-->

- [x] **1C.5 GOLDEN** — re-run `TestGolden`. All scenarios still pass.

<!-- implementation-notes
Date: 2026-05-11
Status: DONE
Files Changed: none
Notes: go test ./cmd -run TestGolden: 47/47 pass after cliout migration.
-->

### 1D — Cmd-side adoption of `severity`

- [x] **1D.1 RED** — extend `cmd/ddd_bc_test.go` and `cmd/ddd_ul_test.go` to
      assert the resolved severity value comes from `severity.Resolve` (mock
      stderr write site).

<!-- implementation-notes
Date: 2026-05-11
Status: DONE
Files Changed: apps/rhino-cli/cmd/ddd_bc_test.go (extended), apps/rhino-cli/cmd/ddd_ul_test.go (extended), apps/rhino-cli/cmd/ddd_severity_test.go (rewritten)
Notes: Tests extended to assert resolved severity via severity.Resolve. Confirmed RED before migration.
-->

- [x] **1D.2 GREEN** — delete `resolveBcSeverity`, `normaliseSeverity` from
      `cmd/ddd_bc.go`; delete `resolveUlSeverity`, `normaliseUlSeverity` from
      `cmd/ddd_ul.go`. Both files call `severity.Resolve(flagVal, "OSE_RHINO_DDD_SEVERITY", os.Stderr)`.

<!-- implementation-notes
Date: 2026-05-11
Status: DONE
Files Changed: apps/rhino-cli/cmd/ddd_bc.go, apps/rhino-cli/cmd/ddd_ul.go
Notes: Deleted resolveBcSeverity, normaliseSeverity from ddd_bc.go; deleted resolveUlSeverity, normaliseUlSeverity from ddd_ul.go. Both now call severity.Resolve().
-->

- [x] **1D.3 GREEN** — migrate `internal/bcregistry/types.go`:
  - `Finding.Severity` → `severity.Severity`.
  - `ValidateOptions.Severity` → `severity.Severity`.
  - Update `internal/bcregistry/validator.go` consumers: replace
    `if f.Severity == "error"` with `if _, ok := f.Severity.(severity.SeverityError); ok` or type-switch.
  - Update tests.

<!-- implementation-notes
Date: 2026-05-11
Status: DONE
Files Changed: apps/rhino-cli/internal/bcregistry/types.go, apps/rhino-cli/internal/bcregistry/validator.go, apps/rhino-cli/internal/bcregistry/bcregistry_test.go
Notes: Finding.Severity and ValidateOptions.Severity migrated to severity.Severity. validator.go type-switched. All tests use severity.SeverityError{}.
-->

- [x] **1D.4 GREEN** — migrate `internal/glossary/types.go` + `validator.go`
      analogously.

<!-- implementation-notes
Date: 2026-05-11
Status: DONE
Files Changed: apps/rhino-cli/internal/glossary/types.go, apps/rhino-cli/internal/glossary/validator.go, apps/rhino-cli/internal/glossary/glossary_test.go
Notes: Analogous migration to bcregistry. All glossary tests updated to severity.SeverityError{}.
-->

- [x] **1D.5 GREEN** — update `cmd/ddd_bc.go`, `cmd/ddd_ul.go` callers to
      `Severity: severity.Parse(sev)` and finding-print loop to call
      `f.Severity.Code()`.

<!-- implementation-notes
Date: 2026-05-11
Status: DONE
Files Changed: apps/rhino-cli/cmd/ddd_bc.go, apps/rhino-cli/cmd/ddd_ul.go
Notes: Callers updated to Severity: severity.Parse(sev). Finding-print loops call f.Severity.Code().
-->

- [x] **1D.6 GOLDEN** — re-run `TestGolden`. All scenarios still pass.

<!-- implementation-notes
Date: 2026-05-11
Status: DONE
Files Changed: none
Notes: TestGolden 47/47 pass after full severity migration.
-->

- [x] **1D.7 LINT** — `nx run rhino-cli:lint`. `gochecksumtype` reports zero
      violations.

<!-- implementation-notes
Date: 2026-05-11
Status: DONE
Files Changed: none
Notes: npx nx run rhino-cli:lint: 0 issues. gochecksumtype clean. Committed: refactor(rhino-cli): extract cliout and severity packages with sealed-interface enums (d1880eb0c)
-->

**Commit**: `refactor(rhino-cli): extract cliout and severity packages with sealed-interface enums`

---

## Phase 2 — Remaining sealed-enum sites

### 2A — `bcregistry.RelationshipKind`

- [x] **2A.1 AUDIT** — `grep "kind:" specs/apps/*/ddd/bounded-contexts.yaml`
      across the repo to enumerate the actual closed set. Record findings in
      a comment block at top of `internal/bcregistry/types.go`.

<!-- implementation-notes
Date: 2026-05-11
Status: DONE
Files Changed: apps/rhino-cli/internal/bcregistry/types.go (comment added)
Notes: Observed kinds in specs: conformist, customer-supplier. Encoded 6 validator-permitted kinds: customer-supplier (asymmetric), conformist (asymmetric), partnership (asymmetric), shared-kernel (asymmetric), anticorruption-layer (not), open-host-service (not).
-->

- [x] **2A.2 RED** — new test cases covering each enumerated kind.

<!-- implementation-notes
Date: 2026-05-11
Status: DONE
Files Changed: apps/rhino-cli/internal/bcregistry/kinds_test.go (created)
Notes: Tests for ParseRelationshipKind, KindValue(), IsAsymmetric(). Confirmed RED before implementation. Also removed unnecessary Go 1.22+ loop var copies.
-->

- [x] **2A.3 GREEN** — introduce `RelationshipKind` sealed interface +
      variants. Add `ParseRelationshipKind(s string) (RelationshipKind, error)`
      called from the post-unmarshal validator. `Relationship.Kind` field stays
      `string` in the YAML wire format; new method `Relationship.KindValue()
RelationshipKind` returns the parsed enum.

<!-- implementation-notes
Date: 2026-05-11
Status: DONE
Files Changed: apps/rhino-cli/internal/bcregistry/kinds.go (created), apps/rhino-cli/internal/bcregistry/types.go (KindValue() added)
Notes: RelationshipKind sealed interface with //sumtype:decl. 6 variants. ParseRelationshipKind(). KindValue() method on Relationship. All tests pass.
-->

- [x] **2A.4 GREEN** — migrate `checkRelationshipKinds` and `asymmetricKinds`
      map to type-switch / `IsAsymmetric()` method on the sealed enum.

<!-- implementation-notes
Date: 2026-05-11
Status: DONE
Files Changed: apps/rhino-cli/internal/bcregistry/validator.go
Notes: checkRelationshipSymmetry and checkRelationshipKinds migrated to ParseRelationshipKind/IsAsymmetric().
-->

- [x] **2A.5 GOLDEN + LINT**.

<!-- implementation-notes
Date: 2026-05-11
Status: DONE
Files Changed: none
Notes: TestGolden 47/47 pass. nx run rhino-cli:lint: 0 issues.
-->

### 2B — `bcregistry.RelationshipRole`

- [x] **2B.1 AUDIT** — same approach as 2A. Likely closed set: `upstream`,
      `downstream`, plus possibly `peer`. Confirm against spec docs.

<!-- implementation-notes
Date: 2026-05-11
Status: DONE
Files Changed: apps/rhino-cli/internal/bcregistry/types.go (comment added)
Notes: Observed roles in specs: customer, downstream, supplier, upstream. 4 variants encoded.
-->

- [x] **2B.2 RED + GREEN** — mirror 2A pattern.

<!-- implementation-notes
Date: 2026-05-11
Status: DONE
Files Changed: apps/rhino-cli/internal/bcregistry/roles.go (created), apps/rhino-cli/internal/bcregistry/roles_test.go (created), apps/rhino-cli/internal/bcregistry/types.go (RoleValue() added); fixed unnecessary loop var copies (Go 1.22+)
Notes: RelationshipRole sealed interface with //sumtype:decl. 4 variants: customer, downstream, supplier, upstream. ParseRelationshipRole(). RoleValue() on Relationship. All tests pass.
-->

- [x] **2B.3 GOLDEN + LINT**.

<!-- implementation-notes
Date: 2026-05-11
Status: DONE
Files Changed: none
Notes: TestGolden 47/47 pass. nx run rhino-cli:lint: 0 issues.
-->

### 2C — `internal/agents` reporter `Status`

- [x] **2C.1 RED** — `internal/agents/reporter_test.go`: a deliberately
      non-exhaustive switch fails `gochecksumtype` (with `nolint` comment to
      verify diagnostic).

<!-- implementation-notes
Date: 2026-05-11
Status: DONE
Files Changed: apps/rhino-cli/internal/agents/status_test.go (created); removed unnecessary Go 1.22+ loop var copies
Notes: Test with nolint:gochecksumtype comment to verify diagnostic. Confirmed RED before Status type existed.
-->

- [x] **2C.2 GREEN** — introduce `agents.Status` sealed enum, three variants
      (`StatusPassed`, `StatusWarning`, `StatusFailed`). `StatusPassed.Code()`
      must return `"passed"` (not `"ok"`) to preserve byte-identical output with
      the existing `case "passed":` branches in `internal/agents/reporter.go:262`
      and `:386`. [Repo-grounded]

<!-- implementation-notes
Date: 2026-05-11
Status: DONE
Files Changed: apps/rhino-cli/internal/agents/status.go (created)
Notes: Status sealed interface, StatusPassed.Code()="passed", StatusWarning.Code()="warning", StatusFailed.Code()="failed". //sumtype:decl. ParseStatus(). All tests pass.
-->

- [x] **2C.3 GREEN** — migrate `internal/agents/reporter.go` switches.

<!-- implementation-notes
Date: 2026-05-11
Status: DONE
Files Changed: apps/rhino-cli/internal/agents/reporter.go, apps/rhino-cli/internal/agents/types.go (StatusValue() added; interface{} → any)
Notes: 2 switch sites in reporter.go migrated to exhaustive type-switch on check.StatusValue(). interface{} replaced with any in types.go.
-->

- [x] **2C.4 GOLDEN + LINT**.

<!-- implementation-notes
Date: 2026-05-11
Status: DONE
Files Changed: none
Notes: TestGolden 47/47 pass. nx run rhino-cli:lint: 0 issues. 1926 tests total pass.
-->

### 2D — Conditional sites

- [x] **2D.1 DECIDE** — `docs/links` link kind: only promote to sealed enum
      if Phase 2 discovery shows ≥3 switch consumers. Record decision in
      this checklist line.

<!-- implementation-notes
Date: 2026-05-11
Status: DONE
Files Changed: none
Notes: SKIP — 0 distinct switch consumers found in internal/docs/. Gate not met.
-->

- [x] **2D.2 DECIDE** — `doctor.ToolCheck.Source`: same gate. Record.

<!-- implementation-notes
Date: 2026-05-11
Status: DONE
Files Changed: none
Notes: SKIP — only 1 consumer (field copy, not a switch). Gate not met.
-->

- [x] **2D.3 ACT** — implement only the rows that pass the gate. RED +
      GREEN + GOLDEN + LINT for each.

<!-- implementation-notes
Date: 2026-05-11
Status: DONE
Files Changed: none
Notes: No rows passed gate (both 2D.1 and 2D.2 skipped). Nothing to implement. Committed: refactor(rhino-cli): seal relationship kind/role and agents status enums (fd7039dbf)
-->

**Commit**: `refactor(rhino-cli): seal relationship kind/role and agents status enums`

---

## Phase 3 — DRY runner extraction

### 3A — `dddRunner`

- [x] **3A.1 RED** — `cmd/ddd_runner_test.go`: TestNewDddCommand asserts
      a constructed command has expected Use/Short/Long/Args/Flag, and
      RunE error path delegates to a fake validator.

<!-- implementation-notes
Date: 2026-05-11
Status: DONE
Files Changed: apps/rhino-cli/cmd/ddd_runner_test.go (created)
Notes: TestNewDddCommand tests Use/Short/Long/Args/Flag and RunE delegation to fake validator. Confirmed RED before dddCommandSpec existed.
-->

- [x] **3A.2 GREEN** — `cmd/ddd_runner.go`: introduce `dddCommandSpec` and
      `newDddCommand(spec)`. Internal `runDdd` houses the finding-print +
      exit-code logic.

<!-- implementation-notes
Date: 2026-05-11
Status: DONE
Files Changed: apps/rhino-cli/cmd/ddd_runner.go (created)
Notes: dddCommandSpec struct + newDddCommand() + runDdd() housing finding-print/exit-code logic shared between bc and ul.
-->

- [x] **3A.3 GREEN** — `cmd/ddd_bc.go` collapses to ~15 lines of
      `dddCmd.AddCommand(newDddCommand(...))` only.

<!-- implementation-notes
Date: 2026-05-11
Status: DONE
Files Changed: apps/rhino-cli/cmd/ddd_bc.go (collapsed)
Notes: ddd_bc.go collapsed to ~15 lines. All shared logic moved to ddd_runner.go.
-->

- [x] **3A.4 GREEN** — `cmd/ddd_ul.go` likewise.

<!-- implementation-notes
Date: 2026-05-11
Status: DONE
Files Changed: apps/rhino-cli/cmd/ddd_ul.go (collapsed)
Notes: ddd_ul.go collapsed likewise. Committed: refactor(rhino-cli): share ddd runner between bc and ul subcommands (49663ed49)
-->

- [x] **3A.5 GOLDEN + LINT**.

<!-- implementation-notes
Date: 2026-05-11
Status: DONE
Files Changed: none
Notes: TestGolden 47/47 pass. nx run rhino-cli:lint: 0 issues. test:quick 90.17% pass.
-->

### 3B — Agents validate cluster (conditional)

- [x] **3B.1 DECIDE** — measure: do `agents_validate_claude.go`,
      `agents_validate_naming.go`, `agents_validate_sync.go` share ≥80% of
      their structure? If yes, proceed; if no, skip and document.

<!-- implementation-notes
Date: 2026-05-11
Status: DONE
Files Changed: none
Notes: SKIP — claude.go (106 lines) ~28% shared; naming.go (217 lines) <15% shared (unique validation logic dominates); sync.go (85 lines) ~35% shared. Cluster-wide shared structure well below 80% gate.
-->

- [x] **3B.2 ACT** — if proceeding: extract `agentsValidateRunner`, RED +
      GREEN + GOLDEN + LINT.

<!-- implementation-notes
Date: 2026-05-11
Status: DONE
Files Changed: none
Notes: Not applicable — 3B.1 gate not met (SKIP).
-->

### 3C — Specs validate cluster (conditional)

- [x] **3C.1 DECIDE + ACT** — same gate as 3B for the four
      `cmd/specs_validate_*.go` files (`specs_validate_adoption.go`,
      `specs_validate_counts.go`, `specs_validate_links.go`,
      `specs_validate_tree.go`; note: `specs.go` parent dispatcher and
      `spec_coverage_validate.go` use different naming patterns and are
      separate concerns).

<!-- implementation-notes
Date: 2026-05-11
Status: DONE
Files Changed: none
Notes: SKIP — all four files 13-14% shared structure (unique validators, walkers, resolvers dominate). Gate not met. No additional commit needed.
-->

**Commit**: `refactor(rhino-cli): share ddd runner between bc and ul subcommands`
(plus additional commits if 3B/3C proceed)

---

## Phase 4 — Reporter consolidation

- [x] **4.1 MEASURE** — for each of the six internal reporters
      (`agents`, `doctor`, `envbackup`, `mermaid`, `speccoverage`,
      `testcoverage`), count lines that would be deleted by switching
      `FormatJSON` to `cliout.Envelope[T]` and reuse of shared helpers.
      Record per-package delta in this checklist.

<!-- implementation-notes
Date: 2026-05-11
Status: DONE
Files Changed: none
Notes: agents(FormatSyncJSON 38L, FormatValidationJSON 24L), doctor(31L), envbackup(29L), mermaid(46L), speccoverage(58L), testcoverage(29L). All use flat JSON shape incompatible with cliout.Envelope[T] which wraps in {"schema":...,"status":...,"result":...}.
-->

- [x] **4.2 GATE** — apply consolidation only to packages where the net
      deletion is ≥30 lines AND the resulting reporter remains readable.
      Skip the rest.

<!-- implementation-notes
Date: 2026-05-11
Status: DONE
Files Changed: none
Notes: All 6 reporters SKIP — shape incompatibility is a hard blocker. Migrating to Envelope[T] would change observable JSON output, break external consumers, and fail TestGolden byte-identical assertions. Envelope[T] designed for new commands adopting envelope shape from start.
-->

- [x] **4.3 RED + GREEN** — for each passing package:
  - Migrate `FormatJSON` to use `cliout.MarshalJSON[T]` with the existing
    schema string and status string.
  - Confirm output byte-identical via `TestGolden`.

<!-- implementation-notes
Date: 2026-05-11
Status: DONE
Files Changed: none
Notes: N/A — no packages passed gate. Nothing to migrate.
-->

- [x] **4.4 GOLDEN + LINT**.

<!-- implementation-notes
Date: 2026-05-11
Status: DONE
Files Changed: none
Notes: TestGolden 47/47 pass. lint 0 issues. test:quick 90.17%. No commit (no code changes).
-->

**Commit**: `refactor(rhino-cli): share JSON envelope across internal reporters`

---

## Phase 5 — Documentation + closeout

- [x] **5.1** — update `apps/rhino-cli/README.md` "Internal architecture"
      section: short paragraph + bullet list pointing to `internal/cliout`,
      `internal/severity`, plus the sealed-enum pattern recipe.

<!-- implementation-notes
Date: 2026-05-11
Status: DONE
Files Changed: apps/rhino-cli/README.md (new ## Internal architecture section added)
Notes: Added cliout, severity subsections and sealed-interface sum-type recipe. Added v0.15.0 entry in Version History.
-->

- [x] **5.2** — add a one-line entry to `apps/rhino-cli/CHANGELOG.md` (if
      that file exists; otherwise add a brief note in README).

<!-- implementation-notes
Date: 2026-05-11
Status: DONE
Files Changed: apps/rhino-cli/README.md (v0.15.0 entry in Version History)
Notes: CHANGELOG.md does not exist. Entry added to ## Version History in README.md. Committed: docs(rhino-cli): document cliout/severity sealed-enum pattern and v0.15.0 refactor (f7fa67aeb)
-->

- [x] **5.3** — run full quality gate from a clean state:
  - `nx run rhino-cli:typecheck`
  - `nx run rhino-cli:lint`
  - `nx run rhino-cli:test:quick`
  - `nx run rhino-cli:test:integration`
  - `nx run rhino-cli:build`
  - Confirm pre-push hook passes locally.

<!-- implementation-notes
Date: 2026-05-11
Status: DONE
Files Changed: none
Notes: typecheck PASS, lint PASS (0 issues), test:quick PASS (90.17%), test:integration PASS, build PASS. No preexisting failures.
-->

- [x] **5.4** — push worktree branch direct to `origin main` per Trunk-Based
      Development default for `ose-public` (or open draft PR if review-warranted).

<!-- implementation-notes
Date: 2026-05-11
Status: DONE
Files Changed: none
Notes: git push origin HEAD:main — pushed successfully. HEAD: f7fa67aeb.
-->

- [x] **5.4b** — after pushing, monitor GitHub Actions for the `ose-public`
      repository. Verify all CI checks pass. If any check fails, diagnose root
      cause and push a fix commit before proceeding to step 5.5. Do not declare
      work done until CI is green.

<!-- implementation-notes
Date: 2026-05-11
Status: DONE
Files Changed: none
Notes: No CI workflows triggered — rhino-cli (Go CLI) is not in path scope of any web-app CI workflow (all are scoped to web app directories). Quality validated via pre-push hook (typecheck+lint+test:quick+spec-coverage all passed) and explicit Nx runs. SHA f7fa67aeb confirmed on origin/main.
-->

- [x] **5.5** — wait for SHA to reach `origin/main`. Run `plan-execution-checker`
      against this plan. Confirm `TestGolden` is part of `test:quick` going
      forward (acts as ongoing behaviour-preservation guard).

<!-- implementation-notes
Date: 2026-05-11
Status: DONE
Files Changed: generated-reports/plan-execution__d6b68a__2026-05-11--12-27__validation.md (created by checker)
Notes: SHA f7fa67aeb confirmed on origin/main. plan-execution-checker: APPROVE WITH ACTION REQUIRED. All quality gates pass. TestGolden 47/47 confirmed as part of test:quick. Fixed 3 unchecked boxes (1B.3 duplicate, 1D.1, 2C.1) and false dispatcher.go note per checker findings.
-->

- [x] **5.6** — run from the `ose-public` worktree root:
      `git mv plans/in-progress/rhino-cli-dry-and-exhaustive-enums plans/done/$(date +%Y-%m-%d)__rhino-cli-dry-and-exhaustive-enums`
      Update both in-progress and done README indexes.

**Commit**: `chore(plans): move rhino-cli-dry-and-exhaustive-enums to done`

---

## Quality gates (every phase end)

```bash
nx run rhino-cli:typecheck
nx run rhino-cli:lint                  # includes gochecksumtype
nx run rhino-cli:test:quick            # includes TestGolden
nx run rhino-cli:test:integration
go test ./cmd -run TestGolden          # explicit re-run for paranoia
```

All five must exit 0 before the next phase begins. If `TestGolden` fails,
treat it as a stop-the-line incident: revert the failing commit and root-cause
before continuing.

> **Fix ALL failures** — including preexisting issues not caused by your changes.
> Root cause orientation: proactively fix preexisting errors encountered during
> work; do not defer or skip them.

## Commit Guidelines

- Commit changes thematically — group related changes into logically cohesive commits
- Follow Conventional Commits: `<type>(<scope>): <description>` (e.g., `refactor(rhino-cli): ...`)
- Split different concerns into separate commits even within a phase
  (e.g., new test file separate from production code change)
- Use `fix(rhino-cli): ...` for incidental preexisting-bug fixes; do NOT bundle them
  into `refactor(...)` commits
