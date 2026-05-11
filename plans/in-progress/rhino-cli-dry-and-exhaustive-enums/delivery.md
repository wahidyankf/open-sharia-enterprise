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

See [Worktree Path Convention](../../../governance/conventions/structure/worktree-path.md) and
[Plans Organization Convention §Worktree Specification](../../../governance/conventions/structure/plans.md#worktree-specification).

---

## Phase 0 — Golden-output safety net

- [ ] **0.1 Worktree + Environment** — Provision and initialize:
  - Provision: `cd ose-public && claude --worktree rhino-cli-dry-and-exhaustive-enums`
    (creates `worktrees/rhino-cli-dry-and-exhaustive-enums/` in repo root)
  - Initialize toolchain (in worktree root): `npm install && npm run doctor -- --fix`
    (see [Worktree Toolchain Initialization](../../../governance/development/workflow/worktree-setup.md))
  - Verify baseline: `nx run rhino-cli:test:quick` — exits 0 before any changes are made

- [ ] **0.2 Fixture catalogue** — author
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
  - `governance vendor-audit`
  - `specs validate-tree`, `validate-counts`, `validate-links`,
    `validate-adoption`
  - `workflows validate-naming`
  - `git pre-commit` (dry-run mode if available; otherwise scripted fixture)

- [ ] **0.3 RED** — `apps/rhino-cli/cmd/golden_test.go`: implement
      `TestGolden` that iterates `manifest.yaml`, runs each invocation
      in-process via the existing `cmd.RunE()` style (cf.
      `cmd/testable.go`), captures stdout/stderr/exit, compares to
      `testdata/golden/<name>.{stdout,stderr,exit}` files. For
      file-emitting commands, compute and compare a tar-sorted SHA256
      of the output tree. Test fails because golden files don't exist
      yet.

- [ ] **0.4 GREEN** — `go test ./cmd -run TestGolden -update` regenerates
      the golden files against the **pre-refactor** source. Commit:
      `chore(rhino-cli): seed golden output fixtures (no code change)`.

- [ ] **0.5 GREEN** — re-run `go test ./cmd -run TestGolden` (without
      `-update`). All scenarios pass against the same source — confirms
      harness is stable.

- [ ] **0.6 REFACTOR** — confirm `test:quick`, `test:integration`,
      `lint` all pass.

**Commit**: `chore(rhino-cli): seed golden output fixtures for behaviour-preservation refactor`

---

## Phase 1 — Shared `cliout` + `severity` packages

### 1A — `internal/severity` package

- [ ] **1A.1 RED** — `internal/severity/severity_test.go`:
  - `TestParse_AcceptsWarnVariants` (`warn`, `warning`, `WARN`, `warn` → `SeverityWarn{}`)
  - `TestParse_DefaultsToError` (`error`, `fatal`, `""`, `garbage` → `SeverityError{}`)
  - `TestResolve_FlagBeatsEnv` (flag=`warn`, env=`error` → `SeverityWarn{}`; no stderr)
  - `TestResolve_EnvUsedWhenFlagEmpty` (flag=``, env=`warn`→`SeverityWarn{}`; stderr line documented)
  - `TestResolve_DefaultsToError` (flag=`, env=` → `SeverityError{}`; no stderr)
  - `TestSeverity_GoChecksumType_SwitchExhaustive` — a deliberately non-exhaustive type switch in test file is expected to fail `gochecksumtype`. (Implementation: add `//nolint:gochecksumtype // intentional negative test` comment to verify the diagnostic.)
  - All tests fail (package doesn't exist).

- [ ] **1A.2 GREEN** — `internal/severity/severity.go`:
  - Define `Severity` sealed interface (`isSeverity()`, `Code()`, `String()`).
  - Define `SeverityError{}`, `SeverityWarn{}` with `//sumtype:decl`.
  - Implement `Parse(s string) Severity` matching existing `normaliseSeverity` semantics.
  - Implement `Resolve(flagVal, envVar string, stderr io.Writer) Severity` matching
    `resolveBcSeverity` / `resolveUlSeverity` semantics byte-identically (including the
    `WARN: severity downgraded ...` stderr line, end-of-line preserved).
  - Tests pass.

- [ ] **1A.3 REFACTOR** — package-doc comment links to
      `internal/testcoverage/types.go` as canonical example.

### 1B — `internal/cliout` package

- [ ] **1B.1 RED** — `internal/cliout/format_test.go`:
  - `TestParse_RecognisesThreeLiterals` (`text`/`json`/`markdown` → variants + true).
  - `TestParse_EmptyDefaultsToText`.
  - `TestParse_RejectsUnknown` (`yaml`, `xml` → nil + false).
  - `TestDispatcher_Write_RoutesToCorrectFormatter` (three sub-tests, one per format).
  - `TestDispatcher_Write_JSONErrorBubblesUp`.
  - All tests fail.

- [ ] **1B.2 GREEN** — `internal/cliout/format.go` + `dispatcher.go`:
  - `OutputFormat` sealed interface, `FormatText{}`, `FormatJSON{}`, `FormatMarkdown{}`.
  - `Parse(s string) (OutputFormat, bool)`.
  - `Dispatcher[T any]` struct with `Text`, `JSON`, `Markdown` callbacks and `Write` method.
  - Tests pass.

- [ ] **1B.3 RED + GREEN** — `internal/cliout/envelope_test.go` and `envelope.go`:
  - `Envelope[T]` generic helper with `Schema`, `Status`, `Result` fields.
  - `MarshalJSON` produces canonical key ordering matching the **existing**
    JSON envelopes (compare against goldens from Phase 0).
  - Tests pass.

### 1C — Cmd-side adoption of `cliout` (flag side only)

- [ ] **1C.1 RED** — extend `cmd/root_test.go` to verify the parsed `--output`
      value is a `cliout.OutputFormat` in a package-level target var.

- [ ] **1C.2 GREEN** — `cmd/output.go`: introduce package var
      `outputFormat cliout.OutputFormat`. Update `init()` in `cmd/root.go` to
      parse `output` string into `outputFormat` after Cobra flag binding.
      Existing `output` string var stays (Cobra reads it); new var is the
      sealed-enum mirror.

- [ ] **1C.3 GREEN** — replace `writeFormatted(cmd, output, ...)` calls in
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

- [ ] **1C.4 GREEN** — delete old `writeFormatted` + `outputFuncs` from
      `cmd/helpers.go`.

- [ ] **1C.5 GOLDEN** — re-run `TestGolden`. All scenarios still pass.

### 1D — Cmd-side adoption of `severity`

- [ ] **1D.1 RED** — extend `cmd/ddd_bc_test.go` and `cmd/ddd_ul_test.go` to
      assert the resolved severity value comes from `severity.Resolve` (mock
      stderr write site).

- [ ] **1D.2 GREEN** — delete `resolveBcSeverity`, `normaliseSeverity` from
      `cmd/ddd_bc.go`; delete `resolveUlSeverity`, `normaliseUlSeverity` from
      `cmd/ddd_ul.go`. Both files call `severity.Resolve(flagVal, "OSE_RHINO_DDD_SEVERITY", os.Stderr)`.

- [ ] **1D.3 GREEN** — migrate `internal/bcregistry/types.go`:
  - `Finding.Severity` → `severity.Severity`.
  - `ValidateOptions.Severity` → `severity.Severity`.
  - Update `internal/bcregistry/validator.go` consumers: replace
    `if f.Severity == "error"` with `if _, ok := f.Severity.(severity.SeverityError); ok` or type-switch.
  - Update tests.

- [ ] **1D.4 GREEN** — migrate `internal/glossary/types.go` + `validator.go`
      analogously.

- [ ] **1D.5 GREEN** — update `cmd/ddd_bc.go`, `cmd/ddd_ul.go` callers to
      `Severity: severity.Parse(sev)` and finding-print loop to call
      `f.Severity.Code()`.

- [ ] **1D.6 GOLDEN** — re-run `TestGolden`. All scenarios still pass.

- [ ] **1D.7 LINT** — `nx run rhino-cli:lint`. `gochecksumtype` reports zero
      violations.

**Commit**: `refactor(rhino-cli): extract cliout and severity packages with sealed-interface enums`

---

## Phase 2 — Remaining sealed-enum sites

### 2A — `bcregistry.RelationshipKind`

- [ ] **2A.1 AUDIT** — `grep "kind:" specs/apps/*/ddd/bounded-contexts.yaml`
      across the repo to enumerate the actual closed set. Record findings in
      a comment block at top of `internal/bcregistry/types.go`.

- [ ] **2A.2 RED** — new test cases covering each enumerated kind.

- [ ] **2A.3 GREEN** — introduce `RelationshipKind` sealed interface +
      variants. Add `ParseRelationshipKind(s string) (RelationshipKind, error)`
      called from the post-unmarshal validator. `Relationship.Kind` field stays
      `string` in the YAML wire format; new method `Relationship.KindValue()
RelationshipKind` returns the parsed enum.

- [ ] **2A.4 GREEN** — migrate `checkRelationshipKinds` and `asymmetricKinds`
      map to type-switch / `IsAsymmetric()` method on the sealed enum.

- [ ] **2A.5 GOLDEN + LINT**.

### 2B — `bcregistry.RelationshipRole`

- [ ] **2B.1 AUDIT** — same approach as 2A. Likely closed set: `upstream`,
      `downstream`, plus possibly `peer`. Confirm against spec docs.

- [ ] **2B.2 RED + GREEN** — mirror 2A pattern.

- [ ] **2B.3 GOLDEN + LINT**.

### 2C — `internal/agents` reporter `Status`

- [ ] **2C.1 RED** — `internal/agents/reporter_test.go`: a deliberately
      non-exhaustive switch fails `gochecksumtype` (with `nolint` comment to
      verify diagnostic).

- [ ] **2C.2 GREEN** — introduce `agents.Status` sealed enum, three variants
      (`StatusPassed`, `StatusWarning`, `StatusFailed`). `StatusPassed.Code()`
      must return `"passed"` (not `"ok"`) to preserve byte-identical output with
      the existing `case "passed":` branches in `internal/agents/reporter.go:262`
      and `:386`. [Repo-grounded]

- [ ] **2C.3 GREEN** — migrate `internal/agents/reporter.go` switches.

- [ ] **2C.4 GOLDEN + LINT**.

### 2D — Conditional sites

- [ ] **2D.1 DECIDE** — `docs/links` link kind: only promote to sealed enum
      if Phase 2 discovery shows ≥3 switch consumers. Record decision in
      this checklist line.

- [ ] **2D.2 DECIDE** — `doctor.ToolCheck.Source`: same gate. Record.

- [ ] **2D.3 ACT** — implement only the rows that pass the gate. RED +
      GREEN + GOLDEN + LINT for each.

**Commit**: `refactor(rhino-cli): seal relationship kind/role and agents status enums`

---

## Phase 3 — DRY runner extraction

### 3A — `dddRunner`

- [ ] **3A.1 RED** — `cmd/ddd_runner_test.go`: TestNewDddCommand asserts
      a constructed command has expected Use/Short/Long/Args/Flag, and
      RunE error path delegates to a fake validator.

- [ ] **3A.2 GREEN** — `cmd/ddd_runner.go`: introduce `dddCommandSpec` and
      `newDddCommand(spec)`. Internal `runDdd` houses the finding-print +
      exit-code logic.

- [ ] **3A.3 GREEN** — `cmd/ddd_bc.go` collapses to ~15 lines of
      `dddCmd.AddCommand(newDddCommand(...))` only.

- [ ] **3A.4 GREEN** — `cmd/ddd_ul.go` likewise.

- [ ] **3A.5 GOLDEN + LINT**.

### 3B — Agents validate cluster (conditional)

- [ ] **3B.1 DECIDE** — measure: do `agents_validate_claude.go`,
      `agents_validate_naming.go`, `agents_validate_sync.go` share ≥80% of
      their structure? If yes, proceed; if no, skip and document.

- [ ] **3B.2 ACT** — if proceeding: extract `agentsValidateRunner`, RED +
      GREEN + GOLDEN + LINT.

### 3C — Specs validate cluster (conditional)

- [ ] **3C.1 DECIDE + ACT** — same gate as 3B for the four
      `cmd/specs_validate_*.go` files (`specs_validate_adoption.go`,
      `specs_validate_counts.go`, `specs_validate_links.go`,
      `specs_validate_tree.go`; note: `specs.go` parent dispatcher and
      `spec_coverage_validate.go` use different naming patterns and are
      separate concerns).

**Commit**: `refactor(rhino-cli): share ddd runner between bc and ul subcommands`
(plus additional commits if 3B/3C proceed)

---

## Phase 4 — Reporter consolidation

- [ ] **4.1 MEASURE** — for each of the six internal reporters
      (`agents`, `doctor`, `envbackup`, `mermaid`, `speccoverage`,
      `testcoverage`), count lines that would be deleted by switching
      `FormatJSON` to `cliout.Envelope[T]` and reuse of shared helpers.
      Record per-package delta in this checklist.

- [ ] **4.2 GATE** — apply consolidation only to packages where the net
      deletion is ≥30 lines AND the resulting reporter remains readable.
      Skip the rest.

- [ ] **4.3 RED + GREEN** — for each passing package:
  - Migrate `FormatJSON` to use `cliout.MarshalJSON[T]` with the existing
    schema string and status string.
  - Confirm output byte-identical via `TestGolden`.

- [ ] **4.4 GOLDEN + LINT**.

**Commit**: `refactor(rhino-cli): share JSON envelope across internal reporters`

---

## Phase 5 — Documentation + closeout

- [ ] **5.1** — update `apps/rhino-cli/README.md` "Internal architecture"
      section: short paragraph + bullet list pointing to `internal/cliout`,
      `internal/severity`, plus the sealed-enum pattern recipe.

- [ ] **5.2** — add a one-line entry to `apps/rhino-cli/CHANGELOG.md` (if
      that file exists; otherwise add a brief note in README).

- [ ] **5.3** — run full quality gate from a clean state:
  - `nx run rhino-cli:typecheck`
  - `nx run rhino-cli:lint`
  - `nx run rhino-cli:test:quick`
  - `nx run rhino-cli:test:integration`
  - `nx run rhino-cli:build`
  - Confirm pre-push hook passes locally.

- [ ] **5.4** — push worktree branch direct to `origin main` per Trunk-Based
      Development default for `ose-public` (or open draft PR if review-warranted).

- [ ] **5.4b** — after pushing, monitor GitHub Actions for the `ose-public`
      repository. Verify all CI checks pass. If any check fails, diagnose root
      cause and push a fix commit before proceeding to step 5.5. Do not declare
      work done until CI is green.

- [ ] **5.5** — wait for SHA to reach `origin/main`. Run `plan-execution-checker`
      against this plan. Confirm `TestGolden` is part of `test:quick` going
      forward (acts as ongoing behaviour-preservation guard).

- [ ] **5.6** — run from the `ose-public` worktree root:
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
