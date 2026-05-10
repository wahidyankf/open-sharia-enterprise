# PRD — rhino-cli DRY + Enum Refactor Pass

## Product Overview

A 16-item internal refactor of `apps/rhino-cli`, executed as one
coordinated plan but landed in themed, independently-revertable phases.
Zero behaviour change. Output of every command is byte-identical before
and after. Test suite passes throughout.

## Personas (Maintainer Hats)

- **CLI maintainer hat** — touches `cmd/` and `internal/` to add or fix
  rhino-cli commands; benefits from one canonical shape per pattern.
- **Agent author hat** — runs `rhino-cli agents validate-claude`;
  benefits from typed `CheckStatus` catching typos.
- **BDD/spec author hat** — runs `rhino-cli spec-coverage validate`;
  benefits from extractor consolidation when adding a language.
- **DDD author hat** — runs `rhino-cli ddd bc` / `ddd ul`; benefits from
  one shared severity resolver.
- **CI consumer** (every workspace's pre-push) — sees zero change. Hard
  acceptance criterion.

## User Stories

- **US-1** — As a CLI maintainer, I want a single canonical shape for
  spec-validate subcommands so adding a fifth subcommand doesn't require
  copy-pasting `resolveXxxApps` + driver + finding printer.
- **US-2** — As an agent author, I want the compiler to flag a typo in
  a `ValidationCheck.Status` value so I cannot land a `"passsed"` typo
  that silently no-ops the validator.
- **US-3** — As a BDD author, I want adding a new language for
  `spec-coverage` to be a single-map-entry change, not a multi-file
  scavenger hunt across `extractAllStepTexts`, `extractScenarioTitles`,
  `isTestFile`, and a new `<lang>_steps.go`.
- **US-4** — As a DDD author, I want `ddd bc` and `ddd ul` to share
  one severity resolver so a fix to `--severity` precedence is a
  one-line change, not a two-line change in two files.
- **US-5** — As a CI consumer of `rhino-cli`, I want the refactor to
  preserve every command's output byte-for-byte so my pre-push hooks
  don't break.

## Functional Requirements

### Type-Safety Requirements

- **FR-1** — `internal/speccoverage` defines `type matcherKind int` (or
  `type matcherKind string`; choice in tech-docs.md) with exported
  consts for the closed set `{exact, pattern}`. Every switch on
  `matcherKind` has a `default: panic(fmt.Sprintf("unhandled matcherKind: %v", k))`
  branch.
- **FR-2** — `internal/agents` defines `type CheckStatus string` with
  consts `StatusPassed`, `StatusWarning`, `StatusFailed`. Every
  string literal `"passed"|"warning"|"failed"` in non-test code is
  replaced by the typed const. Test files may keep string literals if
  they assert against JSON output where strings are the wire format.
- **FR-3** — `cmd` defines `type Criticality string` with consts
  `CriticalityHigh`, `CriticalityMedium`, `CriticalityLow`. Every
  `SpecFinding.Criticality` assignment uses the typed const.
- **FR-4** — `internal/bcregistry` and `internal/glossary` each define
  `type Severity string` with consts `SeverityError`, `SeverityWarn`.
  All severity comparisons use `==`/typed comparison, not string
  comparison.
- **FR-5** — `internal/mermaid` `Direction`, `ViolationKind`,
  `WarningKind` switches gain explicit `default: panic` branches OR
  document why a no-op default is correct.

### DRY Requirements

- **FR-6** — `internal/speccoverage` adds two helpers:
  `scanLines(path, sm, []lineRule) error` for line-by-line extractors
  and `scanFull(path, sm, []fullRule) error` for whole-file
  extractors. Each `<lang>_steps.go` file shrinks to a regex-table
  declaration plus a one-line call to the appropriate helper.
- **FR-7** — `extractAllStepTexts` switch becomes a registry lookup
  in `var stepExtractorsByExt = map[string]func(string, *stepMatcher) error`.
  Same for `extractScenarioTitles`.
- **FR-8** — `cmd/severity.go` (new file) defines `resolveSeverity(flag, envKey, defaultSev) Severity`.
  `ddd_bc.go` and `ddd_ul.go` call this single helper. The duplicated
  `normaliseSeverity` / `normaliseUlSeverity` functions are deleted.
- **FR-9** — `cmd/specs_driver.go` (new file) defines
  `runSpecsValidator(cmd, args, label, fn, resolver)` shared driver.
  Each `specs_validate_*.go` runE shrinks to a 3-line call.
  `resolveSpecsApps` (for app-name commands) and
  `resolveSpecsFolders` (for folder-path commands) are the two
  shared resolvers.
- **FR-10** — `internal/agents.ValidationResult` gains an `Add(check)`
  method that appends + increments the correct counter based on
  `CheckStatus`. `sync_validator.go` callers use `Add` instead of the
  4× duplicated tally block.
- **FR-11** — `internal/agents` adds `passed(name, msg)` and
  `failed(name, expected, actual, msg)` constructor helpers.
  `agent_validator.go` and `skill_validator.go` use them in place of
  literal `ValidationCheck{...}` struct expressions where the helper
  shape fits.
- **FR-12** — `internal/doctor` extracts `withEmptyOK(compareFn) compareFn`
  decorator. `compareExact`, `compareMajor`, `compareMajorGTE`,
  `compareGTE` are wrapped with it; the duplicated `if required == ""`
  preamble in each is deleted.
- **FR-13** — `internal/doctor.toolDef` gains a `minimal bool` field.
  `MinimalTools` map in `types.go` is deleted. `CheckAll`'s scope
  filter checks `def.minimal` directly.

### Legacy Cleanup Requirements

- **FR-14** — `stepMatcher.exact` and `stepMatcher.patterns` legacy
  fields are deleted. All test code synthesizing matchers directly is
  migrated to `addExactWithOrigin` / `addPatternWithOrigin`.
  `matches()` operates on `entries` directly. The `exactIndex` map is
  retained for O(1) exact lookup.

### Cobra-Layer DRY Requirements

- **FR-15** — `cmd/naming_driver.go` (new file) defines
  `runNamingValidator(cmd, label, kind, validatorFn) error`.
  `agents_validate_naming.go` and `workflows_validate_naming.go` runE
  bodies shrink to a single call.
- **FR-16** — `cmd/helpers.go` adds `mustFindGitRoot(cmd) (string, error)`
  encapsulating the `findGitRoot()` + error-wrap pattern. The 24
  callers in `cmd/` collapse to a single helper call.

## Non-Functional Requirements

- **NFR-1** — Coverage stays ≥ 90% (Go-tier threshold).
  `rhino-cli test-coverage validate` exits 0.
- **NFR-2** — `nx run rhino-cli:lint` exits 0; no new
  `staticcheck`/`go vet` violations introduced.
- **NFR-3** — `nx run rhino-cli:test:quick` exits 0; no test deletions
  except where a test asserts against the deleted legacy `exact` /
  `patterns` fields directly.
- **NFR-4** — Pre-push hook timing parity: refactored CLI runs no
  slower than the baseline within ±10% wall-clock on the maintainer's
  laptop (informal, measured before/after the plan).

## Gherkin Acceptance Criteria

```gherkin
Feature: rhino-cli refactor preserves every command's behaviour

  Background:
    Given the worktree has the refactor applied
    And the rhino-cli binary is rebuilt from source

  # --- Behaviour parity (the main contract) ---

  Scenario: agents validate-claude command output is unchanged
    Given a fixture of agent files with known outcomes
    When I run "rhino-cli agents validate-claude"
    Then the exit code is the same as the pre-refactor baseline
    And the stdout text output matches the pre-refactor baseline byte-for-byte
    And the stdout JSON output (with -o json) matches the pre-refactor baseline

  Scenario: spec-coverage validate output is unchanged
    Given a fixture spec tree with known coverage gaps
    When I run "rhino-cli spec-coverage validate <specs> <app>"
    Then the exit code is the same as the pre-refactor baseline
    And the gap reports match the pre-refactor baseline byte-for-byte

  Scenario: doctor scope filtering still works
    When I run "rhino-cli doctor --scope minimal"
    Then only the seven minimal-scope tools are checked
    And the order matches the pre-refactor baseline

  Scenario: ddd bc severity precedence is preserved
    Given OSE_RHINO_DDD_SEVERITY is set to "warn"
    And the user passes no --severity flag
    When I run "rhino-cli ddd bc <app>"
    Then findings report severity "warn"
    And stderr contains the deprecation warning message

  Scenario: ddd ul severity precedence is preserved
    Given OSE_RHINO_DDD_SEVERITY is set to "warn"
    And the user passes "--severity error"
    When I run "rhino-cli ddd ul <app>"
    Then the --severity flag wins
    And findings report severity "error"

  Scenario: specs validate-tree default app expansion is unchanged
    Given the user runs the command with no positional and no --apps
    When I run "rhino-cli specs validate-tree"
    Then validation runs for every app in allowlist.AppsWithDDD
    And the output order matches the allowlist order

  # --- Type-safety wins (the new contract) ---

  Scenario: invalid CheckStatus value is a compile error
    Given a developer changes a string-typed status to "passsed"
    When the developer runs "go build ./..."
    Then the compile fails with a type mismatch error
    And the binary is not produced

  Scenario: matcherKind switch with missing case panics with a clear message
    Given a hypothetical future matcherKind value is added without updating the orphan switch
    When the orphan check encounters such an entry
    Then the program panics with "unhandled matcherKind: <value>"
    And the message names the unhandled value

  Scenario: doctor adding a tool no longer requires updating two places
    Given a developer adds a new toolDef entry to buildToolDefs
    And the developer marks it minimal: true
    When the developer runs "rhino-cli doctor --scope minimal"
    Then the new tool is included in the minimal scope automatically
    And no separate MinimalTools map needs editing

  # --- DRY wins (mostly invisible, surface only via test edits) ---

  Scenario: Adding a new language for spec-coverage touches one map entry
    Given a developer adds a hypothetical .swift file extractor
    When the developer registers the extractor in stepExtractorsByExt
    Then "rhino-cli spec-coverage validate" picks up .swift files automatically
    And no edits to extractAllStepTexts switch body are required

  Scenario: Test suite still passes
    When I run "nx run rhino-cli:test:quick"
    Then the exit code is 0
    And coverage report shows ≥ 90% line coverage
    And no test was skipped or deleted that asserted real behaviour

  Scenario: Pre-push hook for affected projects passes
    When I run "npx nx affected -t typecheck lint test:quick"
    Then the exit code is 0

  # --- Legacy cleanup ---

  Scenario: stepMatcher legacy fields are removed
    When I grep for ".exact" or ".patterns" field access in non-test code
    Then no matches are found
    And the canonical "entries" plus "exactIndex" fields are the only state
```

## Product Risks

- **PR-1** — A test in `internal/speccoverage` synthesizes `stepMatcher`
  state by appending directly to `.exact` / `.patterns`. **Likelihood**:
  Confirmed by grep; the question is _how many_. **Impact**: Each such
  test must migrate to `addExactWithOrigin` / `addPatternWithOrigin`.
  **Mitigation**: Phase 2 starts with an audit; threshold gate (>5
  files) defers item 14 to a follow-up plan.

- **PR-2** — A switch over the new typed enum is missed in a peripheral
  file (e.g., a reporter that formats output by string-comparing
  `Status`). **Likelihood**: Low — `grep` covers it. **Impact**: Stale
  string-typed call site silently keeps working but loses the
  type-safety win. **Mitigation**: Each enum-tightening phase ends with
  a `grep -rn '"old-literal"'` pass that must return zero.

- **PR-3** — JSON wire format breakage. The existing JSON outputs use
  string values like `"passed"`, `"HIGH"`, `"error"`. Typed enums must
  marshal to the same string for downstream consumers. **Mitigation**:
  Use string-typed enums (`type CheckStatus string`) where JSON output
  is involved, NOT int-typed iota enums; matcherKind can be int-typed
  because it never marshals.

- **PR-4** — Internal API change for `bcregistry`/`glossary` `Severity`
  string-to-typed migration could affect a caller. **Likelihood**:
  Low — only `cmd/ddd_*.go` calls these packages and they're being
  refactored in the same plan. **Mitigation**: Phase 5 sequences both
  ends in the same commit boundary.

## In-Scope / Out-of-Scope Recap

| In scope                                                                  | Out of scope                                                              |
| ------------------------------------------------------------------------- | ------------------------------------------------------------------------- |
| 16 items listed in README.md / FR-1 to FR-16                              | New rhino-cli features, flags, or subcommands                             |
| Test migrations strictly required by the refactor                         | New tests beyond those covering the new helpers                           |
| Doc-comment updates that follow renamed types                             | Standalone documentation rewrite                                          |
| Internal export-signature changes (`bcregistry.Severity` etc.)            | Cross-workspace API changes (no rhino-cli library consumers exist anyway) |
| Phase ordering optimization (broad-reach phases first; risky phases last) | `ose-primer` propagation of any patterns surfaced                         |
