# Product Requirements — rhino-cli DRY + Exhaustive Enum Typing

## Product Overview

This plan refactors `apps/rhino-cli` to eliminate copy-paste duplication in
output formatting and severity resolution, and promotes plain `string` enum
fields to `//sumtype:decl` sealed interfaces enforced by `gochecksumtype`.
The result is a codebase where adding a new output format or severity variant
requires touching one place instead of fifteen, and where the Go compiler (via
the linter) rejects non-exhaustive type switches over these enums. No CLI
surface changes; all twenty-plus subcommands continue to produce byte-identical
output.

## Personas

- **Contributor** — the solo maintainer implementing this refactor; needs
  clear per-phase TDD steps and golden-test safety net to catch regressions.
- **Code reviewer** — reviews diffs; benefits from smaller, focused per-phase
  commits with no mixed concerns.
- **CI operator** — runs `nx run rhino-cli:lint` and `nx run rhino-cli:test:quick`
  in GitHub Actions; needs all quality gates to stay green throughout.
- **`swe-golang-dev` agent** — the suggested executor for Go implementation
  steps; needs explicit file paths, exact commands, and concrete acceptance
  criteria in delivery checkboxes.

## User Stories

- As a contributor, I want a single `cliout.Dispatcher[T]` that routes output
  to text/JSON/markdown, so that adding a new reporter does not require
  copy-pasting the `outputFuncs` pattern into a fifteenth file.
- As a contributor, I want `internal/severity` to own `Parse` and `Resolve`,
  so that a change to severity precedence logic requires editing one file,
  not two.
- As a code reviewer, I want per-phase commits that each keep `TestGolden`
  green, so that any phase can be independently reverted if a downstream issue
  surfaces.
- As a CI operator, I want `gochecksumtype` to reject non-exhaustive switches
  on every sealed enum introduced by this plan, so that a future variant
  addition cannot silently become a no-op branch.
- As a `swe-golang-dev` agent, I want every delivery checkbox to name the
  exact file, verbatim command, and observable acceptance criterion, so that I
  can execute without consulting additional documents.

## Product Scope

### In scope

- Extracting `internal/cliout` (output format sealed enum + dispatcher + JSON
  envelope helper) from the fifteen `cmd/*.go` files that currently repeat
  `outputFuncs`.
- Extracting `internal/severity` (severity sealed enum + parse + resolve) from
  `cmd/ddd_bc.go` and `cmd/ddd_ul.go`.
- Migrating `bcregistry.Relationship.Kind`, `bcregistry.Relationship.Role`,
  and `agents` reporter `Status` to sealed enums.
- Extracting a shared `dddRunner` so `ddd_bc.go` and `ddd_ul.go` share
  finding-print logic.
- Conditional extraction of `agentsValidateRunner` and `specsValidateRunner`
  if Phase 3 discovery confirms ≥80% / ≥3-command structural sharing.
- Phase 0 golden-output harness as a permanent behaviour-preservation guard.

### Out of scope

- Any change to `rhino-cli` CLI surface (flags, subcommand names, output text).
- Extraction to `libs/golang-commons` (considered only if `oseplatform-cli` or
  `ayokoding-cli` could reuse; default is rhino-cli `internal/`).
- `doctor.ToolCheck.Source` and `docs/links` link kind (conditional; only
  promoted if Phase 2/3 discovery confirms a closed set and ≥3 switch
  consumers).
- Version bump or CHANGELOG addition beyond a brief architecture note.

## Product Risks

- **Phase 4 `Reporter[T]` abstraction degrades readability**: if the generic
  envelope helper makes individual reporters harder to understand, the 30-net-
  line gate will cause Phase 4 to be skipped for those packages.
- **Golden test harness incomplete for edge cases**: if `manifest.yaml` misses
  a subcommand × output-format combination, a regression in that path can pass
  `TestGolden`. Mitigation: the fixture catalogue in delivery step 0.2 is
  exhaustive by design; gaps found during execution should be added before
  proceeding to Phase 1.
- **`doctor` fixture isolation unresolved**: the `doctor` package uses
  `CommandRunner` DI for test-time mocking; the golden harness approach for
  fixture isolation must be designed during Phase 0 (see tech-docs.md §Phase 0
  manifest.yaml note).
- **Merge conflict with concurrent feature work**: any concurrent PR adding a
  new `cmd/*.go` file using `outputFuncs` would need to be rebased onto Phase
  1. Mitigation: execute this plan before starting other cmd-layer work.

## 1. Functional requirements

### 1.1 Behaviour preservation (hard invariant)

For every `rhino-cli` subcommand invocation `inv` with arguments `args`,
flags `flags`, and stdin `stdin`, the pre-refactor build and the post-refactor
build must produce:

- Identical stdout (byte-for-byte).
- Identical stderr (byte-for-byte).
- Identical exit code.
- Identical filesystem mutations (for commands that write files:
  `agents sync`, `env backup`, `env restore`, `test-coverage merge`).
- Identical environment-variable read pattern (no new env vars consulted).

This invariant is enforced by golden-output fixtures captured in Phase 0
and replayed in every subsequent phase. Drift is a stop-the-line bug.

### 1.2 Shared output dispatcher

A new package or file (working name `internal/cliout` or `cmd/output.go`)
hosts:

- `OutputFormat` sealed interface with variants `FormatText{}`, `FormatJSON{}`,
  `FormatMarkdown{}`. Each variant implements `Code() string` returning the
  CLI literal (`"text"`, `"json"`, `"markdown"`).
- `ParseOutputFormat(s string) (OutputFormat, bool)` — recognises the three
  literals, returns `FormatText{}` as default when called with `""`,
  returns `false` for unknown.
- `Dispatcher[T any]` struct or equivalent that bundles three formatter
  callbacks and a `Write(cmd *cobra.Command, fmt OutputFormat, t T) error`
  method.

The fifteen `cmd/*.go` files that currently spell out `outputFuncs{text, json,
markdown}` + `writeFormatted(cmd, output, ...)` use the new helper. [Repo-grounded]

### 1.3 Shared severity enum

A new package `internal/severity` (or co-located shared `cmd/severity.go`):

- `Severity` sealed interface with variants `SeverityError{}` and
  `SeverityWarn{}`. Each implements `Code() string` returning `"error"` /
  `"warn"`.
- `Parse(s string) Severity` — case-insensitive, trims whitespace; accepts
  `"warn" | "warning"` → `SeverityWarn{}`; everything else → `SeverityError{}`
  (matches the current `normaliseSeverity` default).
- `Resolve(flagVal string, envVar string) Severity` — reproduces the current
  flag → env → default `"error"` precedence; emits the same stderr WARN line
  on env-driven downgrade.

`cmd/ddd_bc.go` and `cmd/ddd_ul.go` consume the shared resolver.
`internal/bcregistry/types.go` and `internal/glossary/types.go` change
`Finding.Severity` and `ValidateOptions.Severity` from `string` to the new
`Severity` interface. Existing callers updated.

### 1.4 Sealed enum adoption — sites in scope

The plan converts each of the following plain-`string` fields or flag values
into a `//sumtype:decl` sealed-interface enum (final list pinned in
[tech-docs.md](./tech-docs.md)):

| Site                                  | Today                                                            | Becomes                                                                  |
| ------------------------------------- | ---------------------------------------------------------------- | ------------------------------------------------------------------------ |
| `cmd/root.go` `--output`              | `var output string` (default `"text"`)                           | parsed once into `cliout.OutputFormat`, cmds receive interface           |
| `cmd/ddd_bc.go` `--severity`          | `var bcSeverity string`                                          | parsed via `severity.Resolve(...)`                                       |
| `cmd/ddd_ul.go` `--severity`          | `var ulSeverity string`                                          | parsed via `severity.Resolve(...)`                                       |
| `bcregistry.Finding.Severity`         | `string`                                                         | `severity.Severity`                                                      |
| `bcregistry.ValidateOptions.Severity` | `string`                                                         | `severity.Severity`                                                      |
| `glossary.Finding.Severity`           | `string`                                                         | `severity.Severity`                                                      |
| `glossary.ValidateOptions.Severity`   | `string`                                                         | `severity.Severity`                                                      |
| `bcregistry.Relationship.Kind`        | `string` (`customer-supplier`, `conformist`, `partnership`, …)   | sealed `RelationshipKind` (set fixed by `checkRelationshipKinds`)        |
| `bcregistry.Relationship.Role`        | `string` (`upstream`, `downstream`, …)                           | sealed `RelationshipRole`                                                |
| `agents` reporter status              | `string` (`"passed"` / `"warning"` / `"failed"`) [Repo-grounded] | sealed `agents.Status` (`StatusPassed`, `StatusWarning`, `StatusFailed`) |
| `doctor.CheckOptions.Scope`           | already sealed (`Scope`); `Source` field on tool checks          | `Source` becomes sealed if a fixed set is established                    |
| `docs/links` link kind                | string consts in `links_categorizer.go`                          | sealed `links.Kind` (only if site has ≥3 switch consumers; else keep)    |

The `Source` and `links.Kind` rows are conditional — promoted only if the
discovery in Phase 1 confirms a closed set and ≥3 switch consumers benefit.
Sites that fail the "≥3 consumers" rule stay plain `string` to avoid
over-engineering.

### 1.5 DRY pairs explicitly addressed

- `cmd/ddd_bc.go` ↔ `cmd/ddd_ul.go` — share `dddRunner` helper consuming a
  `dddCommandSpec` describing only the per-command differences (cmd name,
  short/long, example, validate function).
- The four `cmd/agents_validate_*.go` commands (`claude`, `naming`, `sync`,
  plus the parent dispatcher) — share an `agentsValidateRunner` if the
  variation is purely the validator function and the report renderer.
- The four `cmd/specs_validate_*.go` commands (`specs_validate_adoption.go`,
  `specs_validate_counts.go`, `specs_validate_links.go`, `specs_validate_tree.go`)
  [Repo-grounded] — apply the same pattern only if Phase 3 discovery confirms
  ≥3 commands share the same shape. (Note: `cmd/specs.go` parent dispatcher and
  `cmd/spec_coverage_validate.go` use different naming patterns; they are
  separate concerns.)

### 1.6 Reporter consolidation (Phase 4)

A generic `cliout.Reporter[T any]` shape replaces per-package
`FormatText / FormatJSON / FormatMarkdown` triplets where the public
contract is the same shape (input: result struct + verbose/quiet flags;
output: string). Specialised reporters that diverge meaningfully
(`testcoverage.FormatTextPerFile`, `doctor.symbolFor`) stay specialised
but share a common JSON envelope helper.

Hard rule: a consolidation must remove ≥30 net lines or be reverted.

## 2. Non-functional requirements

### 2.1 Linting

- `gochecksumtype` (already in `.golangci.yml`) must run clean on every
  introduced sealed interface — every `switch v := x.(type)` covers every
  variant or has `default` documented as intentional.
- `golangci-lint` overall passes on `apps/rhino-cli/...`.

### 2.2 Coverage

- `nx run rhino-cli:test:quick` continues to enforce ≥90% line coverage.
- New shared packages (`internal/cliout`, `internal/severity`) have unit
  tests achieving ≥90% line coverage individually.

### 2.3 Backward compatibility

- No `rhino-cli` version bump required — refactor is internal to a
  single binary; CLI surface unchanged.
- No git-hook contract change: `.husky/pre-commit`, `.husky/pre-push`
  continue to invoke the same `rhino-cli ...` commands with the same args.

### 2.4 Documentation

- `apps/rhino-cli/README.md` adds a short "Internal architecture" section
  pointing to the new shared packages.
- Each new shared package has a package-doc comment explaining the sealed
  enum + parser pattern and linking to one of the existing examples
  (`internal/testcoverage/types.go`).

## 3. Gherkin acceptance criteria

### Feature: Behaviour preservation

```gherkin
Feature: rhino-cli refactor preserves exact behaviour

  Background:
    Given a baseline rhino-cli binary built at the pre-refactor commit
    And a refactored rhino-cli binary built at the post-refactor commit
    And a fixture catalogue listing every documented subcommand with
      representative args, all three --output modes, and verbose/quiet
      permutations

  Scenario: Stdout matches byte-for-byte
    When each fixture invocation is run against the baseline binary
    And the same invocation is run against the refactored binary
    Then the captured stdout streams are identical

  Scenario: Stderr matches byte-for-byte
    When each fixture invocation is run against both binaries
    Then the captured stderr streams are identical

  Scenario: Exit codes match
    When each fixture invocation is run against both binaries
    Then both binaries return the same exit code

  Scenario Outline: File-emitting commands produce identical files
    Given a fixture for "<cmd>" that writes to a temp directory
    When each binary runs the fixture against fresh temp dirs
    Then the resulting file trees are byte-identical

    Examples:
      | cmd                    |
      | agents sync            |
      | env backup             |
      | env restore            |
      | test-coverage merge    |
```

### Feature: Severity is sealed and exhaustively checked

```gherkin
Feature: severity is a sealed enum

  Scenario: Parse accepts the documented literals
    When severity.Parse is called with "warn"
    Then it returns SeverityWarn{}
    When severity.Parse is called with "warning"
    Then it returns SeverityWarn{}
    When severity.Parse is called with "error"
    Then it returns SeverityError{}
    When severity.Parse is called with "ERROR"
    Then it returns SeverityError{}

  Scenario: Parse defaults to error for unknown values
    When severity.Parse is called with "fatal"
    Then it returns SeverityError{}
    When severity.Parse is called with ""
    Then it returns SeverityError{}

  Scenario: Resolve honours flag > env > default precedence
    Given the OSE_RHINO_DDD_SEVERITY env var is set to "warn"
    When severity.Resolve is called with flag="error" and the same env var
    Then it returns SeverityError{}
    And nothing is written to stderr

  Scenario: Env-var downgrade emits the documented WARN line
    Given the OSE_RHINO_DDD_SEVERITY env var is set to "warn"
    When severity.Resolve is called with flag="" and the same env var
    Then it returns SeverityWarn{}
    And stderr contains 'WARN: severity downgraded to "warn" via OSE_RHINO_DDD_SEVERITY env var'

  Scenario: gochecksumtype catches missing variants
    Given a new severity variant SeverityInfo{} is added to the sealed interface
    When go vet runs with the gochecksumtype linter
    Then every type switch over Severity that doesn't handle SeverityInfo fails the lint
```

### Feature: Output format is sealed and parsed once

```gherkin
Feature: --output flag is parsed once into a sealed enum

  Scenario: ParseOutputFormat recognises the three documented literals
    When cliout.ParseOutputFormat is called with "text"
    Then it returns FormatText{} and true
    When cliout.ParseOutputFormat is called with "json"
    Then it returns FormatJSON{} and true
    When cliout.ParseOutputFormat is called with "markdown"
    Then it returns FormatMarkdown{} and true

  Scenario: ParseOutputFormat treats empty as text default
    When cliout.ParseOutputFormat is called with ""
    Then it returns FormatText{} and true

  Scenario: ParseOutputFormat rejects unknown values
    When cliout.ParseOutputFormat is called with "yaml"
    Then it returns nil and false

  Scenario: Cobra commands receive the parsed enum
    When any subcommand RunE is invoked
    Then it consumes a cliout.OutputFormat value, not a string
```

### Feature: ddd bc and ddd ul share a single implementation

```gherkin
Feature: ddd bc and ddd ul share a runner

  Scenario: A bug fix in one applies to the other
    Given the shared dddRunner helper exists
    When a fix is made to the finding-print loop in dddRunner
    Then both rhino-cli ddd bc and rhino-cli ddd ul show the fix
    And no second edit to a sibling file is required

  Scenario: The cmd-file pair has no copy-paste severity resolver
    When grep -n "func resolveBcSeverity" cmd/ddd_bc.go is run
    Then no match is returned
    When grep -n "func resolveUlSeverity" cmd/ddd_ul.go is run
    Then no match is returned
    When grep -n "func normaliseSeverity" cmd/*.go is run
    Then no match is returned
```

### Feature: Lint and coverage gates stay green

```gherkin
Feature: Quality gates pass on every phase

  Scenario: golangci-lint passes after each phase
    When nx run rhino-cli:lint is invoked at the end of any phase
    Then the command exits 0

  Scenario: gochecksumtype passes on every sealed enum
    When the lint run reaches a sealed interface declared with //sumtype:decl
    Then every type switch over that interface is reported exhaustive

  Scenario: test:quick stays at or above 90 percent
    When nx run rhino-cli:test:quick is invoked at the end of any phase
    Then the command exits 0
    And the reported coverage is >= 90 percent
```
