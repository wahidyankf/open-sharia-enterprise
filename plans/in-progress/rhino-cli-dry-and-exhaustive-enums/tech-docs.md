# Technical Design — rhino-cli DRY + Exhaustive Enum Typing

## Architecture overview

Three new shared compilation units, every existing command and internal
package converging on them:

```
apps/rhino-cli/
├── cmd/
│   ├── output.go            ← NEW: glues writeFormatted into cliout
│   ├── severity.go          ← NEW: glues flag parsing into severity pkg
│   ├── ddd_runner.go        ← NEW: shared dddBcCmd / dddUlCmd runner
│   ├── ddd_bc.go            ← shrinks ~80%
│   ├── ddd_ul.go            ← shrinks ~80%
│   └── ... (other cmd files: drop outputFuncs boilerplate)
└── internal/
    ├── cliout/              ← NEW package
    │   ├── format.go        ← OutputFormat sealed interface + Parse
    │   ├── dispatcher.go    ← Dispatcher[T] helper
    │   ├── envelope.go      ← shared JSON envelope helpers
    │   └── format_test.go
    ├── severity/            ← NEW package
    │   ├── severity.go      ← Severity sealed interface + Parse + Resolve
    │   └── severity_test.go
    ├── bcregistry/          ← Severity / Kind / Role → sealed enums
    ├── glossary/            ← Severity → sealed enum
    ├── agents/              ← reporter Status → sealed enum
    └── ... (others adopt cliout.Reporter[T] where it removes ≥30 lines)
```

## Existing pattern (reuse, do not reinvent)

The repo already ships the sealed-interface + `//sumtype:decl` pattern.
Canonical examples:

- `internal/testcoverage/types.go` — `Format` interface, 5 variants
- `internal/doctor/types.go` — `ToolStatus`, `Scope` interfaces
- `internal/mermaid/types.go` — `Direction`, `ViolationKind`, `WarningKind`

`gochecksumtype` is enabled in `.golangci.yml` line 20. New enums follow
the **same shape**:

```go
// Severity is a sealed enum: "error" or "warn".
//
//sumtype:decl
type Severity interface {
    isSeverity()
    Code() string
}

// SeverityError is the default; rhino-cli exits non-zero on findings.
type SeverityError struct{}

func (SeverityError) isSeverity()    {}
func (SeverityError) Code() string   { return "error" }
func (SeverityError) String() string { return "error" }

// SeverityWarn is the downgrade variant; reachable only via flag or env.
type SeverityWarn struct{}

func (SeverityWarn) isSeverity()    {}
func (SeverityWarn) Code() string   { return "warn" }
func (SeverityWarn) String() string { return "warn" }
```

The `isSeverity()` unexported method seals the interface to the same
package. `//sumtype:decl` activates `gochecksumtype` exhaustiveness on
type switches.

## Phase 0 — Golden-output safety net

### Why first

Every later phase rests on byte-identical output. The only way to know
the refactor preserves behaviour is to capture the pre-refactor output
first, in a way the post-refactor test suite can replay.

### Fixture catalogue shape

`apps/rhino-cli/cmd/testdata/golden/` directory tree:

```
golden/
├── manifest.yaml                       # list of fixture entries
├── ddd-bc-no-findings.txt              # expected stdout
├── ddd-bc-no-findings.stderr.txt       # expected stderr
├── ddd-bc-no-findings.exit              # single int
├── ddd-bc-warn-downgrade.{stdout,stderr,exit}
├── doctor-text.{stdout,stderr,exit}
├── doctor-json.{stdout,stderr,exit}
├── doctor-markdown.{stdout,stderr,exit}
├── agents-sync-noop.{stdout,stderr,exit,fs-hash}
└── ...
```

`manifest.yaml` lists each fixture:

```yaml
- name: ddd-bc-no-findings
  cmd: [ddd, bc, organiclever]
  fixture_repo: testdata/fixtures/organiclever-clean
  env: {}
- name: ddd-bc-warn-downgrade
  cmd: [ddd, bc, organiclever, --severity=warn]
  fixture_repo: testdata/fixtures/organiclever-with-finding
  env: {}
- name: doctor-json
  cmd: [doctor, --output=json]
  # doctor requires a dependency-injected CommandRunner for test-time tool
  # version mocking — not environment variables. The DI pattern is
  # fakeRunnerConfig in internal/doctor/checker_test.go. The exact approach
  # for golden-harness fixture isolation must be designed during Phase 0
  # implementation (e.g., expose a RunnerFactory hook on the doctor command).
  env: {}
```

### Golden test harness

A new `cmd/golden_test.go` (built without `//go:build integration` so it
runs in `test:quick`) iterates the manifest, runs `cmd.RunE` in-process
against the fixture repo, captures stdout/stderr/exit, and compares to
the recorded files. `-update` flag regenerates fixtures.

### Critical: capture before any refactor commit

The first commit on the worktree branch:

1. Adds the manifest + harness.
2. Runs harness with `-update` against pre-refactor source.
3. Commits the captured fixtures + manifest.
4. Reverts the `-update` invocation; harness now compares.

This commit is the "before" anchor. Every subsequent commit must keep
`go test ./cmd -run TestGolden` green.

## Phase 1 — Shared cliout + severity

### `internal/cliout`

```go
// Package cliout centralises CLI output formatting for rhino-cli commands.
package cliout

//sumtype:decl
type OutputFormat interface {
    isOutputFormat()
    Code() string
    String() string
}

type FormatText struct{}
type FormatJSON struct{}
type FormatMarkdown struct{}

// (interface methods elided — same shape as Severity above)

// Parse returns (FormatText{}, true) for "" or "text", FormatJSON for "json",
// FormatMarkdown for "markdown". Returns (nil, false) for unknown values.
func Parse(s string) (OutputFormat, bool) { ... }

// Dispatcher bundles three formatter callbacks; one variant runs per Write.
type Dispatcher[T any] struct {
    Text     func(t T, verbose, quiet bool) string
    JSON     func(t T) (string, error)
    Markdown func(t T) string
}

// Write picks the variant matching fmt and writes to w. Returns the JSON
// formatter's error verbatim; never returns an error for Text/Markdown.
func (d Dispatcher[T]) Write(w io.Writer, fmt OutputFormat, t T, verbose, quiet bool) error
```

Each cmd file replaces the local `outputFuncs` literal with a `cliout.Dispatcher[*Result]`
construction and calls `d.Write(cmd.OutOrStdout(), parsedFormat, result, verbose, quiet)`.

### `internal/severity`

```go
package severity

//sumtype:decl
type Severity interface {
    isSeverity()
    Code() string
    String() string
}

type SeverityError struct{}
type SeverityWarn struct{}

// Parse is case-insensitive, trims whitespace. "warn"/"warning" → Warn;
// everything else (including "") → Error.
func Parse(s string) Severity { ... }

// Resolve implements the flag > env > default precedence used by ddd bc
// and ddd ul today. envVar is the env-var name to consult when flagVal is
// empty. stderr receives the documented WARN line on env-driven downgrade.
func Resolve(flagVal, envVar string, stderr io.Writer) Severity { ... }
```

`cmd/severity.go` exposes one cobra-side flag-binding helper that takes a
`*cobra.Command`, a target `*severity.Severity` pointer, and the env-var
name. Used by `ddd_bc.go` and `ddd_ul.go`.

### Internal struct migrations (Phase 1, tail end)

- `bcregistry.Finding.Severity` and `ValidateOptions.Severity` change type
  from `string` to `severity.Severity`. Validator code path updated; existing
  `if f.Severity == "error"` comparisons become type switches caught by
  `gochecksumtype`.
- `glossary.Finding.Severity` and `ValidateOptions.Severity` likewise.
- All test fixtures updated.

## Phase 2 — Remaining sealed-enum sites

Per `prd.md` §1.4 table. Two non-trivial ones:

### `bcregistry.Relationship.Kind`

`validator.go:301` already enumerates the closed set:

```go
asymmetricKinds := map[string]bool{
    "customer-supplier": true,
    "conformist":        true,
}
```

`validator.go:347` enumerates the full closed set in `checkRelationshipKinds`.
The plan introduces:

```go
//sumtype:decl
type RelationshipKind interface {
    isRelationshipKind()
    Code() string
    IsAsymmetric() bool
}

type KindPartnership struct{}        // symmetric
type KindSharedKernel struct{}       // symmetric
type KindCustomerSupplier struct{}   // asymmetric
type KindConformist struct{}         // asymmetric
type KindAntiCorruption struct{}     // asymmetric (one-way; not in symmetry check)
type KindOpenHostService struct{}    // asymmetric (one-way; not in symmetry check)
// Six variants match the knownKinds map in validator.go:350 [Repo-grounded]
// Note: anticorruption-layer and open-host-service are intentionally one-way;
// checkRelationshipSymmetry does not include them in the asymmetricKinds map.
```

YAML unmarshalling: `Kind string` stays in the YAML wire format (back-compat
for `bounded-contexts.yaml` registries); a post-unmarshal validator promotes
the string to the sealed variant, returning a parse error on unknown values.

### `agents` reporter status

`internal/agents/agent_validator.go:40` documents three statuses:
`"passed" | "warning" | "failed"` [Repo-grounded — confirmed via grep on
`agent_validator.go` and `reporter.go:262` `case "passed":`].
Note: the actual string is `"passed"`, not `"ok"`. Promote to:

```go
//sumtype:decl
type Status interface {
    isStatus()
    Code() string
    IsFailure() bool // true only for StatusFailed
}

// StatusPassed represents a check that passed. Code() returns "passed" to
// preserve byte-identical output with the existing reporter.go switches.
type StatusPassed struct{}

func (StatusPassed) isStatus()      {}
func (StatusPassed) Code() string   { return "passed" }
func (StatusPassed) IsFailure() bool { return false }

type StatusWarning struct{}

func (StatusWarning) isStatus()      {}
func (StatusWarning) Code() string   { return "warning" }
func (StatusWarning) IsFailure() bool { return false }

type StatusFailed struct{}

func (StatusFailed) isStatus()      {}
func (StatusFailed) Code() string   { return "failed" }
func (StatusFailed) IsFailure() bool { return true }
```

Reporter switches in `internal/agents/reporter.go` (`case "passed":`,
`case "warning":`, and `case "failed":` at lines 262–264 and 386–388)
become exhaustive type switches. The `Code()` return values MUST match
the existing string literals byte-identically to preserve golden output.

## Phase 3 — DRY runner extraction

### `dddRunner`

```go
// dddCommandSpec describes the per-command differences between ddd bc and
// ddd ul; everything else is shared.
type dddCommandSpec struct {
    Use       string
    Short     string
    Long      string
    Example   string
    Validator func(opts ValidateOptions) ([]Finding, error)
}

func newDddCommand(spec dddCommandSpec) *cobra.Command {
    var sevFlag string
    cmd := &cobra.Command{
        Use:           spec.Use,
        Short:         spec.Short,
        Long:          spec.Long,
        Example:       spec.Example,
        Args:          cobra.ExactArgs(1),
        SilenceErrors: true,
        RunE: func(cmd *cobra.Command, args []string) error {
            return runDdd(cmd, args, sevFlag, spec.Validator)
        },
    }
    cmd.Flags().StringVar(&sevFlag, "severity", "", "override finding severity: warn|error")
    return cmd
}
```

`cmd/ddd_bc.go` collapses to:

```go
func init() {
    dddCmd.AddCommand(newDddCommand(dddCommandSpec{
        Use:       "bc <app>",
        Short:     "Validate bounded-context structural parity ...",
        Long:      `Verify that the filesystem ...`,
        Example:   `  rhino-cli ddd bc organiclever ...`,
        Validator: bcValidateAllFn,
    }))
}
```

Same for `ddd_ul.go`.

The shared `runDdd` function houses the finding-print loop and exit-code
computation; both files lose their bespoke versions.

### Agent / spec runners

Apply the same template **only if** Phase 3 discovery shows ≥3 commands
share the shape with ≤2 per-command differences. Don't force a runner
abstraction onto two commands that diverge in five places.

## Phase 4 — Reporter consolidation

Each of the six `internal/*/reporter.go` files implements:

```go
func FormatText(r *Result, verbose, quiet bool) string
func FormatJSON(r *Result) (string, error)
func FormatMarkdown(r *Result) string
```

with a per-package JSON envelope struct. A shared `cliout.Envelope[T]`:

```go
type Envelope[T any] struct {
    Schema  string `json:"schema"`
    Status  string `json:"status"`
    Result  T      `json:"result"`
}

func MarshalJSON[T any](schema, status string, payload T) (string, error)
```

removes the per-package envelope boilerplate. Each package keeps its
own `FormatText` / `FormatMarkdown` (per-package wording differs in
small ways) but `FormatJSON` uses the shared helper.

Hard gate: this phase is **skipped** for any package where the
consolidation removes <30 net lines.

## Cobra flag → sealed enum binding

```go
func BindOutputFormat(cmd *cobra.Command, target *cliout.OutputFormat) {
    var raw string
    cmd.PersistentFlags().StringVarP(&raw, "output", "o", "text",
        "output format: text, json, markdown")
    cobra.OnInitialize(func() {
        v, ok := cliout.Parse(raw)
        if !ok {
            fmt.Fprintf(os.Stderr, "invalid --output: %q\n", raw)
            osExit(1)
        }
        *target = v
    })
}
```

`cmd/root.go` keeps the `--output` flag declaration and parses the value
exactly once at startup; every cmd accesses the sealed enum via the
package-level target.

## Behaviour-preservation tactics

| Tactic                                | What it catches                                            |
| ------------------------------------- | ---------------------------------------------------------- |
| Golden stdout/stderr/exit fixtures    | Any line-level drift in user-visible output                |
| Golden file-tree hashes               | Drift in `agents sync`, `env backup`, `env restore` output |
| `go test -run TestGolden` in pre-push | Catches regressions before they hit CI                     |
| `gochecksumtype` lint                 | Missing variant in any switch over an introduced enum      |
| `nx run rhino-cli:test:integration`   | Real-filesystem behavioural parity (mocked → real path)    |
| Per-phase `nx affected -t lint`       | Surface unused imports, dead refs, etc. on each phase end  |

## Rollback strategy

Per-phase commits, each independently revertible:

- `commit 1` — Phase 0 golden harness + recorded fixtures.
- `commit 2..N` — Phase 1 changes (cliout/severity package, callers).
- `commit N+1..M` — Phase 2 (additional enum sites).
- `commit M+1..P` — Phase 3 (runners).
- `commit P+1..Q` — Phase 4 (reporters), only if net deletion holds.

Phase 0's commit must never be reverted; phases 1–4 can each be reverted
in isolation if downstream contributors hit blocking issues.

## Worktree compliance

Per Subrepo Worktree Workflow Standard 11, this plan modifies
`apps/rhino-cli` (and possibly `libs/golang-commons` if a primitive is
extracted there). Execution path:

```
cd ose-public && claude --worktree rhino-cli-dry-and-exhaustive-enums
```

Worktree lands at `worktrees/rhino-cli-dry-and-exhaustive-enums/` per the repo's
worktree-path override. Commits accumulate on `worktree-rhino-cli-dry-and-exhaustive-enums`,
publish to `main` direct (Trunk Based Development default for `ose-public`).

## Open questions

| Q                                                                                                          | Resolution path                                                                                                                                         |
| ---------------------------------------------------------------------------------------------------------- | ------------------------------------------------------------------------------------------------------------------------------------------------------- |
| Should `cliout` live at `apps/rhino-cli/internal/cliout` or `libs/golang-commons/cliout`?                  | Phase 1 discovery: extract to libs only if `oseplatform-cli` / `ayokoding-cli` could reuse. Default to rhino-cli `internal/`.                           |
| Does `Relationship.Kind` have a closed set, or is it open in practice?                                     | Audit every registered `bounded-contexts.yaml`. If open, keep `string` and skip that row.                                                               |
| Phase 4 reporter consolidation — does it survive the 30-net-line rule?                                     | Decided at end of Phase 3; documented in `delivery.md` checkbox.                                                                                        |
| Should `--severity` value validation happen at flag parse (reject unknown) or fall through to error today? | Today: silent fallback to `error`. Preservation rule: keep silent fallback. New `severity.Parse` matches existing `normaliseSeverity` defaults exactly. |
