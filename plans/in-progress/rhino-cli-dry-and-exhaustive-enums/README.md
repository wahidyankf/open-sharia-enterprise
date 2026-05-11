# rhino-cli — DRY Refactor + Exhaustive Enum Typing

**Status**: In Progress
**Scope**: `ose-public` — `apps/rhino-cli` (and minimally `libs/golang-commons` if extraction warranted)
**Branch**: TBD (worktree to be created at execution time per Subrepo Worktree Workflow)

## Problem

`apps/rhino-cli` has grown to 19k+ lines across ~50 cobra commands and 14 internal
packages. Two recurring forms of pain are visible:

1. **Duplication** — sibling `cmd/*.go` files (e.g., `ddd_bc.go` ↔ `ddd_ul.go`,
   the six `*-validate-*` agent/spec/workflow commands) carry hand-copied
   severity resolvers, init blocks, output dispatchers, and run-loop shapes.
   Six internal packages (`agents`, `doctor`, `envbackup`, `mermaid`,
   `speccoverage`, `testcoverage`) each independently implement
   `FormatText / FormatJSON / FormatMarkdown` plus their own JSON envelope.
2. **Stringly-typed enum fields** — `Finding.Severity string`, `Relationship.Kind
string`, `Relationship.Role string`, the `--output` global flag, the
   `--severity` CLI flag, and several internal validator switches are plain
   `string` values matched by ad-hoc `switch` blocks. Adding a new variant
   requires hunting every `switch` and praying defaults catch typos. The repo
   already ships the **sealed interface + `//sumtype:decl`** pattern (see
   `internal/testcoverage/types.go`, `internal/doctor/types.go`,
   `internal/mermaid/types.go`) plus the `gochecksumtype` linter enabled in
   `.golangci.yml` — but most of the codebase hasn't adopted it.

## Goal

Reduce duplication and make enum-shaped data exhaustively type-checked, **with
zero behavioural change**. After this plan:

- Every CLI command, output payload, exit code, log line, and error message is
  byte-identical to the pre-refactor build (verified by golden output tests
  added in early phases).
- The `//sumtype:decl` sealed-interface pattern replaces every plain-`string`
  enum site listed in [prd.md](./prd.md) §3.
- A single shared `cmd/output.go` (or `internal/cliout`) hosts the
  `outputFuncs` / `writeFormatted` plumbing; per-command duplication is gone.
- A single shared `internal/severity` (or equivalent) package hosts
  `Severity` enum + parser + env-var resolution; `ddd_bc` / `ddd_ul` share it.
- `internal/cliout` (new) provides a common `Reporter[T]` shape so the six
  internal reporters stop redeclaring near-identical `FormatJSON` envelopes.
- `gochecksumtype` runs clean on every `switch` over an introduced enum.

## Non-goals

- **No new commands, flags, or output formats.** `--output text|json|markdown`
  stays exactly three variants. Help text wording stays identical unless a
  Cobra-required field forces a one-line edit (recorded in `delivery.md`).
- **No public-API rename** to packages outside `apps/rhino-cli/...`. Importers
  outside this module (none today, but the Go module path is public) must keep
  working.
- **No coverage-target reduction.** `test:quick` stays ≥90% and pre-push gates
  unchanged.
- **No upgrade of go.mod toolchain or cobra major version.** Mechanical refactor
  only.

## Approach

Five sequenced phases, each ending in a green `nx run rhino-cli:test:quick`,
`nx run rhino-cli:test:integration`, and `nx run rhino-cli:lint`:

1. **Phase 0** — Golden-output safety net. Capture pre-refactor stdout/stderr
   for every command in every output mode as fixtures; new test asserts the
   refactored build matches byte-for-byte.
2. **Phase 1** — Extract shared `cliout` (output dispatcher) and `severity`
   (sealed-interface enum + parser). Cmd-side and internal-side adopters
   converge to the new APIs.
3. **Phase 2** — Adopt sealed-interface enums for the remaining stringly-typed
   sites: `Relationship.Kind`, `Relationship.Role`, `bcregistry`/`glossary`
   severity, `agents` validation status, `docs/links` link kind.
4. **Phase 3** — DRY the `ddd_bc` / `ddd_ul` pair behind a shared `dddRunner`
   helper. Same for the agent-validation cmd cluster.
5. **Phase 4** — Reporter consolidation. Single generic `cliout.Reporter[T]`
   replaces six near-identical `Format{Text,JSON,Markdown}` triplets where the
   shape genuinely fits; reporters that diverge (per-file coverage breakdowns,
   doctor symbols) stay specialised but use a shared JSON envelope.

## Documents

- [brd.md](./brd.md) — business rationale (why this matters, who benefits)
- [prd.md](./prd.md) — product requirements + Gherkin acceptance criteria
- [tech-docs.md](./tech-docs.md) — architecture, naming, sumtype-decl recipe
- [delivery.md](./delivery.md) — TDD-shaped step-by-step checklist

## Behaviour-preservation invariant

This plan is a **pure refactor**. Every step must keep the following identical
to the baseline (captured in Phase 0):

- Stdout / stderr for every `rhino-cli <cmd> [...flags]` invocation across all
  three `--output` values, both `--verbose` and `--quiet`.
- Exit code for every invocation including failure paths.
- File outputs from `agents sync`, `env backup`, `env restore`,
  `test-coverage merge` byte-for-byte.
- JSON / Markdown payload schemas (field names, ordering, types).

Any drift caught by Phase 0 golden tests is a stop-the-line bug; revert and
fix before continuing.
