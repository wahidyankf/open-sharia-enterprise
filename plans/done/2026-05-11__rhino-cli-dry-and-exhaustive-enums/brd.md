# Business Requirements — rhino-cli DRY + Exhaustive Enum Typing

## Why this matters

`rhino-cli` is the repository's load-bearing automation surface. Every
pre-commit, pre-push, and CI quality gate routes through it: `docs
validate-links`, `agents sync`, `agents validate-claude`, `agents
validate-sync`, `test-coverage validate`, `spec-coverage validate`, `doctor`,
`docs validate-mermaid`, `ddd bc`, `ddd ul`, `repo-governance vendor-audit`,
`specs validate-*`, `workflows validate-naming`, `env backup/restore`,
`git pre-commit`.

A bug in `rhino-cli` blocks every push from every contributor and every
quality gate on every PR. Reliability is a contract; refactor risk is real.

Two forces have been pulling the codebase toward future incidents:

### Force 1 — Copy-paste duplication

`cmd/ddd_bc.go` and `cmd/ddd_ul.go` are 90%+ identical: same severity
resolution (`resolveBcSeverity` ↔ `resolveUlSeverity`), same env-var lookup,
same finding-print loop, same exit-code logic. Anyone fixing a bug in one
must remember to fix it in the other. A recent change to one without the
other would silently desync the two commands' behaviour.

Six commands (`doctor`, `env backup`, `env restore`, `test-coverage merge`,
`test-coverage diff`, `test-coverage validate`, `spec-coverage validate`,
`docs validate-mermaid`) wire up the same `outputFuncs{text,json,markdown}`

- `writeFormatted` block — and six internal packages each implement a
  near-identical `FormatText` / `FormatJSON` / `FormatMarkdown` trio.

### Force 2 — Stringly-typed enums

Plain-string enum fields (`Severity string`, `Kind string`, `Role string`,
`--output string`) sidestep Go's type system. A typo in a `switch` default
arm becomes silent fallback. Adding a new severity tomorrow requires
hunting every site by `grep`.

The repo **already invested** in the fix — three internal packages
(`testcoverage`, `doctor`, `mermaid`) use the sealed-interface +
`//sumtype:decl` pattern, and `gochecksumtype` is enabled in
`.golangci.yml`. The pattern is proven, the linter is paid for, but most
of the codebase hasn't adopted it. This plan finishes the migration.

## Who benefits

- **Contributors** — fixing one severity bug fixes both `ddd bc` and
  `ddd ul`. Adding a new output format touches one file, not eight.
- **Reviewers** — adding a new severity variant fails compilation in every
  switch arm that doesn't handle it. Code review can trust the linter.
- **CI** — fewer near-duplicate test files; faster `test:quick`.
- **Operators** — confidence that quality-gate behaviour is consistent
  across commands that share semantics.

## Success criteria

1. Pre/post refactor stdout, stderr, exit code identical for every command
   in every `--output` mode (golden-test enforced).
2. `nx run rhino-cli:test:quick` ≥90% coverage threshold still passing.
3. `nx run rhino-cli:lint` passes including `gochecksumtype` linter on every
   newly introduced sealed interface.
4. Cmd-file lines deleted ≥ cmd-file lines added (net reduction).
5. Every CLI flag value that accepts a fixed set (`--severity warn|error`,
   `--output text|json|markdown`, `doctor --scope full|minimal`) maps to a
   sealed interface enum with an exhaustively-checked parser.

## Out of scope

- Adding new CLI commands, flags, or output formats.
- Performance optimisation (refactor only; cycles spent must come from
  duplication removal, not algorithmic changes).
- Cross-app refactor (e.g., teaching `oseplatform-cli` or `ayokoding-cli` to
  share rhino-cli internals). Future plan.
- Migration of `libs/golang-commons` patterns. Touch only if extraction is
  the cleanest landing site for a shared primitive.

## Risk

| Risk                                                   | Mitigation                                                                                                                                                          |
| ------------------------------------------------------ | ------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| Behavioural drift slips past unit tests                | Phase 0 captures golden stdout/stderr/exit-code fixtures BEFORE any refactor; every later phase asserts byte-identical match.                                       |
| Sealed-interface enums break external Go importers     | rhino-cli `internal/` packages are import-restricted by `internal/`; no external imports possible. Public types in `cmd/` are not imported from outside the binary. |
| `gochecksumtype` produces false positives              | Pattern already in tree; existing usage proves clean. New adopters follow same shape.                                                                               |
| Phase 4 (reporter consolidation) over-abstracts        | Hard rule: if `Reporter[T]` adds indirection without removing ≥30 lines of duplication, keep the specialised reporter.                                              |
| Concurrent work on `rhino-cli` from other contributors | Use subrepo worktree per Worktree Workflow Standard 11. Land each phase in a small commit set; rebase frequently.                                                   |
