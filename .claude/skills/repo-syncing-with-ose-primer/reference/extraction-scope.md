# Extraction Scope

Frozen list of paths the propagation-maker's `parity-check` mode enumerates. Established at Phase 7 of the 2026-04-18 ose-primer-separation plan. Future extraction exercises MUST add new scope documents rather than editing this one — this list is a fixed reference point for the 2026-04-18 extraction only.

## Scope

### Demo apps (17 directories under `apps/`)

1. `apps/a-demo-be-clojure-pedestal`
2. `apps/a-demo-be-csharp-aspnetcore`
3. `apps/a-demo-be-e2e`
4. `apps/a-demo-be-elixir-phoenix`
5. `apps/a-demo-be-fsharp-giraffe`
6. `apps/a-demo-be-golang-gin`
7. `apps/a-demo-be-java-springboot`
8. `apps/a-demo-be-java-vertx`
9. `apps/a-demo-be-kotlin-ktor`
10. `apps/a-demo-be-python-fastapi`
11. `apps/a-demo-be-rust-axum`
12. `apps/a-demo-be-ts-effect`
13. `apps/a-demo-fe-dart-flutterweb`
14. `apps/a-demo-fe-e2e`
15. `apps/a-demo-fe-ts-nextjs`
16. `apps/a-demo-fe-ts-tanstack-start`
17. `apps/a-demo-fs-ts-nextjs`

### Demo contract specs (1 directory under `specs/apps/`)

1. `specs/apps/a-demo`

## Out of scope (explicitly)

The parity check does NOT cover the following, even though they are affected by the extraction:

- `.github/workflows/test-a-demo-*.yml` — deleted alongside the apps; not content-bearing.
- `.github/actions/setup-{clojure,elixir,flutter,jvm,rust}/` — demo-only composite actions; deleted in Phase 8 Commit A.
- `codecov.yml` flags and ignore patterns keyed on demo paths — pruned inline in Phase 8 Commit E.
- `go.work` use directives for demo Go backends — pruned in Phase 8 Commit E.
- `open-sharia-enterprise.sln` project blocks for demo C# backend — pruned in Phase 8 Commit E.
- `docs/reference/demo-apps-ci-coverage.md` — deleted in Phase 8 Commit D; the narrative content has no counterpart in the primer.
- `libs/clojure-openapi-codegen`, `libs/elixir-cabbage`, `libs/elixir-gherkin`, `libs/elixir-openapi-codegen` — deleted in Phase 8 Commit I; primer retains these if needed.
- `apps/rhino-cli/cmd/java*.go`, `apps/rhino-cli/cmd/contracts*.go`, `apps/rhino-cli/internal/java/` — demo-only commands trimmed in Phase 8 Commit J; primer retains the fuller CLI.

The parity check focuses on whether the primer carries byte-equivalent or newer state for the 18 in-scope paths. Configuration cleanup and library removal follow afterward as their own commits and are not verified by parity.

## Verdict rule

For each in-scope path:

1. Compute a content hash (recursive content-only hash, ignoring mtime) for the `ose-public` version.
2. Compute a content hash for the `ose-primer` version at `$OSE_PRIMER_CLONE/` relative to origin/main.
3. Classify: `equal` (hashes match), `primer-newer` (primer has strictly more recent content-bearing commits touching this path), `public-newer` (public has strictly more recent content-bearing commits), or `missing-from-primer` (path absent in primer).

The overall verdict is `parity verified: ose-public may safely remove` if and only if every in-scope path classifies as `equal`, `primer-newer`, or (rarely) `missing-from-primer` combined with a documented decision to drop the path entirely. Any `public-newer` row triggers `parity NOT verified` and the catch-up loop.

## Invariants

- The scope list is frozen. Adding or removing paths from this list requires a convention amendment, not a plan update.
- The parity check is read-only; it never mutates either repo.
- A `missing-from-primer` verdict does NOT automatically fail parity; it requires an explicit operator decision recorded in the report.
