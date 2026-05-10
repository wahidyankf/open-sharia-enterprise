# BRD: Consolidate CLI Specs Under `behavior/`

## Problem

`specs/apps/*/cli/` directories sit at the same level as `ddd/`, `components/`,
`containers/`, `product/`, and `system-context/`. These are different concern types:
`ddd/` is domain model docs, `components/` is C4 L3 diagrams, `behavior/` is Gherkin
behavioral contracts. `cli/` is also Gherkin behavioral contracts — but it lives outside
`behavior/`, inconsistent with `web/` and `api/` which live inside it.

The top-level `cli/` slot predates the DDD format adoption. It was never migrated when
`web/` and `api/` moved under `behavior/`.

## Business Value

**Architectural consistency.** All Gherkin feature files live under `behavior/`, organized
by interface perspective. No ambiguity about where new interfaces go. Engineers and agents
scanning `specs/apps/<name>/` find behavioral contracts in one place.

**Extensibility.** New interface surfaces (mobile API, GraphQL, webhook) have an obvious
home: `behavior/<surface>/`. No ad-hoc top-level directories required.

**rhino already started this.** `specs/apps/rhino/behavior/cli/` was created for the `specs`
subcommand. The top-level `cli/` is live legacy. This plan completes the migration.

## Success Criteria

- No `specs/apps/*/cli/` directories remain.
- All Gherkin specs accessible under `specs/apps/*/behavior/cli/`.
- All tests pass (`test:quick`, `test:integration`, `spec-coverage`) for all three CLIs.
- `behavior/README.md` for oseplatform and ayokoding documents the new `cli/` perspective.

## Risks

**rhino `behavior/cli/gherkin/specs/` edge case.** Four planned `validate-*.feature` files
already live at `behavior/cli/gherkin/specs/`. If `spec-coverage validate` recurses into
that subfolder after pointing at `behavior/cli/gherkin/`, it will check those planned files
for implementation coverage and fail. Mitigation: delivery step verifies this and resolves
before marking complete. See [tech-docs.md](./tech-docs.md).
