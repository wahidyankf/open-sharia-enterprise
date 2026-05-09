# BRD — wahidyankf-web DDD + New Specs Format

## Why this work matters

`wahidyankf-web` is the personal portfolio site. It is the smallest of the three Phase-1 web apps that need the new specs format — making it the right pilot before applying the same shape to `oseplatform-web` and `ayokoding-web`.

Today the gap is silent:

- A reader of `specs/apps/wahidyankf/` cannot tell from the tree which scenarios exercise which part of the source code.
- A developer renaming a CV component cannot rely on tooling to flag stale glossary entries — there is no glossary.
- The per-bounded-context Gherkin coverage gate that `organiclever` uses (`spec-coverage` walking `behavior/web/gherkin/<bc>/`) cannot be wired without first creating the `<bc>/` folders.

## Business value

1. **Refactor confidence**: bringing wahidyankf-web up to the same DDD invariants as `organiclever` means renames and moves break the build immediately, not silently.
2. **Onboarding clarity**: a new contributor reads `specs/apps/wahidyankf/README.md`, follows the same five-folder C4 tree they saw in `organiclever`, and knows where to look. Cross-app consistency is the value, not novelty.
3. **Plan 4 unblocker**: `bdd-ddd-tooling-gap-fill` wires `specs validate-adoption` and `validate-tree` into pre-push for an allowlist of web apps. Until wahidyankf is on the new format, the allowlist must skip it.

## Why now

- `organiclever`'s tree is the reference. The longer wahidyankf, oseplatform, and ayokoding stay on the legacy flat layout, the more documentation, agent definitions, and tooling assume a single layout shape that doesn't actually exist everywhere.
- Plan 4 fixes apply across all four apps. Doing each web app's adoption first, then a single tooling-fixes plan, avoids interleaving production-code refactor with validator changes — much easier to debug.

## Cost

- Production code refactor in `apps/wahidyankf-web/src/` (move existing components into `src/contexts/<bc>/<layer>/`). All imports updated mechanically.
- New ~700 lines of glossary content across 5 BCs.
- New `bounded-contexts.yaml` (~80 lines).
- One pass updating `wahidyankf-web/project.json` to invoke `ddd bc/ul` + `spec-coverage` in `test:quick`.
- One pass updating `apps/wahidyankf-web/README.md` to link to the new spec layout.

## Risk

**Low.** wahidyankf-web has:

- no tRPC server,
- no shared kernel with other apps,
- no consumers of its internals beyond its own routes,
- a small surface (one portfolio site, three pages plus chrome).

The refactor is closed-system. The biggest concrete risk is import path churn breaking `nx run wahidyankf-web:test:quick`; mitigated by phased TDD delivery (refactor one BC at a time, run tests after each phase).

## Stakeholders

Single maintainer (the site owner). No external contracts to honour. No coordinated downstream consumer.

## Out of scope (deferred)

- E2E test step file reorganization (`apps/wahidyankf-web-fe-e2e/`).
- Migrating wahidyankf to the same governance allowlist that plan 4 will introduce — that is plan 4's job.
- Adding `behavior/api/gherkin/` — wahidyankf-web has no tRPC today; if a server side appears later, that goes in a new plan.
