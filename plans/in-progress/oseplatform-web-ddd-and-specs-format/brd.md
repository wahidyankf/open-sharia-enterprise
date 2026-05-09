# BRD — oseplatform-web DDD + New Specs Format

## Why this work matters

`oseplatform-web` is the primary marketing + content surface for the platform. It is the most complex of the three Phase-1 web apps (7 bounded contexts vs 5 for wahidyankf and 6 for ayokoding) because it carries both UI and tRPC server logic in a single Next.js deployment.

Today the gap is significant:

- The `c4/` folder pre-dates the canonical five-folder C4 split. Readers cannot follow `system-context → containers → components` because the documents sit flat next to each other.
- The `be/gherkin/` slug describes tRPC HTTP scenarios, but `be` implies a separate deployable backend container — which oseplatform does not have. New contributors look for `apps/oseplatform-be/` (which doesn't exist) and stall.
- No `bounded-contexts.yaml`, no glossaries: `rhino-cli ddd bc oseplatform` cannot run, so structural drift between specs and `src/server/` is invisible.
- `src/server/` mixes content-retrieval, RSS, search, SEO, and health routers in flat files. A change to one breaks others silently.

## Business value

1. **Refactor confidence**: bringing oseplatform-web to the same DDD invariants as `organiclever` means renames in `src/server/<bc>/` break the build immediately, not silently. Every glossary identifier is grep-validated.
2. **Cross-app consistency**: `organiclever` and the three Phase-1 web apps end up sharing one mental model — same C4 split, same DDD shape. Onboarding cost drops sharply when a contributor reads any one app's specs and recognizes the pattern.
3. **Slug-vs-container clarity**: renaming `be` → `api` removes the confusion that arises whenever a contributor familiar with `organiclever` (where `be` is a separate F#/Giraffe container) reads `oseplatform/` (where `be` was a tRPC perspective inside the same Next.js). `api` honestly names the perspective.
4. **Plan 4 unblocker**: `bdd-ddd-tooling-gap-fill` allowlists this app once it ships the new format.

## Why now

- The `organiclever` reference is stable. The longer oseplatform stays on the legacy mixed layout, the more downstream documentation, agent definitions, and Plan 4 tooling work assumes a single layout shape that doesn't actually hold.
- The slug rename is a small but irreversible cost. Doing it now while the spec tree is small and audience is internal is far cheaper than after external readers (potential employers, conference talks) link to specific paths.

## Cost

- Production code refactor in `apps/oseplatform-web/src/`: split `src/server/` per-BC into `application/` + `infrastructure/`; move `src/components/` + `src/app/` into `src/contexts/<bc>/presentation/`. All imports updated mechanically.
- New ~1,400 lines of glossary content across 7 BCs.
- New `bounded-contexts.yaml` (~140 lines).
- Update `oseplatform-web/project.json` to invoke `ddd bc/ul` + `spec-coverage` (per perspective).
- Reshape `c4/` into `system-context/`, `containers/`, `components/{web,api}/` folders — preserving git history via `git mv`.
- Update `apps/oseplatform-web/README.md` to point at the new spec layout.
- Slug rename `behavior/be/gherkin/` → `behavior/api/gherkin/`. Coordinate with E2E step file globs (separate plan; this plan only flags any breakage).

## Risk

**Medium.** Not because the refactor is unusually fragile — it is closed-system, like wahidyankf — but because of two distinct concerns:

1. **tRPC router split** is real production-code reorganization. A flat `src/server/router.ts` becomes seven per-BC routers stitched together at the app router. If the wiring breaks, content rendering breaks. Mitigated by phased TDD: split one BC at a time, run BE-E2E after each.
2. **Slug rename `be` → `api`** breaks any external link, search-engine cache, or hand-rolled script that referenced `behavior/be/gherkin/`. Mitigated by adding a one-time note in `apps/oseplatform-web/README.md` and (optionally) a redirect file in the legacy path during a deprecation window. Since oseplatform's contributor surface is small, the deprecation window can be zero in practice.

## Success Metrics

- `[Judgment call]` `rhino-cli ddd bc oseplatform` exits 0 with "0 finding(s)" after the refactor lands on `main`.
- `[Judgment call]` `rhino-cli ddd ul oseplatform` exits 0 with "0 finding(s)" after the refactor lands on `main`.
- `[Judgment call]` `nx run oseplatform-web:spec-coverage` reports 0 step gaps across both perspectives (`web` and `api`) for all 7 bounded contexts.
- `[Judgment call]` `bdd-ddd-tooling-gap-fill` can include `oseplatform` in the allowlist gate at plan-4 day-1 without requiring any additional structural changes.
- `[Judgment call]` 0 DDD drift reported by `ddd bc/ul` in the cycle between this plan's archival and the next PR that touches `apps/oseplatform-web/src/`.

## Stakeholders

Single maintainer (the platform owner). External readership is low-volume content consumers (RSS subscribers, search-engine crawlers) who do not interact with the spec tree directly. No external API contract to honour.

## Out of scope (deferred)

- E2E test step file reorganization (`apps/oseplatform-web-{be,fe}-e2e/`).
- `oseplatform-cli` adoption of the new format.
- Migrating oseplatform to the new pre-push allowlist — that is plan 4's job.
- Splitting tRPC into a separate deployable container — would invalidate the slug-vs-container note; not on the roadmap.
