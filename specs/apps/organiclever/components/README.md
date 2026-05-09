# OrganicLever — Components (C4 L3)

Audience: Engineers, Technical Product/Project Managers

Component-level specifications for OrganicLever — what lives inside each container,
sliced by surface (backend, frontend) and including DDD (Domain-Driven Design — applied
to the frontend) bounded-context registry, ubiquitous-language glossaries, and
bounded-context map.

## Children

- `be/` — Backend (F#/Giraffe — F# language on the Giraffe HTTP framework) component specs.
  - `component-be.md` — moved from `c4/` in Phase 2A.
  - `README.md` — moved from legacy flat-root `be/README.md` in Phase 2A.
  - `api.md` — API endpoints, env vars, architecture tree. Authored in Phase 3.
- `web/` — Frontend (Next.js 16) component specs.
  - `component-web.md` — moved from `c4/` in Phase 2A.
  - `README.md` — moved from legacy flat-root `web/README.md` in Phase 2A.
  - `architecture.md` — bounded-context tree, layer rules. Authored in Phase 3.
  - `routes-and-screens.md` — routes/screens/entry-flows tables. Authored in Phase 3.
  - `design-system.md` — palette, typography, dark mode, tokens. Authored in Phase 3.
  - `ddd/` — DDD artifacts (registry + glossaries) consumed by `rhino-cli ddd`. Moved
    here from legacy flat-root `ddd/` in Phase 2A.

## Related

- [`../system-context/`](../system-context/README.md) — C4 L1
- [`../containers/`](../containers/README.md) — C4 L2
- [`../behavior/`](../behavior/README.md) — Gherkin scenarios that exercise the components
