# OSE Platform Web — Components (C4 L3)

Audience: Engineers, Technical Product/Project Managers

Component-level specifications for `ose-web` — what lives inside the single `web`
container, sliced by **perspective** (UI vs tRPC HTTP). DDD bounded-context registry,
ubiquitous-language glossaries, and bounded-context map live one level above at
[`../ddd/`](../ddd/README.md) because the ubiquitous language belongs to the bounded context,
not to one perspective.

## Children

- `web/` — UI perspective (Next.js client + Server Components) component specs.
  - `component-web.md` — moved from `c4/` during DDD-format adoption.
  - `README.md` — perspective overview.
- `api/` — tRPC HTTP perspective (Next.js server + tRPC procedures + route handlers) component specs.
  - `component-api.md` — moved from `c4/component-be.md` during DDD-format adoption (slug rename `be → api`).
  - `README.md` — perspective overview.

## Related

- [`../ddd/`](../ddd/README.md) — DDD artifacts (registry + glossaries + map) consumed by `rhino-cli ddd`
- [`../system-context/`](../system-context/README.md) — C4 L1
- [`../containers/`](../containers/README.md) — C4 L2 (single `web` container; slug-vs-container note)
- [`../behavior/`](../behavior/README.md) — Gherkin scenarios that exercise the components
