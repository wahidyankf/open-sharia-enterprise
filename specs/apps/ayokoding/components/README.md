# AyoKoding — Components (C4 L3)

Audience: Engineers, Technical Product/Project Managers

Component-level specifications for AyoKoding — what lives inside the `web` container,
sliced by **API perspective** (`web` UI-semantic, `api` tRPC HTTP-semantic).

The `web` and `api` slugs here are **perspective slugs**, not separate deployable containers.
Both perspectives execute inside the single `web` container's Next.js runtime. The split
exists so component-level diagrams can describe the UI surface and the tRPC API surface
without conflating them.

DDD bounded-context registry, ubiquitous-language glossaries, and bounded-context map live one
level above at [`../ddd/`](../ddd/README.md) because the ubiquitous language belongs to the
bounded context, not to one perspective surface.

## Children

- `web/` — Web (UI-semantic) component specs.
  - `component-web.md` — moved from `c4/`.
  - `README.md` — perspective-level intro (this folder's own README).
- `api/` — API (tRPC HTTP-semantic) component specs.
  - `component-api.md` — moved from `c4/component-be.md` and renamed (slug rename `be` → `api`).
  - `README.md` — perspective-level intro.

## Related

- [`../ddd/`](../ddd/README.md) — DDD artifacts (registry + glossaries + map) consumed by `rhino-cli ddd`
- [`../system-context/`](../system-context/README.md) — C4 L1
- [`../containers/`](../containers/README.md) — C4 L2
- [`../behavior/`](../behavior/README.md) — Gherkin scenarios that exercise the components
