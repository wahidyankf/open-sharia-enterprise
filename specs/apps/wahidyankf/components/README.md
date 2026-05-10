# wahidyankf-web — Components (C4 L3)

Audience: Engineers, Technical Product/Project Managers

Component-level specifications for wahidyankf-web — what lives inside the `web` container,
sliced by bounded context. DDD bounded-context registry, ubiquitous-language glossaries, and
the bounded-context map live one level above at `../ddd/` because the ubiquitous language
belongs to the bounded context, not to one implementation surface.

## Children

- `web/` — Frontend (Next.js 16) component specs.
  - `README.md` — per-BC component overview.
  - `component-web.md` — Mermaid C4 L3 diagram (planned).

## Related

- [`../ddd/`](../ddd/README.md) — DDD artifacts (registry + glossaries + map)
- [`../system-context/`](../system-context/README.md) — C4 L1
- [`../containers/`](../containers/README.md) — C4 L2
- [`../behavior/`](../behavior/README.md) — Gherkin scenarios that exercise the components
