# wahidyankf-web — System Context (C4 L1)

Audience: Engineers, Technical Product/Project Managers

System-context specifications for wahidyankf-web — actors, external systems, and the
boundary of what this site builds vs what it depends on. This is C4 Level 1.

## Actors

| Actor   | Role                                                                    |
| ------- | ----------------------------------------------------------------------- |
| Visitor | Any anonymous user who loads the portfolio site in a browser (no login) |

## External systems

| System       | Interaction                                              |
| ------------ | -------------------------------------------------------- |
| Vercel CDN   | Serves all static assets; handles HTTPS and edge caching |
| Google Fonts | Provides the Inter font loaded at build time             |

There is no external API, no database, and no authentication provider.

## Planned children

- `context.md` — Mermaid C4 L1 diagram. Actors: `Visitor`. External: `Vercel CDN`.

## Related

- [`../product/`](../product/README.md) — Product framing (above C4)
- [`../containers/`](../containers/README.md) — C4 L2 zoom into the single `web` container
- [`../components/`](../components/README.md) — C4 L3 zoom into per-container internals
