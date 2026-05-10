# wahidyankf-web — Containers (C4 L2)

Audience: Engineers, Technical Product/Project Managers

Container-level specifications for wahidyankf-web — the single deployable unit (`web`, a
Next.js 16 app on Vercel) and its runtime characteristics. This is C4 Level 2.

## Containers

| Container | Technology | Deployment          | Description                                      |
| --------- | ---------- | ------------------- | ------------------------------------------------ |
| `web`     | Next.js 16 | Vercel (static CDN) | All five bounded contexts served from one origin |

There is no backend container, no database container, and no message bus. All content is
static TypeScript modules bundled at build time.

## Planned children

- `container.md` — Mermaid C4 L2 diagram showing the single `web` container, the `Visitor`
  actor, the Vercel CDN edge, and no server-side services.

## Related

- [`../system-context/`](../system-context/README.md) — C4 L1
- [`../components/`](../components/README.md) — C4 L3 zoom into the `web` container's internals
