# AyoKoding — Containers (C4 L2)

Audience: Engineers, Technical Product/Project Managers

Container-level specifications for AyoKoding — the deployable units and how they fit
together. This is C4 Level 2.

AyoKoding ships **one deployable container**: `web` (Next.js 16 application that includes
both the SSR/SSG server tier and the in-browser client tier). The Gherkin behavior tree splits
along **API perspective** (`web` UI-semantic vs `api` tRPC HTTP-semantic), not along container
boundaries. See `container.md` for the slug-vs-container distinction.

Two legacy slugs are preserved unchanged and out of scope for this plan: `cli` (owned by the
separate `ayokoding-cli` Go binary) and `build-tools` (build-time index-generation scripts).

## Children

- `container.md` — moved here from the legacy `c4/` folder.

## Related

- [`../system-context/`](../system-context/README.md) — C4 L1
- [`../components/`](../components/README.md) — C4 L3 zoom into per-perspective internals
