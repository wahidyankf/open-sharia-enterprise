# OSE Platform Web — Containers (C4 L2)

Audience: Engineers, Technical Product/Project Managers

Container-level specifications for `ose-web` — the single deployable unit (`web`,
Next.js 16 + tRPC v11) and how it sits inside the system. This is C4 Level 2.

## Slug-vs-container distinction

`ose-web` deploys as ONE container (`web`). The tRPC API runs **inside** that same
Next.js process — there is no separate backend deployable.

Behavior is documented from two perspectives, named by Gherkin slugs `web` (UI semantic) and
`api` (tRPC HTTP semantic). Both perspectives describe the same single container; they slice
behaviour by audience, not by deployable. See [`container.md`](./container.md) for details.

## Children

- `container.md` — single-container diagram and the slug-vs-container note.

## Related

- [`../system-context/`](../system-context/README.md) — C4 L1
- [`../components/`](../components/README.md) — C4 L3 zoom into each perspective's internals
- [`../behavior/`](../behavior/README.md) — Gherkin scenarios from both `web` and `api` perspectives
