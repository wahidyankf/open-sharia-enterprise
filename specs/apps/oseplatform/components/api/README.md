# OSE Platform Web — `api` perspective (tRPC HTTP)

Audience: Engineers, Technical Product/Project Managers

tRPC HTTP-semantic component specifications for the single `web` container. This is the **`api`
perspective** — what the tRPC layer returns to a caller (browser, Playwright, scripts).

> **Slug note**: this perspective is `api`, not `be`. The tRPC layer runs **inside** the same
> Next.js process as the UI — there is no separate backend container. See
> [`../../containers/container.md`](../../containers/container.md) for the slug-vs-container
> distinction.

## Children

- `component-api.md` — C4 L3 component diagram for the tRPC HTTP perspective. Router,
  procedures, route handlers, content services, search index, markdown pipeline.

## What this perspective covers

| Bounded Context | tRPC procedure(s) / Route handler(s)                            |
| --------------- | --------------------------------------------------------------- |
| `content`       | `content.getBySlug`, `content.listUpdates`                      |
| `search`        | `search.query`                                                  |
| `health`        | `health.check` (formerly `meta.health`)                         |
| `rss-feed`      | `/feed.xml` route handler                                       |
| `seo`           | `/sitemap.xml`, `/robots.txt` route handlers; metadata builders |

## Gherkin source

tRPC HTTP scenarios live under [`../../behavior/api/gherkin/`](../../behavior/api/gherkin/),
organized per bounded context.

## Related

- **Container diagram**: [`../../containers/container.md`](../../containers/container.md)
- **web perspective**: [`../web/`](../web/README.md)
- **DDD registry**: [`../../ddd/bounded-contexts.yaml`](../../ddd/bounded-contexts.yaml)
- **Parent**: [`../README.md`](../README.md)
