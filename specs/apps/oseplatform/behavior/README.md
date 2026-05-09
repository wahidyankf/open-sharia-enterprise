# OSE Platform Web — Behavior

Audience: Engineers, Technical Product/Project Managers

Behavior specifications for `oseplatform-web` — Gherkin scenarios that exercise the single
`web` container through two perspectives. Sliced by perspective so each project (FE-E2E,
BE-E2E) wires step implementations against the right glob.

## Slug-vs-container distinction

`oseplatform-web` deploys as ONE container (`web`). The tRPC API runs **inside** that same
Next.js process — no separate backend deployable. Behaviour is documented from two
**perspectives** (UI and tRPC HTTP), captured as Gherkin slugs `web` and `api`. The slugs
do **not** map to separate containers; they slice behaviour by audience (visitor vs API
caller). See [`../containers/container.md`](../containers/container.md) for the full note.

## Children

- `web/` — UI-semantic scenarios. Migrated here from legacy `web/gherkin/` during
  DDD-format adoption.
- `api/` — tRPC HTTP-semantic scenarios. Migrated here from legacy `be/gherkin/` (slug
  rename `be → api`) during DDD-format adoption.

## Perspectives

| Perspective | Background                 | Step style                                       | Consumed by                                          |
| ----------- | -------------------------- | ------------------------------------------------ | ---------------------------------------------------- |
| `web`       | `Given the app is running` | `clicks`, `types`, `sees`, `navigates`           | `apps/oseplatform-web-fe-e2e` (Playwright)           |
| `api`       | `Given the API is running` | `sends GET/POST`, `status code`, `response body` | `apps/oseplatform-web-be-e2e` (Playwright tRPC HTTP) |

## Gherkin coverage

### `web/gherkin/`

Organized per bounded context (one folder per BC, matching the
[DDD registry](../ddd/bounded-contexts.yaml)).

| Bounded Context | Features                                             | Count |
| --------------- | ---------------------------------------------------- | ----- |
| app-shell       | `accessibility`, `navigation`, `responsive`, `theme` | 4     |
| landing         | `landing-page`                                       | 1     |
| **Total**       |                                                      | **5** |

### `api/gherkin/`

Organized per bounded context.

| Bounded Context | Feature             | Count |
| --------------- | ------------------- | ----- |
| content         | `content-retrieval` | 1     |
| search          | `search`            | 1     |
| rss-feed        | `rss-feed`          | 1     |
| seo             | `seo`               | 1     |
| health          | `health`            | 1     |
| **Total**       |                     | **5** |

## Related

- [`../components/`](../components/README.md) — C4 L3 components that the scenarios exercise
- [`../ddd/`](../ddd/README.md) — DDD bounded-context registry, glossaries, BC map
- [`../containers/`](../containers/README.md) — single `web` container, slug-vs-container note
