# OSE Platform Web — `api` perspective Gherkin

tRPC HTTP-semantic Gherkin scenarios for `ose-web`. Step style: `sends GET/POST`,
`status code`, `response body`. Background: `Given the API is running`. Consumed by
`apps/ose-web-be-e2e` (Playwright tRPC HTTP).

> **Slug note**: this perspective is `api`, not `be`. The tRPC layer runs **inside** the same
> Next.js process as the UI — there is no separate backend container. See
> [`../../../containers/container.md`](../../../containers/container.md) for the
> slug-vs-container distinction.

Organized per bounded context (one folder per BC, matching
[`../../../ddd/bounded-contexts.yaml`](../../../ddd/bounded-contexts.yaml)).

## Coverage

| Bounded Context | Folder      | Feature             | Scenarios |
| --------------- | ----------- | ------------------- | --------- |
| content         | `content/`  | `content-retrieval` | 1 file    |
| search          | `search/`   | `search`            | 1 file    |
| rss-feed        | `rss-feed/` | `rss-feed`          | 1 file    |
| seo             | `seo/`      | `seo`               | 1 file    |
| health          | `health/`   | `health`            | 1 file    |

## File naming

`[domain-capability].feature` (kebab-case). Stay HTTP-semantic — no UI clicks or browser
state. Refer to procedures by their tRPC procedure path (e.g. `content.getBySlug`) or by HTTP
path (`/feed.xml`, `/sitemap.xml`).

## Adding a feature file

1. Identify the bounded context (e.g. `content`, `health`).
2. Create the folder if it does not exist: `behavior/api/gherkin/<bc>/`.
3. Create the `.feature` file: `<domain-capability>.feature`.
4. Open with `Feature:` then a user story block.
5. Use `Given the API is running` as the first Background step.
6. Use only HTTP-semantic steps — no UI verbs.

## Related

- **Parent**: [`../../README.md`](../../README.md)
- **web perspective counterpart**: [`../../web/gherkin/`](../../web/gherkin/README.md)
- **DDD registry**: [`../../../ddd/bounded-contexts.yaml`](../../../ddd/bounded-contexts.yaml)
