# AyoKoding — Behavior

Audience: Engineers, Technical Product/Project Managers

Behavior specifications for AyoKoding — Gherkin scenarios that exercise the product through
two perspectives: the tRPC HTTP API surface (`api`) and the browser UI surface (`web`).
Sliced by perspective so each test runner can wire its step implementations against the
right glob.

## Slug-vs-container distinction

AyoKoding ships **one deployable container**: `web` (Next.js 16). The Gherkin behavior tree
splits along **API perspective**, not deployable-container boundary:

- `web/` — UI-semantic scenarios (DOM, navigation, accessibility, locale switcher).
- `api/` — tRPC HTTP-semantic scenarios (procedure shapes, error codes, locale-scoped
  responses).

The slug `api` is a **perspective slug**, not a container. There is no separate API
container — tRPC procedures execute inside the same `web` container's Next.js server. The
slug exists so specs can talk about API contract behavior without conflating it with UI
behavior. The `organiclever` peer keeps the legacy slug `be` because `organiclever-be` is
a real F#/Giraffe container; ayokoding does not have one.

## Children

- `web/gherkin/` — Browser UI Gherkin scenarios (UI semantic). Moved here from legacy
  flat-root `web/gherkin/`.
- `api/gherkin/` — tRPC API Gherkin scenarios (HTTP semantic). Moved here from legacy
  flat-root `be/gherkin/` (slug rename `be` → `api`).

## Perspectives

| Perspective | Background                 | Step style                                     | Consumed by                              |
| ----------- | -------------------------- | ---------------------------------------------- | ---------------------------------------- |
| `web`       | `Given the app is running` | `clicks`, `types`, `sees`, `navigates`         | `apps/ayokoding-web-fe-e2e` (Playwright) |
| `api`       | `Given the API is running` | `the client calls`, response shape, error code | `apps/ayokoding-web-be-e2e` (Playwright) |

## Gherkin coverage

### `web/gherkin/` — UI perspective

Organized by bounded context (one folder per BC, matching the
[DDD registry](../ddd/bounded-contexts.yaml)).

| Bounded Context | Features                      | Count |
| --------------- | ----------------------------- | ----- |
| app-shell       | `responsive`, `accessibility` | 2     |
| content         | `content-rendering`           | 1     |
| search          | `search`                      | 1     |
| i18n            | `i18n`                        | 1     |
| navigation      | `navigation`                  | 1     |
| **Total**       |                               | **6** |

### `api/gherkin/` — tRPC API perspective

| Bounded Context | Features         | Count |
| --------------- | ---------------- | ----- |
| content         | `content-api`    | 1     |
| search          | `search-api`     | 1     |
| navigation      | `navigation-api` | 1     |
| i18n            | `i18n-api`       | 1     |
| health          | `health-check`   | 1     |
| **Total**       |                  | **5** |

## Related

- [`../components/`](../components/README.md) — C4 L3 components that the scenarios exercise
- [`../ddd/`](../ddd/README.md) — DDD registry + glossaries that own the vocabulary
