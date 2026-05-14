# OSE Application — Behavior

Behavior specifications for OSE Application — Gherkin scenarios exercising the product through
the backend HTTP surface and the frontend UI surface.

## Children

- `be/` — Backend Gherkin scenarios (HTTP semantic)
- `web/` — Frontend Gherkin scenarios (UI semantic)

## Containers

| Container | Perspective                             | Background                 | Consumed by                     |
| --------- | --------------------------------------- | -------------------------- | ------------------------------- |
| `be`      | HTTP-semantic (GET, POST, status codes) | `Given the API is running` | `apps/ose-app-be` (F#/Giraffe)  |
| `web`     | UI-semantic (clicks, types, sees)       | `Given the app is running` | `apps/ose-app-web` (Next.js 16) |

## Gherkin coverage

See [be/gherkin/](./be/gherkin/README.md) and [web/gherkin/](./web/gherkin/README.md).
