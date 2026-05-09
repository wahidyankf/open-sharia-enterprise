# OrganicLever — Behavior

Audience: Engineers, Technical Product/Project Managers

Behavior specifications for OrganicLever — Gherkin scenarios that exercise the product
through both the backend HTTP surface and the frontend UI surface. Sliced by surface so
each project can wire its step implementations against the right glob.

## Children

- `be/` — Backend Gherkin scenarios (HTTP semantic). Moved here from legacy
  flat-root `be/gherkin/` in Phase 2A.
- `web/` — Frontend Gherkin scenarios (UI semantic). Moved here from legacy
  flat-root `web/gherkin/` in Phase 2A.

## Surface split

| Aspect      | Backend (`be/`)                         | Frontend (`web/`)                    |
| ----------- | --------------------------------------- | ------------------------------------ |
| Perspective | HTTP-semantic (GET, POST, status codes) | UI-semantic (clicks, types, sees)    |
| Background  | `Given the API is running`              | `Given the app is running`           |
| Consumed by | `apps/organiclever-be` (F#/Giraffe)     | `apps/organiclever-web` (Next.js 16) |

## Related

- [`../components/`](../components/README.md) — C4 L3 components that the scenarios exercise
- `../containers/contracts/` — OpenAPI contract the backend scenarios assert against (available
  after Phase 2A.7 moves `contracts/` into `containers/`)
