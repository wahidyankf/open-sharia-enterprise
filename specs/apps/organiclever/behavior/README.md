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

## Gherkin coverage

### Backend (`be/gherkin/`)

| Domain | Feature                       | Scenarios |
| ------ | ----------------------------- | --------- |
| health | `health/health-check.feature` | 2         |

### Frontend (`web/gherkin/`)

Organized by bounded context (one folder per context, matching the
[DDD registry](../components/web/ddd/bounded-contexts.yaml)).

| Bounded Context | Features                                       | Count  |
| --------------- | ---------------------------------------------- | ------ |
| app-shell       | `accessibility`, `entry-loggers`, `navigation` | 3      |
| health          | `system-status-be`                             | 1      |
| journal         | `home-screen`, `journal-mechanism`             | 2      |
| landing         | `landing`                                      | 1      |
| routine         | `routine-management`                           | 1      |
| routing         | `app-routes`, `disabled-routes`                | 2      |
| settings        | `dark-mode`, `language`, `settings-screen`     | 3      |
| stats           | `history-screen`, `progress-screen`            | 2      |
| workout-session | `workout-session`                              | 1      |
| **Total**       |                                                | **16** |

## Related

- [`../components/`](../components/README.md) — C4 L3 components that the scenarios exercise
- [`../containers/contracts/`](../containers/contracts/README.md) — OpenAPI contract the
  backend scenarios assert against (moved from legacy `contracts/` in Phase 2A.7)
