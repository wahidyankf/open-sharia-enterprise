# OrganicLever C4 Diagrams

C4 architecture diagrams for the OrganicLever fullstack application (frontend + backend).

## Diagrams

| Level     | File              | What It Shows                                                          |
| --------- | ----------------- | ---------------------------------------------------------------------- |
| Context   | `context.md`      | The system and its two external actors                                 |
| Container | `container.md`    | Runtime containers: Next.js frontend, F#/Giraffe backend, PGlite store |
| Component | `component-be.md` | F#/Giraffe REST API internals: health handler                          |
| Component | `component-web.md` | Next.js frontend internals: 9 DDD bounded contexts                     |

## C4 Level Summary

- **Context** — answers: who uses the system and how?
- **Container** — answers: what processes run and what data stores exist?
- **Component (BE)** — answers: what are the logical building blocks inside the REST API?
- **Component (FE)** — answers: what are the logical building blocks inside the Next.js app?

## Gherkin Specifications

All implementations consume shared Gherkin feature files. Backend and frontend
have separate spec trees with different domain coverage.

### Backend Gherkin

**Location**: [`specs/apps/organiclever/be/gherkin/`](../be/gherkin/README.md)

| Domain | Feature                       | Scenarios |
| ------ | ----------------------------- | --------- |
| health | `health/health-check.feature` | 2         |

### Frontend Gherkin

**Location**: [`specs/apps/organiclever/web/gherkin/`](../web/gherkin/README.md) — organized by
bounded context (one folder per context, matching the
[DDD registry](../ddd/bounded-contexts.yaml)).

| Bounded Context | Features                                                | Count |
| --------------- | ------------------------------------------------------- | ----- |
| app-shell       | `accessibility`, `entry-loggers`, `navigation`          | 3     |
| health          | `system-status-be`                                      | 1     |
| journal         | `home-screen`, `journal-mechanism`                      | 2     |
| landing         | `landing`                                               | 1     |
| routine         | `routine-management`                                    | 1     |
| routing         | `app-routes`, `disabled-routes`                         | 2     |
| settings        | `dark-mode`, `language`, `settings-screen`              | 3     |
| stats           | `history-screen`, `progress-screen`                     | 2     |
| workout-session | `workout-session`                                       | 1     |
| **Total**       |                                                         | **16**|

## Related

- **Parent**: [organiclever specs](../README.md)
- **DDD registry**: [`specs/apps/organiclever/ddd/`](../ddd/README.md)
- **Backend gherkin specs**: [be/gherkin/](../be/gherkin/README.md)
- **Frontend gherkin specs**: [web/gherkin/](../web/gherkin/README.md)
