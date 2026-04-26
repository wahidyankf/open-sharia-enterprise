# Container Diagram: OrganicLever

Level 2 of the C4 model. Shows the runtime containers inside the OrganicLever system boundary:
the Next.js 16 frontend (landing site + system-status diagnostic page) and the F#/Giraffe backend
REST API (health endpoint only).

The frontend is a Next.js App Router application. v0 has no authenticated screens, no
client-side state machine, no remote sync — every productivity-tracking feature in the v0
storyboard lives in the user's browser via `localStorage`. The backend exposes only the health
endpoint; future work will add the productivity-tracking API surface.

```mermaid
%% Color Palette: Blue #0173B2 | Orange #DE8F05 | Teal #029E73 | Purple #CC78BC | Brown #CA9161 | Gray #808080
graph TD
    EU("End User<br/>Desktop / Mobile"):::actor
    OPS("Operations Engineer"):::actor_ops

    subgraph SYSTEM["OrganicLever"]
        FE["Next.js Frontend<br/>──────────────────<br/>Next.js 16, TypeScript<br/><br/>Landing page<br/>System status diagnostics<br/>Server-side rendering"]:::container_fe

        BE["F#/Giraffe Backend<br/>──────────────────<br/>F#, Giraffe<br/><br/>Health endpoint"]:::container_be
    end

    subgraph SPECS["Specifications"]
        BE_GHERKIN["Backend Gherkin<br/>──────────────────<br/>specs/apps/organiclever/be/gherkin/<br/><br/>See be/gherkin/README"]:::spec

        FE_GHERKIN["Frontend Gherkin<br/>──────────────────<br/>specs/apps/organiclever/fe/gherkin/<br/><br/>See fe/gherkin/README"]:::spec
    end

    subgraph CICD["CI Pipelines"]
        MAIN_CI["Main CI<br/>──────────────────<br/>typecheck, lint, test:quick<br/>On push to main"]:::ci

        E2E_CI["E2E CI<br/>──────────────────<br/>Docker Compose stack<br/>Playwright tests"]:::ci
    end

    EU -->|"HTTPS"| FE
    OPS -->|"health check"| BE

    FE -->|"system-status diagnostic<br/>HTTP/JSON"| BE

    BE_GHERKIN -->|"BDD scenarios"| BE
    FE_GHERKIN -->|"BDD scenarios"| FE

    MAIN_CI -->|"test all"| BE
    MAIN_CI -->|"test all"| FE
    E2E_CI -->|"full stack test"| BE
    E2E_CI -->|"full stack test"| FE

    classDef actor fill:#DE8F05,stroke:#000000,color:#000000,stroke-width:2px
    classDef actor_ops fill:#CC78BC,stroke:#000000,color:#000000,stroke-width:2px
    classDef container_fe fill:#0173B2,stroke:#000000,color:#FFFFFF,stroke-width:2px
    classDef container_be fill:#0173B2,stroke:#000000,color:#FFFFFF,stroke-width:2px
    classDef spec fill:#029E73,stroke:#000000,color:#FFFFFF,stroke-width:2px,stroke-dasharray:5 5
    classDef ci fill:#CC78BC,stroke:#000000,color:#000000,stroke-width:2px
```

## Container Implementations

### Backend

| App             | Language | Framework | Database | Coverage |
| --------------- | -------- | --------- | -------- | -------- |
| organiclever-be | F#       | Giraffe   | none     | >= 90%   |

### Frontend

| App              | Language   | Framework  | Coverage |
| ---------------- | ---------- | ---------- | -------- |
| organiclever-web | TypeScript | Next.js 16 | >= 70%   |

## Related

- **Context diagram**: [context.md](./context.md)
- **Backend component diagram**: [component-be.md](./component-be.md)
- **Frontend component diagram**: [component-fe.md](./component-fe.md)
- **Parent**: [organiclever specs](../README.md)
