# Context Diagram: OrganicLever

Level 1 of the C4 model. Shows the OrganicLever system as a single boundary with two external
actors. The system contains both the Next.js frontend — a local-first life-journal app
(workout, reading, learning, meal, focus tracking) backed by in-browser PGlite storage — and
the F#/Giraffe backend REST API (health endpoint only in v0; productivity API surface deferred).
v0 has no authenticated screens.

```mermaid
%% Color Palette: Blue #0173B2 | Orange #DE8F05 | Teal #029E73 | Purple #CC78BC | Brown #CA9161 | Gray #808080
graph TD
    EU("End User<br/>──────────────────<br/>Browse landing page<br/>Open the local-first app<br/><br/>Desktop, Mobile"):::actor

    OPS("Operations Engineer<br/>──────────────────<br/>Health monitoring"):::actor_ops

    SYSTEM["OrganicLever<br/>──────────────────────<br/>Local-first life journal<br/><br/>Landing + life-journal app (PGlite)<br/>System-status diagnostics<br/>Backend health endpoint"]:::system

    CI("CI Pipeline<br/>──────────────────<br/>Main CI: test:quick<br/>E2E: Playwright<br/>PR Quality Gate"):::ci

    EU -->|"browse and interact"| SYSTEM
    OPS -->|"health check"| SYSTEM
    CI -->|"typecheck, lint, test"| SYSTEM

    classDef actor fill:#DE8F05,stroke:#000000,color:#000000,stroke-width:2px
    classDef actor_ops fill:#CC78BC,stroke:#000000,color:#000000,stroke-width:2px
    classDef system fill:#0173B2,stroke:#000000,color:#FFFFFF,stroke-width:3px
    classDef ci fill:#029E73,stroke:#000000,color:#FFFFFF,stroke-width:2px
```

The `specs/apps/organiclever/{be,web}/gherkin/` Gherkin features feed both the Main CI gate
(`test:quick`) and the cron-scheduled E2E pipeline. They are not modeled as a separate actor in
this diagram; see the [Container](./container.md) diagram for spec-to-container wiring.

## Related

- **Container diagram**: [container.md](./container.md)
- **Backend component diagram**: [component-be.md](./component-be.md)
- **Frontend component diagram**: [component-fe.md](./component-fe.md)
- **Parent**: [organiclever specs](../README.md)
