# Demo Application C4 Diagrams

C4 architecture diagrams for the unified demo application (frontend + backend).

## Diagrams

| Level     | File              | What It Shows                                                                 |
| --------- | ----------------- | ----------------------------------------------------------------------------- |
| Context   | `context.md`      | The system and its four external actors                                       |
| Container | `container.md`    | Runtime containers: SPA, Static Server, REST API, Database, File Storage      |
| Component | `component-be.md` | REST API internals: handlers, middleware, services, repositories              |
| Component | `component-fe.md` | SPA internals: pages, shared components, state management, API client, guards |

## C4 Level Summary

- **Context** — answers: who uses the system and how?
- **Container** — answers: what processes run and what data stores exist?
- **Component (BE)** — answers: what are the logical building blocks inside the REST API?
- **Component (FE)** — answers: what are the logical building blocks inside the SPA?

## Implementations

The demo application is implemented in multiple languages and frameworks. All
implementations conform to the same API contract and Gherkin specifications.

### Backend Implementations (11)

| App                         | Language   | Framework    | CI Workflow                            |
| --------------------------- | ---------- | ------------ | -------------------------------------- |
| a-demo-be-golang-gin        | Go         | Gin          | `test-a-demo-be-golang-gin.yml`        |
| a-demo-be-java-springboot   | Java       | Spring Boot  | `test-a-demo-be-java-springboot.yml`   |
| a-demo-be-java-vertx        | Java       | Vert.x       | `test-a-demo-be-java-vertx.yml`        |
| a-demo-be-kotlin-ktor       | Kotlin     | Ktor         | `test-a-demo-be-kotlin-ktor.yml`       |
| a-demo-be-python-fastapi    | Python     | FastAPI      | `test-a-demo-be-python-fastapi.yml`    |
| a-demo-be-rust-axum         | Rust       | Axum         | `test-a-demo-be-rust-axum.yml`         |
| a-demo-be-ts-effect         | TypeScript | Effect       | `test-a-demo-be-ts-effect.yml`         |
| a-demo-be-fsharp-giraffe    | F#         | Giraffe      | `test-a-demo-be-fsharp-giraffe.yml`    |
| a-demo-be-csharp-aspnetcore | C#         | ASP.NET Core | `test-a-demo-be-csharp-aspnetcore.yml` |
| a-demo-be-clojure-pedestal  | Clojure    | Pedestal     | `test-a-demo-be-clojure-pedestal.yml`  |
| a-demo-be-elixir-phoenix    | Elixir     | Phoenix      | `test-a-demo-be-elixir-phoenix.yml`    |

### Frontend Implementations (3)

| App                         | Language   | Framework      | CI Workflow                            |
| --------------------------- | ---------- | -------------- | -------------------------------------- |
| a-demo-fe-ts-nextjs         | TypeScript | Next.js 16     | `test-a-demo-fe-ts-nextjs.yml`         |
| a-demo-fe-ts-tanstack-start | TypeScript | TanStack Start | `test-a-demo-fe-ts-tanstack-start.yml` |
| a-demo-fe-dart-flutterweb   | Dart       | Flutter Web    | `test-a-demo-fe-dart-flutterweb.yml`   |

### CI Workflows

- **Main CI** (`main-ci.yml`): Runs `typecheck`, `lint`, `test:quick` for all
  projects on push to `main`. Uploads coverage to Codecov.
- **Per-app E2E** (`test-a-demo-be-*.yml`, `test-a-demo-fe-*.yml`): Scheduled
  twice daily (6 AM / 6 PM WIB). Starts full stack via Docker Compose, runs
  Playwright E2E tests.
- **PR Quality Gate** (`pr-quality-gate.yml`): Runs `typecheck`, `lint`,
  `test:quick` for affected projects on pull requests.

## API Contract

All implementations conform to a single OpenAPI 3.1 specification:

- **Source**: [`specs/apps/a-demo/contracts/openapi.yaml`](../contracts/openapi.yaml)
- **Bundled**: `specs/apps/a-demo/contracts/generated/openapi-bundled.yaml` (generated)
- **Nx project**: `a-demo-contracts` (targets: `lint`, `bundle`, `docs`)
- **Codegen**: Each implementation generates types from the bundled spec via
  its `codegen` Nx target into `generated-contracts/`

## Gherkin Specifications

All implementations consume shared Gherkin feature files. Backend and frontend
have separate spec trees with different domain coverage.

### Backend Gherkin

**Location**: [`specs/apps/a-demo/be/gherkin/`](../be/gherkin/README.md)

| Domain           | Feature                    | Scenarios |
| ---------------- | -------------------------- | --------- |
| admin            | admin.feature              | 6         |
| authentication   | password-login.feature     | 5         |
| authentication   | token-lifecycle.feature    | 7         |
| expenses         | attachments.feature        | 10        |
| expenses         | currency-handling.feature  | 6         |
| expenses         | expense-management.feature | 7         |
| expenses         | reporting.feature          | 6         |
| expenses         | unit-handling.feature      | 4         |
| health           | health-check.feature       | 2         |
| security         | security.feature           | 5         |
| test-support     | test-api.feature           | 2         |
| token-management | tokens.feature             | 6         |
| user-lifecycle   | registration.feature       | 6         |
| user-lifecycle   | user-account.feature       | 6         |

### Frontend Gherkin

**Location**: [`specs/apps/a-demo/fe/gherkin/`](../fe/gherkin/README.md)

| Domain           | Feature                    | Scenarios |
| ---------------- | -------------------------- | --------- |
| admin            | admin-panel.feature        | 6         |
| authentication   | login.feature              | 5         |
| authentication   | session.feature            | 7         |
| expenses         | attachments.feature        | 10        |
| expenses         | currency-handling.feature  | 6         |
| expenses         | expense-management.feature | 7         |
| expenses         | reporting.feature          | 6         |
| expenses         | unit-handling.feature      | 4         |
| health           | health-status.feature      | 2         |
| layout           | accessibility.feature      | 6         |
| layout           | responsive.feature         | 10        |
| security         | security.feature           | 5         |
| token-management | tokens.feature             | 6         |
| user-lifecycle   | registration.feature       | 6         |
| user-lifecycle   | user-profile.feature       | 6         |

### Three-Level Testing Standard

All implementations follow the same three-level testing pattern:

| Level                            | Scope                             | Database        | HTTP | Gherkin |
| -------------------------------- | --------------------------------- | --------------- | ---- | ------- |
| Unit (`test:unit`)               | Service-layer calls, mocked repos | Mocked          | No   | Yes     |
| Integration (`test:integration`) | Service-layer calls, real DB      | Real PostgreSQL | No   | Yes     |
| E2E (`test:e2e`)                 | Full HTTP via Playwright          | Real PostgreSQL | Yes  | Yes     |

Coverage thresholds: backends >= 90%, frontends >= 70%.

## Related

- **Parent**: [demo specs](../README.md)
- **Backend gherkin specs**: [be/gherkin/](../be/gherkin/README.md)
- **Frontend gherkin specs**: [fe/gherkin/](../fe/gherkin/README.md)
- **API contract**: [contracts/](../contracts/openapi.yaml)
- **Project dependency graph**: [docs/reference/re\_\_project-dependency-graph.md](../../../../docs/reference/project-dependency-graph.md)
