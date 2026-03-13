# Testing Standardization

## Overview

Standardize testing across **all apps and libs** in the monorepo with three simple rules, consistent target naming (`test:unit`, `test:integration`, `test:e2e`), and a uniform `test:quick` quality gate.

**Goal**: Every project in `apps/` and `libs/` follows a standardized testing strategy derived from three rules.

## Three Rules

1. **All apps and libs must have unit tests** (`test:unit`) — except Hugo/static site generator-based projects
2. **All apps must also have integration tests** (`test:integration`) — libs are exempt; Hugo/static sites are exempt
3. **All web apps (APIs and web UIs) must have E2E tests** (`test:e2e`) — CLIs and libs are exempt; Hugo/static sites are exempt

These rules determine the mandatory test levels for each project. Integration tests for demo-be backends run in Docker (docker-compose with PostgreSQL). For demo-be backends specifically, the same Gherkin specs (`specs/apps/demo-be/gherkin/`) serve as the contract at every level — only the step implementations change.

## Project Inventory

### Project Types and Applicable Test Levels

| Project                     | Type              | Rule  | Unit | Integration | E2E | Notes                                       |
| --------------------------- | ----------------- | ----- | ---- | ----------- | --- | ------------------------------------------- |
| `demo-be-java-springboot`   | App — API Backend | 1+2+3 | Yes  | Yes (PG)    | Yes | Gherkin at all 3 levels                     |
| `demo-be-elixir-phoenix`    | App — API Backend | 1+2+3 | Yes  | Yes (PG)    | Yes | Gherkin at all 3 levels                     |
| `demo-be-fsharp-giraffe`    | App — API Backend | 1+2+3 | Yes  | Yes (PG)    | Yes | Gherkin at all 3 levels                     |
| `demo-be-golang-gin`        | App — API Backend | 1+2+3 | Yes  | Yes (PG)    | Yes | Gherkin at all 3 levels                     |
| `demo-be-python-fastapi`    | App — API Backend | 1+2+3 | Yes  | Yes (PG)    | Yes | Gherkin at all 3 levels                     |
| `demo-be-rust-axum`         | App — API Backend | 1+2+3 | Yes  | Yes (PG)    | Yes | Gherkin at all 3 levels                     |
| `demo-be-kotlin-ktor`       | App — API Backend | 1+2+3 | Yes  | Yes (PG)    | Yes | Gherkin at all 3 levels                     |
| `demo-be-java-vertx`        | App — API Backend | 1+2+3 | Yes  | Yes (PG)    | Yes | Gherkin at all 3 levels                     |
| `demo-be-ts-effect`         | App — API Backend | 1+2+3 | Yes  | Yes (PG)    | Yes | Gherkin at all 3 levels                     |
| `demo-be-csharp-aspnetcore` | App — API Backend | 1+2+3 | Yes  | Yes (PG)    | Yes | Gherkin at all 3 levels                     |
| `demo-be-clojure-pedestal`  | App — API Backend | 1+2+3 | Yes  | Yes (PG)    | Yes | Gherkin at all 3 levels                     |
| `demo-be-e2e`               | App — E2E Runner  | —     | —    | —           | Yes | Shared Playwright suite for all backends    |
| `organiclever-web`          | App — Web UI      | 1+2+3 | Yes  | Yes (MSW)   | Yes | E2E via `organiclever-web-e2e`              |
| `organiclever-web-e2e`      | App — E2E Runner  | —     | —    | —           | Yes | Playwright E2E for organiclever-web         |
| `oseplatform-web`           | App — Hugo Site   | —     | —    | —           | —   | Build + link validation only (`test:quick`) |
| `ayokoding-web`             | App — Hugo Site   | —     | —    | —           | —   | Build + link validation only (`test:quick`) |
| `ayokoding-cli`             | App — CLI         | 1+2   | Yes  | Yes (BDD)   | —   | Godog BDD integration tests                 |
| `oseplatform-cli`           | App — CLI         | 1+2   | Yes  | Yes (BDD)   | —   | Godog BDD integration tests                 |
| `rhino-cli`                 | App — CLI         | 1+2   | Yes  | Yes (BDD)   | —   | Godog BDD integration tests                 |
| `golang-commons`            | Library (Go)      | 1     | Yes  | Optional    | —   | Has Godog BDD (not required by rules)       |
| `hugo-commons`              | Library (Go)      | 1     | Yes  | Optional    | —   | Has Godog BDD (not required by rules)       |
| `elixir-cabbage`            | Library (Elixir)  | 1     | Yes  | —           | —   | Unit tests only                             |
| `elixir-gherkin`            | Library (Elixir)  | 1     | Yes  | —           | —   | Unit tests only                             |

**Legend**: PG = PostgreSQL via docker-compose, MSW = Mock Service Worker (in-process), BDD = Godog/feature file integration tests (no database). **Rule** column shows which of the three rules apply.

## New Testing Standard

### Unit Tests (`test:unit`)

- **Infrastructure**: Mocks exclusively. No real databases, no HTTP calls, no API calls, no external services.
- **Entry point**: Call application code directly (service/handler/context functions). NOT through HTTP or Playwright.
- **Spec consumption** (demo-be only): MUST consume all corresponding Gherkin specs from `specs/apps/demo-be/gherkin/`. Gherkin step definitions call service/handler functions with mocked dependencies.
- **Additional scope**: Individual function tests (pure logic, validation, domain rules) that go beyond what Gherkin covers.
- **Applies to**: All projects except Hugo static sites and E2E runners.
- **Deterministic**: Yes. Fully cacheable.

### Integration Tests (`test:integration`)

Integration tests vary by project type:

#### Demo-be Backends (PostgreSQL via docker-compose)

- **Infrastructure**: Runs entirely inside Docker via a per-backend `docker-compose.integration.yml`. Spins up PostgreSQL + test runner containers. No HTTP calls, no API calls, no external service/API calls.
- **Entry point**: Call application code directly (service/handler/context functions). NOT through HTTP or Playwright.
- **Spec consumption**: MUST consume corresponding Gherkin specs. Focus on how components play together with a real database under the restrictions above.
- **Purpose**: Test the service in isolation — verify that business logic, persistence, and data access work correctly together using the same database engine as production.
- **Reproducibility**: The entire test environment is defined in `docker-compose.integration.yml` — anyone with Docker can run integration tests identically, regardless of host OS or installed toolchains.
- **Database lifecycle**: Each test run: (1) `docker compose up` starts PostgreSQL + test runner, (2) test runner waits for PostgreSQL readiness, (3) runs all migrations, (4) executes seed files if present, (5) runs tests, (6) `docker compose down -v` tears down everything including volumes.
- **Deterministic**: Yes (clean containers per run, no shared state). However, NOT cacheable — Docker builds may pull updated base images, and container execution is not tracked by Nx file-based caching.

#### organiclever-web (MSW)

- **Infrastructure**: Mock Service Worker (MSW) for API mocking. In-process, no Docker needed.
- **Entry point**: Component-level tests with mocked API boundaries.
- **Purpose**: Test React components and pages with realistic API responses.
- **Deterministic**: Yes. But NOT cacheable (integration tests are never cached).
- **Status**: Already compliant. No changes needed.

#### Go CLI Apps (BDD)

- **Infrastructure**: Godog BDD tests using feature files from `specs/`. May use tmpdir mocks, mock closures, or in-memory stores. No Docker, no external services.
- **Entry point**: Direct function/command calls via Godog step definitions.
- **Purpose**: Test command workflows and component interactions beyond what unit tests cover.
- **Deterministic**: Yes. But NOT cacheable (integration tests are never cached).
- **Status**: Already compliant. No changes needed for `ayokoding-cli`, `oseplatform-cli`, `rhino-cli`.

**Note on Go libraries**: `golang-commons` and `hugo-commons` already have Godog integration tests. These are retained but **not required** by the rules — libs only need unit tests (Rule 1).

### E2E Tests (`test:e2e`)

- **Infrastructure**: No restrictions. Real HTTP calls, real PostgreSQL database, real services.
- **Entry point**: Through API calls or UI. Uses Playwright.
- **Spec consumption** (demo-be only): MUST consume corresponding Gherkin specs.
- **Purpose**: Verify the fully assembled system works end-to-end.
- **Applies to**: `demo-be-e2e` (all 11 backends), `organiclever-web-e2e`.
- **Deterministic**: Depends on external services. NOT cacheable.

### Projects Without Tests

- **`oseplatform-web`**: Hugo static site. `test:quick` runs link validation only. No unit/integration/e2e.
- **`ayokoding-web`**: Hugo static site. `test:quick` runs link validation only. No unit/integration/e2e.

### Summary Table

| Aspect            | Unit             | Integration (demo-be)   | Integration (other) | E2E                     |
| ----------------- | ---------------- | ----------------------- | ------------------- | ----------------------- |
| **Database**      | Mocked           | Real (PostgreSQL)       | Mocked/in-memory    | Real (PostgreSQL)       |
| **HTTP calls**    | None             | None                    | None (MSW) or N/A   | Yes (Playwright)        |
| **External APIs** | Mocked           | Mocked                  | Mocked              | Real                    |
| **Entry point**   | Direct code call | Direct code call        | Direct code call    | HTTP/UI via Playwright  |
| **Gherkin specs** | Yes (demo-be)    | Yes (demo-be)           | N/A                 | Yes (demo-be)           |
| **Cacheable**     | Yes              | No                      | No                  | No                      |
| **Execution**     | Host (no Docker) | Docker (docker-compose) | Host (no Docker)    | Docker (docker-compose) |

## Current State Assessment

### Per-Project Current State

#### Demo-be Backends (11 projects)

All 11 demo-be backends currently follow the same pattern:

- **"Unit" tests**: Exist in some backends (F#, Go, TS). Test individual functions. Do NOT consume Gherkin specs.
- **"Integration" tests**: Consume Gherkin specs via BDD frameworks. Use **HTTP-level testing** (MockMvc, httptest, TestClient, ConnCase, etc.) with **in-memory stores** (no real database).
- **"E2E" tests**: Single shared Playwright suite (`apps/demo-be-e2e/`) consumes Gherkin specs via playwright-bdd. Makes real HTTP calls to running services.

| Backend                     | Unit Tests       | Integration: HTTP?          | Integration: Database?      | Integration: BDD Framework |
| --------------------------- | ---------------- | --------------------------- | --------------------------- | -------------------------- |
| `demo-be-java-springboot`   | None             | Yes (MockMvc)               | In-memory maps (no real DB) | Cucumber JVM               |
| `demo-be-elixir-phoenix`    | Controller tests | Yes (ConnCase)              | In-memory Agent stores      | Cabbage                    |
| `demo-be-fsharp-giraffe`    | Domain + handler | Yes (WebApplicationFactory) | SQLite in-memory            | TickSpec                   |
| `demo-be-golang-gin`        | Handler tests    | Yes (httptest.Server)       | In-memory maps              | Godog                      |
| `demo-be-python-fastapi`    | None             | Yes (TestClient)            | In-memory maps              | pytest-bdd                 |
| `demo-be-rust-axum`         | None             | Yes (Tower TestClient)      | In-memory maps              | cucumber-rs                |
| `demo-be-kotlin-ktor`       | None             | Yes (testApplication)       | SQLite in-memory            | Cucumber JVM               |
| `demo-be-java-vertx`        | None             | Yes (Vert.x WebClient)      | In-memory maps              | Cucumber JVM               |
| `demo-be-ts-effect`         | Function tests   | Yes (HTTP client to server) | In-memory maps              | Cucumber.js                |
| `demo-be-csharp-aspnetcore` | None             | Yes (WebApplicationFactory) | SQLite in-memory            | Reqnroll                   |
| `demo-be-clojure-pedestal`  | None             | Yes (clj-http)              | SQLite in-memory            | kaocha-cucumber            |

**Key Observation**: The current "integration" tests are the **inverse** of what we want:

- **Current**: HTTP entry point + mocked/in-memory storage
- **Desired**: Direct code entry point + real PostgreSQL database

All 11 backends need to migrate to PostgreSQL. Four backends currently use SQLite in-memory (F#, Kotlin, C#, Clojure) and will need to switch to PostgreSQL. The remaining seven use plain in-memory maps with no database at all.

#### Web Apps

| Project                | Unit Tests | Integration Tests | E2E Tests                  | Status    |
| ---------------------- | ---------- | ----------------- | -------------------------- | --------- |
| `organiclever-web`     | Vitest     | Vitest + MSW      | via `organiclever-web-e2e` | Compliant |
| `organiclever-web-e2e` | —          | —                 | Playwright + bddgen        | Compliant |
| `oseplatform-web`      | —          | —                 | —                          | Compliant |
| `ayokoding-web`        | —          | —                 | —                          | Compliant |

**Gap**: None. Web apps already follow the appropriate standard for their type.

#### Go CLI Apps

| Project           | Unit Tests | Integration Tests | BDD Framework | Status    |
| ----------------- | ---------- | ----------------- | ------------- | --------- |
| `ayokoding-cli`   | Go testing | Godog + features  | Godog         | Compliant |
| `oseplatform-cli` | Go testing | Godog + features  | Godog         | Compliant |
| `rhino-cli`       | Go testing | Godog + features  | Godog         | Compliant |

**Gap**: None. CLI apps already follow the appropriate standard.

#### Libraries

| Library          | Unit Tests | Integration Tests     | BDD Framework | Status    |
| ---------------- | ---------- | --------------------- | ------------- | --------- |
| `golang-commons` | Go testing | Godog + mock closures | Godog         | Compliant |
| `hugo-commons`   | Go testing | Godog + tmpdir mocks  | Godog         | Compliant |
| `elixir-cabbage` | ExUnit     | —                     | —             | Compliant |
| `elixir-gherkin` | ExUnit     | —                     | —             | Compliant |

**Gap**: None. Libraries already follow the appropriate standard.

### E2E Tests (Current)

E2E tests already align with the new standard:

- **`demo-be-e2e`**: Uses Playwright + playwright-bdd, consumes `specs/apps/demo-be/gherkin/**/*.feature`, makes real HTTP calls. **Already compliant.**
- **`organiclever-web-e2e`**: Uses Playwright + bddgen, tests organiclever-web. **Already compliant.**

## Gap Analysis

### What Needs to Change

#### 1. Unit Tests — Demo-be Backends Only

**Current**: Some backends have unit tests but none consume Gherkin specs.

**Changes needed**:

- Add BDD framework configuration for unit test execution (same framework already used for integration)
- Create new step definition files for unit-level execution (steps call service/handler functions with mocked dependencies)
- Ensure all 76 Gherkin scenarios run at the unit level with full mocking
- Keep existing pure function tests alongside Gherkin-driven tests
- Configure `test:unit` nx target to run both Gherkin-driven and pure function unit tests

#### 2. Integration Tests — Demo-be Backends Only

**Current**: Tests call through HTTP (MockMvc, httptest, TestClient, etc.) with mocked/in-memory stores.

**Changes needed**:

- **Remove HTTP layer**: Replace MockMvc/httptest/TestClient/ConnCase/WebApplicationFactory with direct service/context function calls
- **Add docker-compose**: Each backend gets a `docker-compose.integration.yml` defining a PostgreSQL service and a test runner service. Replace SQLite/H2/in-memory maps with PostgreSQL.
- **Database lifecycle**: `docker compose up` spawns fresh PostgreSQL + test runner → migrations → seed files → tests → `docker compose down -v` tears down everything.
- **Rewrite step definitions**: Steps must call service functions directly instead of making HTTP requests
- **Retain BDD framework**: Same Gherkin runner (Cucumber, Godog, TickSpec, etc.), new step implementations
- **Update `test:integration` nx target**: Run `docker compose -f docker-compose.integration.yml up --abort-on-container-exit` and capture exit code

#### 3. All Other Projects — No Changes

Already compliant with the three rules:

- **Web UI** (`organiclever-web`): Rules 1+2+3 — unit + integration (MSW) + E2E already working
- **Hugo sites** (`oseplatform-web`, `ayokoding-web`): Exempt — build + link validation only
- **CLI apps** (`ayokoding-cli`, `oseplatform-cli`, `rhino-cli`): Rules 1+2 — unit + Godog integration already working
- **Libraries** (`golang-commons`, `hugo-commons`): Rule 1 — unit tests already working (integration exists but optional)
- **Libraries** (`elixir-cabbage`, `elixir-gherkin`): Rule 1 — unit tests already working
- **E2E runners** (`demo-be-e2e`, `organiclever-web-e2e`): Playwright already working

### Per-Backend Work Summary (Demo-be Only)

| Backend                     | Unit: Add Gherkin Steps | Integration: Remove HTTP | Integration: Add PostgreSQL | Effort |
| --------------------------- | ----------------------- | ------------------------ | --------------------------- | ------ |
| `demo-be-java-springboot`   | New step definitions    | Replace MockMvc          | Add PostgreSQL              | High   |
| `demo-be-elixir-phoenix`    | New step definitions    | Replace ConnCase         | Add PostgreSQL (Ecto)       | High   |
| `demo-be-fsharp-giraffe`    | New step definitions    | Replace WebAppFactory    | Replace SQLite → PostgreSQL | High   |
| `demo-be-golang-gin`        | New step definitions    | Replace httptest         | Add PostgreSQL              | High   |
| `demo-be-python-fastapi`    | New step definitions    | Replace TestClient       | Add PostgreSQL              | High   |
| `demo-be-rust-axum`         | New step definitions    | Replace Tower TestClient | Add PostgreSQL              | High   |
| `demo-be-kotlin-ktor`       | New step definitions    | Replace testApplication  | Replace SQLite → PostgreSQL | High   |
| `demo-be-java-vertx`        | New step definitions    | Replace WebClient        | Add PostgreSQL              | High   |
| `demo-be-ts-effect`         | New step definitions    | Replace HTTP client      | Add PostgreSQL              | High   |
| `demo-be-csharp-aspnetcore` | New step definitions    | Replace WebAppFactory    | Replace SQLite → PostgreSQL | High   |
| `demo-be-clojure-pedestal`  | New step definitions    | Replace clj-http         | Replace SQLite → PostgreSQL | High   |

## Implementation Strategy

### Phase 1: Update Standards and Documentation

Update all governance and documentation files to reflect the new testing standard before changing any code. See [Documentation Updates Required](#documentation-updates-required) for full list.

### Phase 2: Implement Per Demo-be Backend (One at a Time)

For each backend, follow this sequence (per-backend README + project.json updates are done alongside each backend):

1. **Add unit test step definitions** — Create a new set of Gherkin step definitions that call service/handler functions with fully mocked dependencies. Run all 76 scenarios at unit level.

2. **Refactor integration test step definitions** — Rewrite existing step definitions to:
   - Remove HTTP layer (no MockMvc, httptest, TestClient, etc.)
   - Call service/handler functions directly
   - Connect to PostgreSQL (provided by docker-compose)
   - Run migrations and seed files before test execution
   - Run all 76 scenarios at integration level

3. **Create `docker-compose.integration.yml`** — Define:
   - PostgreSQL service (with healthcheck)
   - Test runner service (language runtime + test command)
   - Volumes for source code and specs mount
   - Network for service communication

4. **Create `Dockerfile.integration`** — Define:
   - Language runtime base image
   - Source code and dependency installation
   - Migration + seed + test execution entrypoint

5. **Update `project.json`** — Ensure `test:unit`, `test:integration`, and `test:quick` targets are correctly configured with proper inputs for caching.

6. **Update app README** — Reflect new test structure and commands.

7. **Verify coverage** — Ensure >=90% line coverage from `test:unit` alone. This is critical: currently coverage is measured from integration runs (HTTP+mock). After standardization, unit tests must reach >=90% on their own. For backends with no existing unit tests (7 out of 11), this means writing comprehensive unit-level Gherkin steps that exercise enough code paths.

### Phase 3: Verify and Adapt Non-Demo-be Projects

Verify that all other projects conform to the standard. Most need no code changes, but some need test suite splitting (organiclever-web, Go CLI apps). Confirm:

- Each project has the correct Nx targets for its type
- `test:quick` is the quality gate and works correctly
- Coverage >= 90% where applicable
- `test:unit` caching works; `test:integration` and `test:e2e` are NOT cached

### Docker Compose Infrastructure (Demo-be)

Each demo-be backend gets a `docker-compose.integration.yml` at `apps/demo-be-*/docker-compose.integration.yml` with a standardized structure:

```yaml
# Example: apps/demo-be-java-springboot/docker-compose.integration.yml
services:
  postgres:
    image: postgres:17-alpine
    environment:
      POSTGRES_DB: demo_be_test
      POSTGRES_USER: demo_be_test
      POSTGRES_PASSWORD: demo_be_test
    healthcheck:
      test: ["CMD-SHELL", "pg_isready -U demo_be_test"]
      interval: 2s
      timeout: 5s
      retries: 10

  test-runner:
    build:
      context: .
      dockerfile: Dockerfile.integration
    depends_on:
      postgres:
        condition: service_healthy
    environment:
      DATABASE_URL: postgresql://demo_be_test:demo_be_test@postgres:5432/demo_be_test
    volumes:
      - ../../specs:/specs:ro
    command: ["run-migrations-then-tests"]
```

Key design decisions:

1. **Per-backend `docker-compose.integration.yml`**: Each backend defines its own compose file. This keeps infrastructure close to the code it serves and allows language-specific customization.
2. **Per-backend `Dockerfile.integration`**: A lightweight Dockerfile that installs the language runtime, copies source code, and defines the test command. Includes migration + seed execution before running tests.
3. **Clean spawn**: `docker compose down -v && docker compose up --abort-on-container-exit` ensures no state carries over. The `-v` flag removes volumes (database data).
4. **Migrations**: The test runner container runs migrations as its first step (Flyway, Ecto, EF Core, Diesel, Alembic, JDBC, etc.) before executing tests.
5. **Seed files**: If a backend has seed data (e.g., default admin user, reference data), the seed file executes after migrations and before tests. Each backend defines its own seed mechanism.
6. **Specs mount**: Gherkin specs from `specs/apps/demo-be/gherkin/` are mounted read-only into the test runner container at `/specs`.
7. **Nx target**: `test:integration` runs `docker compose -f docker-compose.integration.yml down -v && docker compose -f docker-compose.integration.yml up --abort-on-container-exit --build` and checks the exit code. NOT cacheable (`cache: false` in nx.json) — Docker builds and container execution are not tracked by Nx file-based caching.
8. **Coverage extraction**: The test runner writes coverage reports to a mounted volume so `rhino-cli test-coverage validate` can read them on the host.

### Recommended Backend Order

Start with the primary backend, then group by language ecosystem:

1. `demo-be-java-springboot` (primary backend, sets the pattern)
2. `demo-be-kotlin-ktor` (JVM, similar Dockerfile)
3. `demo-be-java-vertx` (JVM, similar Dockerfile)
4. `demo-be-fsharp-giraffe` (has unit tests, .NET)
5. `demo-be-csharp-aspnetcore` (.NET, similar Dockerfile)
6. `demo-be-golang-gin` (has unit tests)
7. `demo-be-ts-effect` (has unit tests, Node.js)
8. `demo-be-python-fastapi` (Python)
9. `demo-be-clojure-pedestal` (JVM/Clojure)
10. `demo-be-elixir-phoenix` (Elixir/OTP)
11. `demo-be-rust-axum` (Rust)

## Mandatory Targets Per Project Type

After standardization, each project type MUST have these Nx targets (derived from the three rules):

| Target             | API Backend (Rule 1+2+3) | Web UI (Rule 1+2+3) | CLI App (Rule 1+2) | Library (Rule 1) | Hugo Site (exempt) | E2E Runner |
| ------------------ | ------------------------ | ------------------- | ------------------ | ---------------- | ------------------ | ---------- |
| `test:unit`        | Required                 | Required            | Required           | Required         | —                  | —          |
| `test:integration` | Required (PG)            | Required (MSW)      | Required (BDD)     | Optional         | —                  | —          |
| `test:e2e`         | via demo-be-e2e          | via \*-e2e project  | —                  | —                | —                  | Required   |
| `test:quick`       | Required                 | Required            | Required           | Required         | Required           | Required   |
| `lint`             | Required                 | Required            | Required           | Required         | —                  | Required   |
| `build`            | Required                 | Required            | Required           | —                | Required           | —          |

### `test:quick` Composition

`test:quick` is the local quality gate (pre-push). It runs everything fast and deterministic — **no Docker, no external services**. It includes (where applicable to the project):

1. **`test:unit`** — unit tests with mocked dependencies
2. **Coverage check** — `rhino-cli test-coverage validate` (>= 90% line coverage from unit tests alone)
3. **Specs coverage check** — verify all Gherkin scenarios are consumed (where applicable)

`test:quick` does **NOT** include `lint`, `typecheck`, `test:integration`, or `test:e2e`:

- **`lint`** and **`typecheck`** remain separate Nx targets, run independently by the pre-push hook (`nx affected -t test:quick`) and CI workflows. This avoids double-running them on PRs where `pr-quality-gate.yml` already runs `nx affected -t lint` and `nx affected -t typecheck` as separate steps.
- **`test:integration`** and **`test:e2e`** run on separate CI schedules.

| Project Type | `test:quick` includes                               |
| ------------ | --------------------------------------------------- |
| API Backend  | `test:unit` + coverage check + specs coverage check |
| Web UI       | `test:unit` + coverage check                        |
| CLI App      | `test:unit` + coverage check                        |
| Library      | `test:unit` + coverage check                        |
| Hugo Site    | Link validation only                                |
| E2E Runner   | bddgen (no actual test execution)                   |

**Coverage measurement change**: Currently coverage for many projects is measured from integration test runs. After standardization, coverage must come from `test:unit` alone. This means unit tests must exercise enough code paths to reach ≥90%. For demo-be backends, the Gherkin-driven unit tests (with mocked dependencies) will be the primary coverage source. For Go CLI apps, unit tests (excluding Godog BDD files) must reach ≥90%.

**organiclever-web special case**: Currently `organiclever-web` runs all Vitest tests (unit + MSW integration) in a single `npx vitest run --coverage`. After standardization, MSW tests are reclassified as `test:integration` and excluded from `test:quick`. Unit tests alone must reach ≥90% coverage. This requires splitting the test suites or adding more unit tests.

### CI Schedules

Integration and E2E tests run on scheduled CI pipelines, not in `test:quick`:

| Schedule             | WIB Times                  | UTC Times                    | What runs                                |
| -------------------- | -------------------------- | ---------------------------- | ---------------------------------------- |
| **Integration** (4x) | 04:00, 10:00, 16:00, 22:00 | 21:00\*, 03:00, 09:00, 15:00 | `test:integration` for all apps          |
| **E2E** (2x)         | 06:00, 18:00               | 23:00\*, 11:00               | `test:e2e` for all web apps (APIs + UIs) |

\* Previous day UTC

This separation keeps `test:quick` fast (seconds to minutes) while integration and E2E tests run with full infrastructure on a predictable schedule. Developers can still run `test:integration` and `test:e2e` locally on demand.

### CI Trigger Summary by Push Method

Code reaches `main` via two paths — direct push or PR merge. Here's what CI runs for each:

| Push Method     | Pre-push (local)            | CI Trigger                                                                                                    |
| --------------- | --------------------------- | ------------------------------------------------------------------------------------------------------------- |
| **Direct push** | `nx affected -t test:quick` | `main-ci.yml`: `nx run-many -t test:quick --all` + coverage uploads                                           |
| **PR → merge**  | `nx affected -t test:quick` | `pr-quality-gate.yml`: `nx affected -t typecheck` + `lint` + `test:quick` → then merge triggers `main-ci.yml` |

Both paths guarantee `test:quick` runs. PRs additionally run `typecheck` and `lint` as separate targets via `pr-quality-gate.yml`.

### Commit and Push Strategy

This plan touches many projects across multiple phases. To avoid burdening the pre-push hook (which runs `nx affected -t test:quick` for all affected projects), use **thematic commits** and **push regularly**:

1. **Commit by theme, not by phase** — Group changes that belong together. For example, when working on `demo-be-java-springboot`, commit unit test step definitions separately from docker-compose infrastructure. This keeps each commit reviewable and the diff small.

2. **Push after each thematic commit** — Don't batch up many commits before pushing. Each push triggers `test:quick` only for affected projects. A small push touching one backend runs `test:quick` for just that backend (seconds). A large push touching 5 backends runs `test:quick` for all 5 (minutes).

3. **One backend at a time** — Complete one demo-be backend fully (all 7 steps) before starting the next. This means each push only affects one backend, keeping pre-push fast.

4. **Documentation commits separate** — Phase 1 documentation changes (CLAUDE.md, nx-targets.md, etc.) should be committed and pushed before starting Phase 2 code changes. Documentation changes trigger minimal `test:quick` runs.

5. **Example commit sequence for one backend**:
   - `test(demo-be-java-springboot): add unit-level Gherkin step definitions` → push
   - `refactor(demo-be-java-springboot): replace MockMvc with direct service calls in integration tests` → push
   - `ci(demo-be-java-springboot): add docker-compose.integration.yml and Dockerfile.integration` → push
   - `chore(demo-be-java-springboot): update project.json targets and README` → push
   - `test(demo-be-java-springboot): verify coverage ≥90% from unit tests` → push

This approach keeps each pre-push gate under a minute and makes it easy to bisect if something breaks.

## Documentation Updates Required

### Files to Update

#### 1. `CLAUDE.md` (Root)

**Sections to update**:

- **"Unit vs. integration test principle"** — Rewrite to reflect the new three-level standard with per-type rules. Current text says "Unit tests cover only what integration tests cannot reach." New text should define each level's boundaries clearly, covering all project types.
- **`test:integration` caching section** — Update the long list describing each project's integration test approach. Remove references to MockMvc, httptest, ConnCase, etc. Describe the new pattern: demo-be uses docker-compose + PostgreSQL; others unchanged.
- **"Common Development Commands"** — Ensure `test:unit`, `test:integration`, `test:e2e` are all listed with correct descriptions.
- **Coverage sections** — Update to clarify coverage is now measured from `test:unit` runs only (not integration). Coverage file paths may change for some backends.
- **`test:quick` description** — Update to reflect that `test:quick` includes only unit + coverage + specs coverage, NOT lint or typecheck (those are separate targets).

#### 2. `governance/development/infra/nx-targets.md`

**Sections to update**:

- **`test:unit` definition** — Add: "Must consume corresponding Gherkin specs with mocked dependencies (demo-be). Calls application code directly."
- **`test:integration` definition** — Rewrite: "Demo-be backends: Uses real PostgreSQL database (clean spawn per run via docker-compose). No HTTP calls. Calls application code directly. Must consume Gherkin specs. Other projects: existing patterns (MSW, Godog, etc.)."
- **`test:e2e` definition** — Add: "Must consume corresponding Gherkin specs (demo-be). Uses Playwright."
- **Caching rules** — Only `test:unit` is cacheable. `test:integration` and `test:e2e` are NOT cacheable (`cache: false` in nx.json).
- **Mandatory targets table** — Update with the per-project-type matrix defined above.

#### 3. `governance/development/infra/bdd-spec-test-mapping.md`

**Sections to update**:

- **Scope** — Extend beyond CLI apps. Add demo-be backends as covered projects.
- **Three-level consumption** — Document that specs are consumed at unit, integration, AND e2e levels with different step implementations.
- **Validation** — Define how to validate that all 76 scenarios pass at each level.

#### 4. `specs/apps/demo-be/README.md`

**Sections to add/update**:

- **Consumption model** — Document the three-level consumption: unit (mocked), integration (PostgreSQL, no HTTP), e2e (full Playwright).
- **Step definition organization** — Recommend directory structure for separating unit vs integration steps within each backend.

#### 5. Each `apps/demo-be-*/README.md` (11 files)

**Updates per backend**:

- **Test structure section** — Document unit vs integration directory layout.
- **Test commands** — List `test:unit`, `test:integration`, `test:quick` commands.
- **Test architecture** — Describe what's mocked at each level and what's real.
- **Docker compose** — Document `docker-compose.integration.yml` and `Dockerfile.integration`.

#### 6. Each `apps/demo-be-*/project.json` (11 files)

**Updates per backend**:

- **`test:unit` target** — Add or update to run BDD specs with mocked dependencies + pure function tests.
- **`test:integration` target** — Update to run BDD specs via docker-compose with real PostgreSQL, no HTTP.
- **`test:quick` target** — Ensure it runs `test:unit` + coverage check + specs coverage check. Does NOT include lint, typecheck, `test:integration`, or `test:e2e`.
- **Caching inputs** — Ensure `specs/apps/demo-be/gherkin/**/*.feature`, `docker-compose.integration.yml`, and `Dockerfile.integration` are in inputs for `test:integration`. Specs also in inputs for `test:unit`.

### Files That Do NOT Need Changes

- **`apps/demo-be-e2e/`** — Already compliant with the new standard.
- **`apps/organiclever-web/`** — Already compliant (Vitest + MSW).
- **`apps/organiclever-web-e2e/`** — Already compliant (Playwright + bddgen).
- **`apps/oseplatform-web/`** — Already compliant (link validation only).
- **`apps/ayokoding-web/`** — Already compliant (link validation only).
- **`apps/ayokoding-cli/`** — Already compliant (Go unit + Godog integration).
- **`apps/oseplatform-cli/`** — Already compliant (Go unit + Godog integration).
- **`apps/rhino-cli/`** — Already compliant (Go unit + Godog integration).
- **`libs/golang-commons/`** — Already compliant (Go unit + Godog integration).
- **`libs/hugo-commons/`** — Already compliant (Go unit + Godog integration).
- **`libs/elixir-cabbage/`** — Already compliant (ExUnit).
- **`libs/elixir-gherkin/`** — Already compliant (ExUnit).
- **`governance/conventions/`** — No convention changes needed.
- **`governance/principles/`** — Principles remain the same.

#### 7. `.github/workflows/` — CI Workflow Updates

##### Current State (18 workflow files)

| Workflow                            | Trigger                            | What it does                                                                     |
| ----------------------------------- | ---------------------------------- | -------------------------------------------------------------------------------- |
| `main-ci.yml`                       | Push to `main`                     | `npx nx run-many -t test:quick --all` + coverage uploads to Codecov              |
| `pr-quality-gate.yml`               | PR opened/sync/reopen              | `nx affected -t typecheck` + `nx affected -t lint` + `nx affected -t test:quick` |
| `validate-docs-links.yml`           | PR opened/sync/reopen              | `rhino-cli docs validate-links`                                                  |
| `format-pr.yml`                     | PR opened/sync/reopen              | Formatting checks                                                                |
| `deploy-ayokoding-web.yml`          | Push to `prod-ayokoding-web`       | Deploy to Vercel                                                                 |
| `deploy-oseplatform-web.yml`        | Push to `prod-oseplatform-web`     | Deploy to Vercel                                                                 |
| `e2e-demo-be-java-springboot.yml`   | Cron 2x daily (WIB 06/18) + manual | Docker compose up → Playwright E2E → teardown                                    |
| `e2e-demo-be-kotlin-ktor.yml`       | Cron 2x daily (WIB 06/18) + manual | Docker compose up → Playwright E2E → teardown                                    |
| `e2e-demo-be-java-vertx.yml`        | Cron 2x daily (WIB 06/18) + manual | Docker compose up → Playwright E2E → teardown                                    |
| `e2e-demo-be-fsharp-giraffe.yml`    | Cron 2x daily (WIB 06/18) + manual | Docker compose up → Playwright E2E → teardown                                    |
| `e2e-demo-be-csharp-aspnetcore.yml` | Cron 2x daily (WIB 06/18) + manual | Docker compose up → Playwright E2E → teardown                                    |
| `e2e-demo-be-golang-gin.yml`        | Cron 2x daily (WIB 06/18) + manual | Docker compose up → Playwright E2E → teardown                                    |
| `e2e-demo-be-ts-effect.yml`         | Cron 2x daily (WIB 06/18) + manual | Docker compose up → Playwright E2E → teardown                                    |
| `e2e-demo-be-python-fastapi.yml`    | Cron 2x daily (WIB 06/18) + manual | Docker compose up → Playwright E2E → teardown                                    |
| `e2e-demo-be-clojure-pedestal.yml`  | Cron 2x daily (WIB 06/18) + manual | Docker compose up → Playwright E2E → teardown                                    |
| `e2e-demo-be-elixir-phoenix.yml`    | Cron 2x daily (WIB 06/18) + manual | Docker compose up → Playwright E2E → teardown                                    |
| `e2e-demo-be-rust-axum.yml`         | Cron 2x daily (WIB 06/18) + manual | Docker compose up → Playwright E2E → teardown                                    |
| `e2e-organiclever-web.yml`          | Cron 2x daily (WIB 06/18) + manual | Docker compose up → Playwright E2E → teardown                                    |

##### Changes Needed

**`main-ci.yml`** — Update coverage file paths:

- **Current**: Coverage uploads reference paths produced by integration test runs (e.g., `apps/demo-be-java-springboot/target/site/jacoco/jacoco.xml` from `mvn jacoco:report -Pintegration`). After standardization, `test:quick` only runs `test:unit`, so coverage comes from unit test runs.
- **Action**: Update each coverage upload's `files:` path to point to the unit test coverage output. The exact paths depend on how each backend configures unit test coverage reporting.
- **JaCoCo report step**: Remove `mvn jacoco:report -Pintegration` step (line 110) — unit test coverage is generated directly by `test:unit`.
- **No structural changes** to the workflow itself — it still runs `test:quick` for all projects on push to `main`.

**`pr-quality-gate.yml`** — No changes needed:

- Already runs `typecheck`, `lint`, and `test:quick` as separate steps. Since `test:quick` no longer includes lint/typecheck, there's no double-running. Already compliant.

**New `integration-ci.yml`** — Create scheduled workflow:

- **Schedule**: Cron 4x daily at WIB 04:00, 10:00, 16:00, 22:00 (UTC 21:00, 03:00, 09:00, 15:00)
- **Trigger**: `schedule` + `workflow_dispatch`
- **What it runs**: `npx nx run-many -t test:integration --all`
- **Infrastructure requirements**: The CI runner needs Docker (for demo-be docker-compose) AND host-level language runtimes (Go for CLI apps, Elixir for phoenix, Node.js for organiclever-web MSW tests). Setup steps similar to `main-ci.yml` are needed.
- **Docker-in-CI**: GitHub Actions `ubuntu-latest` has Docker pre-installed. Demo-be `test:integration` targets run `docker compose up --abort-on-container-exit` which works natively.

**E2E workflows (12 files)** — Already compliant:

- All 12 E2E workflows already run at WIB 06:00/18:00 (UTC cron `0 23 * * *` and `0 11 * * *`). **No changes needed.**

**Other workflows** — No changes needed:

- `validate-docs-links.yml` — PR link validation, unrelated to testing
- `format-pr.yml` — PR formatting checks, unrelated to testing
- `deploy-ayokoding-web.yml` — Deployment only
- `deploy-oseplatform-web.yml` — Deployment only

#### 8. Root `README.md`

**Sections to update**:

- **"CI & Test Coverage" section** — Currently shows E2E + Coverage badges per demo-be backend. After standardization:
  - Add **Integration** badge column for demo-be backends (new CI workflow)
  - Add Integration badge for `organiclever-web` and Go CLI apps
  - Update table headers to reflect three CI types: Integration (4x daily) | E2E (2x daily) | Coverage
- **Badge descriptions** — Update text to explain the CI schedule (integration 4x daily, E2E 2x daily)
- **Clarify** that `test:quick` (run on every push via `main-ci.yml` and on every PR via `pr-quality-gate.yml`) includes unit tests + coverage, but NOT lint, typecheck, integration, or E2E (lint and typecheck run as separate targets)

## Acceptance Criteria

```gherkin
Feature: Testing Standardization

  # === Demo-be Backend Tests ===

  Scenario: Demo-be unit tests consume Gherkin specs with mocked dependencies
    Given a demo-be backend with unit tests configured
    When the unit test suite runs
    Then all 76 Gherkin scenarios from specs/apps/demo-be/gherkin/ execute
    And all dependencies (database, HTTP, external APIs) are mocked
    And no real database connections are created
    And no HTTP requests are made
    And application code is called directly (not through HTTP)

  Scenario: Demo-be integration tests use real PostgreSQL via docker-compose
    Given a demo-be backend with integration tests configured
    When the integration test suite runs
    Then docker-compose starts a fresh PostgreSQL container
    And a test runner container starts after PostgreSQL is healthy
    And all migrations are executed against the fresh database
    And seed files are executed if present
    And all 76 Gherkin scenarios from specs/apps/demo-be/gherkin/ execute
    And no HTTP requests are made
    And no external API calls are made
    And application code is called directly (not through HTTP)
    And docker-compose tears down all containers and volumes after tests

  Scenario: Demo-be E2E tests use Playwright with no restrictions
    Given the demo-be-e2e project
    When the E2E test suite runs against a backend
    Then all 76 Gherkin scenarios from specs/apps/demo-be/gherkin/ execute
    And Playwright makes real HTTP requests to the running backend
    And no infrastructure restrictions are enforced

  Scenario: All three test levels pass for each demo-be backend
    Given a demo-be backend with all test levels implemented
    When test:unit runs
    Then all scenarios pass with mocked dependencies
    When test:integration runs
    Then all scenarios pass with real PostgreSQL database
    When test:e2e runs against the backend
    Then all scenarios pass via Playwright HTTP calls

  # === Non-Demo-be Projects ===

  Scenario: organiclever-web tests pass at unit and integration levels
    Given the organiclever-web project
    When test:quick runs
    Then unit tests pass via Vitest (without MSW)
    And coverage >= 90% from unit tests alone
    When test:integration runs
    Then integration tests pass via Vitest + MSW

  Scenario: Go CLI apps pass unit and integration tests (Rule 1+2)
    Given a Go CLI app project
    When test:quick runs
    Then unit tests pass via Go testing
    And coverage >= 90%
    When test:integration runs
    Then BDD scenarios pass via Godog

  Scenario: Libraries pass unit tests (Rule 1 only)
    Given a library project
    When test:quick runs
    Then unit tests pass
    And coverage >= 90%
    And integration tests are not required

  Scenario: Hugo static sites pass link validation
    Given a Hugo static site project
    When test:quick runs
    Then link validation passes

  # === test:quick (Local Quality Gate) ===

  Scenario: test:quick includes unit tests and coverage only
    Given any project with test:quick configured (not Hugo sites)
    When test:quick runs
    Then test:unit passes with all scenarios
    And line coverage is >= 90% via rhino-cli test-coverage validate (from unit tests alone)
    And specs coverage check passes (where applicable)
    And lint is NOT run (separate target)
    And typecheck is NOT run (separate target)
    And test:integration is NOT run
    And test:e2e is NOT run

  # === CI Schedules ===

  Scenario: Integration tests run 4 times daily via CI
    Given the CI integration schedule
    Then test:integration runs at WIB 04:00, 10:00, 16:00, 22:00
    And it covers all apps that have test:integration
    And each run uses fresh docker-compose containers (demo-be backends)

  Scenario: E2E tests run 2 times daily via CI
    Given the CI E2E schedule
    Then test:e2e runs at WIB 06:00 and 18:00
    And it covers all web apps (API backends + web UIs)
    And each run uses Playwright against running services

  # === Cross-Cutting ===

  Scenario: Only test:unit is cacheable
    Given any project with no code or spec changes
    When test:unit runs a second time
    Then Nx serves the result from cache
    When test:integration runs a second time
    Then Nx does NOT serve from cache (always re-runs)
    When test:e2e runs a second time
    Then Nx does NOT serve from cache (always re-runs)

  Scenario: Every project has mandatory Nx targets derived from three rules
    Given the three rules and mandatory targets table
    Then each API backend has test:unit, test:integration, test:quick, lint, build (Rule 1+2+3)
    And each web UI app has test:unit, test:integration, test:quick, lint, build (Rule 1+2+3)
    And each CLI app has test:unit, test:integration, test:quick, lint, build (Rule 1+2)
    And each library has test:unit, test:quick, lint (Rule 1 only)
    And each Hugo site has test:quick, build (exempt from rules)
    And each E2E runner has test:e2e, test:quick, lint

  Scenario: CI runs correct checks for direct push to main
    Given code is pushed directly to main
    Then main-ci.yml runs test:quick for all projects
    And coverage is uploaded to Codecov from unit test output
    And lint and typecheck are NOT run by main-ci.yml

  Scenario: CI runs correct checks for PR workflow
    Given a PR is opened or updated
    Then pr-quality-gate.yml runs typecheck for affected projects
    And pr-quality-gate.yml runs lint for affected projects
    And pr-quality-gate.yml runs test:quick for affected projects
    When the PR is merged to main
    Then main-ci.yml runs test:quick for all projects

  Scenario: Documentation reflects new standard
    Given the testing standardization is complete
    Then CLAUDE.md describes the three-level testing standard for all project types
    And nx-targets.md defines mandatory targets per project type and CI schedules
    And bdd-spec-test-mapping.md covers demo-be backends
    And specs/apps/demo-be/README.md documents three-level consumption
    And each demo-be backend README describes its test architecture
    And each demo-be project.json has correct test targets
    And root README.md shows Integration, E2E, and Coverage badges per project
```

## Delivery Checklist

### Phase 1: Update Standards and Documentation

Update governance and documentation files before changing any code.

- [ ] **1.1 Update `CLAUDE.md`**
  - [ ] Rewrite "Unit vs. integration test principle" section to describe three-level standard (unit/integration/e2e) with per-type rules
  - [ ] Rewrite `test:integration` caching section — remove MockMvc/httptest/ConnCase/etc. references, describe docker-compose + PostgreSQL for demo-be, others unchanged
  - [ ] Update "Common Development Commands" — list `test:unit`, `test:integration`, `test:e2e` with correct descriptions
  - [ ] Update coverage sections — clarify coverage is measured from `test:unit` only, not integration
  - [ ] Update `test:quick` description — includes unit + coverage + specs coverage only (lint/typecheck are separate targets)
  - [ ] Commit: `docs: update CLAUDE.md testing sections for three-level standard` → push
- [ ] **1.2 Update `governance/development/infra/nx-targets.md`**
  - [ ] Update `test:unit` definition — "Must consume Gherkin specs with mocked dependencies (demo-be). Calls application code directly."
  - [ ] Rewrite `test:integration` definition — "Demo-be: real PostgreSQL via docker-compose, no HTTP, direct code calls. Others: existing patterns (MSW, Godog)."
  - [ ] Update `test:e2e` definition — "Must consume Gherkin specs (demo-be). Uses Playwright."
  - [ ] Update caching rules — only `test:unit` is cacheable; `test:integration` and `test:e2e` are NOT cacheable
  - [ ] Add mandatory targets matrix table (per-project-type)
  - [ ] Commit: `docs: update nx-targets.md with three-level testing standard and caching rules` → push
- [ ] **1.3 Update `governance/development/infra/bdd-spec-test-mapping.md`**
  - [ ] Extend scope beyond CLI apps to include demo-be backends
  - [ ] Document three-level consumption model (unit/integration/e2e with different step implementations)
  - [ ] Define how to validate all 76 scenarios pass at each level
  - [ ] Commit: `docs: update bdd-spec-test-mapping.md for demo-be three-level consumption` → push
- [ ] **1.4 Update `specs/apps/demo-be/README.md`**
  - [ ] Document three-level consumption model: unit (mocked), integration (PostgreSQL, no HTTP), e2e (Playwright)
  - [ ] Recommend directory structure for separating unit vs integration step definitions within each backend
  - [ ] Commit: `docs(demo-be): document three-level spec consumption model` → push

### Phase 2: Demo-be Backend Implementations

Implement one backend at a time. Each backend follows 7 steps. The first backend (`demo-be-java-springboot`) sets the reference pattern. Complete one backend fully before starting the next.

#### 2.1 `demo-be-java-springboot` (reference implementation)

- [ ] **2.1.1 Unit test step definitions**
  - [ ] Create unit-level Cucumber step definition files (separate from integration steps)
  - [ ] Steps call service/repository functions directly with mocked dependencies (no Spring context, no MockMvc)
  - [ ] Run all 76 Gherkin scenarios at unit level
  - [ ] Keep existing pure function unit tests alongside Gherkin-driven tests
  - [ ] Commit: `test(demo-be-java-springboot): add unit-level Gherkin step definitions` → push
- [ ] **2.1.2 Integration test refactor**
  - [ ] Remove MockMvc HTTP layer from existing integration step definitions
  - [ ] Rewrite steps to call service/repository functions directly
  - [ ] Add PostgreSQL connection configuration (reads `DATABASE_URL` env var)
  - [ ] Add database migration support (Flyway or similar)
  - [ ] Add seed data loading if needed
  - [ ] Run all 76 Gherkin scenarios at integration level
  - [ ] Commit: `refactor(demo-be-java-springboot): replace MockMvc with direct service calls in integration tests` → push
- [ ] **2.1.3 Create `docker-compose.integration.yml`**
  - [ ] Define `postgres` service (postgres:17-alpine, healthcheck, credentials)
  - [ ] Define `test-runner` service (depends_on postgres healthy, DATABASE_URL env, specs volume mount)
  - [ ] Mount `../../specs:/specs:ro` for Gherkin specs
  - [ ] Mount coverage output volume for host access
- [ ] **2.1.4 Create `Dockerfile.integration`**
  - [ ] Base image with Java 25 runtime
  - [ ] Copy source code and install dependencies (mvn)
  - [ ] Entrypoint: run migrations → run seed → run tests → output coverage
  - [ ] Commit: `ci(demo-be-java-springboot): add docker-compose.integration.yml and Dockerfile.integration` → push
- [ ] **2.1.5 Update `project.json`**
  - [ ] Update `test:unit` target — run Cucumber with mocked dependencies + existing pure function tests
  - [ ] Update `test:integration` target — `docker compose -f docker-compose.integration.yml down -v && docker compose -f docker-compose.integration.yml up --abort-on-container-exit --build`
  - [ ] Update `test:quick` target — `test:unit` + `rhino-cli test-coverage validate` + specs coverage check (no lint, no typecheck, no integration)
  - [ ] Add caching inputs: `specs/apps/demo-be/gherkin/**/*.feature` for `test:unit`; add `docker-compose.integration.yml` + `Dockerfile.integration` for `test:integration`
- [ ] **2.1.6 Update `apps/demo-be-java-springboot/README.md`**
  - [ ] Document unit vs integration test directory layout
  - [ ] Document `test:unit`, `test:integration`, `test:quick` commands
  - [ ] Document what's mocked at each level and what's real
  - [ ] Document `docker-compose.integration.yml` and `Dockerfile.integration` usage
  - [ ] Commit: `chore(demo-be-java-springboot): update project.json targets and README` → push
- [ ] **2.1.7 Verify coverage**
  - [ ] Run `nx run demo-be-java-springboot:test:unit` and confirm coverage file is generated
  - [ ] Run `rhino-cli test-coverage validate <coverage-file> 90` — confirm ≥90% from unit tests alone
  - [ ] Run `nx run demo-be-java-springboot:test:integration` — confirm all 76 scenarios pass with PostgreSQL
  - [ ] Run `nx run demo-be-java-springboot:test:quick` — confirm full quality gate passes
  - [ ] Commit (if coverage fixes needed): `test(demo-be-java-springboot): reach ≥90% unit test coverage` → push

#### 2.2 `demo-be-kotlin-ktor` (JVM — replace SQLite → PostgreSQL)

- [ ] **2.2.1** Create unit-level Cucumber step definitions (mocked dependencies, no testApplication)
- [ ] **2.2.2** Refactor integration steps — remove `testApplication` HTTP layer, call service functions directly, replace SQLite with PostgreSQL
  - [ ] Commit: `test(demo-be-kotlin-ktor): add unit steps and refactor integration to PostgreSQL` → push
- [ ] **2.2.3** Create `docker-compose.integration.yml` (postgres + Kotlin test runner)
- [ ] **2.2.4** Create `Dockerfile.integration` (Gradle + Java 21 base image)
  - [ ] Commit: `ci(demo-be-kotlin-ktor): add docker-compose and Dockerfile for integration tests` → push
- [ ] **2.2.5** Update `project.json` — `test:unit`, `test:integration`, `test:quick` targets with caching inputs
- [ ] **2.2.6** Update README — test architecture, commands, docker-compose docs
  - [ ] Commit: `chore(demo-be-kotlin-ktor): update project.json targets and README` → push
- [ ] **2.2.7** Verify ≥90% coverage from `test:unit` alone + all 76 scenarios pass at both levels

#### 2.3 `demo-be-java-vertx` (JVM — add PostgreSQL)

- [ ] **2.3.1** Create unit-level Cucumber step definitions (mocked dependencies, no Vert.x WebClient)
- [ ] **2.3.2** Refactor integration steps — remove WebClient HTTP layer, call service functions directly, add PostgreSQL
  - [ ] Commit: `test(demo-be-java-vertx): add unit steps and refactor integration to PostgreSQL` → push
- [ ] **2.3.3** Create `docker-compose.integration.yml` (postgres + Java test runner)
- [ ] **2.3.4** Create `Dockerfile.integration` (Maven + Java 25 base image)
  - [ ] Commit: `ci(demo-be-java-vertx): add docker-compose and Dockerfile for integration tests` → push
- [ ] **2.3.5** Update `project.json` — `test:unit`, `test:integration`, `test:quick` targets with caching inputs
- [ ] **2.3.6** Update README — test architecture, commands, docker-compose docs
  - [ ] Commit: `chore(demo-be-java-vertx): update project.json targets and README` → push
- [ ] **2.3.7** Verify ≥90% coverage from `test:unit` alone + all 76 scenarios pass at both levels

#### 2.4 `demo-be-fsharp-giraffe` (.NET — replace SQLite → PostgreSQL)

- [ ] **2.4.1** Create unit-level TickSpec step definitions (mocked dependencies, no WebApplicationFactory)
- [ ] **2.4.2** Refactor integration steps — remove WebApplicationFactory HTTP layer, call handler functions directly, replace SQLite with PostgreSQL (Npgsql)
  - [ ] Commit: `test(demo-be-fsharp-giraffe): add unit steps and refactor integration to PostgreSQL` → push
- [ ] **2.4.3** Create `docker-compose.integration.yml` (postgres + .NET test runner)
- [ ] **2.4.4** Create `Dockerfile.integration` (.NET 10 SDK base image)
  - [ ] Commit: `ci(demo-be-fsharp-giraffe): add docker-compose and Dockerfile for integration tests` → push
- [ ] **2.4.5** Update `project.json` — `test:unit`, `test:integration`, `test:quick` targets with caching inputs
- [ ] **2.4.6** Update README — test architecture, commands, docker-compose docs
  - [ ] Commit: `chore(demo-be-fsharp-giraffe): update project.json targets and README` → push
- [ ] **2.4.7** Verify ≥90% coverage from `test:unit` alone + all 76 scenarios pass at both levels

#### 2.5 `demo-be-csharp-aspnetcore` (.NET — replace SQLite → PostgreSQL)

- [ ] **2.5.1** Create unit-level Reqnroll step definitions (mocked dependencies, no WebApplicationFactory)
- [ ] **2.5.2** Refactor integration steps — remove WebApplicationFactory HTTP layer, call service functions directly, replace SQLite with PostgreSQL (Npgsql)
  - [ ] Commit: `test(demo-be-csharp-aspnetcore): add unit steps and refactor integration to PostgreSQL` → push
- [ ] **2.5.3** Create `docker-compose.integration.yml` (postgres + .NET test runner)
- [ ] **2.5.4** Create `Dockerfile.integration` (.NET 10 SDK base image)
  - [ ] Commit: `ci(demo-be-csharp-aspnetcore): add docker-compose and Dockerfile for integration tests` → push
- [ ] **2.5.5** Update `project.json` — `test:unit`, `test:integration`, `test:quick` targets with caching inputs
- [ ] **2.5.6** Update README — test architecture, commands, docker-compose docs
  - [ ] Commit: `chore(demo-be-csharp-aspnetcore): update project.json targets and README` → push
- [ ] **2.5.7** Verify ≥90% coverage from `test:unit` alone + all 76 scenarios pass at both levels

#### 2.6 `demo-be-golang-gin` (Go — add PostgreSQL)

- [ ] **2.6.1** Create unit-level Godog step definitions (mocked dependencies, no httptest.Server)
- [ ] **2.6.2** Refactor integration steps — remove httptest HTTP layer, call handler functions directly, add PostgreSQL (pgx or database/sql)
  - [ ] Commit: `test(demo-be-golang-gin): add unit steps and refactor integration to PostgreSQL` → push
- [ ] **2.6.3** Create `docker-compose.integration.yml` (postgres + Go test runner)
- [ ] **2.6.4** Create `Dockerfile.integration` (Go 1.26 base image)
  - [ ] Commit: `ci(demo-be-golang-gin): add docker-compose and Dockerfile for integration tests` → push
- [ ] **2.6.5** Update `project.json` — `test:unit`, `test:integration`, `test:quick` targets with caching inputs
- [ ] **2.6.6** Update README — test architecture, commands, docker-compose docs
  - [ ] Commit: `chore(demo-be-golang-gin): update project.json targets and README` → push
- [ ] **2.6.7** Verify ≥90% coverage from `test:unit` alone + all 76 scenarios pass at both levels

#### 2.7 `demo-be-ts-effect` (Node.js — add PostgreSQL)

- [ ] **2.7.1** Create unit-level Cucumber.js step definitions (mocked dependencies, no HTTP client)
- [ ] **2.7.2** Refactor integration steps — remove HTTP client layer, call service functions directly, add PostgreSQL (pg or drizzle)
  - [ ] Commit: `test(demo-be-ts-effect): add unit steps and refactor integration to PostgreSQL` → push
- [ ] **2.7.3** Create `docker-compose.integration.yml` (postgres + Node.js test runner)
- [ ] **2.7.4** Create `Dockerfile.integration` (Node.js 24 base image)
  - [ ] Commit: `ci(demo-be-ts-effect): add docker-compose and Dockerfile for integration tests` → push
- [ ] **2.7.5** Update `project.json` — `test:unit`, `test:integration`, `test:quick` targets with caching inputs
- [ ] **2.7.6** Update README — test architecture, commands, docker-compose docs
  - [ ] Commit: `chore(demo-be-ts-effect): update project.json targets and README` → push
- [ ] **2.7.7** Verify ≥90% coverage from `test:unit` alone + all 76 scenarios pass at both levels

#### 2.8 `demo-be-python-fastapi` (Python — add PostgreSQL)

- [ ] **2.8.1** Create unit-level pytest-bdd step definitions (mocked dependencies, no TestClient)
- [ ] **2.8.2** Refactor integration steps — remove TestClient HTTP layer, call service functions directly, add PostgreSQL (asyncpg or psycopg)
  - [ ] Commit: `test(demo-be-python-fastapi): add unit steps and refactor integration to PostgreSQL` → push
- [ ] **2.8.3** Create `docker-compose.integration.yml` (postgres + Python test runner)
- [ ] **2.8.4** Create `Dockerfile.integration` (Python 3.13 + uv base image)
  - [ ] Commit: `ci(demo-be-python-fastapi): add docker-compose and Dockerfile for integration tests` → push
- [ ] **2.8.5** Update `project.json` — `test:unit`, `test:integration`, `test:quick` targets with caching inputs
- [ ] **2.8.6** Update README — test architecture, commands, docker-compose docs
  - [ ] Commit: `chore(demo-be-python-fastapi): update project.json targets and README` → push
- [ ] **2.8.7** Verify ≥90% coverage from `test:unit` alone + all 76 scenarios pass at both levels

#### 2.9 `demo-be-clojure-pedestal` (JVM/Clojure — replace SQLite → PostgreSQL)

- [ ] **2.9.1** Create unit-level kaocha-cucumber step definitions (mocked dependencies, no clj-http)
- [ ] **2.9.2** Refactor integration steps — remove clj-http HTTP layer, call handler functions directly, replace SQLite with PostgreSQL (next.jdbc)
  - [ ] Commit: `test(demo-be-clojure-pedestal): add unit steps and refactor integration to PostgreSQL` → push
- [ ] **2.9.3** Create `docker-compose.integration.yml` (postgres + Clojure test runner)
- [ ] **2.9.4** Create `Dockerfile.integration` (Clojure CLI + Java 21 base image)
  - [ ] Commit: `ci(demo-be-clojure-pedestal): add docker-compose and Dockerfile for integration tests` → push
- [ ] **2.9.5** Update `project.json` — `test:unit`, `test:integration`, `test:quick` targets with caching inputs
- [ ] **2.9.6** Update README — test architecture, commands, docker-compose docs
  - [ ] Commit: `chore(demo-be-clojure-pedestal): update project.json targets and README` → push
- [ ] **2.9.7** Verify ≥90% coverage from `test:unit` alone + all 76 scenarios pass at both levels

#### 2.10 `demo-be-elixir-phoenix` (Elixir/OTP — add PostgreSQL via Ecto)

- [ ] **2.10.1** Create unit-level Cabbage step definitions (mocked dependencies, no ConnCase)
- [ ] **2.10.2** Refactor integration steps — remove ConnCase HTTP layer, call context functions directly, add PostgreSQL via Ecto sandbox
  - [ ] Commit: `test(demo-be-elixir-phoenix): add unit steps and refactor integration to PostgreSQL` → push
- [ ] **2.10.3** Create `docker-compose.integration.yml` (postgres + Elixir test runner)
- [ ] **2.10.4** Create `Dockerfile.integration` (elixir:1.19-otp-27-alpine + build-base)
  - [ ] Commit: `ci(demo-be-elixir-phoenix): add docker-compose and Dockerfile for integration tests` → push
- [ ] **2.10.5** Update `project.json` — `test:unit`, `test:integration`, `test:quick` targets with caching inputs
- [ ] **2.10.6** Update README — test architecture, commands, docker-compose docs
  - [ ] Commit: `chore(demo-be-elixir-phoenix): update project.json targets and README` → push
- [ ] **2.10.7** Verify ≥90% coverage from `test:unit` alone + all 76 scenarios pass at both levels

#### 2.11 `demo-be-rust-axum` (Rust — add PostgreSQL)

- [ ] **2.11.1** Create unit-level cucumber-rs step definitions (mocked dependencies, no Tower TestClient)
- [ ] **2.11.2** Refactor integration steps — remove Tower TestClient HTTP layer, call handler functions directly, add PostgreSQL (sqlx)
  - [ ] Commit: `test(demo-be-rust-axum): add unit steps and refactor integration to PostgreSQL` → push
- [ ] **2.11.3** Create `docker-compose.integration.yml` (postgres + Rust test runner)
- [ ] **2.11.4** Create `Dockerfile.integration` (rust:latest base image)
  - [ ] Commit: `ci(demo-be-rust-axum): add docker-compose and Dockerfile for integration tests` → push
- [ ] **2.11.5** Update `project.json` — `test:unit`, `test:integration`, `test:quick` targets with caching inputs
- [ ] **2.11.6** Update README — test architecture, commands, docker-compose docs
  - [ ] Commit: `chore(demo-be-rust-axum): update project.json targets and README` → push
- [ ] **2.11.7** Verify ≥90% coverage from `test:unit` alone + all 76 scenarios pass at both levels

### Phase 3: Verify and Adapt Non-Demo-be Projects

No code changes expected for most. Verify compliance, adapt where needed.

- [ ] **3.1 `organiclever-web`** (requires changes — split Vitest suites)
  - [ ] Separate unit tests from MSW integration tests into distinct test configurations or directories
  - [ ] Configure `test:unit` Nx target to run unit tests only (no MSW), with coverage output
  - [ ] Configure `test:integration` Nx target to run MSW integration tests
  - [ ] Update `test:quick` to run `test:unit` + coverage check only (no MSW tests)
  - [ ] Verify ≥90% coverage from unit tests alone — add unit tests if coverage drops below threshold
  - [ ] Run `nx run organiclever-web:test:quick` — confirm passes
  - [ ] Commit: `refactor(organiclever-web): split unit and MSW integration test suites` → push
- [ ] **3.2 `organiclever-web-e2e`**
  - [ ] Verify `test:e2e` target exists and runs Playwright tests
  - [ ] Verify `test:quick` target runs bddgen only (no actual test execution)
  - [ ] Run `nx run organiclever-web-e2e:test:quick` — confirm passes
- [ ] **3.3 `oseplatform-web`**
  - [ ] Verify `test:quick` target runs link validation only
  - [ ] Run `nx run oseplatform-web:test:quick` — confirm passes
- [ ] **3.4 `ayokoding-web`**
  - [ ] Verify `test:quick` target runs link validation only
  - [ ] Run `nx run ayokoding-web:test:quick` — confirm passes
- [ ] **3.5 `ayokoding-cli`** (may require changes — coverage from unit tests only)
  - [ ] Verify `test:unit` target runs Go unit tests (excluding Godog BDD files)
  - [ ] Verify `test:integration` target runs Godog BDD tests
  - [ ] Update `test:quick` to run `test:unit` + coverage check only (no Godog)
  - [ ] Verify ≥90% coverage from unit tests alone — add unit tests if coverage drops below threshold
  - [ ] Run `nx run ayokoding-cli:test:quick` — confirm passes
  - [ ] Commit: `refactor(ayokoding-cli): separate unit and Godog integration test targets` → push
- [ ] **3.6 `oseplatform-cli`** (may require changes — coverage from unit tests only)
  - [ ] Same as 3.5 but for oseplatform-cli
  - [ ] Run `nx run oseplatform-cli:test:quick` — confirm passes
  - [ ] Commit: `refactor(oseplatform-cli): separate unit and Godog integration test targets` → push
- [ ] **3.7 `rhino-cli`** (may require changes — coverage from unit tests only)
  - [ ] Same as 3.5 but for rhino-cli
  - [ ] Run `nx run rhino-cli:test:quick` — confirm passes
  - [ ] Commit: `refactor(rhino-cli): separate unit and Godog integration test targets` → push
- [ ] **3.8 `golang-commons`**
  - [ ] Verify `test:unit` target runs Go unit tests (Godog integration is optional, already exists)
  - [ ] Verify `test:quick` runs `test:unit` + coverage check
  - [ ] Verify ≥90% coverage from unit tests alone
  - [ ] Run `nx run golang-commons:test:quick` — confirm passes
- [ ] **3.9 `hugo-commons`**
  - [ ] Same as 3.8 but for hugo-commons
  - [ ] Run `nx run hugo-commons:test:quick` — confirm passes
- [ ] **3.10 `elixir-cabbage`**
  - [ ] Verify `test:unit` target runs ExUnit tests
  - [ ] Verify `test:quick` runs `test:unit` + coverage check
  - [ ] Run `nx run elixir-cabbage:test:quick` — confirm passes
- [ ] **3.11 `elixir-gherkin`**
  - [ ] Same as 3.10 but for elixir-gherkin
  - [ ] Run `nx run elixir-gherkin:test:quick` — confirm passes
- [ ] **3.12 `demo-be-e2e`**
  - [ ] Verify `test:e2e` target exists and runs Playwright tests
  - [ ] Verify `test:quick` target runs bddgen (no actual test execution)
  - [ ] Run `nx run demo-be-e2e:test:quick` — confirm passes
- [ ] Commit (if any non-demo-be project.json changes needed): `chore: verify and adapt non-demo-be project test targets` → push

### Phase 4: CI Workflows and README Badges

- [ ] **4.1 Update `main-ci.yml`**
  - [ ] Remove `mvn jacoco:report -Pintegration` step (line 110) — no longer needed
  - [ ] Update each coverage upload `files:` path to point to unit test coverage output (11 backends — paths depend on per-backend configuration in Phase 2)
  - [ ] Verify no other steps depend on integration test output
  - [ ] Commit: `ci: update main-ci.yml coverage paths for unit test output` → push
- [ ] **4.2 Create `integration-ci.yml`**
  - [ ] Create workflow file with cron schedule: `0 21 * * *`, `0 3 * * *`, `0 9 * * *`, `0 15 * * *` (WIB 04, 10, 16, 22)
  - [ ] Add `workflow_dispatch` trigger for manual runs
  - [ ] Add all language runtime setup steps (same as `main-ci.yml`: Volta, Go, Java 21+25, Elixir, .NET, Python, uv, Rust, Hugo, Clojure CLI, clj-kondo)
  - [ ] Add `npm ci` and language-specific dependency install steps
  - [ ] Run `npx nx run-many -t test:integration --all`
  - [ ] Commit: `ci: add integration-ci.yml scheduled workflow (4x daily)` → push
  - [ ] Trigger manually to verify
- [ ] **4.3 Verify `pr-quality-gate.yml`**
  - [ ] Confirm it runs `nx affected -t typecheck`, `nx affected -t lint`, `nx affected -t test:quick` as separate steps
  - [ ] Confirm no changes needed (already compliant)
- [ ] **4.4 Verify E2E workflows (12 files)**
  - [ ] Confirm all `e2e-demo-be-*.yml` and `e2e-organiclever-web.yml` use cron `0 23 * * *` and `0 11 * * *` (WIB 06/18)
  - [ ] Confirm no changes needed (already compliant)
- [ ] **4.5 Update root `README.md`**
  - [ ] Add **Integration** badge column to CI & Test Coverage table for demo-be backends (from `integration-ci.yml`)
  - [ ] Add Integration badge row for `organiclever-web` and Go CLI apps
  - [ ] Update table headers: Integration (4x daily) | E2E (2x daily) | Coverage
  - [ ] Add description text explaining CI schedule (integration 4x daily, E2E 2x daily, test:quick on every push/PR)
  - [ ] Commit: `docs: add integration CI badges and update README test coverage section` → push

### Phase 5: Final Verification

Run all targets end-to-end and confirm the full system works. No commits in this phase — verification only.

- [ ] **5.1 Demo-be backends — all three levels**
  - [ ] Run `nx run-many -t test:unit --projects=demo-be-*` — all 11 backends pass
  - [ ] Run `nx run-many -t test:integration --projects=demo-be-*` — all 11 backends pass with PostgreSQL
  - [ ] Trigger E2E workflows manually — all 11 backends pass via Playwright
- [ ] **5.2 Non-demo-be projects**
  - [ ] Run `nx run-many -t test:quick --all` — all projects pass
  - [ ] Run `nx run organiclever-web:test:integration` — MSW tests pass
  - [ ] Run `nx run ayokoding-cli:test:integration` — Godog BDD tests pass
  - [ ] Run `nx run oseplatform-cli:test:integration` — Godog BDD tests pass
  - [ ] Run `nx run rhino-cli:test:integration` — Godog BDD tests pass
- [ ] **5.3 Coverage**
  - [ ] Verify ≥90% line coverage for all testable projects from `test:unit` output alone
  - [ ] Verify `main-ci.yml` coverage uploads point to correct files
- [ ] **5.4 Nx caching**
  - [ ] Run `nx run demo-be-java-springboot:test:unit` twice — second run served from cache
  - [ ] Run `nx run demo-be-java-springboot:test:integration` twice — second run always re-runs (NOT cached)
  - [ ] Modify a source file — verify `test:unit` cache is invalidated
  - [ ] Modify a Gherkin spec file — verify `test:unit` cache is invalidated
- [ ] **5.5 Mandatory targets audit**
  - [ ] Verify each API backend has: `test:unit`, `test:integration`, `test:quick`, `lint`, `build`
  - [ ] Verify each Web UI app has: `test:unit`, `test:integration`, `test:quick`, `lint`, `build`
  - [ ] Verify each CLI app has: `test:unit`, `test:integration`, `test:quick`, `lint`, `build`
  - [ ] Verify each library has: `test:unit`, `test:quick`, `lint`
  - [ ] Verify each Hugo site has: `test:quick`, `build`
  - [ ] Verify each E2E runner has: `test:e2e`, `test:quick`, `lint`
- [ ] **5.6 CI schedule verification**
  - [ ] Trigger `integration-ci.yml` manually — all integration tests pass
  - [ ] Confirm cron schedule is set to WIB 04, 10, 16, 22 (UTC 21, 03, 09, 15)
  - [ ] Confirm E2E workflows run at WIB 06, 18 (UTC 23, 11)
  - [ ] Confirm `main-ci.yml` runs `test:quick` (not integration/e2e) on push to main
  - [ ] Confirm `pr-quality-gate.yml` runs typecheck + lint + test:quick on PRs
