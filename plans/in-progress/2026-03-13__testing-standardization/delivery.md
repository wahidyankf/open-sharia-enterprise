# Delivery Checklist: Testing Standardization

## Commit and Push Strategy

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

## Phase 1: Update Standards and Documentation

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

## Phase 2: Demo-be Backend Implementations

Implement one backend at a time. Each backend follows 7 steps. The first backend (`demo-be-java-springboot`) sets the reference pattern. Complete one backend fully before starting the next.

### 2.1 `demo-be-java-springboot` (reference implementation)

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

### 2.2 `demo-be-kotlin-ktor` (JVM — replace SQLite → PostgreSQL)

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

### 2.3 `demo-be-java-vertx` (JVM — add PostgreSQL)

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

### 2.4 `demo-be-fsharp-giraffe` (.NET — replace SQLite → PostgreSQL)

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

### 2.5 `demo-be-csharp-aspnetcore` (.NET — replace SQLite → PostgreSQL)

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

### 2.6 `demo-be-golang-gin` (Go — add PostgreSQL)

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

### 2.7 `demo-be-ts-effect` (Node.js — add PostgreSQL)

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

### 2.8 `demo-be-python-fastapi` (Python — add PostgreSQL)

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

### 2.9 `demo-be-clojure-pedestal` (JVM/Clojure — replace SQLite → PostgreSQL)

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

### 2.10 `demo-be-elixir-phoenix` (Elixir/OTP — add PostgreSQL via Ecto)

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

### 2.11 `demo-be-rust-axum` (Rust — add PostgreSQL)

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

## Phase 3: Verify and Adapt Non-Demo-be Projects

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

## Phase 4: CI Workflows and README Badges

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

## Phase 5: Final Verification

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
