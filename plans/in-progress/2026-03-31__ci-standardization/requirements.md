# Requirements: CI/CD Standardization

## Current State Audit

### R1: Git Hooks

#### R1.1: Pre-Commit Hook (9 Steps)

**Current Implementation** (`.husky/pre-commit`):

| Step | Action                                    | Condition               | Notes                                 |
| ---- | ----------------------------------------- | ----------------------- | ------------------------------------- |
| 1    | Validate `.claude/` + `.opencode/` config | If config files staged  | rhino-cli agent sync                  |
| 2    | Validate docker-compose files             | If compose files staged | `docker compose config`               |
| 3    | Run `nx affected -t run-pre-commit`       | Always                  | Per-project pre-commit hooks          |
| 4    | Auto-add ayokoding-web content            | Always                  | `git add apps/ayokoding-web/content/` |
| 5    | Run lint-staged                           | Always                  | Prettier, gofmt, fantomas             |
| 6    | Run `mix format` for Elixir               | If `.ex`/`.exs` staged  | From project root                     |
| 7    | Validate docs naming + auto-fix           | Always                  | rhino-cli docs validate-naming        |
| 8    | Validate markdown links                   | If `.md` staged         | rhino-cli docs validate-links         |
| 9    | Run `npm run lint:md`                     | Always                  | markdownlint-cli2                     |

**Gaps**:

- Step 4 (auto-add ayokoding-web content) is app-specific logic in a global hook
- Step 6 (Elixir formatting) is language-specific logic that should be in lint-staged or per-project
- No timeout protection -- a hung step blocks the entire commit
- Steps 7-9 all touch markdown but run independently -- could be consolidated

#### R1.2: Commit-Msg Hook

**Current**: `npx commitlint --edit` with `@commitlint/config-conventional`

**Status**: Standardized. No changes needed.

#### R1.3: Pre-Push Hook

**Current**:

```bash
PARALLEL=$(( $(nproc 2>/dev/null || sysctl -n hw.ncpu 2>/dev/null || echo 4) - 1 ))
npx nx affected -t typecheck lint test:quick --parallel="$PARALLEL"
npm run lint:md
```

**Gaps**:

- No mechanism to skip on `--no-verify` (intentional -- prevents bypass)
- Markdown linting runs on ALL files, not just affected -- slow on large repos
- No progress feedback during long runs (can take several minutes)
- Missing spec-coverage validation

### R2: Formatting, Typecheck, Lint, test:quick

#### R2.1: Formatting Tools by Language

| Language               | Tool                       | Trigger                  | Config                         |
| ---------------------- | -------------------------- | ------------------------ | ------------------------------ |
| JS/TS/JSON/YAML/CSS/MD | Prettier 3.6.2             | lint-staged (pre-commit) | `.prettierrc.json`             |
| Go                     | gofmt                      | lint-staged (pre-commit) | Built-in                       |
| F#                     | Fantomas                   | lint-staged (pre-commit) | `.fantomasrc.json` (if exists) |
| Elixir                 | mix format                 | Pre-commit step 6        | `.formatter.exs`               |
| Java                   | Checkstyle (lint only)     | `nx lint`                | `checkstyle.xml`               |
| Kotlin                 | ktlint (via Gradle)        | `nx lint`                | Built-in                       |
| Python                 | (not formatted on commit)  | Manual                   | N/A                            |
| Rust                   | rustfmt (not in hooks)     | Manual                   | `rustfmt.toml`                 |
| C#                     | (not formatted on commit)  | Manual                   | `.editorconfig`                |
| Clojure                | cljfmt (not in hooks)      | Manual                   | N/A                            |
| Dart                   | dart format (not in hooks) | Manual                   | Built-in                       |

**Gap**: Python, Rust, C#, Clojure, and Dart lack automated formatting in pre-commit hooks.

#### R2.2: Typecheck Tools by Language

| Language   | Tool             | Nx Target      | Notes                           |
| ---------- | ---------------- | -------------- | ------------------------------- |
| TypeScript | tsc / oxlint     | `typecheck`    | Various tsconfig setups         |
| Go         | go vet           | Part of `lint` | Integrated with golangci-lint   |
| Java       | javac (Maven)    | `typecheck`    | `mvn compile -q`                |
| Kotlin     | kotlinc (Gradle) | `typecheck`    | `./gradlew compileKotlin`       |
| F#         | dotnet build     | `typecheck`    | MSBuild with warnings-as-errors |
| C#         | dotnet build     | `typecheck`    | MSBuild with warnings-as-errors |
| Python     | mypy/pyright     | `typecheck`    | Type checking via strict mode   |
| Rust       | cargo check      | `typecheck`    | Part of Rust toolchain          |
| Elixir     | mix compile      | `typecheck`    | `--warnings-as-errors`          |
| Clojure    | clj-kondo        | `typecheck`    | Static analysis                 |
| Dart       | dart analyze     | `typecheck`    | Strict mode                     |

**Status**: Standardized across all projects via Nx targets.

#### R2.3: Lint Tools by Language

| Language   | Tool                              | Nx Target |
| ---------- | --------------------------------- | --------- |
| TypeScript | oxlint                            | `lint`    |
| Go         | golangci-lint                     | `lint`    |
| Java       | Checkstyle + PMD                  | `lint`    |
| Kotlin     | ktlint + detekt                   | `lint`    |
| F#         | fsharplint + analyzers            | `lint`    |
| C#         | dotnet format --verify-no-changes | `lint`    |
| Python     | ruff                              | `lint`    |
| Rust       | clippy                            | `lint`    |
| Elixir     | credo                             | `lint`    |
| Clojure    | clj-kondo + eastwood              | `lint`    |
| Dart       | dart analyze                      | `lint`    |

**Status**: Standardized across all projects via Nx targets.

#### R2.4: test:quick Composition

`test:quick` = `test:unit` + coverage validation (via `rhino-cli test-coverage validate`)

| Category          | Coverage Threshold | Apps                                      |
| ----------------- | ------------------ | ----------------------------------------- |
| Backends (BE)     | 90%                | 11 demo backends, organiclever-be         |
| CLIs              | 90%                | rhino-cli, ayokoding-cli, oseplatform-cli |
| Content platforms | 80%                | ayokoding-web, oseplatform-web            |
| Fullstack (FS)    | 75%                | a-demo-fs-ts-nextjs                       |
| Frontends (FE)    | 70%                | 3 demo frontends, organiclever-fe         |

**Gap**: Coverage threshold rationale is undocumented. Needs explicit justification in governance.

### R3: Three-Level Testing

#### R3.1: Backend Testing (11 implementations)

All demo backends share Gherkin specs from `specs/apps/a-demo/be/gherkin/` (14 feature files,
78 scenarios).

| Level       | Real DB          | Real HTTP            | Gherkin | Docker | Cacheable |
| ----------- | ---------------- | -------------------- | ------- | ------ | --------- |
| Unit        | No               | No                   | Yes     | No     | Yes       |
| Integration | Yes (PostgreSQL) | **No**               | Yes     | Yes    | No        |
| E2E         | Yes (PostgreSQL) | **Yes** (Playwright) | Yes     | Yes    | No        |

**Key constraint**: Integration tests call service functions directly -- no HTTP layer. This is
intentional to isolate database interaction testing from HTTP routing concerns.

**Gap**: No `Dockerfile.integration` template documented. Each backend has its own with slight
variations.

#### R3.2: Frontend Testing (3 implementations)

All demo frontends share Gherkin specs from `specs/apps/a-demo/fe/gherkin/`.

| Level       | API Mocking  | Browser         | Gherkin | Docker | Cacheable        |
| ----------- | ------------ | --------------- | ------- | ------ | ---------------- |
| Unit        | MSW          | JSDOM/happy-dom | No      | No     | Yes              |
| Integration | MSW          | JSDOM/happy-dom | No      | No     | Yes (overridden) |
| E2E         | Real backend | Playwright      | Yes     | Yes    | No               |

**Gap**: Frontend unit/integration tests do not consume Gherkin specs (only E2E does).

#### R3.3: Fullstack Testing (1 implementation)

`a-demo-fs-ts-nextjs` combines backend and frontend in a single Next.js app.

| Level       | Real DB | Browser    | Gherkin        | Docker | Cacheable |
| ----------- | ------- | ---------- | -------------- | ------ | --------- |
| Unit        | No      | JSDOM      | Yes (BE specs) | No     | Yes       |
| Integration | Yes     | No         | Yes            | Yes    | No        |
| E2E         | Yes     | Playwright | Yes            | Yes    | No        |

**Gap**: No dedicated E2E workflow in GitHub Actions for fullstack app. Currently tested as part
of frontend E2E.

#### R3.4: CLI Testing (3 implementations)

All CLIs written in Go, using godog for Gherkin BDD.

| Level       | Real Filesystem       | Gherkin     | Docker | Cacheable        |
| ----------- | --------------------- | ----------- | ------ | ---------------- |
| Unit        | No (mocked I/O)       | Yes (godog) | No     | Yes              |
| Integration | Yes (`/tmp` fixtures) | Yes (godog) | No     | Yes (overridden) |
| E2E         | N/A                   | N/A         | N/A    | N/A              |

**Status**: Well-standardized. No Docker needed for CLI testing.

#### R3.5: Content Platform Testing (2 implementations)

ayokoding-web and oseplatform-web (Next.js 16).

| Level          | Real API    | Browser    | Docker | Cacheable |
| -------------- | ----------- | ---------- | ------ | --------- |
| Unit (BE + FE) | No          | JSDOM      | No     | Yes       |
| Integration    | Real tRPC   | No         | No     | Yes       |
| E2E (BE)       | Real tRPC   | No         | Yes    | No        |
| E2E (FE)       | Real server | Playwright | Yes    | No        |

#### R3.6: OrganicLever Testing

| Component       | Level       | Real DB          | Docker | Cacheable |
| --------------- | ----------- | ---------------- | ------ | --------- |
| organiclever-be | Unit        | No               | No     | Yes       |
| organiclever-be | Integration | Yes (PostgreSQL) | Yes    | No        |
| organiclever-be | E2E         | Yes              | Yes    | No        |
| organiclever-fe | Unit        | No (MSW)         | No     | Yes       |
| organiclever-fe | Integration | No (MSW)         | No     | Yes       |
| organiclever-fe | E2E         | Yes (full stack) | Yes    | No        |

### R4: Specs Folder Structure

#### R4.1: Current Structure

```
specs/
├── apps/
│   ├── a-demo/
│   │   ├── be/gherkin/          # 14 feature files, 78 scenarios
│   │   ├── fe/gherkin/          # Frontend Gherkin specs
│   │   └── contracts/           # OpenAPI 3.1 spec + generated output
│   ├── organiclever/
│   │   ├── be/gherkin/
│   │   ├── fe/gherkin/
│   │   ├── contracts/
│   │   └── c4/                  # C4 architecture diagrams
│   ├── ayokoding/
│   │   ├── be/gherkin/
│   │   ├── fe/gherkin/
│   │   ├── c4/
│   │   └── build-tools/gherkin/
│   ├── oseplatform/
│   │   ├── be/gherkin/
│   │   ├── fe/gherkin/
│   │   └── c4/
│   ├── rhino-cli/               # 15 feature files across 10 domains
│   ├── ayokoding-cli/
│   └── oseplatform-cli/
├── libs/
│   ├── golang-commons/
│   ├── hugo-commons/
│   └── ts-ui/gherkin/
└── apps-labs/                   # Empty (future)
```

**Gaps**:

- `a-demo/` lacks `c4/` directory (other apps have it)
- CLI specs use inconsistent subdirectory structure (flat domains vs `gherkin/` nesting)
- No `specs/apps/a-demo/fs/gherkin/` for fullstack-specific specs
- No README.md in several spec subdirectories
- `apps-labs/` exists but is empty with no convention documented

#### R4.2: Contract Specs

Both `a-demo-contracts` and `organiclever-contracts` follow the same pattern:

```
specs/apps/{app}/contracts/
├── openapi.yaml               # Entry point
├── paths/                     # Endpoint definitions
├── schemas/                   # Data models
├── examples/                  # Request/response examples
├── generated/
│   ├── openapi-bundled.yaml
│   ├── openapi-bundled.json
│   └── docs/
├── .spectral.yaml             # Validation rules
└── project.json               # Nx targets: bundle, lint, docs
```

**Status**: Well-standardized. Contract codegen is a dependency of `typecheck` and `build`.

### R5: GitHub Actions Workflows

#### R5.1: Current Workflow Inventory

| Workflow                               | Trigger               | Jobs                    | Runtime(s)             |
| -------------------------------------- | --------------------- | ----------------------- | ---------------------- |
| `pr-quality-gate.yml`                  | PR (open/sync/reopen) | 1 monolithic            | 13+ runtimes           |
| `pr-format.yml`                        | PR                    | 1                       | Node.js                |
| `pr-validate-links.yml`                | PR                    | 1                       | Go                     |
| `codecov-upload.yml`                   | Push to main          | 1                       | 13+ runtimes           |
| `test-a-demo-be-golang-gin.yml`        | Cron 2x daily         | 2 (integration, e2e)    | Go + Node.js           |
| `test-a-demo-be-java-springboot.yml`   | Cron 2x daily         | 2                       | Java + Node.js         |
| `test-a-demo-be-java-vertx.yml`        | Cron 2x daily         | 2                       | Java + Node.js         |
| `test-a-demo-be-python-fastapi.yml`    | Cron 2x daily         | 2                       | Python + Node.js       |
| `test-a-demo-be-rust-axum.yml`         | Cron 2x daily         | 2                       | Rust + Node.js         |
| `test-a-demo-be-kotlin-ktor.yml`       | Cron 2x daily         | 2                       | Kotlin + Node.js       |
| `test-a-demo-be-fsharp-giraffe.yml`    | Cron 2x daily         | 2                       | .NET + Node.js         |
| `test-a-demo-be-csharp-aspnetcore.yml` | Cron 2x daily         | 2                       | .NET + Node.js         |
| `test-a-demo-be-clojure-pedestal.yml`  | Cron 2x daily         | 2                       | Clojure + Node.js      |
| `test-a-demo-be-elixir-phoenix.yml`    | Cron 2x daily         | 2                       | Elixir + Node.js       |
| `test-a-demo-be-ts-effect.yml`         | Cron 2x daily         | 2                       | Node.js                |
| `test-a-demo-fe-ts-nextjs.yml`         | Cron 2x daily         | 1                       | Node.js + Go           |
| `test-a-demo-fe-dart-flutterweb.yml`   | Cron 2x daily         | 1                       | Flutter + Go + Node.js |
| `test-a-demo-fe-ts-tanstack-start.yml` | Cron 2x daily         | 1                       | Node.js + Go           |
| `test-a-demo-fs-ts-nextjs.yml`         | Cron 2x daily         | 1                       | Node.js                |
| `test-organiclever.yml`                | Cron 2x daily         | 3 (BE int, FE int, E2E) | .NET + Node.js         |
| `test-and-deploy-ayokoding-web.yml`    | Push to main          | 4                       | Node.js + Go           |
| `test-and-deploy-oseplatform-web.yml`  | Push to main          | 4                       | Node.js + Go           |

**Total**: 22 workflows, ~4,500 lines of YAML

#### R5.2: Duplication Analysis

The 11 backend test workflows share this identical structure:

```
1. Checkout code
2. Setup language runtime (varies)
3. Setup Node.js + npm install
4. Generate contract types
5. Run integration tests via docker-compose
6. Checkout code (E2E job)
7. Setup Node.js + Go + npm install
8. Start full stack via docker-compose
9. Wait for health endpoints
10. Install Playwright + run E2E
11. Upload artifacts
12. Teardown docker-compose
```

Only steps 2 and the docker-compose file paths vary between backends. This is **~130 lines of
duplicated YAML per workflow** = ~1,430 lines of duplication across 11 workflows.

#### R5.3: PR Quality Gate Bottleneck

The current PR quality gate installs ALL runtimes in a single job:

- Go 1.26
- Java 21 + Java 25 (two JDKs)
- .NET 10
- Elixir 1.19 + Erlang/OTP 27
- Python 3.13
- Rust (latest stable)
- Flutter (stable)
- Clojure CLI
- golangci-lint, oapi-codegen, datamodel-code-generator, Fantomas, dotnet-fsharplint

**Problem**: A PR touching only `organiclever-fe` (TypeScript) still installs Go, Java, .NET,
Elixir, Python, Rust, Flutter, and Clojure. This wastes ~5-8 minutes of setup time.

**Solution**: Split into parallel, language-scoped jobs. Use `nx show projects --affected` to
determine which language families are needed, then only run relevant setup jobs.

### R6: Docker Infrastructure

#### R6.1: Development Docker Compose (`infra/dev/`)

All dev compose files follow this pattern:

```yaml
services:
  postgres:
    image: postgres:17-alpine
    ports: ["5432:5432"]
    volumes: ["{app}-data:/var/lib/postgresql/data"]
    healthcheck:
      test: ["CMD-SHELL", "pg_isready -U {user}"]

  { app }:
    build:
      context: ../../../
      dockerfile: infra/dev/{app}/Dockerfile.be.dev
    ports: ["{port}:{port}"]
    volumes:
      - ../../../apps/{app}:/app/apps/{app} # Source mount for hot-reload
      - ../../../specs:/app/specs:ro # Gherkin specs
      - ../../../generated-contracts:/app/generated-contracts:ro
    depends_on:
      postgres: { condition: service_healthy }
    environment:
      DATABASE_URL: "postgres://{user}:{pass}@postgres:5432/{db}"
```

**Hot-reload mechanisms by language**:

| Language           | Mechanism               | Tool                                |
| ------------------ | ----------------------- | ----------------------------------- |
| Go                 | File watcher + rebuild  | `air` or `go run` with volume mount |
| Java (Spring Boot) | Spring DevTools         | `mvn spring-boot:run`               |
| Java (Vert.x)      | Vert.x launcher         | `mvn exec:java` with redeploy       |
| TypeScript         | tsx watch               | `npx tsx watch`                     |
| Python             | uvicorn reload          | `uvicorn --reload`                  |
| Rust               | cargo watch             | `cargo watch -x run`                |
| Kotlin             | Gradle continuous       | `./gradlew run --continuous`        |
| F#                 | dotnet watch            | `dotnet watch run`                  |
| C#                 | dotnet watch            | `dotnet watch run`                  |
| Elixir             | Phoenix code reloader   | `mix phx.server`                    |
| Clojure            | nREPL + tools.namespace | `clojure -M:dev`                    |

#### R6.2: Integration Test Docker (`apps/*/docker-compose.integration.yml`)

All integration compose files follow this pattern:

```yaml
services:
  postgres:
    image: postgres:17-alpine
    tmpfs: /var/lib/postgresql/data # Ephemeral for speed
    healthcheck: ...

  test-runner:
    build:
      context: .
      dockerfile: Dockerfile.integration
    depends_on:
      postgres: { condition: service_healthy }
    volumes:
      - ../../specs:/specs:ro # Mount Gherkin specs
    environment:
      DATABASE_URL: "..."
```

**Gap**: `Dockerfile.integration` files are not templated. Each backend has its own with
language-specific build and test commands.

#### R6.3: CI Overlay Docker Compose (`infra/dev/*/docker-compose.ci.yml`)

CI overlays extend dev compose files with:

- Production environment variables (`GIN_MODE=release`, `NODE_ENV=production`, etc.)
- `ENABLE_TEST_API=true` flag for test-only endpoints
- Frontend service added (for full-stack E2E)
- Hardcoded credentials (test-only)

#### R6.4: Dockerfile Patterns

**Production Dockerfiles** (multi-stage):

| Stage      | Purpose                              | Base Image Pattern                            |
| ---------- | ------------------------------------ | --------------------------------------------- |
| deps/build | Install dependencies + compile       | `{language}:{version}-alpine`                 |
| runtime    | Minimal runtime with built artifacts | `alpine:3.22` or `{runtime}:{version}-alpine` |

**Standardization needs**:

- All should use non-root users (some don't)
- Health check commands vary (curl vs wget vs none)
- `.dockerignore` is at repo root, not per-app
- No LABEL metadata (version, maintainer, etc.)

### R7: Missing CI Coverage

| Gap                                                      | Impact                                | Priority |
| -------------------------------------------------------- | ------------------------------------- | -------- |
| spec-coverage validation not in CI                       | Specs can drift from tests undetected | High     |
| Python/Rust/C#/Clojure/Dart not auto-formatted on commit | Manual formatting burden              | Medium   |
| No Docker layer caching in CI                            | Slow integration/E2E test cycles      | Medium   |
| Frontend unit/integration tests don't consume Gherkin    | Spec coverage gap                     | Low      |

## Acceptance Criteria

### AC1: Git Hooks

```gherkin
Feature: Standardized Git Hooks

  Scenario: Pre-commit validates staged files efficiently
    Given a developer has staged files for commit
    When the pre-commit hook runs
    Then only relevant formatters execute for the staged file types
    And language-specific formatting is handled by lint-staged (not separate hook steps)
    And the hook completes within 30 seconds for typical changes

  Scenario: Pre-push runs cacheable quality gates
    Given a developer pushes to the remote
    When the pre-push hook runs
    Then typecheck, lint, and test:quick run for affected projects only
    And Nx caching is used to skip unchanged projects
    And markdown linting runs only on affected markdown files

  Scenario: All languages have automated formatting
    Given a developer stages files in any supported language
    When the pre-commit hook runs
    Then the files are auto-formatted using the language-appropriate tool
    And the formatted files are re-staged automatically
```

### AC2: PR Quality Gate

```gherkin
Feature: Optimized PR Quality Gate

  Scenario: Language-scoped parallel jobs
    Given a PR that only modifies TypeScript files
    When the PR quality gate workflow runs
    Then only the Node.js runtime is installed
    And Go, Java, .NET, Elixir, Python, Rust, Flutter, and Clojure setup is skipped
    And the workflow completes faster than the current monolithic approach

  Scenario: Full quality gate on cross-language PR
    Given a PR that modifies both Go and TypeScript files
    When the PR quality gate workflow runs
    Then both Go and Node.js runtimes are installed in parallel jobs
    And typecheck, lint, and test:quick run for affected projects
    And results from all parallel jobs are aggregated
```

### AC3: Consolidated Backend Test Workflows

```gherkin
Feature: Consolidated Backend Test Workflows

  Scenario: Single workflow tests all backends via matrix
    Given the consolidated backend test workflow
    When triggered on schedule (2x daily)
    Then all 11 backends are tested via a matrix strategy
    And each matrix entry uses the appropriate composite action for language setup
    And integration tests run before E2E tests (sequential dependency)
    And test artifacts are uploaded per backend

  Scenario: Backend test workflow can be triggered manually
    Given a developer wants to test a specific backend
    When they trigger the workflow via workflow_dispatch
    Then they can select which backend(s) to test
    And only the selected backends are tested
```

### AC4: Docker Standardization

```gherkin
Feature: Standardized Docker Infrastructure

  Scenario: All Dockerfiles follow the template
    Given the Dockerfile template in governance documentation
    When a new backend is added
    Then the Dockerfile follows multi-stage build pattern
    And uses non-root user in runtime stage
    And includes LABEL metadata
    And has a HEALTHCHECK instruction

  Scenario: Integration test Dockerfiles are templated
    Given the Dockerfile.integration template
    When a new backend needs integration tests
    Then the template provides language-specific build and test commands
    And specs are mounted read-only from the host
    And the test runner exits with appropriate exit code

  Scenario: Docker layer caching speeds up CI
    Given the CI workflows use Docker
    When integration or E2E tests run
    Then Docker layers are cached between workflow runs
    And subsequent runs reuse cached layers when Dockerfiles haven't changed
```

### AC5: Local Development with Docker

```gherkin
Feature: Local Development with Docker Compose

  Scenario: Developer starts any backend with hot-reload
    Given a developer wants to work on a specific backend
    When they run the documented docker-compose dev command
    Then the backend starts with hot-reload enabled
    And source code changes are reflected without container restart
    And the database is seeded with initial data
    And the developer can access the service at the documented port

  Scenario: Developer starts full stack locally
    Given a developer wants to run backend + frontend + database
    When they run the full-stack docker-compose command
    Then all services start with appropriate dependencies
    And health checks ensure services are ready before dependent services start
    And hot-reload works for both backend and frontend
```

### AC6: Spec-Coverage Integration

```gherkin
Feature: Spec-Coverage Validation in CI

  Scenario: CI validates spec-to-test mapping
    Given the spec-coverage validation is integrated into CI
    When a developer adds a new Gherkin scenario without implementing it
    Then the CI pipeline flags the unimplemented scenario
    And the build fails with a clear error message

  Scenario: Spec-coverage runs for all testable projects
    Given all projects that consume Gherkin specs
    When the CI pipeline runs
    Then spec-coverage validation runs for each project
    And coverage gaps are reported per project
```

### AC7: Governance Documentation

```gherkin
Feature: CI Conventions Documentation

  Scenario: CI conventions are documented
    Given the governance documentation
    When a developer looks for CI/CD guidance
    Then they find a comprehensive CI conventions document
    And it covers git hooks, GitHub Actions, Docker, and testing standards
    And it includes decision matrices for choosing testing strategies
    And it documents coverage threshold rationale

  Scenario: Adding a new app follows documented process
    Given the CI conventions documentation
    When a developer adds a new backend in a new language
    Then they can follow the documented checklist
    And the checklist covers: Dockerfile, docker-compose, workflow, specs, coverage
    And the new app integrates into the existing CI pipeline
```
