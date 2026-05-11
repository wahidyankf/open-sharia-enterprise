# Product Requirements Document — organiclever-be-java-migration

## Product Overview

This plan delivers a Java/Spring Boot 4.0 replacement for the `organiclever-be` REST API
backend, preserving the existing `GET /api/v1/health` contract and all Nx quality-gate
targets while eliminating all F#/Giraffe/.NET artifacts from the codebase. The result is a
production-quality Java backend aligned with the ose-primer reference implementation
(`crud-be-java-springboot`) and ready for future feature additions (auth, CRUD, DB).

## Personas

| Persona      | Hat                                          | Relationship to this plan                                                           |
| ------------ | -------------------------------------------- | ----------------------------------------------------------------------------------- |
| BE Developer | Solo maintainer wearing backend engineer hat | Executes delivery checklist; writes Java source, pom.xml, Cucumber step definitions |
| Tech Lead    | Solo maintainer wearing architecture hat     | Reviews Nx target design, NullAway config, JaCoCo exclusion decisions               |

Consuming agents: `plan-execution` workflow executes delivery checklist; `swe-java-dev`
handles Java source creation steps; `plan-execution-checker` validates completed work.

## User Stories

- As a BE Developer, I want a Spring Boot health endpoint so that the app exposes its status
  via the established `/api/v1/health` contract without requiring any changes to
  `organiclever-web` or `organiclever-be-e2e`.
- As a Tech Lead, I want Maven-based Nx targets (`build`, `typecheck`, `lint`, `test:quick`,
  `test:integration`, `codegen`, `spec-coverage`) so that CI quality gates work identically
  to other Java projects in the monorepo without custom scripting.

## Functional Requirements

| ID   | Requirement                                                          |
| ---- | -------------------------------------------------------------------- |
| FR-1 | `GET /api/v1/health` returns HTTP 200 with body `{"status":"UP"}`    |
| FR-2 | Anonymous access to `/api/v1/health` is permitted (no auth required) |
| FR-3 | Health response does not expose internal component details           |
| FR-4 | Application starts on port 8202                                      |
| FR-5 | CORS allows any origin/method/header (matches v0 F# behaviour)       |

## Non-Functional Requirements

| ID    | Requirement                                                                                       |
| ----- | ------------------------------------------------------------------------------------------------- |
| NFR-1 | Maven build completes in a clean environment without .NET toolchain                               |
| NFR-2 | Unit test coverage ≥90% (JaCoCo line coverage, `target/site/jacoco/jacoco.xml`)                   |
| NFR-3 | Zero Checkstyle violations (Google Java Style subset)                                             |
| NFR-4 | Zero PMD violations (configured ruleset)                                                          |
| NFR-5 | Zero NullAway violations in `com.organicleverbe` package (JSpecify annotations)                   |
| NFR-6 | All Gherkin scenarios in `specs/apps/organiclever/behavior/be/gherkin/` have step implementations |
| NFR-7 | Docker-based integration runner exits 0                                                           |
| NFR-8 | No F#/dotnet artifacts remain in `apps/organiclever-be/`                                          |

## Acceptance Criteria (Gherkin)

```gherkin
Feature: Java Spring Boot Migration — Quality Gate

  Scenario: Maven build produces runnable artefact
    Given the organiclever-be source tree uses Java 25 and Spring Boot 4.0
    When the developer runs "nx run organiclever-be:build"
    Then the command exits with code 0
    And a JAR file matching "target/organiclever-be-*.jar" exists

  Scenario: Health endpoint returns service status UP
    Given the Spring Boot application is running on port 8202
    When a client sends GET /api/v1/health
    Then the HTTP response status is 200
    And the response JSON body is {"status": "UP"}

  Scenario: Anonymous health check hides component details
    Given no authentication token is provided
    When a client sends GET /api/v1/health
    Then the HTTP response status is 200
    And the response body contains the key "status" with value "UP"
    And the response body does not contain the key "components"
    And the response body does not contain the key "details"

  Scenario: Unit tests pass with ≥90% line coverage
    Given Cucumber BDD step definitions implement all health-check scenarios
    When "nx run organiclever-be:test:quick" executes
    Then all unit tests pass
    And rhino-cli reports ≥90% line coverage from jacoco.xml
    And DDD bounded-context validation passes for "organiclever"
    And DDD ubiquitous-language validation passes for "organiclever"

  Scenario: Lint passes with Checkstyle and PMD
    Given checkstyle.xml and pmd-ruleset.xml are present
    When "nx run organiclever-be:lint" executes
    Then "mvn checkstyle:check" exits 0
    And "mvn pmd:check" exits 0

  Scenario: Null-safety typecheck passes
    Given all public API methods in com.organicleverbe are annotated with JSpecify
    When "nx run organiclever-be:typecheck" executes
    Then "mvn compile -Pnullcheck" exits 0 with no NullAway errors in com.organicleverbe
    And no missing annotation warnings are reported
    (Note: rhino-cli validate-annotations is ose-primer-only; full annotation
     coverage check will be added when ose-primer propagation brings java
     subcommand to ose-public)

  Scenario: Spec coverage validates all Gherkin scenarios
    Given feature files exist under specs/apps/organiclever/behavior/be/gherkin/
    When "nx run organiclever-be:spec-coverage" executes
    Then rhino-cli reports all scenarios covered by Java step definitions

  Scenario: Integration tests pass in Docker
    When "nx run organiclever-be:test:integration" executes
    Then docker compose brings up the test-runner service
    And the test-runner container exits with code 0
    And docker compose tears down all containers

  Scenario: Contract codegen uses Java generator
    Given the bundled OpenAPI spec exists at
      specs/apps/organiclever/containers/contracts/generated/openapi-bundled.yaml
    When "nx run organiclever-be:codegen" executes
    Then Java model classes are generated under
      apps/organiclever-be/generated-contracts/src/main/java/com/organicleverbe/contracts/
    And no import clean-up post-processing is applied
      (rhino-cli contracts java-clean-imports is ose-primer-only, not in ose-public)

  Scenario: No F# toolchain artefacts remain
    Given the migration is complete
    When a developer searches apps/organiclever-be/ for dotnet-related files
    Then no files matching "*.fs", "*.fsproj", "global.json", "dotnet-tools.json",
      or "fsharplint.json" exist in the directory tree
```

## Product Risks

| Risk                                                                                                    | Impact                                                                    | Mitigation                                                                                                                                                                            |
| ------------------------------------------------------------------------------------------------------- | ------------------------------------------------------------------------- | ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| 90% JaCoCo coverage may be hard to meet with health-only scope                                          | Unit tests might fall below threshold                                     | JaCoCo exclusions configured for application main, package-info, generated contracts, and config classes; simulated state store tests cover HealthController indirectly via step defs |
| Simulated unit test state store does not exercise real HTTP stack                                       | Coverage gaps in HTTP-layer behavior may go undetected by unit tests      | Controller correctness verified at Phase 5 via Docker integration tests that hit the real endpoint                                                                                    |
| JaCoCo exclusion list may hide uncovered controller code                                                | False sense of 90% coverage                                               | Verify by inspecting the generated `jacoco.xml` report manually; review excluded classes list in `pom.xml`                                                                            |
| `typecheck` target omits annotation validation (ose-public rhino-cli lacks `java validate-annotations`) | Unannotated public APIs may escape detection until ose-primer propagation | NullAway via `mvn compile -Pnullcheck` still validates package-level `@NullMarked` annotations; full annotation coverage validated when ose-primer adopts the subcommand              |

## Out of Scope

- Adding new API endpoints (auth, CRUD, database)
- Liquibase migrations or JPA entities
- Spring Security JWT (not needed for health-only v0)
- `organiclever-web` changes
- `organiclever-be-e2e` changes
