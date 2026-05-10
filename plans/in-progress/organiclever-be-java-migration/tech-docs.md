# Technical Documentation — organiclever-be-java-migration

## Technology Stack

| Concern | Choice | Confidence | Reason |
|---------|--------|------------|--------|
| Language | Java 25 | `[Repo-grounded]` — matches `ose-primer/apps/crud-be-java-springboot/pom.xml` | Matches `ose-primer` reference; latest LTS baseline |
| Framework | Spring Boot 4.0 | `[Repo-grounded]` — `spring-boot-starter-parent` 4.0.4 in ose-primer pom.xml; `[Judgment call]` — Spring Boot 4.0 requires Java 17+ minimum per Spring project conventions; Java 25 exceeds this minimum | `spring-boot-starter-parent` 4.0.4 |
| Build | Maven | `[Repo-grounded]` — verified in ose-primer reference | Standard JVM build tool for financial industry |
| Test runner | JUnit 5 + Cucumber JVM 7.34.2 | `[Repo-grounded]` — `cucumber-bom` 7.34.2 in ose-primer pom.xml | Gherkin feature consumption |
| Coverage | JaCoCo 0.8.13 | `[Repo-grounded]` — `jacoco-maven-plugin` 0.8.13 in ose-primer pom.xml | XML report at `target/site/jacoco/jacoco.xml` |
| Lint | Checkstyle 3.6.0 + PMD 3.28.0 | `[Repo-grounded]` — versions verified in ose-primer pom.xml | Google Java Style + code quality |
| Null safety | NullAway 0.12.6 + ErrorProne 2.37.0 + JSpecify 1.0.0 | `[Repo-grounded]` — versions verified in ose-primer pom.xml | Zero-NPE discipline |
| Codegen | `openapi-generator-cli` java generator | `[Repo-grounded]` — generator type verified in ose-primer project.json | Replaces `fsharp-giraffe-server` |
| Container | Maven multi-stage Docker image | `[Repo-grounded]` — Dockerfile.integration verified in ose-primer | Integration test runner |

## Package Structure

Root package: `com.organicleverbe`

```
src/main/java/com/organicleverbe/
├── OrganicleverBeApplication.java   # @SpringBootApplication entry point
├── package-info.java                # @NullMarked for root package
├── health/
│   └── controller/
│       ├── HealthController.java    # GET /api/v1/health → {"status":"UP"}
│       └── package-info.java
└── config/
    ├── CorsConfig.java              # AllowAnyOrigin/Method/Header (CORS)
    ├── GlobalExceptionHandler.java  # @RestControllerAdvice error mapping
    └── package-info.java

src/test/java/com/organicleverbe/
├── unit/
│   ├── package-info.java
│   ├── health/
│   │   ├── HealthUnitTest.java          # @Suite Cucumber runner for health scenarios
│   │   └── HealthUnitContextConfig.java # @CucumberContextConfiguration + @SpringBootTest(NONE)
│   └── steps/
│       ├── BaseUnitCucumberContextConfig.java
│       ├── UnitTestApplication.java     # Minimal @SpringBootApplication for unit context
│       ├── UnitStateStore.java          # Per-scenario state (statusCode, responseBody)
│       ├── UnitCommonSteps.java         # "the API is running" + "response status code is {int}"
│       └── UnitHealthSteps.java         # When/Then step defs for health scenarios

src/main/resources/
└── application.yml    # port: 8202, health endpoint config, CORS
```

## Key Design Decisions

### No Spring Security / No DB (v0 scope)

`organiclever-be` is at v0: health endpoint only. No auth, no JPA, no Liquibase. The
`pom.xml` includes only `spring-boot-starter-web`, `spring-boot-starter-actuator`, and
test/lint/null-safety tooling. Security and DB deps are added in future feature plans.

### Health Endpoint Strategy

Two options for the health endpoint:

1. **Custom controller** (`GET /api/v1/health` returning `{"status":"UP"}`) — matches current
   F# behaviour exactly. Simple, no Spring Actuator complexity.
2. **Spring Actuator** (`/actuator/health`) — standard but changes the URL.

**Choice**: Custom controller at `/api/v1/health`. Preserves the contract. Spring Actuator
is still in `pom.xml` for future use (monitoring integration) but not exposed as the primary
health API.

### Unit Tests via Simulated State Store (no real server)

Unit tests use `@SpringBootTest(webEnvironment = NONE)` with a minimal `UnitTestApplication`
(no JPA, no security). `UnitStateStore` simulates HTTP state directly in step definitions —
steps set `statusCode` and `responseBody` manually rather than calling through a real HTTP
stack. This avoids port conflicts and runs fast without Docker. Matches the ose-primer health
unit test pattern exactly. Controller correctness is verified at the integration test level
(Docker) where the real endpoint is exercised.

### JaCoCo Exclusions

Exclude from coverage measurement:
- `OrganicleverBeApplication.class` — main entry point
- `**/package-info.class` — no logic
- `**/contracts/*.class` — OpenAPI-generated code
- `**/config/GlobalExceptionHandler.class` — Spring MVC dispatch-path only
- `**/config/CorsConfig.class` — pure Spring config

### NullAway Configuration

```xml
-Xplugin:ErrorProne -XepDisableAllChecks -Xep:NullAway:ERROR
  -XepOpt:NullAway:AnnotatedPackages=com.organicleverbe
  -XepOpt:NullAway:UnannotatedSubPackages=com.organicleverbe.contracts
  -XepOpt:NullAway:JSpecifyMode=true
```

`com.organicleverbe.contracts` is excluded from null annotation requirements because the
classes are OpenAPI-generated.

### Codegen Target Change

| Attribute | Old (F#) | New (Java) |
|-----------|----------|------------|
| Generator | `fsharp-giraffe-server` | `java` |
| Model package | `OrganicLeverBe.Contracts` | `com.organicleverbe.contracts` |
| Output | `generated-contracts/` | `generated-contracts/src/main/java/` |
| Post-process | none | _(none — see NOTE below)_ |

> **NOTE — ose-public rhino-cli limitations**: `rhino-cli contracts java-clean-imports` and
> `rhino-cli java validate-annotations` exist only in
> `ose-primer/apps/rhino-cli` — they are NOT present in `apps/rhino-cli` (ose-public).
> A scan of `apps/rhino-cli/cmd/` confirms no `contracts.go`, `java.go`, or
> `java_validate_annotations.go` files exist. `[Unverified — ose-primer only]`
>
> Consequence for this plan:
>
> - `codegen` target: omit `java-clean-imports` post-processing — the plain java generator
>   output is used directly without post-processing
> - `typecheck` target: use only `mvn compile -Pnullcheck` (NullAway via Maven profile) —
>   omit the `rhino-cli java validate-annotations` annotation-coverage check
>
> These commands will be adopted into ose-public's rhino-cli in a future plan via the
> ose-primer propagation workflow (`repo-ose-primer-propagation-maker`). When adopted,
> the `codegen` and `typecheck` targets should be updated to include them.

## Updated `project.json` Targets

```json
{
  "name": "organiclever-be",
  "$schema": "../../node_modules/nx/schemas/project-schema.json",
  "sourceRoot": "apps/organiclever-be/src",
  "projectType": "application",
  "targets": {
    "codegen": {
      "executor": "nx:run-commands",
      "options": {
        "command": "npx openapi-generator-cli generate -i $(pwd)/specs/apps/organiclever/containers/contracts/generated/openapi-bundled.yaml -g java -o $(pwd)/apps/organiclever-be/generated-contracts --model-package com.organicleverbe.contracts --additional-properties=library=resttemplate,dateLibrary=java8,openApiNullable=false,useBeanValidation=false,serializationLibrary=jackson --global-property=models,modelDocs=false,apiDocs=false"
      },
      "// NOTE": "java-clean-imports omitted — rhino-cli contracts java-clean-imports does not exist in ose-public apps/rhino-cli [Unverified — ose-primer only]",
      "dependsOn": ["organiclever-contracts:bundle"],
      "cache": true,
      "inputs": [
        "{workspaceRoot}/specs/apps/organiclever/containers/contracts/generated/openapi-bundled.yaml",
        "{workspaceRoot}/apps/rhino-cli/cmd/**/*.go",
        "{workspaceRoot}/apps/rhino-cli/internal/**/*.go"
      ],
      "outputs": ["{projectRoot}/generated-contracts"]
    },
    "build": {
      "executor": "nx:run-commands",
      "options": {
        "command": "mvn clean package -DskipTests",
        "cwd": "apps/organiclever-be"
      },
      "outputs": ["{projectRoot}/target"],
      "dependsOn": ["codegen"]
    },
    "dev": {
      "executor": "nx:run-commands",
      "options": {
        "command": "mvn spring-boot:run -Dspring-boot.run.profiles=dev",
        "cwd": "apps/organiclever-be"
      }
    },
    "start": {
      "executor": "nx:run-commands",
      "options": {
        "command": "sh -c 'java -jar target/organiclever-be-*.jar'",
        "cwd": "apps/organiclever-be"
      }
    },
    "test:quick": {
      "executor": "nx:run-commands",
      "options": {
        "commands": [
          "(cd ../../apps/rhino-cli && CGO_ENABLED=0 go run main.go ddd bc organiclever)",
          "(cd ../../apps/rhino-cli && CGO_ENABLED=0 go run main.go ddd ul organiclever)",
          "mvn test",
          "(cd ../../apps/rhino-cli && CGO_ENABLED=0 go run main.go test-coverage validate apps/organiclever-be/target/site/jacoco/jacoco.xml 90)"
        ],
        "parallel": false,
        "cwd": "apps/organiclever-be"
      },
      "cache": true,
      "inputs": [
        "{projectRoot}/src/**",
        "{projectRoot}/pom.xml",
        "{projectRoot}/generated-contracts/**/*",
        "{workspaceRoot}/specs/apps/organiclever/behavior/be/gherkin/**/*.feature",
        "{workspaceRoot}/specs/apps/organiclever/ddd/bounded-contexts.yaml",
        "{workspaceRoot}/specs/apps/organiclever/ddd/ubiquitous-language/**/*.md"
      ],
      "outputs": ["{projectRoot}/target/site/jacoco/jacoco.xml"]
    },
    "test:unit": {
      "executor": "nx:run-commands",
      "options": {
        "command": "mvn test",
        "cwd": "apps/organiclever-be"
      },
      "cache": true,
      "inputs": [
        "{projectRoot}/src/**",
        "{projectRoot}/pom.xml",
        "{projectRoot}/generated-contracts/**/*",
        "{workspaceRoot}/specs/apps/organiclever/behavior/be/gherkin/**/*.feature"
      ]
    },
    "test:integration": {
      "executor": "nx:run-commands",
      "options": {
        "command": "docker compose -f docker-compose.integration.yml down -v && docker compose -f docker-compose.integration.yml up --abort-on-container-exit --build",
        "cwd": "apps/organiclever-be"
      },
      "cache": false
    },
    "lint": {
      "executor": "nx:run-commands",
      "options": {
        "commands": ["mvn checkstyle:check", "mvn pmd:check"],
        "parallel": false,
        "cwd": "apps/organiclever-be"
      }
    },
    "typecheck": {
      "executor": "nx:run-commands",
      "options": {
        "commands": [
          "cd apps/organiclever-be && mvn compile -Pnullcheck"
        ],
        "// NOTE": "rhino-cli java validate-annotations omitted — does not exist in ose-public apps/rhino-cli [Unverified — ose-primer only]; will be added when ose-primer propagation brings java subcommand to ose-public",
        "parallel": false
      },
      "dependsOn": ["codegen"],
      "cache": true
    },
    "spec-coverage": {
      "// NOTE": "No --exclude-dir test-support flag needed — specs/apps/organiclever/behavior/be/gherkin/ has no test-support/ subdirectory [Repo-grounded]",
      "command": "CGO_ENABLED=0 go run -C apps/rhino-cli main.go spec-coverage validate --shared-steps specs/apps/organiclever/behavior/be/gherkin apps/organiclever-be",
      "cache": true,
      "inputs": [
        "{workspaceRoot}/specs/apps/organiclever/behavior/be/gherkin/**/*.feature",
        "{projectRoot}/**/*.java"
      ]
    }
  },
  "tags": ["type:app", "platform:spring-boot", "lang:java", "domain:organiclever"],
  "implicitDependencies": ["organiclever-contracts", "rhino-cli"]
}
```

## Files to Add

```
apps/organiclever-be/
├── pom.xml                                  # Maven project (Spring Boot 4.0, Java 25)
├── checkstyle.xml                           # Google Java Style subset
├── pmd-ruleset.xml                          # PMD quality rules
├── .dockerignore                            # Exclude target/, .git
├── src/
│   ├── main/
│   │   ├── java/com/organicleverbe/
│   │   │   ├── OrganicleverBeApplication.java
│   │   │   ├── package-info.java
│   │   │   ├── health/controller/
│   │   │   │   ├── HealthController.java
│   │   │   │   └── package-info.java
│   │   │   └── config/
│   │   │       ├── CorsConfig.java
│   │   │       ├── GlobalExceptionHandler.java
│   │   │       └── package-info.java
│   │   └── resources/
│   │       └── application.yml
│   └── test/
│       ├── java/com/organicleverbe/unit/
│       │   ├── package-info.java
│       │   ├── health/
│       │   │   ├── HealthUnitTest.java
│       │   │   └── HealthUnitContextConfig.java
│       │   └── steps/
│       │       ├── BaseUnitCucumberContextConfig.java
│       │       ├── UnitTestApplication.java
│       │       ├── UnitStateStore.java
│       │       ├── UnitCommonSteps.java
│       │       └── UnitHealthSteps.java
│       └── resources/
│           ├── application-unit-test.yml
│           └── junit-platform.properties
└── Dockerfile.integration                   # Replace: Maven/Java instead of .NET SDK
```

## Files to Remove

```
apps/organiclever-be/
├── global.json
├── dotnet-tools.json
├── fsharplint.json
├── src/OrganicLeverBe/
│   ├── OrganicLeverBe.fsproj
│   ├── Program.fs
│   ├── Domain/Types.fs
│   └── Handlers/HealthHandler.fs
└── tests/OrganicLeverBe.Tests/
    ├── OrganicLeverBe.Tests.fsproj
    ├── HttpTestFixture.fs
    ├── State.fs
    ├── Integration/FeatureRunner.fs
    ├── Integration/Steps/CommonSteps.fs
    ├── Integration/Steps/HealthSteps.fs
    └── Unit/UnitFeatureRunner.fs
```

## Docker Integration Test Architecture

The integration runner uses a Maven multi-stage Docker image:

```dockerfile
FROM maven:3.9-eclipse-temurin-25-alpine AS builder
WORKDIR /build
COPY apps/organiclever-be/pom.xml .
COPY apps/organiclever-be/src/ src/
COPY apps/organiclever-be/generated-contracts/ generated-contracts/
COPY specs/apps/organiclever/behavior/be/gherkin /specs/apps/organiclever/behavior/be/gherkin
RUN mvn test -Pintegration -Dsurefire.failIfNoSpecifiedTests=false
```

The integration profile in `pom.xml` runs tests matching `**/*IT.java` (none in v0 — the
test runner exits 0 with no tests). When integration tests are added in future plans,
they live in `src/test/java/com/organicleverbe/integration/`.

## Coverage Threshold Mapping

| Old (F#/AltCover) | New (Java/JaCoCo) |
|-------------------|-------------------|
| `altcov.info` LCOV | `target/site/jacoco/jacoco.xml` XML |
| AltCover `--linecover` | JaCoCo line coverage |
| ≥90% line | ≥90% line |

`rhino-cli test-coverage validate` already supports both formats. No rhino-cli changes needed.

## Dependencies

The following external toolchain components must be available before execution:

| Dependency | Version | How to verify | How to install if missing |
|-----------|---------|---------------|--------------------------|
| Java 25 | JDK 25 | `java -version` | `npm run doctor -- --fix` (Volta-managed); or `sdk install java 25-open` |
| Maven | 3.9+ | `mvn -version` | System package manager (`brew install maven`, `apt install maven`); or Docker-based build |
| Docker | 20+ | `docker info` | Install Docker Desktop from docker.com |
| Go | 1.22+ | `go version` | `npm run doctor -- --fix` (Volta/doctor managed) |
| Node.js / npx | 24.x | `node -version` | Managed by Volta; `npm run doctor -- --fix` |

> **NOTE — ose-public rhino-cli gap**: `apps/rhino-cli` does not have `contracts java-clean-imports`
> or `java validate-annotations` subcommands. These exist only in `ose-primer/apps/rhino-cli`.
> See §Codegen Target Change NOTE above. This is a known limitation, not a setup gap — the
> `project.json` targets for this plan are written to avoid these missing commands.

## Rollback Procedure

To revert the migration and restore the F# implementation from git history:

```bash
# Find the commit SHA just before Phase 1 removal commit
git log --oneline apps/organiclever-be/ | head -20

# Restore F# source files from before the migration
git checkout <pre-migration-sha> -- apps/organiclever-be/

# Commit the revert
git add apps/organiclever-be/
git commit -m "revert(organiclever-be): restore F#/Giraffe implementation"
git push origin main
```

Alternatively, use `git revert <commit-sha>` for each migration phase commit in reverse
order (Phase 6 docs → Phase 5 Docker → Phase 4 Nx targets → Phase 3 Java source →
Phase 2 Maven scaffold → Phase 1 F# removal).

The `apps/organiclever-be-e2e` and `organiclever-web` projects require no rollback action
— neither was modified during this migration.

## `application.yml` (v0)

```yaml
spring:
  application:
    name: organiclever-be
  web:
    error:
      include-message: always
      include-binding-errors: always

server:
  port: 8202

management:
  endpoints:
    web:
      exposure:
        include: health
      base-path: /
  endpoint:
    health:
      show-details: never
```
