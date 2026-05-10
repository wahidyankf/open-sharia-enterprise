# Technical Documentation — organiclever-be-java-migration

## Technology Stack

| Concern | Choice | Reason |
|---------|--------|--------|
| Language | Java 25 | Matches `ose-primer` reference; latest LTS baseline |
| Framework | Spring Boot 4.0 | `spring-boot-starter-parent` 4.0.4 |
| Build | Maven | Standard JVM build tool for financial industry |
| Test runner | JUnit 5 + Cucumber JVM 7.34.2 | Gherkin feature consumption |
| Coverage | JaCoCo 0.8.13 | XML report at `target/site/jacoco/jacoco.xml` |
| Lint | Checkstyle 3.6.0 + PMD 3.28.0 | Google Java Style + code quality |
| Null safety | NullAway 0.12.6 + ErrorProne 2.37.0 + JSpecify 1.0.0 | Zero-NPE discipline |
| Codegen | `openapi-generator-cli` java generator | Replaces `fsharp-giraffe-server` |
| Container | Maven multi-stage Docker image | Integration test runner |

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
| Post-process | none | `rhino-cli contracts java-clean-imports` |

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
        "command": "npx openapi-generator-cli generate -i $(pwd)/specs/apps/organiclever/containers/contracts/generated/openapi-bundled.yaml -g java -o $(pwd)/apps/organiclever-be/generated-contracts --model-package com.organicleverbe.contracts --additional-properties=library=resttemplate,dateLibrary=java8,openApiNullable=false,useBeanValidation=false,serializationLibrary=jackson --global-property=models,modelDocs=false,apiDocs=false && (WS_ROOT=$(pwd) && cd $(pwd)/apps/rhino-cli && CGO_ENABLED=0 go run main.go contracts java-clean-imports $WS_ROOT/apps/organiclever-be/generated-contracts)"
      },
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
          "nx run rhino-cli:build --skip-nx-cache && ./apps/rhino-cli/dist/rhino-cli java validate-annotations apps/organiclever-be/src/main/java",
          "cd apps/organiclever-be && mvn compile -Pnullcheck"
        ],
        "parallel": false
      },
      "dependsOn": ["codegen"],
      "cache": true
    },
    "spec-coverage": {
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
