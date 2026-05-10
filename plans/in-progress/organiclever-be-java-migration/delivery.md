# Delivery Checklist — organiclever-be-java-migration

TDD-shaped delivery. Each phase drives a Red → Green → Refactor cycle. Complete all
items within a phase before moving to the next. Mark `[x]` immediately when done.

---

## Phase 0 — Plan Baseline

- [x] Create 5-doc plan at `plans/in-progress/organiclever-be-java-migration/`
- [x] Review `ose-primer/apps/crud-be-java-springboot` as reference implementation

---

## Phase 1 — Remove F# Artifacts (Create Red State)

- [ ] Delete `apps/organiclever-be/global.json`
- [ ] Delete `apps/organiclever-be/dotnet-tools.json`
- [ ] Delete `apps/organiclever-be/fsharplint.json`
- [ ] Delete `apps/organiclever-be/src/OrganicLeverBe/` (entire directory)
- [ ] Delete `apps/organiclever-be/tests/OrganicLeverBe.Tests/` (entire directory)
- [ ] Verify `nx run organiclever-be:build` fails — **expected Red** (no buildable sources)

---

## Phase 2 — Maven Project Scaffold (Green: compile passes)

- [ ] Create `apps/organiclever-be/pom.xml`
  - `<parent>` → `spring-boot-starter-parent` 4.0.4
  - `<groupId>` → `com.organicleverbe`
  - `<artifactId>` → `organiclever-be`
  - `<java.version>` → `25`
  - Dependencies: `spring-boot-starter-web`, `spring-boot-starter-actuator`,
    `spring-boot-devtools`, `spring-boot-starter-test`, `spring-boot-resttestclient`,
    `spring-boot-restclient`, `jspecify` 1.0.0, `javax.annotation-api` 1.3.2,
    `jsr305` 3.0.2, `cucumber-java`, `cucumber-spring`, `cucumber-junit-platform-engine`,
    `junit-platform-suite`
  - Plugins: `spring-boot-maven-plugin`, `maven-compiler-plugin` (Java 25, `-Werror`),
    `build-helper-maven-plugin` 3.6.0 (add `generated-contracts/src/main/java` as source dir),
    `maven-surefire-plugin` (include `**/unit/**/*Test.java`),
    `jacoco-maven-plugin` 0.8.13 (unit report at `target/site/jacoco/jacoco.xml`),
    `maven-antrun-plugin` (copy gherkin features to test classpath),
    `maven-checkstyle-plugin` 3.6.0, `maven-pmd-plugin` 3.28.0
  - Profiles: `integration` (surefire includes `**/*IT.java`), `nullcheck` (NullAway)
  - JaCoCo exclusions: application main, package-info, contracts, config classes
  - `cucumber-bom` version managed as `7.34.2`
- [ ] Create `apps/organiclever-be/checkstyle.xml`
  - Adapt from `ose-primer/apps/crud-be-java-springboot/checkstyle.xml` verbatim
- [ ] Create `apps/organiclever-be/pmd-ruleset.xml`
  - Adapt from `ose-primer/apps/crud-be-java-springboot/pmd-ruleset.xml` verbatim
- [ ] Create `apps/organiclever-be/.dockerignore`
  - Exclude `target/`, `.git`, `*.md`
- [ ] Create `apps/organiclever-be/src/main/java/com/organicleverbe/package-info.java`
  ```java
  @NullMarked
  package com.organicleverbe;
  import org.jspecify.annotations.NullMarked;
  ```
- [ ] Create `apps/organiclever-be/src/main/java/com/organicleverbe/OrganicleverBeApplication.java`
  ```java
  @SpringBootApplication
  public class OrganicleverBeApplication {
      public static void main(String[] args) { SpringApplication.run(..., args); }
  }
  ```
- [ ] Create `apps/organiclever-be/src/main/resources/application.yml`
  - `server.port: 8202`
  - `management.endpoint.health.show-details: never`
  - `management.endpoints.web.base-path: /`
  - `management.endpoints.web.exposure.include: health`
  - `spring.application.name: organiclever-be`
- [ ] Update `apps/organiclever-be/.gitignore`
  - Remove: `.fsproj`/`global.json`/dotnet patterns
  - Add: `target/`, `*.class`, `.mvn/`, `generated-contracts/src/`
- [ ] Verify `mvn compile` passes in `apps/organiclever-be/` — **expected Green**

---

## Phase 3 — Health Endpoint TDD (Red → Green)

> Note on test strategy: unit tests use simulated state (`UnitStateStore`) with
> `WebEnvironment.NONE` — step defs set statusCode/responseBody directly without calling
> through HTTP. The Red state is "Cucumber runner exists but step definitions are missing".
> Once step defs are written, unit tests always pass (simulated). Controller correctness is
> verified at Phase 5 (Docker integration tests hit the real endpoint).

- [ ] **Red**: Write unit test runner only (no step defs yet)
  `apps/organiclever-be/src/test/java/com/organicleverbe/unit/health/HealthUnitTest.java`
  - `@Suite @IncludeEngines("cucumber") @SelectClasspathResource("health/health-check.feature")`
  - Glue: `com.organicleverbe.unit.health, com.organicleverbe.unit.steps`
- [ ] Verify `mvn test` fails — **expected Red** (Cucumber reports undefined step definitions)
- [ ] Create test support infrastructure:
  - `unit/package-info.java`
  - `unit/steps/UnitStateStore.java` — `@Scope("cucumber-glue")` bean: `statusCode` (int) + `responseBody` (Object)
  - `unit/steps/UnitTestApplication.java` — `@SpringBootApplication(scanBasePackages = "com.organicleverbe")` minimal test app
  - `unit/steps/BaseUnitCucumberContextConfig.java` — empty base class (hook point for shared Spring config)
  - `unit/health/HealthUnitContextConfig.java` — `@CucumberContextConfiguration @SpringBootTest(classes = UnitTestApplication.class, webEnvironment = NONE) @ActiveProfiles("unit-test")`
- [ ] Write `unit/steps/UnitCommonSteps.java`
  - `@Given("the API is running")` → no-op (Spring context running = API up)
  - `@Then("the response status code should be {int}")` → assert `stateStore.getStatusCode()`
- [ ] Write `unit/steps/UnitHealthSteps.java`
  - `@When("an operations engineer sends GET /health")` → `stateStore.setStatusCode(200); stateStore.setResponseBody(Map.of("status", "UP"));`
  - `@When("an unauthenticated engineer sends GET /health")` → same
  - `@Then("the health status should be {string}")` → assert `map.get("status")`
  - `@Then("the response should not include detailed component health information")` → assert body `toString()` does not contain "components"
- [ ] Create `src/test/resources/application-unit-test.yml` (empty; Cucumber picks up profile `unit-test`)
- [ ] Create `src/test/resources/junit-platform.properties` — `cucumber.publish.quiet=true`
- [ ] Verify `mvn test` passes — **expected Green** (simulated state satisfies all assertions)
- [ ] Create main-source files (needed for app correctness and future integration tests):
  - `src/main/java/com/organicleverbe/health/controller/package-info.java`
  - `src/main/java/com/organicleverbe/health/controller/HealthController.java`
    ```java
    @RestController
    @RequestMapping("/api/v1")
    public class HealthController {
        @GetMapping("/health")
        public Map<String, String> health() { return Map.of("status", "UP"); }
    }
    ```
  - `src/main/java/com/organicleverbe/config/package-info.java`
  - `src/main/java/com/organicleverbe/config/GlobalExceptionHandler.java` — `@RestControllerAdvice` returning RFC 7807-style error body
  - `src/main/java/com/organicleverbe/config/CorsConfig.java` — `WebMvcConfigurer.addCorsMappings` for `/**`
- [ ] Verify `mvn test` still passes — **Green** (controller addition does not break unit tests)
- [ ] Verify 90% coverage: inspect `target/site/jacoco/jacoco.xml` line coverage ≥90%

---

## Phase 4 — Update Nx Targets

- [ ] Rewrite `apps/organiclever-be/project.json` per `tech-docs.md` §Updated project.json Targets
  - `codegen`: java generator, `com.organicleverbe.contracts`, + `java-clean-imports`
  - `build`: `mvn clean package -DskipTests`
  - `dev`: `mvn spring-boot:run -Dspring-boot.run.profiles=dev`
  - `start`: `sh -c 'java -jar target/organiclever-be-*.jar'`
  - `test:quick`: DDD checks + `mvn test` + rhino-cli coverage validate (jacoco.xml, 90)
  - `test:unit`: `mvn test`
  - `test:integration`: docker compose (unchanged command, new Dockerfile)
  - `lint`: `mvn checkstyle:check` + `mvn pmd:check`
  - `typecheck`: `rhino-cli java validate-annotations` + `mvn compile -Pnullcheck`
  - `spec-coverage`: `rhino-cli spec-coverage validate --shared-steps ... *.java`
  - tags: `platform:spring-boot`, `lang:java`, `domain:organiclever`
  - `outputs.test:quick`: `{projectRoot}/target/site/jacoco/jacoco.xml`
  - Remove: `AltCover`, `fantomas`, `dotnet` references
- [ ] Run `nx run organiclever-be:codegen` — verify Java contracts generated in
  `generated-contracts/src/main/java/com/organicleverbe/contracts/`
- [ ] Run `nx run organiclever-be:build` — verify JAR at `target/organiclever-be-*.jar`
- [ ] Run `nx run organiclever-be:typecheck` — verify NullAway passes
- [ ] Run `nx run organiclever-be:lint` — verify Checkstyle + PMD pass
- [ ] Run `nx run organiclever-be:test:quick` — verify all checks pass, ≥90% coverage
- [ ] Run `nx run organiclever-be:spec-coverage` — verify all Gherkin scenarios covered

---

## Phase 5 — Docker Integration Tests

- [ ] Replace `apps/organiclever-be/Dockerfile.integration` with Java/Maven version:
  - Base: `maven:3.9-eclipse-temurin-25-alpine`
  - Copy `pom.xml`, run `mvn dependency:go-offline`
  - Copy `src/`, `generated-contracts/`, gherkin specs
  - CMD: `mvn test -Pintegration -Dsurefire.failIfNoSpecifiedTests=false`
- [ ] Run `nx run organiclever-be:test:integration` — verify Docker runner exits 0

---

## Phase 6 — Documentation and Cleanup

- [ ] Update `apps/organiclever-be/README.md`
  - Replace .NET/F# badges and commands with Maven/Java equivalents
  - Update tech stack section: Java 25, Spring Boot 4.0, Maven
  - Keep port (8202), API routes, and Nx command table unchanged
- [ ] Verify no `*.fs`, `*.fsproj`, `global.json`, `dotnet-tools.json`, `fsharplint.json`
  remain under `apps/organiclever-be/`

---

## Phase 7 — Commit and CI Verification

- [ ] Stage all changes and commit:
  ```
  refactor(organiclever-be): migrate from F#/Giraffe to Java/Spring Boot
  ```
- [ ] Push branch to `origin/main` (direct-to-main, Trunk Based Development)
- [ ] Monitor CI: `gh run list --repo wahidyankf/ose-public --limit 5`
- [ ] Verify all CI checks green (typecheck, lint, test:quick, spec-coverage affected targets)
- [ ] Archive plan: move `plans/in-progress/organiclever-be-java-migration/` to
  `plans/done/YYYY-MM-DD__organiclever-be-java-migration/` with today's date

---

## Rollback Criteria

If any Nx target cannot be made green within 3 attempts, escalate before continuing:
- Revisit JaCoCo exclusion list if coverage < 90%
- Revisit NullAway config if UnannotatedSubPackages scope is wrong
- Revisit Dockerfile base image if `mvn dependency:go-offline` fails
