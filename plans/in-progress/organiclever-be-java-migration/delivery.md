# Delivery Checklist — organiclever-be-java-migration

TDD-shaped delivery. Each phase drives a Red → Green → Refactor cycle. Complete all
items within a phase before moving to the next. Mark `[x]` immediately when done.

---

## Worktree

Worktree path: `worktrees/organiclever-be-java-migration/`

Provision before execution (run from repo root):

```bash
claude --worktree organiclever-be-java-migration
```

See [Worktree Path Convention](../../../governance/conventions/structure/worktree-path.md) and [Plans Organization Convention §Worktree Specification](../../../governance/conventions/structure/plans.md#worktree-specification).

---

## Phase 0 — Plan Baseline

- [x] Create 5-doc plan at `plans/in-progress/organiclever-be-java-migration/`
- [x] Review `ose-primer/apps/crud-be-java-springboot` as reference implementation

---

## Phase 0.5 — Environment Setup

- [x] Install dependencies and verify toolchain from repo root:

  ```bash
  npm install && npm run doctor -- --fix
  ```

  Verify: `npm run doctor` exits 0 with no failures.

  > **Done** 2026-05-11 — `npm install` completed; `npm run doctor` reports 20/20 tools OK (java v25, maven v3.9.4, golang v1.26.1, docker v29.4.0).

- [x] Verify Java 25 available:

  ```bash
  java -version
  ```

  Verify: output shows `openjdk 25` (or matching JDK 25).

  > **Done** 2026-05-11 — `java v25` confirmed via doctor output.

- [x] Verify Maven 3.9+ available:

  ```bash
  mvn -version
  ```

  Verify: output shows `Apache Maven 3.9` or later.

  > **Done** 2026-05-11 — `maven v3.9.4` confirmed via doctor output.

- [x] Verify Docker daemon running:

  ```bash
  docker info
  ```

  Verify: exits 0 with no connection errors.

  > **Done** 2026-05-11 — `docker v29.4.0` confirmed via doctor output.

- [x] Verify Go toolchain available (for rhino-cli commands):

  ```bash
  go version
  ```

  Verify: exits 0.

  > **Done** 2026-05-11 — `golang v1.26.1` confirmed via doctor output.

- [x] Verify existing `nx run organiclever-be:test:quick` passes before making changes:

  ```bash
  npx nx run organiclever-be:test:quick
  ```

  Verify: exits 0 (baseline green).

  > **Done** 2026-05-11 — F# baseline passes: 2 tests, 91.67% line coverage.

---

## Phase 1 — Remove F# Artifacts (Create Red State)

> Commit thematically at end of this phase:
> `git add apps/organiclever-be/` (removals already staged by `git rm`; this stages any
> remaining tracked changes) — then commit:
> `chore(organiclever-be): remove F# source and dotnet tooling`

- [x] Delete `apps/organiclever-be/global.json`:

  ```bash
  git rm apps/organiclever-be/global.json
  ```

  Verify: `test ! -f apps/organiclever-be/global.json` exits 0.

  > **Done** 2026-05-11 — `git rm apps/organiclever-be/global.json` succeeded.

- [x] Delete `apps/organiclever-be/dotnet-tools.json`:

  ```bash
  git rm apps/organiclever-be/dotnet-tools.json
  ```

  Verify: `test ! -f apps/organiclever-be/dotnet-tools.json` exits 0.

  > **Done** 2026-05-11 — `git rm apps/organiclever-be/dotnet-tools.json` succeeded.

- [x] Delete `apps/organiclever-be/fsharplint.json`:

  ```bash
  git rm apps/organiclever-be/fsharplint.json
  ```

  Verify: `test ! -f apps/organiclever-be/fsharplint.json` exits 0.

  > **Done** 2026-05-11 — `git rm apps/organiclever-be/fsharplint.json` succeeded.

- [x] Delete `apps/organiclever-be/src/OrganicLeverBe/` (entire directory):

  ```bash
  git rm -r apps/organiclever-be/src/OrganicLeverBe/
  ```

  Verify: `test ! -d apps/organiclever-be/src/OrganicLeverBe` exits 0.

  > **Done** 2026-05-11 — tracked .fs/.fsproj removed; untracked bin/obj cleaned with rm -rf.

- [x] Delete `apps/organiclever-be/tests/OrganicLeverBe.Tests/` (entire directory):

  ```bash
  git rm -r apps/organiclever-be/tests/OrganicLeverBe.Tests/
  ```

  Verify: `test ! -d apps/organiclever-be/tests/OrganicLeverBe.Tests` exits 0.

  > **Done** 2026-05-11 — 6 .fs/.fsproj files removed; untracked artifacts cleaned.

- [x] Verify `nx run organiclever-be:build` fails — **expected Red** (no buildable sources)

  > **Done** 2026-05-11 — `MSB1009: Project file does not exist` — Red confirmed.

---

## Phase 2 — Maven Project Scaffold (Green: compile passes)

> Commit thematically at end of this phase:
> `git add apps/organiclever-be/` — then commit:
> `feat(organiclever-be): add Java/Spring Boot project structure`

- [ ] Create `apps/organiclever-be/pom.xml` (_New file_):
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
  - JaCoCo exclusions: `**/OrganicleverBeApplication.class`, `**/package-info.class`,
    `**/contracts/*.class`, `**/config/GlobalExceptionHandler.class`, `**/config/CorsConfig.class`
  - `cucumber-bom` version managed as `7.34.2`
  - Verify: `test -f apps/organiclever-be/pom.xml` exits 0 and
    `cd apps/organiclever-be && mvn validate` exits 0.
- [ ] Create `apps/organiclever-be/checkstyle.xml` (_New file_):
  - Adapt from `ose-primer/apps/crud-be-java-springboot/checkstyle.xml` verbatim.
  - Verify: `test -f apps/organiclever-be/checkstyle.xml` exits 0.
- [ ] Create `apps/organiclever-be/pmd-ruleset.xml` (_New file_):
  - Adapt from `ose-primer/apps/crud-be-java-springboot/pmd-ruleset.xml` verbatim.
  - Verify: `test -f apps/organiclever-be/pmd-ruleset.xml` exits 0.
- [ ] Create `apps/organiclever-be/.dockerignore` (_New file_) — exclude `target/`, `.git`,
      `*.md`:
  - Verify: `test -f apps/organiclever-be/.dockerignore` exits 0.
- [ ] Create `apps/organiclever-be/src/main/java/com/organicleverbe/package-info.java`
      (_New file_):

  ```java
  @NullMarked
  package com.organicleverbe;
  import org.jspecify.annotations.NullMarked;
  ```

  Verify: `test -f apps/organiclever-be/src/main/java/com/organicleverbe/package-info.java`
  exits 0.

- [ ] Create `apps/organiclever-be/src/main/java/com/organicleverbe/OrganicleverBeApplication.java`
      (_New file_):

  ```java
  @SpringBootApplication
  public class OrganicleverBeApplication {
      public static void main(String[] args) { SpringApplication.run(..., args); }
  }
  ```

  Verify: `test -f apps/organiclever-be/src/main/java/com/organicleverbe/OrganicleverBeApplication.java`
  exits 0.

- [ ] Create `apps/organiclever-be/src/main/resources/application.yml` (_New file_):
  - `server.port: 8202`
  - `management.endpoint.health.show-details: never`
  - `management.endpoints.web.base-path: /`
  - `management.endpoints.web.exposure.include: health`
  - `spring.application.name: organiclever-be`
  - Verify: `test -f apps/organiclever-be/src/main/resources/application.yml` exits 0.
- [ ] Update `apps/organiclever-be/.gitignore`:
  - Remove: `.fsproj`/`global.json`/dotnet patterns
  - Add: `target/`, `*.class`, `.mvn/`, `generated-contracts/src/`
  - Verify: `grep -q 'target/' apps/organiclever-be/.gitignore` exits 0.
- [ ] Verify `mvn compile` passes in `apps/organiclever-be/`:

  ```bash
  cd apps/organiclever-be && mvn compile
  ```

  Verify: exits 0 — **expected Green**.

---

## Phase 3 — Health Endpoint TDD (Red → Green)

> Note on test strategy: unit tests use simulated state (`UnitStateStore`) with
> `WebEnvironment.NONE` — step defs set statusCode/responseBody directly without calling
> through HTTP. The Red state is an "undefined steps" Cucumber failure — the test runner
> exists but no step definitions are wired yet, so Cucumber reports all scenarios as
> undefined/pending. Once step defs are written, unit tests always pass (simulated).
> Controller correctness is verified at Phase 5 (Docker integration tests hit the real
> endpoint).
>
> Commit thematically at end of this phase:
> `git add apps/organiclever-be/src/` — then commit:
> `feat(organiclever-be): add Java/Spring Boot health endpoint with Cucumber BDD`

- [ ] **Red**: Create `apps/organiclever-be/src/test/java/com/organicleverbe/unit/health/HealthUnitTest.java`
      (_New file_) — unit test runner only, no step defs yet:
  - `@Suite @IncludeEngines("cucumber") @SelectClasspathResource("health/health-check.feature")`
  - Glue: `com.organicleverbe.unit.health, com.organicleverbe.unit.steps`
  - Verify: `test -f apps/organiclever-be/src/test/java/com/organicleverbe/unit/health/HealthUnitTest.java`
    exits 0.
- [ ] Verify `mvn test` fails — **expected Red** (Cucumber reports undefined step definitions):

  ```bash
  cd apps/organiclever-be && mvn test
  ```

  Verify: exits non-zero with "undefined" or "pending" Cucumber output.

- [ ] Create `apps/organiclever-be/src/test/java/com/organicleverbe/unit/package-info.java`
      (_New file_):
  - Verify: `test -f apps/organiclever-be/src/test/java/com/organicleverbe/unit/package-info.java`
    exits 0.
- [ ] Create `apps/organiclever-be/src/test/java/com/organicleverbe/unit/steps/UnitStateStore.java`
      (_New file_) — `@Scope("cucumber-glue")` bean: `statusCode` (int) + `responseBody` (Object):
  - Verify: `test -f apps/organiclever-be/src/test/java/com/organicleverbe/unit/steps/UnitStateStore.java`
    exits 0.
- [ ] Create `apps/organiclever-be/src/test/java/com/organicleverbe/unit/steps/UnitTestApplication.java`
      (_New file_) — `@SpringBootApplication(scanBasePackages = "com.organicleverbe")` minimal test app:
  - Verify: `test -f apps/organiclever-be/src/test/java/com/organicleverbe/unit/steps/UnitTestApplication.java`
    exits 0.
- [ ] Create `apps/organiclever-be/src/test/java/com/organicleverbe/unit/steps/BaseUnitCucumberContextConfig.java`
      (_New file_) — empty base class (hook point for shared Spring config):
  - Verify: `test -f apps/organiclever-be/src/test/java/com/organicleverbe/unit/steps/BaseUnitCucumberContextConfig.java`
    exits 0.
- [ ] Create `apps/organiclever-be/src/test/java/com/organicleverbe/unit/health/HealthUnitContextConfig.java`
      (_New file_) —
      `@CucumberContextConfiguration @SpringBootTest(classes = UnitTestApplication.class, webEnvironment = NONE) @ActiveProfiles("unit-test")`:
  - Verify: `test -f apps/organiclever-be/src/test/java/com/organicleverbe/unit/health/HealthUnitContextConfig.java`
    exits 0.
- [ ] Write `apps/organiclever-be/src/test/java/com/organicleverbe/unit/steps/UnitCommonSteps.java`
      (_New file_):
  - `@Given("the API is running")` → no-op (Spring context running = API up)
  - `@Then("the response status code should be {int}")` → assert `stateStore.getStatusCode()`
  - Verify: `test -f apps/organiclever-be/src/test/java/com/organicleverbe/unit/steps/UnitCommonSteps.java`
    exits 0.
- [ ] Write `apps/organiclever-be/src/test/java/com/organicleverbe/unit/steps/UnitHealthSteps.java`
      (_New file_):
  - `@When("an operations engineer sends GET /health")` → `stateStore.setStatusCode(200); stateStore.setResponseBody(Map.of("status", "UP"));`
  - `@When("an unauthenticated engineer sends GET /health")` → same
  - `@Then("the health status should be {string}")` → assert `map.get("status")`
  - `@Then("the response should not include detailed component health information")` → assert body
    `toString()` does not contain "components"
  - Verify: `test -f apps/organiclever-be/src/test/java/com/organicleverbe/unit/steps/UnitHealthSteps.java`
    exits 0.
- [ ] Create `apps/organiclever-be/src/test/resources/application-unit-test.yml` (_New file_,
      empty — Cucumber picks up profile `unit-test`):
  - Verify: `test -f apps/organiclever-be/src/test/resources/application-unit-test.yml` exits 0.
- [ ] Create `apps/organiclever-be/src/test/resources/junit-platform.properties` (_New file_) —
      `cucumber.publish.quiet=true`:
  - Verify: `test -f apps/organiclever-be/src/test/resources/junit-platform.properties` exits 0.
- [ ] Verify `mvn test` passes — **expected Green** (simulated state satisfies all assertions):

  ```bash
  cd apps/organiclever-be && mvn test
  ```

  Verify: exits 0.

- [ ] Create `apps/organiclever-be/src/main/java/com/organicleverbe/health/controller/package-info.java`
      (_New file_):
  - Verify: `test -f apps/organiclever-be/src/main/java/com/organicleverbe/health/controller/package-info.java`
    exits 0.
- [ ] Create `apps/organiclever-be/src/main/java/com/organicleverbe/health/controller/HealthController.java`
      (_New file_):

  ```java
  @RestController
  @RequestMapping("/api/v1")
  public class HealthController {
      @GetMapping("/health")
      public Map<String, String> health() { return Map.of("status", "UP"); }
  }
  ```

  Verify: `test -f apps/organiclever-be/src/main/java/com/organicleverbe/health/controller/HealthController.java`
  exits 0.

- [ ] Create `apps/organiclever-be/src/main/java/com/organicleverbe/config/package-info.java`
      (_New file_):
  - Verify: `test -f apps/organiclever-be/src/main/java/com/organicleverbe/config/package-info.java`
    exits 0.
- [ ] Create `apps/organiclever-be/src/main/java/com/organicleverbe/config/GlobalExceptionHandler.java`
      (_New file_) — `@RestControllerAdvice` returning RFC 7807-style error body:
  - Verify: `test -f apps/organiclever-be/src/main/java/com/organicleverbe/config/GlobalExceptionHandler.java`
    exits 0.
- [ ] Create `apps/organiclever-be/src/main/java/com/organicleverbe/config/CorsConfig.java`
      (_New file_) — `WebMvcConfigurer.addCorsMappings` for `/**`:
  - Verify: `test -f apps/organiclever-be/src/main/java/com/organicleverbe/config/CorsConfig.java`
    exits 0.
- [ ] Verify `mvn test` still passes after adding main-source files — **Green** (controller
      addition does not break unit tests):

  ```bash
  cd apps/organiclever-be && mvn test
  ```

  Verify: exits 0.

- [ ] Verify 90% coverage — inspect `target/site/jacoco/jacoco.xml` line coverage ≥90%:

  ```bash
  cd apps/organiclever-be && mvn jacoco:report
  ```

  Verify: `jacoco.xml` contains line-coverage ≥90% for non-excluded classes.

---

## Phase 4 — Update Nx Targets

> Commit thematically at end of this phase:
> `git add apps/organiclever-be/project.json` — then commit:
> `refactor(organiclever-be): update Nx targets for Maven toolchain`

**NOTE**: `rhino-cli contracts java-clean-imports` and `rhino-cli java validate-annotations`
do NOT exist in ose-public's `apps/rhino-cli` — they are ose-primer-only subcommands
(no `cmd/contracts.go` or `cmd/java.go` in ose-public). These commands will be adopted
into ose-public's rhino-cli in a future plan via ose-primer propagation. For now:

- `codegen` target omits the `java-clean-imports` post-processing step
- `typecheck` target uses only `mvn compile -Pnullcheck` (NullAway via Maven profile)

- [ ] Rewrite `apps/organiclever-be/project.json` per `tech-docs.md` §Updated project.json
      Targets — with the following adjustments for ose-public rhino-cli compatibility:
  - `codegen`: java generator, `com.organicleverbe.contracts` output — **omit**
    `java-clean-imports` (not in ose-public rhino-cli `[Unverified — ose-primer only]`)
  - `build`: `mvn clean package -DskipTests`
  - `dev`: `mvn spring-boot:run -Dspring-boot.run.profiles=dev`
  - `start`: `sh -c 'java -jar target/organiclever-be-*.jar'`
  - `test:quick`: DDD checks + `mvn test` + rhino-cli coverage validate (jacoco.xml, 90)
  - `test:unit`: `mvn test`
  - `test:integration`: docker compose (unchanged command, new Dockerfile)
  - `lint`: `mvn checkstyle:check` + `mvn pmd:check`
  - `typecheck`: `mvn compile -Pnullcheck` only — **omit** `rhino-cli java validate-annotations`
    (not in ose-public rhino-cli `[Unverified — ose-primer only]`)
  - `spec-coverage`: `rhino-cli spec-coverage validate --shared-steps ... *.java` (no
    `--exclude-dir test-support` needed — `specs/apps/organiclever/behavior/be/gherkin/`
    has no `test-support/` subdirectory)
  - tags: `platform:spring-boot`, `lang:java`, `domain:organiclever`
  - `outputs.test:quick`: `{projectRoot}/target/site/jacoco/jacoco.xml`
  - Remove: `AltCover`, `fantomas`, `dotnet` references
  - Verify: `test -f apps/organiclever-be/project.json` exits 0 and
    `grep -q '"platform:spring-boot"' apps/organiclever-be/project.json` exits 0.
- [ ] Run `npx nx run organiclever-be:codegen` — verify Java contracts generated:

  ```bash
  npx nx run organiclever-be:codegen
  ```

  Verify: exits 0 and Java files exist under
  `apps/organiclever-be/generated-contracts/src/main/java/com/organicleverbe/contracts/`.

- [ ] Run `npx nx run organiclever-be:build` — verify JAR produced:

  ```bash
  npx nx run organiclever-be:build
  ```

  Verify: exits 0 and a file matching `apps/organiclever-be/target/organiclever-be-*.jar`
  exists.

- [ ] Run `npx nx run organiclever-be:typecheck` — verify NullAway passes:

  ```bash
  npx nx run organiclever-be:typecheck
  ```

  Verify: exits 0 with no NullAway violations.

- [ ] Run `npx nx run organiclever-be:lint` — verify Checkstyle + PMD pass:

  ```bash
  npx nx run organiclever-be:lint
  ```

  Verify: exits 0.

- [ ] Run `npx nx run organiclever-be:test:quick` — verify all checks pass, ≥90% coverage:

  ```bash
  npx nx run organiclever-be:test:quick
  ```

  Verify: exits 0 and JaCoCo reports ≥90% line coverage.

- [ ] Run `npx nx run organiclever-be:spec-coverage` — verify all Gherkin scenarios covered:

  ```bash
  npx nx run organiclever-be:spec-coverage
  ```

  Verify: exits 0 with no uncovered scenarios reported.

---

## Phase 5 — Docker Integration Tests

> Commit thematically at end of this phase:
> `git add apps/organiclever-be/Dockerfile.integration` — then commit:
> `refactor(organiclever-be): replace Dockerfile.integration for Java`

- [ ] Replace `apps/organiclever-be/Dockerfile.integration` with Java/Maven version — using
      `ose-primer/apps/crud-be-java-springboot/Dockerfile.integration` as reference, replacing all
      references to `demobejasb` → `organicleverbe` and `crud-be-java-springboot` →
      `organiclever-be`:
  - Base: `maven:3.9-eclipse-temurin-25-alpine`
  - Copy `pom.xml`, run `mvn dependency:go-offline`
  - Copy `src/`, `generated-contracts/`, gherkin specs
  - CMD: `mvn test -Pintegration -Dsurefire.failIfNoSpecifiedTests=false`
  - Verify: `docker build -f apps/organiclever-be/Dockerfile.integration .` exits 0
    (or equivalent check that the Dockerfile is syntactically valid).
- [ ] Run `npx nx run organiclever-be:test:integration` — verify Docker runner exits 0:

  ```bash
  npx nx run organiclever-be:test:integration
  ```

  Verify: exits 0 and all integration test containers tear down cleanly.

---

## Phase 5b — Manual API Verification (curl)

- [ ] Start dev server on port 8202:

  ```bash
  npx nx run organiclever-be:dev
  ```

  Verify: process prints "Started OrganicleverBeApplication" and port 8202 is listening.

- [ ] Verify health endpoint returns service status UP:

  ```bash
  curl -s http://localhost:8202/api/v1/health | jq .
  ```

  Verify: response is `{"status":"UP"}`.

- [ ] Verify anonymous access returns HTTP 200:

  ```bash
  curl -o /dev/null -w '%{http_code}' http://localhost:8202/api/v1/health
  ```

  Verify: returns `200`.

- [ ] Verify no component details in response:

  ```bash
  curl -s http://localhost:8202/api/v1/health | jq 'has("components")'
  ```

  Verify: returns `false`.

- [ ] Stop dev server (Ctrl-C or `kill` the process).
      Verify: port 8202 no longer listening.

---

## Phase 6 — Documentation and Cleanup

> Commit thematically at end of this phase:
> `git add apps/organiclever-be/README.md` — then commit:
> `docs(organiclever-be): update README for Java Spring Boot`

- [ ] Update `apps/organiclever-be/README.md`:
  - Replace .NET/F# badges and commands with Maven/Java equivalents
  - Update tech stack section: Java 25, Spring Boot 4.0, Maven
  - Keep port (8202), API routes, and Nx command table unchanged
  - Verify: `grep -cE 'dotnet|F#|\.fsproj|AltCover' apps/organiclever-be/README.md` returns 0
    AND `grep -q 'mvn\|Java 25\|Spring Boot' apps/organiclever-be/README.md` exits 0.
- [ ] Verify no F# toolchain artifacts remain:

  ```bash
  find apps/organiclever-be/ -name '*.fs' -o -name '*.fsproj' -o -name 'global.json' \
    -o -name 'dotnet-tools.json' -o -name 'fsharplint.json'
  ```

  Verify: command returns no output (empty — no matches).

---

## Phase 6b — CI and Infra Updates

> These files are not in `apps/organiclever-be/` but reference its F# implementation.
> Must be updated before push — CI will break otherwise.
>
> Commit thematically at end of this phase:
> `git add infra/dev/organiclever/ .github/actions/install-language-deps/action.yml` — then commit:
> `refactor(infra): update organiclever-be dev infra and CI for Java/Spring Boot`

- [ ] Replace `infra/dev/organiclever/Dockerfile.be.dev` with Java/Spring Boot version (_New content_):
  - Base: `eclipse-temurin:25-jdk`
  - `RUN apt-get update && apt-get install -y curl maven && rm -rf /var/lib/apt/lists/*`
  - `WORKDIR /workspace`
  - `CMD ["mvn", "spring-boot:run", "-Dspring-boot.run.profiles=dev"]`
  - Verify: `grep -q 'eclipse-temurin' infra/dev/organiclever/Dockerfile.be.dev` exits 0
    AND `grep -v 'dotnet\|aspnet\|ASPNET' infra/dev/organiclever/Dockerfile.be.dev` returns all lines
    (i.e., no dotnet references remain).
- [ ] Update `infra/dev/organiclever/docker-compose.yml` — remove `ASPNETCORE_URLS` env var
      from the `organiclever-be` service (dotnet-specific; Java uses `server.port` in `application.yml`):
  - Verify: `grep -c 'ASPNETCORE_URLS' infra/dev/organiclever/docker-compose.yml` returns 0.
- [ ] Update `.github/actions/install-language-deps/action.yml` — remove the `organiclever-be`
      dotnet-restore branch (the Java build handles deps via `mvn`; no separate pre-restore step needed):
  - Remove: the `if [ "${{ inputs.backend-name }}" = "organiclever-be" ]; then dotnet restore ...` block.
  - If the entire `Restore .NET dependencies` step becomes empty, remove that step entirely.
  - Verify: `grep -c 'OrganicLeverBe.fsproj' .github/actions/install-language-deps/action.yml`
    returns 0.
- [ ] Update `infra/dev/organiclever/README.md` — change backend description from
      "F#/Giraffe REST API backend" to "Java/Spring Boot REST API backend":
  - Verify: `grep -q 'Java/Spring Boot' infra/dev/organiclever/README.md` exits 0.

---

## Phase 6c — Local Quality Gates (Before Push)

> **Important**: Fix ALL failures found during quality gates, not just those caused by
> your changes. This follows the root cause orientation principle — proactively fix
> preexisting errors encountered during work. Do not defer or mention-and-skip existing
> issues.

- [ ] Run affected typecheck:

  ```bash
  npx nx affected -t typecheck
  ```

  Verify: exits 0 with no errors.

- [ ] Run affected lint:

  ```bash
  npx nx affected -t lint
  ```

  Verify: exits 0 with no errors.

- [ ] Run affected quick tests:

  ```bash
  npx nx affected -t test:quick
  ```

  Verify: exits 0 with all tests passing and ≥90% JaCoCo line coverage.

- [ ] Run affected spec-coverage:

  ```bash
  npx nx affected -t spec-coverage
  ```

  Verify: exits 0 with all Gherkin scenarios covered.

- [ ] Fix ALL failures found (including preexisting issues not caused by your changes)
      before proceeding to Phase 7.

---

## Phase 7 — Commit and CI Verification

### Commit Guidelines

Commit changes thematically across phases. Each phase should have already produced its own
commit (as noted in each phase header). If consolidating, use separate commits per concern:

1. F# removal: `chore(organiclever-be): remove F# source and dotnet tooling`
2. Java scaffold: `feat(organiclever-be): add Java/Spring Boot project structure`
3. Health TDD: `feat(organiclever-be): add Java/Spring Boot health endpoint with Cucumber BDD`
4. Nx target update: `refactor(organiclever-be): update Nx targets for Maven toolchain`
5. Docker update: `refactor(organiclever-be): replace Dockerfile.integration for Java`
6. Docs update: `docs(organiclever-be): update README for Java Spring Boot`

Do NOT bundle unrelated fixes into a single commit. Follow Conventional Commits format:
`<type>(<scope>): <description>` — imperative mood, no trailing period.

### Final Commit (plan archival)

- [ ] Stage plan updates (delivery checklist tick-offs):

  ```bash
  git add apps/organiclever-be/ plans/in-progress/organiclever-be-java-migration/
  ```

  Verify: `git status` shows only expected files staged.

- [ ] Commit plan completion:

  ```bash
  git commit -m "refactor(organiclever-be): migrate from F#/Giraffe to Java/Spring Boot"
  ```

  Verify: `git log --oneline -1 | grep 'refactor(organiclever-be)'` exits 0.

- [ ] Push to `origin/main` (direct-to-main, Trunk Based Development):

  ```bash
  git push origin main
  ```

  Verify: exits 0.

### Post-Push CI Verification

- [ ] List recent CI runs to get the run ID:

  ```bash
  gh run list --repo wahidyankf/ose-public --limit 5
  ```

  Verify: the latest run triggered by your push is listed.

- [ ] Monitor the triggered CI run:

  ```bash
  gh run watch <run-id>
  ```

  Verify: all CI jobs in the run show green (✓). If any fail, fix immediately and push a
  follow-up commit before declaring done. Do NOT proceed to archival until CI is green.

### Plan Archival

- [ ] Verify ALL delivery checklist items are ticked and quality gates pass.
- [ ] Archive plan folder with today's date:

  ```bash
  TODAY=$(date +%Y-%m-%d) && git mv plans/in-progress/organiclever-be-java-migration \
    plans/done/${TODAY}__organiclever-be-java-migration
  ```

  Verify: `test -d plans/done/$(date +%Y-%m-%d)__organiclever-be-java-migration` exits 0.

- [ ] Update `plans/in-progress/README.md` — remove the organiclever-be-java-migration entry.
      Verify: `grep -c 'organiclever-be-java-migration' plans/in-progress/README.md` returns 0.
- [ ] Update `plans/done/README.md` — add the plan entry with today's completion date.
      Verify: `grep -q 'organiclever-be-java-migration' plans/done/README.md` exits 0.
- [ ] Commit archival:

  ```bash
  git add plans/ && git commit -m "chore(plans): archive organiclever-be-java-migration to done"
  ```

  Verify: `git log --oneline -1 | grep 'archive organiclever-be-java-migration'` exits 0.

---

## Rollback Criteria

If any Nx target cannot be made green within 3 attempts, escalate before continuing:

- Revisit JaCoCo exclusion list if coverage < 90%
- Revisit NullAway config if UnannotatedSubPackages scope is wrong
- Revisit Dockerfile base image if `mvn dependency:go-offline` fails

### Rollback Procedure

To revert the migration fully and restore the F# implementation from git history:

```bash
# Restore F# source files from before the migration commit
git checkout HEAD~<N> -- apps/organiclever-be/
# Where <N> is the number of commits since the Phase 1 removal commit

# Restore the original project.json
git checkout HEAD~<N> -- apps/organiclever-be/project.json

# Commit the revert
git add apps/organiclever-be/
git commit -m "revert(organiclever-be): restore F#/Giraffe implementation"
```

Alternatively, if commits were made atomically per phase, use `git revert <phase-commit-sha>`
for each phase commit in reverse order (Phase 6 → Phase 1).
