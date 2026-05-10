# Business Requirements Document — organiclever-be-java-migration

## Business Problem

The OSE Platform must operate effectively in financial-industry contexts where Java/Spring Boot
is the dominant backend technology (banking cores, payment gateways, Islamic finance systems).
The current `organiclever-be` is implemented in F#/Giraffe — a functional, niche stack that
provides minimal JVM learning value. A working Spring Boot implementation on a real, contracted
API surface trains the full JVM workflow that practitioners encounter daily in Sharia-compliant
enterprise environments.

## Business Goals

| ID | Goal |
|----|------|
| BG-1 | Gain hands-on Spring Boot 4.0 experience with a production-quality toolchain |
| BG-2 | Align the OSE Platform backend with financial-industry standard (JVM/Spring) |
| BG-3 | Maintain full feature parity with the current v0 API (health endpoint) |
| BG-4 | Preserve CI quality gate integrity (coverage, lint, spec-coverage, integration tests) |
| BG-5 | Provide a live reference for future Spring Boot features (auth, CRUD, DB) |

## Business Value

- **Skill transfer**: Spring Boot is used in 70%+ of enterprise Java backends in financial
  services. Mastery here directly enables OrganicLever feature development and future
  Sharia-compliance engine work.
- **Ecosystem familiarity**: Maven, JaCoCo, Checkstyle, PMD, NullAway, and Cucumber-JVM are
  the exact toolchain developers encounter in Indonesian and global Islamic finance firms.
- **No regression**: The OpenAPI contract is unchanged, so `organiclever-web` and
  `organiclever-be-e2e` require zero modification.
- **Template value**: The resulting implementation becomes a second reference alongside
  `ose-primer/apps/crud-be-java-springboot` for onboarding future contributors.

## Constraints

| ID | Constraint |
|----|-----------|
| C-1 | API contract (`specs/apps/organiclever/containers/contracts/`) must not change |
| C-2 | Nx target names must remain identical (`build`, `dev`, `start`, `lint`, `typecheck`, `test:quick`, `test:unit`, `test:integration`, `spec-coverage`, `codegen`) |
| C-3 | Coverage threshold ≥90% (JaCoCo XML; same as `crud-be-java-springboot`) |
| C-4 | Dev port remains 8202 |
| C-5 | Java 25, Spring Boot 4.0 (matching `ose-primer` reference) |
| C-6 | No new API endpoints — v0 scope is health check only |

## Success Criteria

- All six quality gate Nx targets pass locally and on CI
- `organiclever-be-e2e` BE E2E tests pass without modification
- No F# toolchain artifacts remain (`*.fsproj`, `global.json`, `dotnet-tools.json`,
  `fsharplint.json`, `*.fs` source files)
- `project.json` tags read `lang:java` and `platform:spring-boot`
