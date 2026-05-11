# Business Requirements Document — organiclever-be-java-migration

## Business Problem

The OSE Platform must operate effectively in financial-industry contexts where Java/Spring Boot
is the dominant backend technology (banking cores, payment gateways, Islamic finance systems).
The current `organiclever-be` is implemented in F#/Giraffe — a functional, niche stack that
provides minimal JVM learning value. A working Spring Boot implementation on a real, contracted
API surface trains the full JVM workflow that practitioners encounter daily in Sharia-compliant
enterprise environments.

## Business Goals

| ID   | Goal                                                                                  |
| ---- | ------------------------------------------------------------------------------------- |
| BG-1 | Gain hands-on Spring Boot 4.0 experience with a production-quality toolchain          |
| BG-2 | Align the OSE Platform backend with financial-industry standard (JVM/Spring)          |
| BG-3 | Maintain full feature parity with the current v0 API (health endpoint)                |
| BG-4 | Preserve CI quality gate integrity (coverage, lint, spec-coverage, integration tests) |
| BG-5 | Provide a live reference for future Spring Boot features (auth, CRUD, DB)             |

## Business Value

- **Skill transfer**: _Judgment call — industry surveys circa 2024:_ Spring Boot is the
  dominant backend framework in financial-industry Java shops; no baseline measured for
  exact percentage. Mastery here directly enables OrganicLever feature development and
  future Sharia-compliance engine work.
- **Ecosystem familiarity**: Maven, JaCoCo, Checkstyle, PMD, NullAway, and Cucumber-JVM are
  the exact toolchain developers encounter in Indonesian and global Islamic finance firms.
- **No regression**: The OpenAPI contract is unchanged, so `organiclever-web` and
  `organiclever-be-e2e` require zero modification.
- **Template value**: The resulting implementation becomes a second reference alongside
  `ose-primer/apps/crud-be-java-springboot` for onboarding future contributors.

## Constraints

| ID  | Constraint                                                                                                                                                      |
| --- | --------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| C-1 | API contract (`specs/apps/organiclever/containers/contracts/`) must not change                                                                                  |
| C-2 | Nx target names must remain identical (`build`, `dev`, `start`, `lint`, `typecheck`, `test:quick`, `test:unit`, `test:integration`, `spec-coverage`, `codegen`) |
| C-3 | Coverage threshold ≥90% (JaCoCo XML; same as `crud-be-java-springboot`)                                                                                         |
| C-4 | Dev port remains 8202                                                                                                                                           |
| C-5 | Java 25, Spring Boot 4.0 (matching `ose-primer` reference)                                                                                                      |
| C-6 | No new API endpoints — v0 scope is health check only                                                                                                            |

## Success Criteria

- All six quality gate Nx targets pass locally and on CI
- `organiclever-be-e2e` BE E2E tests pass without modification
- No F# toolchain artifacts remain (`*.fsproj`, `global.json`, `dotnet-tools.json`,
  `fsharplint.json`, `*.fs` source files)
- `project.json` tags read `lang:java` and `platform:spring-boot`

## Affected Roles

| Role         | Hat                                          | Involvement                                      |
| ------------ | -------------------------------------------- | ------------------------------------------------ |
| BE Developer | Solo maintainer wearing backend engineer hat | Executes the migration; primary implementor      |
| DevOps       | Solo maintainer wearing infrastructure hat   | Manages Docker Dockerfile.integration changes    |
| Tech Lead    | Solo maintainer wearing architecture hat     | Reviews design decisions; approves quality gates |

Consuming agents: `plan-execution` workflow (executes delivery checklist),
`plan-execution-checker` (validates completed work), `swe-java-dev` (suggested executor for
Java source edits).

## Business Non-Goals

- No new business features (authentication, CRUD endpoints, database integration) — this
  migration is a pure technology swap of the existing health-only v0 API
- No frontend changes — `organiclever-web` is unaffected
- No API contract changes — the OpenAPI spec at
  `specs/apps/organiclever/containers/contracts/` is frozen for this migration
- No cost reduction or infrastructure savings — the migration does not change deployment
  topology or operational costs
- Not a step toward multi-tenant deployment — single-maintainer scope throughout

## Business Risks

| Risk                                                                                                       | Likelihood | Mitigation                                                                                                                                |
| ---------------------------------------------------------------------------------------------------------- | ---------- | ----------------------------------------------------------------------------------------------------------------------------------------- |
| Java 25 EA toolchain not installed in local `npm run doctor` setup                                         | Medium     | Run `npm run doctor -- --fix` in Phase 0.5; Java 25 is managed by Volta. If unavailable, install via `sdk install java 25-open` (SDKMAN). |
| Maven dependency resolution failures in isolated CI (Maven Central rate limits or artifact unavailability) | Low        | Use `mvn dependency:go-offline` in Dockerfile to pre-cache; CI environment has Maven Central access.                                      |
| ose-public rhino-cli lacks `contracts java-clean-imports` and `java validate-annotations` subcommands      | Confirmed  | Per tech-docs.md NOTE, these are ose-primer-only; omit from Nx targets. Adopt via ose-primer propagation plan when available.             |
| JaCoCo 90% coverage hard to achieve with health-only scope                                                 | Low        | JaCoCo exclusions configured for application main, package-info, generated contracts, and config classes — coverage should be achievable. |
