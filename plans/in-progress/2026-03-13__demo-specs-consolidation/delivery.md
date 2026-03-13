# Delivery Checklist

## Phase 1: Directory Restructuring

- [ ] Create `specs/apps/demo/` directory
- [ ] `git mv` BE gherkin specs: `specs/apps/demo-be/gherkin/` → `specs/apps/demo/be/gherkin/`
- [ ] `git mv` BE .gitignore if present
- [ ] `git mv` FE gherkin specs: `specs/apps/demo-fe/gherkin/` → `specs/apps/demo/fe/gherkin/`
- [ ] Remove old `specs/apps/demo-be/` directory (after extracting gherkin)
- [ ] Remove old `specs/apps/demo-fe/` directory (after extracting gherkin)
- [ ] Verify new directory structure matches target layout

## Phase 2: C4 Diagram Merge

- [ ] Write unified `specs/apps/demo/c4/context.md` (L1 — combined system context)
- [ ] Write unified `specs/apps/demo/c4/container.md` (L2 — all containers in one diagram)
- [ ] Adapt `specs/apps/demo/c4/component-be.md` (L3 — from old demo-be component.md)
- [ ] Adapt `specs/apps/demo/c4/component-fe.md` (L3 — from old demo-fe component.md)
- [ ] Write `specs/apps/demo/c4/README.md` (index with 4 diagrams)

## Phase 3: README Files

- [ ] Write `specs/apps/demo/README.md` (unified overview, links to be/, fe/, c4/)
- [ ] Write `specs/apps/demo/be/README.md` (adapted from old demo-be/README.md)
- [ ] Write `specs/apps/demo/fe/README.md` (adapted from old demo-fe/README.md)
- [ ] Verify `specs/apps/demo/be/gherkin/README.md` exists and links are correct
- [ ] Verify `specs/apps/demo/fe/gherkin/README.md` exists and links are correct
- [ ] Update internal cross-references in all README files

## Phase 4: Specs Validation Gate (OCD Mode)

Run the [specs-validation workflow](../../../governance/workflows/specs/specs-validation.md) in
**OCD mode** on the newly merged `specs/apps/demo/` to catch all issues before propagating paths
to 11 backends. This is the quality gate — fix everything in the specs themselves before touching
any application code.

- [ ] Run specs-validation workflow: `folders: [specs/apps/demo], mode: ocd`
- [ ] All 7 validation categories pass at ZERO findings:
  - [ ] Structural Completeness — every directory has README.md
  - [ ] Feature File Inventory — README counts match actual .feature files and scenarios
  - [ ] Gherkin Format Compliance — headers, user stories, Background steps, naming
  - [ ] Cross-Spec Consistency — `be/` and `fe/` shared domains align (auth, expenses, etc.)
  - [ ] C4 Diagram Consistency — accessible colors, actor coherence across L1/L2/L3
  - [ ] Cross-Reference Integrity — all markdown links resolve
  - [ ] Spec-to-Implementation Alignment — README references point to real implementations
- [ ] Commit specs fixes (if any) before proceeding to path rewiring

**Why before Phase 5**: If we rewire 107+ files to point at broken specs, we'd have to fix specs
AND re-validate all 11 backends. Fixing specs in isolation (before rewiring) keeps the blast
radius contained to `specs/apps/demo/` only.

## Phase 5: Backend Path Updates (HIGH priority)

### Project Configuration (11 files)

- [ ] `apps/demo-be-java-springboot/project.json`
- [ ] `apps/demo-be-java-vertx/project.json`
- [ ] `apps/demo-be-kotlin-ktor/project.json`
- [ ] `apps/demo-be-golang-gin/project.json`
- [ ] `apps/demo-be-python-fastapi/project.json`
- [ ] `apps/demo-be-rust-axum/project.json`
- [ ] `apps/demo-be-ts-effect/project.json`
- [ ] `apps/demo-be-fsharp-giraffe/project.json`
- [ ] `apps/demo-be-elixir-phoenix/project.json`
- [ ] `apps/demo-be-csharp-aspnetcore/project.json`
- [ ] `apps/demo-be-clojure-pedestal/project.json`

### Build Configuration (3 files)

- [ ] `apps/demo-be-java-springboot/pom.xml`
- [ ] `apps/demo-be-java-vertx/pom.xml`
- [ ] `apps/demo-be-kotlin-ktor/build.gradle.kts`

### Test Runner Configuration (~12 files)

- [ ] `apps/demo-be-golang-gin/internal/bdd/suite_test.go`
- [ ] `apps/demo-be-golang-gin/internal/integration/suite_test.go`
- [ ] `apps/demo-be-golang-gin/internal/integration_pg/suite_test.go`
- [ ] `apps/demo-be-python-fastapi/tests/unit/conftest.py`
- [ ] `apps/demo-be-python-fastapi/tests/integration/conftest.py`
- [ ] `apps/demo-be-rust-axum/tests/unit/main.rs`
- [ ] `apps/demo-be-rust-axum/tests/integration/main.rs`
- [ ] `apps/demo-be-ts-effect/.cucumber.js`
- [ ] `apps/demo-be-ts-effect/cucumber.js`
- [ ] `apps/demo-be-ts-effect/cucumber.unit.js`
- [ ] `apps/demo-be-elixir-phoenix/config/test.exs`
- [ ] `apps/demo-be-elixir-phoenix/config/integration.exs`

### Docker Configuration (~16 files)

- [ ] `apps/demo-be-fsharp-giraffe/Dockerfile.integration`
- [ ] `apps/demo-be-csharp-aspnetcore/Dockerfile.integration`
- [ ] `apps/demo-be-kotlin-ktor/Dockerfile.integration`
- [ ] `apps/demo-be-ts-effect/Dockerfile.integration`
- [ ] `apps/demo-be-clojure-pedestal/Dockerfile.integration`
- [ ] `apps/demo-be-python-fastapi/docker-compose.integration.yml`
- [ ] `infra/dev/demo-be-java-springboot/docker-compose.yml`
- [ ] `infra/dev/demo-be-java-vertx/docker-compose.yml`
- [ ] `infra/dev/demo-be-kotlin-ktor/docker-compose.yml`
- [ ] `infra/dev/demo-be-golang-gin/docker-compose.yml`
- [ ] `infra/dev/demo-be-python-fastapi/docker-compose.yml`
- [ ] `infra/dev/demo-be-fsharp-giraffe/docker-compose.yml`
- [ ] `infra/dev/demo-be-elixir-phoenix/docker-compose.yml`
- [ ] `infra/dev/demo-be-clojure-pedestal/docker-compose.yml`
- [ ] `infra/dev/demo-be-rust-axum/docker-compose.yml`
- [ ] `infra/dev/demo-be-csharp-aspnetcore/docker-compose.yml`

### E2E Test Suite

- [ ] `apps/demo-be-e2e/playwright.config.ts`

## Phase 6: Documentation Updates (LOW priority)

- [ ] `CLAUDE.md` — update specs path references
- [ ] `apps/demo-be-java-springboot/README.md`
- [ ] `apps/demo-be-java-vertx/README.md`
- [ ] `apps/demo-be-golang-gin/README.md`
- [ ] `apps/demo-be-python-fastapi/README.md`
- [ ] `apps/demo-be-elixir-phoenix/README.md`
- [ ] `apps/demo-be-fsharp-giraffe/README.md`
- [ ] `apps/demo-be-rust-axum/README.md`
- [ ] `apps/demo-be-ts-effect/README.md`
- [ ] `apps/demo-be-csharp-aspnetcore/README.md`
- [ ] `apps/demo-be-kotlin-ktor/README.md`
- [ ] `apps/demo-be-clojure-pedestal/README.md`
- [ ] `apps/demo-be-e2e/README.md`
- [ ] `governance/development/infra/bdd-spec-test-mapping.md`
- [ ] `governance/development/quality/three-level-testing-standard.md`
- [ ] `governance/development/infra/nx-targets.md`
- [ ] `docs/explanation/software-engineering/automation-testing/tools/playwright/ex-soen-aute-to-pl__bdd.md`
- [ ] `governance/conventions/formatting/diagrams.md`
- [ ] `governance/workflows/specs/specs-validation.md` (update example paths)
- [ ] `specs/apps-labs/README.md`
- [ ] `.claude/agents/specs-checker.md` (update example folder paths)
- [ ] `.claude/agents/specs-maker.md` (update example target paths)
- [ ] `.claude/agents/specs-fixer.md` (update example paths)
- [ ] Run `npm run sync:claude-to-opencode` to sync `.opencode/agent/specs-*.md`

## Phase 7: Historical Plans (LOW priority)

- [ ] Update `plans/done/` references where trivially fixable
- [ ] Accept that some historical references may remain as-is

## Phase 8: Stale Reference Check

- [ ] `grep -r "specs/apps/demo-be" . --include='*.json' --include='*.xml' --include='*.yml' --include='*.yaml' --include='*.ts' --include='*.js' --include='*.go' --include='*.rs' --include='*.py' --include='*.exs' --include='*.ex' --include='*.fs' --include='*.cs' --include='*.kts' --include='*.clj' --include='*.edn' --include='*.md' --exclude-dir=node_modules --exclude-dir=.nx --exclude-dir=dist --exclude-dir=target --exclude-dir=build --exclude-dir=.features-gen --exclude-dir=generated-reports` — returns nothing (except `plans/done/`)
- [ ] `grep -r "specs/apps/demo-fe" . --include='*.json' --include='*.ts' --include='*.md' --include='*.yml' --include='*.yaml' --exclude-dir=node_modules --exclude-dir=.nx --exclude-dir=dist --exclude-dir=target --exclude-dir=build --exclude-dir=.features-gen --exclude-dir=generated-reports` — returns nothing (except `plans/done/`)
- [ ] Verify no stale references remain outside `plans/done/` historical records

## Phase 9: Local Validation — Lint and Typecheck

- [ ] `npm run lint:md` passes
- [ ] `npm run format:md:check` passes
- [ ] `nx affected -t lint` passes
- [ ] `nx affected -t typecheck` passes

## Phase 10: Local Validation — test:quick (All 11 Backends)

- [ ] `nx run demo-be-java-springboot:test:quick` passes
- [ ] `nx run demo-be-java-vertx:test:quick` passes
- [ ] `nx run demo-be-kotlin-ktor:test:quick` passes
- [ ] `nx run demo-be-golang-gin:test:quick` passes
- [ ] `nx run demo-be-python-fastapi:test:quick` passes
- [ ] `nx run demo-be-rust-axum:test:quick` passes
- [ ] `nx run demo-be-ts-effect:test:quick` passes
- [ ] `nx run demo-be-fsharp-giraffe:test:quick` passes
- [ ] `nx run demo-be-elixir-phoenix:test:quick` passes
- [ ] `nx run demo-be-csharp-aspnetcore:test:quick` passes
- [ ] `nx run demo-be-clojure-pedestal:test:quick` passes

## Phase 11: Local Validation — Non-Backend Projects

- [ ] `nx run organiclever-web:test:quick` passes
- [ ] `nx run rhino-cli:test:quick` passes
- [ ] `nx run ayokoding-cli:test:quick` passes
- [ ] `nx run oseplatform-cli:test:quick` passes

## Phase 12: Push and GitHub Actions — Main CI

- [ ] Commit and push to main
- [ ] Main CI workflow passes (runs `test:quick` for all affected projects)

## Phase 13: GitHub Actions — Integration + E2E (All 11 Backends)

Trigger all 11 integration + E2E workflows manually and verify they pass:

- [ ] `test-integration-e2e-demo-be-java-springboot` — SUCCESS
- [ ] `test-integration-e2e-demo-be-java-vertx` — SUCCESS
- [ ] `test-integration-e2e-demo-be-kotlin-ktor` — SUCCESS
- [ ] `test-integration-e2e-demo-be-golang-gin` — SUCCESS
- [ ] `test-integration-e2e-demo-be-python-fastapi` — SUCCESS
- [ ] `test-integration-e2e-demo-be-rust-axum` — SUCCESS
- [ ] `test-integration-e2e-demo-be-ts-effect` — SUCCESS
- [ ] `test-integration-e2e-demo-be-fsharp-giraffe` — SUCCESS
- [ ] `test-integration-e2e-demo-be-elixir-phoenix` — SUCCESS
- [ ] `test-integration-e2e-demo-be-csharp-aspnetcore` — SUCCESS
- [ ] `test-integration-e2e-demo-be-clojure-pedestal` — SUCCESS

## Phase 14: GitHub Actions — Other Workflows

- [ ] `test-integration-e2e-organiclever-web` — SUCCESS (trigger manually)
- [ ] `pr-validate-links` — no broken links from path changes
- [ ] `pr-format` — no formatting issues
- [ ] `pr-quality-gate` — passes

## Phase 15: Cleanup

- [ ] Move this plan to `plans/done/`
- [ ] Update `plans/in-progress/README.md`
- [ ] Update `plans/done/README.md`
