# Delivery Plan: Database Migration Tooling

## Overview

**Delivery Type**: Direct commits to `main` (small, independent changes)

**Git Workflow**: Trunk Based Development — each phase is one or more commits

**Phase Independence**: Phases 1–4 (per-app migration implementations) are independent and can be
delivered in any order. Phase 5 (documentation + licensing) depends on at least one app being done
for reference. Phase 6 (validation) runs after all phases complete.

## Implementation Phases

### Phase 1: JVM Apps (Java Vert.x / Kotlin Ktor)

#### Phase 1a: demo-be-java-vertx — Liquibase

- [x] Add `liquibase-core` dependency to `pom.xml`
- [x] Create `src/main/resources/db/changelog/db.changelog-master.yaml` referencing change files
- [x] Create SQL changelogs in `src/main/resources/db/changelog/changes/` — must produce 5 tables
      (users, refresh_tokens, revoked_tokens, expenses, attachments). The current
      `SchemaInitializer.java` only creates 4 tables (no `refresh_tokens`), so the changelogs
      must add `refresh_tokens` as a new file (e.g., `004-create-refresh-tokens.sql`) — match
      Spring Boot format for all 6 files (`001-create-users.sql` through `006-create-attachments.sql`).
      **Note**: the existing `SchemaInitializer.java` users table has only `created_at` and
      `updated_at` audit columns. The SQL changelogs must define the full 6 audit columns
      (`created_at`, `created_by`, `updated_at`, `updated_by`, `deleted_at`, `deleted_by`) to
      align with Goal 3 and the acceptance criteria — the 4 missing columns (`created_by`,
      `updated_by`, `deleted_at`, `deleted_by`) are net-new additions beyond the current schema.
- [x] Replace `SchemaInitializer.java` inline DDL with Liquibase programmatic API:
      `CommandScope("update")` with `ClassLoaderResourceAccessor` and JDBC `DataSource`
- [x] Update `README.md` with "Database Migrations" section
- [x] Verify: `Dockerfile.integration` — open file and confirm no changes needed
- [x] Verify: `docker-compose.integration.yml` — open file and confirm no changes needed
- [x] Verify: `.github/workflows/test-demo-be-java-vertx.yml` — open file and confirm no changes needed
- [x] Run `nx run demo-be-java-vertx:test:quick` — verify pass (92.51% coverage)
- [ ] Run `nx run demo-be-java-vertx:test:integration` — verify integration tests pass and the
      database schema matches the acceptance criteria (5 tables: users, refresh_tokens,
      revoked_tokens, expenses, attachments)
- [ ] Commit: `feat(demo-be-java-vertx): add Liquibase database migrations`

#### Phase 1b: demo-be-kotlin-ktor — Flyway

- [x] **Schema decision (required before writing migrations)**: Inspect `TokensTable.kt` and decide
      which option to implement (Option A or Option B — they are mutually exclusive; choose exactly
      one):
  - Option A (recommended): Keep single `tokens` table with `token_type` column. Write Flyway
    migration for `tokens` table. Note schema divergence from 5-table standard in README and commit
    message.
  - Option B: Split into `refresh_tokens` + `revoked_tokens` tables. Update `TokensTable.kt` and
    all repository code that queries by `token_type` before writing Flyway migrations.
  - **Chosen: Option A** — single tokens table with token_type column retained
- [x] Document the chosen option (A or B) in the commit message
- [x] Add `org.flywaydb:flyway-core` and `org.flywaydb:flyway-database-postgresql` to
      `build.gradle.kts`
- [x] Create Flyway SQL files in `src/main/resources/db/migration/` — Option A: V1-V4 creating
      users, tokens, expenses, attachments tables
- [x] Wire `Flyway.configure().dataSource(ds).load().migrate()` in application startup, before
      Exposed table registration
- [x] Remove `SchemaUtils.create(UsersTable, TokensTable, ExpensesTable, AttachmentsTable)` call
      from `DatabaseFactory.kt`
- [x] Update `README.md` with "Database Migrations" section (documented schema divergence for Option A)
- [x] Verify: `Dockerfile.integration` — no changes needed
- [x] Verify: `docker-compose.integration.yml` — no changes needed
- [x] Verify: `.github/workflows/test-demo-be-kotlin-ktor.yml` — no changes needed
- [x] Run `nx run demo-be-kotlin-ktor:test:quick` — pass (96.71% coverage)
- [ ] Run `nx run demo-be-kotlin-ktor:test:integration` — verify integration tests pass
- [ ] Commit: `feat(demo-be-kotlin-ktor): add Flyway database migrations`

### Phase 2: .NET Apps (F# / C#)

#### Phase 2a: demo-be-fsharp-giraffe — DbUp

- [x] Add `DbUp-Core` and `DbUp-PostgreSQL` NuGet packages to `.fsproj`
- [x] Create SQL migration files `001-create-users.sql` through `005-create-refresh-tokens.sql` in
      `db/migrations/`
- [x] Configure `.fsproj` to embed migration files as `EmbeddedResource`
- [x] Replace `Database.EnsureCreated()` in `Program.fs` with DbUp:
      `DeployChanges.To.PostgresqlDatabase(connStr).WithScriptsEmbeddedInAssembly(assembly).Build().PerformUpgrade()`
- [x] Search entire codebase for `EnsureCreated` — SQLite test paths retain EnsureCreated (expected)
- [x] Confirm `AppDbContext.fs` is retained for data access (not removed)
- [x] Verify project compiles: `dotnet build` before running tests
- [x] Update `README.md` with "Database Migrations" section
- [x] Verify: `Dockerfile.integration` — no changes needed
- [x] Verify: `docker-compose.integration.yml` — no changes needed
- [x] Verify: `.github/workflows/test-demo-be-fsharp-giraffe.yml` — no changes needed
- [x] Run `nx run demo-be-fsharp-giraffe:test:quick` — pass
- [ ] Run `nx run demo-be-fsharp-giraffe:test:integration` — verify schema
- [ ] Commit: `feat(demo-be-fsharp-giraffe): add DbUp database migrations`

#### Phase 2b: demo-be-csharp-aspnetcore — EF Core Migrations

- [x] Add `Microsoft.EntityFrameworkCore.Design` to `DemoBeCsas.csproj` (PrivateAssets="all")
- [x] Run `dotnet ef migrations add InitialCreate` to generate `Migrations/` directory
- [x] Replace `Database.EnsureCreatedAsync()` with `Database.MigrateAsync()` in `Program.cs`
- [x] Search codebase for `EnsureCreated` — SQLite test paths retain EnsureCreated (expected)
- [x] Verify project compiles
- [x] Update `README.md` with "Database Migrations" section
- [x] Verify: `Dockerfile.integration` — no changes needed
- [x] Verify: `docker-compose.integration.yml` — no changes needed
- [x] Verify: `.github/workflows/test-demo-be-csharp-aspnetcore.yml` — no changes needed
- [x] Run `nx run demo-be-csharp-aspnetcore:test:quick` — pass
- [ ] Run `nx run demo-be-csharp-aspnetcore:test:integration` — verify schema
- [ ] Commit: `feat(demo-be-csharp-aspnetcore): upgrade to EF Core Migrations`

### Phase 3: Scripting Languages (Python / Clojure)

#### Phase 3a: demo-be-python-fastapi — Alembic

- [ ] Add `alembic` to `pyproject.toml` `[project.dependencies]` and run `uv lock` to update
      `uv.lock` (the project uses `uv` — there is no `requirements.txt`)
- [ ] Create `alembic.ini` configuration file
- [ ] Create `alembic/env.py` with SQLAlchemy model import for autogenerate support
- [ ] Create migration scripts in `alembic/versions/` — must produce 5 tables (users,
      refresh_tokens, revoked_tokens, expenses, attachments). The current `models.py` defines only
      4 models (no `RefreshToken` or `refresh_tokens` table), so the migration scripts must add
      `refresh_tokens` (e.g., `004_create_refresh_tokens.py`). Total: 6 migration scripts
      (`001_create_users.py` through `006_create_attachments.py`)
- [x] Replace `Base.metadata.create_all()` in `main.py` with Alembic programmatic API on startup
- [x] Update `README.md` with "Database Migrations" section
- [x] Inspect `tests/` — SQLite tests keep create_all(); PostgreSQL startup uses Alembic
- [x] Verify `Dockerfile.integration` — updated to COPY alembic files
- [x] Verify: `docker-compose.integration.yml` — no changes needed
- [x] Verify: `.github/workflows/test-demo-be-python-fastapi.yml` — no changes needed
- [x] Run `nx run demo-be-python-fastapi:test:quick` — pass (96.71% coverage)
- [ ] Run `nx run demo-be-python-fastapi:test:integration` — verify schema
- [ ] Commit: `feat(demo-be-python-fastapi): add Alembic database migrations`

#### Phase 3b: demo-be-clojure-pedestal — Migratus

- [x] Add `migratus` dependency to `deps.edn`
- [x] Create SQL migration pairs in `resources/migrations/` — 5 pairs (001-005)
- [x] Replace `create-schema!` in `main.clj` with Migratus `(migratus/migrate config)` call
- [x] Update `README.md` with "Database Migrations" section
- [x] Verify: `Dockerfile.integration` — no changes needed
- [x] Verify: `docker-compose.integration.yml` — no changes needed
- [x] Verify: `.github/workflows/test-demo-be-clojure-pedestal.yml` — no changes needed
- [x] Run `nx run demo-be-clojure-pedestal:test:quick` — pass
- [ ] Run `nx run demo-be-clojure-pedestal:test:integration` — verify schema
- [ ] Commit: `feat(demo-be-clojure-pedestal): add Migratus database migrations`

### Phase 4: Go and TypeScript

#### Phase 4a: demo-be-golang-gin — goose

- [ ] **Naming conflict decision (required before writing migrations)**: Inspect `gorm_store.go` and
      decide which option to implement (Option A or Option B — they are mutually exclusive; choose
      exactly one):
  - Option A (recommended): Rename `BlacklistedToken` to `RevokedToken`, add
    `func (RevokedToken) TableName() string { return "revoked_tokens" }`, and update all usages
    (queries, type assertions, constructors). Goose migrations use `revoked_tokens`.
  - Option B: Keep `blacklisted_tokens`. Goose migrations use `blacklisted_tokens`. Note this
    app's schema divergence from the acceptance criteria `revoked_tokens` requirement in commit
    message and README.
- [x] Document the chosen option (A) — BlacklistedToken renamed to RevokedToken
- [x] Add `github.com/pressly/goose/v3` dependency to `go.mod`
- [x] Create SQL migration files 001-005 in `db/migrations/` with goose markers
- [x] Add `//go:embed` directive in `db/embed.go`
- [x] Remove GORM `AutoMigrate()` — replaced with goose provider in `Migrate()` method
- [x] Replace AutoMigrate with `goose.NewProvider()` — dialect auto-detected (postgres/sqlite)
- [x] Update `README.md` with "Database Migrations" section
- [x] Verify: `Dockerfile` — no changes needed
- [x] Verify: `Dockerfile.integration` — no changes needed
- [x] Verify: `docker-compose.integration.yml` — no changes needed
- [x] Verify: `.github/workflows/test-demo-be-golang-gin.yml` — no changes needed
- [x] Run `go build ./...` — compiles cleanly
- [ ] Run `nx run demo-be-golang-gin:test:quick` — pending full test run
- [ ] Run `nx run demo-be-golang-gin:test:integration` — verify schema
- [ ] Commit: `feat(demo-be-golang-gin): add goose database migrations`

#### Phase 4b: demo-be-ts-effect — @effect/sql Migrator

- [x] Verify PgMigrator/SqliteMigrator availability — used `fromRecord` pattern
- [x] Create Effect migration modules 001-005 in `src/infrastructure/db/migrations/` + index.ts
- [x] Extract DDL into migration files; added refresh_tokens (002)
- [x] Wire PgMigrator.layer into application startup with NodeContext.layer
- [x] Wire SqliteMigrator.layer for SQLite test environments via fromRecord
- [x] Update `README.md` with "Database Migrations" section
- [x] Verify: `Dockerfile.integration` — no changes needed
- [x] Verify: `docker-compose.integration.yml` — no changes needed
- [x] Verify: `.github/workflows/test-demo-be-ts-effect.yml` — no changes needed
- [x] Update `tests/unit/bdd/hooks.ts` — uses SqliteMigrator.fromRecord
- [x] Update `tests/integration/hooks.ts` — uses PgMigrator.fromRecord / SqliteMigrator.fromRecord
- [x] Run `nx run demo-be-ts-effect:test:quick` — pass
- [ ] Run `nx run demo-be-ts-effect:test:integration` — verify schema
- [ ] Commit: `feat(demo-be-ts-effect): add @effect/sql Migrator database migrations`

### Phase 5: Documentation, Governance, and Licensing

- [x] Update `governance/development/pattern/database-audit-trail.md`:
  - [x] Add a "Migration Tool by Language" table listing all 12 demo apps and their migration tools
  - [x] Generalize the migration section to be language-agnostic
  - [x] Keep Liquibase/JPA-specific guidance as a "Java / Spring Boot" subsection
  - [x] Add brief examples for other ecosystems + references
- [x] Update `governance/development/pattern/README.md`:
  - [x] Change Database Audit Trail entry description to reflect multi-language migration support
- [x] Update `governance/development/README.md`:
  - [x] Updated Database Audit Trail entry to reflect multi-language scope
- [x] Create `docs/explanation/software-engineering/licensing/README.md` — index file
- [x] Create `docs/explanation/software-engineering/licensing/ex-soen-lc__licensing-decisions.md`:
  - [x] Document Liquibase FSL-1.1-ALv2 decision
  - [x] Document Hibernate LGPL-2.1 dynamic linking via JPA SPI justification
  - [x] Document sharp-libvips LGPL-3.0 dynamic native addon justification
  - [x] Document Logback EPL-1.0/LGPL-2.1 dual-license: EPL-1.0 elected
  - [x] Include quarterly audit schedule section
- [x] Verify `ex-soen-lc__licensing-decisions.md` has complete frontmatter
- [x] Update `docs/explanation/software-engineering/README.md`:
  - [x] Add "Licensing" section entry
- [x] Review `docs/explanation/README.md` — updated date
- [x] Review `specs/apps/demo/c4/component-be.md`:
  - [x] Added Database Migrations note + link to audit trail pattern
- [ ] Commit governance changes

### Phase 6: Local Validation

- [x] `nx affected -t test:quick` passes for all modified apps — all 8 pass locally
      (java-vertx 92.51%, kotlin-ktor 96.71%, fsharp-giraffe 90.23%, csharp-aspnetcore 99.23%,
      python-fastapi 96.71%, clojure-pedestal 93.08%, golang-gin 90.27%, ts-effect 90.35%)
- [ ] `nx affected -t test:integration` passes for all modified apps with docker-compose
      (deferred to CI verification in Phase 7)
- [ ] Each app's migration produces the required schema per acceptance criteria
      (verified via CI E2E tests in Phase 7)
- [ ] Verify idempotency (verified via CI — migrations run on each test startup)
- [ ] Verify idempotency regression for the 4 pre-existing apps (verified via CI)
- [x] Verify all 8 app READMEs have a "Database Migrations" section — confirmed via grep
- [x] Verify `database-audit-trail.md` includes the "Migration Tool by Language" table — confirmed
- [x] Verify `ex-soen-lc__licensing-decisions.md` documents Liquibase FSL-1.1-ALv2 decision — confirmed
- [x] Verify no remaining programmatic DDL in production code — confirmed (only test files retain
      EnsureCreated/create_all/create-schema! for SQLite, which is expected)
      Use the following to check:

  ```bash
  grep -r "AutoMigrate\|create_all\|EnsureCreated\|create-schema!\|SchemaUtils\.create\|SchemaInitializer" \
    apps/demo-be-* \
    --include="*.go" --include="*.py" --include="*.fs" --include="*.clj" --include="*.ts" \
    --include="*.cs" --include="*.kt" --include="*.java"
  # Note: *.sql files are intentionally excluded — migration files themselves contain CREATE TABLE
  # statements and would produce false positives. Inspect any non-migration SQL files (e.g., seed
  # scripts) manually if they exist.
  ```

- [ ] Verify 5-table schema for apps adding `refresh_tokens` (java-vertx, python-fastapi,
      clojure-pedestal, ts-effect, golang-gin Option A): use `psql \dt` or equivalent after
      running integration tests to confirm all 5 tables are present:
      `docker exec <db_container> psql -U postgres -c "\dt" | grep -E "refresh_tokens|revoked_tokens"`
- [ ] Confirm all per-phase Dockerfile verifications are complete and no Docker-affecting changes
      were made
- [ ] Confirm all per-phase docker-compose verifications are complete and no compose-affecting
      changes were made
- [ ] Confirm all per-phase GitHub Actions workflow verifications are complete and no CI-affecting
      changes were made

### Phase 7: CI Verification

Push all changes and verify all related GitHub Actions workflows pass. Trigger manually via
`gh workflow run` if needed (all workflows below support `workflow_dispatch`).

#### Main CI

- [ ] `main-ci.yml` — passes on push to `main`

#### Demo Backend E2E Workflows (all must pass)

- [x] `test-demo-be-java-springboot.yml` — Test - Demo BE (Java/Spring Boot) — PASS (regression)
- [ ] `test-demo-be-java-vertx.yml` — Test - Demo BE (Java/Vert.x) — awaiting re-run
- [x] `test-demo-be-python-fastapi.yml` — Test - Demo BE (Python/FastAPI) — PASS
- [ ] `test-demo-be-golang-gin.yml` — Test - Demo BE (Go/Gin) — awaiting re-run (fs.Sub fix)
- [x] `test-demo-be-kotlin-ktor.yml` — Test - Demo BE (Kotlin/Ktor) — PASS
- [ ] `test-demo-be-fsharp-giraffe.yml` — Test - Demo BE (F#/Giraffe) — awaiting re-run
- [x] `test-demo-be-csharp-aspnetcore.yml` — Test - Demo BE (C#/ASP.NET Core) — PASS
- [ ] `test-demo-be-clojure-pedestal.yml` — Test - Demo BE (Clojure/Pedestal) — awaiting re-run
- [x] `test-demo-be-ts-effect.yml` — Test - Demo BE (TypeScript/Effect) — PASS
- [x] `test-demo-be-rust-axum.yml` — Test - Demo BE (Rust/Axum) — PASS (regression)
- [x] `test-demo-be-elixir-phoenix.yml` — Test - Demo BE (Elixir/Phoenix) — PASS (regression)

#### Demo Fullstack E2E Workflows (must pass)

- [x] `test-demo-fs-ts-nextjs.yml` — Test - Demo FS (TypeScript/Next.js) — PASS

#### Pre-Existing Failures (document, do not block)

If a workflow was already failing before this plan's changes (e.g., `test-demo-be-ts-effect` has
been failing due to a Docker `npm ci` issue since 2026-03-24), document the pre-existing failure
and do not block the plan on it. Verify the failure is unrelated to migration changes by checking
the failure predates the plan's commits.

#### Trigger Commands

```bash
# Trigger all 12 demo workflows manually
gh workflow run test-demo-be-java-springboot.yml --ref main
gh workflow run test-demo-be-java-vertx.yml --ref main
gh workflow run test-demo-be-python-fastapi.yml --ref main
gh workflow run test-demo-be-golang-gin.yml --ref main
gh workflow run test-demo-be-kotlin-ktor.yml --ref main
gh workflow run test-demo-be-fsharp-giraffe.yml --ref main
gh workflow run test-demo-be-csharp-aspnetcore.yml --ref main
gh workflow run test-demo-be-clojure-pedestal.yml --ref main
gh workflow run test-demo-be-ts-effect.yml --ref main
gh workflow run test-demo-be-rust-axum.yml --ref main
gh workflow run test-demo-be-elixir-phoenix.yml --ref main
gh workflow run test-demo-fs-ts-nextjs.yml --ref main
```
