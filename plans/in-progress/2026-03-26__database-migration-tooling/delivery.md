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

- [ ] Add `liquibase-core` dependency to `pom.xml`
- [ ] Create `src/main/resources/db/changelog/db.changelog-master.yaml` referencing change files
- [ ] Create SQL changelogs `001-create-users.sql` through `006-create-attachments.sql` in
      `src/main/resources/db/changelog/changes/` (match Spring Boot format)
- [ ] Replace `SchemaInitializer.java` inline DDL with Liquibase programmatic API:
      `CommandScope("update")` with `ClassLoaderResourceAccessor` and JDBC `DataSource`
- [ ] Update `README.md` with "Database Migrations" section
- [ ] Verify: `Dockerfile.integration` — no changes needed
- [ ] Verify: `docker-compose.integration.yml` — no changes needed
- [ ] Verify: `.github/workflows/test-demo-be-java-vertx.yml` — no changes needed
- [ ] Run `nx run demo-be-java-vertx:test:quick` — verify pass
- [ ] Run `nx run demo-be-java-vertx:test:integration` — verify schema matches previous approach
- [ ] Commit: `feat(demo-be-java-vertx): add Liquibase database migrations`

#### Phase 1b: demo-be-kotlin-ktor — Flyway

- [ ] Add `org.flywaydb:flyway-core` and `org.flywaydb:flyway-database-postgresql` to
      `build.gradle.kts`
- [ ] Create Flyway SQL files `V1__create_users.sql` through `V6__create_attachments.sql` in
      `src/main/resources/db/migration/`
- [ ] Wire `Flyway.configure().dataSource(ds).load().migrate()` in application startup, before
      Exposed table registration
- [ ] Remove `SchemaUtils.create()` calls (if any explicit calls exist)
- [ ] Update `README.md` with "Database Migrations" section
- [ ] Verify: `Dockerfile.integration` — no changes needed
- [ ] Verify: `docker-compose.integration.yml` — no changes needed
- [ ] Verify: `.github/workflows/test-demo-be-kotlin-ktor.yml` — no changes needed
- [ ] Run `nx run demo-be-kotlin-ktor:test:quick` — verify pass
- [ ] Run `nx run demo-be-kotlin-ktor:test:integration` — verify schema matches previous approach
- [ ] Commit: `feat(demo-be-kotlin-ktor): add Flyway database migrations`

### Phase 2: .NET Apps (F# / C#)

#### Phase 2a: demo-be-fsharp-giraffe — DbUp

- [ ] Add `DbUp-Core` and `DbUp-PostgreSQL` NuGet packages to `.fsproj`
- [ ] Create SQL migration files `001-create-users.sql` through `006-create-attachments.sql` in
      `db/migrations/`
- [ ] Configure `.fsproj` to embed migration files as `EmbeddedResource`
- [ ] Replace `Database.EnsureCreatedAsync()` in `Program.fs` with DbUp:
      `DeployChanges.To.PostgresqlDatabase(connStr).WithScriptsEmbeddedInAssembly(assembly).Build().PerformUpgrade()`
- [ ] Update `README.md` with "Database Migrations" section
- [ ] Verify: `Dockerfile.integration` — no changes needed
- [ ] Verify: `docker-compose.integration.yml` — no changes needed
- [ ] Verify: `.github/workflows/test-demo-be-fsharp-giraffe.yml` — no changes needed
- [ ] Run `nx run demo-be-fsharp-giraffe:test:quick` — verify pass
- [ ] Run `nx run demo-be-fsharp-giraffe:test:integration` — verify schema matches previous
      approach
- [ ] Commit: `feat(demo-be-fsharp-giraffe): add DbUp database migrations`

#### Phase 2b: demo-be-csharp-aspnetcore — EF Core Migrations

- [ ] Run `dotnet ef migrations add InitialCreate` to generate `Migrations/` directory
- [ ] Replace `Database.EnsureCreatedAsync()` with `Database.MigrateAsync()` in `Program.cs`
- [ ] Update `README.md` with "Database Migrations" section
- [ ] Verify: `Dockerfile.integration` — no changes needed
- [ ] Verify: `docker-compose.integration.yml` — no changes needed
- [ ] Verify: `.github/workflows/test-demo-be-csharp-aspnetcore.yml` — no changes needed
- [ ] Run `nx run demo-be-csharp-aspnetcore:test:quick` — verify pass
- [ ] Run `nx run demo-be-csharp-aspnetcore:test:integration` — verify schema matches previous
      approach
- [ ] Commit: `feat(demo-be-csharp-aspnetcore): upgrade to EF Core Migrations`

### Phase 3: Scripting Languages (Python / Clojure)

#### Phase 3a: demo-be-python-fastapi — Alembic

- [ ] Add `alembic` dependency to `pyproject.toml` or `requirements.txt`
- [ ] Create `alembic.ini` configuration file
- [ ] Create `alembic/env.py` with SQLAlchemy model import for autogenerate support
- [ ] Create migration scripts `001_create_users.py` through `006_create_attachments.py` in
      `alembic/versions/`
- [ ] Replace `Base.metadata.create_all()` in `main.py` with Alembic `upgrade("head")` on startup
- [ ] Update `README.md` with "Database Migrations" section
- [ ] Verify: `Dockerfile.integration` — may need `pip install alembic` if not in requirements
- [ ] Verify: `docker-compose.integration.yml` — no changes needed
- [ ] Verify: `.github/workflows/test-demo-be-python-fastapi.yml` — no changes needed
- [ ] Run `nx run demo-be-python-fastapi:test:quick` — verify pass
- [ ] Run `nx run demo-be-python-fastapi:test:integration` — verify schema matches previous
      approach
- [ ] Commit: `feat(demo-be-python-fastapi): add Alembic database migrations`

#### Phase 3b: demo-be-clojure-pedestal — Migratus

- [ ] Add `migratus` dependency to `deps.edn`
- [ ] Create SQL migration pairs `001-create-users.up.sql` / `.down.sql` through
      `006-create-attachments.up.sql` / `.down.sql` in `resources/migrations/`
- [ ] Replace `create-schema!` in `src/demo_be_cjpd/db/schema.clj` with Migratus
      `(migratus/migrate config)` call
- [ ] Update `README.md` with "Database Migrations" section
- [ ] Verify: `Dockerfile.integration` — no changes needed
- [ ] Verify: `docker-compose.integration.yml` — no changes needed
- [ ] Verify: `.github/workflows/test-demo-be-clojure-pedestal.yml` — no changes needed
- [ ] Run `nx run demo-be-clojure-pedestal:test:quick` — verify pass
- [ ] Run `nx run demo-be-clojure-pedestal:test:integration` — verify schema matches previous
      approach
- [ ] Commit: `feat(demo-be-clojure-pedestal): add Migratus database migrations`

### Phase 4: Go and TypeScript

#### Phase 4a: demo-be-golang-gin — goose

- [ ] Add `github.com/pressly/goose/v3` dependency to `go.mod`
- [ ] Create SQL migration files `001_create_users.sql` through `006_create_attachments.sql` in
      `db/migrations/` with `-- +goose Up` / `-- +goose Down` markers
- [ ] Replace GORM `AutoMigrate()` in `internal/store/store.go` (or equivalent) with goose
      embedded migrations (`goose.Up(db, migrationsDir)`)
- [ ] Update `README.md` with "Database Migrations" section
- [ ] Verify: `Dockerfile` — no changes needed (goose compiles into Go binary)
- [ ] Verify: `Dockerfile.integration` — no changes needed
- [ ] Verify: `docker-compose.integration.yml` — no changes needed
- [ ] Verify: `.github/workflows/test-demo-be-golang-gin.yml` — no changes needed
- [ ] Run `nx run demo-be-golang-gin:test:quick` — verify pass
- [ ] Run `nx run demo-be-golang-gin:test:integration` — verify schema matches previous approach
- [ ] Commit: `feat(demo-be-golang-gin): add goose database migrations`

#### Phase 4b: demo-be-ts-effect — @effect/sql Migrator

- [ ] Create Effect migration modules `001_create_users.ts` through `006_create_attachments.ts` in
      `src/infrastructure/db/migrations/`
- [ ] Extract DDL from `src/infrastructure/db/schema.ts` into migration files; keep type definitions
- [ ] Wire `PgMigrator.run` (or `SqliteMigrator.run`) into the Effect application layer startup
- [ ] Update `README.md` with "Database Migrations" section
- [ ] Verify: `Dockerfile.integration` — no changes needed
- [ ] Verify: `docker-compose.integration.yml` — no changes needed
- [ ] Verify: `.github/workflows/test-demo-be-ts-effect.yml` — no changes needed
- [ ] Run `nx run demo-be-ts-effect:test:quick` — verify pass
- [ ] Run `nx run demo-be-ts-effect:test:integration` — verify schema matches previous approach
- [ ] Commit: `feat(demo-be-ts-effect): add Effect SQL Migrator database migrations`

### Phase 5: Documentation, Governance, and Licensing

- [ ] Update `governance/development/pattern/database-audit-trail.md`:
  - Add a "Migration Tool by Language" table listing all 12 demo apps and their migration tools
  - Generalize the migration section to be language-agnostic
  - Keep Liquibase/JPA-specific guidance as a "Java / Spring Boot" subsection
  - Add brief examples for other ecosystems
- [ ] Update `governance/development/pattern/README.md`:
  - Change Database Audit Trail entry description to reflect multi-language migration support
- [ ] Update `governance/development/README.md`:
  - Change Database Audit Trail entry in Pattern Documentation section to reflect multi-language
    scope
- [ ] Create `docs/explanation/software-engineering/licensing/` directory with:
  - `README.md` — Index for licensing documentation
  - `licensing-decisions.md` — Document all non-OSI and copyleft licensing decisions:
    - **Liquibase FSL-1.1**: Why kept (non-compete does not apply to this project; we are an
      enterprise platform, not a competing migration tool). Lists affected apps
      (`demo-be-java-springboot`, `demo-be-java-vertx`). Notes that FSL converts to Apache 2.0
      after 2 years.
    - **Hibernate LGPL-2.1**: Dynamic linking via JPA SPI justification
    - **sharp-libvips LGPL-3.0**: Dynamic native addon justification
    - **Logback EPL-1.0/LGPL-2.1**: EPL-1.0 elected
    - **Audit schedule**: Quarterly or on major dependency upgrades
- [ ] Review `specs/apps/demo/c4/component-be.md`:
  - Add migration tool as a component in C4 diagram if not already present
- [ ] Commit: `docs(governance): generalize database audit trail pattern, add licensing decisions`

### Phase 6: Validation

- [ ] `nx affected -t test:quick` passes for all modified apps
- [ ] `nx affected -t test:integration` passes for all modified apps with docker-compose
- [ ] Each app's migration produces the same schema as the current programmatic approach
- [ ] Verify idempotency: running each app twice does not fail on already-applied migrations
- [ ] Verify all 8 app READMEs have a "Database Migrations" section
- [ ] Verify `database-audit-trail.md` includes the "Migration Tool by Language" table
- [ ] Verify `licensing-decisions.md` documents Liquibase FSL-1.1 decision with rationale
- [ ] Verify no remaining `AutoMigrate()`, `create_all()`, `EnsureCreated()`, `create-schema!`, or
      inline DDL `CREATE TABLE` calls in modified apps (except within migration files themselves)
- [ ] Verify all Dockerfiles, docker-compose files, and GitHub Actions workflows still work
