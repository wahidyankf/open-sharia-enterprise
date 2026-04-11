# a-demo-be-golang-gin

Go + Gin REST API backend — the default demo backend. Alternative implementations exist:
`a-demo-be-java-springboot` (Java/Spring Boot), `a-demo-be-elixir-phoenix` (Elixir/Phoenix), and other demo-be backends. Uses Go and the Gin framework.

## Tech Stack

| Concern   | Choice                             |
| --------- | ---------------------------------- |
| Language  | Go 1.24                            |
| Framework | Gin                                |
| Database  | GORM + PostgreSQL (production)     |
| JWT       | golang-jwt                         |
| Passwords | bcrypt                             |
| BDD Tests | Godog (Cucumber for Go) + httptest |
| Coverage  | go test -coverprofile + rhino-cli  |
| Linting   | golangci-lint                      |
| Port      | 8201                               |

## Local Development

### Prerequisites

- Go 1.24+
- PostgreSQL (or use Docker Compose)

### Environment Variables

| Variable       | Default                                                                                      | Description        |
| -------------- | -------------------------------------------------------------------------------------------- | ------------------ |
| `PORT`         | `8201`                                                                                       | HTTP port          |
| `DATABASE_URL` | `postgresql://a_demo_be_golang_gin:a_demo_be_golang_gin@localhost:5432/a_demo_be_golang_gin` | PostgreSQL URL     |
| `JWT_SECRET`   | (dev default)                                                                                | JWT signing secret |

### Run locally

```bash
# Start PostgreSQL
docker compose -f ../../infra/dev/a-demo-be-golang-gin/docker-compose.yml up -d a-demo-be-golang-gin-db

# Run dev server
go run cmd/server/main.go

# Health check
curl http://localhost:8201/health
```

## Nx Targets

```bash
nx build a-demo-be-golang-gin                   # Compile binary (depends on codegen)
nx dev a-demo-be-golang-gin                     # Start development server
nx run a-demo-be-golang-gin:typecheck           # go vet ./... (depends on codegen)
nx run a-demo-be-golang-gin:test:quick          # Unit (BDD) tests + coverage gate (>=90%)
nx run a-demo-be-golang-gin:test:unit           # BDD unit tests only (verbose)
nx run a-demo-be-golang-gin:test:integration    # PostgreSQL integration tests via Docker Compose
nx lint a-demo-be-golang-gin                    # Run golangci-lint
```

`codegen` generates Go types from the OpenAPI contract spec into `generated-contracts/` and is a
dependency of both `typecheck` and `build`.

## Database Migrations

Schema migrations use [goose v3](https://github.com/pressly/goose) with SQL migration files
embedded at compile time via `embed.FS`. GORM is retained for all query operations — only schema
creation has moved to goose.

### Migration files

Migration files live in `db/migrations/` and follow the `NNN_description.sql` naming convention:

| File                            | Table            | Description                            |
| ------------------------------- | ---------------- | -------------------------------------- |
| `001_create_users.sql`          | `users`          | User accounts with 6 audit columns     |
| `002_create_refresh_tokens.sql` | `refresh_tokens` | Refresh token storage                  |
| `003_create_revoked_tokens.sql` | `revoked_tokens` | Revoked access token JTIs              |
| `004_create_expenses.sql`       | `expenses`       | Financial entries (income and expense) |
| `005_create_attachments.sql`    | `attachments`    | Files attached to expense entries      |

### How migrations run

`GORMStore.Migrate()` is called at startup. It creates a goose provider from the embedded
`db/migrations/*.sql` files and applies any pending `Up` migrations. The goose version table
(`goose_db_version`) tracks applied migrations.

The dialect is auto-detected from the GORM dialector name (`postgres` or `sqlite`), so the
same code path works in production (PostgreSQL) and in legacy SQLite-based local development.

### Adding a new migration

```bash
# Create a new sequential migration file
touch apps/a-demo-be-golang-gin/db/migrations/006_description.sql
```

Each file must contain `-- +goose Up` and `-- +goose Down` sections:

```sql
-- +goose Up
ALTER TABLE users ADD COLUMN phone TEXT;

-- +goose Down
ALTER TABLE users DROP COLUMN phone;
```

## API Endpoints

See [plan README](../../plans/done/2026-03-11__demo-be-golang-gin/README.md) for the full API surface.

## Test Architecture

This project follows the three-level testing standard where the same Gherkin feature files
(`specs/apps/a-demo/be/gherkin/`) drive all three levels. Only the step implementations differ.

### Level 1: Unit tests (`test:quick`, `test:unit`)

- **Package**: `internal/bdd/`
- **Build tag**: none (runs with plain `go test ./...`)
- **Store**: `store.MemoryStore` (in-memory Go maps, no external deps)
- **HTTP**: `net/http/httptest` (in-process)
- **Test function**: `TestUnit`
- **Cacheable**: yes
- **Coverage**: >=90% line coverage enforced via `rhino-cli test-coverage validate`

The `internal/bdd/` package mirrors the step definitions in `internal/integration/` but without
the `//go:build integration` tag, so every scenario runs as part of the standard `go test ./...`
and contributes to coverage measurement.

Infrastructure packages excluded from coverage measurement:

- `gorm_store` - PostgreSQL driver code, no logic to unit-test
- `internal/server` - server wiring, tested at e2e level
- `cmd/server` - entry point, single-line `main()`

### Level 2: Integration tests (`test:integration`)

- **Package**: `internal/integration_pg/`
- **Build tag**: `//go:build integration_pg`
- **Store**: `store.GORMStore` (real PostgreSQL via GORM)
- **HTTP**: `net/http/httptest` (in-process, same router)
- **Test function**: `TestIntegrationPG`
- **Cacheable**: no (requires Docker, external PostgreSQL service)
- **Runner**: `docker compose -f docker-compose.integration.yml up --abort-on-container-exit --build`

Each scenario is isolated by truncating all tables (`TRUNCATE TABLE ... CASCADE`) in the `Before`
hook before each scenario runs. PostgreSQL is started as a `postgres:17-alpine` container with
`tmpfs` storage so data never persists between runs.

The `Dockerfile.integration` builds the Go project inside a `golang:1.24-alpine` container and
runs `go test -tags=integration_pg`. The `docker-compose.integration.yml` mounts
`../../specs` at `/specs` so the Godog path `/specs/apps/a-demo/be/gherkin` resolves correctly.

### Level 3: E2E tests

E2E tests for all demo-be backends live in the shared `a-demo-be-e2e` Playwright project.

### Legacy integration package

The `internal/integration/` package (`//go:build integration`, `TestIntegration`) predates the
three-level architecture. It runs the same Godog scenarios against `MemoryStore` but requires
`-tags=integration` to execute. It is kept for reference but is superseded by `internal/bdd/`
for coverage purposes.

## Related Documentation

- [Three-Level Testing Standard](../../governance/development/quality/three-level-testing-standard.md) — Unit, integration, and E2E testing boundaries
- [Code Coverage Reference](../../docs/reference/code-coverage.md) — Coverage tools, thresholds, and local vs Codecov
- [Project Dependency Graph](../../docs/reference/project-dependency-graph.md) — Nx dependency visualization
- [Backend Gherkin Specs](../../specs/apps/a-demo/be/gherkin/README.md) — Shared feature files (source of truth)
- [OpenAPI Contract](../../specs/apps/a-demo/contracts/README.md) — API contract and codegen
