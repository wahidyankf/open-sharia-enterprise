# a-demo-be-ts-effect

TypeScript/Effect REST API backend — a functional twin of `a-demo-be-golang-gin`, `a-demo-be-python-fastapi`,
`a-demo-be-rust-axum`, and others, using Node.js, Vite, and Effect TS.

**tsex** = **T**ype**S**cript + **E**ffect**X** — matching the suffix pattern of other demo-be
variants.

## Tech Stack

| Concern          | Choice                                           |
| ---------------- | ------------------------------------------------ |
| Language         | TypeScript (strict)                              |
| Runtime          | Node.js (managed by Volta)                       |
| Build            | Vite (library mode for server build)             |
| Web framework    | `@effect/platform` Node.js HTTP server           |
| Database         | `@effect/sql` + SQLite (unit) / PostgreSQL (int) |
| JWT              | `jose` library                                   |
| Password hashing | `bcrypt`                                         |
| Unit BDD tests   | Cucumber.js + SQLite in-memory                   |
| Coverage         | Vitest v8 → LCOV → rhino-cli                     |
| Linting          | oxlint                                           |
| Port             | **8201**                                         |

## Database Migrations

Schema management uses `@effect/sql` migrators (`PgMigrator` for PostgreSQL,
`SqliteMigrator` for SQLite) with numbered TypeScript migration files.

### Migration files

Migration files live in `src/infrastructure/db/migrations/` and follow the naming
convention `NNN_<name>.ts` (e.g. `001_create_users.ts`). Each file exports a default
`Effect` that runs the DDL for that migration step:

```typescript
import { SqlClient } from "@effect/sql";
import { Effect } from "effect";

export default Effect.gen(function* () {
  const sql = yield* SqlClient.SqlClient;
  yield* sql`CREATE TABLE IF NOT EXISTS ...`;
});
```

The migrator tracks applied migrations in a `effect_sql_migrations` table and runs
only pending migrations on startup — idempotent and safe to call on every boot.

### Current migrations

| File                           | Creates                |
| ------------------------------ | ---------------------- |
| `001_create_users.ts`          | `users` table          |
| `002_create_refresh_tokens.ts` | `refresh_tokens` table |
| `003_create_revoked_tokens.ts` | `revoked_tokens` table |
| `004_create_expenses.ts`       | `expenses` table       |
| `005_create_attachments.ts`    | `attachments` table    |

### How migrations run

- **Production / dev server** (`src/main.ts`): migrations run automatically before
  the HTTP server layer starts, using `PgMigrator` (PostgreSQL) or `SqliteMigrator`
  (SQLite) depending on `DATABASE_URL`.
- **Unit BDD tests** (`tests/unit/bdd/hooks.ts`): `SqliteMigrator.fromRecord` loads
  migrations in-process against a temp SQLite file before each test run.
- **Integration tests** (`tests/integration/hooks.ts`): `PgMigrator.fromRecord` or
  `SqliteMigrator.fromRecord` (depending on `DATABASE_URL`) runs migrations against
  the real database before the test suite starts.

### Adding a migration

1. Create `src/infrastructure/db/migrations/NNN_<name>.ts` with a default Effect export.
2. Register it in `src/infrastructure/db/migrations/index.ts` under a matching key
   (`"NNNN_<name>"`).

## Test Architecture

This project uses a three-level testing strategy:

### Level 1: Unit tests (`tests/unit/`)

Pure unit tests covering isolated functions, domain logic, and algorithms using Vitest. These run
fast with no external dependencies.

### Level 2: Unit BDD (`tests/unit/bdd/`)

Cucumber.js BDD scenarios from `specs/apps/a-demo/be/gherkin/` run against an in-process server
backed by SQLite in-memory. All shared Gherkin scenarios execute with no real database required.
Deterministic, fast, and safe to cache.

Step definitions in `tests/unit/bdd/steps/` mirror the shared spec. The hooks start a local HTTP
server on port 8300 using SQLite and clear tables before each scenario for full isolation.

Both levels run as part of `test:quick` (the pre-push quality gate).

### Level 3: Integration tests (`tests/integration/`)

Full BDD scenarios run against a real PostgreSQL 17 database via Docker Compose. This level
validates database compatibility and production-equivalent behaviour. Results are never cached.

```bash
nx run a-demo-be-ts-effect:test:integration  # requires Docker
```

## Nx Targets

```bash
nx dev a-demo-be-ts-effect                      # Start dev server with tsx watch
nx build a-demo-be-ts-effect                    # Build with Vite (depends on codegen)
nx start a-demo-be-ts-effect                    # Run built dist/main.js
nx run a-demo-be-ts-effect:codegen              # Generate contract types from OpenAPI spec
nx run a-demo-be-ts-effect:test:quick           # Unit tests + coverage + BDD scenarios (pre-push gate)
nx run a-demo-be-ts-effect:test:unit            # Unit tests + BDD scenarios only
nx run a-demo-be-ts-effect:test:integration     # Cucumber.js BDD against PostgreSQL (Docker)
nx run a-demo-be-ts-effect:lint                 # oxlint
nx run a-demo-be-ts-effect:typecheck            # tsc --noEmit (depends on codegen)
```

## Environment Variables

| Variable         | Default                                 | Description              |
| ---------------- | --------------------------------------- | ------------------------ |
| `PORT`           | `8201`                                  | HTTP server port         |
| `DATABASE_URL`   | `sqlite::memory:`                       | PostgreSQL or SQLite URL |
| `APP_JWT_SECRET` | `dev-jwt-secret-at-least-32-chars-long` | JWT signing secret       |

## Local Development

### Direct (Node.js)

```bash
cd apps/a-demo-be-ts-effect
npm install
DATABASE_URL=sqlite::memory: npx tsx src/main.ts
```

### Docker Compose

```bash
cd infra/dev/a-demo-be-ts-effect
docker compose up --build
```

Then verify: `curl http://localhost:8201/health`

## API Endpoints

| Method | Path                                            | Auth  | Description           |
| ------ | ----------------------------------------------- | ----- | --------------------- |
| GET    | `/health`                                       | No    | Health check          |
| POST   | `/api/v1/auth/register`                         | No    | Register new user     |
| POST   | `/api/v1/auth/login`                            | No    | Login, return JWT     |
| POST   | `/api/v1/auth/refresh`                          | JWT   | Refresh access token  |
| POST   | `/api/v1/auth/logout`                           | No    | Logout (revoke token) |
| POST   | `/api/v1/auth/logout-all`                       | JWT   | Revoke all tokens     |
| GET    | `/api/v1/users/me`                              | JWT   | Current user profile  |
| PATCH  | `/api/v1/users/me`                              | JWT   | Update display name   |
| POST   | `/api/v1/users/me/password`                     | JWT   | Change password       |
| POST   | `/api/v1/users/me/deactivate`                   | JWT   | Self-deactivate       |
| GET    | `/api/v1/admin/users`                           | Admin | List/search users     |
| POST   | `/api/v1/admin/users/{id}/disable`              | Admin | Disable user          |
| POST   | `/api/v1/admin/users/{id}/enable`               | Admin | Enable user           |
| POST   | `/api/v1/admin/users/{id}/unlock`               | Admin | Unlock locked account |
| POST   | `/api/v1/admin/users/{id}/force-password-reset` | Admin | Generate reset token  |
| POST   | `/api/v1/expenses`                              | JWT   | Create expense        |
| GET    | `/api/v1/expenses`                              | JWT   | List expenses         |
| GET    | `/api/v1/expenses/{id}`                         | JWT   | Get expense           |
| PUT    | `/api/v1/expenses/{id}`                         | JWT   | Update expense        |
| DELETE | `/api/v1/expenses/{id}`                         | JWT   | Delete expense        |
| GET    | `/api/v1/expenses/summary`                      | JWT   | Summary by currency   |
| POST   | `/api/v1/expenses/{id}/attachments`             | JWT   | Upload attachment     |
| GET    | `/api/v1/expenses/{id}/attachments`             | JWT   | List attachments      |
| DELETE | `/api/v1/expenses/{id}/attachments/{aid}`       | JWT   | Delete attachment     |
| GET    | `/api/v1/reports/pl`                            | JWT   | P&L report            |
| GET    | `/api/v1/tokens/claims`                         | JWT   | Decode JWT claims     |
| GET    | `/.well-known/jwks.json`                        | No    | JWKS endpoint         |

## Architecture

The application uses Effect TS throughout:

- **Routes**: `HttpRouter` handlers returning `Effect` values
- **Services**: `Context.Tag` services with `Layer` composition
- **Database**: `@effect/sql` with SQLite (unit BDD / local dev) or PostgreSQL (integration)
- **Errors**: `Data.TaggedError` domain errors mapped to HTTP responses
- **Tests**: Three-level strategy — unit (Vitest), unit BDD (Cucumber+SQLite), integration (Cucumber+PostgreSQL+Docker)

## Related Documentation

- [Three-Level Testing Standard](../../governance/development/quality/three-level-testing-standard.md) — Unit, integration, and E2E testing boundaries
- [Code Coverage Reference](../../docs/reference/code-coverage.md) — Coverage tools, thresholds, and local vs Codecov
- [Project Dependency Graph](../../docs/reference/project-dependency-graph.md) — Nx dependency visualization
- [Backend Gherkin Specs](../../specs/apps/a-demo/be/gherkin/README.md) — Shared feature files (source of truth)
- [OpenAPI Contract](../../specs/apps/a-demo/contracts/README.md) — API contract and codegen
- [a-demo-be-e2e](../a-demo-be-e2e/README.md) — Shared E2E test suite
