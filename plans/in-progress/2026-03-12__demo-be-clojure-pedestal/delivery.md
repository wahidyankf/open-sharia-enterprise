# Delivery Checklist: demo-be-clojure-pedestal

## Phase 1: Project Scaffold

- [ ] Create `apps/demo-be-clojure-pedestal/` directory
- [ ] Create `deps.edn` with all dependencies and aliases
- [ ] Create `build.clj` for uberjar build
- [ ] Create `project.json` with Nx targets
- [ ] Create `tests.edn` for kaocha configuration
- [ ] Create `.clj-kondo/config.edn`
- [ ] Create `resources/logback.xml`
- [ ] Create `README.md`

## Phase 2: Core Implementation

- [ ] `config.clj` — environment variable loading
- [ ] `db/core.clj` — datasource creation (PostgreSQL / SQLite)
- [ ] `db/schema.clj` — DDL table creation
- [ ] `auth/password.clj` — bcrypt hash/verify
- [ ] `auth/jwt.clj` — JWT sign/verify/JWKS
- [ ] `domain/user.clj` — user status, password policy
- [ ] `domain/expense.clj` — expense types, currency, units
- [ ] `domain/attachment.clj` — file type/size validation

## Phase 3: Database Repositories

- [ ] `db/user_repo.clj` — user CRUD
- [ ] `db/token_repo.clj` — token revocation
- [ ] `db/expense_repo.clj` — expense CRUD + summary
- [ ] `db/attachment_repo.clj` — attachment CRUD

## Phase 4: Interceptors

- [ ] `interceptors/json.clj` — JSON body parsing & response
- [ ] `interceptors/auth.clj` — JWT authentication
- [ ] `interceptors/admin.clj` — admin role check
- [ ] `interceptors/error.clj` — error handling
- [ ] `interceptors/multipart.clj` — multipart file upload

## Phase 5: Handlers & Routes

- [ ] `handlers/health.clj` — health check
- [ ] `handlers/auth.clj` — register, login, refresh, logout, logout-all
- [ ] `handlers/user.clj` — profile, password change, deactivate
- [ ] `handlers/admin.clj` — user management
- [ ] `handlers/expense.clj` — expense CRUD + summary
- [ ] `handlers/attachment.clj` — file attachments
- [ ] `handlers/report.clj` — P&L report
- [ ] `handlers/token.clj` — JWT claims
- [ ] `handlers/jwks.clj` — JWKS endpoint
- [ ] `routes.clj` — route table
- [ ] `server.clj` — Pedestal server setup
- [ ] `main.clj` — entry point

## Phase 6: Integration Tests

- [ ] Set up feature file access (symlink or copy)
- [ ] `common_steps.clj` — shared steps
- [ ] `health_steps.clj` — 2 scenarios
- [ ] `auth_steps.clj` — 12 scenarios (login + token lifecycle)
- [ ] `user_steps.clj` — 12 scenarios (registration + account)
- [ ] `admin_steps.clj` — 6 scenarios
- [ ] `expense_steps.clj` — 17 scenarios (CRUD + currency + units)
- [ ] `report_steps.clj` — 6 scenarios
- [ ] `attachment_steps.clj` — 10 scenarios
- [ ] `token_steps.clj` — 11 scenarios (tokens + security)
- [ ] All 76 scenarios passing

## Phase 7: Unit Tests & Coverage

- [ ] Unit tests for domain logic
- [ ] Unit tests for JWT
- [ ] Unit tests for password hashing
- [ ] Unit tests for repositories
- [ ] cloverage LCOV output ≥90%
- [ ] `rhino-cli test-coverage validate` passes

## Phase 8: Infrastructure

- [ ] `infra/dev/demo-be-clojure-pedestal/docker-compose.yml`
- [ ] `infra/dev/demo-be-clojure-pedestal/docker-compose.e2e.yml`
- [ ] `infra/dev/demo-be-clojure-pedestal/Dockerfile.be.dev`

## Phase 9: CI/CD

- [ ] `.github/workflows/e2e-demo-be-clojure-pedestal.yml`
- [ ] Update `.github/workflows/main-ci.yml` — Clojure setup + coverage upload
- [ ] Update `codecov.yml` — add flag

## Phase 10: Cross-references

- [ ] Update `CLAUDE.md` — add to Current Apps, add coverage info
- [ ] Update `README.md` — add badge + coverage row
- [ ] Update `specs/apps/demo-be/README.md` — add implementation row
- [ ] Update `apps/demo-be-e2e/project.json` — add to implicitDependencies

## Phase 11: Verification

- [ ] `nx run demo-be-clojure-pedestal:test:quick` passes locally
- [ ] `nx run demo-be-clojure-pedestal:build` produces uberjar
- [ ] Main CI passes on push
- [ ] E2E workflow passes (manual trigger)
