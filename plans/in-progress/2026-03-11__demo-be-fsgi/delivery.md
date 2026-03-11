# Delivery Checklist: demo-be-fsgi

Execute phases in order. Each phase produces a working, committable state.

---

## Phase 0: Prerequisites

- [ ] Verify .NET 9 SDK available locally and in CI (`actions/setup-dotnet@v4`)
- [ ] Verify `rhino-cli test-coverage validate` supports LCOV (it does — already used by
      `organiclever-web` and `demo-be-exph`)
- [ ] Confirm TickSpec supports current Gherkin syntax (Given/When/Then with regex and
      doc_string parameters)
- [ ] Verify `demo-be-e2e` Playwright config reads `BASE_URL` from env (it does)
- [ ] Install Fantomas and FSharpLint tools locally for development

---

## Phase 1: Project Scaffold

**Commit**: `feat(demo-be-fsgi): scaffold F#/Giraffe project`

- [ ] Create `apps/demo-be-fsgi/` directory structure per tech-docs.md
- [ ] Create `global.json` pinning .NET SDK 9.0.x
- [ ] Create `src/DemoBeFsgi/DemoBeFsgi.fsproj` with all NuGet dependencies
- [ ] Create `tests/DemoBeFsgi.Tests/DemoBeFsgi.Tests.fsproj` with test dependencies
- [ ] Create minimal `Program.fs` with Giraffe health endpoint
- [ ] Create `project.json` with all Nx targets from tech-docs.md
- [ ] Add `.editorconfig` and `.fantomas` configuration
- [ ] Add `README.md` covering local dev, Docker, env vars, API endpoints, Nx targets
- [ ] Add `.fsproj` build target to copy Gherkin specs to output directory
- [ ] Verify `dotnet build` compiles with zero warnings
- [ ] Verify `dotnet fantomas --check src/ tests/` passes
- [ ] Commit

---

## Phase 2: Domain Types and Database

**Commit**: `feat(demo-be-fsgi): add domain types and EF Core database`

- [ ] Create `Domain/Types.fs` — shared discriminated unions (`Currency`, `Role`,
      `UserStatus`, `DomainError`)
- [ ] Create `Domain/User.fs` — User record with validation functions
- [ ] Create `Domain/Expense.fs` — Expense record with currency precision validation
- [ ] Create `Domain/Attachment.fs` — Attachment record
- [ ] Create `Infrastructure/AppDbContext.fs` — EF Core DbContext
- [ ] Create `Infrastructure/Repositories.fs` — repository interface + implementations
- [ ] Create `Infrastructure/PasswordHasher.fs` — BCrypt wrapper
- [ ] Create EF Core migration for users, expenses, attachments, revoked_tokens tables
- [ ] Write unit tests for domain validation (username, password, currency, amount)
- [ ] Verify `dotnet test --filter Category=Unit` passes
- [ ] Commit

---

## Phase 3: Health Endpoint

**Commit**: `feat(demo-be-fsgi): add /health endpoint`

- [ ] Create `Handlers/HealthHandler.fs` returning `{"status": "UP"}`
- [ ] Add route `GET /health` (public, no auth)
- [ ] Create `TestFixture.fs` with WebApplicationFactory + SQLite in-memory setup
- [ ] Create `State.fs` — step state record for TickSpec
- [ ] Write TickSpec integration test consuming `health-check.feature` (2 scenarios)
- [ ] Create `Integration/Steps/CommonSteps.fs` with shared step definitions
- [ ] Verify `dotnet test --filter Category=Integration` passes — 2 scenarios
- [ ] Commit

---

## Phase 4: Auth — Register and Login

**Commit**: `feat(demo-be-fsgi): add register and login endpoints`

- [ ] Create `Auth/JwtService.fs` — JWT generation (access + refresh tokens)
- [ ] Create `Auth/JwtMiddleware.fs` — authentication middleware using Giraffe
- [ ] Create `Handlers/AuthHandler.fs`:
  - `POST /api/v1/auth/register` → 201 `{"id":...,"username":...}`
  - `POST /api/v1/auth/login` → 200 `{"access_token":...,"refresh_token":...}`
- [ ] Add routes: public scope for `/api/v1/auth/*`
- [ ] Write TickSpec integration tests for `registration.feature` (6) and
      `password-login.feature` (5 scenarios)
- [ ] Verify 13 integration scenarios pass
- [ ] Commit

---

## Phase 5: Token Lifecycle and Management

**Commit**: `feat(demo-be-fsgi): add token lifecycle and management endpoints`

- [ ] Add `POST /api/v1/auth/refresh` — refresh access token
- [ ] Add `POST /api/v1/auth/logout` — revoke current token
- [ ] Add `POST /api/v1/auth/logout-all` — revoke all tokens for user
- [ ] Create `Handlers/TokenHandler.fs`:
  - `GET /api/v1/tokens/claims` — decode JWT claims
  - `GET /.well-known/jwks.json` — JWKS endpoint
- [ ] Implement token revocation table in database
- [ ] Write TickSpec integration tests for `token-lifecycle.feature` (7) and
      `tokens.feature` (6 scenarios)
- [ ] Verify 26 integration scenarios pass
- [ ] Commit

---

## Phase 6: User Account and Security

**Commit**: `feat(demo-be-fsgi): add user account and security endpoints`

- [ ] Create `Handlers/UserHandler.fs`:
  - `GET /api/v1/users/me` — current user profile
  - `PUT /api/v1/users/me/password` — change password
  - `DELETE /api/v1/users/me` — self-deactivate
- [ ] Implement account lockout (configurable failed attempts threshold)
- [ ] Write TickSpec integration tests for `user-account.feature` (6) and
      `security.feature` (5 scenarios)
- [ ] Verify 37 integration scenarios pass
- [ ] Commit

---

## Phase 7: Admin

**Commit**: `feat(demo-be-fsgi): add admin endpoints`

- [ ] Create `Auth/AdminMiddleware.fs` — admin role verification
- [ ] Create `Handlers/AdminHandler.fs`:
  - `GET /api/v1/admin/users` — list/search with pagination
  - `PUT /api/v1/admin/users/{id}/status` — enable/disable
  - `POST /api/v1/admin/users/{id}/reset-password-token` — generate reset token
- [ ] Write TickSpec integration tests for `admin.feature` (6 scenarios)
- [ ] Verify 43 integration scenarios pass
- [ ] Commit

---

## Phase 8: Expenses — CRUD and Currency

**Commit**: `feat(demo-be-fsgi): add expense CRUD and currency handling`

- [ ] Create `Handlers/ExpenseHandler.fs`:
  - `POST /api/v1/expenses` — create
  - `GET /api/v1/expenses` — list own
  - `GET /api/v1/expenses/{id}` — get by ID
  - `PUT /api/v1/expenses/{id}` — update
  - `DELETE /api/v1/expenses/{id}` — delete
- [ ] Implement currency precision enforcement (USD: 2dp, IDR: 0dp)
- [ ] Implement ownership checks (403 for other users' expenses)
- [ ] Write TickSpec integration tests for `expense-management.feature` (7) and
      `currency-handling.feature` (6 scenarios)
- [ ] Verify 56 integration scenarios pass
- [ ] Commit

---

## Phase 9: Expenses — Units, Reporting, Attachments

**Commit**: `feat(demo-be-fsgi): add unit handling, reporting, and attachments`

- [ ] Implement unit-of-measure field on expenses
- [ ] Create `Handlers/ExpenseHandler.report` — P&L per currency with date range filter
- [ ] Create `Handlers/AttachmentHandler.fs`:
  - `POST /api/v1/expenses/{id}/attachments` — upload file
  - `GET /api/v1/expenses/{id}/attachments` — list
  - `DELETE /api/v1/expenses/{id}/attachments/{aid}` — delete
- [ ] Implement file size limit (10MB) with proper error response
- [ ] Write TickSpec integration tests for `unit-handling.feature` (4),
      `reporting.feature` (6), and `attachments.feature` (10 scenarios)
- [ ] Verify all 76 integration scenarios pass
- [ ] Commit

---

## Phase 10: Coverage and Quality Gate

**Commit**: `fix(demo-be-fsgi): achieve 90% coverage and pass quality gates`

- [ ] Run full test suite with coverage: `dotnet test --collect:"XPlat Code Coverage"`
- [ ] Validate: `rhino-cli test-coverage validate <lcov-path> 90` passes
- [ ] Verify `dotnet fantomas --check src/ tests/` passes
- [ ] Verify `dotnet fsharplint lint src/DemoBeFsgi/DemoBeFsgi.fsproj` passes
- [ ] Verify `dotnet build /p:TreatWarningsAsErrors=true` passes
- [ ] Write additional unit tests if coverage below 90%
- [ ] Commit

---

## Phase 11: Infra — Docker Compose

**Commit**: `feat(infra): add demo-be-fsgi docker-compose dev environment`

- [ ] Create `infra/dev/demo-be-fsgi/Dockerfile.be.dev` (.NET 9 SDK Alpine)
- [ ] Create `infra/dev/demo-be-fsgi/docker-compose.yml` with PostgreSQL + app
- [ ] Create `infra/dev/demo-be-fsgi/docker-compose.e2e.yml` (E2E overrides)
- [ ] Create `infra/dev/demo-be-fsgi/README.md` with startup instructions
- [ ] Manual test: `docker compose up --build` → health check passes

---

## Phase 12: GitHub Actions — E2E Workflow

**Commit**: `ci: add e2e-demo-be-fsgi GitHub Actions workflow`

- [ ] Create `.github/workflows/e2e-demo-be-fsgi.yml`:
  - Trigger: schedule (same crons as jasb/exph) + `workflow_dispatch`
  - Job: checkout → docker compose up → wait-healthy → Volta → npm ci →
    `nx run demo-be-e2e:test:e2e` with `BASE_URL=http://localhost:8201` →
    upload artifact → docker down
- [ ] Trigger `workflow_dispatch` manually; verify green

---

## Phase 13: CI — main-ci.yml Update

**Commit**: `ci: add .NET SDK setup and demo-be-fsgi coverage upload to main-ci`

- [ ] Add `actions/setup-dotnet@v4` step to `main-ci.yml` (.NET 9)
- [ ] Add coverage upload step for `apps/demo-be-fsgi/coverage/**/coverage.info`
      with flag `demo-be-fsgi`
- [ ] Push to `main`; verify `Main CI` workflow passes

---

## Phase 14: Documentation Updates

**Commit**: `docs: add demo-be-fsgi to project documentation`

- [ ] Update `CLAUDE.md`:
  - Add `demo-be-fsgi` to Current Apps list with description
  - Add F# coverage info to coverage section
  - Add `demo-be-fsgi` to `test:integration` caching note
- [ ] Update `README.md`:
  - Add demo-be-fsgi badge in demo apps section
  - Add description line
- [ ] Update `specs/apps/demo-be/README.md`:
  - Add F#/Giraffe row to Implementations table
- [ ] Update `apps/demo-be-e2e/project.json`:
  - Add `demo-be-fsgi` to `implicitDependencies`
- [ ] Update `plans/in-progress/README.md`:
  - Remove this plan from active list (move to done)

---

## Phase 15: Final Validation

- [ ] `nx run demo-be-fsgi:test:quick` passes (76 scenarios, ≥90% coverage, lint clean)
- [ ] `nx run demo-be-fsgi:test:unit` passes
- [ ] `nx run demo-be-fsgi:test:integration` passes — all 76 scenarios
- [ ] `nx run demo-be-fsgi:lint` passes
- [ ] `nx run demo-be-fsgi:typecheck` passes
- [ ] `nx run demo-be-fsgi:build` produces working artifact
- [ ] Docker Compose stack starts and health check passes
- [ ] `e2e-demo-be-fsgi.yml` workflow green
- [ ] `main-ci.yml` workflow green
- [ ] All documentation updated
- [ ] Move plan folder to `plans/done/`
