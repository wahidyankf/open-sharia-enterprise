# Plan: a-demo-be-python-fastapi (In Progress)

Python / FastAPI reimplementation of the demo backend REST API — a functional twin of
`apps/a-demo-be-java-springboot` (Java/Spring Boot), `apps/a-demo-be-elixir-phoenix` (Elixir/Phoenix), and
`apps/a-demo-be-fsharp-giraffe` (F#/Giraffe) using Python 3.13 and FastAPI.

**Status**: In Progress

## Goals

- Provide a functionally equivalent backend to `a-demo-be-java-springboot`, `a-demo-be-elixir-phoenix`, and
  `a-demo-be-fsharp-giraffe` using the Python ecosystem
- Consume the shared `specs/apps/a-demo/be/gherkin/` Gherkin feature files (76 scenarios across
  13 feature files) for BDD integration tests
- Integrate into the Nx monorepo with the same target surface (`build`, `dev`, `start`,
  `test:quick`, `test:unit`, `test:integration`, `lint`, `typecheck`)
- Reuse the existing `a-demo-be-e2e` Playwright BDD test suite for E2E validation
- Add a dedicated GitHub Actions workflow and Docker Compose infra

## Naming

`pyfa` = **PY**thon + **FA**stAPI — matching the suffix pattern of `-jasb` (Java Spring Boot),
`-exph` (Elixir Phoenix), and `-fsgi` (F# Giraffe).

## API Surface (identical to all demo-be implementations)

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

## Tech Stack

| Concern          | Choice                                                                    |
| ---------------- | ------------------------------------------------------------------------- |
| Language         | Python 3.13+                                                              |
| Web framework    | FastAPI (with Uvicorn)                                                    |
| Database ORM     | SQLAlchemy 2.0+ with PostgreSQL (prod) / SQLite in-memory (tests)         |
| JWT              | PyJWT                                                                     |
| Password hashing | passlib with bcrypt                                                       |
| BDD (int. tests) | pytest-bdd (pytest plugin for Gherkin)                                    |
| Linting          | Ruff (replaces flake8, isort, pycodestyle)                                |
| Type checking    | Pyright (strict mode)                                                     |
| Formatting       | Ruff format (replaces Black)                                              |
| Coverage         | coverage.py → LCOV → `rhino-cli test-coverage validate`                   |
| Port             | **8201** (same as all demo-be variants — mutually exclusive alternatives) |
| Package manager  | uv (fast pip replacement)                                                 |
| Virtual env      | uv venv                                                                   |

## Gherkin Scenario Count

| Feature file               | Scenarios |
| -------------------------- | --------- |
| health-check.feature       | 2         |
| password-login.feature     | 5         |
| token-lifecycle.feature    | 7         |
| registration.feature       | 6         |
| user-account.feature       | 6         |
| security.feature           | 5         |
| tokens.feature             | 6         |
| admin.feature              | 6         |
| expense-management.feature | 7         |
| currency-handling.feature  | 6         |
| unit-handling.feature      | 4         |
| reporting.feature          | 6         |
| attachments.feature        | 10        |
| **Total**                  | **76**    |

## Related Files

- `apps/a-demo-be-python-fastapi/` — application source
- `infra/dev/a-demo-be-python-fastapi/` — Docker Compose dev infra
- `.github/workflows/e2e-a-demo-be-python-fastapi.yml` — E2E workflow
- `.github/workflows/main-ci.yml` — Python/uv setup + coverage upload
- `specs/apps/a-demo/be/` — shared Gherkin specs (consumed, not modified)
- `apps/a-demo-be-e2e/` — reused Playwright E2E suite (consumed, not modified)

## Files to Update

| File                                         | Change                                                                  |
| -------------------------------------------- | ----------------------------------------------------------------------- |
| `CLAUDE.md`                                  | Add a-demo-be-python-fastapi to Current Apps list, add Python coverage  |
| `README.md`                                  | Add a-demo-be-python-fastapi badge and description in demo apps section |
| `specs/apps/a-demo/be/README.md`             | Add Python/FastAPI row to Implementations table                         |
| `apps/a-demo-be-e2e/project.json`            | Add `a-demo-be-python-fastapi` to `implicitDependencies`                |
| `.github/workflows/main-ci.yml`              | Add Python/uv setup + coverage upload step                              |
| `package.json`                               | Add ruff format to lint-staged for Python files                         |
| `governance/development/infra/nx-targets.md` | Add Python tags, update test:quick composition table                    |

## Git Workflow

Trunk Based Development — all work on `main` branch. No feature branch needed.

## See Also

- [requirements.md](./requirements.md) — acceptance criteria
- [tech-docs.md](./tech-docs.md) — technical design
- [delivery.md](./delivery.md) — delivery checklist
