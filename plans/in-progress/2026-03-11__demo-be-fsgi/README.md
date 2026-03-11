# Plan: demo-be-fsgi

F# / Giraffe reimplementation of the demo backend REST API — a functional twin of
`apps/demo-be-jasb` (Java/Spring Boot) and `apps/demo-be-exph` (Elixir/Phoenix) using F#,
Giraffe, and ASP.NET Core.

## Goals

- Provide a functionally equivalent backend to `demo-be-jasb` and `demo-be-exph` using the
  F# ecosystem
- Consume the shared `specs/apps/demo-be/gherkin/` Gherkin feature files (76 scenarios across
  13 feature files) for BDD integration tests
- Integrate into the Nx monorepo with the same target surface (`build`, `dev`, `start`,
  `test:quick`, `test:unit`, `test:integration`, `lint`, `typecheck`)
- Reuse the existing `demo-be-e2e` Playwright BDD test suite for E2E validation
- Add a dedicated GitHub Actions workflow and Docker Compose infra

## Naming

`fsgi` = **F#** (**F**-**S**harp) + **Gi**raffe — matching the suffix pattern of `-jasb`
(Java Spring Boot) and `-exph` (Elixir Phoenix).

## API Surface (identical to demo-be-jasb and demo-be-exph)

| Method | Path                                            | Auth  | Description           |
| ------ | ----------------------------------------------- | ----- | --------------------- |
| GET    | `/health`                                       | No    | Health check          |
| POST   | `/api/v1/auth/register`                         | No    | Register new user     |
| POST   | `/api/v1/auth/login`                            | No    | Login, return JWT     |
| POST   | `/api/v1/auth/refresh`                          | JWT   | Refresh access token  |
| POST   | `/api/v1/auth/logout`                           | JWT   | Logout (revoke token) |
| POST   | `/api/v1/auth/logout-all`                       | JWT   | Revoke all tokens     |
| GET    | `/api/v1/users/me`                              | JWT   | Current user profile  |
| PUT    | `/api/v1/users/me/password`                     | JWT   | Change password       |
| DELETE | `/api/v1/users/me`                              | JWT   | Self-deactivate       |
| GET    | `/api/v1/admin/users`                           | Admin | List/search users     |
| PUT    | `/api/v1/admin/users/{id}/status`               | Admin | Enable/disable user   |
| POST   | `/api/v1/admin/users/{id}/reset-password-token` | Admin | Generate reset token  |
| POST   | `/api/v1/expenses`                              | JWT   | Create expense        |
| GET    | `/api/v1/expenses`                              | JWT   | List expenses         |
| GET    | `/api/v1/expenses/{id}`                         | JWT   | Get expense           |
| PUT    | `/api/v1/expenses/{id}`                         | JWT   | Update expense        |
| DELETE | `/api/v1/expenses/{id}`                         | JWT   | Delete expense        |
| GET    | `/api/v1/expenses/report`                       | JWT   | P&L report            |
| POST   | `/api/v1/expenses/{id}/attachments`             | JWT   | Upload attachment     |
| GET    | `/api/v1/expenses/{id}/attachments`             | JWT   | List attachments      |
| DELETE | `/api/v1/expenses/{id}/attachments/{aid}`       | JWT   | Delete attachment     |
| GET    | `/api/v1/tokens/claims`                         | JWT   | Decode JWT claims     |
| GET    | `/.well-known/jwks.json`                        | No    | JWKS endpoint         |

## Tech Stack

| Concern          | Choice                                                                  |
| ---------------- | ----------------------------------------------------------------------- |
| Language         | F# 9 (.NET 9)                                                           |
| Web framework    | Giraffe (functional ASP.NET Core)                                       |
| Database ORM     | Entity Framework Core + Npgsql (PostgreSQL)                             |
| JWT              | System.IdentityModel.Tokens.Jwt + Microsoft.IdentityModel.JsonWebTokens |
| Password hashing | BCrypt.Net-Next                                                         |
| BDD (int. tests) | TickSpec (F#-native Gherkin runner) + xUnit                             |
| Linting          | FSharpLint                                                              |
| Formatting       | Fantomas (MANDATORY)                                                    |
| Type checking    | F# compiler with `<TreatWarningsAsErrors>true</TreatWarningsAsErrors>`  |
| Coverage         | Coverlet → LCOV → `rhino-cli test-coverage validate`                    |
| Port             | **8201** (same as demo-be-jasb/exph — mutually exclusive alternatives)  |

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

- `apps/demo-be-fsgi/` — application source (to be created)
- `infra/dev/demo-be-fsgi/` — Docker Compose dev infra (to be created)
- `.github/workflows/e2e-demo-be-fsgi.yml` — E2E workflow (to be created)
- `.github/workflows/main-ci.yml` — add .NET SDK setup + coverage upload (to be updated)
- `specs/apps/demo-be/` — shared Gherkin specs (consumed, not modified)
- `apps/demo-be-e2e/` — reused Playwright E2E suite (consumed, not modified)

## Files to Update

| File                            | Change                                                      |
| ------------------------------- | ----------------------------------------------------------- |
| `CLAUDE.md`                     | Add demo-be-fsgi to Current Apps list, add F# coverage info |
| `README.md`                     | Add demo-be-fsgi badge and description in demo apps section |
| `specs/apps/demo-be/README.md`  | Add F#/Giraffe row to Implementations table                 |
| `apps/demo-be-e2e/project.json` | Add `demo-be-fsgi` to `implicitDependencies`                |
| `.github/workflows/main-ci.yml` | Add .NET SDK setup + coverage upload step                   |
| `plans/in-progress/README.md`   | Add this plan to active plans list                          |

## See Also

- [requirements.md](./requirements.md) — acceptance criteria
- [tech-docs.md](./tech-docs.md) — technical design
- [delivery.md](./delivery.md) — delivery checklist
