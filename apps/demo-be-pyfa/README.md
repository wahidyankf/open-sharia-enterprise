# demo-be-pyfa

Python/FastAPI implementation of the demo backend REST API — a functional twin of `demo-be-jasb`,
`demo-be-exph`, and `demo-be-fsgi`.

## Tech Stack

| Concern | Choice |
| --- | --- |
| Language | Python 3.13+ |
| Web framework | FastAPI (Uvicorn) |
| Database ORM | SQLAlchemy 2.0+ (PostgreSQL prod / SQLite in-memory for tests) |
| JWT | PyJWT |
| Password hashing | passlib + bcrypt |
| BDD tests | pytest-bdd (Gherkin feature files) |
| Linting | Ruff |
| Type checking | Pyright |
| Coverage | coverage.py + LCOV |
| Port | **8201** |
| Package manager | uv |

## Local Development

### Prerequisites

- Python 3.13+
- [uv](https://github.com/astral-sh/uv) (`pip install uv` or `curl -LsSf https://astral.sh/uv/install.sh | sh`)

### Setup

```bash
cd apps/demo-be-pyfa
uv sync
```

### Environment Variables

| Variable | Default | Description |
| --- | --- | --- |
| `DATABASE_URL` | `sqlite:///:memory:` | SQLAlchemy database URL |
| `APP_JWT_SECRET` | dev default | Secret for JWT signing (min 32 chars) |
| `APP_JWT_ISSUER` | `demo-be-pyfa` | JWT issuer claim |
| `MAX_FAILED_LOGIN_ATTEMPTS` | `5` | Login attempts before account lock |

Create a `.env` file or export these variables before running.

### Start Dev Server

```bash
uv run uvicorn demo_be_pyfa.main:app --reload --port 8201
```

Or via Nx:

```bash
nx dev demo-be-pyfa
```

### With Docker Compose (PostgreSQL)

```bash
cd infra/dev/demo-be-pyfa
docker compose up --build
```

## Nx Targets

| Target | Description |
| --- | --- |
| `nx build demo-be-pyfa` | Build distributable wheel |
| `nx dev demo-be-pyfa` | Start dev server with reload |
| `nx start demo-be-pyfa` | Start production server |
| `nx run demo-be-pyfa:test:quick` | Full quality gate (tests + coverage + lint + typecheck) |
| `nx run demo-be-pyfa:test:unit` | Unit tests only |
| `nx run demo-be-pyfa:test:integration` | BDD integration tests only |
| `nx lint demo-be-pyfa` | Ruff lint check |
| `nx run demo-be-pyfa:typecheck` | Pyright type check |

## API Endpoints

| Method | Path | Auth | Description |
| --- | --- | --- | --- |
| GET | `/health` | No | Health check |
| POST | `/api/v1/auth/register` | No | Register new user |
| POST | `/api/v1/auth/login` | No | Login, return JWT |
| POST | `/api/v1/auth/refresh` | No | Refresh access token |
| POST | `/api/v1/auth/logout` | No | Logout (revoke token) |
| POST | `/api/v1/auth/logout-all` | JWT | Revoke all tokens |
| GET | `/api/v1/users/me` | JWT | Current user profile |
| PATCH | `/api/v1/users/me` | JWT | Update display name |
| POST | `/api/v1/users/me/password` | JWT | Change password |
| POST | `/api/v1/users/me/deactivate` | JWT | Self-deactivate |
| GET | `/api/v1/admin/users` | Admin | List/search users |
| POST | `/api/v1/admin/users/{id}/disable` | Admin | Disable user |
| POST | `/api/v1/admin/users/{id}/enable` | Admin | Enable user |
| POST | `/api/v1/admin/users/{id}/unlock` | Admin | Unlock locked account |
| POST | `/api/v1/admin/users/{id}/force-password-reset` | Admin | Generate reset token |
| POST | `/api/v1/expenses` | JWT | Create expense |
| GET | `/api/v1/expenses` | JWT | List expenses |
| GET | `/api/v1/expenses/summary` | JWT | Summary by currency |
| GET | `/api/v1/expenses/{id}` | JWT | Get expense |
| PUT | `/api/v1/expenses/{id}` | JWT | Update expense |
| DELETE | `/api/v1/expenses/{id}` | JWT | Delete expense |
| POST | `/api/v1/expenses/{id}/attachments` | JWT | Upload attachment |
| GET | `/api/v1/expenses/{id}/attachments` | JWT | List attachments |
| DELETE | `/api/v1/expenses/{id}/attachments/{aid}` | JWT | Delete attachment |
| GET | `/api/v1/reports/pl` | JWT | P&L report |
| GET | `/api/v1/tokens/claims` | JWT | Decode JWT claims |
| GET | `/.well-known/jwks.json` | No | JWKS endpoint |

## Gherkin BDD Tests

Integration tests consume the shared `specs/apps/demo-be/gherkin/` feature files (76 scenarios
across 13 features) using **pytest-bdd**. The FastAPI `TestClient` with SQLite in-memory provides
fully in-process, deterministic testing — no external services required.

```bash
# Run all integration tests
uv run pytest -m integration

# Run all tests with coverage
uv run coverage run -m pytest
uv run coverage lcov -o coverage/lcov.info
```
