# Demo Backend Dev Stack — JASB (Java/Spring Boot)

Local development environment for `demo-be-java-springboot`, the Java/Spring Boot
alternative backend for the Demo Backend platform. Runs on the same port (8201) as the
Go/Gin backend (`demo-be-golang-gin`) and other alternative implementations — the
stacks are mutually exclusive and **must not be started simultaneously**.

## Port Assignment

| Service                 | Port |
| ----------------------- | ---- |
| demo-be-db              | 5432 |
| demo-be-java-springboot | 8201 |

## Quick Start

```bash
# From workspace root
cd infra/dev/demo-be-java-springboot

# First run — build image and start services
docker compose up --build

# Subsequent runs (image cached)
docker compose up
```

Spring Boot auto-migrates the database on startup via Flyway, so the schema is always up
to date.

## Environment Variables

| Variable            | Default                                    | Description                      |
| ------------------- | ------------------------------------------ | -------------------------------- |
| `POSTGRES_USER`     | `organiclever`                             | PostgreSQL username              |
| `POSTGRES_PASSWORD` | `organiclever`                             | PostgreSQL password              |
| `APP_JWT_SECRET`    | `change-me-in-dev-only-not-for-production` | JWT signing secret (HMAC-SHA256) |

Override defaults by setting variables in your shell or in a `.env` file alongside
`docker-compose.yml`.

## Manual Smoke Test

```bash
# Health check
curl http://localhost:8201/health
# Expected: {"status":"UP"}

# Register a user
curl -X POST http://localhost:8201/api/v1/auth/register \
  -H "Content-Type: application/json" \
  -d '{"username":"alice","email":"alice@example.com","password":"Str0ng#Pass1"}'
# Expected: {"id":"<uuid>","username":"alice","email":"alice@example.com"}

# Login
curl -X POST http://localhost:8201/api/v1/auth/login \
  -H "Content-Type: application/json" \
  -d '{"username":"alice","password":"Str0ng#Pass1"}'
# Expected: {"accessToken":"<jwt>","refreshToken":"<refresh>","tokenType":"Bearer"}

# Get profile (replace <jwt> with accessToken from login)
curl http://localhost:8201/api/v1/users/me \
  -H "Authorization: Bearer <jwt>"
# Expected: {"id":"<uuid>","username":"alice","email":"alice@example.com",...}
```

## CI Tests

```bash
# Start backend only in CI mode (frontend does not start — explicit service name)
docker compose -f docker-compose.yml -f docker-compose.ci.yml up --build -d demo-be-java-springboot

# Run BE E2E tests from workspace root
BASE_URL=http://localhost:8201 npx nx run demo-be-e2e:test:e2e

# --- OR ---

# Start full stack in CI mode (backend + frontend)
docker compose -f docker-compose.yml -f docker-compose.ci.yml up --build -d

# Run FE E2E tests from workspace root
BASE_URL=http://localhost:3301 BACKEND_URL=http://localhost:8201 npx nx run demo-fe-e2e:test:e2e

# Stop stack
docker compose -f docker-compose.yml -f docker-compose.ci.yml down -v
```
