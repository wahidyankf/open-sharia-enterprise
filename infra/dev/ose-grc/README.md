# OSE GRC Local Development Infrastructure

Docker Compose setup for running the full OSE GRC stack locally.

## Services

| Service     | Port | Description                 |
| ----------- | ---- | --------------------------- |
| postgres    | 5432 | PostgreSQL 17 database      |
| ose-grc-be  | 8302 | F#/Giraffe REST API backend |
| ose-grc-web | 3300 | Next.js 16 frontend         |

## Quick Start

```bash
# Copy env (edit OPENROUTER_API_KEY if needed)
cp infra/dev/ose-grc/.env.example infra/dev/ose-grc/.env

# Start all services
docker compose -f infra/dev/ose-grc/docker-compose.yml up --build -d

# Verify health
curl -sf http://localhost:8302/api/v1/health
curl -sf http://localhost:3300

# Tear down (removes volumes)
docker compose -f infra/dev/ose-grc/docker-compose.yml down -v
```

## Environment Variables

| Variable            | Default                          | Required |
| ------------------- | -------------------------------- | -------- |
| DATABASE_URL        | postgresql://ose_grc:ose_grc@... | No       |
| OPENROUTER_API_KEY  | (empty)                          | No\*     |
| OPENROUTER_MODEL    | openrouter/auto                  | No       |
| OPENROUTER_BASE_URL | https://openrouter.ai/api/v1     | No       |

\*Required for AI-assisted gap analysis (not needed for bootstrap health-only scaffold).

## CI Variant

`docker-compose.ci.yml` overrides environment variables for GitHub Actions E2E runs.

## Behavior & Architecture

See [specs/apps/ose-grc/containers/deployment.md](../../../specs/apps/ose-grc/containers/deployment.md)
for environments, Docker images, and deployment details.
