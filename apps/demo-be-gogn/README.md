# demo-be-gogn

Go + Gin REST API backend — a functional twin of `demo-be-jasb` (Java/Spring Boot),
`demo-be-exph` (Elixir/Phoenix), and other demo-be backends using Go and the Gin framework.

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

| Variable       | Default                                                              | Description        |
| -------------- | -------------------------------------------------------------------- | ------------------ |
| `PORT`         | `8201`                                                               | HTTP port          |
| `DATABASE_URL` | `postgresql://demo_be_gogn:demo_be_gogn@localhost:5432/demo_be_gogn` | PostgreSQL URL     |
| `JWT_SECRET`   | (dev default)                                                        | JWT signing secret |

### Run locally

```bash
# Start PostgreSQL
docker compose -f ../../infra/dev/demo-be-gogn/docker-compose.yml up -d demo-be-gogn-db

# Run dev server
go run cmd/server/main.go

# Health check
curl http://localhost:8201/health
```

## Nx Targets

```bash
nx build demo-be-gogn          # Compile binary
nx dev demo-be-gogn            # Start development server
nx run demo-be-gogn:test:quick # Unit + integration tests + coverage gate + lint
nx run demo-be-gogn:test:unit  # Unit tests only
nx run demo-be-gogn:test:integration  # Integration (Godog) tests only
nx lint demo-be-gogn           # Run golangci-lint
```

## API Endpoints

See [plan README](../../plans/done/2026-03-11__demo-be-gogn/README.md) for the full API surface.

## Test Architecture

Integration tests use Godog with httptest and in-memory store implementations
(Go maps with sync.Mutex). No external services required. Godog reads feature files from
`specs/apps/demo-be/gherkin/`.
