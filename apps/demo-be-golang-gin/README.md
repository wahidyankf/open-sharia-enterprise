# demo-be-golang-gin

Go + Gin REST API backend — a functional twin of `demo-be-java-springboot` (Java/Spring Boot),
`demo-be-elixir-phoenix` (Elixir/Phoenix), and other demo-be backends using Go and the Gin framework.

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

| Variable       | Default                                                                                | Description        |
| -------------- | -------------------------------------------------------------------------------------- | ------------------ |
| `PORT`         | `8201`                                                                                 | HTTP port          |
| `DATABASE_URL` | `postgresql://demo_be_golang_gin:demo_be_golang_gin@localhost:5432/demo_be_golang_gin` | PostgreSQL URL     |
| `JWT_SECRET`   | (dev default)                                                                          | JWT signing secret |

### Run locally

```bash
# Start PostgreSQL
docker compose -f ../../infra/dev/demo-be-golang-gin/docker-compose.yml up -d demo-be-golang-gin-db

# Run dev server
go run cmd/server/main.go

# Health check
curl http://localhost:8201/health
```

## Nx Targets

```bash
nx build demo-be-golang-gin          # Compile binary
nx dev demo-be-golang-gin            # Start development server
nx run demo-be-golang-gin:test:quick # Unit + integration tests + coverage gate + lint
nx run demo-be-golang-gin:test:unit  # Unit tests only
nx run demo-be-golang-gin:test:integration  # Integration (Godog) tests only
nx lint demo-be-golang-gin           # Run golangci-lint
```

## API Endpoints

See [plan README](../../plans/done/2026-03-11__demo-be-golang-gin/README.md) for the full API surface.

## Test Architecture

Integration tests use Godog with httptest and in-memory store implementations
(Go maps with sync.Mutex). No external services required. Godog reads feature files from
`specs/apps/demo-be/gherkin/`.
