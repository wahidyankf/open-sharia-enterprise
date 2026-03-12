# demo-be-rsax

Rust + Axum REST API backend — a functional twin of `demo-be-jasb` (Java/Spring Boot),
`demo-be-exph` (Elixir/Phoenix), and other demo-be backends using Rust and the Axum framework.

## Tech Stack

| Concern   | Choice                            |
| --------- | --------------------------------- |
| Language  | Rust (stable)                     |
| Framework | Axum 0.8                          |
| Runtime   | Tokio                             |
| Database  | SQLx + SQLite                     |
| JWT       | jsonwebtoken                      |
| Passwords | bcrypt                            |
| BDD Tests | cucumber-rs + Tower TestClient    |
| Coverage  | cargo-llvm-cov (LCOV) + rhino-cli |
| Linting   | clippy + rustfmt                  |
| Port      | 8201                              |

## Local Development

### Prerequisites

- Rust (stable toolchain)
- SQLite (bundled via sqlx)

### Environment Variables

| Variable       | Default           | Description        |
| -------------- | ----------------- | ------------------ |
| `PORT`         | `8201`            | HTTP port          |
| `DATABASE_URL` | `sqlite::memory:` | SQLite connection  |
| `JWT_SECRET`   | (dev default)     | JWT signing secret |

### Run locally

```bash
# Run dev server (SQLite in-memory)
cargo run

# Health check
curl http://localhost:8201/health
```

## Nx Targets

```bash
nx build demo-be-rsax          # Compile release binary
nx dev demo-be-rsax            # Start development server
nx run demo-be-rsax:test:quick # Unit + integration tests + coverage gate + lint
nx run demo-be-rsax:test:unit  # Unit tests only
nx run demo-be-rsax:test:integration  # Integration (cucumber-rs) tests only
nx lint demo-be-rsax           # Run clippy + rustfmt check
```

## API Endpoints

See [plan README](../../plans/done/2026-03-11__demo-be-rsax/README.md) for the full API surface.

## Test Architecture

Integration tests use cucumber-rs with Tower TestClient and in-memory store implementations.
No external services required. Cucumber reads feature files from `specs/apps/demo-be/gherkin/`.
