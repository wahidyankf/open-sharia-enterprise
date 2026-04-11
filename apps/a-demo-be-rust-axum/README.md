# a-demo-be-rust-axum

Rust + Axum REST API backend — a functional twin of `a-demo-be-golang-gin` (Go/Gin),
`a-demo-be-elixir-phoenix` (Elixir/Phoenix), and other demo-be backends using Rust and the Axum framework.

## Tech Stack

| Concern   | Choice                                          |
| --------- | ----------------------------------------------- |
| Language  | Rust (stable)                                   |
| Framework | Axum 0.8                                        |
| Runtime   | Tokio                                           |
| Database  | SQLx + SQLite (unit) / PostgreSQL (integration) |
| JWT       | jsonwebtoken                                    |
| Passwords | bcrypt                                          |
| BDD Tests | cucumber-rs + Tower TestClient                  |
| Coverage  | cargo-llvm-cov (LCOV) + rhino-cli               |
| Linting   | clippy + rustfmt                                |
| Port      | 8201                                            |

## Local Development

### Prerequisites

- Rust (stable toolchain)
- SQLite (bundled via sqlx)
- Docker (for integration tests with PostgreSQL)

### Environment Variables

| Variable       | Default           | Description         |
| -------------- | ----------------- | ------------------- |
| `PORT`         | `8201`            | HTTP port           |
| `DATABASE_URL` | `sqlite::memory:` | Database connection |
| `JWT_SECRET`   | (dev default)     | JWT signing secret  |

### Run locally

```bash
# Run dev server (SQLite in-memory)
cargo run

# Health check
curl http://localhost:8201/health
```

## Nx Targets

```bash
nx build a-demo-be-rust-axum                    # Compile release binary (depends on codegen)
nx dev a-demo-be-rust-axum                      # Start development server
nx run a-demo-be-rust-axum:typecheck            # cargo check (depends on codegen)
nx run a-demo-be-rust-axum:test:quick           # Unit tests + coverage gate (no lint; depends on codegen)
nx run a-demo-be-rust-axum:test:unit            # Unit tests only (lib + BDD with SQLite in-memory; depends on codegen)
nx run a-demo-be-rust-axum:test:integration     # Integration tests via Docker Compose (PostgreSQL)
nx lint a-demo-be-rust-axum                     # Run clippy + rustfmt check
```

`codegen` generates Rust model structs from the OpenAPI contract spec into `generated-contracts/`
and is a dependency of `typecheck`, `build`, `test:quick`, and `test:unit`.

## API Endpoints

See [plan README](../../plans/done/2026-03-11__demo-be-rust-axum/README.md) for the full API surface.

## Test Architecture

This project uses a three-level test architecture:

### Unit Tests (`tests/unit/`)

- Run all shared Gherkin scenarios using cucumber-rs with Tower TestClient
- Use SQLite in-memory database (no external services required)
- Also include inline `#[cfg(test)]` unit tests in source modules
- Run via `cargo test --lib --test unit`
- Coverage measured with cargo-llvm-cov and validated at ≥90%
- Used by `test:quick` and `test:unit` targets

### Integration Tests (`tests/integration/`)

- Run all shared Gherkin scenarios against a real PostgreSQL 17 database
- Launched via Docker Compose (`docker-compose.integration.yml`)
- Reads specs from `/specs/apps/a-demo/be/gherkin/` (mounted volume)
- Not cached — always runs fresh
- Used by `test:integration` target

### End-to-End Tests

- Covered by `a-demo-be-e2e` (Playwright) running against the full deployed stack
- Not part of this project's targets

### Running Integration Tests

```bash
# Runs PostgreSQL + test-runner in Docker, tears down on completion
nx run a-demo-be-rust-axum:test:integration
```

## Related Documentation

- [Three-Level Testing Standard](../../governance/development/quality/three-level-testing-standard.md) — Unit, integration, and E2E testing boundaries
- [Code Coverage Reference](../../docs/reference/code-coverage.md) — Coverage tools, thresholds, and local vs Codecov
- [Project Dependency Graph](../../docs/reference/project-dependency-graph.md) — Nx dependency visualization
- [Backend Gherkin Specs](../../specs/apps/a-demo/be/gherkin/README.md) — Shared feature files (source of truth)
- [OpenAPI Contract](../../specs/apps/a-demo/contracts/README.md) — API contract and codegen
