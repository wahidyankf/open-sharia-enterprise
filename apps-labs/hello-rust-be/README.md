# hello-rust-be

Minimal Actix Web + Tokio hello world server — an experiment in `apps-labs/` exploring Rust
backend development in the OSE repository context.

## Prerequisites

- [Rust](https://rustup.rs/) stable toolchain (managed via `rust-toolchain.toml`)

Install rustup if not already present:

```bash
curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh
```

## Build and run

```bash
cd apps-labs/hello-rust-be

# Build (first run downloads crates — takes a minute)
cargo build

# Run the server
cargo run
# → Starting server at http://127.0.0.1:8080
```

## Endpoints

| Method | Path      | Response                     |
| ------ | --------- | ---------------------------- |
| GET    | `/`       | `Hello, World!` (plain text) |
| GET    | `/health` | `{"status":"ok"}` (JSON)     |

```bash
curl http://127.0.0.1:8080/
# → Hello, World!

curl http://127.0.0.1:8080/health
# → {"status":"ok"}
```

## Stack

- **[Actix Web 4](https://actix.rs/)** — HTTP framework
- **[Tokio 1](https://tokio.rs/)** — async runtime
