# ose-grc-be

F#/Giraffe REST API backend for the OSE GRC (Governance, Risk and Compliance) platform.

## Quick Start

```bash
# Install .NET tools (AltCover, fsharp-analyzers)
cd apps/ose-grc-be && dotnet tool restore

# Run development server (localhost:8302)
nx dev ose-grc-be

# Run unit tests
nx run ose-grc-be:test:unit
```

## Commands

| Command                              | Description                                               |
| ------------------------------------ | --------------------------------------------------------- |
| `nx dev ose-grc-be`                  | Start development server on localhost:8302                |
| `nx build ose-grc-be`                | Production build to `dist/`                               |
| `nx run ose-grc-be:test:quick`       | Unit tests + AltCover coverage validation (>=90%)         |
| `nx run ose-grc-be:test:unit`        | Unit tests only                                           |
| `nx run ose-grc-be:test:integration` | Integration tests via Docker Compose                      |
| `nx run ose-grc-be:lint`             | Fantomas format check + FSharpLint + G-Research analyzers |
| `nx run ose-grc-be:typecheck`        | Strict build (TreatWarningsAsErrors)                      |
| `nx run ose-grc-be:spec-coverage`    | Validate BDD spec coverage via rhino-cli                  |

## Prerequisites

- .NET 10 SDK (managed via `global.json`)
- Docker (for integration tests)
- Volta + Node.js (for `nx` commands)

## Environment Variables

Copy `.env.example` to `.env` and fill in values:

| Variable              | Description                                   |
| --------------------- | --------------------------------------------- |
| `DATABASE_URL`        | PostgreSQL connection string (Npgsql format)  |
| `OPENROUTER_API_KEY`  | OpenRouter API key (never commit real key)    |
| `OPENROUTER_MODEL`    | Model identifier (default: `openrouter/auto`) |
| `OPENROUTER_BASE_URL` | OpenRouter base URL                           |

## Tech Stack

- **Language**: F# (.NET 10)
- **Web framework**: Giraffe 7
- **Database**: PostgreSQL via EF Core 10 + Npgsql + EFCore.NamingConventions
- **Migrations**: DbUp 5
- **Testing**: xUnit 2 + TickSpec 2 (BDD) + AltCover 9 (coverage)
- **Analyzers**: FSharpLint + G-Research.FSharp.Analyzers

## Related

- **Specs**: `specs/apps/ose-grc/`
- **Contracts**: `specs/apps/ose-grc/containers/contracts/` (OpenAPI 3.1)
- **Infra**: `infra/dev/ose-grc/`
- **E2E tests**: `apps/ose-grc-be-e2e/`
