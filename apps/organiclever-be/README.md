# organiclever-be

F#/Giraffe REST API backend for OrganicLever. Today ships one endpoint: health check.

## Quick Start

```bash
nx dev organiclever-be   # http://localhost:8202
```

## Commands

| Nx target                                 | What it does                      |
| ----------------------------------------- | --------------------------------- |
| `nx dev organiclever-be`                  | Dev server (localhost:8202)       |
| `nx build organiclever-be`                | Production build                  |
| `nx run organiclever-be:test:quick`       | BDD unit tests + AltCover (90%)   |
| `nx run organiclever-be:test:unit`        | BDD unit tests only               |
| `nx run organiclever-be:test:integration` | Integration tests (real HTTP)     |
| `nx run organiclever-be:lint`             | Fantomas + FSharpLint + analyzers |
| `nx run organiclever-be:typecheck`        | Build with TreatWarningsAsErrors  |

## Prerequisites

- **.NET 10 SDK** (`dotnet --version` → 10.x)
- **Fantomas** (`dotnet tool install -g fantomas`)
- **FSharpLint** (`dotnet tool install -g dotnet-fsharplint`)

## Environment Variables

No required environment variables today. Future endpoints will document theirs here.

## Tech Stack

- **Language**: F# (functional, type-safe)
- **Framework**: Giraffe (functional ASP.NET Core)
- **Runtime**: .NET 10
- **Port**: 8202 | **API base**: `/api/v1`
- **Testing**: TickSpec (BDD), xunit, AltCover (≥90% coverage)

## Behavior & Architecture

| Artifact      | Location                                                                                    |
| ------------- | ------------------------------------------------------------------------------------------- |
| API reference | [specs/…/components/be/api.md](../../specs/apps/organiclever/components/be/api.md)          |
| Gherkin specs | [specs/…/behavior/be/gherkin/](../../specs/apps/organiclever/behavior/be/gherkin/README.md) |
| Deployment    | [specs/…/containers/deployment.md](../../specs/apps/organiclever/containers/deployment.md)  |

## Related

- [organiclever-be-e2e](../organiclever-be-e2e/README.md) — Playwright BE E2E tests
- [specs/apps/organiclever/](../../specs/apps/organiclever/README.md) — full spec tree
