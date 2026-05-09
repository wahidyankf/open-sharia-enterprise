# OrganicLever Backend — API

**Audience:** Engineers, Technical Product/Project Managers

OrganicLever backend (`organiclever-be`) is an F#/Giraffe (a functional web framework
on top of ASP.NET Core — think Node.js Express but functional and typed) REST API. v0
ships one endpoint: the health check. All productivity-tracking endpoints are deferred
to future iterations; the backend is deployed alongside the frontend for operational
readiness, not because v0 features require it.

## Endpoints

| Method | Path             | Auth | Description                                 |
| ------ | ---------------- | ---- | ------------------------------------------- |
| GET    | `/api/v1/health` | None | Health check — returns 200 with status body |

## Environment variables

v0 has no required environment variables. The health endpoint runs without any
configuration. Future endpoints will document their variables here.

| Variable     | Scope | Required | Description |
| ------------ | ----- | -------- | ----------- |
| (none in v0) | —     | No       | —           |

## Architecture

```
apps/organiclever-be/
├── src/
│   └── OrganicLeverBe/
│       ├── Program.fs               # Entry point, routing, DI registration
│       ├── Domain/
│       │   └── Types.fs             # Domain error types (reserved for future features)
│       └── Handlers/
│           └── HealthHandler.fs     # GET /api/v1/health
└── tests/
    └── OrganicLeverBe.Tests/
        ├── State.fs                 # BDD step state record
        ├── HttpTestFixture.fs       # WebApplicationFactory wrapper
        ├── Unit/                    # Unit BDD runner (TickSpec + xunit)
        └── Integration/             # Integration BDD runner and step definitions
```

## Tech stack

| Layer     | Technology                                               |
| --------- | -------------------------------------------------------- |
| Language  | F# (functional, type-safe)                               |
| Framework | Giraffe (functional ASP.NET Core web framework)          |
| Runtime   | .NET 10                                                  |
| Port      | 8202 (development)                                       |
| API base  | `/api/v1`                                                |
| Testing   | TickSpec (BDD step runner), xunit, AltCover (coverage)   |
| Coverage  | ≥90% line coverage enforced by `rhino-cli test-coverage` |

## BDD test coverage

All tests consume Gherkin specs from
`specs/apps/organiclever/behavior/be/gherkin/`. The same `.feature` files drive:

- **Unit tests** (`test:unit`) — in-process via `WebApplicationFactory`
- **Coverage gate** (`test:quick`) — unit tests + AltCover 90% threshold

## Related

- [Behavior specs](../../behavior/be/gherkin/README.md) — Gherkin acceptance criteria
- [Container diagram](../../containers/container.md) — where this service fits
- [Deployment](../../containers/deployment.md) — environments and Docker images
