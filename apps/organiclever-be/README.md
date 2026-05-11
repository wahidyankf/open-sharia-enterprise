# organiclever-be

Java 25/Spring Boot 4.0 REST API backend for OrganicLever. Today ships one endpoint: health check.

## Quick Start

```bash
nx dev organiclever-be   # http://localhost:8202
```

## Commands

| Nx target                                 | What it does                          |
| ----------------------------------------- | ------------------------------------- |
| `nx dev organiclever-be`                  | Dev server (localhost:8202)           |
| `nx build organiclever-be`                | Production build                      |
| `nx run organiclever-be:test:quick`       | Unit tests + JaCoCo coverage (90%)    |
| `nx run organiclever-be:test:unit`        | Unit tests only                       |
| `nx run organiclever-be:test:integration` | Integration tests (real HTTP)         |
| `nx run organiclever-be:lint`             | Checkstyle + SpotBugs + PMD analyzers |
| `nx run organiclever-be:typecheck`        | Compile with strict error checking    |

## Prerequisites

- **Java 25 JDK** (`java --version` → 25.x)
- **Maven 3.9** (`mvn --version` → 3.9.x)

## Environment Variables

No required environment variables today. Future endpoints will document theirs here.

## Tech Stack

- **Language**: Java 25
- **Framework**: Spring Boot 4.0.4
- **Build tool**: Maven 3.9
- **Port**: 8202 | **API base**: `/api/v1`
- **Testing**: JUnit 5, Spring Boot Test, JaCoCo (≥90% coverage)
- **Coverage report**: `target/site/jacoco/jacoco.xml`

## Behavior & Architecture

| Artifact      | Location                                                                                    |
| ------------- | ------------------------------------------------------------------------------------------- |
| API reference | [specs/…/components/be/api.md](../../specs/apps/organiclever/components/be/api.md)          |
| Gherkin specs | [specs/…/behavior/be/gherkin/](../../specs/apps/organiclever/behavior/be/gherkin/README.md) |
| Deployment    | [specs/…/containers/deployment.md](../../specs/apps/organiclever/containers/deployment.md)  |

## Related

- [organiclever-be-e2e](../organiclever-be-e2e/README.md) — Playwright BE E2E tests
- [specs/apps/organiclever/](../../specs/apps/organiclever/README.md) — full spec tree
