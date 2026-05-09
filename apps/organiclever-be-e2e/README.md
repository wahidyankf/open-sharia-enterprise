# organiclever-be-e2e

End-to-end tests for the OrganicLever backend API using playwright-bdd
and Playwright's `APIRequestContext` (no browser required).

**Behavior specs** (source of truth):
[`specs/apps/organiclever/behavior/be/gherkin/`](../../specs/apps/organiclever/behavior/be/gherkin/)

## Prerequisites

Backend must be running on `http://localhost:8202` (or `BASE_URL`):

```bash
nx dev organiclever-be
# OR via Docker Compose:
# cd apps/organiclever-be && docker compose -f docker-compose.integration.yml up -d
```

## Setup

```bash
nx install organiclever-be-e2e   # Install Playwright (one-time)
```

## Running Tests

```bash
# Run all BDD E2E tests headlessly
nx run organiclever-be-e2e:test:e2e

# Interactive Playwright UI
nx run organiclever-be-e2e:test:e2e:ui

# View HTML report from last run
nx run organiclever-be-e2e:test:e2e:report

# Pre-push quality gate (lint + typecheck)
nx run organiclever-be-e2e:test:quick
```

`test:e2e` runs on a scheduled cron (twice daily), not on pre-push.

## Environment Variables

| Variable   | Default                 | Description                  |
| ---------- | ----------------------- | ---------------------------- |
| `BASE_URL` | `http://localhost:8202` | Backend base URL             |
| `CI`       | unset                   | Enables CI mode (no retries) |

## Project Structure

```
apps/organiclever-be-e2e/
├── playwright.config.ts    # Playwright + playwright-bdd config
├── steps/                   # BDD step implementations
└── utils/response-store.ts  # Shared APIResponse state between steps
```

## Related

- [Gherkin specs](../../specs/apps/organiclever/behavior/be/gherkin/) — feature files (source of truth)
- [organiclever-be](../organiclever-be/README.md) — backend under test
