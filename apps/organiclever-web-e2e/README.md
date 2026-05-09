# organiclever-web-e2e

End-to-end tests for the [OrganicLever frontend](../organiclever-web/README.md),
using playwright-bdd to drive a real browser from Gherkin feature files.

**Behavior specs** (source of truth):
[`specs/apps/organiclever/behavior/web/gherkin/`](../../specs/apps/organiclever/behavior/web/gherkin/)

## Prerequisites

Frontend must be running on `http://localhost:3200` (or `BASE_URL`):

```bash
nx dev organiclever-web
```

## Setup

```bash
nx run organiclever-web-e2e:install   # Install Playwright + browser deps (one-time)
```

## Running Tests

```bash
# Run all BDD E2E tests headlessly
nx run organiclever-web-e2e:test:e2e

# Interactive Playwright UI
nx run organiclever-web-e2e:test:e2e:ui

# View HTML report from last run
nx run organiclever-web-e2e:test:e2e:report

# Pre-push quality gate (typecheck + lint)
nx run organiclever-web-e2e:test:quick
```

`test:e2e` runs on a scheduled cron, not on pre-push.

## Environment Variables

| Variable   | Default                 | Description                     |
| ---------- | ----------------------- | ------------------------------- |
| `BASE_URL` | `http://localhost:3200` | Frontend base URL               |
| `CI`       | unset                   | Enables CI mode (single worker) |

## Project Structure

```
apps/organiclever-web-e2e/
├── playwright.config.ts   # Playwright + playwright-bdd config
├── steps/                  # BDD step definitions (per feature domain)
└── .features-gen/          # Auto-generated spec files (gitignored)
```

## Related

- [Gherkin specs](../../specs/apps/organiclever/behavior/web/gherkin/) — feature files (source of truth)
- [organiclever-web](../organiclever-web/README.md) — frontend under test
