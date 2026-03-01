# organiclever-be-e2e

End-to-end tests for the [organiclever-be](../organiclever-be) Spring Boot REST API backend.

Tests use Playwright's `APIRequestContext` to validate HTTP endpoints — no browser needed.

## What This Tests

- `GET /api/v1/hello` — returns `{"message": "world!"}`
- `GET /actuator/health` — returns `{"status": "UP"}`

## Prerequisites

The backend must be running on `http://localhost:8201` before executing tests.

**Recommended — Docker Compose** (no local Java/Maven required):

```bash
cd infra/dev/organiclever && docker compose up -d
```

**Alternative — local Maven** (requires Maven installed):

```bash
nx dev organiclever-be
```

See [organiclever-be README](../organiclever-be/README.md) for full startup options.

## Setup

Install Playwright and its dependencies (one-time setup):

```bash
nx install organiclever-be-e2e
cd apps/organiclever-be-e2e && npx playwright install --with-deps && cd ../..
```

## Running Tests

```bash
# Run all E2E tests headlessly
nx run organiclever-be-e2e:test:e2e

# Run with interactive Playwright UI
nx run organiclever-be-e2e:test:e2e:ui

# View HTML report from last run
nx run organiclever-be-e2e:test:e2e:report

# Lint TypeScript source files (oxlint)
nx run organiclever-be-e2e:lint

# Pre-push quality gate (same as lint for E2E projects)
nx run organiclever-be-e2e:test:quick
```

**See**: [Nx Target Standards](../../governance/development/infra/nx-targets.md) for canonical E2E target names. `test:e2e` runs on a scheduled cron (twice daily at 6 AM and 6 PM WIB via GitHub Actions), not on pre-push.

## Environment Variables

| Variable   | Default                 | Description      |
| ---------- | ----------------------- | ---------------- |
| `BASE_URL` | `http://localhost:8201` | Backend base URL |
| `CI`       | unset                   | Enables CI mode  |

Override the base URL to test against a different environment:

```bash
BASE_URL=http://staging.example.com nx run organiclever-be-e2e:test:e2e
```

## Project Structure

```
apps/organiclever-be-e2e/
├── playwright.config.ts       # Playwright configuration
├── package.json               # Playwright dependency (pinned)
├── tsconfig.json              # TypeScript config
├── tests/
│   ├── e2e/
│   │   ├── hello/
│   │   │   └── hello.spec.ts      # Tests for GET /api/v1/hello
│   │   └── actuator/
│   │       └── health.spec.ts     # Tests for GET /actuator/health
└── test-results/              # JUnit XML output (git-ignored)
```

## Related

- [organiclever-be](../organiclever-be/README.md) — The backend being tested
- [Playwright docs](../../docs/explanation/software-engineering/automation-testing/tools/playwright/README.md) — Playwright standards for this project
