# demo-fe-e2e

End-to-end tests for all [demo-fe frontends](../../specs/apps/demo/fe/README.md),
using [playwright-bdd](https://github.com/vitalets/playwright-bdd) to drive tests from Gherkin
feature files.

Tests use Playwright to drive a real browser against a running frontend + backend stack.

## What This Tests

Feature files in [`specs/apps/demo/fe/gherkin/`](../../specs/apps/demo/fe/gherkin/README.md) are the source of truth:

- `health/` - Service health status indicator
- `authentication/` - Login form, session lifecycle, logout
- `user-lifecycle/` - Registration, profile management, password change
- `security/` - Password complexity, account lockout, admin unlock
- `token-management/` - Session info, token verification
- `admin/` - User listing, search, disable/enable
- `expenses/` - CRUD UI, currency, units, reporting, attachments
- `layout/` - Responsive design, WCAG accessibility

## Architecture

```
specs/apps/demo/fe/gherkin/**/*.feature    <- source of truth (read-only)
        |
        v  (defineBddConfig reads features)
playwright.config.ts
        |
        v  (bddgen generates)
.features-gen/**/*.spec.ts            <- auto-generated, gitignored
        |
        v  (playwright test runs)
tests/steps/**/*.ts                   <- step implementations
```

## Tested Frontends

This centralized E2E suite tests any demo-fe frontend by setting `BASE_URL`:

| Frontend            | Framework               | Default Port |
| ------------------- | ----------------------- | ------------ |
| `demo-fe-ts-nextjs` | Next.js 16 (App Router) | 3301         |

## Prerequisites

The frontend must be running on `http://localhost:3301` (or the URL set via `BASE_URL`) and a
backend must be running on `http://localhost:8201` before executing tests.

**Start a backend**:

```bash
nx dev demo-be-golang-gin
```

**Start a frontend**:

```bash
nx dev demo-fe-ts-nextjs
```

## Setup

Install Playwright and its dependencies (one-time setup):

```bash
nx install demo-fe-e2e
cd apps/demo-fe-e2e && npx playwright install --with-deps chromium && cd ../..
```

## Running Tests

```bash
# Run all BDD E2E tests headlessly (generates specs then runs)
nx run demo-fe-e2e:test:e2e

# Run with interactive Playwright UI
nx run demo-fe-e2e:test:e2e:ui

# View HTML report from last run
nx run demo-fe-e2e:test:e2e:report

# Generate spec files only (without running tests)
cd apps/demo-fe-e2e && npx bddgen

# Lint TypeScript source files (oxlint)
nx run demo-fe-e2e:lint

# Type check
nx typecheck demo-fe-e2e

# Pre-push quality gate (typecheck + lint)
nx run demo-fe-e2e:test:quick
```

**See**: [Nx Target Standards](../../governance/development/infra/nx-targets.md) for canonical E2E target names. `test:e2e` runs on a scheduled cron (twice daily at 6 AM and 6 PM WIB via GitHub Actions), not on pre-push.

## CI Integration

This suite runs in two contexts:

1. **`test-demo-fe-ts-nextjs.yml`** — the dedicated frontend workflow. Triggers on changes to `demo-fe-ts-nextjs`, `demo-fe-e2e`, or `demo-be-golang-gin`. Always uses the Go/Gin backend stack via `infra/dev/demo-fe-ts-nextjs/docker-compose.yml`.

2. **Each `test-demo-be-*.yml` backend workflow** — after the backend's integration and BE E2E tests pass, an `e2e-fe` job runs on a fresh runner. It starts the **same backend** being tested (with `ENABLE_TEST_API=true`) alongside the Next.js frontend via that backend's `infra/dev/demo-be-<name>/docker-compose.ci.yml` overlay. This validates that the frontend works end-to-end against every supported backend — not just Go/Gin.

## Environment Variables

| Variable   | Default                 | Description                     |
| ---------- | ----------------------- | ------------------------------- |
| `BASE_URL` | `http://localhost:3301` | Frontend base URL               |
| `CI`       | unset                   | Enables CI mode (single worker) |

Override the base URL to test a different frontend:

```bash
BASE_URL=http://localhost:3301 nx run demo-fe-e2e:test:e2e
```

## Project Structure

```
apps/demo-fe-e2e/
├── playwright.config.ts           # Playwright + playwright-bdd configuration
├── package.json                   # Dependencies (playwright, playwright-bdd)
├── tsconfig.json                  # TypeScript config
├── tests/
│   └── steps/                     # BDD step definitions
└── .features-gen/                 # Auto-generated spec files (gitignored)
```

## Related Documentation

- [Three-Level Testing Standard](../../governance/development/quality/three-level-testing-standard.md) — Unit, integration, and E2E testing boundaries
- [Code Coverage Reference](../../docs/reference/re__code-coverage.md) — Coverage tools, thresholds, and local vs Codecov
- [Project Dependency Graph](../../docs/reference/re__project-dependency-graph.md) — Nx dependency visualization
- [Frontend Gherkin Specs](../../specs/apps/demo/fe/gherkin/README.md) — Shared feature files (source of truth)
- [OpenAPI Contract](../../specs/apps/demo/contracts/README.md) — API contract and codegen
- [Playwright docs](../../docs/explanation/software-engineering/automation-testing/tools/playwright/README.md) — Playwright standards
