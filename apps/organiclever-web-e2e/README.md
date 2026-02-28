# organiclever-web-e2e

End-to-end tests for the [organiclever-web](../organiclever-web) Next.js landing and promotional website.

Tests use Playwright browser automation to validate UI pages across Chromium, Firefox, and WebKit.

## What This Tests

- `login` — sign-in form, validation, and redirect behavior (4 tests)
- `home` — landing page content and navigation (7 tests)
- `dashboard` — authenticated dashboard view (6 tests)
- `members` — member listing and management (7 tests)
- `member-detail` — individual member detail page (3 tests)

## Prerequisites

The frontend must be running on `http://localhost:3200` before executing tests.

**Recommended — Docker Compose** (starts both backend and frontend):

```bash
npm run organiclever:dev
```

**Alternative — Nx dev server** (Next.js only):

```bash
nx dev organiclever-web
```

See [organiclever-web README](../organiclever-web/README.md) for full startup options.

## Setup

Install Playwright browsers and their dependencies (one-time setup):

```bash
nx install organiclever-web-e2e
cd apps/organiclever-web-e2e && npx playwright install --with-deps && cd ../..
```

## Running Tests

```bash
# Run all E2E tests headlessly
nx run organiclever-web-e2e:test:e2e

# Run with interactive Playwright UI
nx run organiclever-web-e2e:test:e2e:ui

# View HTML report from last run
nx run organiclever-web-e2e:test:e2e:report

# Lint TypeScript source files (oxlint)
nx run organiclever-web-e2e:lint

# Pre-push quality gate (same as lint for E2E projects)
nx run organiclever-web-e2e:test:quick
```

**See**: [Nx Target Standards](../../governance/development/infra/nx-targets.md) for canonical E2E target names. `test:e2e` runs on a scheduled cron (twice daily at 7 AM and 7 PM WIB via GitHub Actions), not on pre-push.

Override the base URL to test against a different environment:

```bash
BASE_URL=http://staging.example.com nx run organiclever-web-e2e:test:e2e
```

## Environment Variables

| Variable   | Default                 | Description       |
| ---------- | ----------------------- | ----------------- |
| `BASE_URL` | `http://localhost:3200` | Frontend base URL |
| `CI`       | unset                   | Enables CI mode   |

## Project Structure

```
apps/organiclever-web-e2e/
├── playwright.config.ts       # Playwright configuration
├── package.json               # Playwright dependency (pinned)
├── tsconfig.json              # TypeScript config
├── tests/
│   ├── utils/
│   │   ├── auth.ts            # loginWithUI() and logoutViaAPI() helpers
│   │   └── test-config.ts     # Shared test configuration
│   └── e2e/
│       └── pages/
│           ├── login.spec.ts          # Login page tests
│           ├── home.spec.ts           # Home page tests
│           ├── dashboard.spec.ts      # Dashboard page tests
│           ├── members.spec.ts        # Members listing tests
│           └── member-detail.spec.ts  # Member detail page tests
└── test-results/              # JUnit XML output (git-ignored)
```

## Related

- [organiclever-web](../organiclever-web/README.md) — The frontend being tested
- [organiclever-be-e2e](../organiclever-be-e2e/README.md) — API-level E2E counterpart (tests the Spring Boot backend)
- [organiclever-app-web-e2e](../organiclever-app-web-e2e/README.md) — Browser-based E2E for the Flutter web UI
- [Playwright docs](../../docs/explanation/software-engineering/automation-testing/tools/playwright/README.md) — Playwright standards for this project
