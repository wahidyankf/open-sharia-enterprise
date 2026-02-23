# OrganicLever App Web E2E

Playwright browser-based E2E tests for the OrganicLever Flutter web application.

## Overview

This project drives a real Chromium browser against the `organiclever-app` Flutter web app
running on port 3100. It validates the HomeScreen UI — initial state and fetch interaction — by
simulating real user actions.

**Counterpart**: [`organiclever-be-e2e`](../organiclever-be-e2e/) tests the REST API directly
(no browser). This project tests the Flutter web UI end-to-end.

## Prerequisites

Both services must be running before executing tests:

| Service            | Port | Command                    |
| ------------------ | ---- | -------------------------- |
| `organiclever-be`  | 8100 | `npm run organiclever:dev` |
| `organiclever-app` | 3100 | `nx dev organiclever-app`  |

## Setup

### 1. Install Node dependencies

```bash
nx install organiclever-app-web-e2e
```

### 2. Install Playwright browsers

```bash
cd apps/organiclever-app-web-e2e && npx playwright install --with-deps && cd ../..
```

## Running Tests

```bash
# Run all tests (headless Chromium)
nx run organiclever-app-web-e2e:test:e2e

# Open Playwright UI (interactive mode)
nx run organiclever-app-web-e2e:test:e2e:ui

# View HTML report from last run
nx run organiclever-app-web-e2e:test:e2e:report
```

**See**: [Nx Target Standards](../../governance/development/infra/nx-targets.md) for canonical E2E target names. `test:e2e` runs on a scheduled cron (4x/day via GitHub Actions), not on pre-push.

### Targeting Other Environments

Use the `BASE_URL` environment variable to point tests at a different host:

```bash
BASE_URL=http://staging.example.com nx run organiclever-app-web-e2e:test:e2e
```

## Project Structure

```
apps/organiclever-app-web-e2e/
├── project.json             # Nx configuration (install, test:e2e, test:e2e:ui, test:e2e:report targets)
├── package.json             # Pinned @playwright/test@1.50.1 dependency
├── tsconfig.json            # TypeScript config (extends workspace base)
├── playwright.config.ts     # Playwright config (baseURL, Chromium project, reporters)
├── tests/
│   ├── e2e/
│   │   └── home/
│   │       └── home.spec.ts # Tests for HomeScreen (initial state + fetch interaction)
│   ├── pages/
│   │   └── HomePage.ts      # Page Object for HomeScreen selectors and actions
│   └── utils/
│       └── page-helpers.ts  # Shared page utilities (waitForAppReady)
└── README.md                # This file
```

## What Is Tested

### HomeScreen — Initial State

- App title "OrganicLever" is visible in the AppBar
- "Fetch Hello" button is present and clickable

### HomeScreen — Fetch Interaction

- Clicking "Fetch Hello" triggers an API call to `organiclever-be`
- "API Response:" label appears after the call completes
- The message `world!` returned by the backend is displayed

## Related Links

- [organiclever-app README](../organiclever-app/README.md) — Flutter app being tested
- [organiclever-be-e2e README](../organiclever-be-e2e/README.md) — API-level E2E tests
- [OrganicLever infrastructure](../../infra/dev/organiclever/README.md) — Docker Compose setup
- [Playwright documentation](https://playwright.dev/docs/intro)
