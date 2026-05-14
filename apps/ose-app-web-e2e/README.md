# ose-app-web-e2e

Playwright-BDD frontend E2E tests for ose-app-web.

## Quick Start

1. Start dev servers: `nx dev ose-app-web` and `nx dev ose-app-be`
2. Run E2E: `nx run ose-app-web-e2e:test:e2e`

## Commands

| Command                                | Description                 |
| -------------------------------------- | --------------------------- |
| `nx run ose-app-web-e2e:test:e2e`      | Run FE E2E tests headlessly |
| `nx run ose-app-web-e2e:test:e2e:ui`   | Run with Playwright UI      |
| `nx run ose-app-web-e2e:spec-coverage` | Check Gherkin step coverage |

## Feature Files

- [smoke.feature](../../specs/apps/ose-app/behavior/web/gherkin/smoke.feature)
