# demo-fe-ts-tanstack-start

Demo Frontend - TanStack Start (TypeScript) implementation consuming the
[demo-be API](../demo-be-golang-gin/README.md). An alternative to
[demo-fe-ts-nextjs](../demo-fe-ts-nextjs/README.md).

## Overview

- **Framework**: TanStack Start (Vite)
- **Language**: TypeScript
- **State Management**: TanStack Query v5
- **BDD Tool**: @amiceli/vitest-cucumber
- **Port**: 3301
- **Specs**: [`specs/apps/demo/fe/gherkin/`](../../specs/apps/demo/fe/gherkin/README.md)

## Prerequisites

- **Node.js 24** (managed by Volta)
- **npm 11**
- A running [demo-be backend](../demo-be-golang-gin/README.md) on port 8201 (for E2E tests)

## Nx Commands

```bash
# Start development server (localhost:3301)
nx dev demo-fe-ts-tanstack-start

# Production build
nx build demo-fe-ts-tanstack-start

# Start production server
nx run demo-fe-ts-tanstack-start:start

# Type checking
nx typecheck demo-fe-ts-tanstack-start

# Lint code (oxlint)
nx lint demo-fe-ts-tanstack-start

# Fast quality gate: unit tests + coverage check
nx run demo-fe-ts-tanstack-start:test:quick

# Unit tests only
nx run demo-fe-ts-tanstack-start:test:unit
```

**See**: [Nx Target Standards](../../governance/development/infra/nx-targets.md) for canonical target names.

## Project Structure

```
apps/demo-fe-ts-tanstack-start/
├── src/
│   ├── routes/                   # TanStack Router route definitions
│   ├── components/               # Reusable React components
│   ├── lib/                      # Utilities, API clients, hooks
│   └── test/                     # Test utilities
├── test/
│   └── unit/                     # Unit test step definitions (BDD)
├── vite.config.ts                # Vite configuration
├── vitest.config.ts              # Vitest configuration (coverage thresholds)
├── tsconfig.json                 # TypeScript configuration
└── project.json                  # Nx targets and tags
```

## Testing

Two levels of testing consume the shared Gherkin scenarios from [`specs/apps/demo/fe/gherkin/`](../../specs/apps/demo/fe/gherkin/README.md):

| Level | Tool                        | Dependencies | Command                                      | Cached? |
| ----- | --------------------------- | ------------ | -------------------------------------------- | ------- |
| Unit  | @amiceli/vitest-cucumber    | All mocked   | `nx run demo-fe-ts-tanstack-start:test:unit` | Yes     |
| E2E   | Playwright + playwright-bdd | Full stack   | `nx run demo-fe-e2e:test:e2e`                | No      |

**Coverage**: Measured from `test:unit` only (Vitest v8). `test:quick` = `test:unit` + `rhino-cli test-coverage validate` (>=70%). Both `test:quick` and `test:unit` include `{projectRoot}/src/generated-contracts/**/*` as cache inputs so contract changes invalidate the cache.

### Unit Tests

Steps test component logic and state management with fully mocked dependencies.
No DOM rendering, no HTTP calls:

```bash
nx run demo-fe-ts-tanstack-start:test:unit
```

### E2E Tests

The [`demo-fe-e2e`](../demo-fe-e2e/) project provides centralized Playwright-based E2E tests
for all demo-fe frontends. Run them after starting this frontend and a backend:

```bash
# Start backend
nx dev demo-be-golang-gin

# Start this frontend (in another terminal)
nx dev demo-fe-ts-tanstack-start

# Run E2E tests (in another terminal)
BASE_URL=http://localhost:3301 nx run demo-fe-e2e:test:e2e
```

## Related Documentation

- [Three-Level Testing Standard](../../governance/development/quality/three-level-testing-standard.md) — Unit, integration, and E2E testing boundaries
- [Code Coverage Reference](../../docs/reference/re__code-coverage.md) — Coverage tools, thresholds, and local vs Codecov
- [Project Dependency Graph](../../docs/reference/re__project-dependency-graph.md) — Nx dependency visualization
- [Frontend Gherkin Specs](../../specs/apps/demo/fe/gherkin/README.md) — Shared feature files (source of truth)
- [OpenAPI Contract](../../specs/apps/demo/contracts/README.md) — API contract and codegen
- [demo-fe-e2e](../demo-fe-e2e/README.md) — Centralized E2E tests for all demo-fe frontends
