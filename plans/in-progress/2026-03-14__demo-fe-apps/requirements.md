# Requirements: Demo Frontend Apps

## Overview

Six new applications that provide the frontend counterpart to the existing demo-be backend
implementations. Five frontend frameworks (Next.js, TanStack Start, React Router, Flutter Web,
Phoenix LiveView) and one centralized Playwright E2E test suite.

## Functional Requirements

### FR-01: Feature Parity

All five frontend implementations (`demo-fe-ts-nextjs`, `demo-fe-ts-tanstackstart`,
`demo-fe-ts-remix`, `demo-fe-dart-flutter`, and `demo-fe-elixir-phoenix`) must implement identical
functionality:

1. **Health status page** — display backend health status
2. **Authentication** — login form, session management, automatic token refresh, logout
3. **User lifecycle** — registration form, profile management, password change, self-deactivation
4. **Security** — password complexity validation in forms, account lockout display, admin unlock
5. **Token management** — session info display, token verification, multi-device logout
6. **Admin panel** — user listing with search, disable/enable accounts, password reset token generation
7. **Expenses** — CRUD UI with currency precision, unit-of-measure support, P&L reporting, file
   attachment upload/download/delete
8. **Layout** — responsive design (desktop/tablet/mobile viewports), WCAG AA accessibility

### FR-02: Backend Compatibility

- All frontends connect to `demo-be-java-springboot` backend on port 8201
- API proxy configured to forward `/api/v1/*`, `/health`, and `/.well-known/*` to the backend
- JWT tokens stored securely (httpOnly cookies or secure localStorage with refresh rotation)

### FR-03: React Query Integration

- All API calls use TanStack Query v5 (`@tanstack/react-query`)
- `QueryClientProvider` wraps the app root
- SSR: Query prefetching and dehydration/hydration for initial page loads
- Optimistic updates for expense CRUD operations
- Automatic token refresh via query interceptor or middleware
- Query invalidation on mutations (create/update/delete expenses, user profile changes)
- Error boundaries for failed queries

### FR-03b: Flutter State Management (demo-fe-dart-flutter)

- All API calls use dio HTTP client with interceptors for auth headers
- State management via Riverpod 3.0 (providers for auth, user, expenses, admin)
- Token refresh via dio interceptor (intercept 401, refresh, retry)
- Provider invalidation on mutations (create/update/delete expenses, profile changes)
- Error handling via AsyncValue (Riverpod's loading/error/data pattern)

### FR-03c: Phoenix LiveView State Management (demo-fe-elixir-phoenix)

- All API calls use Req HTTP client from server-side LiveView processes
- State managed via socket assigns (LiveView's built-in stateful process model)
- Token refresh via LiveView `handle_info/2` callback (scheduled refresh before expiry)
- Real-time updates via LiveView's automatic diff-based re-rendering on assign changes
- Error handling via `put_flash/3` for user-visible errors
- No client-side JavaScript framework — all rendering server-side with WebSocket diffs

### FR-04: Gherkin Spec Coverage (Unit Tests)

- All 92 scenarios from `specs/apps/demo/fe/gherkin/` must pass at the unit test level
- TypeScript apps: unit tests consume Gherkin feature files via `@amiceli/vitest-cucumber`,
  step definitions render components with React Testing Library (`@testing-library/react` +
  `@testing-library/user-event`) in jsdom, user interactions via `userEvent`, assertions via
  RTL queries (`getByRole`, `getByText`), API responses mocked via `vi.mock`
- Flutter app: unit tests consume Gherkin feature files via bdd_widget_test, step definitions
  test widget logic with mocked dio responses
- LiveView app: unit tests consume Gherkin feature files via Cabbage, step definitions use
  Phoenix.LiveViewTest to simulate user interactions with mocked HTTP responses
- No real HTTP calls in unit tests
- Coverage measured at unit level: >=90% line coverage via `rhino-cli test-coverage validate`

### FR-05: E2E Test Coverage (demo-fe-e2e)

- All 92 scenarios from `specs/apps/demo/fe/gherkin/` must pass at the E2E level
- E2E tests use Playwright + playwright-bdd v8
- Tests drive a real browser against running frontend + backend + PostgreSQL
- Centralized suite tests any frontend implementation via `BASE_URL` env var
- Sequential execution (`workers: 1`) to avoid state conflicts
- **No direct database access** — E2E tests interact exclusively through HTTP APIs
- Database cleanup between scenarios via `POST /api/v1/test/reset-db` (test-only endpoint)
- Admin user promotion via `POST /api/v1/test/promote-admin` (test-only endpoint)
- Test-only endpoints gated behind `ENABLE_TEST_API=true` environment variable
- All other test setup (user registration, login, expense creation) uses existing public APIs

### FR-06: Test-Only Backend API

- `demo-be-java-springboot` (and all demo-be backends) must expose test-only endpoints:
  - `POST /api/v1/test/reset-db` — delete all data from all tables
  - `POST /api/v1/test/promote-admin` — promote a user to ADMIN role (body: `{"username": "..."}`)
- Endpoints disabled by default, enabled only when `ENABLE_TEST_API=true` is set
- Endpoints prefixed with `/api/v1/test/` and require no authentication
- Test API controller/router conditionally loaded (not registered in production)
- New Gherkin feature file `specs/apps/demo/be/gherkin/test-support/test-api.feature` documents
  the test-only endpoints for consistent implementation across all backends

## Non-Functional Requirements

### NFR-01: Nx Integration

Each app must have a `project.json` with these targets:

**TypeScript frontend apps** (`demo-fe-ts-nextjs`, `demo-fe-ts-tanstackstart`, `demo-fe-ts-remix`):

| Target       | Required | Cacheable | Description                                          |
| ------------ | -------- | --------- | ---------------------------------------------------- |
| `dev`        | Yes      | No        | Start development server                             |
| `build`      | Yes      | Yes       | Production build                                     |
| `start`      | Yes      | No        | Start production server                              |
| `typecheck`  | Yes      | Yes       | `tsc --noEmit`                                       |
| `lint`       | Yes      | Yes       | `oxlint`                                             |
| `test:quick` | Yes      | Yes       | Unit tests + coverage + spec coverage                |
| `test:unit`  | Yes      | Yes       | Vitest unit tests (@amiceli/vitest-cucumber + jsdom) |

**Flutter frontend app** (`demo-fe-dart-flutter`):

| Target       | Required | Cacheable | Description                                       |
| ------------ | -------- | --------- | ------------------------------------------------- |
| `dev`        | Yes      | No        | `flutter run -d chrome --web-port 3301`           |
| `build`      | Yes      | Yes       | `flutter build web --release`                     |
| `start`      | Yes      | No        | Serve `build/web/` on port 3301                   |
| `lint`       | Yes      | Yes       | `dart analyze`                                    |
| `test:quick` | Yes      | Yes       | Widget tests + coverage + spec coverage           |
| `test:unit`  | Yes      | Yes       | `flutter test` (widget tests via bdd_widget_test) |

**Phoenix LiveView frontend app** (`demo-fe-elixir-phoenix`):

| Target       | Required | Cacheable | Description                                   |
| ------------ | -------- | --------- | --------------------------------------------- |
| `dev`        | Yes      | No        | `mix phx.server` on port 3301                 |
| `build`      | Yes      | Yes       | `mix compile`                                 |
| `start`      | Yes      | No        | `mix phx.server` (release mode)               |
| `lint`       | Yes      | Yes       | `mix credo`                                   |
| `test:quick` | Yes      | Yes       | LiveViewTest + coverage + spec coverage       |
| `test:unit`  | Yes      | Yes       | `mix coveralls.lcov` (Cabbage + LiveViewTest) |

**E2E app** (`demo-fe-e2e`):

| Target            | Required | Cacheable | Description                                       |
| ----------------- | -------- | --------- | ------------------------------------------------- |
| `install`         | Yes      | No        | `npm install` (installs project-local Playwright) |
| `lint`            | Yes      | Yes       | `oxlint`                                          |
| `typecheck`       | Yes      | Yes       | `bddgen && tsc --noEmit`                          |
| `test:quick`      | Yes      | Yes       | lint + typecheck in parallel                      |
| `test:e2e`        | Yes      | No        | `bddgen && playwright test`                       |
| `test:e2e:ui`     | Yes      | No        | `bddgen && playwright test --ui`                  |
| `test:e2e:report` | Yes      | No        | `playwright show-report`                          |

### NFR-02: Nx Tags

| App                        | Tags                                                              |
| -------------------------- | ----------------------------------------------------------------- |
| `demo-fe-ts-nextjs`        | `type:app`, `platform:nextjs`, `lang:ts`, `domain:demo-fe`        |
| `demo-fe-ts-tanstackstart` | `type:app`, `platform:tanstackstart`, `lang:ts`, `domain:demo-fe` |
| `demo-fe-ts-remix`         | `type:app`, `platform:reactrouter`, `lang:ts`, `domain:demo-fe`   |
| `demo-fe-dart-flutter`     | `type:app`, `platform:flutter`, `lang:dart`, `domain:demo-fe`     |
| `demo-fe-elixir-phoenix`   | `type:app`, `platform:phoenix`, `lang:elixir`, `domain:demo-fe`   |
| `demo-fe-e2e`              | `type:e2e`, `platform:playwright`, `lang:ts`, `domain:demo-fe`    |

### NFR-03: Docker Compose (E2E Infrastructure)

Each frontend needs a Docker Compose setup for E2E testing:

```
infra/dev/demo-fe-ts-nextjs/
├── docker-compose.yml    # FE app + demo-be-java-springboot + PostgreSQL
└── .env.example

infra/dev/demo-fe-ts-tanstackstart/
├── docker-compose.yml    # FE app + demo-be-java-springboot + PostgreSQL
└── .env.example

infra/dev/demo-fe-ts-remix/
├── docker-compose.yml    # FE app + demo-be-java-springboot + PostgreSQL
└── .env.example

infra/dev/demo-fe-dart-flutter/
├── docker-compose.yml    # FE app + demo-be-java-springboot + PostgreSQL
└── .env.example

infra/dev/demo-fe-elixir-phoenix/
├── docker-compose.yml    # FE app + demo-be-java-springboot + PostgreSQL
└── .env.example
```

Services:

- **db**: PostgreSQL 17 with demo-be schema
- **backend**: `demo-be-java-springboot` on port 8201
- **frontend**: The specific frontend app on its dev port
- **e2e-runner** (optional): Runs `demo-fe-e2e` against the frontend

### NFR-04: GitHub Actions Workflows

Five E2E workflows (one per frontend), each:

- Scheduled 2x daily at 7 AM / 7 PM WIB (1 hour after demo-be workflows at 6 AM / 6 PM WIB)
- Triggered on push to `main` when relevant files change
- Spins up PostgreSQL + backend + frontend via Docker Compose
- Runs `demo-fe-e2e` test suite with `BASE_URL` pointing to the frontend
- Uploads test artifacts (screenshots, traces) on failure
- Uploads coverage to Codecov (for unit test coverage from `test:quick`)

### NFR-05: Performance & Accessibility

- Lighthouse score >= 90 for Performance, Accessibility, Best Practices
- WCAG AA compliance (color contrast, keyboard navigation, screen reader support)
- Responsive breakpoints: mobile (< 768px), tablet (768-1024px), desktop (> 1024px)

## Acceptance Criteria

### AC-01: demo-fe-ts-nextjs

```gherkin
Feature: Next.js Frontend Application
  Scenario: All unit tests pass
    Given the demo-fe-ts-nextjs app is built
    When I run "nx run demo-fe-ts-nextjs:test:unit"
    Then all 92 Gherkin scenarios pass
    And line coverage is >= 90%

  Scenario: Application builds successfully
    Given all dependencies are installed
    When I run "nx build demo-fe-ts-nextjs"
    Then the build completes without errors

  Scenario: Development server starts
    Given all dependencies are installed
    When I run "nx dev demo-fe-ts-nextjs"
    Then the server starts on port 3301
    And the health page shows backend status

  Scenario: API proxy works
    Given the frontend is running on port 3301
    And a demo-be backend is running on port 8201
    When the frontend makes a request to /api/v1/health
    Then the request is proxied to the backend
    And the response is returned to the frontend
```

### AC-02: demo-fe-ts-tanstackstart

```gherkin
Feature: TanStack Start Frontend Application
  Scenario: All unit tests pass
    Given the demo-fe-ts-tanstackstart app is built
    When I run "nx run demo-fe-ts-tanstackstart:test:unit"
    Then all 92 Gherkin scenarios pass
    And line coverage is >= 90%

  Scenario: Application builds successfully
    Given all dependencies are installed
    When I run "nx build demo-fe-ts-tanstackstart"
    Then the build completes without errors

  Scenario: Development server starts
    Given all dependencies are installed
    When I run "nx dev demo-fe-ts-tanstackstart"
    Then the server starts on port 3301
    And the health page shows backend status

  Scenario: Type-safe routing works
    Given the app has routes defined in app/routes/
    When I navigate between routes
    Then all route parameters are fully type-safe
    And the URL matches the expected pattern
```

### AC-03: demo-fe-ts-remix

```gherkin
Feature: React Router (Remix) Frontend Application
  Scenario: All unit tests pass
    Given the demo-fe-ts-remix app is built
    When I run "nx run demo-fe-ts-remix:test:unit"
    Then all 92 Gherkin scenarios pass
    And line coverage is >= 90%

  Scenario: Application builds successfully
    Given all dependencies are installed
    When I run "nx build demo-fe-ts-remix"
    Then the build completes without errors

  Scenario: Development server starts
    Given all dependencies are installed
    When I run "nx dev demo-fe-ts-remix"
    Then the server starts on port 3301
    And the health page shows backend status

  Scenario: Loaders and actions work
    Given the app has routes defined in app/routes/
    When I navigate to a page with a loader
    Then the loader data is available on initial render
    And form submissions use actions for data mutations
```

### AC-04: demo-fe-dart-flutter

```gherkin
Feature: Flutter Web Frontend Application
  Scenario: All unit tests pass
    Given the demo-fe-dart-flutter app is built
    When I run "nx run demo-fe-dart-flutter:test:unit"
    Then all 92 Gherkin scenarios pass
    And line coverage is >= 90%

  Scenario: Application builds successfully
    Given Flutter SDK is installed
    When I run "nx build demo-fe-dart-flutter"
    Then the build completes without errors
    And build/web/ contains static assets

  Scenario: Development server starts
    Given Flutter SDK is installed
    When I run "nx dev demo-fe-dart-flutter"
    Then the app starts in Chrome on port 3301
    And the health page shows backend status

  Scenario: Web-first rendering
    Given the app is running in a browser
    Then the CanvasKit or Skwasm renderer is used
    And the UI renders correctly across viewports
```

### AC-05: demo-fe-elixir-phoenix

```gherkin
Feature: Phoenix LiveView Frontend Application
  Scenario: All unit tests pass
    Given the demo-fe-elixir-phoenix app is compiled
    When I run "nx run demo-fe-elixir-phoenix:test:unit"
    Then all 92 Gherkin scenarios pass
    And line coverage is >= 90%

  Scenario: Application compiles successfully
    Given Elixir and OTP are installed
    When I run "nx build demo-fe-elixir-phoenix"
    Then compilation completes without warnings or errors

  Scenario: Development server starts
    Given Elixir and OTP are installed
    When I run "nx dev demo-fe-elixir-phoenix"
    Then the server starts on port 3301
    And the health page shows backend status

  Scenario: Server-side rendering works
    Given the app is running
    When a user navigates to any page
    Then the initial HTML is fully rendered on first load
    And subsequent interactions use WebSocket diffs
```

### AC-06: demo-fe-e2e

```gherkin
Feature: Centralized Frontend E2E Test Suite
  Scenario: E2E tests pass against Next.js frontend
    Given demo-fe-ts-nextjs is running on port 3301
    And demo-be-java-springboot is running on port 8201
    And PostgreSQL is running with a clean database
    When I run "BASE_URL=http://localhost:3301 nx run demo-fe-e2e:test:e2e"
    Then all 92 Gherkin scenarios pass

  Scenario: E2E tests pass against TanStack Start frontend
    Given demo-fe-ts-tanstackstart is running on port 3301
    And demo-be-java-springboot is running on port 8201
    And PostgreSQL is running with a clean database
    When I run "BASE_URL=http://localhost:3301 nx run demo-fe-e2e:test:e2e"
    Then all 92 Gherkin scenarios pass

  Scenario: E2E tests pass against React Router (Remix) frontend
    Given demo-fe-ts-remix is running on port 3301
    And demo-be-java-springboot is running on port 8201
    And PostgreSQL is running with a clean database
    When I run "BASE_URL=http://localhost:3301 nx run demo-fe-e2e:test:e2e"
    Then all 92 Gherkin scenarios pass

  Scenario: E2E tests pass against Flutter Web frontend
    Given demo-fe-dart-flutter is running on port 3301
    And demo-be-java-springboot is running on port 8201
    And PostgreSQL is running with a clean database
    When I run "BASE_URL=http://localhost:3301 nx run demo-fe-e2e:test:e2e"
    Then all 92 Gherkin scenarios pass

  Scenario: E2E tests pass against Phoenix LiveView frontend
    Given demo-fe-elixir-phoenix is running on port 3301
    And demo-be-java-springboot is running on port 8201
    And PostgreSQL is running with a clean database
    When I run "BASE_URL=http://localhost:3301 nx run demo-fe-e2e:test:e2e"
    Then all 92 Gherkin scenarios pass

  Scenario: Database cleanup between scenarios
    Given E2E tests are running
    When a scenario completes
    Then the database is cleaned for the next scenario
    And no state leaks between scenarios

  Scenario: Test artifacts on failure
    Given an E2E scenario fails
    Then a screenshot is captured
    And a trace file is available
    And the HTML report includes the failure details
```

### AC-07: CI/CD

```gherkin
Feature: Continuous Integration
  Scenario: Pre-push quality gate passes
    When I run "nx affected -t test:quick"
    Then demo-fe-ts-nextjs lint, typecheck, and unit tests pass
    And demo-fe-ts-tanstackstart lint, typecheck, and unit tests pass
    And demo-fe-ts-remix lint, typecheck, and unit tests pass
    And demo-fe-dart-flutter lint and unit tests pass
    And demo-fe-elixir-phoenix lint and unit tests pass
    And demo-fe-e2e lint and typecheck pass

  Scenario: E2E workflows run in CI
    Given changes are pushed to main
    When the e2e-demo-fe-ts-nextjs workflow triggers
    Then it spins up PostgreSQL + backend + frontend
    And runs all 92 E2E scenarios
    And uploads artifacts on failure

  Scenario: Coverage is uploaded to Codecov
    Given the main-ci workflow runs
    When unit tests complete for all five frontends
    Then LCOV coverage is uploaded to Codecov
    And the coverage badge reflects the latest result
```
