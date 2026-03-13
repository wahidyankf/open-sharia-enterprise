# Delivery Checklist: Demo Frontend Apps

## Phase 1: Project Scaffolding — demo-fe-ts-nextjs

- [ ] Create `apps/demo-fe-ts-nextjs/` directory structure
- [ ] Initialize Next.js 16 with App Router (`next.config.ts`, `tsconfig.json`)
- [ ] Set up `package.json` with React 19, Next.js 16, React Query v5 dependencies
- [ ] Configure API proxy rewrites in `next.config.ts` (forward `/api/*`, `/health`, `/.well-known/*` to `BACKEND_URL`)
- [ ] Create root layout with `QueryClientProvider`
- [ ] Create `project.json` with all Nx targets (`dev`, `build`, `start`, `typecheck`, `lint`, `test:quick`, `test:unit`)
- [ ] Verify `nx dev demo-fe-ts-nextjs` starts on port 3301

## Phase 2: Project Scaffolding — demo-fe-ts-tanstackstart

- [ ] Create `apps/demo-fe-ts-tanstackstart/` directory structure
- [ ] Initialize TanStack Start v1 RC (`app.config.ts`, `tsconfig.json`)
- [ ] Set up `package.json` with React 19, `@tanstack/react-start`, `@tanstack/react-router`, React Query v5
- [ ] Configure API proxy (server functions or Nitro proxy)
- [ ] Create root layout with `QueryClientProvider` and TanStack Router
- [ ] Create `app/client.tsx`, `app/ssr.tsx`, `app/router.tsx` entry points
- [ ] Create `project.json` with all Nx targets
- [ ] Verify `nx dev demo-fe-ts-tanstackstart` starts on port 3301

## Phase 3: Project Scaffolding — demo-fe-ts-remix

- [ ] Create `apps/demo-fe-ts-remix/` directory structure
- [ ] Initialize React Router v7 framework mode (`react-router.config.ts`, `tsconfig.json`)
- [ ] Set up `package.json` with React 19, `react-router`, `@react-router/dev`, `@react-router/serve`, React Query v5
- [ ] Configure Vite proxy in `vite.config.ts` (forward `/api/*`, `/health`, `/.well-known/*` to `BACKEND_URL`)
- [ ] Create root layout (`app/root.tsx`) with `QueryClientProvider`
- [ ] Create file-based routes in `app/routes/` with loader/action pattern
- [ ] Create `project.json` with all Nx targets (`dev`, `build`, `start`, `typecheck`, `lint`, `test:quick`, `test:unit`)
- [ ] Verify `nx dev demo-fe-ts-remix` starts on port 3301

## Phase 4: Project Scaffolding — demo-fe-dart-flutter

- [ ] Create `apps/demo-fe-dart-flutter/` directory structure
- [ ] Initialize Flutter web project (`pubspec.yaml`, `analysis_options.yaml`)
- [ ] Configure web-first rendering (CanvasKit/Skwasm in `web/index.html`)
- [ ] Set up dependencies: Riverpod 3.0, dio, go_router, bdd_widget_test
- [ ] Create `lib/main.dart` with `ProviderScope` root and `MaterialApp.router`
- [ ] Create `project.json` with all Nx targets (`dev`, `build`, `start`, `lint`, `test:quick`, `test:unit`)
- [ ] Verify `nx dev demo-fe-dart-flutter` starts Chrome on port 3301

## Phase 5: Project Scaffolding — demo-fe-elixir-phoenix

- [ ] Create `apps/demo-fe-elixir-phoenix/` directory structure
- [ ] Initialize Phoenix 1.8 project with LiveView (`mix.exs`, `config/`)
- [ ] Set up dependencies: Phoenix LiveView 1.1, Req, Cabbage, excoveralls
- [ ] Configure endpoint, router, and LiveView socket (`endpoint.ex`, `router.ex`)
- [ ] Create root layout with navigation (`root.html.heex`, `app.html.heex`)
- [ ] Configure Req HTTP client module for backend API calls
- [ ] Create `project.json` with all Nx targets (`dev`, `build`, `start`, `lint`, `test:quick`, `test:unit`)
- [ ] Verify `nx dev demo-fe-elixir-phoenix` starts on port 3301

## Phase 6: Project Scaffolding — demo-fe-e2e

- [ ] Create `apps/demo-fe-e2e/` directory structure
- [ ] Set up `package.json` with Playwright, playwright-bdd v8 dependencies
- [ ] Create `playwright.config.ts` with `defineBddConfig` pointing to `specs/apps/demo/fe/gherkin/`
- [ ] Create `tsconfig.json`
- [ ] Create `project.json` with all Nx targets (`lint`, `typecheck`, `test:quick`, `test:e2e`, `test:e2e:ui`, `test:e2e:report`)
- [ ] Create `.gitignore` (`.features-gen/`, `test-results/`, `playwright-report/`)
- [ ] Verify `npx bddgen` generates `.features-gen/` from FE specs
- [ ] Verify `nx run demo-fe-e2e:test:quick` passes (lint + typecheck)

## Phase 7: Shared API Client & React Query Hooks (TypeScript)

- [ ] Implement `lib/api/client.ts` — base fetch wrapper with auth headers
- [ ] Implement `lib/api/auth.ts` — register, login, logout, refresh, logout-all
- [ ] Implement `lib/api/users.ts` — get/update profile, change password, deactivate
- [ ] Implement `lib/api/admin.ts` — list users, disable/enable/unlock, force password reset
- [ ] Implement `lib/api/expenses.ts` — CRUD, summary, attachments
- [ ] Implement `lib/api/tokens.ts` — claims, JWKS
- [ ] Implement `lib/queries/use-auth.ts` — useLogin, useRegister, useLogout, useRefresh
- [ ] Implement `lib/queries/use-user.ts` — useCurrentUser, useUpdateProfile, useChangePassword
- [ ] Implement `lib/queries/use-admin.ts` — useAdminUsers, useDisableUser, useEnableUser, useUnlockUser
- [ ] Implement `lib/queries/use-expenses.ts` — useExpenses, useCreateExpense, useUpdateExpense, useDeleteExpense, useSummary, useAttachments
- [ ] Implement `lib/queries/use-tokens.ts` — useTokenClaims
- [ ] Copy API client + query hooks to all three TypeScript frontends (initially duplicated, not shared lib)

## Phase 8: Auth Infrastructure (TypeScript)

- [ ] Implement auth provider/context (token storage, refresh logic)
- [ ] Implement auth guard/protected route wrapper
- [ ] Implement automatic token refresh on 401 responses
- [ ] Implement logout (clear tokens, invalidate queries)
- [ ] Wire auth into Next.js layout, TanStack Start root layout, and React Router root layout

## Phase 9: Flutter API Client & Riverpod Providers

- [ ] Implement `lib/api/dio_client.dart` — dio instance with base URL, auth interceptor
- [ ] Implement `lib/api/auth_api.dart` — register, login, logout, refresh, logout-all
- [ ] Implement `lib/api/users_api.dart` — get/update profile, change password, deactivate
- [ ] Implement `lib/api/admin_api.dart` — list users, disable/enable/unlock, force password reset
- [ ] Implement `lib/api/expenses_api.dart` — CRUD, summary, attachments
- [ ] Implement `lib/api/tokens_api.dart` — claims, JWKS
- [ ] Implement `lib/providers/auth_provider.dart` — auth state + token refresh via dio interceptor
- [ ] Implement `lib/providers/user_provider.dart` — current user, profile updates
- [ ] Implement `lib/providers/admin_provider.dart` — admin user listing and actions
- [ ] Implement `lib/providers/expense_provider.dart` — expense CRUD + summary + attachments
- [ ] Implement `lib/providers/token_provider.dart` — token claims

## Phase 10: Phoenix LiveView API Client & State

- [ ] Implement `lib/demo_fe_exph/api/client.ex` — Req wrapper with auth headers
- [ ] Implement `lib/demo_fe_exph/api/auth.ex` — register, login, logout, refresh
- [ ] Implement `lib/demo_fe_exph/api/users.ex` — profile, password, deactivate
- [ ] Implement `lib/demo_fe_exph/api/admin.ex` — user management
- [ ] Implement `lib/demo_fe_exph/api/expenses.ex` — CRUD, summary, attachments
- [ ] Implement `lib/demo_fe_exph/api/tokens.ex` — claims, JWKS
- [ ] Implement auth plug for session management (JWT in session, refresh via `handle_info/2`)
- [ ] Implement protected route plug (redirect to login if unauthenticated)

## Phase 11: Pages — Health & Authentication (Next.js)

- [ ] Health status page (`/`) — display backend health
- [ ] Login page (`/login`) — email/password form, error display
- [ ] Registration page (`/register`) — form with password complexity validation
- [ ] Session management — token refresh indicator, multi-device logout

## Phase 12: Pages — User Lifecycle & Security (Next.js)

- [ ] Profile page (`/profile`) — display name edit, password change, self-deactivation
- [ ] Account lockout display — show locked status and admin unlock flow
- [ ] Password complexity — real-time validation in registration and password change forms

## Phase 13: Pages — Admin & Token Management (Next.js)

- [ ] Admin panel (`/admin`) — user listing with search, pagination
- [ ] Admin actions — disable/enable accounts, password reset token generation, unlock
- [ ] Token info page (`/tokens`) — session info display, token verification

## Phase 14: Pages — Expenses (Next.js)

- [ ] Expense list page (`/expenses`) — CRUD UI with create/edit forms
- [ ] Expense detail page (`/expenses/[id]`) — view, edit, delete
- [ ] Currency precision handling — correct decimal formatting per currency
- [ ] Unit-of-measure support in expense forms
- [ ] File attachment upload/download/delete on expense detail
- [ ] P&L reporting page (`/expenses/summary`) — summary by currency
- [ ] Optimistic updates for expense CRUD via React Query

## Phase 15: Pages — Layout & Accessibility (Next.js)

- [ ] Responsive layout — header, navigation, sidebar, footer
- [ ] Mobile viewport (< 768px) — hamburger menu, stacked layout
- [ ] Tablet viewport (768-1024px) — adapted layout
- [ ] Desktop viewport (> 1024px) — full sidebar layout
- [ ] WCAG AA — keyboard navigation, focus indicators, screen reader labels
- [ ] Color contrast compliance

## Phase 16: Pages — TanStack Start (Mirror Next.js)

- [ ] Port all pages from Next.js to TanStack Start routes
- [ ] Adapt routing to TanStack Router file-based conventions (`$id.tsx` for dynamic params)
- [ ] Use TanStack Router loaders for data prefetching where applicable
- [ ] Verify all routes are type-safe (TanStack Router type inference)
- [ ] Verify parity with Next.js implementation

## Phase 17: Pages — React Router / Remix (Mirror Next.js)

- [ ] Port all pages from Next.js to React Router v7 file-based routes (`app/routes/`)
- [ ] Adapt routing to React Router conventions (`_index.tsx`, `$id.tsx` for dynamic params)
- [ ] Use React Router loaders for server-side data fetching
- [ ] Use React Router actions for form mutations (login, register, expense CRUD)
- [ ] Configure `QueryClientProvider` in `app/root.tsx`
- [ ] Verify Vite proxy forwards API calls to backend on port 8201
- [ ] Verify parity with Next.js implementation

## Phase 18: Pages — Flutter Web (Mirror Next.js)

- [ ] Implement health screen — display backend health status
- [ ] Implement login screen — email/password form, error display
- [ ] Implement registration screen — form with password complexity validation
- [ ] Implement profile screen — display name edit, password change, self-deactivation
- [ ] Implement admin screen — user listing with search, disable/enable/unlock actions
- [ ] Implement token info screen — session info display
- [ ] Implement expense list screen — CRUD UI with create/edit dialogs
- [ ] Implement expense detail screen — view, edit, delete, attachments
- [ ] Implement expense summary screen — P&L reporting by currency
- [ ] Implement responsive layout — adaptive for mobile/tablet/desktop viewports
- [ ] Configure go_router routes with auth guard (redirect to login if unauthenticated)
- [ ] Verify parity with Next.js implementation

## Phase 19: Pages — Phoenix LiveView (Mirror Next.js)

- [ ] Implement `HealthLive` — display backend health status
- [ ] Implement `LoginLive` — email/password form, error flash messages
- [ ] Implement `RegisterLive` — form with password complexity validation
- [ ] Implement `ProfileLive` — display name edit, password change, self-deactivation
- [ ] Implement `AdminLive` — user listing with search, disable/enable/unlock actions
- [ ] Implement `TokensLive` — session info display
- [ ] Implement `ExpenseListLive` — CRUD UI with modal forms
- [ ] Implement `ExpenseDetailLive` — view, edit, delete, file attachments
- [ ] Implement `ExpenseSummaryLive` — P&L reporting by currency
- [ ] Implement responsive layout — Tailwind CSS responsive classes, mobile nav
- [ ] Configure LiveView router with auth pipeline (redirect to login if unauthenticated)
- [ ] Verify parity with Next.js implementation (all pages server-rendered via WebSocket diffs)

## Phase 20: Unit Tests — Next.js (@amiceli/vitest-cucumber + Vitest)

- [ ] Set up Vitest + @amiceli/vitest-cucumber adapter for running Gherkin scenarios
- [ ] Configure `vitest.config.ts` with unit project pointing to step definitions
- [ ] Implement step definitions for health domain (2 scenarios)
- [ ] Implement step definitions for authentication domain (12 scenarios)
- [ ] Implement step definitions for user-lifecycle domain (12 scenarios)
- [ ] Implement step definitions for security domain (5 scenarios)
- [ ] Implement step definitions for token-management domain (6 scenarios)
- [ ] Implement step definitions for admin domain (6 scenarios)
- [ ] Implement step definitions for expenses domain (33 scenarios)
- [ ] Implement step definitions for layout domain (16 scenarios)
- [ ] Verify all 92 scenarios pass
- [ ] Configure v8 coverage with LCOV output
- [ ] Verify `rhino-cli test-coverage validate` passes at >= 90%
- [ ] Verify `rhino-cli spec-coverage validate` passes

## Phase 21: Unit Tests — TanStack Start (@amiceli/vitest-cucumber + Vitest)

- [ ] Set up Vitest + @amiceli/vitest-cucumber adapter (same pattern as Next.js)
- [ ] Port/adapt step definitions from Next.js (mostly identical, different component imports)
- [ ] Verify all 92 scenarios pass
- [ ] Verify coverage >= 90%
- [ ] Verify spec-coverage passes

## Phase 22: Unit Tests — React Router / Remix (@amiceli/vitest-cucumber + Vitest)

- [ ] Set up Vitest + @amiceli/vitest-cucumber adapter (same pattern as Next.js)
- [ ] Configure `vitest.config.ts` with unit project pointing to step definitions
- [ ] Port/adapt step definitions from Next.js (mostly identical, different component imports)
- [ ] Verify all 92 scenarios pass
- [ ] Configure v8 coverage with LCOV output
- [ ] Verify coverage >= 90%
- [ ] Verify spec-coverage passes

## Phase 23: Unit Tests — Flutter (bdd_widget_test)

- [ ] Set up bdd_widget_test to consume `specs/apps/demo/fe/gherkin/` feature files
- [ ] Configure `test/` directory with step definitions organized by domain
- [ ] Implement step definitions for health domain (2 scenarios)
- [ ] Implement step definitions for authentication domain (12 scenarios)
- [ ] Implement step definitions for user-lifecycle domain (12 scenarios)
- [ ] Implement step definitions for security domain (5 scenarios)
- [ ] Implement step definitions for token-management domain (6 scenarios)
- [ ] Implement step definitions for admin domain (6 scenarios)
- [ ] Implement step definitions for expenses domain (33 scenarios)
- [ ] Implement step definitions for layout domain (16 scenarios)
- [ ] Verify all 92 scenarios pass via `flutter test`
- [ ] Configure `flutter test --coverage` for LCOV output
- [ ] Verify `rhino-cli test-coverage validate` passes at >= 90%
- [ ] Verify `rhino-cli spec-coverage validate` passes

## Phase 24: Unit Tests — Phoenix LiveView (Cabbage + LiveViewTest)

- [ ] Set up Cabbage to consume `specs/apps/demo/fe/gherkin/` feature files
- [ ] Configure `test/` directory with step definitions organized by domain
- [ ] Configure `test_load_filters` in `mix.exs` for `*_steps.exs` files (Elixir 1.19)
- [ ] Implement step definitions for health domain (2 scenarios)
- [ ] Implement step definitions for authentication domain (12 scenarios)
- [ ] Implement step definitions for user-lifecycle domain (12 scenarios)
- [ ] Implement step definitions for security domain (5 scenarios)
- [ ] Implement step definitions for token-management domain (6 scenarios)
- [ ] Implement step definitions for admin domain (6 scenarios)
- [ ] Implement step definitions for expenses domain (33 scenarios)
- [ ] Implement step definitions for layout domain (16 scenarios)
- [ ] Verify all 92 scenarios pass via `mix test`
- [ ] Configure excoveralls with LCOV output
- [ ] Verify `rhino-cli test-coverage validate` passes at >= 90%
- [ ] Verify `rhino-cli spec-coverage validate` passes

## Phase 25: Test-Only Backend API

- [ ] Create `specs/apps/demo/be/gherkin/test-support/test-api.feature` — Gherkin spec for test-only endpoints
- [ ] Update `specs/apps/demo/be/README.md` — document test-support domain
- [ ] Implement test-only controller in `demo-be-java-springboot` — `POST /api/v1/test/reset-db` and `POST /api/v1/test/promote-admin`
- [ ] Gate test controller behind `ENABLE_TEST_API=true` environment variable (not registered in production)
- [ ] Verify test API works: reset-db clears all tables, promote-admin sets user role to ADMIN
- [ ] Update `demo-be-java-springboot` unit/integration tests to cover test API controller

## Phase 26: E2E Step Definitions (demo-fe-e2e)

- [ ] Create `tests/utils/token-store.ts` — JWT token state management
- [ ] Create `tests/utils/response-store.ts` — response state for assertions
- [ ] Create `tests/utils/page-helpers.ts` — common page interaction helpers
- [ ] Create `tests/fixtures/test-api.ts` — test API client (reset-db, promote-admin via HTTP)
- [ ] Create `tests/hooks/cleanup.hooks.ts` — calls `POST /api/v1/test/reset-db` between scenarios
- [ ] Implement `tests/steps/common.steps.ts` — shared background steps
- [ ] Implement `tests/steps/common-setup.steps.ts` — auth setup helpers (using public APIs + test API)
- [ ] Implement step definitions for health domain (2 scenarios)
- [ ] Implement step definitions for authentication domain (12 scenarios)
- [ ] Implement step definitions for user-lifecycle domain (12 scenarios)
- [ ] Implement step definitions for security domain (5 scenarios)
- [ ] Implement step definitions for token-management domain (6 scenarios)
- [ ] Implement step definitions for admin domain (6 scenarios)
- [ ] Implement step definitions for expenses domain (33 scenarios)
- [ ] Implement step definitions for layout domain (16 scenarios)
- [ ] Verify all 92 E2E scenarios pass against Next.js frontend
- [ ] Verify all 92 E2E scenarios pass against TanStack Start frontend
- [ ] Verify all 92 E2E scenarios pass against React Router (Remix) frontend
- [ ] Verify all 92 E2E scenarios pass against Flutter Web frontend
- [ ] Verify all 92 E2E scenarios pass against Phoenix LiveView frontend

## Phase 27: Docker Compose & Dockerfiles

- [ ] Create `apps/demo-fe-ts-nextjs/Dockerfile` — multi-stage Next.js build
- [ ] Create `apps/demo-fe-ts-tanstackstart/Dockerfile` — multi-stage TanStack Start build
- [ ] Create `apps/demo-fe-ts-remix/Dockerfile` — multi-stage React Router build
- [ ] Create `apps/demo-fe-dart-flutter/Dockerfile` — multi-stage Flutter web build (dart:3.11 + nginx)
- [ ] Create `apps/demo-fe-elixir-phoenix/Dockerfile` — multi-stage Elixir release build
- [ ] Create `infra/dev/demo-fe-ts-nextjs/docker-compose.yml` — FE + BE + DB
- [ ] Create `infra/dev/demo-fe-ts-nextjs/.env.example`
- [ ] Create `infra/dev/demo-fe-ts-tanstackstart/docker-compose.yml` — FE + BE + DB
- [ ] Create `infra/dev/demo-fe-ts-tanstackstart/.env.example`
- [ ] Create `infra/dev/demo-fe-ts-remix/docker-compose.yml` — FE + BE + DB
- [ ] Create `infra/dev/demo-fe-ts-remix/.env.example`
- [ ] Create `infra/dev/demo-fe-dart-flutter/docker-compose.yml` — FE + BE + DB
- [ ] Create `infra/dev/demo-fe-dart-flutter/.env.example`
- [ ] Create `infra/dev/demo-fe-elixir-phoenix/docker-compose.yml` — FE + BE + DB
- [ ] Create `infra/dev/demo-fe-elixir-phoenix/.env.example`
- [ ] Set `ENABLE_TEST_API=true` on backend service in all Docker Compose files
- [ ] Verify Docker Compose starts all services for Next.js variant
- [ ] Verify Docker Compose starts all services for TanStack Start variant
- [ ] Verify Docker Compose starts all services for React Router (Remix) variant
- [ ] Verify Docker Compose starts all services for Flutter variant
- [ ] Verify Docker Compose starts all services for Phoenix variant

## Phase 28: GitHub Actions Workflows

- [ ] Create `.github/workflows/e2e-demo-fe-ts-nextjs.yml` — E2E CI workflow
- [ ] Create `.github/workflows/e2e-demo-fe-ts-tanstackstart.yml` — E2E CI workflow
- [ ] Create `.github/workflows/e2e-demo-fe-ts-remix.yml` — E2E CI workflow
- [ ] Create `.github/workflows/e2e-demo-fe-dart-flutter.yml` — E2E CI workflow
- [ ] Create `.github/workflows/e2e-demo-fe-elixir-phoenix.yml` — E2E CI workflow
- [ ] Update `.github/workflows/main-ci.yml` — add coverage upload steps for all five frontends
- [ ] Verify E2E workflow runs successfully in CI for Next.js
- [ ] Verify E2E workflow runs successfully in CI for TanStack Start
- [ ] Verify E2E workflow runs successfully in CI for React Router (Remix)
- [ ] Verify E2E workflow runs successfully in CI for Flutter
- [ ] Verify E2E workflow runs successfully in CI for Phoenix

## Phase 29: Governance Rules Update (via `repo-governance-maker`)

- [ ] Update `governance/development/quality/three-level-testing-standard.md` — add "Demo-fe frontend" row to Applicability table: unit + E2E only (no `test:integration`), >=90% line coverage, Gherkin specs from `specs/apps/demo/fe/gherkin/`
- [ ] Update `governance/development/infra/nx-targets.md` — add demo-fe target definitions (no `test:integration` target for demo-fe apps)
- [ ] Update `governance/development/infra/github-actions-workflow-naming.md` — add all five new workflow name/filename pairs to the reference table (`E2E - demo-fe-ts-nextjs`, `E2E - demo-fe-ts-tanstackstart`, `E2E - demo-fe-ts-remix`, `E2E - demo-fe-dart-flutter`, `E2E - demo-fe-elixir-phoenix`)
- [ ] Verify governance docs are consistent: demo-fe apps use two-level testing (unit + E2E), same >=90% coverage threshold as demo-be apps

## Phase 30: Documentation & Monorepo Updates

- [ ] Create `apps/demo-fe-ts-nextjs/README.md`
- [ ] Create `apps/demo-fe-ts-tanstackstart/README.md`
- [ ] Create `apps/demo-fe-ts-remix/README.md`
- [ ] Create `apps/demo-fe-dart-flutter/README.md`
- [ ] Create `apps/demo-fe-elixir-phoenix/README.md`
- [ ] Create `apps/demo-fe-e2e/README.md`
- [ ] Update `CLAUDE.md` — add demo-fe apps to Current Apps list and coverage notes
- [ ] Update root `README.md` — add demo-fe apps to project listing
- [ ] Update `specs/apps/demo/fe/README.md` — add all five implementation rows
- [ ] Update `.dockerignore` if needed for FE specs

## Phase 31: Final Validation

- [ ] `nx run demo-fe-ts-nextjs:test:quick` passes (lint, typecheck, unit tests, coverage, spec-coverage)
- [ ] `nx run demo-fe-ts-tanstackstart:test:quick` passes
- [ ] `nx run demo-fe-ts-remix:test:quick` passes (lint, typecheck, unit tests, coverage, spec-coverage)
- [ ] `nx run demo-fe-dart-flutter:test:quick` passes (lint, unit tests, coverage, spec-coverage)
- [ ] `nx run demo-fe-elixir-phoenix:test:quick` passes (lint, unit tests, coverage, spec-coverage)
- [ ] `nx run demo-fe-e2e:test:quick` passes (lint, typecheck)
- [ ] `nx affected -t test:quick` passes with all six new apps
- [ ] E2E passes against Next.js frontend (`BASE_URL=http://localhost:3301`)
- [ ] E2E passes against TanStack Start frontend (`BASE_URL=http://localhost:3301`)
- [ ] E2E passes against React Router (Remix) frontend (`BASE_URL=http://localhost:3301`)
- [ ] E2E passes against Flutter Web frontend (`BASE_URL=http://localhost:3301`)
- [ ] E2E passes against Phoenix LiveView frontend (`BASE_URL=http://localhost:3301`)
- [ ] Pre-push hooks pass
- [ ] CI workflows trigger and complete successfully
- [ ] No regressions in existing apps

## Completion Criteria

All phases checked off. All 92 Gherkin scenarios pass at both unit and E2E levels for all five
frontends. CI green. Coverage >= 90% for all five frontends.
