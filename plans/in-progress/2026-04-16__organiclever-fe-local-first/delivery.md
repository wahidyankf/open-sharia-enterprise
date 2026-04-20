# Delivery — OrganicLever FE Local-First Mode

See [`README.md`](./README.md) for overview, [`brd.md`](./brd.md) for business intent, [`prd.md`](./prd.md) for R1–R7 and Gherkin acceptance criteria, and [`tech-docs.md`](./tech-docs.md) for architecture and implementation details.

## Phase 0 — Environment Setup

- [ ] Install dependencies from repo root: `npm install`
- [ ] Converge polyglot toolchain: `npm run doctor -- --fix`
- [ ] Verify dev server starts: `nx dev organiclever-fe`
- [ ] Set `ORGANICLEVER_BE_URL` in local `.env.local` if needed for development
- [ ] Confirm existing tests pass before making changes: `nx run organiclever-fe:test:quick`

## Phase 1 — Routes and Middleware

- [ ] Delete `apps/organiclever-fe/src/app/login/`
- [ ] Delete `apps/organiclever-fe/src/app/profile/`
- [ ] Delete `apps/organiclever-fe/src/app/api/auth/`
- [ ] Delete `apps/organiclever-fe/src/proxy.ts`
- [ ] Delete `apps/organiclever-fe/test/unit/steps/authentication/google-login.steps.tsx` (imports deleted `@/app/login/page` — must be removed before typecheck in Phase 3)
- [ ] Delete `apps/organiclever-fe/test/unit/steps/authentication/profile.steps.tsx` (imports deleted `@/app/profile/page` — must be removed before typecheck in Phase 3)
- [ ] Delete `apps/organiclever-fe/test/unit/steps/authentication/route-protection.steps.tsx` (imports deleted routes — must be removed before typecheck in Phase 3)
- [ ] Delete `apps/organiclever-fe-e2e/steps/google-login.steps.ts`
- [ ] Delete `apps/organiclever-fe-e2e/steps/profile.steps.ts`
- [ ] Delete `apps/organiclever-fe-e2e/steps/route-protection.steps.ts`
- [ ] Remove stale coverage exclusions from `apps/organiclever-fe/vitest.config.ts` `exclude` array: delete the `"src/proxy.ts"` and `"src/app/api/**"` entries (they will match nothing after the deletions above, but clean them up explicitly)
- [ ] Delete `apps/organiclever-fe/src/components/profile-card.tsx` if unreferenced after route removals
- [ ] Delete `apps/organiclever-fe/src/components/profile-card.stories.tsx` when deleting `profile-card.tsx`
- [ ] Rewrite `apps/organiclever-fe/src/app/page.tsx` as a static landing server component
  - Landing page must include at least: an h1 heading (the landing-page heading BRD success metric 1 references), a tagline, and a link to `/system/status/be`

## Phase 2 — Diagnostic Page

- [ ] Create `apps/organiclever-fe/src/app/system/status/be/page.tsx` with `force-dynamic` export
- [ ] Implement `probeBackend()` helper returning `unset | up | down` variants
- [ ] Wire `AbortSignal.timeout(3000)` into the `fetch` call
- [ ] Render UP view (URL, latency, body)
- [ ] Render DOWN view (URL, reason)
- [ ] Render Not-Configured view
- [ ] Verify no `throw` path reaches the page boundary

## Phase 3 — Dormant Code Guardrails

- [ ] Confirm `src/services/` and `src/layers/` compile standalone
- [ ] Confirm `src/lib/auth/` files are untouched (e.g., `src/lib/auth/cookies.ts` must still be present)
- [ ] Verify named BE files still present: `ls apps/organiclever-fe/src/services/auth-service.ts apps/organiclever-fe/src/services/backend-client.ts apps/organiclever-fe/src/services/errors.ts apps/organiclever-fe/src/layers/backend-client-live.ts` (BRD success metric 6)
- [ ] Run `nx run organiclever-fe:lint`
- [ ] Note any unused-export warnings from services/layers in the lint output
- [ ] If lint reports unused exports, add `src/services/index.ts` re-exporting `AuthService`, `BackendClient`, `NetworkError`
- [ ] Re-run `nx run organiclever-fe:lint` — must pass with zero errors
- [ ] Run `nx run organiclever-fe:typecheck`

## Phase 4 — Specs and Tests

- [ ] Delete `specs/apps/organiclever/fe/gherkin/authentication/google-login.feature`
- [ ] Delete `specs/apps/organiclever/fe/gherkin/authentication/profile.feature`
- [ ] Delete `specs/apps/organiclever/fe/gherkin/authentication/route-protection.feature`
- [ ] Add `specs/apps/organiclever/fe/gherkin/system/system-status-be.feature` covering the four `/system/status/be` scenarios
- [ ] Add unit step file `apps/organiclever-fe/test/unit/steps/system/system-status-be.steps.tsx`
- [ ] Add `specs/apps/organiclever/fe/gherkin/landing/landing.feature` covering the "Root renders landing without BE" scenario
- [ ] Add unit step file `apps/organiclever-fe/test/unit/steps/landing/landing.steps.tsx`
- [ ] Add e2e step file `apps/organiclever-fe-e2e/steps/landing.steps.ts`
- [ ] Add `specs/apps/organiclever/fe/gherkin/routing/disabled-routes.feature` covering the "Disabled routes return 404" Scenario Outline
- [ ] Add unit step file `apps/organiclever-fe/test/unit/steps/routing/disabled-routes.steps.tsx`
- [ ] Add e2e step file `apps/organiclever-fe-e2e/steps/disabled-routes.steps.ts`
- [ ] Add `apps/organiclever-fe-e2e/steps/system-status-be.steps.ts` (playwright-bdd step definitions)
- [ ] Run `nx run organiclever-fe:test:quick` — command must exit 0 (includes ≥70% coverage gate)
- [ ] Run `nx run organiclever-fe:test:integration`
  > Expected to pass with zero tests because `passWithNoTests: true` is set globally in `vitest.config.ts`. No `test/integration/` directory exists.
- [ ] If `test:integration` fails (global `passWithNoTests` may not propagate to individual Vitest project configs): add `passWithNoTests: true` inside the integration project config in `vitest.config.ts` and re-run
- [ ] Run `nx run organiclever-fe:spec-coverage`
- [ ] Run `nx run organiclever-fe-e2e:test:e2e` (against local docker-compose)

## Phase 5 — Documentation

- [ ] Rewrite Architecture section in `apps/organiclever-fe/README.md` for local-first mode
- [ ] Document `/system/status/be` failure modes and env var in the README
- [ ] Note dormant BE integration code in the README
- [ ] Remove `NEXT_PUBLIC_GOOGLE_CLIENT_ID` from the Environment Variables table in `apps/organiclever-fe/README.md` (only `ORGANICLEVER_BE_URL` should remain, marked as optional)
- [ ] Update `CLAUDE.md` organiclever-fe description if it mentions BFF (current description does not mention BFF — verify no update needed)
- [ ] Update CLAUDE.md coverage thresholds table — change `organiclever-fe` Notes column from `"fe threshold: API/auth layers fully mocked by design"` to `"dormant BE integration code (services/, layers/) excluded from coverage measurement"`
- [ ] Update CLAUDE.md `test:integration` caching list — remove `"organiclever-fe (MSW)"` and replace with `"organiclever-fe (no integration tests; cache: true with passWithNoTests prevents unnecessary re-runs)"`
- [ ] Remove `"Cookie-based authentication"` from the Features list in `docs/reference/system-architecture/applications.md` organiclever-fe section
- [ ] Verify whether `"JSON data files for content"` still applies to the landing-page-only surface; remove it from the Features list if not applicable
- [ ] Grep `docs/` and `governance/` for BFF references: `grep -rn "BFF" docs/ governance/`
- [ ] Grep `docs/` and `governance/` for `/api/auth` references: `grep -rn "/api/auth" docs/ governance/`
- [ ] Update or remove any BFF or `/api/auth` references found

## Local Quality Gates (Before Push)

- [ ] Run affected typecheck: `nx affected -t typecheck`
- [ ] Run affected linting: `nx affected -t lint`
- [ ] Run affected quick tests: `nx affected -t test:quick`
- [ ] Run affected spec coverage: `nx affected -t spec-coverage`
- [ ] Fix ALL failures found — including preexisting issues not caused by your changes
- [ ] Verify all checks pass before pushing

> **Important**: Fix ALL failures found during quality gates, not just those caused by your
> changes. This follows the root cause orientation principle — proactively fix preexisting
> errors encountered during work.

## Commit Guidelines

- [ ] Commit changes thematically — group related changes into logically cohesive commits
- [ ] Follow Conventional Commits format: `<type>(<scope>): <description>`
- [ ] Split different domains/concerns into separate commits (e.g., routes in one commit, tests in another, docs in another)
- [ ] Do NOT bundle unrelated fixes into a single commit

## Manual UI Verification (Playwright MCP — Local)

- [ ] Start dev server: `nx dev organiclever-fe`
- [ ] Navigate to landing page via `browser_navigate http://localhost:3200/`
- [ ] Inspect landing page via `browser_snapshot` — verify heading renders, no redirect occurs
- [ ] Check for JS errors via `browser_console_messages` — must be zero errors
- [ ] Verify no unexpected network requests from landing page via `browser_network_requests` — `/` must make zero outbound fetch calls to organiclever-be
- [ ] Navigate to status page: `browser_navigate http://localhost:3200/system/status/be`
- [ ] Verify "Not configured" renders via `browser_snapshot`
- [ ] Check for JS errors via `browser_console_messages` — must be zero errors
- [ ] Verify no unexpected network requests via `browser_network_requests` — when `ORGANICLEVER_BE_URL` is unset, `/system/status/be` must make zero outbound fetch calls
- [ ] Take screenshots via `browser_take_screenshot` for visual record
- [ ] Verify `/login` returns 404: `browser_navigate http://localhost:3200/login`
- [ ] Verify `/profile` returns 404: `browser_navigate http://localhost:3200/profile`
- [ ] Verify disabled API routes return 404 (curl):
  - [ ] `curl -so /dev/null -w "%{http_code}" http://localhost:3200/api/auth/google` → 404
  - [ ] `curl -so /dev/null -w "%{http_code}" http://localhost:3200/api/auth/refresh` → 404
  - [ ] `curl -so /dev/null -w "%{http_code}" http://localhost:3200/api/auth/me` → 404

## Post-Push CI Verification

> **Important**: `test-and-deploy-organiclever.yml` is a **scheduled** workflow (runs at
> 06:00 WIB and 18:00 WIB daily) with `workflow_dispatch` support. It does **not** fire
> automatically on push to `main`. After pushing, manually trigger it from the GitHub
> Actions tab using `workflow_dispatch`.

- [ ] Push changes to `main`
- [ ] Trigger `test-and-deploy-organiclever.yml` manually via GitHub Actions `workflow_dispatch`
- [ ] Monitor the triggered run in GitHub Actions
- [ ] Verify all CI jobs pass in this dependency order: `spec-coverage`, `fe-lint`, `be-integration`, `fe-integration` → `e2e` → `detect-changes` → `deploy`

  > **Note**: The `spec-coverage` CI job covers all four OrganicLever projects (`organiclever-be`, `organiclever-fe`, `organiclever-be-e2e`, `organiclever-fe-e2e`). A failure in any of them blocks the deploy chain. If `organiclever-be` spec-coverage fails for reasons unrelated to this plan, fix it or confirm it was already failing before this work.

- [ ] If any CI job fails: diagnose and fix the root cause
- [ ] Push the fix as a follow-up commit to `main`
- [ ] Re-trigger `test-and-deploy-organiclever.yml` via `workflow_dispatch`
- [ ] Do NOT proceed to Vercel verification until the `deploy` job completes green

## Phase 6 — Manual Verification on Vercel

- [ ] Confirm CI workflow's `deploy` job ran and force-pushed to `prod-organiclever-web` (the workflow runs `git push origin HEAD:prod-organiclever-web --force` automatically; users do NOT push to this branch directly)
- [ ] Confirm Vercel build succeeds with `ORGANICLEVER_BE_URL` unset
- [ ] Visit https://www.organiclever.com/ — landing renders
- [ ] `curl -sS https://www.organiclever.com/` — confirm HTTP 200 and body contains landing-page heading (BRD success metric 1)
- [ ] Visit https://www.organiclever.com/system/status/be — "Not configured" renders, HTTP 200
- [ ] `curl -sS https://www.organiclever.com/system/status/be` — confirm HTTP 200 and body contains "Not configured" (BRD success metric 3)
- [ ] Check Vercel function logs for `prod-organiclever-web` — verify no runtime errors appear for `/` or `/system/status/be` within the first few minutes of the promote (BRD success metric 5)
- [ ] Verify disabled routes return 404 on production (BRD success metric 4):
  - [ ] `curl -so /dev/null -w "%{http_code}" https://www.organiclever.com/login` → 404
  - [ ] `curl -so /dev/null -w "%{http_code}" https://www.organiclever.com/profile` → 404
  - [ ] `curl -so /dev/null -w "%{http_code}" https://www.organiclever.com/api/auth/google` → 404
  - [ ] `curl -so /dev/null -w "%{http_code}" https://www.organiclever.com/api/auth/refresh` → 404
  - [ ] `curl -so /dev/null -w "%{http_code}" https://www.organiclever.com/api/auth/me` → 404
- [ ] Set `ORGANICLEVER_BE_URL` in Vercel (`prod-organiclever-web` project) to an unreachable host (e.g., `http://nowhere.invalid`)
- [ ] Trigger a new Vercel production deploy for `prod-organiclever-web`
- [ ] Visit https://www.organiclever.com/system/status/be — verify "DOWN" renders with reason and HTTP 200
- [ ] Remove `ORGANICLEVER_BE_URL` from Vercel `prod-organiclever-web` environment settings
- [ ] Trigger another new Vercel production deploy for `prod-organiclever-web` to pick up the env var removal (the currently running build baked the unreachable URL at deploy time; removing the env var does not affect the live build)
- [ ] Confirm https://www.organiclever.com/system/status/be shows "Not configured" — production is now in the expected steady state

## Phase 7 — Close Out

- [ ] Move plan folder: `git mv plans/in-progress/2026-04-16__organiclever-fe-local-first plans/done/`
- [ ] Update `plans/in-progress/README.md` — remove plan entry
- [ ] Update `plans/done/README.md` — add plan entry with completion date
- [ ] Commit: `chore(plans): move organiclever-fe-local-first to done`
