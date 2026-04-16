# OrganicLever FE — Local-First Mode

**Status**: In Progress
**Created**: 2026-04-16
**Scope**: `apps/organiclever-fe/` (and supporting docs/specs); `apps/organiclever-be/` untouched

## Overview

Pivot `organiclever-fe` to a local-first mode so it can deploy to Vercel
(`prod-organiclever-web` → www.organiclever.com) without depending on the F#/Giraffe backend at
`apps/organiclever-be/`. The backend is not deployed yet and will be re-wired in a future phase.

For this phase, the FE runs as a standalone landing site with a single diagnostic surface:
`/system/status/be`, which probes `organiclever-be`'s `/health` endpoint and degrades gracefully
when the BE is unconfigured, unreachable, or slow — never blanking the page.

The existing BFF/auth code (Effect TS service layer + Route Handlers) is preserved as dormant
library code in `src/services/` and `src/layers/` so future rewire is a routes-only change.

## Context and Motivation

Current state (`apps/organiclever-fe/`):

- Root `/` → redirects to `/profile` or `/login` based on cookie
- `/login` → Google Identity Services → posts to `/api/auth/google` (BFF route)
- `/profile` → server component fetches profile via `AuthService` → `BackendClient` → BE
- `/api/auth/{google,refresh,me}` → Route Handlers proxying to BE
- `src/proxy.ts` — dead-code middleware helper (exports `proxy` function but is not wired as
  Next.js middleware; Next.js requires the file to be named `middleware.ts` to execute)
- `ORGANICLEVER_BE_URL` default `http://localhost:8202`

Target state:

- Root `/` → renders a landing page (no network dependency)
- `/login`, `/profile`, `/api/auth/*` — removed
- `/system/status/be` — new server-rendered diagnostic page
- BE service/layer/contract code retained, unreferenced by any route
- `ORGANICLEVER_BE_URL` becomes purely optional (runtime-only)

Motivation:

- Ship a live landing site while backend platform work is in flight
- Validate Vercel deploy pipeline end-to-end with the new workflow
  (`test-and-deploy-organiclever.yml`) on a realistic FE-only surface
- Give ops a clearly-labelled place to watch BE readiness once it deploys

## Scope

**In scope:**

- `apps/organiclever-fe/src/` route and middleware changes
- `apps/organiclever-fe/` docs (README, env)
- `specs/apps/organiclever/fe/gherkin/` additions and removals
- `apps/organiclever-fe/test/unit/` additions and removals
- `apps/organiclever-fe-e2e/` spec changes for removed/added pages
- Top-level doc touch-ups referring to organiclever-fe BFF pattern

**Out of scope (explicitly preserved):**

- `apps/organiclever-be/` source and tests
- `specs/apps/organiclever/contracts/` OpenAPI spec
- `organiclever-contracts` Nx project
- `organiclever-be-e2e` workflow — still runs against local BE in CI
- The `codegen` Nx target on `organiclever-fe` (kept for future rewire)
- `.github/workflows/test-and-deploy-organiclever.yml` (already correctly gates deploy)

## Requirements

### R1 — Disable BE-dependent user routes

Remove `/login`, `/profile`, and `/api/auth/*` from the built surface so Vercel does not expose
routes that 503 on invoke. Keep `src/services/auth-service.ts`,
`src/layers/backend-client-live.ts`, `src/services/backend-client.ts`, and `src/services/errors.ts`
as library code; their unit tests stay green.

### R2 — Landing page at `/`

Root renders a static landing page — no cookie inspection, no redirect, no BE call. Content can
be the minimum viable "OrganicLever — coming soon" card; it can be expanded later without a plan
revision.

### R3 — Diagnostic page `/system/status/be`

Server-rendered page that:

1. Reads `ORGANICLEVER_BE_URL` at request time.
2. If unset → renders "Not configured — set `ORGANICLEVER_BE_URL` to probe".
3. If set → `fetch(${url}/health)` with `AbortSignal.timeout(3000)`.
4. On success → renders `UP`, the URL, the response body, and round-trip latency.
5. On any failure (timeout, network error, non-2xx, JSON parse error) → renders `DOWN` with the
   reason. Never throws; never returns non-200 to the user.

All failures are caught at the page level. The page is marked `dynamic = 'force-dynamic'` so
Vercel never attempts to prerender it at build time.

### R4 — `ORGANICLEVER_BE_URL` is optional

Unset at build time on Vercel is allowed. Nothing in the build pipeline depends on it. At runtime
only `/system/status/be` reads it; other routes must not reference it.

### R5 — Dead-code file `src/proxy.ts` removed

`src/proxy.ts` is dead code — it exports a `proxy` function but is never imported anywhere and is
not wired as Next.js middleware (Next.js only executes files named `middleware.ts`). Remove
`src/proxy.ts` entirely. If future route protection is introduced, it will be a new `middleware.ts`
created in its own plan.

### R6 — Documentation refreshed

- `apps/organiclever-fe/README.md` — rewrite the Architecture section to describe the local-first
  mode; list `/system/status/be` and its failure modes; note BE code is dormant.
- `CLAUDE.md` — update the `organiclever-fe` description if it claims a BFF pattern.
- `docs/reference/system-architecture/applications.md` — update the organiclever-fe section to
  reflect landing-site scope; explicitly note BE integration is deferred.

### R7 — Vercel deploys green without BE

Deploying `main` to `prod-organiclever-web`:

- Build succeeds with `ORGANICLEVER_BE_URL` unset.
- `/` renders.
- `/system/status/be` renders with "Not configured" branch and HTTP 200.
- No console errors visible in Vercel's function logs for either request.

## Acceptance Criteria (Gherkin)

```gherkin
Feature: Local-first organiclever-fe

  Scenario: Root renders landing without BE
    Given ORGANICLEVER_BE_URL is unset
    When a visitor requests GET /
    Then the response status is 200
    And the body contains the landing page heading
    And no request is made to organiclever-be
    And the final response URL is /

  Scenario: BE status page shows Not Configured when env unset
    Given ORGANICLEVER_BE_URL is unset
    When a visitor requests GET /system/status/be
    Then the response status is 200
    And the body contains "Not configured"

  Scenario: BE status page shows UP when backend healthy
    Given ORGANICLEVER_BE_URL is "http://be.example.test"
    And GET http://be.example.test/health returns 200 with body {"status":"UP"}
    When a visitor requests GET /system/status/be
    Then the response status is 200
    And the body contains "UP"
    And the body contains the backend URL

  Scenario: BE status page shows DOWN when backend unreachable
    Given ORGANICLEVER_BE_URL is "http://be.example.test"
    And GET http://be.example.test/health fails with connection refused
    When a visitor requests GET /system/status/be
    Then the response status is 200
    And the body contains "DOWN"
    And the body contains the failure reason
    And no uncaught exception reaches the Next.js error boundary

  Scenario: BE status page shows DOWN when backend times out
    Given ORGANICLEVER_BE_URL is "http://be.example.test"
    And GET http://be.example.test/health does not respond within 3 seconds
    When a visitor requests GET /system/status/be
    Then the response status is 200
    And the body contains "DOWN"
    And the body contains "timeout"

  Scenario Outline: Disabled routes return 404
    When a visitor requests <method> <path>
    Then the response status is 404

    Examples:
      | method | path             |
      | GET    | /login           |
      | GET    | /profile         |
      | POST   | /api/auth/google |
```

## Technical Documentation

### Route tree after this plan

```
src/app/
├── layout.tsx
├── page.tsx                     # landing
├── globals.css
├── metadata.ts
└── system/
    └── status/
        └── be/
            └── page.tsx         # diagnostic, force-dynamic
```

### `/system/status/be` implementation sketch

```tsx
export const dynamic = "force-dynamic";

type Probe =
  | { kind: "unset" }
  | { kind: "up"; url: string; latencyMs: number; body: unknown }
  | { kind: "down"; url: string; reason: string };

async function probeBackend(): Promise<Probe> {
  const url = process.env["ORGANICLEVER_BE_URL"];
  if (!url) return { kind: "unset" };
  const started = performance.now();
  try {
    const res = await fetch(`${url}/health`, { signal: AbortSignal.timeout(3000) });
    if (!res.ok) {
      return { kind: "down", url, reason: `HTTP ${res.status}` };
    }
    const body = (await res.json().catch(() => null)) as unknown;
    return { kind: "up", url, latencyMs: Math.round(performance.now() - started), body };
  } catch (error) {
    const reason = error instanceof Error ? error.message : String(error);
    return { kind: "down", url, reason };
  }
}
```

Key details:

- `force-dynamic` disables prerender so Vercel build never fetches the BE.
- `AbortSignal.timeout(3000)` caps the SSR blocking time at 3 s; any overrun converts to a DOWN
  render.
- The page function itself has no `throw` paths — every branch returns JSX.

### Landing page

`src/app/page.tsx` becomes a minimal server component. No `cookies()` read, no `redirect()`, no
network call. Exact copy is deferred to implementation; acceptable stand-in: an h1, a tagline, and
a link to `/system/status/be` for operators.

### Dormant BE integration code

Preserve (no changes):

- `src/services/auth-service.ts`
- `src/services/backend-client.ts`
- `src/services/errors.ts`
- `src/layers/backend-client-live.ts`
- `src/layers/backend-client-test.ts`
- `src/layers/auth/` (if present)
- `src/lib/auth/cookies.ts`
- `generated-contracts/` (regenerated by `codegen`)

If lint flags unused exports from these modules, disable the rule locally or add an `index.ts`
re-export to keep them "live" from a tree-shaking perspective — do not delete. Inspect `src/lib/`
for any additional auth helpers and preserve them alongside `src/services/` and `src/layers/`.

### Middleware

`src/proxy.ts` deleted (dead code — never executed as middleware; Next.js requires `middleware.ts`).
No replacement for this phase. Next.js treats a missing middleware file as no-op — no config
change elsewhere.

### Test strategy

- **Unit (Vitest + `@amiceli/vitest-cucumber`)**: new feature file
  `specs/apps/organiclever/fe/gherkin/system/system-status-be.feature` covering the four
  `/system/status/be` scenarios. Mock `fetch` via `vi.stubGlobal`. Remove login/profile/route-protection
  feature files. Also add `specs/apps/organiclever/fe/gherkin/landing/landing.feature` for the
  root landing scenario.
- **Integration (MSW)**: if an integration harness exists for the removed routes, prune it. Add
  an MSW handler for `/health` exercising UP/DOWN paths.
- **E2E (`organiclever-fe-e2e`)**: remove login/profile/route-protection Playwright step files;
  add `apps/organiclever-fe-e2e/steps/system-status-be.steps.ts` (playwright-bdd step definitions)
  that hits `/system/status/be` with and without `ORGANICLEVER_BE_URL` via compose env override.
  The gherkin feature file under `specs/` is shared by both unit and e2e runners.

### Impact on CI

`.github/workflows/test-and-deploy-organiclever.yml` already spins up the full stack
(DB + BE + FE) via docker-compose before E2E, so `/health` is reachable there. The existing
`e2e` job continues to gate the `deploy` job unchanged. No workflow edits required for this plan.

### Impact on Vercel

- Project on Vercel: `prod-organiclever-web` branch → www.organiclever.com
- Build command: default Next.js build via Nx (`nx build organiclever-fe`)
- Env vars: none required. `ORGANICLEVER_BE_URL` may be left unset.
- Functions: `/system/status/be` runs as a dynamic Node.js server render on each request.

## Risks and Mitigations

| Risk                                                            | Mitigation                                                            |
| --------------------------------------------------------------- | --------------------------------------------------------------------- |
| Build-time SSR tries to fetch BE and fails                      | `export const dynamic = 'force-dynamic'` on the status page           |
| SSR hangs 30 s when BE host is slow, Vercel times out whole req | `AbortSignal.timeout(3000)` inside `fetch`                            |
| Removed routes leave dead imports, failing typecheck/lint       | Run `nx run organiclever-fe:typecheck` and `:lint` before commit      |
| Dormant auth code rots because nothing imports it               | Add a `src/services/index.ts` re-export so tree-shaking doesn't panic |
| BE E2E in CI suddenly breaks because FE no longer hits BE       | BE E2E runs against BE directly via `organiclever-be-e2e`, not via FE |
| Future rewire blocked by deletions                              | Keep `services/`, `layers/`, `generated-contracts/` intact            |

## Delivery Checklist

### Phase 0 — Environment Setup

- [ ] Install dependencies from repo root: `npm install`
- [ ] Converge polyglot toolchain: `npm run doctor -- --fix`
- [ ] Verify dev server starts: `nx dev organiclever-fe`
- [ ] Set `ORGANICLEVER_BE_URL` in local `.env.local` if needed for development
- [ ] Confirm existing tests pass before making changes: `nx run organiclever-fe:test:quick`

### Phase 1 — Routes and Middleware

- [ ] Delete `apps/organiclever-fe/src/app/login/`
- [ ] Delete `apps/organiclever-fe/src/app/profile/`
- [ ] Delete `apps/organiclever-fe/src/app/api/auth/`
- [ ] Delete `apps/organiclever-fe/src/proxy.ts`
- [ ] Delete `apps/organiclever-fe/src/components/profile-card.tsx` if unreferenced after route removals
- [ ] Delete `apps/organiclever-fe/src/components/profile-card.stories.tsx` when deleting `profile-card.tsx`
- [ ] Rewrite `apps/organiclever-fe/src/app/page.tsx` as a static landing server component

### Phase 2 — Diagnostic Page

- [ ] Create `apps/organiclever-fe/src/app/system/status/be/page.tsx` with `force-dynamic` export
- [ ] Implement `probeBackend()` helper returning `unset | up | down` variants
- [ ] Wire `AbortSignal.timeout(3000)` into the `fetch` call
- [ ] Render UP view (URL, latency, body)
- [ ] Render DOWN view (URL, reason)
- [ ] Render Not-Configured view
- [ ] Verify no `throw` path reaches the page boundary

### Phase 3 — Dormant Code Guardrails

- [ ] Confirm `src/services/` and `src/layers/` compile standalone
- [ ] Add `src/services/index.ts` re-exporting `AuthService`, `BackendClient`, `NetworkError` if lint flags them as unused
- [ ] Run `nx run organiclever-fe:typecheck`
- [ ] Run `nx run organiclever-fe:lint`

### Phase 4 — Specs and Tests

- [ ] Delete `specs/apps/organiclever/fe/gherkin/authentication/google-login.feature`
- [ ] Delete `specs/apps/organiclever/fe/gherkin/authentication/profile.feature`
- [ ] Delete `specs/apps/organiclever/fe/gherkin/authentication/route-protection.feature`
- [ ] Delete `apps/organiclever-fe/test/unit/steps/authentication/google-login.steps.tsx`
- [ ] Delete `apps/organiclever-fe/test/unit/steps/authentication/profile.steps.tsx`
- [ ] Delete `apps/organiclever-fe/test/unit/steps/authentication/route-protection.steps.tsx`
- [ ] Delete `apps/organiclever-fe-e2e/steps/google-login.steps.ts`
- [ ] Delete `apps/organiclever-fe-e2e/steps/profile.steps.ts`
- [ ] Delete `apps/organiclever-fe-e2e/steps/route-protection.steps.ts`
- [ ] Add `specs/apps/organiclever/fe/gherkin/system/system-status-be.feature` covering the four `/system/status/be` scenarios
- [ ] Add unit step file `apps/organiclever-fe/test/unit/steps/system/system-status-be.steps.tsx`
- [ ] Add `specs/apps/organiclever/fe/gherkin/landing/landing.feature` covering the "Root renders landing without BE" scenario
- [ ] Add unit step file `apps/organiclever-fe/test/unit/steps/landing/landing.steps.tsx`
- [ ] Add e2e step file `apps/organiclever-fe-e2e/steps/landing.steps.ts`
- [ ] Add `specs/apps/organiclever/fe/gherkin/routing/disabled-routes.feature` covering the "Disabled routes return 404" Scenario Outline
- [ ] Add unit step file `apps/organiclever-fe/test/unit/steps/routing/disabled-routes.steps.tsx`
- [ ] Add e2e step file `apps/organiclever-fe-e2e/steps/disabled-routes.steps.ts`
- [ ] Add MSW handler for `/health` (UP and failure variants)
- [ ] Add `apps/organiclever-fe-e2e/steps/system-status-be.steps.ts` (playwright-bdd step definitions)
- [ ] Run `nx run organiclever-fe:test:quick` — must pass with ≥70% coverage
- [ ] Run `nx run organiclever-fe:test:integration`
- [ ] Run `nx run organiclever-fe:spec-coverage`
- [ ] Run `nx run organiclever-fe-e2e:test:e2e` (against local docker-compose)

### Phase 5 — Documentation

- [ ] Rewrite Architecture section in `apps/organiclever-fe/README.md` for local-first mode
- [ ] Document `/system/status/be` failure modes and env var in the README
- [ ] Note dormant BE integration code in the README
- [ ] Update `CLAUDE.md` organiclever-fe description if it mentions BFF
- [ ] Update `docs/reference/system-architecture/applications.md` organiclever-fe section
- [ ] Grep `docs/` and `governance/` for BFF / `/api/auth` references and update or remove

### Local Quality Gates (Before Push)

- [ ] Run affected typecheck: `nx affected -t typecheck`
- [ ] Run affected linting: `nx affected -t lint`
- [ ] Run affected quick tests: `nx affected -t test:quick`
- [ ] Run affected spec coverage: `nx affected -t spec-coverage`
- [ ] Fix ALL failures found — including preexisting issues not caused by your changes
- [ ] Verify all checks pass before pushing

> **Important**: Fix ALL failures found during quality gates, not just those caused by your
> changes. This follows the root cause orientation principle — proactively fix preexisting
> errors encountered during work.

### Commit Guidelines

- [ ] Commit changes thematically — group related changes into logically cohesive commits
- [ ] Follow Conventional Commits format: `<type>(<scope>): <description>`
- [ ] Split different domains/concerns into separate commits (e.g., routes in one commit, tests in another, docs in another)
- [ ] Do NOT bundle unrelated fixes into a single commit

### Manual UI Verification (Playwright MCP — Local)

- [ ] Start dev server: `nx dev organiclever-fe`
- [ ] Navigate to landing page via `browser_navigate http://localhost:3200/`
- [ ] Inspect landing page via `browser_snapshot` — verify heading renders, no redirect occurs
- [ ] Check for JS errors via `browser_console_messages` — must be zero errors
- [ ] Navigate to status page: `browser_navigate http://localhost:3200/system/status/be`
- [ ] Verify "Not configured" renders via `browser_snapshot`
- [ ] Check for JS errors via `browser_console_messages` — must be zero errors
- [ ] Take screenshots via `browser_take_screenshot` for visual record
- [ ] Verify `/login` returns 404: `browser_navigate http://localhost:3200/login`
- [ ] Verify `/profile` returns 404: `browser_navigate http://localhost:3200/profile`

### Post-Push CI Verification

- [ ] Push changes to `main`
- [ ] Monitor `.github/workflows/test-and-deploy-organiclever.yml` in GitHub Actions
- [ ] Verify all CI checks (lint, typecheck, test:quick, spec-coverage, e2e) pass
- [ ] If any CI check fails, fix immediately and push a follow-up commit before proceeding
- [ ] Do NOT proceed to Vercel verification until CI is green

### Phase 6 — Manual Verification on Vercel

- [ ] Push merged work to `prod-organiclever-web`
- [ ] Confirm Vercel build succeeds with `ORGANICLEVER_BE_URL` unset
- [ ] Visit https://www.organiclever.com/ — landing renders
- [ ] Visit https://www.organiclever.com/system/status/be — "Not configured" renders, HTTP 200
- [ ] Set `ORGANICLEVER_BE_URL` in Vercel to an unreachable host, redeploy preview, verify DOWN renders with reason and HTTP 200
- [ ] Remove `ORGANICLEVER_BE_URL` from Vercel once verification done

### Phase 7 — Close Out

- [ ] Move plan folder: `git mv plans/in-progress/2026-04-16__organiclever-fe-local-first plans/done/`
- [ ] Update `plans/in-progress/README.md` — remove plan entry
- [ ] Update `plans/done/README.md` — add plan entry with completion date
- [ ] Commit: `chore(plans): move organiclever-fe-local-first to done`

## References

- `apps/organiclever-fe/` — affected app
- `apps/organiclever-be/` — untouched; future re-enablement target
- `.github/workflows/test-and-deploy-organiclever.yml` — deploy pipeline
- [Plans Organization Convention](../../../governance/conventions/structure/plans.md)
- [CLAUDE.md](../../../CLAUDE.md) — `organiclever-fe` section
