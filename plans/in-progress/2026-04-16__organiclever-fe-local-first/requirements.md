# Requirements — OrganicLever FE Local-First Mode

See [`README.md`](./README.md) for overview, context, and scope. Architecture and implementation
notes live in [`tech-docs.md`](./tech-docs.md). Execution checklist lives in
[`delivery.md`](./delivery.md).

## Functional Requirements

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
3. If set → `fetch(${url}/health)` with `AbortSignal.timeout(3000)` (3-second timeout).
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
    And the page loads at / without intermediate redirect

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
      | method | path                |
      | GET    | /login              |
      | GET    | /profile            |
      | POST   | /api/auth/google    |
      | GET    | /api/auth/refresh   |
      | GET    | /api/auth/me        |
```
