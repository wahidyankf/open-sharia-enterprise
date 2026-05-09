# Ubiquitous Language — health

**Bounded context**: `health`
**Maintainer**: organiclever-web team
**Last reviewed**: 2026-05-09
**Audience:** Engineers, Technical Product/Project Managers

## One-line summary

Backend health-endpoint client and the `/system/status/be` diagnostic page. Reads
`ORGANICLEVER_BE_URL`, probes `GET /health`, and renders an UP / DOWN / Not configured
tile. The `health` context is dormant today (no live backend deployed) but its code is
preserved for future rewire.

## Term index

| Term              | Code identifier(s)                                                                                                       | Used in features   |
| ----------------- | ------------------------------------------------------------------------------------------------------------------------ | ------------------ |
| `Backend URL`     | `ORGANICLEVER_BE_URL` (env var, server-only)                                                                             | `health/*.feature` |
| `Health probe`    | `BackendClient` (Effect Service Tag), `get` method on `BackendClientService`                                             | `health/*.feature` |
| `Health status`   | `ApiError`, `NetworkError` (tagged error types in infrastructure/errors.ts); UP/DOWN/Not configured are UI string labels | `health/*.feature` |
| `Status tile`     | Server-rendered page component at system/status/be (no exported component)                                               | `health/*.feature` |
| `Backend client`  | `BackendClient` (Effect Service Tag), `BackendClientLive`, `createBackendClientTest`                                     | `health/*.feature` |
| `Diagnostic page` | Server-rendered at route system/status/be (force-dynamic; no exported identifier)                                        | `health/*.feature` |

## Terms in detail

### Term: `Backend URL`

The runtime-configured address of the OrganicLever backend, provided via the
`ORGANICLEVER_BE_URL` environment variable. Server-only — never exposed to the browser.
Read at request time (not at build time) by the `Diagnostic page`. When unset, the page
renders "Not configured" without attempting a probe.

**Code identifier(s)**:
`ORGANICLEVER_BE_URL` — the Node.js environment variable read in the server-rendered page
(`apps/organiclever-web/src/app/system/status/be/page.tsx`).

**Used in features**: `health/*.feature`

**Forbidden synonyms in this context**: "API URL" (too generic — inside `health` always
use `Backend URL`); "host" (the value includes scheme and port, not just a hostname).

**Related**: `Health probe`, `Diagnostic page`

---

### Term: `Health probe`

A `GET <Backend URL>/health` HTTP request with a 3-second timeout, executed on each
server render of the `Diagnostic page`. Issued via the `BackendClient` Effect service.
Succeeds with a 2xx response body; fails with `NetworkError` on connection error,
timeout, or non-2xx status.

**Code identifier(s)**:
`BackendClient` — the Effect Service Tag defining the `get` and `post` port methods
(`apps/organiclever-web/src/contexts/health/infrastructure/backend-client.ts`).
`BackendClientLive` — the live implementation that issues real HTTP requests
(`apps/organiclever-web/src/contexts/health/infrastructure/backend-client-live.ts`).

**Used in features**: `health/*.feature`

**Forbidden synonyms in this context**: "Health check" (the backend's perspective — the
client performs a `Health probe`); "Ping" (implies ICMP, not HTTP).

**Related**: `Backend URL`, `Health status`, `Backend client`

---

### Term: `Health status`

The outcome of a `Health probe`, mapped to one of three presentation states:

- **UP** — probe returned 2xx within the timeout.
- **DOWN** — non-2xx, connection error, timeout, or parse error.
- **Not configured** — `ORGANICLEVER_BE_URL` is unset; probe not attempted.

The `Diagnostic page` renders a status tile for exactly one of these states.

**Code identifier(s)**:
`NetworkError` — the tagged error for connection/timeout failures
(`apps/organiclever-web/src/contexts/health/infrastructure/errors.ts`).
`ApiError` — the tagged error for non-2xx responses (same file).
The three string labels (`"UP"`, `"DOWN"`, `"Not configured"`) are presentation-layer
constants in the server-rendered page.

**Used in features**: `health/*.feature`

**Forbidden synonyms in this context**: "Status" alone (overloaded with session status
in `workout-session` and preferences status in `settings` — inside `health`, always
qualify as "health status").

**Related**: `Health probe`, `Status tile`

---

### Term: `Status tile`

The card rendered on `/system/status/be` for one of the three `Health status` values.
Includes: backend URL, measured probe latency (UP only), response body excerpt (UP only),
and failure reason (DOWN only). Server-rendered on each request — no client-side
hydration logic. The entire page is a single server component marked `force-dynamic`.

**Code identifier(s)**:
Server-rendered page component at
`apps/organiclever-web/src/app/system/status/be/page.tsx`.

**Used in features**: `health/*.feature`

**Forbidden synonyms in this context**: "Status card" (informal — the UL term is `Status
tile`); "Health widget" (too product-flavoured for a diagnostic page).

**Related**: `Health status`, `Diagnostic page`

---

### Term: `Backend client`

The Effect (a typed functional-effects library for TypeScript) service defining the port for
HTTP communication with the OrganicLever backend. Three artifacts:
`BackendClient` (the service tag / interface), `BackendClientLive` (the live HTTP
implementation), and `createBackendClientTest` (the test factory returning a mock). The
port pattern isolates the `health` context's application layer from the HTTP details —
swapping the live layer for a test layer requires only a different Layer binding.

**Code identifier(s)**:
`BackendClient` — Effect Context.Tag defining `BackendClientService` (port interface)
(`apps/organiclever-web/src/contexts/health/infrastructure/backend-client.ts`).
`BackendClientLive` — live HTTP layer (same directory, `backend-client-live.ts`).
`createBackendClientTest` — test factory (same directory, `backend-client-test.ts`).

**Used in features**: `health/*.feature`

**Forbidden synonyms in this context**: "HTTP client" (the mechanism — the domain term
is `Backend client`); "API client" (implies a full REST client — today only probes
`/health`).

**Related**: `Health probe`, `Backend URL`

---

### Term: `Diagnostic page`

The route `/system/status/be` that renders the `Status tile`. Server-rendered on every
request via `export const dynamic = "force-dynamic"` (Vercel never prerenders it at
build time). Never returns HTTP 4xx/5xx regardless of probe outcome — all failure paths
render as "DOWN" within a 200 response.

**Code identifier(s)**:
`system/status/be` — the Next.js route segment
(`apps/organiclever-web/src/app/system/status/be/page.tsx`).
`export const dynamic = "force-dynamic"` — the Next.js cache directive that prevents
build-time prerendering (same file).

**Used in features**: `health/*.feature`

**Forbidden synonyms in this context**: "Status page" (can be confused with a public
operational-status site like status.example.com — the `Diagnostic page` is an internal
developer tool); "Health page" (the page renders health diagnostics but the UL term is
`Diagnostic page`).

**Related**: `Status tile`, `Backend URL`

---

## Forbidden synonyms

- "Status" alone — overloaded with "session status" in `workout-session` and "settings
  status" in `settings`. Inside `health`, always qualify as "health status".
- "Endpoint" — refers to the backend's `GET /health` route. Inside `health`, the backend
  route is the target, not an owned concept; prefer "backend URL" or "health endpoint".
