# Ubiquitous Language — health

**Bounded context**: `health`
**Maintainer**: oseplatform-web team
**Last reviewed**: 2026-05-10
**Audience:** Engineers, Technical Product/Project Managers

## One-line summary

Service liveness probe via the tRPC `healthRouter` with a `check` procedure plus an optional
system-status diagnostic page.

## Term index

| Term           | Code identifier(s)           | Used in features        |
| -------------- | ---------------------------- | ----------------------- |
| `Health probe` | `healthRouter`, `StatusPage` | `health/health.feature` |
| `Status tile`  | `StatusPage`                 | `health/health.feature` |
| `UP`           | `healthRouter`               | `health/health.feature` |

## Terms in detail

### Term: `Health probe`

Read-only tRPC query (`check` procedure on `healthRouter`) returning `{ status: "ok" }`.

**Code identifier(s)**: `healthRouter` (`src/contexts/health/application/router.ts`);
`StatusPage` (`src/contexts/health/presentation/status-page.tsx`).

**Used in features**: `health/health.feature`

**Forbidden synonyms in this context**: "ping"; "heartbeat"; "liveness check".

**Related**: `UP`, `Status tile`

---

### Term: `Status tile`

Visual block rendering the resolved `Health probe` outcome on the system-status page.

**Code identifier(s)**: `StatusPage` (`src/contexts/health/presentation/status-page.tsx`).

**Used in features**: `health/health.feature`

**Forbidden synonyms in this context**: "status badge"; "indicator".

**Related**: `Health probe`

---

### Term: `UP`

The successful outcome of a `Health probe`. Returned by `healthRouter` as `status: "ok"`.

**Code identifier(s)**: `healthRouter` (`src/contexts/health/application/router.ts`).

**Used in features**: `health/health.feature`

**Forbidden synonyms in this context**: "alive"; "ready".

**Related**: `Health probe`
