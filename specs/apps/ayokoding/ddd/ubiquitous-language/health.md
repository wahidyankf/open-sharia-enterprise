# Ubiquitous Language — health

**Bounded context**: `health`
**Maintainer**: ayokoding-web team
**Last reviewed**: 2026-05-10
**Audience:** Engineers, Technical Product/Project Managers

## One-line summary

Service liveness probe — the `health` tRPC procedure returning `{status: "ok"}`. No UI
surface; pure application layer.

## Term index

| Term             | Code identifier(s)                                                       | Used in features              |
| ---------------- | ------------------------------------------------------------------------ | ----------------------------- |
| `health`         | `health` procedure (apps/ayokoding-web/src/contexts/health/application/) | `health/health-check.feature` |
| `Liveness probe` | The intent of `health` — confirms the Next.js process is responsive      | `health/health-check.feature` |

## Terms in detail

### Term: `health`

The tRPC procedure that returns a constant `{status: "ok"}` literal. Used by deployment
infrastructure (Vercel) and external monitors as a `Liveness probe` for the running
Next.js server. Owned by the health BC's application layer.

**Code identifier(s)**:
`health` procedure in
`apps/ayokoding-web/src/contexts/health/application/`.

**Used in features**: `health/health-check.feature`

**Forbidden synonyms in this context**: "Health check" (this is the BACKEND'S perspective —
the procedure is `health`; "health check" is the action a caller performs); "Ping" (implies
ICMP, not tRPC); "Status check" (too generic).

**Related**: `Liveness probe`

---

### Term: `Liveness probe`

The semantic intent of the `health` procedure: confirm the Next.js process is responsive
and serving tRPC requests. Distinct from a readiness probe (which would also check
downstream dependencies); the `Liveness probe` checks only the process itself, returning
ok unconditionally as long as the procedure executes.

**Code identifier(s)**:
The `health` procedure (its purpose, not a separate identifier) in
`apps/ayokoding-web/src/contexts/health/application/`.

**Used in features**: `health/health-check.feature`

**Forbidden synonyms in this context**: "Heartbeat" (different mechanism — implies
periodic self-broadcast); "Readiness probe" (different semantic — checks downstreams).

**Related**: `health`

---

## Forbidden synonyms

- "Health check" — when referring to the procedure itself, use `health`. When referring
  to a caller's action, "health check" is acceptable in prose only, not as a UL term.
- "Ping" / "Heartbeat" — use `Liveness probe` for the semantic.
