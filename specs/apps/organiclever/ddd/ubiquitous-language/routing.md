# Ubiquitous Language — routing

**Bounded context**: `routing`
**Maintainer**: organiclever-web team
**Last reviewed**: 2026-05-09
**Audience:** Engineers, Technical Product/Project Managers

## One-line summary

Disabled-route guards. v0 has no authentication, so `/login` and `/profile` exist only
as 404 stubs to communicate "feature not yet available" without leaking placeholders into
other contexts.

## Term index

| Term             | Code identifier(s)                                                                  | Used in features    |
| ---------------- | ----------------------------------------------------------------------------------- | ------------------- |
| `Disabled route` | `DisabledRoute` (component — commented-out export in routing/presentation/index.ts) | `routing/*.feature` |
| `/login` guard   | Next.js route at app/login/ (no exported identifier in routing context)             | `routing/*.feature` |
| `/profile` guard | Next.js route at app/profile/ (no exported identifier in routing context)           | `routing/*.feature` |
| `Not-found page` | Next.js not-found.tsx special file (no exported identifier in routing context)      | `routing/*.feature` |

## Terms in detail

### Term: `Disabled route`

A route deliberately rendering the `Not-found page` (HTTP 404) because the underlying
feature is not in v0 scope. The route path exists in the Next.js App Router so that any
future feature can reuse it without coordination, but the only content served is the
404 fallback. `DisabledRoute` is the presentation component that renders the "not
available" message inside the shared `not-found.tsx` shell.

**Code identifier(s)**:
`DisabledRoute` — the React component rendered by disabled route segments
(`apps/organiclever-web/src/contexts/routing/presentation/index.ts`).
`not-found.tsx` — the Next.js special file that Next.js invokes for 404 responses
(`apps/organiclever-web/src/app/not-found.tsx`).

**Used in features**: `routing/*.feature`

**Forbidden synonyms in this context**: "Placeholder route" (implies future content is
imminent — the UL term `Disabled route` is explicit that the feature is out of v0
scope); "Coming soon page" (product copy, not a domain term).

**Related**: `/login` guard, `/profile` guard, `Not-found page`

---

### Term: `/login` guard

The 404 stub at the `/login` URL path. Communicates "no authentication in v0" to any
user or crawler that visits the route. Prevents the path from being silently redirected
elsewhere (which could mislead users about auth availability).

**Code identifier(s)**:
`login` — the Next.js App Router route segment directory
(`apps/organiclever-web/src/app/login/`).

**Used in features**: `routing/*.feature`

**Forbidden synonyms in this context**: "Auth" / "authentication" / "session" — not v0
concepts. Any occurrence of these terms in source or specs without an explicit
"out-of-scope" qualifier is a governance finding.

**Related**: `Disabled route`, `/profile` guard

---

### Term: `/profile` guard

The 404 stub at the `/profile` URL path. Communicates "no user profile in v0" —
user identity and preference editing live in `settings`, not in a dedicated profile
surface.

**Code identifier(s)**:
`profile` — the Next.js App Router route segment directory
(`apps/organiclever-web/src/app/profile/`).

**Used in features**: `routing/*.feature`

**Forbidden synonyms in this context**: "Profile" as a `settings`/`preferences` synonym
(the `settings` bounded context owns preferences — `routing` only owns the disabled
stub at this URL path).

**Related**: `Disabled route`, `/login` guard

---

### Term: `Not-found page`

The shared 404 component invoked by Next.js for any unmatched URL — including all
`Disabled route`s. Next.js App Router discovers `not-found.tsx` in the `app/` root and
renders it automatically when `notFound()` is called or a route segment is missing.

**Code identifier(s)**:
`not-found.tsx` — the Next.js special-file convention
(`apps/organiclever-web/src/app/not-found.tsx`).

**Used in features**: `routing/*.feature`

**Forbidden synonyms in this context**: "Error page" (Next.js distinguishes `error.tsx`
for runtime errors from `not-found.tsx` for 404s — these are separate conventions);
"404 page" (an implementation label — the UL term is `Not-found page`).

**Related**: `Disabled route`

---

## Forbidden synonyms

- "Auth" / "authentication" / "session" — not v0 concepts. Anywhere these appear in
  source or specs without explicit "out-of-scope" framing is a finding.
- "Profile" — owned here only as a disabled-route surface. Never as a
  settings/preferences synonym (those belong to `settings`).
