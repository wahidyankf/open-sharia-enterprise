# organiclever-web

Next.js 16 frontend for the OrganicLever life journal — local-first productivity tracker
with PGlite (Postgres-WASM, IndexedDB-backed) for in-browser data storage.

> **Status: Pre-Alpha.** Data model may change between versions without migration.

## Quick Start

```bash
nx dev organiclever-web   # http://localhost:3200
```

## Commands

| Nx target                                  | What it does                             |
| ------------------------------------------ | ---------------------------------------- |
| `nx dev organiclever-web`                  | Dev server (localhost:3200)              |
| `nx build organiclever-web`                | Production build                         |
| `nx run organiclever-web:test:quick`       | Unit tests + coverage (70%) + DDD checks |
| `nx run organiclever-web:test:unit`        | Unit tests only                          |
| `nx run organiclever-web:test:integration` | Integration tests                        |
| `nx run organiclever-web:spec-coverage`    | Gherkin spec coverage                    |
| `nx run organiclever-web:lint`             | Lint with oxlint + ESLint                |
| `nx run organiclever-web:typecheck`        | TypeScript type check                    |

## Environment Variables

| Variable              | Scope       | Required | Description                                         |
| --------------------- | ----------- | -------- | --------------------------------------------------- |
| `ORGANICLEVER_BE_URL` | Server-only | No       | Backend base URL probed by `/system/status/be` only |

## Project Layout

```
apps/organiclever-web/src/
├── app/          # Next.js App Router — thin page wrappers only
├── contexts/     # Bounded-context implementations (journal, routine, stats, …)
├── shared/       # Cross-context utilities (PgliteService, format-relative-time)
└── test/         # Vitest-cucumber step implementations
```

## Tech Stack

- **Next.js 16** — App Router, Server Components
- **PGlite** — Postgres-WASM, IndexedDB-backed; local-first, no backend required
- **XState v5** — UI shell and workout-session FSMs
- **Effect TS** — typed functional effects in infrastructure layer
- **Tailwind CSS v4** — utility-first CSS
- **`@open-sharia-enterprise/ts-ui`** — shared component library

## Behavior & Architecture

| Artifact                     | Location                                                                                                                          |
| ---------------------------- | --------------------------------------------------------------------------------------------------------------------------------- |
| Bounded-context architecture | [specs/…/components/web/architecture.md](../../specs/apps/organiclever/components/web/architecture.md)                            |
| Routes and screens           | [specs/…/components/web/routes-and-screens.md](../../specs/apps/organiclever/components/web/routes-and-screens.md)                |
| Design system                | [specs/…/components/web/design-system.md](../../specs/apps/organiclever/components/web/design-system.md)                          |
| Ubiquitous language          | [specs/…/components/web/ddd/ubiquitous-language/](../../specs/apps/organiclever/components/web/ddd/ubiquitous-language/README.md) |
| Gherkin specs                | [specs/…/behavior/web/gherkin/](../../specs/apps/organiclever/behavior/web/gherkin/README.md)                                     |

## Related

- [organiclever-web-e2e](../organiclever-web-e2e/README.md) — Playwright E2E tests
- [specs/apps/organiclever/](../../specs/apps/organiclever/README.md) — full spec tree
