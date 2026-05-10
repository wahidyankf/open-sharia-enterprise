# ayokoding-web

Fullstack Next.js 16 application that serves the AyoKoding educational content platform. TypeScript stack: tRPC for type-safe API, Zod for validation, shadcn/ui for components, and FlexSearch for full-text search.

## Architecture

- **Framework**: Next.js 16 (App Router, React Server Components)
- **API**: tRPC with server caller (RSC) and HTTP endpoint (search)
- **Content**: Reads markdown from `content/` (co-located in the app)
- **Rendering**: Full SSG via `generateStaticParams` for SEO, client-side only for search/theme/tabs
- **Styling**: Tailwind CSS v4 + shadcn/ui + @tailwindcss/typography
- **Search**: FlexSearch with per-locale indexing
- **i18n**: English (`/en`) and Indonesian (`/id`) with segment mapping
- **Analytics**: Google Analytics GA4 via @next/third-parties

## Quick Start

```bash
# Development server (port 3101)
nx dev ayokoding-web

# Build
nx build ayokoding-web

# Run tests
nx run ayokoding-web:test:quick

# Typecheck
nx run ayokoding-web:typecheck

# Lint
nx run ayokoding-web:lint
```

## Docker

```bash
# Build and run with Docker Compose
cd infra/dev/ayokoding-web
docker compose up --build

# Health check
curl http://localhost:3101/api/trpc/meta.health
```

## Deployment

Deployed to Vercel via production branch `prod-ayokoding-web`.

```bash
# Vercel auto-builds when code is pushed to prod branch
git push origin main:prod-ayokoding-web
```

## Source Layout (BC-organized)

`src/` is organized by **bounded context** (per-BC `application/`, `infrastructure/`,
`presentation/` subfolders) rather than by technical layer. The 6 bounded contexts:

| BC           | Layers present                                | Owns                                                                      |
| ------------ | --------------------------------------------- | ------------------------------------------------------------------------- |
| `app-shell`  | `[application, presentation]`                 | tRPC root router stitching + chrome (header, footer, theme toggle, ui/)   |
| `content`    | `[application, infrastructure, presentation]` | tRPC `content.*` procedures + filesystem reader + markdown rendering      |
| `search`     | `[application, infrastructure, presentation]` | tRPC `search.query` + FlexSearch index (in content infra) + search dialog |
| `i18n`       | `[application, presentation]`                 | Locale schema + `meta.languages` + middleware + locale switcher           |
| `navigation` | `[application, presentation]`                 | tRPC `content.getTree` (owned by navigation BC) + sidebar/breadcrumb/toc  |
| `health`     | `[application]`                               | tRPC `meta.health` liveness probe                                         |

The DDD registry (`specs/apps/ayokoding/ddd/bounded-contexts.yaml`) is the source of truth.
`rhino-cli ddd bc ayokoding` and `rhino-cli ddd ul ayokoding` enforce the structure on every
`test:quick` run.

## Specs

Gherkin acceptance specs live at `specs/apps/ayokoding/` organized by **API perspective**:

- `behavior/web/gherkin/` — UI-semantic scenarios (clicks, sees, navigates), consumed by
  `ayokoding-web-fe-e2e`.
- `behavior/api/gherkin/` — tRPC HTTP-semantic scenarios (the client calls, response shape),
  consumed by `ayokoding-web-be-e2e`.

The slug `api` is a **perspective slug**, not a container — there is no separate API
container. Both perspectives execute inside this single `web` Next.js process. The slug
rename `be` → `api` reflects this (the `organiclever` peer keeps `be` because
`organiclever-be` is a real F#/Giraffe deployment).

## i18n middleware ownership

The Next.js middleware lives at the conventional path `src/middleware.ts` but is reduced to
a one-line re-export from the `i18n` BC's application layer:

```ts
export { middleware, config } from "./contexts/i18n/application/middleware";
```

The actual implementation (locale negotiation, `/` redirect to `/<DEFAULT_LOCALE>`) lives in
`src/contexts/i18n/application/middleware.ts`. This keeps `next dev` and `next build` happy
(they find the middleware where Next.js expects it) while putting all i18n code under the
i18n BC's ownership.

## Related

- [ayokoding-web-be-e2e](../ayokoding-web-be-e2e/) - Backend E2E tests (consumes `behavior/api/gherkin/`)
- [ayokoding-web-fe-e2e](../ayokoding-web-fe-e2e/) - Frontend E2E tests (consumes `behavior/web/gherkin/`)
- [specs/apps/ayokoding/](../../specs/apps/ayokoding/) - C4 + DDD + Gherkin specifications
