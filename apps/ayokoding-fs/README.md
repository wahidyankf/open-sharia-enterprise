# ayokoding-fs

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
nx dev ayokoding-fs

# Build
nx build ayokoding-fs

# Run tests
nx run ayokoding-fs:test:quick

# Typecheck
nx run ayokoding-fs:typecheck

# Lint
nx run ayokoding-fs:lint
```

## Docker

```bash
# Build and run with Docker Compose
cd infra/dev/ayokoding-fs
docker compose up --build

# Health check
curl http://localhost:3101/api/trpc/meta.health
```

## Deployment

Deployed to Vercel via production branch `prod-ayokoding-fs`.

```bash
# Vercel auto-builds when code is pushed to prod branch
git push origin main:prod-ayokoding-fs
```

## Related

- [ayokoding-fs-be-e2e](../ayokoding-fs-be-e2e/) - Backend E2E tests
- [ayokoding-fs-fe-e2e](../ayokoding-fs-fe-e2e/) - Frontend E2E tests
- [specs/apps/ayokoding/](../../specs/apps/ayokoding/) - Gherkin specifications
