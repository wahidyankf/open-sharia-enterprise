# ayokoding-web-v2

Fullstack Next.js 16 application that serves the AyoKoding educational content platform. Replaces the Hugo-based `ayokoding-web` site with a modern TypeScript stack: tRPC for type-safe API, Zod for validation, shadcn/ui for components, and FlexSearch for full-text search.

## Architecture

- **Framework**: Next.js 16 (App Router, React Server Components)
- **API**: tRPC with server caller (RSC) and HTTP endpoint (search)
- **Content**: Reads markdown from `apps/ayokoding-web/content/` (shared with Hugo site)
- **Rendering**: Server-side HTML (ISR) for SEO, client-side only for search/theme/tabs
- **Styling**: Tailwind CSS v4 + shadcn/ui + @tailwindcss/typography
- **Search**: FlexSearch with per-locale indexing
- **i18n**: English (`/en`) and Indonesian (`/id`) with segment mapping
- **Analytics**: Google Analytics GA4 via @next/third-parties

## Quick Start

```bash
# Development server (port 3101)
nx dev ayokoding-web-v2

# Build
nx build ayokoding-web-v2

# Run tests
nx run ayokoding-web-v2:test:quick

# Typecheck
nx run ayokoding-web-v2:typecheck

# Lint
nx run ayokoding-web-v2:lint
```

## Docker

```bash
# Build and run with Docker Compose
cd infra/dev/ayokoding-web-v2
docker compose up --build

# Health check
curl http://localhost:3101/api/trpc/meta.health
```

## Deployment

Deployed to Vercel via production branch `prod-ayokoding-web-v2`.

```bash
# Vercel auto-builds when code is pushed to prod branch
git push origin main:prod-ayokoding-web-v2
```

## Related

- [ayokoding-web](../ayokoding-web/) - Hugo v1 site (content source)
- [ayokoding-web-v2-be-e2e](../ayokoding-web-v2-be-e2e/) - Backend E2E tests
- [ayokoding-web-v2-fe-e2e](../ayokoding-web-v2-fe-e2e/) - Frontend E2E tests
- [specs/apps/ayokoding-web/](../../specs/apps/ayokoding-web/) - Gherkin specifications
