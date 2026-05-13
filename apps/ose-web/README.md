# ose-web

Official website for the **Open Sharia Enterprise** platform — an open-source Sharia-compliant
enterprise solutions platform built in the open.

**Why This Matters**: Islamic finance is a multi-trillion dollar industry, but most
Sharia-compliant enterprise solutions are proprietary and expensive. We're building an
open-source alternative with Sharia-compliance at its core — not bolted on as an afterthought.

**What This Site Does**: Showcases the platform and shares our journey. Regular updates keep the
community informed as we build with radical transparency.

## Architecture

- **Framework**: Next.js 16 (App Router, React Server Components)
- **Language**: TypeScript (strict mode)
- **API**: tRPC for type-safe server-client communication (runs inside the Next.js process)
- **Content**: Markdown with YAML frontmatter from `content/` (~6 pages: Landing, About, updates)
- **Styling**: Tailwind CSS v4 + shadcn/ui
- **Search**: FlexSearch for full-text search
- **Diagrams**: Mermaid diagram support
- **Testing**: Vitest (unit + integration), 80% line coverage enforced via rhino-cli
- **DDD**: Per-BC layout under `src/contexts/<bc>/{application,infrastructure,presentation}/`

## Quick Start

```bash
# Development server (port 3100)
nx dev ose-web

# Production build
nx build ose-web

# Typecheck
nx run ose-web:typecheck

# Lint (oxlint)
nx run ose-web:lint

# Unit tests + DDD validators + coverage + links
nx run ose-web:test:quick

# Integration tests
nx run ose-web:test:integration

# Spec coverage (both web + api perspectives)
nx run ose-web:spec-coverage
```

## Project Structure

```
ose-web/
├── src/
│   ├── app/            # Next.js App Router routes (thin glue, imports from contexts/)
│   ├── contexts/       # DDD bounded contexts (one folder per BC)
│   │   ├── app-shell/  # Site chrome + root tRPC router
│   │   ├── landing/    # Marketing landing page
│   │   ├── content/    # Content retrieval + rendering
│   │   ├── search/     # Search backend + UI
│   │   ├── rss-feed/   # RSS feed generation
│   │   ├── seo/        # Sitemap + metadata
│   │   └── health/     # Health probe + status page
│   └── lib/            # Cross-cutting utilities (tRPC infra, cn)
├── test/               # Test files (Vitest unit + integration)
├── content/            # Markdown pages with YAML frontmatter
└── project.json        # Nx project configuration
```

## Specs

This project follows the canonical C4 + DDD spec layout introduced in the
`ose-web-ddd-and-specs-format` plan. Spec tree: `specs/apps/ose-platform/`.

| Section                                                                              | What it contains                                        |
| ------------------------------------------------------------------------------------ | ------------------------------------------------------- |
| [system-context/](../../specs/apps/ose-platform/system-context/)                     | C4 L1 — actors, external systems                        |
| [containers/](../../specs/apps/ose-platform/containers/)                             | C4 L2 — single `web` container + slug-vs-container note |
| [components/web/](../../specs/apps/ose-platform/components/web/)                     | C4 L3 — UI perspective                                  |
| [components/api/](../../specs/apps/ose-platform/components/api/)                     | C4 L3 — tRPC HTTP perspective (`api` slug, not `be`)    |
| [ddd/bounded-contexts.yaml](../../specs/apps/ose-platform/ddd/bounded-contexts.yaml) | DDD registry (7 BCs, schema v2)                         |
| [ddd/ubiquitous-language/](../../specs/apps/ose-platform/ddd/ubiquitous-language/)   | Per-BC glossaries                                       |
| [behavior/web/gherkin/](../../specs/apps/ose-platform/behavior/web/gherkin/)         | UI-semantic Gherkin (web perspective)                   |
| [behavior/api/gherkin/](../../specs/apps/ose-platform/behavior/api/gherkin/)         | tRPC HTTP-semantic Gherkin (api perspective)            |

> **Slug note**: The tRPC perspective slug is `api`, not `be`. The tRPC server runs **inside**
> the same Next.js process — there is no separate backend container. This is a deliberate
> deviation from `organiclever` where `be` maps to a real separate deployable.

## Bounded Contexts

| BC          | Layers                                    | Gherkin perspective |
| ----------- | ----------------------------------------- | ------------------- |
| `app-shell` | application, presentation                 | `web`               |
| `landing`   | presentation                              | `web`               |
| `content`   | application, infrastructure, presentation | `api`               |
| `search`    | application, infrastructure, presentation | `api`               |
| `rss-feed`  | application                               | `api`               |
| `seo`       | application, presentation                 | `api`               |
| `health`    | application, presentation                 | `api`               |

## Deployment

Deployed to Vercel via production branch `prod-ose-web`.

- **Production**: <https://oseplatform.com>
- **Deploy**: Push `main` to `prod-ose-web`; Vercel builds automatically

```bash
git push origin main:prod-ose-web
```

## Related

- [Specs root](../../specs/apps/ose-platform/README.md)
- [Main Repository](https://github.com/wahidyankf/ose-public)
- [apps-ose-web-deployer](../../.claude/agents/) — AI agent for deployments
