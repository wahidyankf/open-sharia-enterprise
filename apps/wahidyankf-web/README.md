# wahidyankf-web

Personal portfolio / CV / projects site for Wahidyan Kresna Fridayoka.
Adopted from [`wahidyankf/oss / apps-standalone/wahidyankf-web`](https://github.com/wahidyankf/oss/tree/main/apps-standalone/wahidyankf-web)
in 2026-04 and retrofitted to the `ose-public` Nx monorepo conventions.

**Framework**: Next.js 16 (App Router) · React 19 · Tailwind CSS 4
**Language**: TypeScript
**Deployment**: Vercel via `prod-wahidyankf-web` branch
**Production domain**: <https://www.wahidyankf.com/>
**Dev port**: 3201

## Development

```bash
# Start the dev server (localhost:3201)
nx dev wahidyankf-web

# Production build
nx build wahidyankf-web

# Local production preview
nx start wahidyankf-web
```

## Quality gates

```bash
# Type check only
nx run wahidyankf-web:typecheck

# oxlint + jsx-a11y
nx run wahidyankf-web:lint

# Unit tests (Vitest 4, jsdom)
nx run wahidyankf-web:test:unit

# Fast pre-push gate: ddd bc → ddd ul → unit tests + coverage ≥80%
nx run wahidyankf-web:test:quick

# Integration tests (node environment; empty at adoption time)
nx run wahidyankf-web:test:integration

# Gherkin spec coverage check
nx run wahidyankf-web:spec-coverage
```

## Testing stack

- **Vitest 4** + `@vitejs/plugin-react` + `jsdom` for unit tests
- **`@amiceli/vitest-cucumber`** for Gherkin acceptance specs at the unit
  level (feature files under `specs/apps/wahidyankf/behavior/web/gherkin/`)
- **`@testing-library/react`** + **`@testing-library/jest-dom`** for
  component interaction
- Coverage enforced at ≥80% via `rhino-cli test-coverage validate` —
  aligned to `apps/ayokoding-web` and `apps/oseplatform-web`

End-to-end tests live in the sibling project `apps/wahidyankf-web-fe-e2e/`
using Playwright-BDD and `@axe-core/playwright` for WCAG 2.1 AA smoke.

## Specs

Platform-agnostic specifications for this app live at
[`specs/apps/wahidyankf/`](../../specs/apps/wahidyankf/README.md):

- **Five-folder C4 + DDD tree**: `product/`, `system-context/`, `containers/`,
  `components/`, `behavior/`
- **DDD registry**: [`specs/apps/wahidyankf/ddd/bounded-contexts.yaml`](../../specs/apps/wahidyankf/ddd/bounded-contexts.yaml) —
  five bounded contexts (`app-shell`, `home`, `cv`, `personal-projects`, `search`)
- **Ubiquitous-language glossaries**:
  [`specs/apps/wahidyankf/ddd/ubiquitous-language/`](../../specs/apps/wahidyankf/ddd/ubiquitous-language/README.md)
- **Gherkin features**:
  [`specs/apps/wahidyankf/behavior/web/gherkin/`](../../specs/apps/wahidyankf/behavior/web/gherkin/README.md) —
  7 feature files organized per bounded context

`rhino-cli ddd bc wahidyankf` and `rhino-cli ddd ul wahidyankf` run as the first
two commands in `test:quick` to enforce structural and vocabulary invariants before
unit tests run.

## Deployment

`prod-wahidyankf-web` branch receives force-pushes from `main` via the
`apps-wahidyankf-web-deployer` agent. Vercel watches the branch and
rebuilds on every push.

## Structure

```
apps/wahidyankf-web/
├── public/                   # Static assets (favicon, fonts)
├── src/
│   ├── app/                  # Next.js App Router routing shell (thin wrappers)
│   │   ├── cv/page.tsx       # Routes to CvContent from cv context
│   │   ├── personal-projects/page.tsx
│   │   ├── fonts/            # GeistVF, GeistMonoVF woff
│   │   ├── layout.tsx
│   │   ├── head.tsx
│   │   ├── page.tsx          # Routes to HomeContent from home context
│   │   └── globals.css       # Tailwind 4 entry
│   ├── contexts/             # DDD bounded contexts
│   │   ├── app-shell/presentation/   # Navigation, style utility
│   │   ├── cv/
│   │   │   ├── application/  # data.ts (CVEntry, cvData, helpers), markdown.tsx
│   │   │   └── presentation/ # CvContent.tsx
│   │   ├── home/presentation/        # HomeContent.tsx
│   │   ├── personal-projects/
│   │   │   ├── application/  # projects.ts (Project, filterProjects)
│   │   │   └── presentation/ # PersonalProjectsContent.tsx
│   │   └── search/
│   │       ├── application/  # search.ts (filterItems, SearchTerm, SearchResult)
│   │       └── presentation/ # SearchSection.tsx (placeholder)
│   └── test/setup.ts         # Vitest + Testing Library setup
└── test/unit/steps/          # Gherkin step implementations
```
