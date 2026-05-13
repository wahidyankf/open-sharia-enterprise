# ose-grc-web

Next.js 16 frontend for OSE GRC (Governance, Risk, and Compliance) platform.

## Quick Start

1. Install dependencies: `npm install`
2. Copy env: `cp .env.example .env.local`
3. Start dev server: `nx dev ose-grc-web`
4. Open: <http://localhost:3300>

## Commands

| Command                         | Description                    |
| ------------------------------- | ------------------------------ |
| `nx dev ose-grc-web`            | Dev server (port 3300)         |
| `nx build ose-grc-web`          | Production build               |
| `nx run ose-grc-web:test:quick` | Unit tests + coverage          |
| `nx run ose-grc-web:typecheck`  | TypeScript typecheck           |
| `nx run ose-grc-web:lint`       | ESLint + oxlint                |
| `nx run ose-grc-web:codegen`    | Generate TS types from OpenAPI |

## Tech Stack

- **Next.js 16** — App Router, React 19
- **TypeScript** — Strict mode
- **tRPC** — Type-safe API calls to ose-grc-be
- **Tailwind v4** — Styling
- **@open-sharia-enterprise/web-ui** — Shared component library
- **Vitest** — Unit testing
- **Storybook** — Component development

## Related

- [ose-grc-be](../ose-grc-be/) — F#/Giraffe backend API
- [ose-grc-be-e2e](../ose-grc-be-e2e/) — BE E2E tests
- [ose-grc-web-e2e](../ose-grc-web-e2e/) — FE E2E tests
- [specs/apps/ose-grc](../../specs/apps/ose-grc/) — DDD specs and behavior
