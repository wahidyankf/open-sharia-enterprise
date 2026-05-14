# ose-app-web

Next.js 16 frontend for OSE Application (Governance, Risk, and Compliance) platform.

## Quick Start

1. Install dependencies: `npm install`
2. Copy env: `cp .env.example .env.local`
3. Start dev server: `nx dev ose-app-web`
4. Open: <http://localhost:3300>

## Commands

| Command                         | Description                    |
| ------------------------------- | ------------------------------ |
| `nx dev ose-app-web`            | Dev server (port 3300)         |
| `nx build ose-app-web`          | Production build               |
| `nx run ose-app-web:test:quick` | Unit tests + coverage          |
| `nx run ose-app-web:typecheck`  | TypeScript typecheck           |
| `nx run ose-app-web:lint`       | ESLint + oxlint                |
| `nx run ose-app-web:codegen`    | Generate TS types from OpenAPI |

## Tech Stack

- **Next.js 16** — App Router, React 19
- **TypeScript** — Strict mode
- **tRPC** — Type-safe API calls to ose-app-be
- **Tailwind v4** — Styling
- **@open-sharia-enterprise/web-ui** — Shared component library
- **Vitest** — Unit testing
- **Storybook** — Component development

## Related

- [ose-app-be](../ose-app-be/) — F#/Giraffe backend API
- [ose-app-be-e2e](../ose-app-be-e2e/) — BE E2E tests
- [ose-app-web-e2e](../ose-app-web-e2e/) — FE E2E tests
- [specs/apps/ose-app](../../specs/apps/ose-app/) — DDD specs and behavior
