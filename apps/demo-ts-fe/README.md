# Demo Next.js Frontend App

## Description

Demo Next.js application demonstrating the Nx monorepo structure and consuming TypeScript libraries.

## Tech Stack

- Next.js 15+ (App Router)
- React 19
- TypeScript 5.x
- Tailwind CSS
- ESLint

## Development

Start the development server:

```bash
nx dev demo-ts-fe
```

The app will be available at http://localhost:3000

## Build

Build for production:

```bash
nx build demo-ts-fe
```

## Serve Production Build

Start the production server:

```bash
nx serve demo-ts-fe
```

## Lint

Run ESLint:

```bash
nx lint demo-ts-fe
```

## Importing Libraries

This app can import from workspace libraries using path mappings:

```typescript
import { greet } from "@open-sharia-enterprise/ts-demo-libs";
```

Path mappings are configured in the workspace `tsconfig.base.json`.
