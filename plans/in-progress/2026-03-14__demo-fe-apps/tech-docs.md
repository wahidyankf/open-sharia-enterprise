# Technical Documentation: Demo Frontend Apps

## Architecture Overview

```mermaid
graph TD
    subgraph Browser
        NX[demo-fe-ts-nextjs<br/>Next.js 16 / Port 3301]
        TS[demo-fe-ts-tanstackstart<br/>TanStack Start / Port 3301]
        RM[demo-fe-ts-remix<br/>React Router v7 / Port 3301]
        FL[demo-fe-dart-flutter<br/>Flutter Web / Port 3301]
        LV[demo-fe-elixir-phoenix<br/>Phoenix LiveView / Port 3301]
    end

    subgraph "Backend"
        BE[demo-be-java-springboot<br/>Port 8201]
    end

    subgraph "Database"
        DB[(PostgreSQL 17)]
    end

    subgraph "E2E Testing"
        E2E[demo-fe-e2e<br/>Playwright + playwright-bdd]
    end

    NX -->|proxy /api/v1/*| BE
    TS -->|proxy /api/v1/*| BE
    RM -->|proxy /api/v1/*| BE
    FL -->|proxy /api/v1/*| BE
    LV -->|server-side Req| BE
    BE --> DB
    E2E -->|drives browser| NX
    E2E -->|drives browser| TS
    E2E -->|drives browser| RM
    E2E -->|drives browser| FL
    E2E -->|drives browser| LV

    style NX fill:#2563eb,color:#fff
    style TS fill:#7c3aed,color:#fff
    style RM fill:#e8590c,color:#fff
    style FL fill:#0175c2,color:#fff
    style LV fill:#4e2a8e,color:#fff
    style BE fill:#059669,color:#fff
    style DB fill:#d97706,color:#fff
    style E2E fill:#dc2626,color:#fff
```

## App 1: demo-fe-ts-nextjs (Next.js 16)

### Framework Details

| Aspect    | Value                                                 |
| --------- | ----------------------------------------------------- |
| Framework | Next.js 16.1.x (latest stable)                        |
| React     | 19.x                                                  |
| Bundler   | Turbopack (default in Next.js 16)                     |
| Routing   | App Router (`app/` directory)                         |
| SSR       | React Server Components + Client Components           |
| API proxy | `next.config.ts` rewrites (preferred over middleware) |
| Dev port  | 3301                                                  |

### Directory Structure

```
apps/demo-fe-ts-nextjs/
├── src/
│   ├── app/                          # Next.js App Router
│   │   ├── layout.tsx                # Root layout with QueryClientProvider
│   │   ├── page.tsx                  # Home / health status page
│   │   ├── login/
│   │   │   └── page.tsx              # Login form
│   │   ├── register/
│   │   │   └── page.tsx              # Registration form
│   │   ├── profile/
│   │   │   └── page.tsx              # User profile management
│   │   ├── admin/
│   │   │   └── page.tsx              # Admin panel (user listing)
│   │   ├── expenses/
│   │   │   ├── page.tsx              # Expense list + CRUD
│   │   │   ├── [id]/
│   │   │   │   └── page.tsx          # Expense detail + attachments
│   │   │   └── summary/
│   │   │       └── page.tsx          # P&L reporting
│   │   └── tokens/
│   │       └── page.tsx              # Token/session info
│   ├── components/                   # Shared UI components
│   │   ├── ui/                       # Base UI primitives
│   │   ├── forms/                    # Form components (login, register, expense)
│   │   ├── layout/                   # Header, nav, sidebar, footer
│   │   └── data-display/            # Tables, cards, summary views
│   ├── lib/
│   │   ├── api/                      # API client functions
│   │   │   ├── client.ts             # Base fetch wrapper with auth headers
│   │   │   ├── auth.ts               # Auth API calls
│   │   │   ├── users.ts              # User API calls
│   │   │   ├── admin.ts              # Admin API calls
│   │   │   ├── expenses.ts           # Expense API calls
│   │   │   └── tokens.ts             # Token API calls
│   │   ├── queries/                  # React Query hooks
│   │   │   ├── use-auth.ts           # useLogin, useRegister, useLogout, useRefresh
│   │   │   ├── use-user.ts           # useCurrentUser, useUpdateProfile, useChangePassword
│   │   │   ├── use-admin.ts          # useAdminUsers, useDisableUser, useEnableUser
│   │   │   ├── use-expenses.ts       # useExpenses, useCreateExpense, useUpdateExpense
│   │   │   └── use-tokens.ts         # useTokenClaims
│   │   ├── auth/                     # Auth state management
│   │   │   ├── auth-provider.tsx      # Auth context + token refresh logic
│   │   │   └── auth-guard.tsx         # Protected route wrapper
│   │   └── utils/                    # Shared utilities
│   └── test/
│       ├── setup.ts                  # Vitest setup (jsdom, RTL cleanup, jest-dom matchers)
│       ├── test-providers.tsx        # QueryClientProvider + AuthProvider + Router wrapper
│       └── mocks/                    # Mock factories for API responses
├── test/
│   └── unit/                         # @amiceli/vitest-cucumber step definitions
│       ├── steps/                    # Gherkin step implementations by domain
│       │   ├── common.steps.ts       # Shared steps (app running, navigation)
│       │   ├── health/
│       │   │   └── health.steps.ts
│       │   ├── authentication/
│       │   │   ├── login.steps.ts
│       │   │   └── session.steps.ts
│       │   ├── user-lifecycle/
│       │   │   ├── registration.steps.ts
│       │   │   └── user-profile.steps.ts
│       │   ├── security/
│       │   │   └── security.steps.ts
│       │   ├── token-management/
│       │   │   └── tokens.steps.ts
│       │   ├── admin/
│       │   │   └── admin-panel.steps.ts
│       │   ├── expenses/
│       │   │   ├── expense-management.steps.ts
│       │   │   ├── currency-handling.steps.ts
│       │   │   ├── unit-handling.steps.ts
│       │   │   ├── reporting.steps.ts
│       │   │   └── attachments.steps.ts
│       │   └── layout/
│       │       ├── responsive.steps.ts
│       │       └── accessibility.steps.ts
│       └── support/                  # Test helpers, mock setup
│           ├── world.ts              # Cucumber World with mocked services
│           └── mock-api.ts           # API response factories
├── next.config.ts                    # API proxy rewrites, Turbopack config
├── tsconfig.json
├── vitest.config.ts                  # Vitest + @amiceli/vitest-cucumber
├── project.json                      # Nx configuration
├── package.json
└── README.md
```

### API Proxy Configuration

```typescript
// next.config.ts
import type { NextConfig } from "next";

const nextConfig: NextConfig = {
  async rewrites() {
    const backendUrl = process.env.BACKEND_URL || "http://localhost:8201";
    return [
      { source: "/api/:path*", destination: `${backendUrl}/api/:path*` },
      { source: "/health", destination: `${backendUrl}/health` },
      {
        source: "/.well-known/:path*",
        destination: `${backendUrl}/.well-known/:path*`,
      },
    ];
  },
};

export default nextConfig;
```

### React Query Setup

```typescript
// src/app/layout.tsx (simplified)
import { QueryClient, QueryClientProvider } from "@tanstack/react-query";

function makeQueryClient() {
  return new QueryClient({
    defaultOptions: {
      queries: {
        staleTime: 60 * 1000, // 1 minute
        retry: 1,
      },
    },
  });
}

// Client-side singleton, server-side per-request
let browserQueryClient: QueryClient | undefined;
function getQueryClient() {
  if (typeof window === "undefined") return makeQueryClient();
  if (!browserQueryClient) browserQueryClient = makeQueryClient();
  return browserQueryClient;
}
```

### Vitest + @amiceli/vitest-cucumber Configuration

```typescript
// vitest.config.ts
import { defineConfig } from "vitest/config";
import react from "@vitejs/plugin-react";
import tsconfigPaths from "vite-tsconfig-paths";

export default defineConfig({
  plugins: [react(), tsconfigPaths()],
  test: {
    passWithNoTests: true,
    coverage: {
      provider: "v8",
      include: ["src/**/*.{ts,tsx}"],
      exclude: ["src/app/layout.tsx", "src/test/**", "**/*.{test,spec}.{ts,tsx}"],
      reporter: ["text", "json-summary", "lcov"],
    },
    projects: [
      {
        test: {
          name: "unit",
          include: ["test/unit/**/*.steps.ts"],
          environment: "jsdom",
          setupFiles: ["./src/test/setup.ts"],
        },
      },
    ],
  },
});
```

The `setup.ts` file configures jsdom with React Testing Library:

```typescript
// src/test/setup.ts
import "@testing-library/jest-dom/vitest";
import { cleanup } from "@testing-library/react";
import { afterEach } from "vitest";

afterEach(() => {
  cleanup();
});
```

Unit tests use `@amiceli/vitest-cucumber` which:

1. Loads feature files from `specs/apps/demo/fe/gherkin/` via `loadFeature()`
2. Maps scenarios to Vitest `describe`/`test` blocks via `describeFeature()`
3. Validates that all Gherkin steps have matching step implementations (strict mode)
4. Step definitions render React components with `@testing-library/react`
5. User interactions via `@testing-library/user-event`
6. All API calls mocked via `vi.mock` — no real HTTP

### Nx project.json

```json
{
  "name": "demo-fe-ts-nextjs",
  "$schema": "../../node_modules/nx/schemas/project-schema.json",
  "projectType": "application",
  "sourceRoot": "apps/demo-fe-ts-nextjs/src",
  "targets": {
    "dev": {
      "executor": "nx:run-commands",
      "options": {
        "command": "next dev --port 3301",
        "cwd": "{projectRoot}"
      }
    },
    "build": {
      "executor": "nx:run-commands",
      "options": {
        "command": "next build",
        "cwd": "{projectRoot}"
      },
      "outputs": ["{projectRoot}/.next"]
    },
    "start": {
      "executor": "nx:run-commands",
      "options": {
        "command": "next start --port 3301",
        "cwd": "{projectRoot}"
      }
    },
    "typecheck": {
      "executor": "nx:run-commands",
      "options": {
        "command": "tsc --noEmit",
        "cwd": "{projectRoot}"
      }
    },
    "lint": {
      "executor": "nx:run-commands",
      "options": {
        "command": "npx oxlint@latest .",
        "cwd": "{projectRoot}"
      }
    },
    "test:quick": {
      "executor": "nx:run-commands",
      "options": {
        "commands": [
          "npx vitest run --coverage && (cd ../../apps/rhino-cli && CGO_ENABLED=0 go run main.go test-coverage validate apps/demo-fe-ts-nextjs/coverage/lcov.info 90)",
          "(cd ../../apps/rhino-cli && go run main.go spec-coverage validate specs/apps/demo/fe apps/demo-fe-ts-nextjs)"
        ],
        "parallel": true,
        "cwd": "{projectRoot}"
      }
    },
    "test:unit": {
      "executor": "nx:run-commands",
      "options": {
        "command": "npx vitest run --project unit",
        "cwd": "{projectRoot}"
      }
    }
  },
  "tags": ["type:app", "platform:nextjs", "lang:ts", "domain:demo-fe"],
  "implicitDependencies": []
}
```

## App 2: demo-fe-ts-tanstackstart (TanStack Start)

### Framework Details

| Aspect         | Value                                                    |
| -------------- | -------------------------------------------------------- |
| Framework      | TanStack Start v1 RC (`@tanstack/react-start` v1.166.x)  |
| React          | 19.x                                                     |
| Router         | TanStack Router (fully type-safe, file-based)            |
| Build tool     | Vite                                                     |
| Server runtime | Nitro (via H3)                                           |
| SSR            | Full-document SSR with streaming (RSC not yet available) |
| API proxy      | Server functions or Nitro proxy middleware               |
| Dev port       | 3301                                                     |

### Directory Structure

```
apps/demo-fe-ts-tanstackstart/
├── app/
│   ├── client.tsx                    # Client hydration entry
│   ├── router.tsx                    # Router configuration with QueryClient
│   ├── ssr.tsx                       # Server-side rendering handler
│   ├── routes/
│   │   ├── __root.tsx                # Root layout (nav, QueryClientProvider)
│   │   ├── index.tsx                 # Home / health status page
│   │   ├── login.tsx                 # Login form
│   │   ├── register.tsx              # Registration form
│   │   ├── profile.tsx               # User profile management
│   │   ├── admin/
│   │   │   └── index.tsx             # Admin panel (user listing)
│   │   ├── expenses/
│   │   │   ├── index.tsx             # Expense list + CRUD
│   │   │   ├── $id.tsx               # Expense detail + attachments
│   │   │   └── summary.tsx           # P&L reporting
│   │   └── tokens.tsx                # Token/session info
│   ├── routeTree.gen.ts              # Auto-generated route tree
│   ├── components/                   # Shared UI components (same structure as Next.js)
│   │   ├── ui/
│   │   ├── forms/
│   │   ├── layout/
│   │   └── data-display/
│   ├── lib/
│   │   ├── api/                      # API client functions (shared pattern)
│   │   ├── queries/                  # React Query hooks (identical to Next.js)
│   │   ├── auth/                     # Auth state management
│   │   ├── server/                   # Server functions
│   │   │   └── api-proxy.ts          # createServerFn for API proxying
│   │   └── utils/
│   └── test/
│       ├── setup.ts
│       └── mocks/
├── test/
│   └── unit/                         # @amiceli/vitest-cucumber step definitions
│       ├── steps/                    # Same structure as Next.js app
│       └── support/
├── app.config.ts                     # TanStack Start + Vite config
├── tsconfig.json
├── vitest.config.ts
├── project.json
├── package.json
└── README.md
```

### TanStack Start Configuration

```typescript
// app.config.ts
import { defineConfig } from "@tanstack/react-start/config";
import tsconfigPaths from "vite-tsconfig-paths";

export default defineConfig({
  vite: {
    plugins: [tsconfigPaths()],
  },
  server: {
    preset: "node-server", // or 'vercel' for deployment
  },
});
```

### API Proxy via Server Functions

```typescript
// app/lib/server/api-proxy.ts
import { createServerFn } from "@tanstack/react-start";

const backendUrl = process.env.BACKEND_URL || "http://localhost:8201";

export const apiProxy = createServerFn({ method: "GET" })
  .validator((input: { path: string; options?: RequestInit }) => input)
  .handler(async ({ data }) => {
    const response = await fetch(`${backendUrl}${data.path}`, data.options);
    return response.json();
  });
```

### Router + React Query Integration

```typescript
// app/router.tsx
import { createRouter as createTanStackRouter } from "@tanstack/react-router";
import { QueryClient } from "@tanstack/react-query";
import { routeTree } from "./routeTree.gen";

export function createRouter() {
  const queryClient = new QueryClient({
    defaultOptions: {
      queries: { staleTime: 60 * 1000, retry: 1 },
    },
  });

  return createTanStackRouter({
    routeTree,
    context: { queryClient },
    defaultPreload: "intent",
  });
}

declare module "@tanstack/react-router" {
  interface Register {
    router: ReturnType<typeof createRouter>;
  }
}
```

### Nx project.json

```json
{
  "name": "demo-fe-ts-tanstackstart",
  "$schema": "../../node_modules/nx/schemas/project-schema.json",
  "projectType": "application",
  "sourceRoot": "apps/demo-fe-ts-tanstackstart/app",
  "targets": {
    "dev": {
      "executor": "nx:run-commands",
      "options": {
        "command": "npx vinxi dev --port 3301",
        "cwd": "{projectRoot}"
      }
    },
    "build": {
      "executor": "nx:run-commands",
      "options": {
        "command": "npx vinxi build",
        "cwd": "{projectRoot}"
      },
      "outputs": ["{projectRoot}/.output"]
    },
    "start": {
      "executor": "nx:run-commands",
      "options": {
        "command": "node .output/server/index.mjs",
        "cwd": "{projectRoot}",
        "env": { "PORT": "3301" }
      }
    },
    "typecheck": {
      "executor": "nx:run-commands",
      "options": {
        "command": "tsc --noEmit",
        "cwd": "{projectRoot}"
      }
    },
    "lint": {
      "executor": "nx:run-commands",
      "options": {
        "command": "npx oxlint@latest .",
        "cwd": "{projectRoot}"
      }
    },
    "test:quick": {
      "executor": "nx:run-commands",
      "options": {
        "commands": [
          "npx vitest run --coverage && (cd ../../apps/rhino-cli && CGO_ENABLED=0 go run main.go test-coverage validate apps/demo-fe-ts-tanstackstart/coverage/lcov.info 90)",
          "(cd ../../apps/rhino-cli && go run main.go spec-coverage validate specs/apps/demo/fe apps/demo-fe-ts-tanstackstart)"
        ],
        "parallel": true,
        "cwd": "{projectRoot}"
      }
    },
    "test:unit": {
      "executor": "nx:run-commands",
      "options": {
        "command": "npx vitest run --project unit",
        "cwd": "{projectRoot}"
      }
    }
  },
  "tags": ["type:app", "platform:tanstackstart", "lang:ts", "domain:demo-fe"],
  "implicitDependencies": []
}
```

**Note on build/dev commands**: TanStack Start v1 RC migrated from Vinxi to Vite. The actual
commands may be `npx vite dev` / `npx vite build` instead of `vinxi`. Verify during implementation
by checking the version's CLI documentation. If using Vite directly, the output directory may be
`.output/` (Nitro) rather than `.next/`.

## App 3: demo-fe-ts-remix (React Router v7 / Remix)

### Framework Details

| Aspect    | Value                                                      |
| --------- | ---------------------------------------------------------- |
| Framework | React Router v7.x (framework mode — successor to Remix)    |
| React     | 19.x                                                       |
| Build     | Vite                                                       |
| Routing   | File-based routes (`app/routes/`) with loaders and actions |
| SSR       | Full-document SSR with loaders (data fetching on server)   |
| API proxy | Vite proxy in `vite.config.ts`                             |
| Config    | `react-router.config.ts`                                   |
| Dev port  | 3301                                                       |

### Architecture Notes

React Router v7 in "framework mode" is the successor to Remix. Key characteristics:

1. **Loaders/Actions**: Data loading via `loader()` and mutations via `action()` per route
2. **File-based routing**: Routes defined in `app/routes/` directory
3. **Vite-powered**: Uses Vite for dev server and production builds
4. **Package**: `react-router` (not `@remix-run/*` — fully merged into React Router)
5. **Config**: `react-router.config.ts` for framework configuration
6. **No separate server runtime**: Vite handles dev, Vite build for production

### Directory Structure

```
apps/demo-fe-ts-remix/
├── app/
│   ├── root.tsx                       # Root layout with QueryClientProvider
│   ├── routes/
│   │   ├── _index.tsx                 # Home / health status page
│   │   ├── login.tsx                  # Login form (loader + action)
│   │   ├── register.tsx               # Registration form (action)
│   │   ├── profile.tsx                # User profile management
│   │   ├── admin.tsx                  # Admin panel (user listing)
│   │   ├── expenses._index.tsx        # Expense list + CRUD
│   │   ├── expenses.$id.tsx           # Expense detail + attachments
│   │   ├── expenses.summary.tsx       # P&L reporting
│   │   └── tokens.tsx                 # Token/session info
│   ├── components/                    # Shared UI components
│   │   ├── ui/                        # Base UI primitives
│   │   ├── forms/                     # Form components
│   │   ├── layout/                    # Header, nav, sidebar, footer
│   │   └── data-display/             # Tables, cards, summary views
│   ├── lib/
│   │   ├── api/                       # API client functions
│   │   │   ├── client.ts              # Base fetch wrapper with auth headers
│   │   │   ├── auth.ts                # Auth API calls
│   │   │   ├── users.ts              # User API calls
│   │   │   ├── admin.ts              # Admin API calls
│   │   │   ├── expenses.ts           # Expense API calls
│   │   │   └── tokens.ts             # Token API calls
│   │   ├── queries/                   # React Query hooks
│   │   │   ├── use-auth.ts
│   │   │   ├── use-user.ts
│   │   │   ├── use-admin.ts
│   │   │   ├── use-expenses.ts
│   │   │   └── use-tokens.ts
│   │   ├── auth/                      # Auth state management
│   │   │   ├── auth-provider.tsx
│   │   │   └── auth-guard.tsx
│   │   └── utils/                     # Shared utilities
│   └── test/
│       ├── setup.ts                   # Vitest setup (jsdom, RTL cleanup)
│       ├── test-providers.tsx         # QueryClientProvider + AuthProvider wrapper
│       └── mocks/                     # Mock factories for API responses
├── test/
│   └── unit/                          # @amiceli/vitest-cucumber step definitions
│       ├── steps/                     # Gherkin step implementations by domain
│       └── support/                   # Test helpers, mock setup
├── react-router.config.ts             # React Router framework config
├── vite.config.ts                     # Vite config with API proxy
├── vitest.config.ts                   # Vitest + @amiceli/vitest-cucumber
├── tsconfig.json
├── project.json                       # Nx configuration
├── package.json
└── README.md
```

### React Router Configuration

```typescript
// react-router.config.ts
import type { Config } from "@react-router/dev/config";

export default {
  ssr: true,
  appDirectory: "app",
} satisfies Config;
```

### Vite Config with API Proxy

```typescript
// vite.config.ts
import { reactRouter } from "@react-router/dev/vite";
import { defineConfig } from "vite";
import tsconfigPaths from "vite-tsconfig-paths";

export default defineConfig({
  plugins: [reactRouter(), tsconfigPaths()],
  server: {
    port: 3301,
    proxy: {
      "/api": process.env.BACKEND_URL || "http://localhost:8201",
      "/health": process.env.BACKEND_URL || "http://localhost:8201",
      "/.well-known": process.env.BACKEND_URL || "http://localhost:8201",
    },
  },
});
```

### Loader/Action Pattern

```typescript
// app/routes/expenses._index.tsx
import type { Route } from "./+types/expenses._index";
import { useLoaderData } from "react-router";
import { useCreateExpense, useDeleteExpense } from "~/lib/queries/use-expenses";

export async function loader({ request }: Route.LoaderArgs) {
  // Server-side data fetching (SSR)
  return { title: "Expenses" };
}

export default function ExpensesPage() {
  const data = useLoaderData<typeof loader>();
  const createExpense = useCreateExpense();
  // React Query handles client-side data fetching + cache
  return <ExpenseList />;
}
```

### Nx project.json

```json
{
  "name": "demo-fe-ts-remix",
  "$schema": "../../node_modules/nx/schemas/project-schema.json",
  "projectType": "application",
  "sourceRoot": "apps/demo-fe-ts-remix/app",
  "targets": {
    "dev": {
      "executor": "nx:run-commands",
      "options": {
        "command": "npx react-router dev --port 3301",
        "cwd": "{projectRoot}"
      }
    },
    "build": {
      "executor": "nx:run-commands",
      "options": {
        "command": "npx react-router build",
        "cwd": "{projectRoot}"
      },
      "outputs": ["{projectRoot}/build"]
    },
    "start": {
      "executor": "nx:run-commands",
      "options": {
        "command": "npx react-router-serve build/server/index.js",
        "cwd": "{projectRoot}",
        "env": { "PORT": "3301" }
      }
    },
    "typecheck": {
      "executor": "nx:run-commands",
      "options": {
        "command": "npx react-router typegen && tsc --noEmit",
        "cwd": "{projectRoot}"
      }
    },
    "lint": {
      "executor": "nx:run-commands",
      "options": {
        "command": "npx oxlint@latest .",
        "cwd": "{projectRoot}"
      }
    },
    "test:quick": {
      "executor": "nx:run-commands",
      "options": {
        "commands": [
          "npx vitest run --coverage && (cd ../../apps/rhino-cli && CGO_ENABLED=0 go run main.go test-coverage validate apps/demo-fe-ts-remix/coverage/lcov.info 90)",
          "(cd ../../apps/rhino-cli && go run main.go spec-coverage validate specs/apps/demo/fe apps/demo-fe-ts-remix)"
        ],
        "parallel": true,
        "cwd": "{projectRoot}"
      }
    },
    "test:unit": {
      "executor": "nx:run-commands",
      "options": {
        "command": "npx vitest run --project unit",
        "cwd": "{projectRoot}"
      }
    }
  },
  "tags": ["type:app", "platform:reactrouter", "lang:ts", "domain:demo-fe"],
  "implicitDependencies": []
}
```

## App 4: demo-fe-dart-flutter (Flutter Web)

### Framework Details

| Aspect      | Value                                                   |
| ----------- | ------------------------------------------------------- |
| Framework   | Flutter 3.41.x (web-first)                              |
| Language    | Dart 3.11.x                                             |
| Renderer    | CanvasKit (default) / Skwasm (WASM, preferred for perf) |
| State mgmt  | Riverpod 3.0                                            |
| HTTP client | dio (with interceptors for auth)                        |
| Routing     | go_router                                               |
| Dev port    | 3301                                                    |

### Directory Structure

```
apps/demo-fe-dart-flutter/
├── lib/
│   ├── main.dart                     # Entry point
│   ├── app.dart                      # MaterialApp + GoRouter + ProviderScope
│   ├── core/
│   │   ├── api/                      # dio client, interceptors, error handling
│   │   │   ├── api_client.dart       # Base dio instance with auth interceptor
│   │   │   ├── auth_api.dart         # Auth API calls
│   │   │   ├── users_api.dart        # User API calls
│   │   │   ├── admin_api.dart        # Admin API calls
│   │   │   ├── expenses_api.dart     # Expense API calls
│   │   │   └── tokens_api.dart       # Token API calls
│   │   ├── providers/                # Riverpod providers
│   │   │   ├── auth_provider.dart    # Auth state + token refresh
│   │   │   ├── user_provider.dart    # User profile state
│   │   │   ├── admin_provider.dart   # Admin panel state
│   │   │   ├── expenses_provider.dart # Expense list + CRUD state
│   │   │   └── tokens_provider.dart  # Token claims state
│   │   ├── models/                   # Data models (freezed/json_serializable)
│   │   ├── router/                   # go_router configuration
│   │   └── theme/                    # Material theme, responsive breakpoints
│   ├── features/
│   │   ├── health/                   # Health status page
│   │   ├── auth/                     # Login, registration screens
│   │   ├── profile/                  # User profile, password change
│   │   ├── admin/                    # Admin panel
│   │   ├── expenses/                 # Expense CRUD, summary, attachments
│   │   ├── tokens/                   # Token info page
│   │   └── layout/                   # Responsive shell, navigation
│   └── shared/                       # Shared widgets (buttons, forms, tables)
├── test/
│   └── unit/                         # bdd_widget_test step definitions
│       ├── steps/                    # By domain (same structure as TS apps)
│       └── support/                  # Mock factories, test helpers
├── web/
│   ├── index.html
│   └── manifest.json
├── pubspec.yaml                      # Dependencies
├── analysis_options.yaml             # Dart linter (very_good_analysis)
├── build.yaml                        # build_runner config (bdd_widget_test)
├── project.json                      # Nx configuration
└── README.md
```

### API Client (dio)

```dart
// lib/core/api/api_client.dart
import 'package:dio/dio.dart';
import 'package:riverpod_annotation/riverpod_annotation.dart';

part 'api_client.g.dart';

@riverpod
Dio apiClient(ApiClientRef ref) {
  final dio = Dio(BaseOptions(baseUrl: ''));  // Proxied, no full URL needed

  dio.interceptors.add(InterceptorsWrapper(
    onRequest: (options, handler) {
      final token = ref.read(accessTokenProvider);
      if (token != null) {
        options.headers['Authorization'] = 'Bearer $token';
      }
      handler.next(options);
    },
    onError: (error, handler) async {
      if (error.response?.statusCode == 401) {
        // Attempt token refresh
        final refreshed = await ref.read(authProvider.notifier).refresh();
        if (refreshed) {
          // Retry original request
          handler.resolve(await dio.fetch(error.requestOptions));
          return;
        }
      }
      handler.next(error);
    },
  ));

  return dio;
}
```

### Nx project.json

```json
{
  "name": "demo-fe-dart-flutter",
  "$schema": "../../node_modules/nx/schemas/project-schema.json",
  "projectType": "application",
  "sourceRoot": "apps/demo-fe-dart-flutter/lib",
  "targets": {
    "dev": {
      "executor": "nx:run-commands",
      "options": {
        "command": "flutter run -d chrome --web-port 3301",
        "cwd": "{projectRoot}"
      }
    },
    "build": {
      "executor": "nx:run-commands",
      "options": {
        "command": "flutter build web --release",
        "cwd": "{projectRoot}"
      },
      "outputs": ["{projectRoot}/build/web"]
    },
    "start": {
      "executor": "nx:run-commands",
      "options": {
        "command": "npx serve build/web -l 3301",
        "cwd": "{projectRoot}"
      }
    },
    "lint": {
      "executor": "nx:run-commands",
      "options": {
        "command": "dart analyze",
        "cwd": "{projectRoot}"
      }
    },
    "test:quick": {
      "executor": "nx:run-commands",
      "options": {
        "commands": [
          "flutter test --coverage && (cd ../../apps/rhino-cli && CGO_ENABLED=0 go run main.go test-coverage validate apps/demo-fe-dart-flutter/coverage/lcov.info 90)",
          "(cd ../../apps/rhino-cli && go run main.go spec-coverage validate specs/apps/demo/fe apps/demo-fe-dart-flutter)"
        ],
        "parallel": true,
        "cwd": "{projectRoot}"
      }
    },
    "test:unit": {
      "executor": "nx:run-commands",
      "options": {
        "command": "flutter test",
        "cwd": "{projectRoot}"
      }
    }
  },
  "tags": ["type:app", "platform:flutter", "lang:dart", "domain:demo-fe"],
  "implicitDependencies": []
}
```

## App 5: demo-fe-elixir-phoenix (Phoenix LiveView)

### Framework Details

| Aspect      | Value                                                   |
| ----------- | ------------------------------------------------------- |
| Framework   | Phoenix 1.8.x + LiveView 1.1.x                          |
| Language    | Elixir 1.19+ / OTP 27                                   |
| Rendering   | Server-side HTML via WebSocket (no client JS framework) |
| State mgmt  | Server-side socket assigns (stateful BEAM processes)    |
| HTTP client | Req (calls demo-be API from server)                     |
| Assets      | esbuild + Tailwind CSS (Phoenix defaults)               |
| Dev port    | 3301                                                    |

### Architecture Notes

Phoenix LiveView is fundamentally different from the other frontends:

1. **Server-rendered**: All HTML is rendered on the server. No React/Vue/Angular needed.
2. **WebSocket diffs**: After initial HTTP render, a WebSocket sends minimal diffs on state changes.
3. **Stateful processes**: Each user session is a BEAM process (~40KB) holding socket assigns.
4. **No API proxy needed**: The LiveView server calls the demo-be API directly via Req from
   server-side code — no browser-to-API proxying required.
5. **SEO friendly**: Initial page load is fully rendered HTML (unlike Flutter Web's canvas).

### Directory Structure

```
apps/demo-fe-elixir-phoenix/
├── config/
│   ├── config.exs                    # Shared config
│   ├── dev.exs                       # Dev config (port 3301)
│   ├── test.exs                      # Test config
│   ├── prod.exs                      # Production config
│   └── runtime.exs                   # Runtime config (env vars)
├── lib/
│   ├── demo_fe_exph/                 # Business logic
│   │   ├── application.ex            # OTP application
│   │   └── api_client.ex             # Req-based demo-be API client
│   └── demo_fe_exph_web/             # Web layer
│       ├── components/               # Reusable function components
│       │   ├── core_components.ex    # Forms, buttons, tables, modals
│       │   └── layouts/
│       │       ├── app.html.heex     # App layout (nav, sidebar)
│       │       └── root.html.heex    # Root HTML layout
│       ├── live/                     # LiveView modules
│       │   ├── health_live.ex        # Health status page
│       │   ├── login_live.ex         # Login form
│       │   ├── register_live.ex      # Registration form
│       │   ├── profile_live.ex       # User profile management
│       │   ├── admin_live.ex         # Admin panel (user listing)
│       │   ├── expenses_live.ex      # Expense list + CRUD
│       │   ├── expense_detail_live.ex # Expense detail + attachments
│       │   ├── summary_live.ex       # P&L reporting
│       │   └── tokens_live.ex        # Token/session info
│       ├── endpoint.ex               # HTTP endpoint
│       ├── router.ex                 # Routes
│       └── telemetry.ex              # Telemetry
├── assets/
│   ├── js/
│   │   ├── app.js                    # LiveSocket setup, hooks
│   │   └── hooks/                    # Custom JS hooks (file upload, clipboard)
│   └── css/
│       └── app.css                   # Tailwind CSS
├── test/
│   ├── demo_fe_exph/                 # Business logic tests
│   ├── demo_fe_exph_web/
│   │   └── live/                     # LiveViewTest tests
│   └── unit/                         # Cabbage BDD step definitions
│       ├── steps/                    # By domain
│       └── support/                  # Test helpers, mock API responses
├── mix.exs                           # Dependencies
├── mix.lock                          # Dependency lock
├── coveralls.json                    # Coverage config (excoveralls)
├── .credo.exs                        # Credo linter config
├── .formatter.exs                    # Code formatter config
├── project.json                      # Nx configuration
└── README.md
```

### API Client (Req from server-side)

```elixir
# lib/demo_fe_exph/api_client.ex
defmodule DemoFeExph.ApiClient do
  @backend_url System.get_env("BACKEND_URL", "http://localhost:8201")

  def get_health do
    Req.get!("#{@backend_url}/health").body
  end

  def login(email, password) do
    Req.post!("#{@backend_url}/api/v1/auth/login",
      json: %{email: email, password: password}
    )
  end

  def list_expenses(token) do
    Req.get!("#{@backend_url}/api/v1/expenses",
      headers: [{"authorization", "Bearer #{token}"}]
    ).body
  end

  # ... other API calls
end
```

### LiveView Pattern

```elixir
# lib/demo_fe_exph_web/live/expenses_live.ex
defmodule DemoFeExphWeb.ExpensesLive do
  use DemoFeExphWeb, :live_view

  def mount(_params, session, socket) do
    token = session["access_token"]
    expenses = DemoFeExph.ApiClient.list_expenses(token)

    {:ok, assign(socket,
      expenses: expenses,
      token: token,
      page_title: "Expenses"
    )}
  end

  def handle_event("delete", %{"id" => id}, socket) do
    case DemoFeExph.ApiClient.delete_expense(socket.assigns.token, id) do
      {:ok, _} ->
        expenses = DemoFeExph.ApiClient.list_expenses(socket.assigns.token)
        {:noreply, assign(socket, expenses: expenses)
         |> put_flash(:info, "Expense deleted")}
      {:error, reason} ->
        {:noreply, put_flash(socket, :error, "Failed: #{reason}")}
    end
  end
end
```

### Nx project.json

```json
{
  "name": "demo-fe-elixir-phoenix",
  "$schema": "../../node_modules/nx/schemas/project-schema.json",
  "projectType": "application",
  "sourceRoot": "apps/demo-fe-elixir-phoenix/lib",
  "targets": {
    "dev": {
      "executor": "nx:run-commands",
      "options": {
        "command": "mix phx.server",
        "cwd": "{projectRoot}",
        "env": { "PORT": "3301" }
      }
    },
    "build": {
      "executor": "nx:run-commands",
      "options": {
        "command": "mix compile",
        "cwd": "{projectRoot}"
      }
    },
    "start": {
      "executor": "nx:run-commands",
      "options": {
        "command": "mix phx.server",
        "cwd": "{projectRoot}",
        "env": { "MIX_ENV": "prod", "PORT": "3301" }
      }
    },
    "lint": {
      "executor": "nx:run-commands",
      "options": {
        "command": "mix credo --strict",
        "cwd": "{projectRoot}"
      }
    },
    "test:quick": {
      "executor": "nx:run-commands",
      "options": {
        "commands": [
          "MIX_ENV=test mix coveralls.lcov && (cd ../../apps/rhino-cli && CGO_ENABLED=0 go run main.go test-coverage validate apps/demo-fe-elixir-phoenix/cover/lcov.info 90)",
          "(cd ../../apps/rhino-cli && go run main.go spec-coverage validate specs/apps/demo/fe apps/demo-fe-elixir-phoenix)"
        ],
        "parallel": false,
        "cwd": "{projectRoot}"
      }
    },
    "test:unit": {
      "executor": "nx:run-commands",
      "options": {
        "command": "MIX_ENV=test mix coveralls.lcov",
        "cwd": "{projectRoot}"
      }
    }
  },
  "tags": ["type:app", "platform:phoenix", "lang:elixir", "domain:demo-fe"],
  "implicitDependencies": []
}
```

## App 6: demo-fe-e2e (Centralized E2E Suite)

### Architecture

Mirrors `demo-be-e2e` exactly — a standalone Playwright project that tests any frontend
implementation via `BASE_URL`.

### Directory Structure

```
apps/demo-fe-e2e/
├── tests/
│   ├── steps/                        # Step definitions by domain
│   │   ├── common.steps.ts           # Shared steps (app running, navigation)
│   │   ├── common-setup.steps.ts     # Background steps, auth helpers
│   │   ├── health/
│   │   │   └── health.steps.ts
│   │   ├── auth/
│   │   │   ├── login.steps.ts
│   │   │   └── session.steps.ts
│   │   ├── user/
│   │   │   ├── registration.steps.ts
│   │   │   └── user-profile.steps.ts
│   │   ├── security/
│   │   │   └── security.steps.ts
│   │   ├── token-management/
│   │   │   └── tokens.steps.ts
│   │   ├── admin/
│   │   │   └── admin-panel.steps.ts
│   │   ├── expenses/
│   │   │   ├── expenses.steps.ts
│   │   │   ├── currency.steps.ts
│   │   │   ├── units.steps.ts
│   │   │   ├── reporting.steps.ts
│   │   │   └── attachments.steps.ts
│   │   └── layout/
│   │       ├── responsive.steps.ts
│   │       └── accessibility.steps.ts
│   ├── hooks/
│   │   └── cleanup.hooks.ts          # Calls POST /api/v1/test/reset-db between scenarios
│   ├── fixtures/
│   │   └── test-api.ts               # Test API client (reset-db, promote-admin)
│   ├── utils/
│   │   ├── token-store.ts            # JWT token state management
│   │   ├── response-store.ts         # Response state for assertions
│   │   └── page-helpers.ts           # Page interaction helpers
│   └── pages/                        # Page Object Model (optional)
│       ├── login.page.ts
│       ├── register.page.ts
│       ├── profile.page.ts
│       ├── admin.page.ts
│       ├── expenses.page.ts
│       └── nav.page.ts
├── playwright.config.ts
├── tsconfig.json
├── project.json
├── package.json
├── .gitignore                        # .features-gen/, test-results/, playwright-report/
└── README.md
```

### Playwright Configuration

```typescript
// playwright.config.ts
import { defineConfig } from "@playwright/test";
import { defineBddConfig } from "playwright-bdd";

const testDir = defineBddConfig({
  featuresRoot: "../../specs/apps/demo/fe/gherkin",
  features: "../../specs/apps/demo/fe/gherkin/**/*.feature",
  steps: ["./tests/steps/**/*.ts", "./tests/hooks/**/*.ts"],
});

export default defineConfig({
  testDir,
  fullyParallel: false,
  forbidOnly: !!process.env.CI,
  retries: process.env.CI ? 2 : 0,
  workers: 1,
  reporter: [["html"], ["junit", { outputFile: "test-results/junit.xml" }]],
  use: {
    baseURL: process.env.BASE_URL || "http://localhost:3301",
    trace: "on-first-retry",
    screenshot: "only-on-failure",
    viewport: { width: 1280, height: 720 },
  },
  projects: [{ name: "chromium", use: { browserName: "chromium" } }],
});
```

### Nx project.json

```json
{
  "name": "demo-fe-e2e",
  "$schema": "../../node_modules/nx/schemas/project-schema.json",
  "projectType": "application",
  "sourceRoot": "apps/demo-fe-e2e/tests",
  "targets": {
    "install": {
      "executor": "nx:run-commands",
      "options": {
        "command": "npm install",
        "cwd": "apps/demo-fe-e2e"
      }
    },
    "lint": {
      "executor": "nx:run-commands",
      "options": {
        "command": "npx oxlint@latest .",
        "cwd": "apps/demo-fe-e2e"
      }
    },
    "typecheck": {
      "executor": "nx:run-commands",
      "options": {
        "command": "npx bddgen && npx tsc --noEmit",
        "cwd": "apps/demo-fe-e2e"
      }
    },
    "test:quick": {
      "executor": "nx:run-commands",
      "options": {
        "commands": ["npx oxlint@latest .", "npx bddgen && npx tsc --noEmit"],
        "parallel": true,
        "cwd": "apps/demo-fe-e2e"
      }
    },
    "test:e2e": {
      "executor": "nx:run-commands",
      "options": {
        "command": "npx bddgen && npx playwright test",
        "cwd": "apps/demo-fe-e2e"
      }
    },
    "test:e2e:ui": {
      "executor": "nx:run-commands",
      "options": {
        "command": "npx bddgen && npx playwright test --ui",
        "cwd": "apps/demo-fe-e2e"
      }
    },
    "test:e2e:report": {
      "executor": "nx:run-commands",
      "options": {
        "command": "npx playwright show-report",
        "cwd": "apps/demo-fe-e2e"
      }
    }
  },
  "tags": ["type:e2e", "platform:playwright", "lang:ts", "domain:demo-fe"],
  "implicitDependencies": [
    "demo-fe-ts-nextjs",
    "demo-fe-ts-tanstackstart",
    "demo-fe-ts-remix",
    "demo-fe-dart-flutter",
    "demo-fe-elixir-phoenix"
  ]
}
```

## Shared Patterns

### React Query Hook Pattern (shared across all three TypeScript frontends)

```typescript
// lib/queries/use-expenses.ts
import { useQuery, useMutation, useQueryClient, queryOptions } from "@tanstack/react-query";
import * as expensesApi from "../api/expenses";

export const expenseListOptions = (token: string) =>
  queryOptions({
    queryKey: ["expenses"],
    queryFn: () => expensesApi.listExpenses(token),
  });

export function useExpenses(token: string) {
  return useQuery(expenseListOptions(token));
}

export function useCreateExpense(token: string) {
  const queryClient = useQueryClient();
  return useMutation({
    mutationFn: (data: CreateExpenseInput) => expensesApi.createExpense(token, data),
    onSuccess: () => {
      queryClient.invalidateQueries({ queryKey: ["expenses"] });
    },
  });
}
```

### API Client Pattern (shared across all three TypeScript frontends)

```typescript
// lib/api/client.ts
const BASE_URL = ""; // Proxied — no need for full URL

export class ApiError extends Error {
  constructor(
    public status: number,
    public body: unknown,
  ) {
    super(`API error ${status}`);
  }
}

export async function apiFetch<T>(path: string, options?: RequestInit): Promise<T> {
  const response = await fetch(`${BASE_URL}${path}`, {
    ...options,
    headers: {
      "Content-Type": "application/json",
      ...options?.headers,
    },
  });
  if (!response.ok) {
    throw new ApiError(response.status, await response.json());
  }
  return response.json();
}
```

### Unit Test BDD Pattern (@amiceli/vitest-cucumber + React Testing Library)

The unit tests use `@amiceli/vitest-cucumber` which maps `.feature` files to Vitest test cases:

1. `loadFeature()` loads a `.feature` file
2. `describeFeature()` maps the feature to a Vitest `describe` block
3. `Scenario` maps to Vitest `describe`, steps (`Given`/`When`/`Then`) map to Vitest `test`
4. Strict validation: missing or extra steps cause test failures
5. Step definitions render components with `@testing-library/react` in jsdom
6. User interactions via `@testing-library/user-event` (clicks, types, selects)
7. Assertions via RTL queries (`getByRole`, `getByText`, `getByLabelText`)
8. All API calls are mocked via `vi.mock` — no real HTTP

```typescript
// test/unit/steps/auth/login.spec.ts
import { loadFeature, describeFeature } from "@amiceli/vitest-cucumber";
import { render, screen } from "@testing-library/react";
import userEvent from "@testing-library/user-event";
import { LoginPage } from "../../src/pages/login";
import { TestProviders } from "../../src/test/test-providers";

const feature = await loadFeature(
  "specs/apps/demo/fe/gherkin/authentication/login.feature",
);

describeFeature(feature, ({ Scenario }) => {
  Scenario("Successful login", ({ Given, When, Then }) => {
    Given("the app is running", () => {
      vi.mocked(api.getHealth).mockResolvedValue({ status: "UP" });
    });

    Given("the user is on the login page", () => {
      render(<LoginPage />, { wrapper: TestProviders });
    });

    When("the user enters email {string} and password {string}", async (email, password) => {
      const user = userEvent.setup();
      await user.type(screen.getByLabelText(/email/i), email);
      await user.type(screen.getByLabelText(/password/i), password);
    });

    When("the user clicks the login button", async () => {
      const user = userEvent.setup();
      await user.click(screen.getByRole("button", { name: /login/i }));
    });

    Then("the user should see the dashboard", () => {
      expect(screen.getByText(/dashboard/i)).toBeInTheDocument();
    });
  });
});
```

**Key RTL principles for step definitions**:

- Query by accessibility role (`getByRole`) first, then label (`getByLabelText`), then text (`getByText`) — never by test ID unless unavoidable
- Use `userEvent` (not `fireEvent`) for realistic user interactions
- Assert on what the user sees, not internal component state
- Wrap renders with `TestProviders` (QueryClientProvider, AuthProvider, router)

**Spec file generation**: Use the CLI to scaffold step definitions from feature files:

```bash
npx @amiceli/vitest-cucumber --feature specs/apps/demo/fe/gherkin/authentication/login.feature --spec test/unit/steps/auth/login.spec.ts
```

### E2E Step Pattern (playwright-bdd)

```typescript
// tests/steps/auth/login.steps.ts
import { createBdd } from "playwright-bdd";
import { expect, test } from "@playwright/test";

const { Given, When, Then } = createBdd(test);

Given("the user is on the login page", async ({ page }) => {
  await page.goto("/login");
});

When("the user enters email {string} and password {string}", async ({ page }, email: string, password: string) => {
  await page.fill('[name="email"]', email);
  await page.fill('[name="password"]', password);
});

When("the user clicks the login button", async ({ page }) => {
  await page.click('button[type="submit"]');
});

Then("the user should see the dashboard", async ({ page }) => {
  await expect(page.locator("h1")).toContainText("Dashboard");
});
```

## Docker Compose (E2E Infrastructure)

### demo-fe-ts-nextjs

```yaml
# infra/dev/demo-fe-ts-nextjs/docker-compose.yml
services:
  db:
    image: postgres:17-alpine
    environment:
      POSTGRES_USER: demo_be
      POSTGRES_PASSWORD: demo_be
      POSTGRES_DB: demo_be
    ports:
      - "5432:5432"
    healthcheck:
      test: ["CMD-SHELL", "pg_isready -U demo_be"]
      interval: 5s
      timeout: 5s
      retries: 5

  backend:
    build:
      context: ../../../apps/demo-be-java-springboot
    environment:
      DATABASE_URL: postgresql://demo_be:demo_be@db:5432/demo_be
      APP_JWT_SECRET: test-secret-key-for-e2e-testing-only-min-32-chars
      ENABLE_TEST_API: "true"
    ports:
      - "8201:8201"
    depends_on:
      db:
        condition: service_healthy

  frontend:
    build:
      context: ../../..
      dockerfile: apps/demo-fe-ts-nextjs/Dockerfile
    environment:
      BACKEND_URL: http://backend:8201
    ports:
      - "3301:3301"
    depends_on:
      - backend
```

### demo-fe-ts-tanstackstart

Same pattern, substituting the frontend service with the TanStack Start app on port 3301.

## GitHub Actions Workflows

### Scheduling

Demo-fe E2E workflows run on a 2x daily schedule, **1 hour after** the demo-be integration + E2E
workflows to allow backend CI to finish first:

| Workflow Set | UTC Cron       | WIB Time   |
| ------------ | -------------- | ---------- |
| demo-be-\*   | `0 23`, `0 11` | 6 AM, 6 PM |
| demo-fe-\*   | `0 0`, `0 12`  | 7 AM, 7 PM |

### E2E Workflow Pattern

```yaml
# .github/workflows/e2e-demo-fe-ts-nextjs.yml
name: E2E - demo-fe-ts-nextjs

on:
  schedule:
    - cron: "0 0 * * *" # 7 AM WIB (UTC+7) — 1h after demo-be workflows
    - cron: "0 12 * * *" # 7 PM WIB (UTC+7)
  push:
    branches: [main]
    paths:
      - "apps/demo-fe-ts-nextjs/**"
      - "apps/demo-fe-e2e/**"
      - "specs/apps/demo/fe/**"
  workflow_dispatch:

jobs:
  e2e:
    runs-on: ubuntu-latest
    services:
      postgres:
        image: postgres:17-alpine
        env:
          POSTGRES_USER: demo_be
          POSTGRES_PASSWORD: demo_be
          POSTGRES_DB: demo_be
        ports:
          - 5432:5432
        options: >-
          --health-cmd "pg_isready -U demo_be"
          --health-interval 10s
          --health-timeout 5s
          --health-retries 5

    steps:
      - uses: actions/checkout@v4

      - name: Setup Node.js
        uses: actions/setup-node@v4
        with:
          node-version-file: "package.json"

      - name: Setup Java (for backend)
        uses: actions/setup-java@v4
        with:
          distribution: "temurin"
          java-version: "25"

      - name: Install dependencies
        run: npm ci

      - name: Build and start backend
        run: |
          cd apps/demo-be-java-springboot
          ./mvnw -q package -DskipTests
          java -jar target/*.jar &
        env:
          DATABASE_URL: postgresql://demo_be:demo_be@localhost:5432/demo_be
          APP_JWT_SECRET: test-secret-key-for-e2e-testing-only-min-32-chars
          ENABLE_TEST_API: "true"

      - name: Build and start frontend
        run: |
          nx build demo-fe-ts-nextjs
          nx start demo-fe-ts-nextjs &
        env:
          BACKEND_URL: http://localhost:8201

      - name: Wait for services
        run: |
          npx wait-on http://localhost:8201/health http://localhost:3301 --timeout 60000

      - name: Install Playwright browsers
        run: npx playwright install --with-deps chromium
        working-directory: apps/demo-fe-e2e

      - name: Run E2E tests
        run: nx run demo-fe-e2e:test:e2e
        env:
          BASE_URL: http://localhost:3301

      - name: Upload test artifacts
        if: failure()
        uses: actions/upload-artifact@v4
        with:
          name: e2e-results-nextjs
          path: |
            apps/demo-fe-e2e/test-results/
            apps/demo-fe-e2e/playwright-report/
```

## Key Technical Decisions

### Decision 1: Two-Level Testing (No Integration)

**Rationale**: Frontend integration testing (rendering components with mocked API) overlaps heavily
with unit testing for component logic. The gap between "mock API at fetch level" (unit) and
"drive real browser" (E2E) is smaller for frontends than backends. Two levels provide sufficient
confidence without the overhead of a third.

### Decision 2: @amiceli/vitest-cucumber for Unit Tests

**Rationale**: `@amiceli/vitest-cucumber` (88 GitHub stars, ~6,430 downloads/week, last published
2026-03-08) is the most actively maintained Vitest-native Gherkin BDD library. It provides strict
validation — missing steps cause test failures, which aligns with our spec-coverage requirements.
A CLI tool generates spec stubs from `.feature` files. Step definitions use React Testing Library
(`@testing-library/react` + `@testing-library/user-event`) to render components in jsdom and
simulate user interactions. This was chosen over Cucumber.js (which would need a custom Vitest
adapter) and QuickPickle (smaller community, no published RTL companion).

### Decision 3: playwright-bdd for E2E (Matching demo-be-e2e)

**Rationale**: `demo-be-e2e` already uses playwright-bdd v8 with `defineBddConfig`. Reusing the
same approach keeps E2E patterns consistent. The `featuresRoot` config points to
`specs/apps/demo/fe/gherkin/` instead of `be/gherkin/`.

### Decision 4: Shared React Query Hooks

**Rationale**: All three TypeScript frontends use React Query v5. The query hooks (`use-expenses.ts`,
`use-auth.ts`, etc.) contain pure React Query logic with no framework-specific code. These could
potentially be extracted to a shared lib (`libs/ts-demo-fe-queries`) in the future, but initially
each app maintains its own copy to avoid premature abstraction.

### Decision 5: TanStack Start RC Maturity Risk

**Rationale**: TanStack Start is at v1 RC (v1.166.x), not yet GA. The API is considered stable
and the framework is feature-complete for non-RSC use cases. The demo app is low-risk — if the
API changes before 1.0, migration effort is bounded. This also serves as a real-world evaluation
of the framework.

### Decision 6: Default Backend for E2E

**Rationale**: E2E workflows use `demo-be-java-springboot` as the backend because it is the most
mainstream implementation (Spring Boot is the most widely adopted Java framework).

### Decision 7: Flutter Web — CanvasKit Renderer with Skwasm Target

**Rationale**: CanvasKit is the stable default renderer. Skwasm (WASM-based) offers 40% faster
loading but requires COOP/COEP headers. Start with CanvasKit, migrate to Skwasm when server
headers are configured. Flutter Web is not ideal for SEO but fits this demo app (authenticated
dashboard, not a public marketing site).

### Decision 8: Phoenix LiveView — Server-Side API Calls

**Rationale**: Unlike the other frontends where the browser calls the API via a proxy, LiveView
calls the demo-be API directly from the server using Req. This is the natural LiveView pattern —
state lives on the server, so API calls happen there too. No API proxy configuration needed.
The trade-off is that all traffic routes through the LiveView server (no direct browser-to-API).

### Decision 9: Cabbage for LiveView BDD (Reuse Existing Patch)

**Rationale**: This repo already uses Cabbage for `demo-be-elixir-phoenix` with a patched
`background_steps` fix. Reusing the same library avoids introducing another BDD tool. The
Cabbage step definitions call `Phoenix.LiveViewTest` functions instead of HTTP endpoints,
testing the same Gherkin specs at the widget/LiveView test level.

### Decision 10: bdd_widget_test for Flutter BDD

**Rationale**: `bdd_widget_test` generates standard `flutter_test` widget tests from Gherkin
`.feature` files via `build_runner`. This is compatible with `flutter test --coverage` for LCOV
output, which `rhino-cli test-coverage validate` can consume. The alternative (`flutter_gherkin`)
is a runtime runner that doesn't integrate as cleanly with Flutter's coverage tooling.

### Decision 11: E2E Test API — No Direct Database Access

**Rationale**: Unlike `demo-be-e2e` (which connects directly to PostgreSQL for cleanup and admin
role promotion), `demo-fe-e2e` must NOT access the backend database directly. The frontend E2E
tests interact exclusively through HTTP APIs. This requires test-only API endpoints on the
backend for operations that have no public API equivalent.

**Test-only endpoints** (gated behind `ENABLE_TEST_API=true` environment variable):

| Endpoint                          | Purpose                               | Why Not Public                                    |
| --------------------------------- | ------------------------------------- | ------------------------------------------------- |
| `POST /api/v1/test/reset-db`      | Delete all data between E2E scenarios | Destructive — wipes entire database               |
| `POST /api/v1/test/promote-admin` | Promote a user to ADMIN role          | Privilege escalation — bypasses normal admin flow |

**Public API coverage** — most E2E setup uses existing public endpoints:

| Setup Need         | Public API Used                          |
| ------------------ | ---------------------------------------- |
| Register a user    | `POST /api/v1/auth/register`             |
| Log in             | `POST /api/v1/auth/login`                |
| Create expenses    | `POST /api/v1/expenses`                  |
| Deactivate account | `POST /api/v1/users/me/deactivate`       |
| Upload attachments | `POST /api/v1/expenses/{id}/attachments` |

**Security**: Test-only endpoints are:

1. **Disabled by default** — only enabled when `ENABLE_TEST_API=true` is set
2. **Not registered in production** — the test API controller/router is conditionally loaded
3. **Prefixed with `/api/v1/test/`** — clearly separated from production endpoints
4. **No authentication required** — they run in a controlled test environment only

**Impact on specs**: New Gherkin feature file `specs/apps/demo/be/gherkin/test-support/test-api.feature`
documents the test-only endpoints. This ensures all demo-be backends implement them consistently.

**Impact on demo-be backends**: Each backend must add the test-only controller/router behind
the `ENABLE_TEST_API` flag. The E2E Docker Compose sets this variable for the backend service.

### Decision 12: React Router v7 Framework Mode (Remix Successor)

**Rationale**: React Router v7 in "framework mode" is the official successor to Remix. The Remix
team merged Remix into React Router, making `react-router` the single package for both routing
and full-stack framework features (loaders, actions, SSR). This gives us a Vite-based React
framework with a different data-loading paradigm (loader/action per route) compared to Next.js
(RSC/App Router) and TanStack Start (type-safe router + server functions). The three TypeScript
frontends now showcase three distinct approaches to React full-stack development.
