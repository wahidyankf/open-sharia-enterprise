---
title: Next.js with TypeScript Framework
description: Full-stack React framework for building production-ready web applications with server-side rendering, static generation, and optimized performance
category: explanation
subcategory: platform-web
tags:
  - nextjs
  - react
  - typescript
  - frontend
  - framework
  - ssr
  - ssg
  - server-components
  - index
principles:
  - automation-over-manual
  - explicit-over-implicit
  - immutability
  - pure-functions
  - reproducibility
created: 2026-01-29
updated: 2026-01-29
---

# Next.js with TypeScript Framework

**Understanding-oriented documentation** for Next.js with TypeScript framework in the open-sharia-enterprise platform.

## Overview

Next.js is a React framework that extends React with powerful features for building production-ready web applications. It provides server-side rendering (SSR), static site generation (SSG), incremental static regeneration (ISR), and React Server Components out of the box. Combined with TypeScript, Next.js enables building full-stack, type-safe applications with excellent performance and developer experience.

This documentation covers Next.js 14+ with TypeScript 5+ and focuses on the App Router (React Server Components architecture), which is the recommended approach for new applications.

**Version**: Next.js 14+ (targeting latest stable release)
**TypeScript Version**: TypeScript 5+
**React Version**: React 18+
**Node.js Version**: Node.js 18+

**Important**: Next.js is built on React. Before working with Next.js, ensure you understand React fundamentals from [React with TypeScript documentation](../fe-react/README.md).

## Framework Standards

**This documentation is the authoritative reference** for Next.js with TypeScript usage standards in the open-sharia-enterprise platform.

All Next.js applications MUST follow the patterns and practices documented here:

1. **[Idioms](./ex-so-plwe-fene__idioms.md)** - Next.js + TypeScript-specific patterns
2. **[Best Practices](./ex-so-plwe-fene__best-practices.md)** - Framework standards
3. **[Anti-Patterns](./ex-so-plwe-fene__anti-patterns.md)** - Common mistakes to avoid
4. **[Routing](./ex-so-plwe-fene__routing.md)** - App Router file-based routing
5. **[Server Components](./ex-so-plwe-fene__server-components.md)** - React Server Components patterns
6. **[Rendering Strategies](./ex-so-plwe-fene__rendering-strategies.md)** - SSR, SSG, ISR, CSR
7. **[Configuration](./ex-so-plwe-fene__configuration.md)** - Next.js configuration

**For Agents**: Reference this documentation when building Next.js applications.

**Language Standards**: Also follow [TypeScript](../../prog-lang/typescript/README.md) and [React](../fe-react/README.md) standards.

### Quick Standards Reference

- **Project Structure**: See [Architecture Integration](#architecture-integration)
- **Routing**: See [Routing](./ex-so-plwe-fene__routing.md)
- **Server Components**: See [Server Components](./ex-so-plwe-fene__server-components.md)
- **Data Fetching**: See [Data Fetching](./ex-so-plwe-fene__data-fetching.md)
- **Security**: See [Security](./ex-so-plwe-fene__security.md)
- **Testing**: See [Testing](./ex-so-plwe-fene__testing.md)

## Software Engineering Principles

Next.js with TypeScript usage in this platform follows the five software engineering principles from [governance/principles/software-engineering/](../../../../../governance/principles/software-engineering/README.md):

1. **[Automation Over Manual](../../../../../governance/principles/software-engineering/automation-over-manual.md)** - Next.js automates through automatic code splitting, image optimization, font optimization, TypeScript compilation, Fast Refresh, and build optimization
2. **[Explicit Over Implicit](../../../../../governance/principles/software-engineering/explicit-over-implicit.md)** - Next.js enforces through explicit file-based routing, clear rendering boundaries (Server vs Client Components), explicit data fetching, visible configuration
3. **[Immutability Over Mutability](../../../../../governance/principles/software-engineering/immutability.md)** - Use immutable state updates, React Server Components (inherently immutable), readonly TypeScript types, functional patterns
4. **[Pure Functions Over Side Effects](../../../../../governance/principles/software-engineering/pure-functions.md)** - Apply functional component model: pure Server Components, side effects in Client Components with hooks, Server Actions for mutations
5. **[Reproducibility First](../../../../../governance/principles/software-engineering/reproducibility.md)** - Next.js enables through version pinning (package-lock.json), next.config.ts configuration, environment-specific builds, Docker support, deterministic builds

**See Also**: [Server Components](./ex-so-plwe-fene__server-components.md) for functional patterns with Next.js.

## Quick Reference

**Jump to:**

- [Overview](#overview) - Next.js with TypeScript in the platform
- [Software Engineering Principles](#software-engineering-principles) - Five core principles
- [Next.js Version Strategy](#nextjs-version-strategy) - Version timeline and management
- [Documentation Structure](#documentation-structure) - Guide to documentation files
- [Key Capabilities](#key-capabilities) - Framework features
- [Use Cases](#use-cases) - When to use Next.js
- [Architecture Integration](#architecture-integration) - Application architecture
- [Development Workflow](#development-workflow) - Setup, configuration, testing
- [Learning Path](#learning-path) - Recommended reading order
- [Code Examples from Platform](#code-examples-from-platform) - Real implementations
- [Integration with Other Documentation](#integration-with-other-documentation) - Cross-references
- [Next.js Ecosystem](#nextjs-ecosystem) - Next.js plugins and tools
- [Tools and Development Environment](#tools-and-development-environment) - Build tools, IDEs
- [Resources and References](#resources-and-references) - External resources
- [Related Documentation](#related-documentation) - Cross-references

**Core Documentation:**

- [Idioms](./ex-so-plwe-fene__idioms.md) - Next.js + TypeScript patterns
- [Best Practices](./ex-so-plwe-fene__best-practices.md) - Framework coding standards
- [Anti-Patterns](./ex-so-plwe-fene__anti-patterns.md) - Common Next.js mistakes
- [Routing](./ex-so-plwe-fene__routing.md) - App Router file-based routing system
- [Server Components](./ex-so-plwe-fene__server-components.md) - React Server Components architecture
- [Rendering Strategies](./ex-so-plwe-fene__rendering-strategies.md) - SSR, SSG, ISR, CSR explained
- [Configuration](./ex-so-plwe-fene__configuration.md) - Next.js config, environment variables
- [Data Fetching](./ex-so-plwe-fene__data-fetching.md) - Server and client data fetching patterns
- [REST APIs](./ex-so-plwe-fene__rest-apis.md) - API routes and external API consumption
- [Security](./ex-so-plwe-fene__security.md) - XSS prevention, CSRF, authentication
- [Testing](./ex-so-plwe-fene__testing.md) - Unit, integration, E2E testing
- [Observability](./ex-so-plwe-fene__observability.md) - Error tracking, monitoring, analytics
- [Performance](./ex-so-plwe-fene__performance.md) - Optimization strategies
- [Deployment](./ex-so-plwe-fene__deployment.md) - Vercel, Docker, self-hosted deployment
- [Version Migration](./ex-so-plwe-fene__version-migration.md) - Upgrading Next.js versions

## Next.js Version Strategy

```mermaid
%% Color Palette: Blue #0173B2, Orange #DE8F05, Teal #029E73, Purple #CC78BC
%% All colors are color-blind friendly and meet WCAG AA contrast standards

timeline
    title Next.js Version Timeline (2016-2026)
    2016-10 : Next.js 1.0 : Initial release : SSR for React
    2018-09 : Next.js 7 : DX improvements : Improved build performance
    2020-07 : Next.js 9.5 : ISR introduced : Incremental Static Regeneration
    2021-06 : Next.js 11 : Image optimization : Script optimization
    2022-10 : Next.js 13.0 : App Router beta : React Server Components : Turbopack
    2023-05 : Next.js 13.4 : App Router stable : Server Actions alpha
    2024-05 : Next.js 14.0 : Server Actions stable : Partial Prerendering preview
    2025-Q2 : Next.js 15 (Planned) : Enhanced RSC : Partial Prerendering stable
```

**Platform Strategy**: Next.js 14+ (current) → Next.js 15 (future)

### Current Version: Next.js 14+

**Platform Standard**: Next.js 14 is the target version for all Next.js projects.

**Rationale**:

- Stable App Router with React Server Components
- Stable Server Actions for mutations
- Turbopack for faster local development (alpha)
- Enhanced image optimization
- Improved TypeScript support
- Partial Prerendering (preview)
- Better caching strategies

**Key Features**:

- **App Router** - File-based routing with React Server Components
- **Server Components** - Server-side rendering by default
- **Server Actions** - Type-safe mutations without API routes
- **Streaming** - Progressive HTML streaming
- **Metadata API** - Type-safe SEO and social sharing
- **Image Optimization** - Automatic responsive images
- **Font Optimization** - Zero layout shift with Google Fonts
- **Partial Prerendering** - Static shell with dynamic content (preview)

### Future: Next.js 15

**Expected**: Q2-Q3 2026 (speculative)

**Anticipated Changes**:

- Partial Prerendering as stable feature
- Enhanced React Server Components
- Improved caching primitives
- Better Turbopack integration
- Enhanced middleware capabilities
- Improved dev server performance

## Documentation Structure

### [Next.js with TypeScript Idioms](./ex-so-plwe-fene__idioms.md)

Framework-specific patterns for writing idiomatic Next.js applications with TypeScript.

**Covers**:

- Server Component patterns
- Client Component patterns ('use client' directive)
- Server Actions for mutations
- App Router navigation
- Metadata API usage
- Image and Font optimization
- Route Handlers (API routes)
- Middleware patterns
- Error boundaries and error handling
- Loading states and Suspense
- TypeScript with Next.js

### [Next.js Best Practices](./ex-so-plwe-fene__best-practices.md)

Proven approaches for building production-ready Next.js applications.

**Covers**:

- Project structure and organization
- Server vs Client Component decisions
- Data fetching strategies
- Caching and revalidation
- Performance optimization
- SEO and metadata best practices
- Error handling patterns
- Testing strategies
- TypeScript best practices for Next.js
- Accessibility compliance
- Security considerations

### [Next.js Anti-Patterns](./ex-so-plwe-fene__anti-patterns.md)

Common mistakes and problematic patterns to avoid.

**Covers**:

- Misusing Server Components
- Unnecessary Client Components
- Over-fetching data
- Missing error boundaries
- Improper caching strategies
- Ignoring loading states
- Poor image optimization
- Inefficient data fetching
- Security vulnerabilities
- SEO mistakes

### Documentation Organization

```mermaid
%% Color Palette: Blue #0173B2, Orange #DE8F05, Teal #029E73, Purple #CC78BC
%% All colors are color-blind friendly and meet WCAG AA contrast standards

flowchart TD
    A["Next.js Documentation"]:::blue
    B["Core Patterns"]:::teal
    C["Application Layers"]:::teal
    D["Advanced Topics"]:::purple
    E["Integration"]:::orange

    A --> B
    A --> C
    A --> D
    A --> E

    B --> B1["Idioms"]
    B --> B2["Best Practices"]
    B --> B3["Anti-Patterns"]

    C --> C1["Routing (App Router)"]
    C --> C2["Server Components"]
    C --> C3["Rendering Strategies"]
    C --> C4["Data Fetching"]

    D --> D1["Security"]
    D --> D2["Testing"]
    D --> D3["Observability"]
    D --> D4["Performance"]

    E --> E1["REST APIs"]
    E --> E2["Deployment"]
    E --> E3["Version Migration"]

    classDef blue fill:#0173B2,stroke:#000000,color:#FFFFFF,stroke-width:2px
    classDef orange fill:#DE8F05,stroke:#000000,color:#FFFFFF,stroke-width:2px
    classDef teal fill:#029E73,stroke:#000000,color:#FFFFFF,stroke-width:2px
    classDef purple fill:#CC78BC,stroke:#000000,color:#000000,stroke-width:2px
```

### Specialized Topics

Deep-dive documentation on critical Next.js areas:

#### [Routing (App Router)](./ex-so-plwe-fene__routing.md)

Comprehensive guide to Next.js App Router file-based routing system.

**Covers**:

- File-based routing conventions (page.tsx, layout.tsx, template.tsx)
- Dynamic routes and route parameters
- Route groups and parallel routes
- Intercepting routes
- Navigation (Link, useRouter, redirect)
- Route handlers (API routes in App Router)
- Middleware for request interception
- Internationalization (i18n) routing
- Loading and error states
- Not found pages (not-found.tsx)

#### [Server Components](./ex-so-plwe-fene__server-components.md)

Comprehensive guide to React Server Components in Next.js.

**Covers**:

- Server Component fundamentals
- Client Component ('use client') boundaries
- Composition patterns
- Props passing between Server and Client Components
- Server-only code
- Third-party package compatibility
- Data fetching in Server Components
- Server Actions for mutations
- Streaming and Suspense
- When to use Server vs Client Components

#### [Rendering Strategies](./ex-so-plwe-fene__rendering-strategies.md)

Comprehensive guide to rendering strategies in Next.js.

**Covers**:

- Static Site Generation (SSG) - generateStaticParams
- Server-Side Rendering (SSR) - dynamic rendering
- Incremental Static Regeneration (ISR) - revalidate
- Client-Side Rendering (CSR) - Client Components
- Streaming SSR with Suspense
- Partial Prerendering (PPR)
- When to use each strategy
- Performance implications
- Caching behavior
- Hybrid rendering approaches

#### [Data Fetching](./ex-so-plwe-fene__data-fetching.md)

Comprehensive guide to fetching data in Next.js applications.

**Covers**:

- Server Component data fetching with fetch
- Parallel and sequential data fetching
- Request memoization
- Data caching and revalidation
- Dynamic data fetching
- Client-side data fetching (React Query, SWR)
- Server Actions for mutations
- Form handling with Server Actions
- Error handling
- Loading states and streaming
- TypeScript types for API responses

#### [REST APIs Integration](./ex-so-plwe-fene__rest-apis.md)

Comprehensive guide to working with REST APIs in Next.js.

**Covers**:

- Route Handlers (API routes in App Router)
- External API consumption
- Request/response handling
- Error handling
- TypeScript types for API responses
- Authentication and authorization
- CORS handling
- Rate limiting
- API client abstraction
- Server Actions vs Route Handlers
- Financial examples: Zakat API, Murabaha API calls

#### [Security](./ex-so-plwe-fene__security.md)

Comprehensive guide to Next.js application security.

**Covers**:

- Server Component security advantages
- XSS prevention
- CSRF protection with Server Actions
- Content Security Policy (CSP)
- Authentication patterns (NextAuth.js, Auth.js)
- Authorization in Server Components
- Secure environment variables
- API route security
- Input validation
- Rate limiting
- Security headers (middleware)

#### [Testing](./ex-so-plwe-fene__testing.md)

Comprehensive guide to testing Next.js applications.

**Covers**:

- Unit testing components (Vitest, React Testing Library)
- Testing Server Components
- Testing Client Components
- Testing Server Actions
- Testing Route Handlers
- Integration testing
- E2E testing with Playwright
- Mocking fetch requests
- Testing with TypeScript
- Snapshot testing
- Coverage requirements
- TDD workflow with Next.js

#### [Observability](./ex-so-plwe-fene__observability.md)

Comprehensive guide to monitoring and debugging Next.js applications.

**Covers**:

- Error boundaries (global and route-level)
- Error handling in Server Components
- Logging strategies
- Sentry integration
- Performance monitoring (Web Vitals)
- React DevTools
- Next.js DevTools
- Server-side logging
- User analytics
- APM integration
- Source maps for production debugging

#### [Performance Optimization](./ex-so-plwe-fene__performance.md)

Comprehensive guide to optimizing Next.js application performance.

**Covers**:

- Image optimization (next/image)
- Font optimization (next/font)
- Code splitting and lazy loading
- Server Component advantages
- Streaming and Suspense
- Route prefetching
- Caching strategies
- Bundle size optimization
- Web Vitals (LCP, FID, CLS)
- React Profiler
- Production profiling
- Database query optimization

#### [Deployment](./ex-so-plwe-fene__deployment.md)

Comprehensive guide to deploying Next.js applications.

**Covers**:

- Vercel deployment (recommended)
- Docker containerization
- Self-hosted deployment (Node.js server)
- Static export (output: 'export')
- Environment variables
- CI/CD with GitHub Actions
- Build optimization
- Caching configuration
- CDN integration
- Monitoring and rollback
- Zero-downtime deployments
- Incremental adoption strategies

#### [Version Migration](./ex-so-plwe-fene__version-migration.md)

Comprehensive guide to upgrading Next.js versions.

**Covers**:

- Pages Router to App Router migration
- Next.js 12 to 13 migration
- Next.js 13 to 14 migration
- Codemods for automated migration
- Breaking changes and deprecations
- Incremental adoption strategies
- Testing migration changes
- TypeScript compatibility
- Third-party library compatibility
- Step-by-step migration checklist

## Key Capabilities

### App Router (File-Based Routing)

Next.js uses the file system for routing, with the `app` directory:

- **page.tsx** - Route UI (Server Component by default)
- **layout.tsx** - Shared UI across routes
- **loading.tsx** - Loading states with Suspense
- **error.tsx** - Error boundaries
- **not-found.tsx** - 404 pages
- **route.ts** - API endpoints (Route Handlers)

### React Server Components

Server Components run on the server and send HTML to the client:

- **Zero JavaScript** - No client-side JavaScript for Server Components
- **Direct Backend Access** - Access databases, file systems, internal APIs
- **Automatic Code Splitting** - Only Client Components sent to browser
- **Streaming** - Progressive rendering with Suspense
- **SEO Friendly** - Fully rendered HTML for search engines

### Rendering Strategies

Next.js supports multiple rendering strategies in one application:

- **Static Generation (SSG)** - Pre-render at build time
- **Server-Side Rendering (SSR)** - Render on each request
- **Incremental Static Regeneration (ISR)** - Update static content after build
- **Client-Side Rendering (CSR)** - Render in browser with Client Components
- **Streaming SSR** - Stream HTML progressively with Suspense
- **Partial Prerendering (PPR)** - Static shell with dynamic content (preview)

### Server Actions

Type-safe server mutations without API routes:

- **Form Handling** - Progressive enhancement with forms
- **Type Safety** - Full TypeScript support
- **Automatic Revalidation** - Invalidate cache after mutations
- **Error Handling** - Server-side validation and error handling
- **Optimistic Updates** - Client-side optimistic UI
- **CSRF Protection** - Built-in security

### Built-in Optimizations

Next.js optimizes automatically:

- **Image Optimization** - Responsive images with next/image
- **Font Optimization** - Zero layout shift with next/font
- **Code Splitting** - Automatic route-based splitting
- **Prefetching** - Automatic route prefetching
- **Compression** - Gzip/Brotli compression
- **Caching** - Intelligent caching strategies

## Use Cases

**Use Next.js when you need:**

✅ Server-side rendering for SEO and performance
✅ Static site generation with dynamic capabilities
✅ Full-stack applications (frontend + API routes)
✅ E-commerce sites with product pages
✅ Marketing sites with content management
✅ Dashboards with authentication
✅ Multi-page applications with complex routing
✅ Progressive Web Apps (PWAs)
✅ Applications requiring excellent Core Web Vitals

**Consider alternatives when:**

❌ Building pure static sites without dynamic content - use Hugo/Astro instead
❌ Building real-time collaborative applications - use React SPA with WebSockets
❌ Building native mobile apps - use React Native instead
❌ Building simple landing pages - use plain React or static HTML
❌ Team unfamiliar with React - learn React first

## Architecture Integration

### Next.js App Router Structure

Typical Next.js 14+ application structure with App Router:

```
app/
├── (marketing)/           # Route group (doesn't affect URL)
│   ├── layout.tsx         # Marketing layout
│   ├── page.tsx           # Home page (/)
│   └── about/
│       └── page.tsx       # About page (/about)
├── (app)/                 # Route group for application
│   ├── layout.tsx         # App layout (with auth)
│   ├── zakat/
│   │   ├── page.tsx       # Zakat calculator (/zakat)
│   │   ├── loading.tsx    # Loading state
│   │   ├── error.tsx      # Error boundary
│   │   └── [id]/
│   │       └── page.tsx   # Dynamic route (/zakat/[id])
│   └── murabaha/
│       ├── page.tsx       # Murabaha contracts (/murabaha)
│       └── new/
│           └── page.tsx   # New contract (/murabaha/new)
├── api/                   # Route Handlers (API routes)
│   ├── zakat/
│   │   └── route.ts       # POST /api/zakat
│   └── murabaha/
│       └── route.ts       # GET/POST /api/murabaha
├── _components/           # Shared components (not routable)
│   ├── ui/
│   │   ├── Button.tsx
│   │   └── Input.tsx
│   └── features/
│       ├── ZakatForm.tsx
│       └── MurabahaCard.tsx
├── _actions/              # Server Actions
│   ├── zakat-actions.ts
│   └── murabaha-actions.ts
├── _lib/                  # Shared utilities
│   ├── db.ts
│   ├── auth.ts
│   └── utils.ts
├── layout.tsx             # Root layout
├── loading.tsx            # Global loading
├── error.tsx              # Global error boundary
├── not-found.tsx          # 404 page
└── globals.css            # Global styles
```

### Server vs Client Components

**Server Components (default)** - Run on server, send HTML:

```typescript
// app/zakat/page.tsx (Server Component)
import { fetchZakatRates } from '@/lib/db';

export default async function ZakatPage() {
  const rates = await fetchZakatRates(); // Direct database access

  return (
    <div>
      <h1>Zakat Calculator</h1>
      <ZakatForm rates={rates} />
    </div>
  );
}
```

**Client Components** - Run in browser, use hooks:

```typescript
// app/_components/ZakatForm.tsx (Client Component)
'use client';

import { useState } from 'react';
import { submitZakat } from '@/actions/zakat-actions';

export function ZakatForm({ rates }: { rates: ZakatRates }) {
  const [wealth, setWealth] = useState(0);

  return (
    <form action={submitZakat}>
      <input
        type="number"
        value={wealth}
        onChange={(e) => setWealth(Number(e.target.value))}
      />
      <button type="submit">Calculate Zakat</button>
    </form>
  );
}
```

### Functional Programming Patterns

Apply functional principles in Next.js applications:

**Pure Server Components**:

```typescript
// Pure Server Component - no side effects
interface ZakatResultProps {
  wealth: number;
  nisab: number;
  zakatAmount: number;
}

export function ZakatResult({ wealth, nisab, zakatAmount }: ZakatResultProps) {
  const eligible = wealth >= nisab;

  return (
    <div>
      <p>Wealth: ${wealth}</p>
      <p>Nisab: ${nisab}</p>
      <p>Eligible: {eligible ? 'Yes' : 'No'}</p>
      {eligible && <p>Zakat Amount: ${zakatAmount}</p>}
    </div>
  );
}
```

**Server Actions for Side Effects**:

```typescript
// app/_actions/zakat-actions.ts
"use server";

import { revalidatePath } from "next/cache";
import { db } from "@/lib/db";

export async function submitZakat(formData: FormData) {
  const wealth = Number(formData.get("wealth"));
  const nisab = Number(formData.get("nisab"));

  // Calculate zakat
  const zakatAmount = wealth >= nisab ? wealth * 0.025 : 0;

  // Save to database
  await db.zakat.create({
    data: { wealth, nisab, zakatAmount },
  });

  // Revalidate cache
  revalidatePath("/zakat");

  return { success: true, zakatAmount };
}
```

## Development Workflow

### Project Setup

**Create new Next.js + TypeScript application:**

```bash
# Using npx (recommended)
npx create-next-app@latest zakat-calculator

# Interactive prompts:
# ✔ Would you like to use TypeScript? Yes
# ✔ Would you like to use ESLint? Yes
# ✔ Would you like to use Tailwind CSS? Yes
# ✔ Would you like to use `src/` directory? No
# ✔ Would you like to use App Router? Yes
# ✔ Would you like to customize the default import alias? No

# Navigate and start dev server
cd zakat-calculator
npm run dev

# Open http://localhost:3000
```

**Using Nx workspace:**

```bash
# Generate Next.js app in Nx workspace
nx generate @nx/next:application zakat-calculator

# Development server
nx serve zakat-calculator

# Build for production
nx build zakat-calculator

# Run tests
nx test zakat-calculator
```

### Configuration

**next.config.ts** (TypeScript config):

```typescript
import type { NextConfig } from "next";

const nextConfig: NextConfig = {
  // TypeScript strict mode
  typescript: {
    ignoreBuildErrors: false,
  },

  // ESLint strict mode
  eslint: {
    ignoreDuringBuilds: false,
  },

  // Image optimization
  images: {
    domains: ["example.com", "cdn.example.com"],
    formats: ["image/avif", "image/webp"],
  },

  // Environment variables
  env: {
    CUSTOM_KEY: process.env.CUSTOM_KEY,
  },

  // Headers for security
  async headers() {
    return [
      {
        source: "/:path*",
        headers: [
          {
            key: "X-Frame-Options",
            value: "DENY",
          },
          {
            key: "X-Content-Type-Options",
            value: "nosniff",
          },
          {
            key: "Referrer-Policy",
            value: "strict-origin-when-cross-origin",
          },
        ],
      },
    ];
  },

  // Redirects
  async redirects() {
    return [
      {
        source: "/old-route",
        destination: "/new-route",
        permanent: true,
      },
    ];
  },
};

export default nextConfig;
```

**Environment Variables (.env.local)**:

```bash
# Database
DATABASE_URL=postgresql://user:password@localhost:5432/db

# API Keys (never commit to version control)
NEXT_PUBLIC_API_URL=https://api.oseplatform.com
API_SECRET_KEY=secret_key_here

# Auth
NEXTAUTH_URL=http://localhost:3000
NEXTAUTH_SECRET=your_secret_here

# Analytics
NEXT_PUBLIC_GA_ID=G-XXXXXXXXXX
```

**TypeScript Configuration (tsconfig.json)**:

```json
{
  "compilerOptions": {
    "target": "ES2020",
    "lib": ["dom", "dom.iterable", "esnext"],
    "allowJs": true,
    "skipLibCheck": true,
    "strict": true,
    "noEmit": true,
    "esModuleInterop": true,
    "module": "esnext",
    "moduleResolution": "bundler",
    "resolveJsonModule": true,
    "isolatedModules": true,
    "jsx": "preserve",
    "incremental": true,
    "plugins": [
      {
        "name": "next"
      }
    ],
    "paths": {
      "@/*": ["./*"]
    }
  },
  "include": ["next-env.d.ts", "**/*.ts", "**/*.tsx", ".next/types/**/*.ts"],
  "exclude": ["node_modules"]
}
```

### Testing Strategy

**1. Component Tests** (React Testing Library):

```typescript
import { render, screen } from '@testing-library/react';
import { describe, it, expect } from 'vitest';
import ZakatPage from '@/app/zakat/page';

describe('ZakatPage', () => {
  it('renders zakat calculator heading', () => {
    render(<ZakatPage />);
    expect(screen.getByRole('heading', { name: /zakat calculator/i })).toBeInTheDocument();
  });
});
```

**2. Server Action Tests**:

```typescript
import { describe, it, expect, vi } from "vitest";
import { submitZakat } from "@/actions/zakat-actions";

describe("submitZakat", () => {
  it("calculates zakat correctly", async () => {
    const formData = new FormData();
    formData.append("wealth", "10000");
    formData.append("nisab", "5000");

    const result = await submitZakat(formData);

    expect(result.success).toBe(true);
    expect(result.zakatAmount).toBe(250);
  });
});
```

**3. E2E Tests** (Playwright):

```typescript
import { test, expect } from "@playwright/test";

test("complete zakat calculation flow", async ({ page }) => {
  await page.goto("http://localhost:3000/zakat");

  await page.fill('input[name="wealth"]', "10000");
  await page.fill('input[name="nisab"]', "5000");
  await page.click('button:has-text("Calculate")');

  await expect(page.locator("text=Zakat Amount: $250")).toBeVisible();
});
```

## Learning Path

### 1. Prerequisites

Before learning Next.js, ensure you understand:

- **[React Fundamentals](../fe-react/README.md)** - Components, props, state, hooks
- **[TypeScript Basics](../../prog-lang/typescript/README.md)** - Types, interfaces, generics
- **HTML/CSS** - Web fundamentals
- **Node.js** - JavaScript runtime

### 2. Start with Idioms

Read [Next.js Idioms](./ex-so-plwe-fene__idioms.md) to understand framework patterns:

- App Router file-based routing
- Server vs Client Components
- Server Actions for mutations
- Data fetching patterns
- Metadata API

### 3. Apply Best Practices

Read [Next.js Best Practices](./ex-so-plwe-fene__best-practices.md) for production standards:

- Project organization
- Rendering strategy selection
- Performance optimization
- SEO best practices
- Security considerations
- Testing strategies

### 4. Avoid Anti-Patterns

Read [Next.js Anti-Patterns](./ex-so-plwe-fene__anti-patterns.md) to prevent common mistakes:

- Misusing Server Components
- Unnecessary Client Components
- Poor caching strategies
- Missing error boundaries
- SEO mistakes

### 5. Deep Dive into Architecture

Read complementary documentation:

- [Routing (App Router)](./ex-so-plwe-fene__routing.md)
- [Server Components](./ex-so-plwe-fene__server-components.md)
- [Rendering Strategies](./ex-so-plwe-fene__rendering-strategies.md)
- [Data Fetching](./ex-so-plwe-fene__data-fetching.md)

## Code Examples from Platform

### Basic Page with Server Component

```typescript
// app/zakat/page.tsx
import { fetchZakatRates } from '@/lib/db';
import { ZakatCalculator } from '@/components/ZakatCalculator';

export const metadata = {
  title: 'Zakat Calculator | OSE Platform',
  description: 'Calculate your zakat obligations based on current rates',
};

export default async function ZakatPage() {
  // Server Component - direct database access
  const rates = await fetchZakatRates();

  return (
    <div>
      <h1>Zakat Calculator</h1>
      <p>Calculate your zakat based on current nisab rates</p>
      <ZakatCalculator rates={rates} />
    </div>
  );
}
```

### Server Action for Form Submission

```typescript
// app/_actions/zakat-actions.ts
"use server";

import { z } from "zod";
import { revalidatePath } from "next/cache";
import { db } from "@/lib/db";

const zakatSchema = z.object({
  wealth: z.number().positive(),
  nisab: z.number().positive(),
  email: z.string().email(),
});

export async function calculateAndSaveZakat(formData: FormData) {
  // Parse and validate
  const rawData = {
    wealth: Number(formData.get("wealth")),
    nisab: Number(formData.get("nisab")),
    email: String(formData.get("email")),
  };

  const validatedData = zakatSchema.parse(rawData);

  // Calculate zakat
  const eligible = validatedData.wealth >= validatedData.nisab;
  const zakatAmount = eligible ? validatedData.wealth * 0.025 : 0;

  // Save to database
  const record = await db.zakatCalculation.create({
    data: {
      ...validatedData,
      zakatAmount,
      eligible,
      calculatedAt: new Date(),
    },
  });

  // Revalidate the zakat list page
  revalidatePath("/zakat/history");

  return {
    success: true,
    data: {
      id: record.id,
      zakatAmount,
      eligible,
    },
  };
}
```

### Client Component with Form

```typescript
// app/_components/ZakatCalculator.tsx
'use client';

import { useState } from 'react';
import { calculateAndSaveZakat } from '@/actions/zakat-actions';

interface ZakatCalculatorProps {
  rates: {
    goldNisab: number;
    silverNisab: number;
  };
}

export function ZakatCalculator({ rates }: ZakatCalculatorProps) {
  const [isPending, setIsPending] = useState(false);
  const [result, setResult] = useState<{ zakatAmount: number; eligible: boolean } | null>(null);

  async function handleSubmit(formData: FormData) {
    setIsPending(true);
    try {
      const response = await calculateAndSaveZakat(formData);
      if (response.success) {
        setResult(response.data);
      }
    } catch (error) {
      console.error('Failed to calculate zakat:', error);
    } finally {
      setIsPending(false);
    }
  }

  return (
    <form action={handleSubmit}>
      <div>
        <label htmlFor="wealth">Total Wealth ($)</label>
        <input id="wealth" name="wealth" type="number" required />
      </div>

      <div>
        <label htmlFor="nisab">Nisab Threshold ($)</label>
        <input
          id="nisab"
          name="nisab"
          type="number"
          defaultValue={rates.goldNisab}
          required
        />
      </div>

      <div>
        <label htmlFor="email">Email</label>
        <input id="email" name="email" type="email" required />
      </div>

      <button type="submit" disabled={isPending}>
        {isPending ? 'Calculating...' : 'Calculate Zakat'}
      </button>

      {result && (
        <div>
          <h3>Result</h3>
          <p>Eligible: {result.eligible ? 'Yes' : 'No'}</p>
          {result.eligible && <p>Zakat Amount: ${result.zakatAmount.toFixed(2)}</p>}
        </div>
      )}
    </form>
  );
}
```

### Dynamic Route with Loading and Error States

```typescript
// app/murabaha/[id]/page.tsx
import { notFound } from 'next/navigation';
import { fetchContract } from '@/lib/db';
import { ContractDetails } from '@/components/ContractDetails';

interface PageProps {
  params: { id: string };
}

export async function generateMetadata({ params }: PageProps) {
  const contract = await fetchContract(params.id);

  if (!contract) {
    return {
      title: 'Contract Not Found',
    };
  }

  return {
    title: `Murabaha Contract #${contract.id}`,
    description: `View details for Murabaha contract #${contract.id}`,
  };
}

export default async function ContractPage({ params }: PageProps) {
  const contract = await fetchContract(params.id);

  if (!contract) {
    notFound();
  }

  return (
    <div>
      <h1>Murabaha Contract #{contract.id}</h1>
      <ContractDetails contract={contract} />
    </div>
  );
}

// app/murabaha/[id]/loading.tsx
export default function Loading() {
  return (
    <div>
      <div className="animate-pulse">
        <div className="h-8 bg-gray-200 rounded w-1/4 mb-4"></div>
        <div className="h-4 bg-gray-200 rounded w-full mb-2"></div>
        <div className="h-4 bg-gray-200 rounded w-3/4 mb-2"></div>
      </div>
    </div>
  );
}

// app/murabaha/[id]/error.tsx
'use client';

export default function Error({
  error,
  reset,
}: {
  error: Error & { digest?: string };
  reset: () => void;
}) {
  return (
    <div>
      <h2>Failed to load contract</h2>
      <p>{error.message}</p>
      <button onClick={reset}>Try again</button>
    </div>
  );
}

// app/murabaha/[id]/not-found.tsx
export default function NotFound() {
  return (
    <div>
      <h2>Contract Not Found</h2>
      <p>The requested contract does not exist.</p>
    </div>
  );
}
```

## Integration with Other Documentation

### React Foundation

- **[React with TypeScript](../fe-react/README.md)** - Core React documentation (prerequisite)
- **[React Idioms](../fe-react/ex-so-plwe-fere__idioms.md)** - React patterns used in Next.js
- **[React Best Practices](../fe-react/ex-so-plwe-fere__best-practices.md)** - React standards

### TypeScript Language Standards

- **[TypeScript](../../prog-lang/typescript/README.md)** - Core TypeScript documentation
- **[TypeScript Idioms](../../prog-lang/typescript/ex-so-prla-ty__idioms.md)** - Modern TypeScript patterns
- **[TypeScript Best Practices](../../prog-lang/typescript/ex-so-prla-ty__best-practices.md)** - TypeScript coding standards

### Architecture Patterns

- **[Domain-Driven Design](../../architecture/domain-driven-design-ddd/README.md)** - DDD patterns in full-stack apps
- **[C4 Architecture Model](../../architecture/c4-architecture-model/README.md)** - System documentation

### Development Practices

- **[Test-Driven Development](../../development/test-driven-development-tdd/README.md)** - TDD with Next.js
- **[Behavior-Driven Development](../../development/behavior-driven-development-bdd/README.md)** - BDD with Cucumber
- **[Functional Programming](../../../../../governance/development/pattern/functional-programming.md)** - FP principles

### Code Quality

- **[Code Quality Standards](../../../../../governance/development/quality/code.md)** - Quality requirements
- **[Implementation Workflow](../../../../../governance/development/workflow/implementation.md)** - Development process
- **[Commit Messages](../../../../../governance/development/workflow/commit-messages.md)** - Conventional Commits

## Next.js Ecosystem

### Core Next.js

- Next.js framework
- React 18 (with Server Components)
- Next.js Router (App Router)
- Next.js Image (next/image)
- Next.js Font (next/font)
- Next.js Link (next/link)

### Authentication

- Auth.js (formerly NextAuth.js) - Authentication (recommended)
- Clerk - Authentication and user management
- Supabase Auth - Authentication with Supabase
- Custom JWT authentication

### Data Fetching

- Native fetch with caching
- TanStack Query - Client-side data fetching
- SWR - Client-side data fetching
- Prisma - Database ORM
- Drizzle - TypeScript SQL ORM

### Forms

- React Hook Form - Form handling (recommended)
- Formik - Form library
- Zod - Schema validation
- Server Actions - Native form handling

### UI Components

- shadcn/ui - Accessible component primitives (recommended)
- Material-UI (MUI) - Comprehensive component library
- Chakra UI - Accessible component system
- Radix UI - Unstyled accessible components

### Styling

- Tailwind CSS - Utility-first CSS (recommended)
- CSS Modules - Built-in CSS scoping
- Styled Components - CSS-in-JS
- Emotion - CSS-in-JS

### Testing

- Vitest - Unit testing (recommended)
- React Testing Library - Component testing
- Playwright - E2E testing
- Cypress - E2E testing (alternative)

### Deployment

- Vercel - Recommended hosting platform
- Docker - Containerization
- Self-hosted - Node.js server
- Static Export - CDN deployment

## Tools and Development Environment

### Package Managers

**npm** (Default):

```bash
npm install
npm run dev
npm run build
```

**pnpm** (Recommended for performance):

```bash
pnpm install
pnpm dev
pnpm build
```

**yarn**:

```bash
yarn install
yarn dev
yarn build
```

### Development Environment

**VS Code** (Recommended):

- ESLint extension
- Prettier extension
- TypeScript and JavaScript Language Features
- Tailwind CSS IntelliSense
- Auto Import extension
- Path Intellisense

**WebStorm** (Alternative):

- Built-in Next.js support
- Excellent refactoring tools
- Integrated testing runner

### Code Quality Tools

- **ESLint** - Linting with next/core-web-vitals config
- **Prettier** - Code formatting
- **TypeScript** - Static type checking
- **Husky** - Git hooks
- **lint-staged** - Run linters on staged files

### Reproducible Next.js Development

**Version Management**:

```json
{
  "engines": {
    "node": ">=18.17.0",
    "npm": ">=9.0.0"
  },
  "volta": {
    "node": "20.11.0",
    "npm": "10.2.4"
  }
}
```

**Dependency Pinning**:

```bash
# Use package-lock.json (npm) or pnpm-lock.yaml (pnpm)
# Commit lock file to git
git add package-lock.json
git commit -m "chore: update dependencies"
```

**Docker Development Container**:

```dockerfile
FROM node:20-alpine

WORKDIR /app

# Copy package files
COPY package.json package-lock.json ./

# Install dependencies
RUN npm ci

# Copy source
COPY . .

# Expose Next.js port
EXPOSE 3000

# Start development server
CMD ["npm", "run", "dev"]
```

## Resources and References

### Official Documentation

- [Next.js Documentation](https://nextjs.org/docs)
- [React Documentation](https://react.dev/)
- [TypeScript Documentation](https://www.typescriptlang.org/docs/)
- [Vercel Documentation](https://vercel.com/docs)

### Best Practices

- [Next.js Learn Course](https://nextjs.org/learn)
- [React Server Components](https://nextjs.org/docs/app/building-your-application/rendering/server-components)
- [Next.js Examples](https://github.com/vercel/next.js/tree/canary/examples)
- [Next.js Commerce](https://vercel.com/templates/next.js/nextjs-commerce)

### Books and Courses

- "Next.js in Action" by Morten Barklund
- "Real-World Next.js" by Michele Riva
- Frontend Masters Next.js courses
- Egghead.io Next.js courses

### Migration Guides

- [App Router Migration Guide](https://nextjs.org/docs/app/building-your-application/upgrading/app-router-migration)
- [Next.js 13 to 14 Guide](https://nextjs.org/docs/app/building-your-application/upgrading/version-14)

## Related Documentation

### Core Next.js Documentation

- **[Next.js Idioms](./ex-so-plwe-fene__idioms.md)** - Framework patterns
- **[Next.js Best Practices](./ex-so-plwe-fene__best-practices.md)** - Production standards
- **[Next.js Anti-Patterns](./ex-so-plwe-fene__anti-patterns.md)** - Common mistakes

### Application Layer Documentation

- **[Routing (App Router)](./ex-so-plwe-fene__routing.md)** - File-based routing
- **[Server Components](./ex-so-plwe-fene__server-components.md)** - RSC patterns
- **[Rendering Strategies](./ex-so-plwe-fene__rendering-strategies.md)** - SSR/SSG/ISR/CSR
- **[Data Fetching](./ex-so-plwe-fene__data-fetching.md)** - Data loading patterns

### Advanced Topics

- **[Security](./ex-so-plwe-fene__security.md)** - Next.js security
- **[Testing](./ex-so-plwe-fene__testing.md)** - Testing strategies
- **[Observability](./ex-so-plwe-fene__observability.md)** - Monitoring and debugging
- **[Performance](./ex-so-plwe-fene__performance.md)** - Optimization

### Integration Documentation

- **[REST APIs](./ex-so-plwe-fene__rest-apis.md)** - API routes and consumption
- **[Deployment](./ex-so-plwe-fene__deployment.md)** - Deployment strategies
- **[Version Migration](./ex-so-plwe-fene__version-migration.md)** - Upgrading versions

### Platform Documentation

- **[Libraries and Frameworks Index](../README.md)** - Parent frameworks documentation
- **[React with TypeScript](../fe-react/README.md)** - React foundation (prerequisite)
- **[TypeScript Programming Language](../../prog-lang/typescript/README.md)** - TypeScript standards
- **[Software Design Index](../../README.md)** - Software documentation root
- **[Monorepo Structure](../../../../reference/re__monorepo-structure.md)** - Nx workspace organization

---

**Last Updated**: 2026-01-29
**Next.js Version**: 14+ (TypeScript 5+, React 18+, Node.js 18+)
**Maintainers**: Platform Documentation Team
