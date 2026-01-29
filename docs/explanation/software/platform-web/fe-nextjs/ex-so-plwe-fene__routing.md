---
title: Next.js App Router - Routing Guide
description: Comprehensive guide to Next.js 14+ App Router file-based routing system with Server Components, dynamic routes, parallel routes, and middleware
category: explanation
tags:
  - nextjs
  - routing
  - app-router
  - typescript
  - file-based-routing
  - navigation
created: 2026-01-29
updated: 2026-01-29
---

# Next.js App Router - Routing Guide

This document provides comprehensive guidance for using the Next.js 14+ App Router file-based routing system. The App Router introduces React Server Components, improved data fetching, and enhanced routing capabilities compared to the Pages Router.

**Prerequisites**: This guide assumes familiarity with [React fundamentals](../fe-react/README.md) and [TypeScript](../../prog-lang/typescript/README.md).

## 📂 File-Based Routing Fundamentals

### Core Routing Convention

Next.js uses the file system in the `app` directory to define routes. Each folder represents a route segment that maps to a URL segment.

**Basic Structure**:

```
app/
├── page.tsx              # Homepage (/)
├── about/
│   └── page.tsx          # About page (/about)
├── zakat/
│   ├── page.tsx          # Zakat calculator (/zakat)
│   └── calculator/
│       └── page.tsx      # Calculator page (/zakat/calculator)
```

### Special File Conventions

Next.js recognizes special file names with specific purposes:

| File            | Purpose                                         | Required                   |
| --------------- | ----------------------------------------------- | -------------------------- |
| `layout.tsx`    | Shared UI for route segment and children        | No                         |
| `page.tsx`      | Unique UI and make route publicly accessible    | Yes (for routable segment) |
| `loading.tsx`   | Loading UI with Suspense boundary               | No                         |
| `error.tsx`     | Error UI with error boundary                    | No                         |
| `not-found.tsx` | 404 UI when notFound() called                   | No                         |
| `route.ts`      | API endpoint (Route Handler)                    | No                         |
| `template.tsx`  | Re-rendered layout (new instance on navigation) | No                         |
| `default.tsx`   | Fallback UI for Parallel Routes                 | No                         |

**Example**: Complete route segment

```typescript
// app/zakat/layout.tsx (shared UI)
export default function ZakatLayout({ children }: { children: React.ReactNode }) {
  return (
    <div>
      <nav>
        <a href="/zakat">Calculator</a>
        <a href="/zakat/history">History</a>
      </nav>
      {children}
    </div>
  );
}

// app/zakat/page.tsx (route UI)
export default function ZakatPage() {
  return <h1>Zakat Calculator</h1>;
}

// app/zakat/loading.tsx (loading state)
export default function Loading() {
  return <div>Loading zakat data...</div>;
}

// app/zakat/error.tsx (error boundary)
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
      <h2>Error calculating zakat</h2>
      <p>{error.message}</p>
      <button onClick={reset}>Try again</button>
    </div>
  );
}
```

## 🔗 Dynamic Routes

### Dynamic Segments

Create dynamic routes using square brackets `[param]` in folder names.

**Example**: Dynamic contract details page

```typescript
// app/murabaha/[id]/page.tsx
interface PageProps {
  params: { id: string };
  searchParams: { [key: string]: string | string[] | undefined };
}

export default async function MurabahaContractPage({ params, searchParams }: PageProps) {
  const contract = await fetchContract(params.id);

  return (
    <div>
      <h1>Contract #{params.id}</h1>
      <pre>{JSON.stringify(contract, null, 2)}</pre>
    </div>
  );
}
```

### Catch-All Segments

Use `[...slug]` to catch all subsequent segments.

```typescript
// app/docs/[...slug]/page.tsx
interface PageProps {
  params: { slug: string[] };
}

export default function DocsPage({ params }: PageProps) {
  // /docs/a/b/c => params.slug = ['a', 'b', 'c']
  const path = params.slug.join('/');
  return <h1>Docs: {path}</h1>;
}
```

### Optional Catch-All Segments

Use `[[...slug]]` to make catch-all segments optional.

```typescript
// app/shop/[[...slug]]/page.tsx
// Matches /shop, /shop/clothes, /shop/clothes/tops
export default function ShopPage({ params }: { params: { slug?: string[] } }) {
  if (!params.slug) {
    return <h1>Shop Home</h1>;
  }

  return <h1>Category: {params.slug.join(' > ')}</h1>;
}
```

### Generating Static Params

For static generation, use `generateStaticParams` to pre-render dynamic routes.

```typescript
// app/murabaha/[id]/page.tsx
export async function generateStaticParams() {
  const contracts = await fetchAllContracts();

  return contracts.map((contract) => ({
    id: contract.id,
  }));
}

export default async function ContractPage({ params }: { params: { id: string } }) {
  const contract = await fetchContract(params.id);
  return <ContractDetails contract={contract} />;
}
```

## 📁 Route Groups

Route groups organize routes without affecting the URL structure. Use parentheses `(name)` to create route groups.

**Structure**:

```
app/
├── (marketing)/          # Route group (doesn't affect URL)
│   ├── layout.tsx        # Marketing layout
│   ├── page.tsx          # Homepage (/)
│   └── about/
│       └── page.tsx      # About page (/about)
├── (app)/                # Route group for authenticated app
│   ├── layout.tsx        # App layout with auth
│   ├── zakat/
│   │   └── page.tsx      # Zakat calculator (/zakat)
│   └── murabaha/
│       └── page.tsx      # Murabaha contracts (/murabaha)
```

**Benefits**:

- Organize routes into logical groups
- Apply different layouts to different sections
- Multiple root layouts in one application
- Code organization without URL impact

**Example**: Different layouts for marketing vs app

```typescript
// app/(marketing)/layout.tsx
export default function MarketingLayout({ children }: { children: React.ReactNode }) {
  return (
    <div>
      <header>Marketing Header</header>
      {children}
      <footer>Marketing Footer</footer>
    </div>
  );
}

// app/(app)/layout.tsx
export default function AppLayout({ children }: { children: React.ReactNode }) {
  return (
    <div>
      <nav>App Navigation</nav>
      {children}
    </div>
  );
}
```

## 🔀 Parallel Routes

Parallel routes allow rendering multiple pages simultaneously in the same layout using named slots.

**Convention**: Use `@folder` syntax for named slots.

```
app/
├── @dashboard/           # Named slot
│   └── page.tsx
├── @analytics/           # Named slot
│   └── page.tsx
├── layout.tsx            # Layout consuming slots
└── page.tsx              # Main page
```

**Example**: Dashboard with parallel sections

```typescript
// app/layout.tsx
export default function Layout({
  children,
  dashboard,
  analytics,
}: {
  children: React.ReactNode;
  dashboard: React.ReactNode;
  analytics: React.ReactNode;
}) {
  return (
    <div>
      <nav>Navigation</nav>
      <main>{children}</main>
      <aside>
        {dashboard}
        {analytics}
      </aside>
    </div>
  );
}

// app/@dashboard/page.tsx
export default function Dashboard() {
  return <div>Dashboard Content</div>;
}

// app/@analytics/page.tsx
export default function Analytics() {
  return <div>Analytics Content</div>;
}
```

### Default Files for Parallel Routes

Use `default.tsx` to provide fallback content when a slot doesn't have a matching page during navigation.

```typescript
// app/@dashboard/default.tsx
export default function Default() {
  return <div>Dashboard Not Available</div>;
}
```

## 🔁 Intercepting Routes

Intercepting routes allow loading a route from another part of the application while keeping the context of the current page.

**Convention**:

- `(.)` - match segments on the same level
- `(..)` - match segments one level above
- `(..)(..)` - match segments two levels above
- `(...)` - match segments from root

**Example**: Modal overlays with intercepting routes

```
app/
├── feed/
│   └── page.tsx              # Feed page
├── photo/
│   └── [id]/
│       └── page.tsx          # Full photo page
└── @modal/
    └── (.)photo/
        └── [id]/
            └── page.tsx      # Intercepted modal view
```

```typescript
// app/feed/page.tsx
import Link from 'next/link';

export default function FeedPage() {
  return (
    <div>
      <h1>Feed</h1>
      <Link href="/photo/1">View Photo 1 (Modal)</Link>
    </div>
  );
}

// app/@modal/(.)photo/[id]/page.tsx (intercepted)
export default function PhotoModal({ params }: { params: { id: string } }) {
  return (
    <dialog open>
      <h2>Photo {params.id} (Modal)</h2>
      {/* Modal content */}
    </dialog>
  );
}

// app/photo/[id]/page.tsx (direct navigation)
export default function PhotoPage({ params }: { params: { id: string } }) {
  return (
    <div>
      <h1>Photo {params.id} (Full Page)</h1>
      {/* Full page content */}
    </div>
  );
}
```

**Use Cases**:

- Opening modals while keeping the background page
- Photo viewers with shareable URLs
- Login/signup modals that can also be standalone pages
- Shopping cart overlays

## 🧭 Navigation

### Link Component

Use `next/link` for client-side navigation with automatic prefetching.

```typescript
import Link from 'next/link';

export default function Navigation() {
  return (
    <nav>
      <Link href="/zakat">Zakat Calculator</Link>
      <Link href="/murabaha">Murabaha Contracts</Link>
      <Link href={`/murabaha/${contractId}`}>View Contract</Link>
    </nav>
  );
}
```

**Link Props**:

```typescript
<Link
  href="/zakat"
  prefetch={false}        // Disable prefetching
  replace                 // Replace history instead of push
  scroll={false}          // Disable scroll to top
>
  Zakat Calculator
</Link>
```

### useRouter Hook

Use `useRouter` from `next/navigation` for programmatic navigation in Client Components.

```typescript
'use client';

import { useRouter } from 'next/navigation';

export function ZakatForm() {
  const router = useRouter();

  const handleSubmit = async (formData: FormData) => {
    await submitZakat(formData);
    router.push('/zakat/success');
  };

  return (
    <form action={handleSubmit}>
      {/* Form fields */}
    </form>
  );
}
```

**useRouter Methods**:

- `router.push(href, { scroll: false })` - Navigate to route
- `router.replace(href)` - Replace current history entry
- `router.refresh()` - Refresh current route
- `router.prefetch(href)` - Prefetch route
- `router.back()` - Navigate back
- `router.forward()` - Navigate forward

### redirect Function

Use `redirect()` for server-side redirects in Server Components and Server Actions.

```typescript
import { redirect } from 'next/navigation';

export default async function ProfilePage() {
  const session = await getSession();

  if (!session) {
    redirect('/login');
  }

  return <Profile user={session.user} />;
}
```

### permanentRedirect Function

Use `permanentRedirect()` for permanent (308) redirects.

```typescript
import { permanentRedirect } from "next/navigation";

export default async function OldZakatPage() {
  permanentRedirect("/zakat/calculator");
}
```

## 🌐 Route Handlers (API Routes)

Route Handlers define API endpoints in the App Router using `route.ts` files.

**Supported HTTP Methods**: GET, POST, PUT, PATCH, DELETE, HEAD, OPTIONS

```typescript
// app/api/zakat/route.ts
import { NextRequest, NextResponse } from "next/server";

export async function GET(request: NextRequest) {
  const searchParams = request.nextUrl.searchParams;
  const wealth = searchParams.get("wealth");

  const zakatAmount = calculateZakat(Number(wealth));

  return NextResponse.json({ zakatAmount });
}

export async function POST(request: NextRequest) {
  const body = await request.json();
  const { wealth, nisab, email } = body;

  // Calculate and save
  const result = await saveZakatCalculation({ wealth, nisab, email });

  return NextResponse.json(result, { status: 201 });
}
```

### Dynamic Route Handlers

```typescript
// app/api/murabaha/[id]/route.ts
interface RouteContext {
  params: { id: string };
}

export async function GET(request: NextRequest, context: RouteContext) {
  const contract = await fetchContract(context.params.id);

  if (!contract) {
    return NextResponse.json({ error: "Not found" }, { status: 404 });
  }

  return NextResponse.json(contract);
}

export async function DELETE(request: NextRequest, context: RouteContext) {
  await deleteContract(context.params.id);
  return NextResponse.json({ success: true });
}
```

### Request and Response Handling

```typescript
// app/api/contracts/route.ts
import { NextRequest, NextResponse } from "next/server";

export async function GET(request: NextRequest) {
  // Read search params
  const page = request.nextUrl.searchParams.get("page") || "1";

  // Read headers
  const authorization = request.headers.get("authorization");

  // Read cookies
  const token = request.cookies.get("token");

  const contracts = await fetchContracts({ page: Number(page) });

  // Set response headers
  const response = NextResponse.json(contracts);
  response.headers.set("X-Total-Count", String(contracts.length));

  // Set cookies
  response.cookies.set("last-page", page);

  return response;
}

export async function POST(request: NextRequest) {
  // Read JSON body
  const body = await request.json();

  // Validate
  if (!body.principalAmount || !body.profitMargin) {
    return NextResponse.json({ error: "Missing required fields" }, { status: 400 });
  }

  const contract = await createContract(body);

  return NextResponse.json(contract, {
    status: 201,
    headers: {
      Location: `/api/contracts/${contract.id}`,
    },
  });
}
```

## 🛡️ Middleware

Middleware runs before routes are matched, allowing request/response modification.

```typescript
// middleware.ts (root of project)
import { NextRequest, NextResponse } from "next/server";

export function middleware(request: NextRequest) {
  // Authentication check
  const token = request.cookies.get("token");

  if (!token && request.nextUrl.pathname.startsWith("/app")) {
    return NextResponse.redirect(new URL("/login", request.url));
  }

  // Add custom header
  const response = NextResponse.next();
  response.headers.set("X-Custom-Header", "value");

  return response;
}

export const config = {
  matcher: [
    /*
     * Match all request paths except:
     * - _next/static (static files)
     * - _next/image (image optimization)
     * - favicon.ico (favicon file)
     * - public folder
     */
    "/((?!_next/static|_next/image|favicon.ico|public).*)",
  ],
};
```

### Middleware Use Cases

**1. Authentication**:

```typescript
export function middleware(request: NextRequest) {
  const session = request.cookies.get("session");

  if (!session && request.nextUrl.pathname.startsWith("/dashboard")) {
    return NextResponse.redirect(new URL("/login", request.url));
  }

  return NextResponse.next();
}
```

**2. Internationalization (i18n)**:

```typescript
export function middleware(request: NextRequest) {
  const locale = request.cookies.get("locale")?.value || "en";
  const pathname = request.nextUrl.pathname;

  if (!pathname.startsWith(`/${locale}`)) {
    return NextResponse.redirect(new URL(`/${locale}${pathname}`, request.url));
  }

  return NextResponse.next();
}
```

**3. A/B Testing**:

```typescript
export function middleware(request: NextRequest) {
  const variant = Math.random() > 0.5 ? "a" : "b";

  const response = NextResponse.next();
  response.cookies.set("variant", variant);

  return response;
}
```

**4. Rate Limiting**:

```typescript
const rateLimit = new Map<string, number[]>();

export function middleware(request: NextRequest) {
  const ip = request.ip || "unknown";
  const now = Date.now();

  if (!rateLimit.has(ip)) {
    rateLimit.set(ip, []);
  }

  const timestamps = rateLimit.get(ip)!;
  const recentRequests = timestamps.filter((t) => now - t < 60000); // 1 minute

  if (recentRequests.length > 100) {
    return NextResponse.json({ error: "Rate limit exceeded" }, { status: 429 });
  }

  recentRequests.push(now);
  rateLimit.set(ip, recentRequests);

  return NextResponse.next();
}
```

## 📦 Loading States

Use `loading.tsx` to show loading UI with automatic Suspense boundaries.

```typescript
// app/zakat/loading.tsx
export default function Loading() {
  return (
    <div className="loading-container">
      <div className="spinner" />
      <p>Loading zakat data...</p>
    </div>
  );
}
```

**Skeleton Loading**:

```typescript
// app/murabaha/loading.tsx
export default function Loading() {
  return (
    <div className="space-y-4">
      {/* Skeleton for header */}
      <div className="h-8 bg-gray-200 rounded w-1/3 animate-pulse" />

      {/* Skeleton for cards */}
      {[1, 2, 3].map((i) => (
        <div key={i} className="border rounded p-4 space-y-2">
          <div className="h-4 bg-gray-200 rounded w-full animate-pulse" />
          <div className="h-4 bg-gray-200 rounded w-3/4 animate-pulse" />
          <div className="h-4 bg-gray-200 rounded w-1/2 animate-pulse" />
        </div>
      ))}
    </div>
  );
}
```

### Streaming with Suspense

For more granular loading control, use React Suspense directly.

```typescript
// app/zakat/page.tsx
import { Suspense } from 'react';

export default function ZakatPage() {
  return (
    <div>
      <h1>Zakat Dashboard</h1>

      <Suspense fallback={<div>Loading rates...</div>}>
        <ZakatRates />
      </Suspense>

      <Suspense fallback={<div>Loading history...</div>}>
        <ZakatHistory />
      </Suspense>
    </div>
  );
}

async function ZakatRates() {
  const rates = await fetchRates(); // This can be slow
  return <RatesDisplay rates={rates} />;
}

async function ZakatHistory() {
  const history = await fetchHistory(); // This can also be slow
  return <HistoryList history={history} />;
}
```

## 🚨 Error Handling

Use `error.tsx` to create error boundaries for route segments.

```typescript
// app/zakat/error.tsx
'use client';

import { useEffect } from 'react';

export default function Error({
  error,
  reset,
}: {
  error: Error & { digest?: string };
  reset: () => void;
}) {
  useEffect(() => {
    // Log error to error reporting service
    console.error('Zakat page error:', error);
  }, [error]);

  return (
    <div className="error-container">
      <h2>Something went wrong calculating zakat</h2>
      <p>{error.message}</p>
      <button onClick={reset}>Try again</button>
    </div>
  );
}
```

### Not Found Pages

Use `not-found.tsx` for custom 404 pages.

```typescript
// app/murabaha/[id]/not-found.tsx
export default function NotFound() {
  return (
    <div>
      <h2>Contract Not Found</h2>
      <p>The requested Murabaha contract does not exist.</p>
      <a href="/murabaha">View all contracts</a>
    </div>
  );
}
```

**Trigger notFound()**:

```typescript
// app/murabaha/[id]/page.tsx
import { notFound } from 'next/navigation';

export default async function ContractPage({ params }: { params: { id: string } }) {
  const contract = await fetchContract(params.id);

  if (!contract) {
    notFound(); // Triggers not-found.tsx
  }

  return <ContractDetails contract={contract} />;
}
```

## 🌍 Internationalization (i18n)

Next.js 14+ doesn't have built-in i18n in App Router. Use manual implementation or libraries like `next-intl`.

**Manual Implementation**:

```
app/
├── [lang]/
│   ├── layout.tsx
│   ├── page.tsx
│   └── zakat/
│       └── page.tsx
```

```typescript
// app/[lang]/layout.tsx
export async function generateStaticParams() {
  return [{ lang: 'en' }, { lang: 'id' }, { lang: 'ar' }];
}

export default function LocaleLayout({
  children,
  params,
}: {
  children: React.ReactNode;
  params: { lang: string };
}) {
  return (
    <html lang={params.lang}>
      <body>{children}</body>
    </html>
  );
}

// app/[lang]/page.tsx
import { getDictionary } from '@/lib/dictionaries';

export default async function HomePage({ params }: { params: { lang: string } }) {
  const dict = await getDictionary(params.lang);

  return (
    <div>
      <h1>{dict.home.title}</h1>
      <p>{dict.home.description}</p>
    </div>
  );
}
```

**Dictionary Pattern**:

```typescript
// lib/dictionaries.ts
const dictionaries = {
  en: () => import("@/dictionaries/en.json").then((module) => module.default),
  id: () => import("@/dictionaries/id.json").then((module) => module.default),
  ar: () => import("@/dictionaries/ar.json").then((module) => module.default),
};

export const getDictionary = async (locale: string) => {
  return dictionaries[locale as keyof typeof dictionaries]();
};
```

## 📊 Route Metadata

Use `generateMetadata` for dynamic SEO metadata.

```typescript
// app/murabaha/[id]/page.tsx
import type { Metadata } from 'next';

export async function generateMetadata({
  params,
}: {
  params: { id: string };
}): Promise<Metadata> {
  const contract = await fetchContract(params.id);

  if (!contract) {
    return {
      title: 'Contract Not Found',
    };
  }

  return {
    title: `Murabaha Contract #${contract.id}`,
    description: `View details for Murabaha contract with principal amount $${contract.principalAmount}`,
    openGraph: {
      title: `Murabaha Contract #${contract.id}`,
      description: `Principal: $${contract.principalAmount}, Profit Rate: ${contract.profitMargin}%`,
      images: ['/og-contract.png'],
    },
  };
}

export default async function ContractPage({ params }: { params: { id: string } }) {
  const contract = await fetchContract(params.id);
  return <ContractDetails contract={contract} />;
}
```

## 🔗 Related Documentation

**Next.js Core**:

- [Next.js Idioms](./ex-so-plwe-fene__idioms.md) - Next.js-specific patterns
- [Server Components](./ex-so-plwe-fene__server-components.md) - Server Component architecture
- [Rendering Strategies](./ex-so-plwe-fene__rendering-strategies.md) - SSR, SSG, ISR
- [Best Practices](./ex-so-plwe-fene__best-practices.md) - Production standards

**React Foundation**:

- [React with TypeScript](../fe-react/README.md) - React fundamentals

**Official Resources**:

- [Next.js App Router Documentation](https://nextjs.org/docs/app)
- [Routing Fundamentals](https://nextjs.org/docs/app/building-your-application/routing)

---

This comprehensive routing guide covers all aspects of the Next.js App Router. Use these patterns consistently across all Next.js applications in the platform to ensure scalable, maintainable routing architecture.
