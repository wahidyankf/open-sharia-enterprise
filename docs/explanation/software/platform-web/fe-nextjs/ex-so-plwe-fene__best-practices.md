---
title: Next.js Best Practices
description: Comprehensive best practices for building production-ready Next.js applications with TypeScript, covering project structure, Server/Client Components, data fetching, performance, security, and deployment
category: explanation
tags:
  - nextjs
  - best-practices
  - typescript
  - production
  - server-components
  - performance
created: 2026-01-29
updated: 2026-01-29
---

# Next.js Best Practices

This document provides comprehensive best practices for building production-ready Next.js applications with TypeScript. These guidelines emphasize maintainability, type safety, performance, accessibility, and security for enterprise applications using Next.js 14+ with the App Router.

**Prerequisites**: Familiarity with [React best practices](../fe-react/ex-so-plwe-fere__best-practices.md) and [Next.js fundamentals](./README.md).

## 📐 Project Structure

### Feature-Based Organization (Recommended)

Organize code by business features for better scalability and team autonomy.

```
app/
├── (marketing)/                # Route group (marketing site)
│   ├── layout.tsx
│   ├── page.tsx
│   └── about/
│       └── page.tsx
├── (app)/                      # Route group (authenticated app)
│   ├── layout.tsx
│   ├── zakat/
│   │   ├── page.tsx
│   │   ├── _components/       # Feature-specific components
│   │   │   ├── ZakatForm.tsx
│   │   │   └── ZakatSummary.tsx
│   │   ├── _actions/          # Feature-specific actions
│   │   │   └── zakat-actions.ts
│   │   └── history/
│   │       └── page.tsx
│   └── murabaha/
│       ├── page.tsx
│       ├── _components/
│       ├── _actions/
│       └── [id]/
│           └── page.tsx
├── _components/                # Shared components
│   ├── ui/
│   │   ├── Button.tsx
│   │   ├── Input.tsx
│   │   └── Modal.tsx
│   └── layout/
│       ├── Header.tsx
│       └── Footer.tsx
├── _actions/                   # Shared Server Actions
│   └── common-actions.ts
├── _lib/                       # Shared utilities
│   ├── db.ts
│   ├── auth.ts
│   ├── utils.ts
│   └── constants.ts
├── api/                        # Route Handlers
│   └── webhooks/
│       └── route.ts
├── layout.tsx                  # Root layout
├── loading.tsx                 # Global loading
├── error.tsx                   # Global error boundary
└── not-found.tsx               # Global 404
```

**Benefits**:

- Feature isolation - Each feature is self-contained
- Team autonomy - Teams work on different features without conflicts
- Scalability - Easy to add new features
- Code discoverability - Related code lives together

**Naming Conventions**:

- **`_components/`** - Underscore prefix prevents routing
- **`_actions/`** - Server Actions grouped by feature
- **`_lib/`** - Shared utilities and configurations
- **Route groups `(name)/`** - Organize routes without URL impact

## 🧩 Server vs Client Component Decisions

### Default to Server Components

**Rule**: Start with Server Components, only use Client Components when necessary.

**Server Component (default)**:

```typescript
// app/zakat/page.tsx (Server Component)
import { fetchZakatRates } from '@/lib/db';
import { ZakatForm } from './_components/ZakatForm';

export default async function ZakatPage() {
  // Direct database access
  const rates = await fetchZakatRates();

  return (
    <div>
      <h1>Zakat Calculator</h1>
      <p>Current gold nisab: ${rates.goldNisab}</p>
      <ZakatForm rates={rates} />
    </div>
  );
}
```

**Client Component (when needed)**:

```typescript
// app/zakat/_components/ZakatForm.tsx (Client Component)
'use client';

import { useState } from 'react';
import { submitZakat } from '../_actions/zakat-actions';

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

### Client Component Decision Tree

Use Client Component when:

| Requirement                                   | Example                         |
| --------------------------------------------- | ------------------------------- |
| React hooks (useState, useEffect, useContext) | Interactive forms, animations   |
| Browser APIs (window, localStorage)           | Analytics, local storage access |
| Event handlers (onClick, onChange)            | Buttons, form inputs            |
| Third-party libraries requiring browser       | Charts, maps, rich text editors |
| React Context providers                       | Theme, auth context             |

Use Server Component (default) for:

| Requirement                     | Example                            |
| ------------------------------- | ---------------------------------- |
| Data fetching from databases    | User data, product listings        |
| Accessing server-only resources | Environment variables, file system |
| Sensitive logic                 | API keys, business logic           |
| Static content                  | Text, images, layouts              |
| SEO-critical content            | Product pages, blog posts          |

### Composition Pattern

✅ **Correct** - Server Component wraps Client Components:

```typescript
// app/dashboard/page.tsx (Server Component)
import { fetchDashboardData } from '@/lib/db';
import { InteractiveChart } from './_components/InteractiveChart';

export default async function DashboardPage() {
  const data = await fetchDashboardData();

  return (
    <div>
      {/* Server Component - static header */}
      <header>
        <h1>Dashboard</h1>
        <p>Last updated: {new Date().toLocaleString()}</p>
      </header>

      {/* Client Component - interactive chart */}
      <InteractiveChart data={data} />

      {/* Server Component - static footer */}
      <footer>
        <p>Data refreshed every hour</p>
      </footer>
    </div>
  );
}
```

❌ **Avoid** - Client Component importing Server Components:

```typescript
// ❌ WRONG - Client Component cannot import Server Components
'use client';

import { ServerComponentData } from './ServerComponentData'; // ERROR!

export function ClientWrapper() {
  return <ServerComponentData />;
}
```

✅ **Correct** - Use children prop:

```typescript
// ✅ CORRECT - Pass Server Component as children
'use client';

export function ClientWrapper({ children }: { children: React.ReactNode }) {
  return <div className="wrapper">{children}</div>;
}
```

## 📊 Data Fetching Strategies

### Server Component Data Fetching

✅ **Recommended** - Direct data fetching in Server Components:

```typescript
// app/murabaha/page.tsx (Server Component)
import { db } from '@/lib/db';

export default async function MurabahaPage() {
  // Direct database access
  const contracts = await db.murabahaContract.findMany({
    where: { status: 'active' },
    orderBy: { createdAt: 'desc' },
    take: 10,
  });

  return (
    <div>
      <h1>Active Murabaha Contracts</h1>
      <ContractList contracts={contracts} />
    </div>
  );
}
```

### Parallel Data Fetching

Fetch multiple data sources in parallel for better performance.

```typescript
// app/dashboard/page.tsx (Server Component)
export default async function DashboardPage() {
  // Parallel fetching with Promise.all
  const [contracts, donations, zakatCalculations] = await Promise.all([
    fetchContracts(),
    fetchDonations(),
    fetchZakatCalculations(),
  ]);

  return (
    <div>
      <ContractsSummary contracts={contracts} />
      <DonationsSummary donations={donations} />
      <ZakatSummary calculations={zakatCalculations} />
    </div>
  );
}
```

### Sequential Data Fetching

Use sequential fetching when one request depends on another.

```typescript
// app/murabaha/[id]/page.tsx (Server Component)
export default async function ContractDetailsPage({ params }: { params: { id: string } }) {
  // First fetch
  const contract = await fetchContract(params.id);

  // Second fetch depends on first
  const payments = await fetchPayments(contract.id);

  return (
    <div>
      <ContractHeader contract={contract} />
      <PaymentSchedule payments={payments} />
    </div>
  );
}
```

### Client-Side Data Fetching

For client-side data needs, use React Query or SWR.

```typescript
// app/zakat/_components/LiveRates.tsx (Client Component)
'use client';

import { useQuery } from '@tanstack/react-query';

export function LiveRates() {
  const { data, isLoading, error } = useQuery({
    queryKey: ['zakat-rates'],
    queryFn: async () => {
      const res = await fetch('/api/zakat/rates');
      return res.json();
    },
    refetchInterval: 60000, // Refetch every minute
  });

  if (isLoading) return <div>Loading rates...</div>;
  if (error) return <div>Failed to load rates</div>;

  return (
    <div>
      <p>Gold Nisab: ${data.goldNisab}</p>
      <p>Silver Nisab: ${data.silverNisab}</p>
    </div>
  );
}
```

## 🚀 Caching and Revalidation

### Fetch Caching

Next.js caches `fetch` requests by default in Server Components.

```typescript
// app/murabaha/page.tsx (Server Component)

// ✅ Cached indefinitely (default)
const contracts = await fetch("https://api.example.com/contracts").then((res) => res.json());

// ✅ Revalidate every 3600 seconds (1 hour)
const rates = await fetch("https://api.example.com/rates", {
  next: { revalidate: 3600 },
}).then((res) => res.json());

// ✅ No caching (always fresh)
const liveData = await fetch("https://api.example.com/live", {
  cache: "no-store",
}).then((res) => res.json());
```

### Data Revalidation

Revalidate cached data using `revalidatePath` or `revalidateTag`.

```typescript
// app/_actions/murabaha-actions.ts
"use server";

import { revalidatePath, revalidateTag } from "next/cache";

export async function createContract(formData: FormData) {
  const contract = await db.murabahaContract.create({
    data: {
      principalAmount: Number(formData.get("principalAmount")),
      profitMargin: Number(formData.get("profitMargin")),
    },
  });

  // Revalidate specific path
  revalidatePath("/murabaha");

  // Revalidate all pages using this tag
  revalidateTag("contracts");

  return contract;
}
```

### Route Segment Config

Configure caching behavior per route segment.

```typescript
// app/murabaha/[id]/page.tsx

// Static rendering (default)
export const dynamic = "auto";

// Force dynamic rendering (no caching)
export const dynamic = "force-dynamic";

// Force static rendering
export const dynamic = "force-static";

// Revalidate every 3600 seconds
export const revalidate = 3600;

// No revalidation (cached indefinitely)
export const revalidate = false;

export default async function ContractPage({ params }: { params: { id: string } }) {
  // Page content
}
```

## ⚡ Performance Optimization

### Image Optimization

Use `next/image` for automatic image optimization.

```typescript
import Image from 'next/image';

export function MurabahaCard({ contract }: { contract: Contract }) {
  return (
    <div>
      <Image
        src={contract.imageUrl}
        alt={`Contract ${contract.id}`}
        width={400}
        height={300}
        priority={false} // true for above-the-fold images
        placeholder="blur" // Automatic blur placeholder
        blurDataURL="data:image/jpeg;base64,..."
      />
      <h2>{contract.title}</h2>
    </div>
  );
}
```

### Font Optimization

Use `next/font` for zero-layout-shift font loading.

```typescript
// app/layout.tsx
import { Inter, Cairo } from 'next/font/google';

const inter = Inter({
  subsets: ['latin'],
  display: 'swap',
  variable: '--font-inter',
});

const cairo = Cairo({
  subsets: ['arabic'],
  display: 'swap',
  variable: '--font-cairo',
});

export default function RootLayout({ children }: { children: React.ReactNode }) {
  return (
    <html lang="en" className={`${inter.variable} ${cairo.variable}`}>
      <body>{children}</body>
    </html>
  );
}
```

### Code Splitting and Lazy Loading

Use dynamic imports for code splitting.

```typescript
import dynamic from 'next/dynamic';

// Lazy load heavy component
const HeavyChart = dynamic(() => import('@/components/HeavyChart'), {
  loading: () => <p>Loading chart...</p>,
  ssr: false, // Disable SSR if component uses browser APIs
});

export default function AnalyticsPage() {
  return (
    <div>
      <h1>Analytics</h1>
      <HeavyChart />
    </div>
  );
}
```

### Streaming with Suspense

Stream content as it becomes ready.

```typescript
// app/dashboard/page.tsx
import { Suspense } from 'react';

export default function DashboardPage() {
  return (
    <div>
      <h1>Dashboard</h1>

      {/* Fast content renders immediately */}
      <QuickStats />

      {/* Slow content streams when ready */}
      <Suspense fallback={<LoadingSkeleton />}>
        <SlowContracts />
      </Suspense>

      <Suspense fallback={<LoadingSkeleton />}>
        <SlowAnalytics />
      </Suspense>
    </div>
  );
}

async function SlowContracts() {
  const contracts = await fetchContracts(); // 2 seconds
  return <ContractsTable contracts={contracts} />;
}
```

### Route Prefetching

Next.js automatically prefetches visible `<Link>` components. Control prefetching:

```typescript
import Link from 'next/link';

<Link href="/murabaha" prefetch={false}>
  Contracts (no prefetch)
</Link>

<Link href="/zakat" prefetch={true}>
  Zakat (prefetch on hover)
</Link>
```

## 🔒 Security Best Practices

### Server Actions Security

Server Actions are inherently secure due to server-side execution.

```typescript
// app/_actions/murabaha-actions.ts
"use server";

import { z } from "zod";
import { auth } from "@/lib/auth";

const contractSchema = z.object({
  principalAmount: z.number().positive().max(1000000),
  profitMargin: z.number().min(0).max(100),
});

export async function createContract(formData: FormData) {
  // 1. Authentication check
  const session = await auth();
  if (!session) {
    throw new Error("Unauthorized");
  }

  // 2. Input validation
  const validatedData = contractSchema.parse({
    principalAmount: Number(formData.get("principalAmount")),
    profitMargin: Number(formData.get("profitMargin")),
  });

  // 3. Authorization check
  if (!session.user.canCreateContracts) {
    throw new Error("Forbidden");
  }

  // 4. Business logic on server
  const contract = await db.murabahaContract.create({
    data: {
      ...validatedData,
      userId: session.user.id,
    },
  });

  return contract;
}
```

### Environment Variables

Use environment variables securely.

```typescript
// ✅ CORRECT - Server-only variable (no NEXT_PUBLIC_)
const DATABASE_URL = process.env.DATABASE_URL; // Only accessible on server

// ✅ CORRECT - Public variable (NEXT_PUBLIC_ prefix)
const API_URL = process.env.NEXT_PUBLIC_API_URL; // Accessible on client and server

// ❌ WRONG - Exposing sensitive data
const API_KEY = process.env.NEXT_PUBLIC_API_KEY; // API key exposed to client!
```

**Environment variable structure (.env.local)**:

```bash
# Server-only (no prefix)
DATABASE_URL=postgresql://...
API_SECRET_KEY=secret_key_here

# Public (NEXT_PUBLIC_ prefix)
NEXT_PUBLIC_API_URL=https://api.example.com
NEXT_PUBLIC_SITE_NAME=OSE Platform
```

### Security Headers

Configure security headers in `next.config.ts`.

```typescript
// next.config.ts
const nextConfig = {
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
          {
            key: "Permissions-Policy",
            value: "camera=(), microphone=(), geolocation=()",
          },
          {
            key: "Content-Security-Policy",
            value: "default-src 'self'; script-src 'self' 'unsafe-inline'; style-src 'self' 'unsafe-inline';",
          },
        ],
      },
    ];
  },
};

export default nextConfig;
```

### CSRF Protection

Server Actions have built-in CSRF protection through origin checks.

```typescript
// app/_actions/zakat-actions.ts
"use server";

// CSRF protection is automatic - Next.js validates origin
export async function submitZakat(formData: FormData) {
  // Safe to process - CSRF token validated automatically
  const wealth = Number(formData.get("wealth"));
  await saveZakatCalculation({ wealth });
}
```

## 🧪 Testing Strategies

### Component Testing

Test Server and Client Components with Vitest and React Testing Library.

```typescript
// app/zakat/_components/ZakatForm.test.tsx
import { render, screen, fireEvent } from '@testing-library/react';
import { describe, it, expect, vi } from 'vitest';
import { ZakatForm } from './ZakatForm';

describe('ZakatForm', () => {
  it('calculates zakat correctly', async () => {
    const rates = { goldNisab: 5000, silverNisab: 3500 };
    render(<ZakatForm rates={rates} />);

    const wealthInput = screen.getByLabelText(/wealth/i);
    fireEvent.change(wealthInput, { target: { value: '10000' } });

    const submitButton = screen.getByRole('button', { name: /calculate/i });
    fireEvent.click(submitButton);

    expect(await screen.findByText(/zakat amount: \$250/i)).toBeInTheDocument();
  });
});
```

### Server Action Testing

Test Server Actions with mocked database.

```typescript
// app/_actions/zakat-actions.test.ts
import { describe, it, expect, vi } from "vitest";
import { submitZakat } from "./zakat-actions";

vi.mock("@/lib/db", () => ({
  db: {
    zakatCalculation: {
      create: vi.fn().mockResolvedValue({ id: "1", zakatAmount: 250 }),
    },
  },
}));

describe("submitZakat", () => {
  it("calculates and saves zakat correctly", async () => {
    const formData = new FormData();
    formData.append("wealth", "10000");
    formData.append("nisab", "5000");

    const result = await submitZakat(formData);

    expect(result.success).toBe(true);
    expect(result.zakatAmount).toBe(250);
  });
});
```

### E2E Testing

Test complete user flows with Playwright.

```typescript
// tests/e2e/zakat-calculation.spec.ts
import { test, expect } from "@playwright/test";

test("complete zakat calculation flow", async ({ page }) => {
  await page.goto("http://localhost:3000/zakat");

  await page.fill('input[name="wealth"]', "10000");
  await page.fill('input[name="nisab"]', "5000");
  await page.click('button:has-text("Calculate Zakat")');

  await expect(page.locator("text=Zakat Amount: $250")).toBeVisible();
});
```

## 📊 SEO and Metadata

### Static Metadata

Define metadata for static pages.

```typescript
// app/zakat/page.tsx
import type { Metadata } from 'next';

export const metadata: Metadata = {
  title: 'Zakat Calculator | OSE Platform',
  description: 'Calculate your zakat obligations based on current nisab rates',
  keywords: ['zakat', 'calculator', 'islamic finance'],
  openGraph: {
    title: 'Zakat Calculator',
    description: 'Calculate your zakat obligations',
    images: ['/og-zakat.png'],
  },
  twitter: {
    card: 'summary_large_image',
    title: 'Zakat Calculator',
    description: 'Calculate your zakat obligations',
    images: ['/twitter-zakat.png'],
  },
};

export default function ZakatPage() {
  return <div>Zakat Calculator</div>;
}
```

### Dynamic Metadata

Generate metadata dynamically based on data.

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
    title: `Murabaha Contract #${contract.id} | OSE Platform`,
    description: `View details for Murabaha contract with principal amount $${contract.principalAmount}`,
    openGraph: {
      title: `Murabaha Contract #${contract.id}`,
      description: `Principal: $${contract.principalAmount}, Profit: ${contract.profitMargin}%`,
      images: [
        {
          url: `/api/og/contract/${contract.id}`,
          width: 1200,
          height: 630,
          alt: `Contract ${contract.id}`,
        },
      ],
    },
  };
}

export default async function ContractPage({ params }: { params: { id: string } }) {
  const contract = await fetchContract(params.id);
  return <ContractDetails contract={contract} />;
}
```

## ♿ Accessibility

### Semantic HTML

Use semantic HTML elements for better accessibility.

```typescript
// ✅ CORRECT - Semantic HTML
export default function ZakatPage() {
  return (
    <main>
      <header>
        <h1>Zakat Calculator</h1>
        <nav>
          <ul>
            <li><a href="/zakat/calculator">Calculator</a></li>
            <li><a href="/zakat/history">History</a></li>
          </ul>
        </nav>
      </header>

      <article>
        <h2>Calculate Your Zakat</h2>
        <form>
          {/* Form content */}
        </form>
      </article>

      <footer>
        <p>© 2026 OSE Platform</p>
      </footer>
    </main>
  );
}

// ❌ AVOID - Generic divs without semantic meaning
export default function ZakatPage() {
  return (
    <div>
      <div>
        <div>Zakat Calculator</div>
        <div>
          <div>Calculator</div>
          <div>History</div>
        </div>
      </div>
    </div>
  );
}
```

### ARIA Labels

Provide ARIA labels for better screen reader support.

```typescript
export function ZakatForm() {
  return (
    <form aria-label="Zakat calculation form">
      <div>
        <label htmlFor="wealth-input">Total Wealth ($)</label>
        <input
          id="wealth-input"
          type="number"
          aria-required="true"
          aria-describedby="wealth-help"
        />
        <p id="wealth-help">Enter your total zakatable wealth in US dollars</p>
      </div>

      <button type="submit" aria-label="Calculate zakat amount">
        Calculate Zakat
      </button>
    </form>
  );
}
```

## 🔗 Related Documentation

**Next.js Core**:

- [Next.js Idioms](./ex-so-plwe-fene__idioms.md) - Next.js-specific patterns
- [Server Components](./ex-so-plwe-fene__server-components.md) - Server Component architecture
- [Routing](./ex-so-plwe-fene__routing.md) - App Router fundamentals
- [Anti-Patterns](./ex-so-plwe-fene__anti-patterns.md) - Common mistakes

**React Foundation**:

- [React Best Practices](../fe-react/ex-so-plwe-fere__best-practices.md) - React standards

**Official Resources**:

- [Next.js Documentation](https://nextjs.org/docs)
- [App Router Best Practices](https://nextjs.org/docs/app/building-your-application)

---

This comprehensive best practices guide covers production-ready Next.js development. Apply these patterns consistently across all Next.js applications in the platform to ensure code quality, maintainability, performance, and user experience.
