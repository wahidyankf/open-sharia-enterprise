---
title: Next.js Idioms and Patterns
description: Framework-specific patterns for writing idiomatic Next.js applications with TypeScript, covering Server Components, Client Components, Server Actions, App Router navigation, and Next.js built-in optimizations
category: explanation
tags:
  - nextjs
  - idioms
  - patterns
  - typescript
  - server-components
  - best-practices
created: 2026-01-29
updated: 2026-01-29
---

# Next.js Idioms and Patterns

This document provides framework-specific patterns for writing idiomatic Next.js applications with TypeScript. These idioms represent the "Next.js way" of solving common problems, following the framework's conventions and leveraging its unique features effectively.

**Prerequisites**: Familiarity with [React idioms](../fe-react/ex-so-plwe-fere__idioms.md), [Server Components](./ex-so-plwe-fene__server-components.md), and [TypeScript](../../prog-lang/typescript/README.md).

## 🖥️ Server Component Patterns

### Idiom 1: Async Server Components

Server Components can be async functions for data fetching.

```typescript
// app/zakat/page.tsx
import { db } from '@/lib/db';

export default async function ZakatPage() {
  // Direct async/await in component
  const rates = await db.zakatRate.findFirst({
    orderBy: { createdAt: 'desc' },
  });

  return (
    <div>
      <h1>Zakat Calculator</h1>
      <p>Gold Nisab: ${rates.goldNisab}</p>
    </div>
  );
}
```

**Why**: Server Components run on server, allowing direct async operations without useEffect hooks.

### Idiom 2: Parallel Data Fetching with Promise.all

Fetch multiple data sources in parallel for better performance.

```typescript
// app/dashboard/page.tsx
export default async function DashboardPage() {
  const [contracts, donations, calculations] = await Promise.all([
    db.contract.findMany({ take: 10 }),
    db.donation.findMany({ take: 10 }),
    db.zakatCalculation.findMany({ take: 10 }),
  ]);

  return (
    <div>
      <ContractsSummary contracts={contracts} />
      <DonationsSummary donations={donations} />
      <ZakatSummary calculations={calculations} />
    </div>
  );
}
```

**Why**: Reduces total request time by fetching data concurrently.

### Idiom 3: Server Component Composition

Compose Server Components hierarchically with direct prop passing.

```typescript
// app/murabaha/page.tsx (Parent Server Component)
export default async function MurabahaPage() {
  const contracts = await fetchContracts();

  return (
    <div>
      <h1>Murabaha Contracts</h1>
      <ContractList contracts={contracts} />
    </div>
  );
}

// app/murabaha/_components/ContractList.tsx (Child Server Component)
interface ContractListProps {
  contracts: Contract[];
}

export function ContractList({ contracts }: ContractListProps) {
  return (
    <ul>
      {contracts.map((contract) => (
        <ContractCard key={contract.id} contract={contract} />
      ))}
    </ul>
  );
}

// app/murabaha/_components/ContractCard.tsx (Grandchild Server Component)
interface ContractCardProps {
  contract: Contract;
}

export function ContractCard({ contract }: ContractCardProps) {
  return (
    <li>
      {contract.id} - ${contract.principalAmount}
    </li>
  );
}
```

**Why**: Keeps data fetching logic separated and components focused on presentation.

## 💻 Client Component Patterns

### Idiom 4: 'use client' Directive

Mark components as Client Components with 'use client' directive at the top.

```typescript
// app/_components/ZakatForm.tsx
'use client';

import { useState } from 'react';

export function ZakatForm() {
  const [wealth, setWealth] = useState(0);

  return (
    <form>
      <input
        type="number"
        value={wealth}
        onChange={(e) => setWealth(Number(e.target.value))}
      />
      <button type="submit">Calculate</button>
    </form>
  );
}
```

**Why**: Enables React hooks and browser APIs in the component.

### Idiom 5: Client Component Islands

Use Client Components as "islands" within Server Component trees.

```typescript
// app/zakat/page.tsx (Server Component)
import { fetchZakatRates } from '@/lib/db';
import { ZakatForm } from '@/components/ZakatForm'; // Client Component

export default async function ZakatPage() {
  const rates = await fetchZakatRates();

  return (
    <div>
      {/* Server Component - Static header */}
      <header>
        <h1>Zakat Calculator</h1>
        <p>Current gold nisab: ${rates.goldNisab}</p>
      </header>

      {/* Client Component - Interactive form */}
      <ZakatForm rates={rates} />

      {/* Server Component - Static footer */}
      <footer>
        <p>Data updated: {rates.updatedAt}</p>
      </footer>
    </div>
  );
}
```

**Why**: Minimizes JavaScript bundle size while maintaining interactivity where needed.

### Idiom 6: Children Prop for Server Components

Pass Server Components to Client Components via children prop.

```typescript
// app/_components/TabWrapper.tsx (Client Component)
'use client';

import { useState } from 'react';

interface TabWrapperProps {
  children: React.ReactNode;
}

export function TabWrapper({ children }: TabWrapperProps) {
  const [activeTab, setActiveTab] = useState(0);

  return (
    <div>
      <div className="tabs">
        <button onClick={() => setActiveTab(0)}>Tab 1</button>
        <button onClick={() => setActiveTab(1)}>Tab 2</button>
      </div>
      <div className="content">{children}</div>
    </div>
  );
}

// app/murabaha/page.tsx (Server Component)
import { TabWrapper } from '@/components/TabWrapper';

export default async function MurabahaPage() {
  const contracts = await fetchContracts();

  return (
    <TabWrapper>
      {/* This Server Component is passed as children */}
      <ContractList contracts={contracts} />
    </TabWrapper>
  );
}
```

**Why**: Allows Client Components to wrap Server Components without converting them to Client Components.

## ⚡ Server Actions Pattern

### Idiom 7: Server Actions in Separate Files

Define Server Actions in dedicated files with 'use server' directive.

```typescript
// app/_actions/zakat-actions.ts
"use server";

import { z } from "zod";
import { revalidatePath } from "next/cache";
import { db } from "@/lib/db";

const zakatSchema = z.object({
  wealth: z.number().positive(),
  nisab: z.number().positive(),
});

export async function calculateZakat(formData: FormData) {
  const validatedData = zakatSchema.parse({
    wealth: Number(formData.get("wealth")),
    nisab: Number(formData.get("nisab")),
  });

  const zakatAmount = validatedData.wealth >= validatedData.nisab ? validatedData.wealth * 0.025 : 0;

  const calculation = await db.zakatCalculation.create({
    data: { ...validatedData, zakatAmount },
  });

  revalidatePath("/zakat/history");

  return { success: true, zakatAmount };
}
```

**Why**: Separates server logic from client components, improves code organization and security.

### Idiom 8: Progressive Enhancement with Server Actions

Use Server Actions with form action for progressive enhancement.

```typescript
// app/_components/ZakatForm.tsx (Client Component)
'use client';

import { calculateZakat } from '@/actions/zakat-actions';

export function ZakatForm() {
  return (
    <form action={calculateZakat}>
      <input name="wealth" type="number" required />
      <input name="nisab" type="number" required />
      <button type="submit">Calculate Zakat</button>
    </form>
  );
}
```

**Why**: Works without JavaScript, progressively enhanced with client-side features.

### Idiom 9: Server Action with Programmatic Call

Call Server Actions programmatically for custom handling.

```typescript
// app/_components/ContractForm.tsx (Client Component)
'use client';

import { useState } from 'react';
import { createContract } from '@/actions/contract-actions';

export function ContractForm() {
  const [isPending, setIsPending] = useState(false);
  const [result, setResult] = useState<string | null>(null);

  async function handleSubmit(e: React.FormEvent<HTMLFormElement>) {
    e.preventDefault();
    setIsPending(true);

    try {
      const formData = new FormData(e.currentTarget);
      const response = await createContract(formData);

      if (response.success) {
        setResult(`Contract created: ${response.contractId}`);
      }
    } finally {
      setIsPending(false);
    }
  }

  return (
    <form onSubmit={handleSubmit}>
      <input name="principalAmount" type="number" required />
      <button type="submit" disabled={isPending}>
        {isPending ? 'Creating...' : 'Create Contract'}
      </button>
      {result && <p>{result}</p>}
    </form>
  );
}
```

**Why**: Provides control over loading states and custom error handling.

## 🧭 App Router Navigation Patterns

### Idiom 10: Link Component for Navigation

Use next/link for client-side navigation with automatic prefetching.

```typescript
import Link from 'next/link';

export function Navigation() {
  return (
    <nav>
      <Link href="/zakat">Zakat Calculator</Link>
      <Link href="/murabaha">Murabaha Contracts</Link>
      <Link href="/dashboard">Dashboard</Link>
    </nav>
  );
}
```

**Why**: Provides fast navigation with automatic route prefetching and optimistic UI updates.

### Idiom 11: useRouter for Programmatic Navigation

Use useRouter hook for programmatic navigation in Client Components.

```typescript
// app/_components/SuccessDialog.tsx (Client Component)
'use client';

import { useRouter } from 'next/navigation';

export function SuccessDialog() {
  const router = useRouter();

  function handleClose() {
    router.push('/dashboard');
  }

  return (
    <dialog open>
      <p>Contract created successfully!</p>
      <button onClick={handleClose}>Go to Dashboard</button>
    </dialog>
  );
}
```

**Why**: Allows navigation based on user actions or application logic.

### Idiom 12: redirect for Server-Side Navigation

Use redirect() for server-side redirects in Server Components or Server Actions.

```typescript
// app/dashboard/page.tsx (Server Component)
import { redirect } from 'next/navigation';
import { auth } from '@/lib/auth';

export default async function DashboardPage() {
  const session = await auth();

  if (!session) {
    redirect('/login');
  }

  return <Dashboard user={session.user} />;
}
```

**Why**: Performs server-side redirects before rendering, preventing unnecessary rendering.

## 🎨 Metadata API Patterns

### Idiom 13: Static Metadata

Define static metadata for SEO and social sharing.

```typescript
// app/zakat/page.tsx
import type { Metadata } from 'next';

export const metadata: Metadata = {
  title: 'Zakat Calculator | OSE Platform',
  description: 'Calculate your zakat obligations based on current nisab rates',
  openGraph: {
    title: 'Zakat Calculator',
    description: 'Calculate your zakat obligations',
    images: ['/og-zakat.png'],
  },
};

export default function ZakatPage() {
  return <div>Zakat Calculator</div>;
}
```

**Why**: Provides SEO metadata with type safety.

### Idiom 14: Dynamic Metadata Generation

Generate metadata dynamically based on page data.

```typescript
// app/murabaha/[id]/page.tsx
import type { Metadata } from 'next';

export async function generateMetadata({
  params,
}: {
  params: { id: string };
}): Promise<Metadata> {
  const contract = await fetchContract(params.id);

  return {
    title: `Murabaha Contract #${contract.id}`,
    description: `Principal: $${contract.principalAmount}, Profit: ${contract.profitMargin}%`,
    openGraph: {
      title: `Contract #${contract.id}`,
      images: [`/api/og/contract/${contract.id}`],
    },
  };
}

export default async function ContractPage({ params }: { params: { id: string } }) {
  const contract = await fetchContract(params.id);
  return <ContractDetails contract={contract} />;
}
```

**Why**: Creates dynamic, SEO-friendly metadata based on page content.

## 🖼️ Image Optimization Patterns

### Idiom 15: next/image for Automatic Optimization

Use next/image for automatic image optimization and lazy loading.

```typescript
import Image from 'next/image';

export function ContractCard({ contract }: { contract: Contract }) {
  return (
    <div>
      <Image
        src={contract.imageUrl}
        alt={`Contract ${contract.id}`}
        width={400}
        height={300}
        priority={false} // true for above-the-fold images
        placeholder="blur"
        blurDataURL={contract.blurDataUrl}
      />
      <h2>{contract.title}</h2>
    </div>
  );
}
```

**Why**: Provides automatic image optimization, responsive images, and lazy loading.

### Idiom 16: next/image with Local Images

Import local images for automatic optimization and blur placeholders.

```typescript
import Image from 'next/image';
import zakatImage from '@/public/images/zakat-calculator.png';

export function ZakatHero() {
  return (
    <div>
      <Image
        src={zakatImage}
        alt="Zakat calculator interface"
        placeholder="blur" // Automatic blur placeholder
        priority // Load immediately (above the fold)
      />
    </div>
  );
}
```

**Why**: Automatic optimization and blur placeholder generation for local images.

## 🔤 Font Optimization Patterns

### Idiom 17: next/font for Zero Layout Shift

Use next/font for automatic font optimization with zero layout shift.

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
      <body className="font-sans">{children}</body>
    </html>
  );
}
```

**Why**: Eliminates flash of unstyled text (FOUT) and improves Core Web Vitals.

### Idiom 18: Local Custom Fonts

Use next/font/local for custom font files.

```typescript
// app/layout.tsx
import localFont from 'next/font/local';

const customFont = localFont({
  src: [
    {
      path: './fonts/CustomFont-Regular.woff2',
      weight: '400',
      style: 'normal',
    },
    {
      path: './fonts/CustomFont-Bold.woff2',
      weight: '700',
      style: 'normal',
    },
  ],
  variable: '--font-custom',
});

export default function RootLayout({ children }: { children: React.ReactNode }) {
  return (
    <html lang="en" className={customFont.variable}>
      <body>{children}</body>
    </html>
  );
}
```

**Why**: Optimizes custom fonts with automatic font loading strategies.

## 🎯 Route Handler Patterns

### Idiom 19: GET Route Handler

Create GET API endpoints with Route Handlers.

```typescript
// app/api/zakat/rates/route.ts
import { NextRequest, NextResponse } from "next/server";
import { db } from "@/lib/db";

export async function GET(request: NextRequest) {
  const searchParams = request.nextUrl.searchParams;
  const currency = searchParams.get("currency") || "USD";

  const rates = await db.zakatRate.findFirst({
    where: { currency },
    orderBy: { createdAt: "desc" },
  });

  if (!rates) {
    return NextResponse.json({ error: "Rates not found" }, { status: 404 });
  }

  return NextResponse.json(rates);
}
```

**Why**: Creates type-safe API endpoints with full control over request/response.

### Idiom 20: POST Route Handler with Validation

Create POST endpoints with input validation.

```typescript
// app/api/contracts/route.ts
import { NextRequest, NextResponse } from "next/server";
import { z } from "zod";
import { db } from "@/lib/db";

const contractSchema = z.object({
  principalAmount: z.number().positive().max(1000000),
  profitMargin: z.number().min(0).max(100),
});

export async function POST(request: NextRequest) {
  try {
    const body = await request.json();
    const validatedData = contractSchema.parse(body);

    const contract = await db.contract.create({
      data: validatedData,
    });

    return NextResponse.json(contract, { status: 201 });
  } catch (error) {
    if (error instanceof z.ZodError) {
      return NextResponse.json({ error: "Validation failed", details: error.errors }, { status: 400 });
    }

    return NextResponse.json({ error: "Internal server error" }, { status: 500 });
  }
}
```

**Why**: Provides input validation and proper error handling for API endpoints.

## 🚀 Loading and Error Patterns

### Idiom 21: loading.tsx for Suspense Boundaries

Use loading.tsx for automatic Suspense boundaries.

```typescript
// app/murabaha/loading.tsx
export default function Loading() {
  return (
    <div className="space-y-4">
      <div className="h-8 bg-gray-200 rounded w-1/3 animate-pulse" />
      <div className="h-4 bg-gray-200 rounded w-full animate-pulse" />
      <div className="h-4 bg-gray-200 rounded w-3/4 animate-pulse" />
    </div>
  );
}
```

**Why**: Provides instant loading states while data fetches.

### Idiom 22: error.tsx for Error Boundaries

Use error.tsx for route-level error handling.

```typescript
// app/murabaha/error.tsx
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
      <h2>Failed to load contracts</h2>
      <p>{error.message}</p>
      <button onClick={reset}>Try again</button>
    </div>
  );
}
```

**Why**: Catches errors without crashing the entire application.

### Idiom 23: Streaming with Suspense

Stream individual components with Suspense.

```typescript
// app/dashboard/page.tsx
import { Suspense } from 'react';

export default function DashboardPage() {
  return (
    <div>
      <h1>Dashboard</h1>

      <Suspense fallback={<div>Loading contracts...</div>}>
        <ContractsList />
      </Suspense>

      <Suspense fallback={<div>Loading analytics...</div>}>
        <AnalyticsDashboard />
      </Suspense>
    </div>
  );
}

async function ContractsList() {
  const contracts = await fetchContracts();
  return <ContractsTable contracts={contracts} />;
}

async function AnalyticsDashboard() {
  const analytics = await fetchAnalytics();
  return <AnalyticsChart data={analytics} />;
}
```

**Why**: Progressively streams content as it becomes available.

## 🔐 Type Safety Patterns

### Idiom 24: Typed Route Params

Use TypeScript for type-safe route parameters.

```typescript
// app/murabaha/[id]/page.tsx
interface PageProps {
  params: { id: string };
  searchParams: { [key: string]: string | string[] | undefined };
}

export default async function ContractPage({ params, searchParams }: PageProps) {
  const contract = await fetchContract(params.id);
  const view = searchParams.view || 'summary';

  return <ContractDetails contract={contract} view={view} />;
}
```

**Why**: Provides type safety for route parameters and search params.

### Idiom 25: Typed Server Actions

Define typed Server Action return values.

```typescript
// app/_actions/zakat-actions.ts
"use server";

import { z } from "zod";

interface CalculateZakatResult {
  success: boolean;
  zakatAmount?: number;
  error?: string;
}

const zakatSchema = z.object({
  wealth: z.number().positive(),
  nisab: z.number().positive(),
});

export async function calculateZakat(formData: FormData): Promise<CalculateZakatResult> {
  try {
    const validatedData = zakatSchema.parse({
      wealth: Number(formData.get("wealth")),
      nisab: Number(formData.get("nisab")),
    });

    const zakatAmount = validatedData.wealth >= validatedData.nisab ? validatedData.wealth * 0.025 : 0;

    return { success: true, zakatAmount };
  } catch (error) {
    return { success: false, error: "Validation failed" };
  }
}
```

**Why**: Provides type-safe Server Action responses for better client-side handling.

## 🔗 Related Documentation

**Next.js Core**:

- [Best Practices](./ex-so-plwe-fene__best-practices.md) - Production standards
- [Server Components](./ex-so-plwe-fene__server-components.md) - RSC patterns
- [Routing](./ex-so-plwe-fene__routing.md) - App Router fundamentals
- [Anti-Patterns](./ex-so-plwe-fene__anti-patterns.md) - What to avoid

**React Foundation**:

- [React Idioms](../fe-react/ex-so-plwe-fere__idioms.md) - React patterns

**Official Resources**:

- [Next.js Patterns](https://nextjs.org/docs/app/building-your-application)
- [Server Components](https://nextjs.org/docs/app/building-your-application/rendering/server-components)

---

This comprehensive idioms guide provides Next.js-specific patterns for building idiomatic applications. Use these patterns consistently to write code that follows the "Next.js way" and leverages the framework's unique features effectively.
