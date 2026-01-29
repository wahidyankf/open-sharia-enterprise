---
title: Next.js Anti-Patterns
description: Common mistakes and problematic patterns to avoid in Next.js applications, covering Server Components misuse, unnecessary Client Components, poor caching, missing error boundaries, and SEO mistakes
category: explanation
tags:
  - nextjs
  - anti-patterns
  - typescript
  - mistakes
  - code-smell
  - best-practices
created: 2026-01-29
updated: 2026-01-29
---

# Next.js Anti-Patterns

This document identifies common mistakes and problematic patterns to avoid when building Next.js applications with TypeScript. Learning what NOT to do is as important as learning best practices. These anti-patterns can lead to performance issues, security vulnerabilities, poor user experience, and maintenance nightmares.

**Prerequisites**: Familiarity with [Next.js best practices](./ex-so-plwe-fene__best-practices.md), [Server Components](./ex-so-plwe-fene__server-components.md), and [React anti-patterns](../fe-react/ex-so-plwe-fera__anti-patterns.md).

## 🚫 Server Component Misuse

### Anti-Pattern 1: Using Client Component When Server Component Works

**Problem**: Unnecessary Client Components increase bundle size and reduce performance.

❌ **Wrong** - Unnecessary Client Component:

```typescript
// app/zakat/_components/ZakatRates.tsx
'use client'; // ❌ Unnecessary!

interface ZakatRatesProps {
  rates: { goldNisab: number; silverNisab: number };
}

export function ZakatRates({ rates }: ZakatRatesProps) {
  return (
    <div>
      <h2>Current Rates</h2>
      <p>Gold Nisab: ${rates.goldNisab}</p>
      <p>Silver Nisab: ${rates.silverNisab}</p>
    </div>
  );
}
```

✅ **Correct** - Server Component (default):

```typescript
// app/zakat/_components/ZakatRates.tsx
// No 'use client' needed - Server Component by default

interface ZakatRatesProps {
  rates: { goldNisab: number; silverNisab: number };
}

export function ZakatRates({ rates }: ZakatRatesProps) {
  return (
    <div>
      <h2>Current Rates</h2>
      <p>Gold Nisab: ${rates.goldNisab}</p>
      <p>Silver Nisab: ${rates.silverNisab}</p>
    </div>
  );
}
```

**Impact**:

- Increased JavaScript bundle size
- Slower page load
- Unnecessary hydration cost

### Anti-Pattern 2: Fetching Data in Client Components

**Problem**: Client-side data fetching negates Server Component benefits.

❌ **Wrong** - Client-side data fetching:

```typescript
// app/murabaha/page.tsx
'use client';

import { useEffect, useState } from 'react';

export default function MurabahaPage() {
  const [contracts, setContracts] = useState([]);

  useEffect(() => {
    fetch('/api/contracts')
      .then((res) => res.json())
      .then(setContracts);
  }, []);

  return (
    <div>
      <h1>Murabaha Contracts</h1>
      <ContractList contracts={contracts} />
    </div>
  );
}
```

✅ **Correct** - Server Component with direct data fetching:

```typescript
// app/murabaha/page.tsx
import { fetchContracts } from '@/lib/db';

export default async function MurabahaPage() {
  // Direct database access on server
  const contracts = await fetchContracts();

  return (
    <div>
      <h1>Murabaha Contracts</h1>
      <ContractList contracts={contracts} />
    </div>
  );
}
```

**Impact**:

- Slower initial render (client-side fetch after hydration)
- Poor SEO (content not in initial HTML)
- Unnecessary loading states
- Wasted server resources (API route + database query)

### Anti-Pattern 3: Exposing Server-Only Code to Client

**Problem**: Sensitive code or credentials leaked to client bundle.

❌ **Wrong** - Server code in Client Component:

```typescript
// app/_components/ContractForm.tsx
'use client';

import { useState } from 'react';

export function ContractForm() {
  const [result, setResult] = useState(null);

  async function handleSubmit() {
    // ❌ Database credentials exposed to client!
    const DATABASE_URL = process.env.DATABASE_URL;
    const response = await fetch(DATABASE_URL, {
      /* ... */
    });
    setResult(await response.json());
  }

  return <form onSubmit={handleSubmit}>{/* ... */}</form>;
}
```

✅ **Correct** - Use Server Actions:

```typescript
// app/_actions/contract-actions.ts
'use server';

import { db } from '@/lib/db';

export async function createContract(formData: FormData) {
  // Server-only code - DATABASE_URL stays secure
  const contract = await db.contract.create({
    data: {
      principalAmount: Number(formData.get('principalAmount')),
    },
  });

  return contract;
}

// app/_components/ContractForm.tsx
'use client';

import { createContract } from '@/actions/contract-actions';

export function ContractForm() {
  return (
    <form action={createContract}>
      <input name="principalAmount" type="number" />
      <button type="submit">Create</button>
    </form>
  );
}
```

**Impact**:

- Security vulnerability (exposed credentials)
- Data breach risk
- Compliance violations

## 🔄 Data Fetching Anti-Patterns

### Anti-Pattern 4: Sequential Data Fetching (Waterfalls)

**Problem**: Sequential requests slow down page load significantly.

❌ **Wrong** - Sequential fetching:

```typescript
// app/dashboard/page.tsx
export default async function DashboardPage() {
  const user = await fetchUser(); // 500ms
  const contracts = await fetchContracts(user.id); // 500ms
  const donations = await fetchDonations(user.id); // 500ms
  const analytics = await fetchAnalytics(user.id); // 500ms
  // Total: 2000ms (2 seconds)

  return <Dashboard user={user} contracts={contracts} donations={donations} analytics={analytics} />;
}
```

✅ **Correct** - Parallel fetching:

```typescript
// app/dashboard/page.tsx
export default async function DashboardPage() {
  const user = await fetchUser(); // 500ms

  // Parallel fetching after we have user.id
  const [contracts, donations, analytics] = await Promise.all([
    fetchContracts(user.id),
    fetchDonations(user.id),
    fetchAnalytics(user.id),
  ]);
  // Total: 500ms (user) + 500ms (parallel) = 1000ms (1 second)

  return <Dashboard user={user} contracts={contracts} donations={donations} analytics={analytics} />;
}
```

**Impact**:

- 2x-4x slower page load
- Poor user experience
- Higher server costs

### Anti-Pattern 5: Missing Error Boundaries

**Problem**: Errors crash the entire application instead of isolated components.

❌ **Wrong** - No error boundary:

```typescript
// app/zakat/page.tsx
export default async function ZakatPage() {
  const rates = await fetchZakatRates(); // If this fails, entire page crashes

  return (
    <div>
      <h1>Zakat Calculator</h1>
      <ZakatForm rates={rates} />
    </div>
  );
}
```

✅ **Correct** - Error boundary with fallback:

```typescript
// app/zakat/error.tsx
'use client';

export default function Error({ error, reset }: { error: Error; reset: () => void }) {
  return (
    <div>
      <h2>Failed to load zakat calculator</h2>
      <p>{error.message}</p>
      <button onClick={reset}>Try again</button>
    </div>
  );
}

// app/zakat/page.tsx
export default async function ZakatPage() {
  const rates = await fetchZakatRates(); // Error caught by error.tsx

  return (
    <div>
      <h1>Zakat Calculator</h1>
      <ZakatForm rates={rates} />
    </div>
  );
}
```

**Impact**:

- Poor user experience (white screen of death)
- No error recovery mechanism
- Difficult debugging

### Anti-Pattern 6: Over-Fetching Data

**Problem**: Fetching more data than needed wastes bandwidth and slows responses.

❌ **Wrong** - Over-fetching:

```typescript
// app/murabaha/page.tsx
export default async function MurabahaPage() {
  // ❌ Fetching ALL fields including unnecessary relations
  const contracts = await db.murabahaContract.findMany({
    include: {
      user: true, // Not needed for list view
      payments: true, // Not needed for list view
      auditLogs: true, // Not needed for list view
      documents: true, // Not needed for list view
    },
  });

  return (
    <ul>
      {contracts.map((contract) => (
        <li key={contract.id}>
          {contract.id} - ${contract.principalAmount}
        </li>
      ))}
    </ul>
  );
}
```

✅ **Correct** - Fetch only needed fields:

```typescript
// app/murabaha/page.tsx
export default async function MurabahaPage() {
  // ✅ Only fetch fields needed for list view
  const contracts = await db.murabahaContract.findMany({
    select: {
      id: true,
      principalAmount: true,
      profitMargin: true,
      status: true,
    },
    take: 20, // Pagination
  });

  return (
    <ul>
      {contracts.map((contract) => (
        <li key={contract.id}>
          {contract.id} - ${contract.principalAmount}
        </li>
      ))}
    </ul>
  );
}
```

**Impact**:

- Slower database queries
- Increased bandwidth usage
- Higher server costs

## 💾 Caching Anti-Patterns

### Anti-Pattern 7: Ignoring Cache Configuration

**Problem**: Everything cached forever or nothing cached at all.

❌ **Wrong** - No cache control:

```typescript
// app/murabaha/page.tsx
export default async function MurabahaPage() {
  // ❌ Default cache: cached forever
  const contracts = await fetch('https://api.example.com/contracts').then((res) => res.json());

  return <ContractList contracts={contracts} />;
}
```

✅ **Correct** - Appropriate cache settings:

```typescript
// app/murabaha/page.tsx
export default async function MurabahaPage() {
  // ✅ Revalidate every hour
  const contracts = await fetch('https://api.example.com/contracts', {
    next: { revalidate: 3600 },
  }).then((res) => res.json());

  return <ContractList contracts={contracts} />;
}
```

**Impact**:

- Stale data shown to users
- Unnecessary cache invalidation
- Poor user experience

### Anti-Pattern 8: Not Revalidating After Mutations

**Problem**: Cache not invalidated after data changes, showing stale data.

❌ **Wrong** - No cache revalidation:

```typescript
// app/_actions/contract-actions.ts
"use server";

import { db } from "@/lib/db";

export async function createContract(formData: FormData) {
  const contract = await db.contract.create({
    data: {
      principalAmount: Number(formData.get("principalAmount")),
    },
  });

  // ❌ Cache not revalidated - users see stale data
  return contract;
}
```

✅ **Correct** - Revalidate cache:

```typescript
// app/_actions/contract-actions.ts
"use server";

import { revalidatePath } from "next/cache";
import { db } from "@/lib/db";

export async function createContract(formData: FormData) {
  const contract = await db.contract.create({
    data: {
      principalAmount: Number(formData.get("principalAmount")),
    },
  });

  // ✅ Revalidate cache
  revalidatePath("/murabaha");

  return contract;
}
```

**Impact**:

- Users see outdated data
- Confusion and frustration
- Support tickets

## 🔗 Routing Anti-Patterns

### Anti-Pattern 9: Using Incorrect File Naming

**Problem**: Files don't follow Next.js conventions, breaking routing.

❌ **Wrong** - Incorrect naming:

```
app/
├── zakat.tsx              ❌ Should be zakat/page.tsx
├── murabaha-list.tsx      ❌ Should be murabaha/page.tsx
└── contract-detail.tsx    ❌ Should be murabaha/[id]/page.tsx
```

✅ **Correct** - Convention-based naming:

```
app/
├── zakat/
│   └── page.tsx           ✅ Route: /zakat
├── murabaha/
│   └── page.tsx           ✅ Route: /murabaha
└── murabaha/
    └── [id]/
        └── page.tsx       ✅ Route: /murabaha/[id]
```

**Impact**:

- Routes don't work
- Confusing file structure
- Build errors

### Anti-Pattern 10: Missing Loading and Error States

**Problem**: No feedback during data fetching or errors.

❌ **Wrong** - No loading/error states:

```
app/
└── zakat/
    └── page.tsx           ❌ Missing loading.tsx and error.tsx
```

✅ **Correct** - Complete route segment:

```
app/
└── zakat/
    ├── page.tsx           ✅ Main page
    ├── loading.tsx        ✅ Loading state
    └── error.tsx          ✅ Error boundary
```

**Impact**:

- Poor user experience (no loading feedback)
- Entire page crashes on error
- No error recovery

## 🎨 Client Component Anti-Patterns

### Anti-Pattern 11: Making Entire Page a Client Component

**Problem**: Loses Server Component benefits for entire route.

❌ **Wrong** - Entire page as Client Component:

```typescript
// app/dashboard/page.tsx
'use client'; // ❌ Makes everything client-side

import { useState, useEffect } from 'react';

export default function DashboardPage() {
  const [data, setData] = useState(null);

  useEffect(() => {
    fetch('/api/dashboard').then((res) => res.json()).then(setData);
  }, []);

  return (
    <div>
      <Header />
      <MainContent data={data} />
      <Footer />
    </div>
  );
}
```

✅ **Correct** - Server Component with Client islands:

```typescript
// app/dashboard/page.tsx (Server Component)
import { fetchDashboardData } from '@/lib/db';
import { Header } from '@/components/Header';
import { InteractiveChart } from '@/components/InteractiveChart';
import { Footer } from '@/components/Footer';

export default async function DashboardPage() {
  const data = await fetchDashboardData();

  return (
    <div>
      <Header /> {/* Server Component */}
      <InteractiveChart data={data} /> {/* Client Component */}
      <Footer /> {/* Server Component */}
    </div>
  );
}
```

**Impact**:

- Large JavaScript bundle
- Slower page load
- Loss of SEO benefits

### Anti-Pattern 12: Prop Drilling Through Client Components

**Problem**: Passing Server Component props through multiple Client Components.

❌ **Wrong** - Prop drilling:

```typescript
// app/murabaha/page.tsx (Server Component)
export default async function MurabahaPage() {
  const contracts = await fetchContracts();

  return <ClientWrapper contracts={contracts} />;
}

// app/_components/ClientWrapper.tsx (Client Component)
'use client';

export function ClientWrapper({ contracts }: { contracts: Contract[] }) {
  return <AnotherClientComponent contracts={contracts} />;
}

// app/_components/AnotherClientComponent.tsx (Client Component)
'use client';

export function AnotherClientComponent({ contracts }: { contracts: Contract[] }) {
  return <FinalComponent contracts={contracts} />;
}
```

✅ **Correct** - Use composition with children:

```typescript
// app/murabaha/page.tsx (Server Component)
export default async function MurabahaPage() {
  const contracts = await fetchContracts();

  return (
    <ClientWrapper>
      <ContractList contracts={contracts} /> {/* Server Component */}
    </ClientWrapper>
  );
}

// app/_components/ClientWrapper.tsx (Client Component)
'use client';

export function ClientWrapper({ children }: { children: React.ReactNode }) {
  // Client-side interactivity without prop drilling
  return <div className="interactive-wrapper">{children}</div>;
}
```

**Impact**:

- Unnecessary re-renders
- Complex prop management
- Difficult refactoring

## 🔐 Security Anti-Patterns

### Anti-Pattern 13: Exposing Environment Variables to Client

**Problem**: Sensitive credentials leaked to client bundle.

❌ **Wrong** - Exposed secrets:

```typescript
// app/_components/ApiClient.tsx
'use client';

export function ApiClient() {
  // ❌ API key exposed to client!
  const apiKey = process.env.NEXT_PUBLIC_API_SECRET_KEY;

  fetch('https://api.example.com/data', {
    headers: { Authorization: `Bearer ${apiKey}` },
  });

  return <div>Loading...</div>;
}
```

✅ **Correct** - Server-side API calls:

```typescript
// app/_actions/api-actions.ts
'use server';

export async function fetchExternalData() {
  // ✅ API key stays on server
  const apiKey = process.env.API_SECRET_KEY;

  const response = await fetch('https://api.example.com/data', {
    headers: { Authorization: `Bearer ${apiKey}` },
  });

  return response.json();
}

// app/_components/DataDisplay.tsx
'use client';

import { fetchExternalData } from '@/actions/api-actions';

export function DataDisplay() {
  async function handleFetch() {
    const data = await fetchExternalData();
    // Process data
  }

  return <button onClick={handleFetch}>Fetch Data</button>;
}
```

**Impact**:

- Security breach
- API key theft
- Unauthorized access

### Anti-Pattern 14: No Input Validation in Server Actions

**Problem**: Accepting untrusted input without validation.

❌ **Wrong** - No validation:

```typescript
// app/_actions/contract-actions.ts
"use server";

import { db } from "@/lib/db";

export async function createContract(formData: FormData) {
  // ❌ No validation!
  const contract = await db.contract.create({
    data: {
      principalAmount: Number(formData.get("principalAmount")),
      profitMargin: Number(formData.get("profitMargin")),
    },
  });

  return contract;
}
```

✅ **Correct** - Validate all inputs:

```typescript
// app/_actions/contract-actions.ts
"use server";

import { z } from "zod";
import { db } from "@/lib/db";

const contractSchema = z.object({
  principalAmount: z.number().positive().max(1000000),
  profitMargin: z.number().min(0).max(100),
});

export async function createContract(formData: FormData) {
  // ✅ Validate input
  const validatedData = contractSchema.parse({
    principalAmount: Number(formData.get("principalAmount")),
    profitMargin: Number(formData.get("profitMargin")),
  });

  const contract = await db.contract.create({
    data: validatedData,
  });

  return contract;
}
```

**Impact**:

- SQL injection risk
- Data corruption
- Security vulnerabilities

## 🚀 Performance Anti-Patterns

### Anti-Pattern 15: Not Using next/image

**Problem**: Unoptimized images slow page load significantly.

❌ **Wrong** - Regular img tag:

```typescript
export function ContractCard({ contract }: { contract: Contract }) {
  return (
    <div>
      {/* ❌ Unoptimized image */}
      <img src={contract.imageUrl} alt={contract.title} />
      <h2>{contract.title}</h2>
    </div>
  );
}
```

✅ **Correct** - next/image:

```typescript
import Image from 'next/image';

export function ContractCard({ contract }: { contract: Contract }) {
  return (
    <div>
      {/* ✅ Optimized image */}
      <Image
        src={contract.imageUrl}
        alt={contract.title}
        width={400}
        height={300}
        placeholder="blur"
      />
      <h2>{contract.title}</h2>
    </div>
  );
}
```

**Impact**:

- Slower page load
- Poor Core Web Vitals
- Wasted bandwidth

### Anti-Pattern 16: Not Using next/font

**Problem**: Font loading causes layout shift and slower rendering.

❌ **Wrong** - Google Fonts via link:

```typescript
// app/layout.tsx
export default function RootLayout({ children }: { children: React.ReactNode }) {
  return (
    <html>
      <head>
        {/* ❌ Layout shift, flash of unstyled text */}
        <link href="https://fonts.googleapis.com/css2?family=Inter:wght@400;700&display=swap" rel="stylesheet" />
      </head>
      <body>{children}</body>
    </html>
  );
}
```

✅ **Correct** - next/font:

```typescript
// app/layout.tsx
import { Inter } from 'next/font/google';

const inter = Inter({
  subsets: ['latin'],
  display: 'swap',
  variable: '--font-inter',
});

export default function RootLayout({ children }: { children: React.ReactNode }) {
  return (
    <html className={inter.variable}>
      <body>{children}</body>
    </html>
  );
}
```

**Impact**:

- Cumulative Layout Shift (CLS)
- Poor Core Web Vitals
- Flash of unstyled text (FOUT)

## 📊 SEO Anti-Patterns

### Anti-Pattern 17: Missing Metadata

**Problem**: Poor SEO and social sharing.

❌ **Wrong** - No metadata:

```typescript
// app/zakat/page.tsx
export default function ZakatPage() {
  return (
    <div>
      <h1>Zakat Calculator</h1>
      {/* ... */}
    </div>
  );
}
```

✅ **Correct** - Comprehensive metadata:

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
  },
};

export default function ZakatPage() {
  return (
    <div>
      <h1>Zakat Calculator</h1>
      {/* ... */}
    </div>
  );
}
```

**Impact**:

- Poor search rankings
- Bad social sharing preview
- Lower click-through rates

### Anti-Pattern 18: Client-Side Only Content

**Problem**: Critical content not in initial HTML.

❌ **Wrong** - Client-side rendering:

```typescript
// app/murabaha/page.tsx
'use client';

import { useState, useEffect } from 'react';

export default function MurabahaPage() {
  const [contracts, setContracts] = useState([]);

  useEffect(() => {
    fetch('/api/contracts').then((res) => res.json()).then(setContracts);
  }, []);

  return (
    <div>
      <h1>Murabaha Contracts</h1>
      {contracts.map((contract) => (
        <div key={contract.id}>{contract.title}</div>
      ))}
    </div>
  );
}
```

✅ **Correct** - Server-side rendering:

```typescript
// app/murabaha/page.tsx (Server Component)
import { fetchContracts } from '@/lib/db';

export default async function MurabahaPage() {
  const contracts = await fetchContracts();

  return (
    <div>
      <h1>Murabaha Contracts</h1>
      {contracts.map((contract) => (
        <div key={contract.id}>{contract.title}</div>
      ))}
    </div>
  );
}
```

**Impact**:

- Poor SEO (content not crawlable)
- Slower initial render
- Poor user experience

## 🔗 Related Documentation

**Next.js Core**:

- [Best Practices](./ex-so-plwe-fene__best-practices.md) - What to do instead
- [Server Components](./ex-so-plwe-fene__server-components.md) - Server Component patterns
- [Routing](./ex-so-plwe-fene__routing.md) - Proper routing patterns
- [Performance](./ex-so-plwe-fene__performance.md) - Performance optimization

**React Foundation**:

- [React Anti-Patterns](../fe-react/ex-so-plwe-fera__anti-patterns.md) - React-specific anti-patterns

**Official Resources**:

- [Next.js Documentation](https://nextjs.org/docs)
- [Common Mistakes](https://nextjs.org/docs/messages)

---

This comprehensive anti-patterns guide helps you avoid common Next.js mistakes. Study these patterns to understand what NOT to do, and refer to the best practices documentation for the correct approaches. Prevention is better than debugging!
