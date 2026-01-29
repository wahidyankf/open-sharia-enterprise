---
title: Next.js Data Fetching Guide
description: Comprehensive guide to data fetching in Next.js including Server Component data fetching, Client-side fetching with React Query/SWR, Server Actions, caching strategies, and error handling
category: explanation
tags:
  - nextjs
  - data-fetching
  - server-components
  - typescript
  - caching
  - server-actions
created: 2026-01-29
updated: 2026-01-29
---

# Next.js Data Fetching Guide

This document provides comprehensive guidance on data fetching strategies in Next.js 14+ with TypeScript. Next.js App Router introduces new data fetching patterns centered around Server Components, Server Actions, and enhanced fetch caching that simplify data management while improving performance.

**Prerequisites**: Familiarity with [Server Components](./ex-so-plwe-fene__server-components.md), [rendering strategies](./ex-so-plwe-fene__rendering-strategies.md), and [React fundamentals](../fe-react/README.md).

## 🖥️ Server Component Data Fetching

### Direct Data Fetching

Server Components can fetch data directly without API routes.

```typescript
// app/zakat/page.tsx (Server Component)
import { db } from '@/lib/db';

export default async function ZakatPage() {
  // Direct database access
  const rates = await db.zakatRate.findFirst({
    orderBy: { createdAt: 'desc' },
  });

  return (
    <div>
      <h1>Zakat Calculator</h1>
      <p>Gold Nisab: ${rates.goldNisab}</p>
      <p>Silver Nisab: ${rates.silverNisab}</p>
    </div>
  );
}
```

### Fetch API with Caching

Next.js extends the native fetch API with caching capabilities.

```typescript
// app/murabaha/page.tsx
export default async function MurabahaPage() {
  // Cached indefinitely (default)
  const contracts = await fetch('https://api.example.com/contracts')
    .then((res) => res.json());

  // Revalidate every hour
  const rates = await fetch('https://api.example.com/rates', {
    next: { revalidate: 3600 },
  }).then((res) => res.json());

  // No caching (always fresh)
  const liveData = await fetch('https://api.example.com/live', {
    cache: 'no-store',
  }).then((res) => res.json());

  return (
    <div>
      <ContractList contracts={contracts} />
      <RatesDisplay rates={rates} />
      <LiveFeed data={liveData} />
    </div>
  );
}
```

### TypeScript Types for API Responses

Define types for type-safe data fetching:

```typescript
// lib/types/contracts.ts
export interface MurabahaContract {
  id: string;
  principalAmount: number;
  profitMargin: number;
  termMonths: number;
  status: 'pending' | 'active' | 'completed' | 'cancelled';
  createdAt: string;
  updatedAt: string;
}

// app/murabaha/page.tsx
import type { MurabahaContract } from '@/lib/types/contracts';

export default async function MurabahaPage() {
  const contracts = await fetch('https://api.example.com/contracts')
    .then((res) => res.json() as Promise<MurabahaContract[]>);

  return (
    <ul>
      {contracts.map((contract) => (
        <li key={contract.id}>
          {contract.principalAmount} - {contract.status}
        </li>
      ))}
    </ul>
  );
}
```

## 🔄 Parallel vs Sequential Fetching

### Parallel Fetching (Recommended)

Fetch multiple data sources simultaneously for better performance.

```typescript
// app/dashboard/page.tsx
export default async function DashboardPage() {
  // Parallel fetching with Promise.all
  const [contracts, donations, zakatCalculations] = await Promise.all([
    fetch('https://api.example.com/contracts').then((res) => res.json()),
    fetch('https://api.example.com/donations').then((res) => res.json()),
    fetch('https://api.example.com/zakat').then((res) => res.json()),
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

### Sequential Fetching

Use sequential fetching when one request depends on another.

```typescript
// app/murabaha/[id]/page.tsx
export default async function ContractDetailsPage({ params }: { params: { id: string } }) {
  // First fetch
  const contract = await fetch(`https://api.example.com/contracts/${params.id}`)
    .then((res) => res.json());

  // Second fetch depends on first
  const payments = await fetch(`https://api.example.com/contracts/${contract.id}/payments`)
    .then((res) => res.json());

  const auditLogs = await fetch(`https://api.example.com/contracts/${contract.id}/audit`)
    .then((res) => res.json());

  return (
    <div>
      <ContractHeader contract={contract} />
      <PaymentSchedule payments={payments} />
      <AuditLog logs={auditLogs} />
    </div>
  );
}
```

## 📥 Request Memoization

Next.js automatically deduplicates identical fetch requests within a single render pass.

```typescript
// lib/data/contracts.ts
export async function getContract(id: string) {
  // This fetch is automatically memoized
  const contract = await fetch(`https://api.example.com/contracts/${id}`)
    .then((res) => res.json());

  return contract;
}

// app/murabaha/[id]/page.tsx
import { getContract } from '@/lib/data/contracts';

export default async function ContractPage({ params }: { params: { id: string } }) {
  // These three calls make only ONE network request
  const contract1 = await getContract(params.id);
  const contract2 = await getContract(params.id);
  const contract3 = await getContract(params.id);

  return <ContractDetails contract={contract1} />;
}
```

**Benefits**:

- Automatic deduplication
- Reduced network requests
- Better performance
- No manual caching needed

## 💾 Caching Strategies

### Cache Options

Configure fetch caching behavior:

```typescript
// Default: Cached indefinitely
await fetch("https://api.example.com/data");

// Revalidate after time period
await fetch("https://api.example.com/data", {
  next: { revalidate: 3600 }, // 1 hour
});

// No caching (always fresh)
await fetch("https://api.example.com/data", {
  cache: "no-store",
});

// Force cache (never revalidate)
await fetch("https://api.example.com/data", {
  cache: "force-cache",
});
```

### Cache Tags

Tag requests for targeted revalidation:

```typescript
// app/murabaha/page.tsx
export default async function MurabahaPage() {
  const contracts = await fetch('https://api.example.com/contracts', {
    next: { tags: ['contracts', 'murabaha'] },
  }).then((res) => res.json());

  return <ContractList contracts={contracts} />;
}

// Later, revalidate by tag
import { revalidateTag } from 'next/cache';

// Revalidate all requests tagged 'contracts'
revalidateTag('contracts');
```

### Path Revalidation

Revalidate entire routes:

```typescript
// app/_actions/contract-actions.ts
"use server";

import { revalidatePath } from "next/cache";

export async function createContract(formData: FormData) {
  const contract = await db.contract.create({
    data: {
      principalAmount: Number(formData.get("principalAmount")),
    },
  });

  // Revalidate the contracts list page
  revalidatePath("/murabaha");

  // Revalidate with type
  revalidatePath("/murabaha", "page"); // Just the page
  revalidatePath("/murabaha", "layout"); // Page and layout

  return contract;
}
```

## 🎯 Server Actions

### Form Submission with Server Actions

Server Actions simplify form handling without API routes.

```typescript
// app/_actions/zakat-actions.ts
'use server';

import { z } from 'zod';
import { revalidatePath } from 'next/cache';
import { db } from '@/lib/db';

const zakatSchema = z.object({
  wealth: z.number().positive(),
  nisab: z.number().positive(),
  email: z.string().email(),
});

export async function calculateZakat(formData: FormData) {
  // Parse and validate
  const validatedData = zakatSchema.parse({
    wealth: Number(formData.get('wealth')),
    nisab: Number(formData.get('nisab')),
    email: String(formData.get('email')),
  });

  // Calculate zakat
  const eligible = validatedData.wealth >= validatedData.nisab;
  const zakatAmount = eligible ? validatedData.wealth * 0.025 : 0;

  // Save to database
  const calculation = await db.zakatCalculation.create({
    data: {
      ...validatedData,
      zakatAmount,
      eligible,
      calculatedAt: new Date(),
    },
  });

  // Revalidate cache
  revalidatePath('/zakat/history');

  return {
    success: true,
    calculation: {
      id: calculation.id,
      zakatAmount,
      eligible,
    },
  };
}

// app/zakat/_components/ZakatForm.tsx (Client Component)
'use client';

import { calculateZakat } from '@/actions/zakat-actions';

export function ZakatForm() {
  return (
    <form action={calculateZakat}>
      <input name="wealth" type="number" required />
      <input name="nisab" type="number" required />
      <input name="email" type="email" required />
      <button type="submit">Calculate Zakat</button>
    </form>
  );
}
```

### Programmatic Server Action Calls

Call Server Actions programmatically for more control:

```typescript
// app/_components/ContractForm.tsx
'use client';

import { useState } from 'react';
import { createContract } from '@/actions/contract-actions';

export function ContractForm() {
  const [isPending, setIsPending] = useState(false);
  const [result, setResult] = useState<{ contractId: string } | null>(null);

  async function handleSubmit(e: React.FormEvent<HTMLFormElement>) {
    e.preventDefault();

    setIsPending(true);
    try {
      const formData = new FormData(e.currentTarget);
      const response = await createContract(formData);

      if (response.success) {
        setResult({ contractId: response.contractId });
      }
    } catch (error) {
      console.error('Failed to create contract:', error);
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

      {result && <p>Contract created: {result.contractId}</p>}
    </form>
  );
}
```

### Server Action Error Handling

Handle errors gracefully in Server Actions:

```typescript
// app/_actions/murabaha-actions.ts
"use server";

import { z } from "zod";

const contractSchema = z.object({
  principalAmount: z.number().positive().max(1000000),
  profitMargin: z.number().min(0).max(100),
});

export async function createContract(formData: FormData) {
  try {
    // Validate input
    const validatedData = contractSchema.parse({
      principalAmount: Number(formData.get("principalAmount")),
      profitMargin: Number(formData.get("profitMargin")),
    });

    // Business logic
    const contract = await db.contract.create({
      data: validatedData,
    });

    return {
      success: true,
      contractId: contract.id,
    };
  } catch (error) {
    if (error instanceof z.ZodError) {
      return {
        success: false,
        error: "Validation failed",
        details: error.errors,
      };
    }

    return {
      success: false,
      error: "Failed to create contract",
    };
  }
}
```

## 💻 Client-Side Data Fetching

### Using React Query (Recommended)

React Query provides powerful client-side data fetching with caching.

```typescript
// app/_components/LiveRates.tsx
'use client';

import { useQuery } from '@tanstack/react-query';

interface ZakatRates {
  goldNisab: number;
  silverNisab: number;
  lastUpdated: string;
}

export function LiveRates() {
  const { data, isLoading, error, refetch } = useQuery<ZakatRates>({
    queryKey: ['zakat-rates'],
    queryFn: async () => {
      const res = await fetch('/api/zakat/rates');
      if (!res.ok) throw new Error('Failed to fetch rates');
      return res.json();
    },
    refetchInterval: 60000, // Refetch every minute
    staleTime: 30000, // Consider stale after 30 seconds
  });

  if (isLoading) return <div>Loading rates...</div>;
  if (error) return <div>Error: {error.message}</div>;

  return (
    <div>
      <h2>Current Rates</h2>
      <p>Gold Nisab: ${data?.goldNisab}</p>
      <p>Silver Nisab: ${data?.silverNisab}</p>
      <button onClick={() => refetch()}>Refresh</button>
    </div>
  );
}
```

### Using SWR (Alternative)

SWR is another excellent option for client-side data fetching.

```typescript
// app/_components/ContractList.tsx
'use client';

import useSWR from 'swr';

const fetcher = (url: string) => fetch(url).then((res) => res.json());

export function ContractList() {
  const { data, error, isLoading, mutate } = useSWR(
    '/api/contracts',
    fetcher,
    {
      refreshInterval: 30000, // Refresh every 30 seconds
      revalidateOnFocus: true,
    }
  );

  if (isLoading) return <div>Loading...</div>;
  if (error) return <div>Failed to load contracts</div>;

  return (
    <div>
      <h2>Contracts</h2>
      <ul>
        {data.map((contract: any) => (
          <li key={contract.id}>{contract.title}</li>
        ))}
      </ul>
      <button onClick={() => mutate()}>Refresh</button>
    </div>
  );
}
```

### React Query Setup

Set up React Query provider in your app:

```typescript
// app/_providers/QueryProvider.tsx
'use client';

import { QueryClient, QueryClientProvider } from '@tanstack/react-query';
import { ReactQueryDevtools } from '@tanstack/react-query-devtools';
import { useState } from 'react';

export function QueryProvider({ children }: { children: React.ReactNode }) {
  const [queryClient] = useState(
    () =>
      new QueryClient({
        defaultOptions: {
          queries: {
            staleTime: 60 * 1000, // 1 minute
            gcTime: 5 * 60 * 1000, // 5 minutes (formerly cacheTime)
            refetchOnWindowFocus: false,
            retry: 1,
          },
        },
      })
  );

  return (
    <QueryClientProvider client={queryClient}>
      {children}
      <ReactQueryDevtools initialIsOpen={false} />
    </QueryClientProvider>
  );
}

// app/layout.tsx
import { QueryProvider } from '@/providers/QueryProvider';

export default function RootLayout({ children }: { children: React.ReactNode }) {
  return (
    <html>
      <body>
        <QueryProvider>{children}</QueryProvider>
      </body>
    </html>
  );
}
```

## 🚫 Error Handling

### Server Component Error Handling

Use try-catch in Server Components:

```typescript
// app/murabaha/page.tsx
export default async function MurabahaPage() {
  try {
    const contracts = await fetch('https://api.example.com/contracts')
      .then((res) => {
        if (!res.ok) throw new Error('Failed to fetch contracts');
        return res.json();
      });

    return <ContractList contracts={contracts} />;
  } catch (error) {
    return (
      <div>
        <h2>Failed to load contracts</h2>
        <p>Please try again later</p>
      </div>
    );
  }
}
```

### Error Boundaries

Use error.tsx for route-level error handling:

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
      <h2>Something went wrong</h2>
      <p>{error.message}</p>
      <button onClick={reset}>Try again</button>
    </div>
  );
}
```

### Loading States with Streaming

Provide loading feedback with Suspense:

```typescript
// app/dashboard/page.tsx
import { Suspense } from 'react';

export default function DashboardPage() {
  return (
    <div>
      <h1>Dashboard</h1>

      <Suspense fallback={<ContractsLoading />}>
        <ContractsData />
      </Suspense>

      <Suspense fallback={<DonationsLoading />}>
        <DonationsData />
      </Suspense>
    </div>
  );
}

async function ContractsData() {
  const contracts = await fetchContracts();
  return <ContractsList contracts={contracts} />;
}

function ContractsLoading() {
  return <div className="animate-pulse">Loading contracts...</div>;
}
```

## 🔄 Data Mutations

### Optimistic Updates

Update UI immediately before server confirmation:

```typescript
// app/_components/DonationButton.tsx
'use client';

import { useState } from 'react';
import { makeDonation } from '@/actions/donation-actions';

export function DonationButton({ amount }: { amount: number }) {
  const [isPending, setIsPending] = useState(false);
  const [donated, setDonated] = useState(false);

  async function handleDonation() {
    // Optimistic update
    setDonated(true);
    setIsPending(true);

    try {
      const result = await makeDonation(amount);

      if (!result.success) {
        // Rollback on failure
        setDonated(false);
      }
    } catch (error) {
      // Rollback on error
      setDonated(false);
    } finally {
      setIsPending(false);
    }
  }

  return (
    <button onClick={handleDonation} disabled={isPending || donated}>
      {donated ? 'Thank you!' : `Donate $${amount}`}
    </button>
  );
}
```

### Mutation with React Query

Use mutations with React Query for complex state management:

```typescript
// app/_components/ContractForm.tsx
'use client';

import { useMutation, useQueryClient } from '@tanstack/react-query';

export function ContractForm() {
  const queryClient = useQueryClient();

  const mutation = useMutation({
    mutationFn: async (data: FormData) => {
      const res = await fetch('/api/contracts', {
        method: 'POST',
        body: data,
      });
      if (!res.ok) throw new Error('Failed to create contract');
      return res.json();
    },
    onSuccess: () => {
      // Invalidate and refetch contracts
      queryClient.invalidateQueries({ queryKey: ['contracts'] });
    },
  });

  function handleSubmit(e: React.FormEvent<HTMLFormElement>) {
    e.preventDefault();
    const formData = new FormData(e.currentTarget);
    mutation.mutate(formData);
  }

  return (
    <form onSubmit={handleSubmit}>
      <input name="principalAmount" type="number" required />
      <button type="submit" disabled={mutation.isPending}>
        {mutation.isPending ? 'Creating...' : 'Create Contract'}
      </button>

      {mutation.isError && <p>Error: {mutation.error.message}</p>}
      {mutation.isSuccess && <p>Contract created successfully!</p>}
    </form>
  );
}
```

## 📊 Real-World Examples

### Zakat Calculator with Server Actions

Complete example with validation and error handling:

```typescript
// app/_actions/zakat-actions.ts
"use server";

import { z } from "zod";
import { revalidatePath } from "next/cache";
import { db } from "@/lib/db";
import { auth } from "@/lib/auth";

const zakatSchema = z.object({
  wealth: z.number().positive(),
  nisab: z.number().positive(),
  goldRate: z.number().positive(),
  silverRate: z.number().positive(),
});

export async function calculateAndSaveZakat(formData: FormData) {
  // Authentication check
  const session = await auth();
  if (!session) {
    return { success: false, error: "Unauthorized" };
  }

  try {
    // Validate input
    const validatedData = zakatSchema.parse({
      wealth: Number(formData.get("wealth")),
      nisab: Number(formData.get("nisab")),
      goldRate: Number(formData.get("goldRate")),
      silverRate: Number(formData.get("silverRate")),
    });

    // Business logic
    const eligible = validatedData.wealth >= validatedData.nisab;
    const zakatAmount = eligible ? validatedData.wealth * 0.025 : 0;

    // Save to database
    const calculation = await db.zakatCalculation.create({
      data: {
        userId: session.user.id,
        ...validatedData,
        zakatAmount,
        eligible,
        calculatedAt: new Date(),
      },
    });

    // Revalidate cache
    revalidatePath("/zakat/history");

    return {
      success: true,
      calculation: {
        id: calculation.id,
        zakatAmount,
        eligible,
      },
    };
  } catch (error) {
    if (error instanceof z.ZodError) {
      return {
        success: false,
        error: "Invalid input",
        details: error.errors,
      };
    }

    return {
      success: false,
      error: "Failed to calculate zakat",
    };
  }
}
```

### Murabaha Contract List with Pagination

Server Component with pagination:

```typescript
// app/murabaha/page.tsx
import { db } from '@/lib/db';

interface PageProps {
  searchParams: { page?: string };
}

export default async function MurabahaPage({ searchParams }: PageProps) {
  const page = Number(searchParams.page) || 1;
  const pageSize = 20;

  const [contracts, totalCount] = await Promise.all([
    db.murabahaContract.findMany({
      skip: (page - 1) * pageSize,
      take: pageSize,
      orderBy: { createdAt: 'desc' },
    }),
    db.murabahaContract.count(),
  ]);

  const totalPages = Math.ceil(totalCount / pageSize);

  return (
    <div>
      <h1>Murabaha Contracts</h1>
      <ContractList contracts={contracts} />
      <Pagination currentPage={page} totalPages={totalPages} />
    </div>
  );
}
```

## 🔗 Related Documentation

**Next.js Core**:

- [Server Components](./ex-so-plwe-fene__server-components.md) - Server Component architecture
- [Rendering Strategies](./ex-so-plwe-fene__rendering-strategies.md) - SSR, SSG, ISR
- [REST APIs](./ex-so-plwe-fene__rest-apis.md) - API route integration
- [Best Practices](./ex-so-plwe-fene__best-practices.md) - Production standards

**React Foundation**:

- [React with TypeScript](../fe-react/README.md) - React fundamentals

**Official Resources**:

- [Data Fetching Documentation](https://nextjs.org/docs/app/building-your-application/data-fetching)
- [Server Actions](https://nextjs.org/docs/app/building-your-application/data-fetching/server-actions-and-mutations)

---

This comprehensive data fetching guide covers all Next.js data fetching patterns. Use these strategies to build performant, type-safe applications with optimal data management and user experience.
