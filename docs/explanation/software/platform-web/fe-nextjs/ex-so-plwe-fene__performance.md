---
title: Next.js Performance Optimization
description: Comprehensive performance optimization guide for Next.js covering image optimization, font optimization, code splitting, Server Components advantages, streaming, caching strategies, and Core Web Vitals
category: explanation
tags:
  - nextjs
  - performance
  - optimization
  - typescript
  - core-web-vitals
  - caching
created: 2026-01-29
updated: 2026-01-29
---

# Next.js Performance Optimization

This document provides comprehensive guidance on optimizing Next.js application performance. Performance is critical for user experience, SEO rankings, and business metrics. Next.js provides built-in performance optimizations, but developers must understand and apply additional strategies for production applications.

**Prerequisites**: Familiarity with [rendering strategies](./ex-so-plwe-fene__rendering-strategies.md), [Server Components](./ex-so-plwe-fene__server-components.md), and [data fetching](./ex-so-plwe-fene__data-fetching.md).

## 🖼️ Image Optimization

### next/image Component

Always use `next/image` for automatic image optimization.

```typescript
import Image from 'next/image';

export function ZakatHero() {
  return (
    <Image
      src="/images/zakat-hero.jpg"
      alt="Zakat calculator interface"
      width={1200}
      height={600}
      priority // Load immediately (above the fold)
      placeholder="blur"
      blurDataURL="data:image/jpeg;base64,..."
    />
  );
}
```

**Benefits**:

- Automatic format selection (WebP, AVIF)
- Responsive image sizing
- Lazy loading by default
- Blur placeholder generation
- Cumulative Layout Shift (CLS) prevention

### Responsive Images

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
        sizes="(max-width: 768px) 100vw, (max-width: 1200px) 50vw, 33vw"
        style={{ width: '100%', height: 'auto' }}
      />
    </div>
  );
}
```

### Image Configuration

```typescript
// next.config.ts
const nextConfig = {
  images: {
    formats: ["image/avif", "image/webp"],
    deviceSizes: [640, 750, 828, 1080, 1200, 1920],
    imageSizes: [16, 32, 48, 64, 96, 128, 256],
    minimumCacheTTL: 60,
    remotePatterns: [
      {
        protocol: "https",
        hostname: "api.example.com",
        pathname: "/images/**",
      },
    ],
  },
};

export default nextConfig;
```

## 🔤 Font Optimization

### next/font for Google Fonts

```typescript
// app/layout.tsx
import { Inter, Cairo } from 'next/font/google';

const inter = Inter({
  subsets: ['latin'],
  display: 'swap',
  variable: '--font-inter',
  preload: true,
});

const cairo = Cairo({
  subsets: ['arabic'],
  display: 'swap',
  variable: '--font-cairo',
  weight: ['400', '700'],
});

export default function RootLayout({ children }: { children: React.ReactNode }) {
  return (
    <html lang="en" className={`${inter.variable} ${cairo.variable}`}>
      <body className={inter.className}>{children}</body>
    </html>
  );
}
```

**Benefits**:

- Zero layout shift
- Self-hosted fonts (no external requests)
- Automatic font subsetting
- Preload optimization

### Local Custom Fonts

```typescript
import localFont from "next/font/local";

const customFont = localFont({
  src: [
    {
      path: "./fonts/CustomFont-Regular.woff2",
      weight: "400",
      style: "normal",
    },
    {
      path: "./fonts/CustomFont-Bold.woff2",
      weight: "700",
      style: "normal",
    },
  ],
  variable: "--font-custom",
  display: "swap",
});
```

## 📦 Code Splitting and Lazy Loading

### Dynamic Imports

```typescript
import dynamic from 'next/dynamic';

// Lazy load heavy component
const HeavyChart = dynamic(() => import('@/components/HeavyChart'), {
  loading: () => <div>Loading chart...</div>,
  ssr: false, // Disable SSR if uses browser APIs
});

export default function AnalyticsPage() {
  return (
    <div>
      <h1>Analytics Dashboard</h1>
      <HeavyChart />
    </div>
  );
}
```

### Conditional Imports

```typescript
'use client';

import { useState } from 'react';

export function ContractForm() {
  const [showAdvanced, setShowAdvanced] = useState(false);

  async function handleShowAdvanced() {
    // Import only when needed
    const { AdvancedForm } = await import('@/components/AdvancedForm');
    setShowAdvanced(true);
  }

  return (
    <div>
      <BasicForm />
      {!showAdvanced && (
        <button onClick={handleShowAdvanced}>Show Advanced Options</button>
      )}
      {showAdvanced && <AdvancedFormComponent />}
    </div>
  );
}
```

### Package Optimization

```typescript
// next.config.ts
const nextConfig = {
  experimental: {
    optimizePackageImports: ["@mui/material", "@mui/icons-material", "lodash", "date-fns"],
  },
};

export default nextConfig;
```

## 🚀 Server Components Performance

### Default to Server Components

Server Components reduce JavaScript bundle size significantly.

```typescript
// app/murabaha/page.tsx (Server Component - default)
import { fetchContracts } from '@/lib/db';

export default async function MurabahaPage() {
  // No JavaScript sent to client for this component
  const contracts = await fetchContracts();

  return (
    <div>
      <h1>Murabaha Contracts</h1>
      <ul>
        {contracts.map((contract) => (
          <li key={contract.id}>{contract.title}</li>
        ))}
      </ul>
    </div>
  );
}
```

**Benefits**:

- Zero JavaScript for Server Components
- Direct database access
- Reduced bundle size
- Better SEO

### Minimize Client Components

```typescript
// app/zakat/page.tsx (Server Component)
import { fetchZakatRates } from '@/lib/db';
import { ZakatForm } from './_components/ZakatForm'; // Only Client Component

export default async function ZakatPage() {
  const rates = await fetchZakatRates();

  return (
    <div>
      {/* Server Component - no JS */}
      <header>
        <h1>Zakat Calculator</h1>
        <p>Current rate: ${rates.goldNisab}</p>
      </header>

      {/* Client Component - minimal JS */}
      <ZakatForm rates={rates} />

      {/* Server Component - no JS */}
      <footer>Updated: {rates.updatedAt}</footer>
    </div>
  );
}
```

## 🌊 Streaming and Suspense

### Progressive Rendering

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
      <Suspense fallback={<ContractsLoading />}>
        <ContractsList />
      </Suspense>

      <Suspense fallback={<AnalyticsLoading />}>
        <AnalyticsDashboard />
      </Suspense>
    </div>
  );
}

async function ContractsList() {
  const contracts = await fetchContracts(); // Slow query
  return <ContractsTable contracts={contracts} />;
}
```

**Benefits**:

- Faster Time to First Byte (TTFB)
- Progressive page rendering
- Better perceived performance
- Parallel data fetching

## 💾 Caching Strategies

### Fetch Caching

```typescript
// app/murabaha/page.tsx

// Cached indefinitely (default)
const staticData = await fetch("https://api.example.com/static").then((res) => res.json());

// Revalidate every hour
const periodicData = await fetch("https://api.example.com/periodic", {
  next: { revalidate: 3600 },
}).then((res) => res.json());

// No caching (always fresh)
const liveData = await fetch("https://api.example.com/live", {
  cache: "no-store",
}).then((res) => res.json());
```

### Route Segment Caching

```typescript
// app/zakat/rates/page.tsx

// Revalidate this page every hour
export const revalidate = 3600;

export default async function ZakatRatesPage() {
  const rates = await fetchZakatRates();
  return <RatesDisplay rates={rates} />;
}
```

### Tag-Based Revalidation

```typescript
// app/murabaha/page.tsx
export default async function MurabahaPage() {
  const contracts = await fetch('https://api.example.com/contracts', {
    next: { tags: ['contracts'] },
  }).then((res) => res.json());

  return <ContractList contracts={contracts} />;
}

// Revalidate when data changes
import { revalidateTag } from 'next/cache';

export async function createContract(data: ContractData) {
  await db.contract.create({ data });

  // Revalidate all 'contracts' tagged requests
  revalidateTag('contracts');
}
```

## 🎯 Route Prefetching

### Automatic Link Prefetching

```typescript
import Link from 'next/link';

export function Navigation() {
  return (
    <nav>
      {/* Automatically prefetched on hover */}
      <Link href="/zakat">Zakat Calculator</Link>

      {/* Disable prefetch for rarely visited pages */}
      <Link href="/admin" prefetch={false}>
        Admin
      </Link>
    </nav>
  );
}
```

### Programmatic Prefetching

```typescript
'use client';

import { useRouter } from 'next/navigation';
import { useEffect } from 'react';

export function Dashboard() {
  const router = useRouter();

  useEffect(() => {
    // Prefetch likely next routes
    router.prefetch('/murabaha');
    router.prefetch('/zakat');
  }, [router]);

  return <div>Dashboard</div>;
}
```

## 📊 Bundle Size Optimization

### Analyze Bundle Size

```bash
# Install bundle analyzer
npm install @next/bundle-analyzer

# Enable analysis
ANALYZE=true npm run build
```

```typescript
// next.config.ts
import bundleAnalyzer from "@next/bundle-analyzer";

const withBundleAnalyzer = bundleAnalyzer({
  enabled: process.env.ANALYZE === "true",
});

const nextConfig = {
  // Your config
};

export default withBundleAnalyzer(nextConfig);
```

### Tree Shaking

```typescript
// ❌ Imports entire lodash library
import _ from "lodash";

// ✅ Imports only needed function
import debounce from "lodash/debounce";

// ❌ Imports entire date-fns
import * as dateFns from "date-fns";

// ✅ Imports only needed functions
import { format, addDays } from "date-fns";
```

## 📈 Core Web Vitals

### Largest Contentful Paint (LCP)

**Target**: < 2.5 seconds

```typescript
// app/page.tsx
import Image from 'next/image';
import heroImage from '@/public/hero.jpg';

export default function HomePage() {
  return (
    <div>
      {/* Prioritize above-the-fold images */}
      <Image
        src={heroImage}
        alt="Hero image"
        priority // Load immediately
        placeholder="blur"
      />
    </div>
  );
}
```

### First Input Delay (FID)

**Target**: < 100 milliseconds

```typescript
'use client';

import { useTransition } from 'react';

export function ContractForm() {
  const [isPending, startTransition] = useTransition();

  function handleSubmit(formData: FormData) {
    startTransition(async () => {
      // Non-blocking state update
      await createContract(formData);
    });
  }

  return (
    <form action={handleSubmit}>
      <input name="principalAmount" />
      <button disabled={isPending}>
        {isPending ? 'Creating...' : 'Create Contract'}
      </button>
    </form>
  );
}
```

### Cumulative Layout Shift (CLS)

**Target**: < 0.1

```typescript
import Image from 'next/image';

export function ContractCard() {
  return (
    <div>
      {/* next/image prevents CLS */}
      <Image
        src="/contract.jpg"
        alt="Contract"
        width={400}
        height={300}
        style={{ width: '100%', height: 'auto' }}
      />

      {/* Reserve space for dynamic content */}
      <div style={{ minHeight: '100px' }}>
        <DynamicContent />
      </div>
    </div>
  );
}
```

## 🔄 Database Query Optimization

### Select Only Needed Fields

```typescript
// ❌ Over-fetching
const contract = await db.contract.findUnique({
  where: { id },
  // Fetches all fields
});

// ✅ Select only needed fields
const contract = await db.contract.findUnique({
  where: { id },
  select: {
    id: true,
    principalAmount: true,
    status: true,
    // Exclude unnecessary fields
  },
});
```

### Pagination

```typescript
// app/murabaha/page.tsx
export default async function MurabahaPage({
  searchParams,
}: {
  searchParams: { page?: string };
}) {
  const page = Number(searchParams.page) || 1;
  const pageSize = 20;

  const contracts = await db.contract.findMany({
    skip: (page - 1) * pageSize,
    take: pageSize,
    orderBy: { createdAt: 'desc' },
  });

  return <ContractList contracts={contracts} page={page} />;
}
```

### Database Indexing

```prisma
// prisma/schema.prisma
model MurabahaContract {
  id              String   @id @default(cuid())
  userId          String
  status          String
  principalAmount Float
  createdAt       DateTime @default(now())

  // Indexes for query performance
  @@index([userId])
  @@index([status])
  @@index([createdAt])
  @@index([userId, status])
}
```

## 🚀 Production Optimizations

### Build Configuration

```typescript
// next.config.ts
const nextConfig = {
  // Compress output
  compress: true,

  // Remove X-Powered-By header
  poweredByHeader: false,

  // Production source maps
  productionBrowserSourceMaps: false,

  // SWC minification (default in Next.js 13+)
  swcMinify: true,

  // Compiler options
  compiler: {
    removeConsole: process.env.NODE_ENV === "production",
  },
};

export default nextConfig;
```

### Static Export (if applicable)

```typescript
// next.config.ts
const nextConfig = {
  output: "export", // Static HTML export
  trailingSlash: true,
  images: {
    unoptimized: true, // Required for static export
  },
};

export default nextConfig;
```

## 📊 Performance Monitoring

### Web Vitals Reporting

```typescript
// app/layout.tsx
import { Analytics } from '@vercel/analytics/react';

export default function RootLayout({ children }: { children: React.ReactNode }) {
  return (
    <html>
      <body>
        {children}
        <Analytics />
      </body>
    </html>
  );
}
```

### Custom Web Vitals

```typescript
// app/layout.tsx
"use client";

import { useReportWebVitals } from "next/web-vitals";

export function WebVitals() {
  useReportWebVitals((metric) => {
    console.log(metric);

    // Send to analytics
    if (metric.label === "web-vital") {
      gtag("event", metric.name, {
        value: Math.round(metric.name === "CLS" ? metric.value * 1000 : metric.value),
        event_label: metric.id,
        non_interaction: true,
      });
    }
  });

  return null;
}
```

## 🎯 Performance Checklist

### Pre-Production Performance Checklist

- [ ] Use `next/image` for all images
- [ ] Use `next/font` for fonts
- [ ] Implement code splitting with dynamic imports
- [ ] Default to Server Components
- [ ] Use Suspense for streaming
- [ ] Configure fetch caching appropriately
- [ ] Implement route prefetching
- [ ] Optimize database queries (select, pagination, indexes)
- [ ] Enable compression in production
- [ ] Remove console.log in production
- [ ] Analyze bundle size
- [ ] Test Core Web Vitals (LCP < 2.5s, FID < 100ms, CLS < 0.1)
- [ ] Implement performance monitoring
- [ ] Use CDN for static assets
- [ ] Enable HTTP/2 or HTTP/3
- [ ] Implement service worker for offline support
- [ ] Optimize third-party scripts

### Performance Testing Tools

- **Lighthouse** - Chrome DevTools performance audit
- **WebPageTest** - Detailed performance analysis
- **Chrome DevTools** - Network waterfall, performance profiling
- **React Profiler** - Component render performance
- **Next.js DevTools** - Bundle analysis, route performance

## 🔗 Related Documentation

**Next.js Core**:

- [Rendering Strategies](./ex-so-plwe-fene__rendering-strategies.md) - SSR, SSG, ISR performance
- [Server Components](./ex-so-plwe-fene__server-components.md) - Server Component benefits
- [Data Fetching](./ex-so-plwe-fene__data-fetching.md) - Caching strategies
- [Best Practices](./ex-so-plwe-fene__best-practices.md) - Production standards

**Official Resources**:

- [Next.js Performance](https://nextjs.org/docs/app/building-your-application/optimizing)
- [Web.dev Core Web Vitals](https://web.dev/vitals/)
- [Image Optimization](https://nextjs.org/docs/app/building-your-application/optimizing/images)

---

This comprehensive performance guide covers essential optimization strategies for Next.js applications. Performance optimization is an ongoing process - regularly monitor metrics, analyze bottlenecks, and apply targeted optimizations based on real user data and analytics.
