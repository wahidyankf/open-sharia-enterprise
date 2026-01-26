---
title: Next.js Page Component Template
description: Production-ready template for Next.js App Router page components with metadata, data fetching, error handling, and loading states
category: explanation
tags:
  - nextjs
  - template
  - page-component
  - server-component
  - typescript
created: 2026-01-26
updated: 2026-01-26
---

# Next.js Page Component Template

Production-ready template for creating type-safe Next.js App Router page components with metadata, async data fetching, error handling, loading states, and OSE Platform patterns.

## 📋 Template Usage

**File location**: `app/[feature-path]/page.tsx`

**Use this template when**:

- Creating new pages in App Router
- Need SEO metadata
- Fetching data server-side
- Displaying content to users

## 🎯 Basic Page Template

```typescript
// app/[feature-path]/page.tsx
import { Metadata } from 'next';
import { notFound } from 'next/navigation';

// SEO Metadata
export const metadata: Metadata = {
  title: '[Feature Name] - OSE Platform',
  description: '[Brief description for SEO]',
  openGraph: {
    title: '[Feature Name]',
    description: '[Brief description]',
    type: 'website',
  },
};

// TypeScript types for props
interface PageProps {
  params: {
    // Define dynamic route params
  };
  searchParams: {
    // Define query string params
  };
};

// Main page component (Server Component by default)
export default async function [FeatureName]Page({
  params,
  searchParams,
}: PageProps) {
  // Data fetching
  const data = await fetchData();

  // Handle not found
  if (!data) {
    notFound();
  }

  return (
    <main>
      <h1>[Feature Name]</h1>
      {/* Render your content */}
    </main>
  );
}

// Helper function for data fetching
async function fetchData() {
  // Implement data fetching logic
  const res = await fetch('https://api.example.com/data', {
    cache: 'force-cache', // or 'no-store' for dynamic data
  });

  if (!res.ok) {
    throw new Error('Failed to fetch data');
  }

  return res.json();
}
```

## 📊 Page with Database Query (OSE Platform Example)

```typescript
// app/zakat/dashboard/page.tsx
import { Metadata } from 'next';
import { redirect } from 'next/navigation';
import { auth } from '@/lib/auth';
import { db } from '@/lib/db';
import { ZakatCalculationList } from '@/components/zakat/ZakatCalculationList';
import { ZakatStats } from '@/components/zakat/ZakatStats';

export const metadata: Metadata = {
  title: 'Zakat Dashboard - OSE Platform',
  description: 'View your zakat calculations and payment history',
};

interface PageProps {
  searchParams: {
    year?: string;
  };
}

export default async function ZakatDashboardPage({
  searchParams,
}: PageProps) {
  // Authentication check
  const session = await auth();

  if (!session) {
    redirect('/login');
  }

  // Parse query params
  const year = searchParams.year
    ? parseInt(searchParams.year, 10)
    : new Date().getFullYear();

  // Fetch data
  const [calculations, stats] = await Promise.all([
    getCalculations(session.user.id, year),
    getZakatStats(session.user.id),
  ]);

  return (
    <main className="container mx-auto px-4 py-8">
      <h1 className="text-3xl font-bold mb-6">Zakat Dashboard</h1>

      <ZakatStats stats={stats} />

      <section className="mt-8">
        <h2 className="text-2xl font-semibold mb-4">
          Calculations for {year}
        </h2>
        <ZakatCalculationList calculations={calculations} />
      </section>
    </main>
  );
}

// Data fetching helpers
async function getCalculations(userId: string, year: number) {
  const startDate = new Date(year, 0, 1);
  const endDate = new Date(year, 11, 31, 23, 59, 59);

  return db.zakatCalculation.findMany({
    where: {
      userId,
      createdAt: {
        gte: startDate,
        lte: endDate,
      },
    },
    orderBy: { createdAt: 'desc' },
  });
}

async function getZakatStats(userId: string) {
  const calculations = await db.zakatCalculation.findMany({
    where: { userId },
    select: {
      zakatAmount: true,
      paidAt: true,
    },
  });

  const totalPaid = calculations
    .filter((c) => c.paidAt !== null)
    .reduce((sum, c) => sum + c.zakatAmount, 0);

  const totalPending = calculations
    .filter((c) => c.paidAt === null)
    .reduce((sum, c) => sum + c.zakatAmount, 0);

  return {
    totalPaid,
    totalPending,
    calculationsCount: calculations.length,
  };
}
```

## 🔄 Dynamic Route Page

```typescript
// app/murabaha/applications/[id]/page.tsx
import { Metadata } from 'next';
import { notFound } from 'next/navigation';
import { auth } from '@/lib/auth';
import { db } from '@/lib/db';
import { ApplicationDetails } from '@/components/murabaha/ApplicationDetails';
import { PaymentSchedule } from '@/components/murabaha/PaymentSchedule';

// Dynamic metadata
export async function generateMetadata({
  params,
}: {
  params: { id: string };
}): Promise<Metadata> {
  const application = await db.murabahaApplication.findUnique({
    where: { id: params.id },
    select: { productName: true },
  });

  return {
    title: application
      ? `${application.productName} - Murabaha Application`
      : 'Application Not Found',
    description: 'View your Murabaha financing application details',
  };
}

interface PageProps {
  params: {
    id: string;
  };
}

export default async function MurabahaApplicationPage({
  params,
}: PageProps) {
  const session = await auth();

  if (!session) {
    redirect('/login');
  }

  // Fetch application with authorization check
  const application = await db.murabahaApplication.findUnique({
    where: {
      id: params.id,
      userId: session.user.id, // Verify ownership
    },
    include: {
      payments: {
        orderBy: { dueDate: 'asc' },
      },
    },
  });

  if (!application) {
    notFound();
  }

  return (
    <main className="container mx-auto px-4 py-8">
      <h1 className="text-3xl font-bold mb-6">
        Murabaha Application: {application.productName}
      </h1>

      <ApplicationDetails application={application} />

      <section className="mt-8">
        <h2 className="text-2xl font-semibold mb-4">Payment Schedule</h2>
        <PaymentSchedule payments={application.payments} />
      </section>
    </main>
  );
}

// Generate static params for known applications
export async function generateStaticParams() {
  // For static generation of common/public applications
  const applications = await db.murabahaApplication.findMany({
    where: { status: 'ACTIVE' },
    select: { id: true },
    take: 100, // Limit for build performance
  });

  return applications.map((app) => ({
    id: app.id,
  }));
}
```

## 🔍 Page with Search Params

```typescript
// app/waqf/projects/page.tsx
import { Metadata } from 'next';
import { db } from '@/lib/db';
import { ProjectCard } from '@/components/waqf/ProjectCard';
import { Pagination } from '@/components/ui/Pagination';
import { SearchForm } from '@/components/waqf/SearchForm';

export const metadata: Metadata = {
  title: 'Waqf Projects - OSE Platform',
  description: 'Browse and support ongoing waqf (endowment) projects',
};

interface PageProps {
  searchParams: {
    category?: string;
    status?: string;
    page?: string;
    q?: string;
  };
}

export default async function WaqfProjectsPage({
  searchParams,
}: PageProps) {
  // Parse and validate search params
  const category = searchParams.category ?? 'all';
  const status = searchParams.status ?? 'active';
  const page = parseInt(searchParams.page ?? '1', 10);
  const searchQuery = searchParams.q ?? '';
  const pageSize = 12;

  // Build query filters
  const where: any = {};

  if (category !== 'all') {
    where.category = category.toUpperCase();
  }

  if (status !== 'all') {
    where.status = status.toUpperCase();
  }

  if (searchQuery) {
    where.OR = [
      { name: { contains: searchQuery, mode: 'insensitive' } },
      { description: { contains: searchQuery, mode: 'insensitive' } },
    ];
  }

  // Fetch projects with pagination
  const [projects, totalCount] = await Promise.all([
    db.waqfProject.findMany({
      where,
      take: pageSize,
      skip: (page - 1) * pageSize,
      orderBy: { createdAt: 'desc' },
    }),
    db.waqfProject.count({ where }),
  ]);

  const totalPages = Math.ceil(totalCount / pageSize);

  return (
    <main className="container mx-auto px-4 py-8">
      <h1 className="text-3xl font-bold mb-6">Waqf Projects</h1>

      <SearchForm
        currentCategory={category}
        currentStatus={status}
        currentQuery={searchQuery}
      />

      {projects.length > 0 ? (
        <>
          <div className="grid grid-cols-1 md:grid-cols-2 lg:grid-cols-3 gap-6 mt-8">
            {projects.map((project) => (
              <ProjectCard key={project.id} project={project} />
            ))}
          </div>

          <Pagination
            currentPage={page}
            totalPages={totalPages}
            baseUrl="/waqf/projects"
          />
        </>
      ) : (
        <p className="text-center text-gray-600 mt-8">
          No projects found matching your criteria.
        </p>
      )}
    </main>
  );
}
```

## ⚡ Page with Streaming (Suspense)

```typescript
// app/analytics/page.tsx
import { Metadata } from 'next';
import { Suspense } from 'react';
import { auth } from '@/lib/auth';
import { redirect } from 'next/navigation';
import { ZakatAnalytics } from '@/components/analytics/ZakatAnalytics';
import { MurabahaAnalytics } from '@/components/analytics/MurabahaAnalytics';
import { WaqfAnalytics } from '@/components/analytics/WaqfAnalytics';
import { LoadingSkeleton } from '@/components/ui/LoadingSkeleton';

export const metadata: Metadata = {
  title: 'Analytics Dashboard - OSE Platform',
  description: 'View comprehensive analytics for your Islamic finance activities',
};

export default async function AnalyticsPage() {
  const session = await auth();

  if (!session) {
    redirect('/login');
  }

  return (
    <main className="container mx-auto px-4 py-8">
      <h1 className="text-3xl font-bold mb-8">Analytics Dashboard</h1>

      <div className="space-y-8">
        {/* Each section loads independently */}
        <section>
          <h2 className="text-2xl font-semibold mb-4">Zakat Analytics</h2>
          <Suspense fallback={<LoadingSkeleton />}>
            <ZakatAnalytics userId={session.user.id} />
          </Suspense>
        </section>

        <section>
          <h2 className="text-2xl font-semibold mb-4">Murabaha Analytics</h2>
          <Suspense fallback={<LoadingSkeleton />}>
            <MurabahaAnalytics userId={session.user.id} />
          </Suspense>
        </section>

        <section>
          <h2 className="text-2xl font-semibold mb-4">Waqf Analytics</h2>
          <Suspense fallback={<LoadingSkeleton />}>
            <WaqfAnalytics userId={session.user.id} />
          </Suspense>
        </section>
      </div>
    </main>
  );
}
```

## 🔒 Protected Page with Authorization

```typescript
// app/admin/users/page.tsx
import { Metadata } from 'next';
import { redirect } from 'next/navigation';
import { auth } from '@/lib/auth';
import { db } from '@/lib/db';
import { UserTable } from '@/components/admin/UserTable';

export const metadata: Metadata = {
  title: 'User Management - Admin',
  description: 'Manage platform users',
};

export default async function AdminUsersPage() {
  const session = await auth();

  // Authentication check
  if (!session) {
    redirect('/login');
  }

  // Authorization check (admin only)
  if (session.user.role !== 'ADMIN') {
    redirect('/dashboard'); // Or show 403 error
  }

  // Fetch users (admin can see all)
  const users = await db.user.findMany({
    select: {
      id: true,
      name: true,
      email: true,
      role: true,
      createdAt: true,
      _count: {
        select: {
          zakatCalculations: true,
          murabahaApplications: true,
        },
      },
    },
    orderBy: { createdAt: 'desc' },
  });

  return (
    <main className="container mx-auto px-4 py-8">
      <h1 className="text-3xl font-bold mb-6">User Management</h1>

      <div className="bg-white rounded-lg shadow p-6">
        <UserTable users={users} />
      </div>
    </main>
  );
}
```

## 📄 Static Page (ISR)

```typescript
// app/blog/[slug]/page.tsx
import { Metadata } from 'next';
import { notFound } from 'next/navigation';
import { db } from '@/lib/db';
import { BlogContent } from '@/components/blog/BlogContent';

// Revalidate every hour (ISR)
export const revalidate = 3600;

// Dynamic metadata
export async function generateMetadata({
  params,
}: {
  params: { slug: string };
}): Promise<Metadata> {
  const post = await db.blogPost.findUnique({
    where: { slug: params.slug },
  });

  if (!post) {
    return {
      title: 'Post Not Found',
    };
  }

  return {
    title: `${post.title} - OSE Platform Blog`,
    description: post.excerpt,
    openGraph: {
      title: post.title,
      description: post.excerpt,
      type: 'article',
      publishedTime: post.publishedAt.toISOString(),
    },
  };
}

interface PageProps {
  params: {
    slug: string;
  };
}

export default async function BlogPostPage({ params }: PageProps) {
  const post = await db.blogPost.findUnique({
    where: { slug: params.slug },
    include: {
      author: {
        select: {
          name: true,
          avatar: true,
        },
      },
    },
  });

  if (!post) {
    notFound();
  }

  return (
    <main className="container mx-auto px-4 py-8">
      <article>
        <header className="mb-8">
          <h1 className="text-4xl font-bold mb-4">{post.title}</h1>
          <div className="flex items-center gap-4 text-gray-600">
            <span>By {post.author.name}</span>
            <span>•</span>
            <time dateTime={post.publishedAt.toISOString()}>
              {post.publishedAt.toLocaleDateString()}
            </time>
          </div>
        </header>

        <BlogContent content={post.content} />
      </article>
    </main>
  );
}

// Generate static paths for popular posts
export async function generateStaticParams() {
  const posts = await db.blogPost.findMany({
    where: { published: true },
    select: { slug: true },
    take: 50, // Pre-render top 50 posts
  });

  return posts.map((post) => ({
    slug: post.slug,
  }));
}
```

## 🎯 Complete Example: Zakat Calculator

```typescript
// app/zakat/calculate/page.tsx
import { Metadata } from 'next';
import { redirect } from 'next/navigation';
import { auth } from '@/lib/auth';
import { db } from '@/lib/db';
import { ZakatCalculatorForm } from '@/components/zakat/ZakatCalculatorForm';
import { RecentCalculations } from '@/components/zakat/RecentCalculations';

export const metadata: Metadata = {
  title: 'Calculate Zakat - OSE Platform',
  description: 'Calculate your zakat obligation with our comprehensive calculator',
  keywords: ['zakat', 'calculator', 'Islamic finance', 'charity'],
};

export default async function CalculateZakatPage() {
  const session = await auth();

  if (!session) {
    redirect('/login');
  }

  // Fetch user's recent calculations
  const recentCalculations = await db.zakatCalculation.findMany({
    where: { userId: session.user.id },
    take: 5,
    orderBy: { createdAt: 'desc' },
  });

  // Fetch current nisab threshold
  const nisabThreshold = await getCurrentNisabThreshold();

  return (
    <main className="container mx-auto px-4 py-8">
      <div className="max-w-4xl mx-auto">
        <header className="mb-8">
          <h1 className="text-3xl font-bold mb-2">Zakat Calculator</h1>
          <p className="text-gray-600">
            Calculate your annual zakat obligation based on your wealth
          </p>
        </header>

        <div className="grid lg:grid-cols-3 gap-8">
          <div className="lg:col-span-2">
            <ZakatCalculatorForm
              userId={session.user.id}
              nisabThreshold={nisabThreshold}
            />
          </div>

          <aside>
            <h2 className="text-xl font-semibold mb-4">Recent Calculations</h2>
            <RecentCalculations calculations={recentCalculations} />
          </aside>
        </div>
      </div>
    </main>
  );
}

async function getCurrentNisabThreshold() {
  // Fetch current gold price (85 grams of gold)
  const goldPricePerGram = 60; // USD per gram (example)
  const nisabGoldGrams = 85;

  return goldPricePerGram * nisabGoldGrams;
}
```

## 📚 Best Practices

### 1. Always Define Metadata

```typescript
export const metadata: Metadata = {
  title: "Page Title",
  description: "Page description",
};
```

### 2. Type Props Explicitly

```typescript
interface PageProps {
  params: { id: string };
  searchParams: { page?: string };
}
```

### 3. Handle Authentication Early

```typescript
const session = await auth();

if (!session) {
  redirect("/login");
}
```

### 4. Use notFound() for Missing Resources

```typescript
if (!data) {
  notFound(); // Renders 404 page
}
```

### 5. Implement Error Boundaries

```typescript
// app/[feature]/error.tsx
'use client';

export default function Error({
  error,
  reset,
}: {
  error: Error;
  reset: () => void;
}) {
  return (
    <div>
      <h2>Something went wrong!</h2>
      <button onClick={reset}>Try again</button>
    </div>
  );
}
```

### 6. Optimize Data Fetching

```typescript
// Parallel fetching
const [data1, data2] = await Promise.all([fetchData1(), fetchData2()]);

// Sequential when dependent
const user = await fetchUser();
const userPosts = await fetchUserPosts(user.id);
```

### 7. Use Streaming for Slow Data

```typescript
<Suspense fallback={<Loading />}>
  <SlowComponent />
</Suspense>
```

## 🔗 Related Templates

- [Layout Template](./ex-so-plwe-tsnext-te__layout-template.md) - Page structure
- [API Route Template](./ex-so-plwe-tsnext-te__api-route-template.md) - Data endpoints
- [Server Action Template](./ex-so-plwe-tsnext-te__server-action-template.md) - Form handling

---

This template provides a comprehensive starting point for Next.js App Router pages with OSE Platform patterns and best practices.
