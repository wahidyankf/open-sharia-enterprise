---
title: "Beginner"
weight: 11000001
date: 2026-01-29T16:00:00+07:00
draft: false
description: "Master fundamental Next.js concepts through 25 annotated examples covering Server Components, Client Components, Server Actions, routing, layouts, and data fetching"
tags: ["nextjs", "react", "typescript", "server-components", "tutorial", "by-example", "beginner"]
---

This beginner tutorial covers fundamental Next.js + TypeScript concepts through 25 heavily annotated examples. Each example maintains 1-2.25 comment lines per code line to ensure deep understanding.

## Prerequisites

Before starting, ensure you understand:

- React fundamentals (components, props, state, hooks, JSX)
- TypeScript basics (types, interfaces, generics)
- JavaScript async/await patterns
- Basic web concepts (HTTP, forms, navigation)

## Group 1: Server Components (Default)

### Example 1: Basic Server Component

Server Components are Next.js default. They run on the server and send HTML to the client, enabling direct database access and zero client JavaScript.

```typescript
// app/zakat/page.tsx
// => Server Component (default - NO 'use client' directive)
// => Runs on server during request or build time
// => Perfect for data fetching, database queries
export default async function ZakatPage() {
  // => async keyword allowed in Server Components
  // => Can fetch data at component level
  const nisabRate = 85;                     // => nisabRate is 85 (grams of gold)
  const goldPrice = 950000;                 // => goldPrice is 950000 (IDR per gram)
  const nisabValue = nisabRate * goldPrice; // => nisabValue is 80750000 IDR

  // => Calculation happens on server
  // => Client receives fully rendered HTML
  return (
    <div>
      {/* => JSX rendered to HTML on server */}
      <h1>Zakat Calculator</h1>
      {/* => No hydration needed for static content */}

      <p>Gold Nisab: {nisabRate} grams</p>
      {/* => Output: "Gold Nisab: 85 grams" */}

      <p>Current Price: IDR {goldPrice.toLocaleString()}</p>
      {/* => Output: "Current Price: IDR 950,000" */}

      <p>Nisab Value: IDR {nisabValue.toLocaleString()}</p>
      {/* => Output: "Nisab Value: IDR 80,750,000" */}
    </div>
  );
  // => Entire component output sent as HTML
  // => Zero JavaScript shipped to client for this component
}
```

**Key Takeaway**: Server Components run on the server, can be async, and send HTML to the client. They're the default in Next.js App Router and require no 'use client' directive.

**Expected Output**: Page displays Zakat calculator information with formatted IDR values. View source shows fully rendered HTML, no client-side hydration JavaScript.

**Common Pitfalls**: Trying to use React hooks (useState, useEffect) in Server Components - they only work in Client Components with 'use client' directive.

### Example 2: Server Component with Data Fetching

Server Components can fetch data directly using async/await. Fetch results are automatically cached and deduped across the application.

```typescript
// app/posts/page.tsx
// => Server Component with async data fetching
// => Fetch happens on server, not client
export default async function PostsPage() {
  // => await is allowed in Server Components
  // => Fetch is automatically cached by Next.js
  const res = await fetch('https://jsonplaceholder.typicode.com/posts', {
    // => Fetch options control caching behavior
    next: { revalidate: 3600 }              // => Revalidate every 1 hour (3600 seconds)
  });
  // => res is Response object

  const posts = await res.json();           // => posts is Post[] array from API
  // => posts is [{id: 1, title: "...", body: "..."}, ...]

  return (
    <div>
      <h2>Blog Posts</h2>
      <ul>
        {/* => Map posts array to list items */}
        {posts.slice(0, 5).map((post: any) => (
          // => post.id ensures unique React key
          <li key={post.id}>
            {/* => Display post title */}
            <strong>{post.title}</strong>
            {/* => Output: "sunt aut facere repellat..." */}

            <p>{post.body.slice(0, 100)}...</p>
            {/* => First 100 chars of body */}
          </li>
        ))}
      </ul>
    </div>
  );
  // => Component returns after data is fetched
  // => Client receives fully populated HTML
}
```

**Key Takeaway**: Server Components can use async/await to fetch data. Use `next.revalidate` option to control cache duration and automatic revalidation.

**Expected Output**: Page displays 5 blog posts with titles and truncated body text. Data is fetched on server, HTML sent to client.

**Common Pitfalls**: Forgetting to handle loading states (use loading.tsx file or Suspense boundaries), or not setting appropriate revalidation times.

### Example 3: Adding Client Component with 'use client'

Client Components opt-in with 'use client' directive. They enable React hooks, event handlers, and browser APIs.

```typescript
// app/counter/page.tsx
// => NO 'use client' - this is Server Component wrapper
// => Server Components can import and render Client Components
import CounterButton from './CounterButton';

export default function CounterPage() {
  // => Server Component renders static content
  return (
    <div>
      <h1>Donation Counter</h1>
      {/* => Static heading rendered on server */}

      <CounterButton />
      {/* => Client Component imported and used */}
      {/* => Boundary between Server and Client */}
    </div>
  );
}

// app/counter/CounterButton.tsx
// => 'use client' directive REQUIRED for hooks and interactivity
'use client';

// => React import needed for hooks
import { useState } from 'react';

export default function CounterButton() {
  // => useState only works in Client Components
  const [count, setCount] = useState(0);    // => count starts at 0

  // => Event handler requires Client Component
  const handleClick = () => {
    // => Increment count by 1
    setCount(count + 1);                    // => count becomes count + 1
  };

  return (
    <div>
      <p>Donations: {count}</p>
      {/* => Output: "Donations: 0" initially */}
      {/* => Updates to "Donations: 1", "Donations: 2", etc. */}

      <button onClick={handleClick}>
        {/* => onClick handler requires Client Component */}
        Donate
      </button>
    </div>
  );
}
```

**Key Takeaway**: Use 'use client' directive to create Client Components that can use React hooks and event handlers. Server Components can import and render Client Components.

**Expected Output**: Page shows "Donation Counter" heading (static) and interactive counter button that increments when clicked (client-side).

**Common Pitfalls**: Putting 'use client' in parent when only child needs it (splits components to minimize client JavaScript), or forgetting 'use client' and getting "You're importing a component that needs useState" error.

## Group 2: File-Based Routing

### Example 4: Creating Pages (page.tsx)

Next.js uses file-based routing. Each `page.tsx` file creates a route automatically based on folder structure.

```typescript
// app/page.tsx
// => Root route: "/" (homepage)
// => File name 'page.tsx' is special - creates accessible route
export default function HomePage() {
  // => Renders at domain.com/
  return (
    <div>
      <h1>Welcome to Islamic Finance Platform</h1>
      {/* => Homepage content */}
      <p>Learn about Sharia-compliant financial products.</p>
    </div>
  );
}

// app/about/page.tsx
// => Folder 'about' + file 'page.tsx' creates route "/about"
// => Accessible at domain.com/about
export default function AboutPage() {
  return (
    <div>
      <h1>About Us</h1>
      <p>We provide Sharia-compliant financial education.</p>
    </div>
  );
}

// app/products/murabaha/page.tsx
// => Nested folder structure creates route "/products/murabaha"
// => Accessible at domain.com/products/murabaha
export default function MurabahaPage() {
  return (
    <div>
      <h1>Murabaha Financing</h1>
      <p>Cost-plus financing for asset purchases.</p>
    </div>
  );
}
```

**Key Takeaway**: File system is the router. `page.tsx` files create routes based on their folder path. Nested folders create nested routes.

**Expected Output**: Three routes accessible at /, /about, and /products/murabaha displaying respective content.

**Common Pitfalls**: Creating .tsx files without 'page' in name (won't create routes), or forgetting that only page.tsx files are publicly accessible.

### Example 5: Creating Layouts (layout.tsx)

Layouts wrap page content and persist across route changes. They prevent unnecessary re-renders and enable shared UI.

```typescript
// app/layout.tsx
// => Root layout: wraps ALL pages in application
// => Required file - every Next.js app must have root layout
export default function RootLayout({
  children,
}: {
  children: React.ReactNode;               // => Type annotation for children prop
}) {
  // => children is page content passed automatically
  return (
    <html lang="en">
      {/* => html and body tags REQUIRED in root layout */}
      <body>
        {/* => Header visible on all pages */}
        <header>
          <nav>Islamic Finance Platform</nav>
          {/* => Navigation stays mounted during route changes */}
        </header>

        <main>
          {children}
          {/* => page.tsx content renders here */}
          {/* => Changes based on current route */}
        </main>

        {/* => Footer visible on all pages */}
        <footer>© 2026 Islamic Finance</footer>
      </body>
    </html>
  );
  // => Layout persists, only children change on navigation
}

// app/products/layout.tsx
// => Nested layout: wraps only /products/* pages
// => Inherits from root layout
export default function ProductsLayout({
  children,
}: {
  children: React.ReactNode;
}) {
  return (
    <div>
      {/* => Sidebar visible on all product pages */}
      <aside>
        <h3>Products</h3>
        <ul>
          <li>Murabaha</li>
          <li>Ijarah</li>
          <li>Musharakah</li>
        </ul>
      </aside>

      <div>
        {children}
        {/* => Product page content renders here */}
      </div>
    </div>
  );
  // => Products layout wraps product pages
  // => Root layout wraps products layout
}
```

**Key Takeaway**: Root layout is required and wraps all pages. Nested layouts wrap specific route segments. Layouts persist during navigation, preventing re-renders.

**Expected Output**: All pages show header/footer from root layout. Product pages additionally show sidebar from products layout.

**Common Pitfalls**: Forgetting html/body tags in root layout (Next.js error), or putting 'use client' in layouts when pages need to be Server Components.

### Example 6: Navigation with Link Component

Next.js Link component enables client-side navigation with prefetching. It's faster than browser navigation and maintains application state.

```typescript
// app/page.tsx
// => Import Link from next/link (NOT react-router)
import Link from 'next/link';

export default function HomePage() {
  return (
    <div>
      <h1>Islamic Finance Courses</h1>

      {/* => Link component for client-side navigation */}
      <nav>
        <Link href="/courses/zakat">
          {/* => href prop specifies destination route */}
          Zakat Calculation
        </Link>
        {/* => Prefetches /courses/zakat on hover */}
        {/* => Instant navigation when clicked */}

        <Link href="/courses/murabaha">
          Murabaha Basics
        </Link>
        {/* => Multiple links on same page */}

        <Link href="/about">
          About Us
        </Link>
      </nav>

      {/* => External links use regular <a> tags */}
      <a href="https://example.com" target="_blank" rel="noopener noreferrer">
        External Resource
      </a>
      {/* => Opens in new tab, no prefetching */}
    </div>
  );
}
```

**Key Takeaway**: Use Link component for internal navigation (prefetches on hover, instant client-side routing). Use regular `<a>` tags for external links.

**Expected Output**: Clicking links navigates instantly without full page reload. Hover shows prefetch activity in Network tab.

**Common Pitfalls**: Using `<a>` tags for internal links (causes full page reload, slower), or using Link for external URLs (unnecessary overhead).

### Example 7: Dynamic Routes with [param]

Dynamic routes use [brackets] in folder/file names. They capture URL segments as params accessible in page components.

```typescript
// app/products/[id]/page.tsx
// => [id] folder creates dynamic route
// => Matches /products/1, /products/2, /products/abc, etc.

// => PageProps type for type safety
type PageProps = {
  params: { id: string };                   // => params.id is URL segment
};

export default function ProductPage({ params }: PageProps) {
  // => params object passed automatically by Next.js
  // => params.id is "1" for /products/1
  // => params.id is "murabaha" for /products/murabaha

  return (
    <div>
      <h1>Product: {params.id}</h1>
      {/* => Output: "Product: murabaha" for /products/murabaha */}

      <p>Viewing details for product {params.id}</p>
    </div>
  );
}

// app/blog/[year]/[month]/[slug]/page.tsx
// => Multiple dynamic segments
// => Matches /blog/2026/01/zakat-guide

type BlogPageProps = {
  params: {
    year: string;                           // => year is "2026"
    month: string;                          // => month is "01"
    slug: string;                           // => slug is "zakat-guide"
  };
};

export default function BlogPostPage({ params }: BlogPageProps) {
  return (
    <div>
      <h1>Blog Post: {params.slug}</h1>
      {/* => Output: "Blog Post: zakat-guide" */}

      <time>
        {params.month}/{params.year}
        {/* => Output: "01/2026" */}
      </time>
    </div>
  );
}
```

**Key Takeaway**: Use [param] folders for dynamic URL segments. Next.js passes matched segments as params prop to page component.

**Expected Output**: URLs like /products/murabaha render product page with ID "murabaha". Blog URLs show year, month, and slug from URL.

**Common Pitfalls**: Forgetting that params are always strings (convert to numbers if needed), or not handling invalid param values.

## Group 3: Server Actions (Forms & Mutations)

### Example 8: Basic Server Action for Form Handling

Server Actions are async functions that run on the server. They enable backend logic without API routes, with automatic progressive enhancement.

```typescript
// app/donate/page.tsx
// => Server Component (can use Server Actions directly)

// => Server Action: async function with 'use server' directive
async function handleDonation(formData: FormData) {
  'use server';
  // => 'use server' marks this as Server Action
  // => Runs on server when form submitted

  // => FormData API extracts form values
  const name = formData.get('name') as string;      // => name is "Ahmad"
  const amount = formData.get('amount') as string;  // => amount is "100000"

  // => Server-side processing
  console.log(`Donation from ${name}: IDR ${amount}`);
  // => Server console output: "Donation from Ahmad: IDR 100000"

  // => Could save to database here
  // await db.donations.create({ name, amount: parseInt(amount) });
}

export default function DonatePage() {
  return (
    <div>
      <h1>Make a Donation</h1>

      {/* => Form uses Server Action as action prop */}
      <form action={handleDonation}>
        {/* => action accepts async function */}

        <label>
          Name:
          <input type="text" name="name" required />
          {/* => name attribute required for FormData */}
        </label>

        <label>
          Amount (IDR):
          <input type="number" name="amount" required />
        </label>

        <button type="submit">Donate</button>
        {/* => Submit triggers Server Action */}
        {/* => Works WITHOUT JavaScript (progressive enhancement) */}
      </form>
    </div>
  );
}
```

**Key Takeaway**: Server Actions are async functions with 'use server' directive. They handle form submissions on the server and work without client JavaScript.

**Expected Output**: Form submission logs donation to server console. Page refreshes showing updated state. Works even if JavaScript disabled.

**Common Pitfalls**: Forgetting 'use server' directive (function runs on client), or not using FormData API to extract values.

### Example 9: Server Action with Validation

Server Actions should validate input before processing. Return validation errors to show in UI.

```typescript
// app/zakat/calculate/page.tsx
// => Server Component with validated Server Action

// => Type for validation result
type ActionResult = {
  success: boolean;
  message?: string;
  zakatAmount?: number;
};

async function calculateZakat(formData: FormData): Promise<ActionResult> {
  'use server';
  // => Server Action with validation and return value

  // => Extract form data
  const wealthStr = formData.get('wealth') as string;  // => wealthStr is "100000000"
  const wealth = parseInt(wealthStr);                  // => wealth is 100000000

  // => Server-side validation
  if (isNaN(wealth)) {
    // => Invalid input
    return {
      success: false,
      message: 'Please enter a valid number',
    };
  }

  if (wealth < 0) {
    return {
      success: false,
      message: 'Wealth cannot be negative',
    };
  }

  // => Nisab threshold (85 grams gold * IDR 950,000)
  const nisab = 85 * 950000;                           // => nisab is 80,750,000

  if (wealth < nisab) {
    return {
      success: false,
      message: `Wealth below nisab threshold (IDR ${nisab.toLocaleString()})`,
    };
  }

  // => Calculate 2.5% zakat
  const zakatAmount = wealth * 0.025;                  // => zakatAmount is 2,500,000

  return {
    success: true,
    message: 'Zakat calculated successfully',
    zakatAmount,
  };
}

export default function ZakatCalculatorPage() {
  return (
    <div>
      <h1>Zakat Calculator</h1>

      <form action={calculateZakat}>
        <label>
          Total Wealth (IDR):
          <input type="number" name="wealth" required />
        </label>

        <button type="submit">Calculate</button>
      </form>

      {/* => Result display would use useFormState hook */}
      {/* => See intermediate examples for full implementation */}
    </div>
  );
}
```

**Key Takeaway**: Server Actions can return validation results. Always validate input server-side even if client-side validation exists.

**Expected Output**: Form submission validates wealth amount. Returns error messages for invalid input or amount below nisab threshold.

**Common Pitfalls**: Trusting client-side validation alone (can be bypassed), or not handling all edge cases (NaN, negative numbers, etc.).

### Example 10: Server Action with Revalidation

Server Actions can revalidate cached data after mutations. Use revalidatePath or revalidateTag to refresh specific routes.

```typescript
// app/actions.ts
// => Separate file for reusable Server Actions
'use server';
// => 'use server' at top makes all exports Server Actions

import { revalidatePath } from 'next/cache';
// => Import revalidation function from Next.js

export async function addPost(formData: FormData) {
  // => Server Action that mutates data
  const title = formData.get('title') as string;     // => title is "New Post"
  const content = formData.get('content') as string; // => content is "Post content..."

  // => Save to database
  // await db.posts.create({ title, content });
  console.log(`Created post: ${title}`);
  // => Server output: "Created post: New Post"

  // => Revalidate /posts route to show new post
  revalidatePath('/posts');
  // => Next.js re-renders /posts with fresh data
  // => Cached version invalidated, new data fetched
}

// app/posts/new/page.tsx
import { addPost } from '@/app/actions';
// => Import Server Action from actions file

export default function NewPostPage() {
  return (
    <div>
      <h1>Create Post</h1>

      <form action={addPost}>
        {/* => Server Action from separate file */}

        <label>
          Title:
          <input type="text" name="title" required />
        </label>

        <label>
          Content:
          <textarea name="content" required />
        </label>

        <button type="submit">Publish</button>
      </form>
    </div>
  );
}
```

**Key Takeaway**: Use revalidatePath() in Server Actions to refresh cached routes after data mutations. Ensures users see updated data immediately.

**Expected Output**: After form submission, /posts page automatically refreshes to show new post without manual reload.

**Common Pitfalls**: Forgetting to revalidate (users see stale data), or revalidating wrong path (target the affected route).

## Group 4: Data Fetching Patterns

### Example 11: Parallel Data Fetching

Server Components can fetch multiple data sources in parallel using Promise.all. Improves performance by avoiding sequential waterfalls.

```typescript
// app/dashboard/page.tsx
// => Server Component with parallel data fetching

// => Define data fetch functions
async function getUser() {
  // => Simulated API call
  await new Promise(resolve => setTimeout(resolve, 1000)); // => 1 second delay
  return { name: 'Ahmad', email: 'ahmad@example.com' };
  // => Returns user data
}

async function getDonations() {
  await new Promise(resolve => setTimeout(resolve, 1000)); // => 1 second delay
  return [
    { id: 1, amount: 100000 },
    { id: 2, amount: 250000 },
  ];
  // => Returns donation array
}

async function getStats() {
  await new Promise(resolve => setTimeout(resolve, 1000)); // => 1 second delay
  return { totalDonations: 350000, donorCount: 2 };
  // => Returns statistics
}

export default async function DashboardPage() {
  // => Parallel data fetching with Promise.all
  const [user, donations, stats] = await Promise.all([
    getUser(),                              // => Fetches simultaneously
    getDonations(),                         // => Not sequential
    getStats(),
  ]);
  // => All three requests complete in ~1 second (parallel)
  // => Sequential would take ~3 seconds

  return (
    <div>
      <h1>Dashboard for {user.name}</h1>
      {/* => Output: "Dashboard for Ahmad" */}

      <div>
        <h2>Statistics</h2>
        <p>Total: IDR {stats.totalDonations.toLocaleString()}</p>
        <p>Donors: {stats.donorCount}</p>
      </div>

      <div>
        <h2>Recent Donations</h2>
        <ul>
          {donations.map(donation => (
            <li key={donation.id}>
              IDR {donation.amount.toLocaleString()}
            </li>
          ))}
        </ul>
      </div>
    </div>
  );
}
```

**Key Takeaway**: Use Promise.all() to fetch multiple data sources in parallel. Dramatically reduces page load time compared to sequential fetching.

**Expected Output**: Dashboard loads in ~1 second (parallel) instead of ~3 seconds (sequential). Shows user name, statistics, and donations.

**Common Pitfalls**: Sequential await calls (each waits for previous), or not handling Promise.all rejection (one failure rejects all).

### Example 12: Request Memoization (Automatic Deduplication)

Next.js automatically deduplicates identical fetch requests in a single render pass. Multiple components can fetch same data without redundant requests.

```typescript
// app/components/Header.tsx
// => Server Component fetching user data
async function getUser() {
  console.log('Fetching user data...');     // => Log to verify dedupe
  const res = await fetch('https://api.example.com/user');
  return res.json();
}

export async function Header() {
  const user = await getUser();             // => First call: actual fetch
  // => user is { name: "Fatima", role: "admin" }

  return (
    <header>
      <span>Welcome, {user.name}</span>
      {/* => Output: "Welcome, Fatima" */}
    </header>
  );
}

// app/components/Sidebar.tsx
// => Different component, same fetch function
export async function Sidebar() {
  const user = await getUser();             // => Deduped: uses cached result
  // => No second network request
  // => user is { name: "Fatima", role: "admin" }

  return (
    <aside>
      <p>Role: {user.role}</p>
      {/* => Output: "Role: admin" */}
    </aside>
  );
}

// app/page.tsx
// => Parent component using both
import { Header } from './components/Header';
import { Sidebar } from './components/Sidebar';

export default function HomePage() {
  return (
    <div>
      <Header />
      {/* => Fetches user data */}

      <Sidebar />
      {/* => Reuses fetched user data (deduped) */}
      {/* => Only ONE network request total */}
    </div>
  );
}
```

**Key Takeaway**: Next.js automatically deduplicates identical fetch requests during render. Multiple components can safely fetch same data without performance penalty.

**Expected Output**: Server logs "Fetching user data..." only once despite two components calling getUser(). Single network request serves both.

**Common Pitfalls**: Assuming you need manual caching (Next.js handles it), or using different fetch URLs that could be the same (dedupe requires exact match).

## Group 5: Loading States

### Example 13: Loading UI with loading.tsx

Create loading.tsx file to show instant loading states while page data fetches. Automatically wraps page in Suspense boundary.

```typescript
// app/posts/loading.tsx
// => Special file: shows while posts/page.tsx loads
// => Next.js automatically wraps page in Suspense
export default function Loading() {
  // => Rendered immediately while page fetches data
  return (
    <div>
      <h2>Loading Posts...</h2>
      {/* => User sees this instantly */}

      <div className="skeleton">
        {/* => Skeleton loading UI */}
        <div className="skeleton-line" />
        <div className="skeleton-line" />
        <div className="skeleton-line" />
      </div>
    </div>
  );
  // => Replaced with actual page when data ready
}

// app/posts/page.tsx
// => Server Component with slow data fetch
export default async function PostsPage() {
  // => Simulated slow API
  await new Promise(resolve => setTimeout(resolve, 2000)); // => 2 second delay

  const posts = [
    { id: 1, title: 'Zakat Guide' },
    { id: 2, title: 'Murabaha Basics' },
  ];

  return (
    <div>
      <h2>Posts</h2>
      <ul>
        {posts.map(post => (
          <li key={post.id}>{post.title}</li>
        ))}
      </ul>
    </div>
  );
  // => Renders after 2 second delay
  // => Replaces loading.tsx content
}
```

**Key Takeaway**: Create loading.tsx alongside page.tsx for instant loading states. Next.js automatically wraps page in Suspense, showing loading UI immediately.

**Expected Output**: Navigate to /posts shows "Loading Posts..." immediately, then actual posts after 2 seconds.

**Common Pitfalls**: Not providing loading states (users see blank screen), or making loading UI too complex (should be instant, lightweight).

### Example 14: Manual Suspense Boundaries for Granular Loading

Use React Suspense to show loading states for specific components rather than entire page.

```typescript
// app/dashboard/page.tsx
import { Suspense } from 'react';
// => Import Suspense from React

// => Slow component (fetches data)
async function DonationList() {
  await new Promise(resolve => setTimeout(resolve, 2000)); // => 2 second delay

  const donations = [
    { id: 1, amount: 100000 },
    { id: 2, amount: 250000 },
  ];

  return (
    <ul>
      {donations.map(d => (
        <li key={d.id}>IDR {d.amount.toLocaleString()}</li>
      ))}
    </ul>
  );
}

// => Fast component (no data fetch)
function QuickStats() {
  // => Static content, renders immediately
  return (
    <div>
      <h2>Quick Stats</h2>
      <p>Last updated: Now</p>
    </div>
  );
}

export default function DashboardPage() {
  return (
    <div>
      <h1>Dashboard</h1>

      {/* => QuickStats renders immediately */}
      <QuickStats />

      {/* => Suspense boundary for slow component */}
      <Suspense fallback={<p>Loading donations...</p>}>
        {/* => Shows fallback while DonationList fetches data */}
        <DonationList />
        {/* => Replaced with actual list after 2 seconds */}
      </Suspense>
    </div>
  );
  // => Page partially rendered: QuickStats visible, donations loading
}
```

**Key Takeaway**: Use Suspense boundaries to show loading states for specific components. Fast content renders immediately, slow content shows fallback.

**Expected Output**: Dashboard shows header and QuickStats immediately. "Loading donations..." appears, then replaced with actual list after 2 seconds.

**Common Pitfalls**: Wrapping entire page in Suspense (use loading.tsx instead), or not providing fallback (Suspense requires fallback prop).

## Group 6: Error Handling

### Example 15: Error Boundaries with error.tsx

Create error.tsx to catch errors in page segments. Automatically wraps page in error boundary with retry capability.

```typescript
// app/posts/error.tsx
'use client';
// => Error boundaries MUST be Client Components
// => Needs 'use client' directive

export default function Error({
  error,
  reset,
}: {
  error: Error & { digest?: string };      // => Error object with digest
  reset: () => void;                        // => Function to retry
}) {
  // => error.message contains error text
  // => error.digest is unique error identifier for logging

  return (
    <div>
      <h2>Something went wrong!</h2>
      {/* => User-friendly error heading */}

      <p>{error.message}</p>
      {/* => Display error message */}
      {/* => Output: "Failed to fetch posts" */}

      <button onClick={reset}>
        {/* => reset() retries the page render */}
        Try Again
      </button>
      {/* => Click re-renders page, might succeed */}
    </div>
  );
}

// app/posts/page.tsx
// => Server Component that might throw error
export default async function PostsPage() {
  // => Simulated error
  const shouldFail = Math.random() > 0.5;   // => 50% chance of failure

  if (shouldFail) {
    throw new Error('Failed to fetch posts');
    // => Error caught by error.tsx
  }

  return (
    <div>
      <h2>Posts</h2>
      <p>Success! Posts loaded.</p>
    </div>
  );
}
```

**Key Takeaway**: Create error.tsx Client Component to catch errors in route segment. Provides error info and reset function for retry.

**Expected Output**: 50% of page loads show error UI with retry button. Clicking retry re-renders page (might succeed or fail again).

**Common Pitfalls**: Forgetting 'use client' in error.tsx (must be Client Component), or not handling errors gracefully (show user-friendly messages).

### Example 16: Not Found Pages with not-found.tsx

Create not-found.tsx for custom 404 pages when resource doesn't exist. Use notFound() function to trigger it programmatically.

```typescript
// app/products/[id]/not-found.tsx
// => Custom 404 page for product routes
export default function ProductNotFound() {
  // => Rendered when notFound() called or route doesn't exist
  return (
    <div>
      <h2>Product Not Found</h2>
      {/* => Custom 404 heading */}

      <p>The product you're looking for doesn't exist.</p>

      <a href="/products">
        Back to Products
      </a>
      {/* => Link back to valid route */}
    </div>
  );
}

// app/products/[id]/page.tsx
import { notFound } from 'next/navigation';
// => Import notFound function

// => Simulated database
const products = [
  { id: '1', name: 'Murabaha' },
  { id: '2', name: 'Ijarah' },
];

export default function ProductPage({
  params,
}: {
  params: { id: string };
}) {
  // => Find product by ID
  const product = products.find(p => p.id === params.id);

  if (!product) {
    // => Product doesn't exist
    notFound();
    // => Triggers not-found.tsx rendering
    // => Function does not return
  }

  // => Product exists
  return (
    <div>
      <h1>{product.name}</h1>
      <p>Product ID: {product.id}</p>
    </div>
  );
}
```

**Key Takeaway**: Create not-found.tsx for custom 404 pages. Use notFound() function to programmatically trigger 404 when resource doesn't exist.

**Expected Output**: /products/1 shows Murabaha product. /products/999 shows custom "Product Not Found" page.

**Common Pitfalls**: Not calling notFound() when resource missing (shows error instead of 404), or forgetting to create not-found.tsx (shows default Next.js 404).

## Group 7: Metadata & SEO

### Example 17: Static Metadata

Export metadata object from page to set title, description, and Open Graph tags. Crucial for SEO and social sharing.

```typescript
// app/about/page.tsx
import { Metadata } from 'next';
// => Import Metadata type

// => Static metadata export
export const metadata: Metadata = {
  // => metadata object defines page metadata
  title: 'About Islamic Finance Platform',
  // => <title> tag: "About Islamic Finance Platform"

  description: 'Learn about our Sharia-compliant financial education platform.',
  // => <meta name="description"> for search engines

  openGraph: {
    // => Open Graph tags for social media sharing
    title: 'About Islamic Finance Platform',
    // => og:title for Facebook, LinkedIn

    description: 'Sharia-compliant financial education',
    // => og:description

    type: 'website',
    // => og:type
  },
};

export default function AboutPage() {
  return (
    <div>
      <h1>About Us</h1>
      <p>We provide Sharia-compliant financial education.</p>
    </div>
  );
}
```

**Key Takeaway**: Export metadata object to set page title, description, and Open Graph tags. Improves SEO and social media sharing appearance.

**Expected Output**: Page title shows "About Islamic Finance Platform" in browser tab. Sharing on social media shows custom title/description.

**Common Pitfalls**: Not setting metadata (uses default title), or forgetting description meta tag (reduces SEO effectiveness).

### Example 18: Dynamic Metadata with generateMetadata

Use generateMetadata function to create metadata based on dynamic route parameters or fetched data.

```typescript
// app/products/[id]/page.tsx
import { Metadata } from 'next';

// => Simulated product data
const products = [
  { id: 'murabaha', name: 'Murabaha Financing', description: 'Cost-plus financing' },
  { id: 'ijarah', name: 'Ijarah Leasing', description: 'Islamic leasing' },
];

// => generateMetadata function for dynamic metadata
export async function generateMetadata({
  params,
}: {
  params: { id: string };
}): Promise<Metadata> {
  // => Receives same params as page component
  // => params.id is "murabaha" for /products/murabaha

  // => Fetch product data
  const product = products.find(p => p.id === params.id);

  if (!product) {
    // => Product not found, use default metadata
    return {
      title: 'Product Not Found',
    };
  }

  // => Return metadata for found product
  return {
    title: `${product.name} | Islamic Finance`,
    // => title is "Murabaha Financing | Islamic Finance"

    description: product.description,
    // => description is "Cost-plus financing"

    openGraph: {
      title: product.name,
      description: product.description,
    },
  };
  // => Metadata changes based on product ID
}

export default function ProductPage({
  params,
}: {
  params: { id: string };
}) {
  const product = products.find(p => p.id === params.id);

  if (!product) return <p>Not found</p>;

  return (
    <div>
      <h1>{product.name}</h1>
      <p>{product.description}</p>
    </div>
  );
}
```

**Key Takeaway**: Use generateMetadata() for dynamic metadata based on route params or data. Returns Metadata object like static metadata but computed at request time.

**Expected Output**: /products/murabaha shows "Murabaha Financing | Islamic Finance" title. /products/ijarah shows "Ijarah Leasing | Islamic Finance".

**Common Pitfalls**: Not handling missing data cases (return fallback metadata), or fetching data twice (in generateMetadata and page - use single fetch, Next.js dedupes).

## Group 8: Image Optimization

### Example 19: Image Component for Optimization

Use next/image for automatic image optimization, lazy loading, and responsive sizing. Dramatically improves performance.

```typescript
// app/page.tsx
import Image from 'next/image';
// => Import Image from next/image (NOT img tag)

export default function HomePage() {
  return (
    <div>
      <h1>Islamic Finance Products</h1>

      {/* => Image component with optimization */}
      <Image
        src="/mosque.jpg"
        // => Image path in public/ folder

        alt="Beautiful mosque with Islamic architecture"
        // => REQUIRED: descriptive alt text for accessibility

        width={800}
        // => REQUIRED: image width in pixels

        height={600}
        // => REQUIRED: image height in pixels
        // => Prevents layout shift during loading

        priority
        // => OPTIONAL: load image immediately (above-fold images)
        // => Skips lazy loading for important images
      />
      {/* => Next.js automatically: */}
      {/* => - Optimizes image format (WebP, AVIF) */}
      {/* => - Resizes based on device screen size */}
      {/* => - Lazy loads (except priority images) */}

      {/* => Below-fold image with lazy loading */}
      <Image
        src="/finance-chart.png"
        alt="Financial growth chart showing returns"
        width={600}
        height={400}
        // => No priority prop: lazy loads when scrolled into view
      />
    </div>
  );
}
```

**Key Takeaway**: Use Image component instead of img tag for automatic optimization, responsive sizing, and lazy loading. Always provide alt text, width, and height.

**Expected Output**: Images load in optimized WebP/AVIF format at appropriate sizes for device. Lazy loading improves initial page load time.

**Common Pitfalls**: Using img tag instead of Image (no optimization), forgetting alt text (accessibility fail), or not providing width/height (layout shift).

### Example 20: Responsive Images with fill Property

Use fill property for images that should fill their container (responsive width/height based on parent).

```typescript
// app/gallery/page.tsx
import Image from 'next/image';

export default function GalleryPage() {
  return (
    <div>
      <h1>Islamic Art Gallery</h1>

      {/* => Container with defined dimensions */}
      <div style={{ position: 'relative', width: '100%', height: '400px' }}>
        {/* => Parent MUST have position: relative */}

        <Image
          src="/islamic-calligraphy.jpg"
          alt="Beautiful Arabic calligraphy"
          fill
          // => fill makes image cover container
          // => No width/height props needed

          style={{ objectFit: 'cover' }}
          // => objectFit controls how image fills container
          // => 'cover': crops to fill, maintains aspect ratio
          // => 'contain': fits inside, may have empty space
        />
        {/* => Image automatically responsive to container size */}
      </div>

      {/* => Grid of responsive images */}
      <div style={{ display: 'grid', gridTemplateColumns: 'repeat(3, 1fr)', gap: '1rem' }}>
        {[1, 2, 3].map(id => (
          <div key={id} style={{ position: 'relative', aspectRatio: '16/9' }}>
            {/* => aspectRatio maintains proportions */}

            <Image
              src={`/gallery-${id}.jpg`}
              alt={`Gallery image ${id}`}
              fill
              style={{ objectFit: 'cover' }}
            />
          </div>
        ))}
      </div>
    </div>
  );
}
```

**Key Takeaway**: Use fill property for responsive images that adapt to container size. Parent must have position: relative. Use objectFit to control scaling behavior.

**Expected Output**: Images fill their containers responsively, adapting to screen size. Grid shows three images maintaining aspect ratio.

**Common Pitfalls**: Forgetting position: relative on parent (image won't display), or not setting container dimensions (image has no size reference).

## Group 9: Route Handlers (API Routes)

### Example 21: GET Route Handler

Route Handlers are API endpoints in App Router. Create route.ts files to handle HTTP requests with exported HTTP method functions.

```typescript
// app/api/zakat/route.ts
// => API route at /api/zakat
// => route.ts is special filename for Route Handlers

import { NextResponse } from 'next/server';
// => Import NextResponse for typed responses

// => GET handler: responds to GET /api/zakat
export async function GET() {
  // => Exported async function named after HTTP method

  // => Calculate nisab (85 grams gold * price)
  const goldPricePerGram = 950000;          // => IDR 950,000
  const nisabGrams = 85;                    // => 85 grams
  const nisabValue = nisabGrams * goldPricePerGram; // => 80,750,000 IDR

  // => Return JSON response
  return NextResponse.json({
    // => NextResponse.json() creates JSON response
    nisabGrams,
    goldPricePerGram,
    nisabValue,
    zakatRate: 0.025,                       // => 2.5%
  });
  // => Response: {"nisabGrams":85,"goldPricePerGram":950000,...}
}

// app/page.tsx
// => Client Component consuming API
'use client';

import { useState, useEffect } from 'react';

export default function HomePage() {
  const [data, setData] = useState<any>(null);

  useEffect(() => {
    // => Fetch from API route
    fetch('/api/zakat')
      // => GET /api/zakat triggers GET handler
      .then(res => res.json())
      .then(setData);
  }, []);

  if (!data) return <p>Loading...</p>;

  return (
    <div>
      <h1>Nisab Information</h1>
      <p>Nisab: {data.nisabGrams} grams</p>
      <p>Value: IDR {data.nisabValue.toLocaleString()}</p>
    </div>
  );
}
```

**Key Takeaway**: Create route.ts files with exported GET/POST/etc. functions to handle API requests. Use NextResponse.json() for JSON responses.

**Expected Output**: GET /api/zakat returns JSON with nisab information. Client component fetches and displays data.

**Common Pitfalls**: Not using NextResponse (manual Response construction error-prone), or creating .tsx file instead of .ts (TypeScript file required).

### Example 22: POST Route Handler with Request Body

POST handlers receive Request object with body, headers, and URL. Extract JSON data from request body.

```typescript
// app/api/donations/route.ts
import { NextResponse } from "next/server";
import { NextRequest } from "next/server";
// => Import NextRequest for typed request

// => POST handler: responds to POST /api/donations
export async function POST(request: NextRequest) {
  // => request parameter contains body, headers, etc.

  // => Parse JSON body
  const body = await request.json(); // => body is { name: "Ahmad", amount: 100000 }

  // => Extract data
  const { name, amount } = body; // => name is "Ahmad", amount is 100000

  // => Validation
  if (!name || !amount) {
    return NextResponse.json(
      { error: "Name and amount required" },
      { status: 400 }, // => HTTP 400 Bad Request
    );
  }

  // => Process donation (save to database)
  // await db.donations.create({ name, amount });
  console.log(`Donation from ${name}: IDR ${amount}`);

  // => Return success response
  return NextResponse.json(
    {
      success: true,
      message: `Thank you ${name}!`,
      donationId: Math.random().toString(36).substr(2, 9), // => Random ID
    },
    {
      status: 201, // => HTTP 201 Created
    },
  );
}
```

**Key Takeaway**: POST handlers receive NextRequest with body. Use request.json() to parse JSON. Return appropriate HTTP status codes.

**Expected Output**: POST /api/donations with JSON body creates donation and returns success response with 201 status.

**Common Pitfalls**: Forgetting to await request.json() (promise not resolved), or not validating input (security risk).

## Group 10: Middleware

### Example 23: Basic Middleware for Logging

Middleware runs before every request. Use it for authentication, redirects, logging, or request modification.

```typescript
// middleware.ts
// => File at root of project (same level as app/)
// => Special filename: Next.js recognizes it as middleware

import { NextResponse } from "next/server";
import type { NextRequest } from "next/server";

// => Middleware function runs on every request
export function middleware(request: NextRequest) {
  // => request.nextUrl contains URL information
  const { pathname } = request.nextUrl; // => pathname is "/products/murabaha"

  // => Log all requests
  console.log(`[${new Date().toISOString()}] ${request.method} ${pathname}`);
  // => Server output: "[2026-01-29T12:00:00.000Z] GET /products/murabaha"

  // => Add custom header to request
  const response = NextResponse.next(); // => Continue to requested page
  response.headers.set("x-pathname", pathname);
  // => Custom header accessible in page components

  return response;
  // => Return response to continue request
}

// => Optional: configure which routes middleware runs on
export const config = {
  // => Matcher defines path patterns for middleware
  matcher: [
    // => Run on all routes except static files and API
    "/((?!api|_next/static|_next/image|favicon.ico).*)",
  ],
};
```

**Key Takeaway**: Create middleware.ts at project root to run code before every request. Use for logging, headers, or request modification.

**Expected Output**: Every request logs to server console. Custom header added to responses (visible in browser dev tools).

**Common Pitfalls**: Forgetting to return NextResponse (request hangs), or running expensive operations (middleware should be fast).

### Example 24: Middleware for Authentication Redirect

Use middleware to protect routes by checking authentication and redirecting unauthenticated users.

```typescript
// middleware.ts
import { NextResponse } from "next/server";
import type { NextRequest } from "next/server";

export function middleware(request: NextRequest) {
  // => Check for auth cookie
  const authToken = request.cookies.get("auth_token");
  // => authToken is Cookie object or undefined

  const { pathname } = request.nextUrl;

  // => Protected routes require authentication
  const isProtectedRoute = pathname.startsWith("/dashboard");

  if (isProtectedRoute && !authToken) {
    // => User not authenticated, redirect to login
    const loginUrl = new URL("/login", request.url);
    // => loginUrl is absolute URL to /login

    // => Add redirect parameter to return after login
    loginUrl.searchParams.set("redirect", pathname);
    // => loginUrl is "/login?redirect=/dashboard"

    return NextResponse.redirect(loginUrl);
    // => HTTP 307 redirect to login page
  }

  // => Authenticated or public route, continue
  return NextResponse.next();
}

export const config = {
  // => Only run on dashboard routes
  matcher: "/dashboard/:path*", // => Matches /dashboard and /dashboard/*
};
```

**Key Takeaway**: Use middleware for authentication checks and redirects. Check cookies/headers and redirect unauthenticated users to login.

**Expected Output**: Accessing /dashboard without auth cookie redirects to /login?redirect=/dashboard. Authenticated users proceed to dashboard.

**Common Pitfalls**: Infinite redirect loops (login page also protected), or not handling redirect parameter (users lose intended destination).

### Example 25: Middleware with Request Rewriting

Middleware can rewrite requests to different paths without changing browser URL. Useful for A/B testing or feature flags.

```typescript
// middleware.ts
import { NextResponse } from "next/server";
import type { NextRequest } from "next/server";

export function middleware(request: NextRequest) {
  const { pathname } = request.nextUrl;

  // => A/B testing: show different version based on cookie
  const variant = request.cookies.get("ab_variant")?.value;
  // => variant is "a", "b", or undefined

  if (pathname === "/pricing") {
    // => Check variant cookie
    if (variant === "b") {
      // => Rewrite to variant B page
      return NextResponse.rewrite(new URL("/pricing-variant-b", request.url));
      // => Browser shows /pricing, server renders /pricing-variant-b
    }

    if (!variant) {
      // => No variant cookie, assign randomly
      const response = NextResponse.next();
      const randomVariant = Math.random() > 0.5 ? "a" : "b";

      // => Set variant cookie
      response.cookies.set("ab_variant", randomVariant, {
        maxAge: 60 * 60 * 24 * 30, // => 30 days
      });

      return response;
    }
  }

  return NextResponse.next();
}

export const config = {
  matcher: "/pricing",
};
```

**Key Takeaway**: Use NextResponse.rewrite() to serve different content without changing URL. Perfect for A/B testing, feature flags, or localization.

**Expected Output**: /pricing URL shows variant A or B content based on cookie. Browser URL stays /pricing, server renders different page.

**Common Pitfalls**: Rewriting to non-existent paths (404 error), or not setting matcher (middleware runs on all requests unnecessarily).

## Summary

These 25 beginner examples cover fundamental Next.js concepts:

**Server vs Client Components**: Server Components (default, async, zero JS), Client Components ('use client', hooks, interactivity)

**Routing**: File-based routing (page.tsx), layouts (layout.tsx), navigation (Link), dynamic routes ([param])

**Server Actions**: Form handling ('use server'), validation, revalidation (revalidatePath)

**Data Fetching**: Async Server Components, parallel fetching (Promise.all), automatic deduplication

**Loading & Errors**: Loading states (loading.tsx, Suspense), error boundaries (error.tsx), 404 pages (not-found.tsx)

**Metadata & SEO**: Static metadata, dynamic metadata (generateMetadata), Open Graph tags

**Optimization**: Image optimization (next/image, fill, priority)

**API Routes**: Route Handlers (route.ts), GET/POST handlers, NextResponse

**Middleware**: Request logging, authentication redirects, URL rewriting, cookies

**Next**: [Intermediate examples](/en/learn/software-engineering/web-platform/fe-nextjs/by-example/intermediate) for production patterns, authentication, database integration, and advanced data fetching.
