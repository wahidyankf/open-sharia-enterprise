---
title: "Intermediate"
weight: 11000002
date: 2026-01-29T16:00:00+07:00
draft: false
description: "Master intermediate Next.js patterns through 25 annotated examples covering advanced Server Actions, caching, forms, authentication, client-side data fetching, and pagination"
tags:
  ["nextjs", "react", "typescript", "server-actions", "tutorial", "by-example", "intermediate", "swr", "tanstack-query"]
---

This intermediate tutorial covers production-ready Next.js patterns through 25 heavily annotated examples. Each example maintains 1-2.25 comment lines per code line to ensure deep understanding.

## Prerequisites

Before starting, ensure you understand:

- Beginner Next.js concepts (Server/Client Components, routing, Server Actions)
- React hooks (useState, useEffect, useTransition)
- TypeScript advanced types (generics, union types, type guards)
- HTTP concepts (status codes, headers, cookies)

## Group 1: Advanced Server Actions

### Example 26: Server Action with useFormState Hook

useFormState hook provides Server Action state and pending status in Client Components. Perfect for showing validation errors and loading states.

```typescript
// app/actions.ts
'use server';
// => All exports are Server Actions

// => State type for form results
type FormState = {
  message: string;
  errors?: {
    name?: string;
    amount?: string;
  };
};

export async function submitDonation(
  prevState: FormState,                     // => Previous form state
  formData: FormData                        // => Form data from submission
): Promise<FormState> {
  // => Server Action with state parameter for useFormState

  const name = formData.get('name') as string;         // => name is "Ahmad"
  const amountStr = formData.get('amount') as string;  // => amountStr is "100000"

  // => Validation errors object
  const errors: FormState['errors'] = {};

  if (!name || name.length < 2) {
    errors.name = 'Name must be at least 2 characters';
  }

  const amount = parseInt(amountStr);
  if (isNaN(amount) || amount < 10000) {
    errors.amount = 'Minimum donation is IDR 10,000';
  }

  if (Object.keys(errors).length > 0) {
    // => Validation failed
    return {
      message: 'Validation failed',
      errors,
    };
  }

  // => Save to database
  await new Promise(resolve => setTimeout(resolve, 1000)); // => Simulate delay
  console.log(`Saved donation: ${name} - IDR ${amount}`);

  return {
    message: `Thank you ${name}! Donation of IDR ${amount.toLocaleString()} received.`,
  };
}

// app/donate/page.tsx
'use client';
// => Client Component for useFormState

import { useFormState } from 'react-dom';
// => Import useFormState hook
import { submitDonation } from '../actions';

export default function DonatePage() {
  // => useFormState hook manages Server Action state
  const [state, formAction] = useFormState(submitDonation, {
    message: '',                            // => Initial state
  });
  // => state updates after each submission
  // => formAction is wrapped Server Action

  return (
    <div>
      <h1>Make a Donation</h1>

      <form action={formAction}>
        {/* => Use formAction instead of Server Action directly */}

        <div>
          <label>
            Name:
            <input type="text" name="name" />
          </label>
          {/* => Show field-specific error */}
          {state.errors?.name && (
            <p style={{ color: 'red' }}>{state.errors.name}</p>
          )}
        </div>

        <div>
          <label>
            Amount (IDR):
            <input type="number" name="amount" />
          </label>
          {state.errors?.amount && (
            <p style={{ color: 'red' }}>{state.errors.amount}</p>
          )}
        </div>

        <button type="submit">Donate</button>

        {/* => Show success/error message */}
        {state.message && (
          <p>{state.message}</p>
        )}
      </form>
    </div>
  );
}
```

**Key Takeaway**: Use useFormState hook to manage Server Action state in Client Components. Perfect for validation errors, success messages, and form state persistence.

**Expected Output**: Form shows field-specific validation errors after submission. Success message displays on valid submission. State persists between submissions.

**Common Pitfalls**: Forgetting prevState parameter in Server Action (useFormState requires it), or not typing FormState properly (lose type safety).

### Example 27: Server Action with useFormStatus Hook

useFormStatus hook provides form submission status (pending, data, method). Use in form children to show loading states during submission.

```typescript
// app/actions.ts
'use server';

export async function createPost(formData: FormData) {
  // => Server Action with artificial delay
  const title = formData.get('title') as string;      // => title is "Zakat Guide"
  const content = formData.get('content') as string;  // => content is "..."

  // => Simulate slow database save
  await new Promise(resolve => setTimeout(resolve, 2000)); // => 2 second delay

  console.log(`Created post: ${title}`);
  return { success: true };
}

// app/components/SubmitButton.tsx
'use client';

import { useFormStatus } from 'react-dom';
// => Import useFormStatus hook

export function SubmitButton() {
  // => useFormStatus hook provides form submission status
  const { pending } = useFormStatus();      // => pending is true during submission
  // => Hook MUST be used in component that is child of form

  return (
    <button type="submit" disabled={pending}>
      {/* => Disable button during submission */}
      {pending ? 'Creating Post...' : 'Create Post'}
      {/* => Show loading text when pending */}
    </button>
  );
  // => Component re-renders when pending state changes
}

// app/posts/new/page.tsx
'use client';

import { createPost } from '@/app/actions';
import { SubmitButton } from '@/app/components/SubmitButton';

export default function NewPostPage() {
  return (
    <div>
      <h1>Create New Post</h1>

      <form action={createPost}>
        <label>
          Title:
          <input type="text" name="title" required />
        </label>

        <label>
          Content:
          <textarea name="content" required />
        </label>

        {/* => SubmitButton is child of form (required for useFormStatus) */}
        <SubmitButton />
      </form>
    </div>
  );
}
```

**Key Takeaway**: Use useFormStatus hook in form children to access submission status. Perfect for submit button loading states and disabling during submission.

**Expected Output**: Submit button shows "Creating Post..." and disables during 2-second submission. Re-enables after completion.

**Common Pitfalls**: Using useFormStatus in same component as form (must be in child component), or not disabling inputs during submission (user can modify data).

### Example 28: Progressive Enhancement with Server Actions

Server Actions work without JavaScript through native form submission. Add progressive enhancement with Client Component wrappers.

```typescript
// app/actions.ts
'use server';

import { redirect } from 'next/navigation';
// => Import redirect for server-side navigation

export async function loginUser(formData: FormData) {
  // => Server Action that works without JavaScript
  const email = formData.get('email') as string;         // => email is "user@example.com"
  const password = formData.get('password') as string;   // => password is "password123"

  // => Validate credentials (simplified)
  if (email === 'user@example.com' && password === 'password123') {
    // => Set auth cookie (simplified)
    // cookies().set('auth_token', 'token123');

    // => Redirect on success (works without JavaScript)
    redirect('/dashboard');
    // => Server-side redirect, browser navigates to /dashboard
  }

  // => Invalid credentials
  return { error: 'Invalid email or password' };
}

// app/login/page.tsx
// => Server Component (works without JavaScript)
import { loginUser } from '../actions';

export default function LoginPage() {
  return (
    <div>
      <h1>Login</h1>

      {/* => Plain form, works without JavaScript */}
      <form action={loginUser}>
        <label>
          Email:
          <input type="email" name="email" required />
        </label>

        <label>
          Password:
          <input type="password" name="password" required />
        </label>

        <button type="submit">Login</button>
        {/* => Form submits to server even if JavaScript disabled */}
      </form>
    </div>
  );
}

// app/login-enhanced/page.tsx
'use client';
// => Client Component with progressive enhancement

import { useFormState, useFormStatus } from 'react-dom';
import { loginUser } from '../actions';

function SubmitButton() {
  const { pending } = useFormStatus();
  return (
    <button type="submit" disabled={pending}>
      {pending ? 'Logging in...' : 'Login'}
    </button>
  );
}

export default function LoginEnhancedPage() {
  const [state, formAction] = useFormState(loginUser, {});

  return (
    <div>
      <h1>Login (Enhanced)</h1>

      {/* => Enhanced with client-side state management */}
      <form action={formAction}>
        <label>
          Email:
          <input type="email" name="email" required />
        </label>

        <label>
          Password:
          <input type="password" name="password" required />
        </label>

        <SubmitButton />

        {/* => Show error with JavaScript enabled */}
        {state.error && (
          <p style={{ color: 'red' }}>{state.error}</p>
        )}
      </form>
    </div>
  );
  // => Works without JavaScript (basic form)
  // => Enhanced with JavaScript (loading states, inline errors)
}
```

**Key Takeaway**: Server Actions provide progressive enhancement. Base functionality works without JavaScript, enhanced features activate when JavaScript available.

**Expected Output**: Form works with JavaScript disabled (server-side submission and redirect). With JavaScript, shows loading states and inline errors.

**Common Pitfalls**: Relying on client-side features for core functionality (breaks without JavaScript), or not testing with JavaScript disabled.

## Group 2: Cache Revalidation Strategies

### Example 29: Time-Based Revalidation (ISR)

Use revalidate option to set cache lifetime. Next.js regenerates page after expiration, serving stale content while revalidating.

```typescript
// app/posts/page.tsx
// => Server Component with time-based revalidation

async function getPosts() {
  // => Fetch with revalidate option
  const res = await fetch('https://jsonplaceholder.typicode.com/posts', {
    next: { revalidate: 60 }                // => Revalidate every 60 seconds
  });
  // => First request: fetches from API, caches for 60 seconds
  // => Within 60s: serves from cache (instant)
  // => After 60s: serves stale cache, fetches fresh data in background

  return res.json();
}

export default async function PostsPage() {
  const posts = await getPosts();           // => posts is Post[] array

  return (
    <div>
      <h2>Blog Posts (ISR)</h2>
      {/* => Updated every 60 seconds maximum */}

      <p>Last rendered: {new Date().toLocaleTimeString()}</p>
      {/* => Shows when page was generated */}

      <ul>
        {posts.slice(0, 5).map((post: any) => (
          <li key={post.id}>
            <strong>{post.title}</strong>
          </li>
        ))}
      </ul>
    </div>
  );
}

// Alternative: page-level revalidate export
export const revalidate = 60;               // => Revalidate entire page every 60s
// => All fetch requests in page inherit this revalidate value
```

**Key Takeaway**: Use revalidate option for time-based cache invalidation (ISR). Page serves cached version, regenerates in background after expiration.

**Expected Output**: First visit generates page. Subsequent visits within 60s show cached version (same timestamp). After 60s, next visit triggers background revalidation.

**Common Pitfalls**: Setting revalidate too low (increases server load), or too high (users see stale data for long periods).

### Example 30: On-Demand Revalidation with revalidatePath

Use revalidatePath() to invalidate specific route cache immediately. Perfect for content updates that should be visible instantly.

```typescript
// app/actions.ts
'use server';

import { revalidatePath } from 'next/cache';
// => Import revalidatePath function

export async function updatePost(postId: string, formData: FormData) {
  // => Server Action that updates post
  const title = formData.get('title') as string;      // => title is "Updated Title"
  const content = formData.get('content') as string;  // => content is "Updated content..."

  // => Update in database
  // await db.posts.update({ where: { id: postId }, data: { title, content } });
  console.log(`Updated post ${postId}: ${title}`);

  // => Revalidate specific post page
  revalidatePath(`/posts/${postId}`);
  // => Invalidates cache for /posts/[postId]
  // => Next request will fetch fresh data

  // => Also revalidate posts list
  revalidatePath('/posts');
  // => Invalidates /posts page cache
  // => Shows updated post in list
}

// app/posts/[id]/edit/page.tsx
'use client';

import { updatePost } from '@/app/actions';

export default function EditPostPage({
  params,
}: {
  params: { id: string };
}) {
  // => Bind postId to Server Action
  const updatePostWithId = updatePost.bind(null, params.id);
  // => updatePostWithId now has postId pre-filled

  return (
    <div>
      <h1>Edit Post {params.id}</h1>

      <form action={updatePostWithId}>
        <label>
          Title:
          <input type="text" name="title" required />
        </label>

        <label>
          Content:
          <textarea name="content" required />
        </label>

        <button type="submit">Update Post</button>
      </form>
    </div>
  );
}
```

**Key Takeaway**: Use revalidatePath() in Server Actions to invalidate specific route cache on-demand. Ensures users see fresh data immediately after mutations.

**Expected Output**: After updating post, navigating to /posts/[id] shows updated content immediately (cache invalidated). List also refreshed.

**Common Pitfalls**: Forgetting to revalidate related pages (post page updated but list still shows old data), or revalidating too broadly (invalidates unrelated caches).

### Example 31: Tag-Based Revalidation with revalidateTag

Use fetch cache tags and revalidateTag() to invalidate multiple related requests at once. More efficient than path-based revalidation.

```typescript
// app/lib/data.ts
// => Data fetching functions with cache tags

export async function getUser(userId: string) {
  // => Fetch with cache tag
  const res = await fetch(`https://api.example.com/users/${userId}`, {
    next: {
      tags: [`user-${userId}`],             // => Cache tag for this user
    },
  });
  return res.json();
}

export async function getUserPosts(userId: string) {
  const res = await fetch(`https://api.example.com/users/${userId}/posts`, {
    next: {
      tags: [`user-${userId}-posts`, `posts`], // => Multiple tags
      // => Can revalidate by user or by all posts
    },
  });
  return res.json();
}

// app/actions.ts
'use server';

import { revalidateTag } from 'next/cache';
// => Import revalidateTag function

export async function updateUserProfile(userId: string, formData: FormData) {
  const name = formData.get('name') as string;        // => name is "Ahmad Updated"

  // => Update user in database
  // await db.users.update({ where: { id: userId }, data: { name } });

  // => Revalidate all requests tagged with this user
  revalidateTag(`user-${userId}`);
  // => Invalidates getUser(userId) cache
  // => Also invalidates getUserPosts (has user tag)

  console.log(`Revalidated cache for user ${userId}`);
}

export async function createPost(userId: string, formData: FormData) {
  const title = formData.get('title') as string;
  const content = formData.get('content') as string;

  // => Create post
  // await db.posts.create({ data: { userId, title, content } });

  // => Revalidate posts tag (affects all post lists)
  revalidateTag('posts');
  // => Invalidates all fetches tagged with 'posts'

  // => Also revalidate this user's posts
  revalidateTag(`user-${userId}-posts`);
  // => More granular revalidation
}

// app/users/[id]/page.tsx
import { getUser, getUserPosts } from '@/app/lib/data';

export default async function UserPage({
  params,
}: {
  params: { id: string };
}) {
  // => Both fetches use cache tags
  const [user, posts] = await Promise.all([
    getUser(params.id),                     // => Tagged: user-${id}
    getUserPosts(params.id),                // => Tagged: user-${id}-posts, posts
  ]);

  return (
    <div>
      <h1>{user.name}</h1>
      <p>Posts by {user.name}:</p>
      <ul>
        {posts.map((post: any) => (
          <li key={post.id}>{post.title}</li>
        ))}
      </ul>
    </div>
  );
}
```

**Key Takeaway**: Use cache tags and revalidateTag() to invalidate related data across multiple routes. More flexible and efficient than path-based revalidation.

**Expected Output**: Updating user profile invalidates both user data and user posts (shared tag). Creating post invalidates all post lists (posts tag).

**Common Pitfalls**: Not using consistent tag naming (typos break revalidation), or over-revalidating with broad tags (unnecessary cache invalidation).

## Group 3: Route Organization Patterns

### Example 32: Route Groups for Organization

Use (folder) syntax to organize routes without affecting URL structure. Perfect for grouping related routes or layouts.

```typescript
// app/(marketing)/layout.tsx
// => Parentheses create route group (NOT part of URL)
// => Layout applies to routes in (marketing) group
export default function MarketingLayout({
  children,
}: {
  children: React.ReactNode;
}) {
  return (
    <div>
      {/* => Marketing-specific header */}
      <header style={{ background: '#0173B2', color: 'white', padding: '1rem' }}>
        <h1>Islamic Finance Platform</h1>
        <nav>
          <a href="/">Home</a> | <a href="/about">About</a> | <a href="/pricing">Pricing</a>
        </nav>
      </header>

      <main>
        {children}
        {/* => Marketing pages render here */}
      </main>

      {/* => Marketing-specific footer */}
      <footer style={{ background: '#f5f5f5', padding: '2rem', textAlign: 'center' }}>
        <p>© 2026 Islamic Finance Platform</p>
      </footer>
    </div>
  );
}

// app/(marketing)/page.tsx
// => Route: "/" (group name NOT in URL)
export default function HomePage() {
  return (
    <div>
      <h1>Welcome to Islamic Finance</h1>
      <p>Learn Sharia-compliant financial products.</p>
    </div>
  );
}

// app/(marketing)/about/page.tsx
// => Route: "/about" (NOT "/marketing/about")
export default function AboutPage() {
  return (
    <div>
      <h1>About Us</h1>
      <p>Providing Islamic financial education since 2026.</p>
    </div>
  );
}

// app/(app)/layout.tsx
// => Separate route group for application routes
export default function AppLayout({
  children,
}: {
  children: React.ReactNode;
}) {
  return (
    <div>
      {/* => App-specific header (different from marketing) */}
      <header style={{ background: '#029E73', color: 'white', padding: '1rem' }}>
        <nav>
          <a href="/dashboard">Dashboard</a> | <a href="/profile">Profile</a>
        </nav>
      </header>

      {children}
    </div>
  );
}

// app/(app)/dashboard/page.tsx
// => Route: "/dashboard" (NOT "/app/dashboard")
export default function DashboardPage() {
  return (
    <div>
      <h1>Dashboard</h1>
      <p>Your donation history and stats.</p>
    </div>
  );
}
```

**Key Takeaway**: Use (folder) route groups to organize routes without affecting URLs. Apply different layouts to different groups of routes.

**Expected Output**: "/" and "/about" use marketing layout (blue header). "/dashboard" uses app layout (green header). Route group names not visible in URLs.

**Common Pitfalls**: Forgetting parentheses (creates /marketing path), or nesting route groups unnecessarily (keep structure flat).

### Example 33: Parallel Routes with @folder Convention

Use @folder syntax to render multiple pages in the same layout simultaneously. Perfect for dashboards with multiple sections.

```typescript
// app/dashboard/layout.tsx
// => Layout receiving parallel routes as props
export default function DashboardLayout({
  children,
  analytics,                                // => @analytics parallel route
  notifications,                            // => @notifications parallel route
}: {
  children: React.ReactNode;
  analytics: React.ReactNode;               // => Parallel route slot
  notifications: React.ReactNode;           // => Parallel route slot
}) {
  return (
    <div style={{ display: 'grid', gridTemplateColumns: '1fr 1fr', gap: '1rem' }}>
      {/* => Main content */}
      <div style={{ gridColumn: '1 / -1' }}>
        {children}
        {/* => app/dashboard/page.tsx content */}
      </div>

      {/* => Analytics section */}
      <div>
        <h2>Analytics</h2>
        {analytics}
        {/* => app/dashboard/@analytics/page.tsx content */}
      </div>

      {/* => Notifications section */}
      <div>
        <h2>Notifications</h2>
        {notifications}
        {/* => app/dashboard/@notifications/page.tsx content */}
      </div>
    </div>
  );
}

// app/dashboard/page.tsx
// => Main dashboard page
export default function DashboardPage() {
  return (
    <div>
      <h1>Dashboard Overview</h1>
      <p>Welcome to your dashboard.</p>
    </div>
  );
}

// app/dashboard/@analytics/page.tsx
// => Analytics parallel route (@ makes it parallel)
export default function AnalyticsPage() {
  return (
    <div>
      <p>Total Donations: IDR 1,500,000</p>
      <p>This Month: IDR 350,000</p>
    </div>
  );
}

// app/dashboard/@notifications/page.tsx
// => Notifications parallel route
export default function NotificationsPage() {
  return (
    <div>
      <p>✓ Donation processed</p>
      <p>✓ Profile updated</p>
    </div>
  );
}
```

**Key Takeaway**: Use @folder parallel routes to render multiple pages simultaneously in layout. Each parallel route can have independent loading/error states.

**Expected Output**: /dashboard shows three sections: main content, analytics, and notifications. All rendered in single layout with different data sources.

**Common Pitfalls**: Forgetting @ symbol (creates regular nested route), or not handling default.tsx fallback (shows error when route doesn't exist).

### Example 34: Intercepting Routes for Modals

Use (.)folder and (..)folder syntax to intercept routes and show as modals while preserving URL for direct access.

```typescript
// app/posts/page.tsx
// => Posts list page
import Link from 'next/link';

export default function PostsPage() {
  const posts = [
    { id: '1', title: 'Zakat Guide' },
    { id: '2', title: 'Murabaha Basics' },
  ];

  return (
    <div>
      <h1>Posts</h1>
      <ul>
        {posts.map(post => (
          <li key={post.id}>
            <Link href={`/posts/${post.id}`}>
              {/* => Link to post detail */}
              {post.title}
            </Link>
          </li>
        ))}
      </ul>
    </div>
  );
}

// app/posts/[id]/page.tsx
// => Full post detail page (direct access)
export default function PostDetailPage({
  params,
}: {
  params: { id: string };
}) {
  return (
    <div>
      <h1>Post {params.id} - Full Page</h1>
      <p>This is the full post page (direct access or refresh).</p>
      <a href="/posts">Back to Posts</a>
    </div>
  );
}

// app/posts/(.)posts/[id]/page.tsx
// => Intercepted route (.) means same level
// => Shows as modal when navigating from /posts
'use client';

import { useRouter } from 'next/navigation';

export default function PostModal({
  params,
}: {
  params: { id: string };
}) {
  const router = useRouter();

  return (
    <div
      style={{
        position: 'fixed',
        top: 0,
        left: 0,
        right: 0,
        bottom: 0,
        background: 'rgba(0,0,0,0.5)',
        display: 'flex',
        alignItems: 'center',
        justifyContent: 'center',
      }}
      onClick={() => router.back()}
      // => Click backdrop closes modal
    >
      <div
        style={{
          background: 'white',
          padding: '2rem',
          borderRadius: '8px',
          maxWidth: '500px',
        }}
        onClick={e => e.stopPropagation()}
        // => Prevent closing when clicking modal content
      >
        <h1>Post {params.id} - Modal</h1>
        <p>This is the modal view (soft navigation).</p>
        <button onClick={() => router.back()}>Close</button>
      </div>
    </div>
  );
}
```

**Key Takeaway**: Use (.) intercepting routes to show content as modal on soft navigation while preserving full page for direct access/refresh.

**Expected Output**: Clicking post link from /posts shows modal overlay. Refreshing /posts/1 shows full page. Back button closes modal.

**Common Pitfalls**: Wrong interception syntax (. for same level, .. for parent level), or not handling direct access (only modal, no full page).

## Group 4: Advanced Forms & Validation

### Example 35: Form Validation with Zod Schema

Use Zod for runtime validation in Server Actions. Provides type-safe validation with detailed error messages.

```typescript
// app/lib/schemas.ts
import { z } from "zod";
// => Import Zod schema builder

// => Zod schema for donation form
export const donationSchema = z.object({
  name: z.string().min(2, "Name must be at least 2 characters").max(50, "Name must be less than 50 characters"),
  // => String validation with length constraints

  email: z.string().email("Invalid email address"),
  // => Email format validation

  amount: z
    .number()
    .min(10000, "Minimum donation is IDR 10,000")
    .max(1000000000, "Maximum donation is IDR 1,000,000,000"),
  // => Number validation with range

  category: z.enum(["zakat", "sadaqah", "infaq"], {
    errorMap: () => ({ message: "Invalid category" }),
  }),
  // => Enum validation (must be one of these values)
});

// => TypeScript type inferred from schema
export type DonationInput = z.infer<typeof donationSchema>;
// => DonationInput is { name: string; email: string; amount: number; category: "zakat" | "sadaqah" | "infaq" }

// app/actions.ts
("use server");

import { donationSchema } from "./lib/schemas";

export async function submitDonation(formData: FormData) {
  // => Extract form data
  const rawData = {
    name: formData.get("name"),
    email: formData.get("email"),
    amount: parseFloat(formData.get("amount") as string),
    category: formData.get("category"),
  };

  // => Validate with Zod schema
  const result = donationSchema.safeParse(rawData);
  // => safeParse returns { success: boolean, data?: T, error?: ZodError }

  if (!result.success) {
    // => Validation failed
    const errors = result.error.flatten().fieldErrors;
    // => errors is { name?: string[], email?: string[], ... }

    return {
      success: false,
      errors: {
        // => Convert array errors to single messages
        name: errors.name?.[0],
        email: errors.email?.[0],
        amount: errors.amount?.[0],
        category: errors.category?.[0],
      },
    };
  }

  // => Validation passed, data is type-safe
  const { name, email, amount, category } = result.data;
  // => result.data is DonationInput type

  // => Save to database
  console.log(`Donation: ${name} (${email}) - IDR ${amount} - ${category}`);

  return {
    success: true,
    message: `Thank you ${name}! Your ${category} donation of IDR ${amount.toLocaleString()} has been received.`,
  };
}
```

**Key Takeaway**: Use Zod schemas for type-safe validation in Server Actions. Provides runtime validation, TypeScript types, and detailed error messages.

**Expected Output**: Form submission validates all fields. Invalid data returns field-specific errors. Valid data processes successfully with type safety.

**Common Pitfalls**: Not handling safeParse errors properly (check result.success), or mixing up parse() and safeParse() (parse throws, safeParse returns result).

### Example 36: Optimistic Updates with useOptimistic

Use useOptimistic hook to show immediate UI feedback while Server Action processes. Reverts on error.

```typescript
// app/posts/[id]/page.tsx
'use client';

import { useOptimistic, useTransition } from 'react';
// => Import useOptimistic and useTransition hooks

type Comment = {
  id: string;
  author: string;
  text: string;
  createdAt: Date;
};

export default function PostPage({
  initialComments,
}: {
  initialComments: Comment[];
}) {
  // => useOptimistic manages optimistic state
  const [optimisticComments, addOptimisticComment] = useOptimistic(
    initialComments,                        // => Initial state
    (state, newComment: Comment) => [...state, newComment]
    // => Reducer: how to add optimistic comment
  );

  const [isPending, startTransition] = useTransition();

  async function handleSubmit(formData: FormData) {
    const author = formData.get('author') as string;
    const text = formData.get('text') as string;

    // => Create optimistic comment (shown immediately)
    const optimisticComment: Comment = {
      id: crypto.randomUUID(),
      author,
      text,
      createdAt: new Date(),
    };

    // => Add optimistic comment to UI (instant feedback)
    startTransition(() => {
      addOptimisticComment(optimisticComment);
      // => Comment appears in list immediately
    });

    // => Submit to server (async, might fail)
    try {
      // await createComment(formData);
      // => If server succeeds, page revalidates and shows real comment
      await new Promise(resolve => setTimeout(resolve, 2000));
      console.log('Comment created');
    } catch (error) {
      // => If server fails, optimistic update reverts
      console.error('Failed to create comment');
      // => Optimistic comment disappears from list
    }
  }

  return (
    <div>
      <h1>Post with Comments</h1>

      <div>
        <h2>Comments</h2>
        <ul>
          {optimisticComments.map(comment => (
            <li key={comment.id} style={{ opacity: isPending ? 0.5 : 1 }}>
              {/* => Optimistic comments shown with reduced opacity */}
              <strong>{comment.author}</strong>: {comment.text}
              <small> - {comment.createdAt.toLocaleTimeString()}</small>
            </li>
          ))}
        </ul>
      </div>

      <form action={handleSubmit}>
        <input type="text" name="author" placeholder="Your name" required />
        <textarea name="text" placeholder="Your comment" required />
        <button type="submit" disabled={isPending}>
          {isPending ? 'Posting...' : 'Post Comment'}
        </button>
      </form>
    </div>
  );
}
```

**Key Takeaway**: Use useOptimistic for immediate UI feedback during Server Actions. Shows optimistic state instantly, reverts on error, replaces with real data on success.

**Expected Output**: Submitting comment shows it immediately in list (optimistic). After 2 seconds, comment persists (server success). If error, comment disappears.

**Common Pitfalls**: Not wrapping in startTransition (optimistic update won't work), or forgetting to revalidate on success (shows both optimistic and real comment).

## Group 5: Authentication Patterns

### Example 37: Cookies-Based Authentication

Use Next.js cookies() to manage authentication tokens. Accessible in Server Components and Route Handlers.

```typescript
// app/lib/auth.ts
import { cookies } from 'next/headers';
// => Import cookies function

export async function getCurrentUser() {
  // => Server-only function (can't run on client)
  const cookieStore = cookies();            // => Access cookies
  const authToken = cookieStore.get('auth_token');
  // => authToken is { name: 'auth_token', value: 'token123' } or undefined

  if (!authToken) {
    return null;                            // => Not authenticated
  }

  // => Verify token and get user
  // const user = await db.users.findByToken(authToken.value);
  // => Simplified: return mock user
  return {
    id: '1',
    name: 'Ahmad',
    email: 'ahmad@example.com',
  };
}

// app/actions.ts
'use server';

import { cookies } from 'next/headers';

export async function login(formData: FormData) {
  const email = formData.get('email') as string;
  const password = formData.get('password') as string;

  // => Verify credentials
  if (email === 'user@example.com' && password === 'password123') {
    // => Set auth cookie (simplified)
    cookies().set('auth_token', 'token123', {
      httpOnly: true,                       // => Not accessible via JavaScript (security)
      secure: process.env.NODE_ENV === 'production', // => HTTPS only in production
      sameSite: 'lax',                      // => CSRF protection
      maxAge: 60 * 60 * 24 * 7,            // => 7 days
    });

    return { success: true };
  }

  return { success: false, error: 'Invalid credentials' };
}

export async function logout() {
  // => Delete auth cookie
  cookies().delete('auth_token');
  // => User logged out
}

// app/dashboard/page.tsx
// => Protected page using getCurrentUser
import { getCurrentUser } from '@/app/lib/auth';
import { redirect } from 'next/navigation';

export default async function DashboardPage() {
  const user = await getCurrentUser();      // => Check authentication

  if (!user) {
    // => Not authenticated, redirect to login
    redirect('/login');
    // => Server-side redirect
  }

  return (
    <div>
      <h1>Welcome, {user.name}!</h1>
      <p>Email: {user.email}</p>
      {/* => Protected content only visible to authenticated users */}
    </div>
  );
}
```

**Key Takeaway**: Use cookies() function for authentication in Server Components and Server Actions. Set httpOnly and secure flags for security.

**Expected Output**: Login sets httpOnly cookie. Dashboard checks cookie and shows user data or redirects to login. Logout deletes cookie.

**Common Pitfalls**: Not setting httpOnly (XSS vulnerability), or forgetting secure flag in production (transmits over HTTP).

### Example 38: Middleware-Based Authentication

Use middleware to protect multiple routes at once. More efficient than checking authentication in every page.

```typescript
// middleware.ts
import { NextResponse } from 'next/server';
import type { NextRequest } from 'next/server';

// => Protected route patterns
const protectedRoutes = ['/dashboard', '/profile', '/settings'];

export function middleware(request: NextRequest) {
  const { pathname } = request.nextUrl;

  // => Check if current route is protected
  const isProtected = protectedRoutes.some(route =>
    pathname.startsWith(route)
  );

  if (isProtected) {
    // => Check for auth token
    const authToken = request.cookies.get('auth_token');

    if (!authToken) {
      // => Not authenticated, redirect to login
      const loginUrl = new URL('/login', request.url);
      loginUrl.searchParams.set('redirect', pathname);
      // => Save intended destination for post-login redirect

      return NextResponse.redirect(loginUrl);
    }

    // => TODO: Verify token validity
    // const isValid = await verifyToken(authToken.value);
    // if (!isValid) { return NextResponse.redirect(...) }
  }

  // => Authenticated or public route
  return NextResponse.next();
}

export const config = {
  // => Run middleware on protected routes only
  matcher: ['/dashboard/:path*', '/profile/:path*', '/settings/:path*'],
};

// app/login/page.tsx
'use client';

import { useSearchParams, useRouter } from 'next/navigation';
import { login } from '../actions';

export default function LoginPage() {
  const searchParams = useSearchParams();
  const router = useRouter();
  const redirect = searchParams.get('redirect') || '/dashboard';
  // => Get redirect parameter from URL

  async function handleSubmit(formData: FormData) {
    const result = await login(formData);

    if (result.success) {
      // => Login successful, redirect to intended destination
      router.push(redirect);
    }
  }

  return (
    <div>
      <h1>Login</h1>
      <p>Redirecting to: {redirect}</p>

      <form action={handleSubmit}>
        <input type="email" name="email" required />
        <input type="password" name="password" required />
        <button type="submit">Login</button>
      </form>
    </div>
  );
}
```

**Key Takeaway**: Use middleware to protect multiple routes efficiently. Checks authentication once for all protected paths, redirects with intended destination.

**Expected Output**: Accessing /dashboard without auth redirects to /login?redirect=/dashboard. After login, returns to /dashboard.

**Common Pitfalls**: Infinite redirect loop (login page also protected), or not preserving redirect parameter (users lose intended destination).

## Group 6: Database Integration Patterns

### Example 39: Prisma Integration with Server Components

Use Prisma ORM in Server Components for type-safe database queries. Zero client JavaScript, automatic TypeScript types.

```typescript
// prisma/schema.prisma
// => Prisma schema defines database structure
// model Post {
//   id        String   @id @default(cuid())
//   title     String
//   content   String
//   published Boolean  @default(false)
//   createdAt DateTime @default(now())
// }

// app/lib/prisma.ts
import { PrismaClient } from '@prisma/client';

// => Singleton pattern for Prisma client
const globalForPrisma = globalThis as unknown as {
  prisma: PrismaClient | undefined;
};

export const prisma =
  globalForPrisma.prisma ??
  new PrismaClient({
    log: ['query'],                         // => Log SQL queries in development
  });

if (process.env.NODE_ENV !== 'production') {
  globalForPrisma.prisma = prisma;
}
// => Prevents multiple Prisma instances in development (hot reload)

// app/posts/page.tsx
// => Server Component with Prisma query
import { prisma } from '@/app/lib/prisma';

export default async function PostsPage() {
  // => Type-safe Prisma query
  const posts = await prisma.post.findMany({
    // => posts is Post[] (TypeScript type from schema)
    where: { published: true },             // => Only published posts
    orderBy: { createdAt: 'desc' },         // => Newest first
    take: 10,                               // => Limit to 10 posts
    select: {
      // => Select specific fields
      id: true,
      title: true,
      createdAt: true,
    },
  });
  // => Query runs on server, results cached

  return (
    <div>
      <h1>Published Posts</h1>
      <ul>
        {posts.map(post => (
          <li key={post.id}>
            <strong>{post.title}</strong>
            <small> - {post.createdAt.toLocaleDateString()}</small>
          </li>
        ))}
      </ul>
    </div>
  );
}

// app/actions.ts
'use server';

import { prisma } from './lib/prisma';
import { revalidatePath } from 'next/cache';

export async function createPost(formData: FormData) {
  const title = formData.get('title') as string;
  const content = formData.get('content') as string;

  // => Create post with Prisma
  const post = await prisma.post.create({
    data: {
      title,
      content,
      published: true,
    },
  });
  // => post is Post object with all fields

  // => Revalidate posts page
  revalidatePath('/posts');

  return { success: true, postId: post.id };
}
```

**Key Takeaway**: Use Prisma in Server Components and Server Actions for type-safe database queries. Singleton pattern prevents multiple clients in development.

**Expected Output**: Posts page shows database posts (type-safe query). Creating post saves to database and revalidates page cache.

**Common Pitfalls**: Multiple Prisma instances in development (memory leak), or not revalidating after mutations (stale cache).

### Example 40: Error Handling for Database Queries

Wrap database queries in try-catch blocks to handle errors gracefully. Show user-friendly messages instead of crashes.

```typescript
// app/posts/[id]/page.tsx
import { prisma } from '@/app/lib/prisma';
import { notFound } from 'next/navigation';

export default async function PostPage({
  params,
}: {
  params: { id: string };
}) {
  try {
    // => Query might fail (network error, invalid ID, etc.)
    const post = await prisma.post.findUnique({
      where: { id: params.id },
    });

    if (!post) {
      // => Post not found in database
      notFound();
      // => Triggers not-found.tsx rendering
    }

    return (
      <div>
        <h1>{post.title}</h1>
        <p>{post.content}</p>
      </div>
    );
  } catch (error) {
    // => Database error (connection failed, timeout, etc.)
    console.error('Database error:', error);

    // => Throw error to trigger error.tsx
    throw new Error('Failed to load post');
  }
}

// app/actions.ts
'use server';

import { prisma } from './lib/prisma';

export async function deletePost(postId: string) {
  try {
    // => Delete operation might fail
    await prisma.post.delete({
      where: { id: postId },
    });

    return { success: true };
  } catch (error) {
    // => Handle Prisma errors
    if (error.code === 'P2025') {
      // => Prisma error code: Record not found
      return {
        success: false,
        error: 'Post not found',
      };
    }

    // => Generic error
    console.error('Failed to delete post:', error);
    return {
      success: false,
      error: 'Failed to delete post',
    };
  }
}
```

**Key Takeaway**: Wrap database queries in try-catch blocks. Use notFound() for missing resources, throw errors for server issues, return error objects from Server Actions.

**Expected Output**: Missing post shows not-found.tsx. Database errors show error.tsx. Server Actions return error messages without crashing.

**Common Pitfalls**: Not handling Prisma error codes (generic error messages), or throwing errors in Server Actions (should return error objects).

## Group 7: Client-Side Data Fetching

### Example 41: Client-Side Data Fetching with SWR

Use SWR for client-side data fetching with automatic caching, revalidation, and error handling. Perfect for user-specific or frequently updating data.

```typescript
// app/dashboard/donations/page.tsx
'use client';
// => Client Component for SWR

import useSWR from 'swr';
// => Import SWR hook

// => Fetcher function (required by SWR)
const fetcher = (url: string) => fetch(url).then(res => res.json());
// => fetcher receives URL, returns promise of JSON data

export default function DonationsPage() {
  // => useSWR hook manages data fetching
  const { data, error, isLoading, mutate } = useSWR('/api/donations', fetcher);
  // => data: fetched data (undefined while loading)
  // => error: error object if request failed
  // => isLoading: true during initial fetch
  // => mutate: function to revalidate data manually

  if (isLoading) {
    // => Show loading state
    return <p>Loading donations...</p>;
  }

  if (error) {
    // => Show error state
    return <p>Failed to load donations: {error.message}</p>;
  }

  // => Data loaded successfully
  return (
    <div>
      <h1>Your Donations</h1>

      <ul>
        {data.donations.map((donation: any) => (
          <li key={donation.id}>
            IDR {donation.amount.toLocaleString()} - {donation.date}
          </li>
        ))}
      </ul>

      <button onClick={() => mutate()}>
        {/* => Manually revalidate data */}
        Refresh
      </button>
    </div>
  );
}
// => SWR features:
// => - Automatic caching (subsequent renders instant)
// => - Revalidation on focus (window regains focus)
// => - Revalidation on reconnect (network reconnects)
// => - Interval revalidation (optional)
```

**Key Takeaway**: Use SWR for client-side data fetching with automatic caching, revalidation, and error handling. Perfect for dynamic, user-specific data.

**Expected Output**: Page loads donations from API. Data cached for instant subsequent renders. Clicking "Refresh" manually revalidates.

**Common Pitfalls**: Using SWR in Server Components (only works in Client Components), or not providing fetcher function (required parameter).

### Example 42: Client-Side Data Fetching with TanStack Query

Use TanStack Query (React Query) for advanced client-side data management with powerful caching and synchronization.

```typescript
// app/providers.tsx
'use client';

import { QueryClient, QueryClientProvider } from '@tanstack/react-query';
import { useState } from 'react';

export function Providers({ children }: { children: React.ReactNode }) {
  // => Create QueryClient instance
  const [queryClient] = useState(() => new QueryClient({
    defaultOptions: {
      queries: {
        staleTime: 60 * 1000,               // => Data fresh for 1 minute
        cacheTime: 5 * 60 * 1000,           // => Cache for 5 minutes
      },
    },
  }));

  return (
    <QueryClientProvider client={queryClient}>
      {/* => All children can use TanStack Query hooks */}
      {children}
    </QueryClientProvider>
  );
}

// app/layout.tsx
import { Providers } from './providers';

export default function RootLayout({
  children,
}: {
  children: React.ReactNode;
}) {
  return (
    <html lang="en">
      <body>
        <Providers>
          {children}
        </Providers>
      </body>
    </html>
  );
}

// app/posts/page.tsx
'use client';

import { useQuery, useMutation, useQueryClient } from '@tanstack/react-query';

export default function PostsPage() {
  const queryClient = useQueryClient();

  // => useQuery for fetching data
  const { data, isLoading, error } = useQuery({
    queryKey: ['posts'],                    // => Unique query identifier
    queryFn: async () => {
      // => Query function: fetches data
      const res = await fetch('/api/posts');
      return res.json();
    },
  });
  // => data is cached with key ['posts']

  // => useMutation for mutations
  const createPostMutation = useMutation({
    mutationFn: async (newPost: { title: string }) => {
      // => Mutation function: creates post
      const res = await fetch('/api/posts', {
        method: 'POST',
        body: JSON.stringify(newPost),
      });
      return res.json();
    },
    onSuccess: () => {
      // => Invalidate posts query after successful mutation
      queryClient.invalidateQueries({ queryKey: ['posts'] });
      // => Triggers refetch of posts data
    },
  });

  if (isLoading) return <p>Loading...</p>;
  if (error) return <p>Error: {error.message}</p>;

  return (
    <div>
      <h1>Posts</h1>
      <ul>
        {data.posts.map((post: any) => (
          <li key={post.id}>{post.title}</li>
        ))}
      </ul>

      <button
        onClick={() => createPostMutation.mutate({ title: 'New Post' })}
        disabled={createPostMutation.isPending}
      >
        {createPostMutation.isPending ? 'Creating...' : 'Create Post'}
      </button>
    </div>
  );
}
```

**Key Takeaway**: Use TanStack Query for advanced client-side data management. Provides powerful caching, automatic refetching, and mutation invalidation.

**Expected Output**: Posts load from API with caching. Creating post invalidates cache and refetches automatically. Loading states managed by library.

**Common Pitfalls**: Not wrapping app in QueryClientProvider (hooks won't work), or forgetting to invalidate queries after mutations (stale data).

## Group 8: Advanced Form Patterns

### Example 43: Form Handling with React Hook Form

Use React Hook Form for complex forms with validation, field arrays, and better performance than native form handling.

```typescript
// app/register/page.tsx
'use client';

import { useForm, SubmitHandler } from 'react-hook-form';
// => Import useForm hook and types

// => Form data type
type FormData = {
  name: string;
  email: string;
  password: string;
  confirmPassword: string;
};

export default function RegisterPage() {
  // => useForm hook manages form state
  const {
    register,                               // => Function to register inputs
    handleSubmit,                           // => Form submit handler
    formState: { errors, isSubmitting },    // => Form state
    watch,                                  // => Watch field values
  } = useForm<FormData>();

  // => Submit handler
  const onSubmit: SubmitHandler<FormData> = async (data) => {
    // => data is FormData type (type-safe)
    console.log('Form data:', data);
    // => data is { name: "Ahmad", email: "...", password: "...", ... }

    // => Simulate API call
    await new Promise(resolve => setTimeout(resolve, 2000));
    console.log('Registration successful');
  };

  // => Watch password for confirmation validation
  const password = watch('password');       // => password is current password value

  return (
    <div>
      <h1>Register</h1>

      <form onSubmit={handleSubmit(onSubmit)}>
        {/* => handleSubmit wraps onSubmit with validation */}

        <div>
          <label>Name:</label>
          <input
            type="text"
            {...register('name', {
              // => Register input with validation rules
              required: 'Name is required',
              minLength: {
                value: 2,
                message: 'Name must be at least 2 characters',
              },
            })}
          />
          {/* => Show validation error */}
          {errors.name && <p style={{ color: 'red' }}>{errors.name.message}</p>}
        </div>

        <div>
          <label>Email:</label>
          <input
            type="email"
            {...register('email', {
              required: 'Email is required',
              pattern: {
                value: /^[A-Z0-9._%+-]+@[A-Z0-9.-]+\.[A-Z]{2,}$/i,
                message: 'Invalid email address',
              },
            })}
          />
          {errors.email && <p style={{ color: 'red' }}>{errors.email.message}</p>}
        </div>

        <div>
          <label>Password:</label>
          <input
            type="password"
            {...register('password', {
              required: 'Password is required',
              minLength: {
                value: 8,
                message: 'Password must be at least 8 characters',
              },
            })}
          />
          {errors.password && <p style={{ color: 'red' }}>{errors.password.message}</p>}
        </div>

        <div>
          <label>Confirm Password:</label>
          <input
            type="password"
            {...register('confirmPassword', {
              required: 'Please confirm password',
              validate: value =>
                // => Custom validation: must match password
                value === password || 'Passwords do not match',
            })}
          />
          {errors.confirmPassword && (
            <p style={{ color: 'red' }}>{errors.confirmPassword.message}</p>
          )}
        </div>

        <button type="submit" disabled={isSubmitting}>
          {/* => Disable during submission */}
          {isSubmitting ? 'Registering...' : 'Register'}
        </button>
      </form>
    </div>
  );
}
```

**Key Takeaway**: Use React Hook Form for complex forms with validation, better performance (uncontrolled inputs), and type safety. Built-in validation rules and custom validators.

**Expected Output**: Form validates on submit. Shows field-specific errors. Submit button disables during submission. Type-safe form data.

**Common Pitfalls**: Not using {...register()} spread operator (validation won't work), or forgetting to use handleSubmit wrapper (validation bypassed).

### Example 44: Advanced Zod Validation with Transform

Use Zod transform() to convert and validate form data simultaneously. Perfect for normalizing input before processing.

```typescript
// app/lib/schemas.ts
import { z } from "zod";

// => Schema with transformations
export const productSchema = z.object({
  name: z
    .string()
    .trim() // => Remove whitespace
    .toLowerCase() // => Normalize to lowercase
    .min(3, "Product name must be at least 3 characters"),

  price: z
    .string() // => Input as string (from form)
    .transform((val) => parseFloat(val)) // => Transform to number
    .pipe(
      // => Chain with number validations
      z.number().min(0, "Price must be positive").max(1000000000, "Price too high"),
    ),

  category: z.enum(["murabaha", "ijarah", "musharakah"]).transform((val) => val.toUpperCase()), // => Transform to uppercase

  tags: z
    .string() // => Input: "tag1, tag2, tag3"
    .transform((val) =>
      // => Transform comma-separated string to array
      val
        .split(",")
        .map((tag) => tag.trim())
        .filter((tag) => tag.length > 0),
    )
    .pipe(
      // => Validate array
      z.array(z.string()).min(1, "At least one tag required"),
    ),

  availableFrom: z
    .string() // => Input: "2026-01-29"
    .transform((val) => new Date(val)) // => Transform to Date
    .pipe(z.date().min(new Date(), "Date must be in the future")),
});

// => Inferred type includes transformations
export type ProductInput = z.infer<typeof productSchema>;
// => ProductInput is {
// =>   name: string (trimmed, lowercase),
// =>   price: number (transformed from string),
// =>   category: "MURABAHA" | "IJARAH" | "MUSHARAKAH" (uppercase),
// =>   tags: string[] (transformed from comma-separated),
// =>   availableFrom: Date (transformed from string)
// => }

// app/actions.ts
("use server");

import { productSchema } from "./lib/schemas";

export async function createProduct(formData: FormData) {
  // => Parse and transform form data
  const result = productSchema.safeParse({
    name: formData.get("name"),
    price: formData.get("price"),
    category: formData.get("category"),
    tags: formData.get("tags"),
    availableFrom: formData.get("availableFrom"),
  });

  if (!result.success) {
    return {
      success: false,
      errors: result.error.flatten().fieldErrors,
    };
  }

  // => result.data is transformed and validated
  const { name, price, category, tags, availableFrom } = result.data;
  // => name is trimmed + lowercase
  // => price is number (not string)
  // => category is uppercase enum
  // => tags is array (not comma-separated string)
  // => availableFrom is Date object

  console.log("Creating product:", {
    name, // => "murabaha financing"
    price, // => 1000000 (number)
    category, // => "MURABAHA" (uppercase)
    tags, // => ["islamic", "finance"]
    availableFrom, // => Date object
  });

  return { success: true };
}
```

**Key Takeaway**: Use Zod transform() to convert and normalize data during validation. Reduces manual parsing and ensures consistent data format.

**Expected Output**: Form data automatically transformed (strings to numbers/dates, comma-separated to arrays, normalization). Type-safe transformed data.

**Common Pitfalls**: Not using pipe() after transform() (validation on wrong type), or forgetting transform order matters (operations applied sequentially).

### Example 45: File Upload Handling with Server Actions

Handle file uploads securely with Server Actions. Validate file types, sizes, and save to storage.

```typescript
// app/actions.ts
'use server';

import { writeFile } from 'fs/promises';
import path from 'path';

export async function uploadDocument(formData: FormData) {
  // => Extract file from FormData
  const file = formData.get('document') as File;
  // => file is File object (or null if not uploaded)

  if (!file) {
    return {
      success: false,
      error: 'No file uploaded',
    };
  }

  // => Validate file type
  const allowedTypes = ['application/pdf', 'image/jpeg', 'image/png'];
  if (!allowedTypes.includes(file.type)) {
    return {
      success: false,
      error: 'Invalid file type. Only PDF, JPEG, and PNG allowed.',
    };
  }

  // => Validate file size (max 5MB)
  const maxSize = 5 * 1024 * 1024;          // => 5MB in bytes
  if (file.size > maxSize) {
    return {
      success: false,
      error: 'File too large. Maximum size is 5MB.',
    };
  }

  // => Convert File to buffer
  const bytes = await file.arrayBuffer();
  const buffer = Buffer.from(bytes);
  // => buffer contains file data

  // => Generate unique filename
  const timestamp = Date.now();
  const filename = `${timestamp}-${file.name}`;
  // => filename is "1706524800000-document.pdf"

  // => Save file to public/uploads directory
  const uploadPath = path.join(process.cwd(), 'public', 'uploads', filename);
  await writeFile(uploadPath, buffer);
  // => File saved to disk

  // => Return file URL
  const fileUrl = `/uploads/${filename}`;

  return {
    success: true,
    fileUrl,
    message: `File uploaded: ${file.name}`,
  };
}

// app/upload/page.tsx
'use client';

import { useState } from 'react';
import { uploadDocument } from '../actions';

export default function UploadPage() {
  const [result, setResult] = useState<any>(null);

  async function handleSubmit(formData: FormData) {
    // => Call Server Action with FormData
    const uploadResult = await uploadDocument(formData);
    setResult(uploadResult);
  }

  return (
    <div>
      <h1>Upload Document</h1>

      <form action={handleSubmit}>
        <input
          type="file"
          name="document"
          accept=".pdf,.jpg,.jpeg,.png"
          // => HTML validation (client-side)
          required
        />

        <button type="submit">Upload</button>
      </form>

      {/* => Show upload result */}
      {result && (
        <div>
          {result.success ? (
            <>
              <p style={{ color: 'green' }}>{result.message}</p>
              <a href={result.fileUrl} target="_blank" rel="noopener noreferrer">
                View File
              </a>
            </>
          ) : (
            <p style={{ color: 'red' }}>{result.error}</p>
          )}
        </div>
      )}
    </div>
  );
}
```

**Key Takeaway**: Handle file uploads securely in Server Actions with type/size validation. Convert File to buffer, save to disk or cloud storage.

**Expected Output**: Form uploads file to server. Server validates type/size, saves to public/uploads/, returns URL for access.

**Common Pitfalls**: Not validating file types server-side (security risk), or storing files in wrong directory (not accessible publicly).

## Group 9: Pagination & Infinite Scroll

### Example 46: Pagination with Server Components

Implement pagination with Server Components and URL search params. SEO-friendly, works without JavaScript.

```typescript
// app/posts/page.tsx
// => Server Component with pagination

type PageProps = {
  searchParams: { page?: string };          // => URL search params
};

const POSTS_PER_PAGE = 10;

export default async function PostsPage({ searchParams }: PageProps) {
  // => Get current page from URL (?page=2)
  const currentPage = parseInt(searchParams.page || '1');
  // => currentPage is 2 for ?page=2, default 1

  // => Fetch total count
  const totalPosts = 50;                    // => From database: await db.posts.count()

  // => Calculate pagination
  const totalPages = Math.ceil(totalPosts / POSTS_PER_PAGE);
  // => totalPages is 5 (50 posts / 10 per page)

  const offset = (currentPage - 1) * POSTS_PER_PAGE;
  // => offset is 10 for page 2 (skip first 10 posts)

  // => Fetch posts for current page
  const posts = Array.from({ length: POSTS_PER_PAGE }, (_, i) => ({
    id: offset + i + 1,
    title: `Post ${offset + i + 1}`,
  }));
  // => posts is posts 11-20 for page 2

  return (
    <div>
      <h1>Blog Posts</h1>

      <ul>
        {posts.map(post => (
          <li key={post.id}>{post.title}</li>
        ))}
      </ul>

      {/* => Pagination controls */}
      <div style={{ display: 'flex', gap: '0.5rem', marginTop: '2rem' }}>
        {/* => Previous page link */}
        {currentPage > 1 && (
          <a href={`/posts?page=${currentPage - 1}`}>
            Previous
          </a>
        )}

        {/* => Page number links */}
        {Array.from({ length: totalPages }, (_, i) => i + 1).map(page => (
          <a
            key={page}
            href={`/posts?page=${page}`}
            style={{
              fontWeight: page === currentPage ? 'bold' : 'normal',
              textDecoration: page === currentPage ? 'underline' : 'none',
            }}
          >
            {page}
          </a>
        ))}

        {/* => Next page link */}
        {currentPage < totalPages && (
          <a href={`/posts?page=${currentPage + 1}`}>
            Next
          </a>
        )}
      </div>

      <p>
        Page {currentPage} of {totalPages}
      </p>
    </div>
  );
}
```

**Key Takeaway**: Implement pagination with Server Components using URL search params. SEO-friendly (each page has unique URL), works without JavaScript.

**Expected Output**: Posts show 10 per page. Pagination controls navigate between pages. URL updates to /posts?page=2, /posts?page=3, etc.

**Common Pitfalls**: Not validating page parameter (could be negative or exceed max), or forgetting to handle empty results (page beyond limit).

### Example 47: Infinite Scroll with Intersection Observer

Implement infinite scroll in Client Component using Intersection Observer API. Loads more content as user scrolls.

```typescript
// app/posts/infinite/page.tsx
'use client';

import { useState, useEffect, useRef } from 'react';

export default function InfiniteScrollPage() {
  const [posts, setPosts] = useState<any[]>([]);
  const [page, setPage] = useState(1);
  const [hasMore, setHasMore] = useState(true);
  const [isLoading, setIsLoading] = useState(false);

  // => Ref to sentinel element (bottom of list)
  const sentinelRef = useRef<HTMLDivElement>(null);

  // => Fetch posts function
  async function fetchPosts(pageNum: number) {
    setIsLoading(true);

    // => Fetch from API
    const res = await fetch(`/api/posts?page=${pageNum}&limit=10`);
    const data = await res.json();
    // => data is { posts: [...], hasMore: boolean }

    // => Append new posts
    setPosts(prev => [...prev, ...data.posts]);
    setHasMore(data.hasMore);               // => More pages available?

    setIsLoading(false);
  }

  // => Intersection Observer effect
  useEffect(() => {
    const sentinel = sentinelRef.current;
    if (!sentinel) return;

    // => Create observer
    const observer = new IntersectionObserver(
      (entries) => {
        // => entries[0] is sentinel element
        const entry = entries[0];

        if (entry.isIntersecting && hasMore && !isLoading) {
          // => Sentinel visible, has more pages, not currently loading
          setPage(prev => prev + 1);        // => Increment page
          // => Triggers fetchPosts in separate effect
        }
      },
      {
        threshold: 0.1,                     // => Trigger when 10% visible
      }
    );

    // => Observe sentinel
    observer.observe(sentinel);

    // => Cleanup
    return () => observer.disconnect();
  }, [hasMore, isLoading]);

  // => Fetch posts when page changes
  useEffect(() => {
    fetchPosts(page);
  }, [page]);

  return (
    <div>
      <h1>Infinite Scroll Posts</h1>

      <ul>
        {posts.map((post, index) => (
          <li key={`${post.id}-${index}`}>
            {post.title}
          </li>
        ))}
      </ul>

      {/* => Sentinel element (invisible) */}
      <div ref={sentinelRef} style={{ height: '10px' }} />

      {/* => Loading indicator */}
      {isLoading && <p>Loading more posts...</p>}

      {/* => End message */}
      {!hasMore && <p>No more posts to load.</p>}
    </div>
  );
}
```

**Key Takeaway**: Implement infinite scroll with Intersection Observer API. Automatically loads more content when user scrolls to bottom.

**Expected Output**: Posts load 10 at a time. Scrolling to bottom automatically fetches next page. Loading indicator shows during fetch.

**Common Pitfalls**: Not disconnecting observer (memory leak), or triggering multiple fetches (check isLoading flag).

## Group 10: Search & Filtering

### Example 48: Search with Debouncing

Implement search with debouncing to reduce API calls. Waits for user to stop typing before searching.

```typescript
// app/lib/hooks.ts
'use client';

import { useEffect, useState } from 'react';

// => Custom debounce hook
export function useDebounce<T>(value: T, delay: number): T {
  const [debouncedValue, setDebouncedValue] = useState(value);

  useEffect(() => {
    // => Set timeout to update debounced value
    const timer = setTimeout(() => {
      setDebouncedValue(value);
      // => Updates after delay (500ms)
    }, delay);

    // => Cleanup: cancel timeout if value changes
    return () => clearTimeout(timer);
    // => Prevents update if user still typing
  }, [value, delay]);

  return debouncedValue;
  // => Returns value only after user stops typing for 'delay' ms
}

// app/search/page.tsx
'use client';

import { useState, useEffect } from 'react';
import { useDebounce } from '../lib/hooks';

export default function SearchPage() {
  const [searchTerm, setSearchTerm] = useState('');
  // => searchTerm updates on every keystroke

  const debouncedSearchTerm = useDebounce(searchTerm, 500);
  // => debouncedSearchTerm updates 500ms after user stops typing

  const [results, setResults] = useState<any[]>([]);
  const [isSearching, setIsSearching] = useState(false);

  // => Search effect (triggers on debounced value)
  useEffect(() => {
    if (debouncedSearchTerm.length < 2) {
      // => Don't search for single characters
      setResults([]);
      return;
    }

    // => Perform search
    async function search() {
      setIsSearching(true);

      // => API call with debounced search term
      const res = await fetch(`/api/search?q=${encodeURIComponent(debouncedSearchTerm)}`);
      const data = await res.json();
      // => data is { results: [...] }

      setResults(data.results);
      setIsSearching(false);
    }

    search();
  }, [debouncedSearchTerm]);
  // => Only runs when debouncedSearchTerm changes (500ms after typing stops)

  return (
    <div>
      <h1>Search Posts</h1>

      <input
        type="search"
        placeholder="Search..."
        value={searchTerm}
        onChange={(e) => setSearchTerm(e.target.value)}
        // => Updates on every keystroke (fast)
        style={{ width: '100%', padding: '0.5rem' }}
      />

      {/* => Show searching indicator */}
      {isSearching && <p>Searching...</p>}

      {/* => Show results */}
      <ul>
        {results.map(result => (
          <li key={result.id}>
            <strong>{result.title}</strong>
            <p>{result.excerpt}</p>
          </li>
        ))}
      </ul>

      {/* => No results message */}
      {!isSearching && results.length === 0 && searchTerm.length >= 2 && (
        <p>No results found for "{searchTerm}"</p>
      )}
    </div>
  );
}
```

**Key Takeaway**: Use debouncing to reduce API calls during search. Waits for user to stop typing (500ms) before triggering search.

**Expected Output**: Typing updates input instantly. Search API called only after user stops typing for 500ms. Reduces API calls from dozens to one.

**Common Pitfalls**: Not cleaning up timers (memory leak), or setting debounce delay too long (feels unresponsive).

### Example 49: Real-Time Updates with Server Actions

Use Server Actions for real-time updates without WebSocket complexity. Polling or manual refresh patterns.

```typescript
// app/actions.ts
'use server';

import { revalidatePath } from 'next/cache';

// => Simulated database (in-memory)
let donations: Array<{ id: number; name: string; amount: number; timestamp: Date }> = [];

export async function getDonations() {
  // => Server Action to fetch donations
  return donations;
}

export async function addDonation(formData: FormData) {
  const name = formData.get('name') as string;
  const amount = parseInt(formData.get('amount') as string);

  // => Add donation to "database"
  const donation = {
    id: Date.now(),
    name,
    amount,
    timestamp: new Date(),
  };
  donations.push(donation);

  // => Revalidate donations page
  revalidatePath('/donations/live');
  // => Any user viewing /donations/live sees updated list

  return { success: true };
}

// app/donations/live/page.tsx
// => Server Component showing live donations
import { getDonations } from '@/app/actions';

export const revalidate = 5;                // => Revalidate every 5 seconds
// => Page data refreshes automatically every 5 seconds

export default async function LiveDonationsPage() {
  const donations = await getDonations();   // => Fetch current donations

  return (
    <div>
      <h1>Live Donations</h1>
      <p>Updates every 5 seconds</p>

      <ul>
        {donations.map(donation => (
          <li key={donation.id}>
            <strong>{donation.name}</strong> donated IDR {donation.amount.toLocaleString()}
            <small> - {donation.timestamp.toLocaleTimeString()}</small>
          </li>
        ))}
      </ul>

      {donations.length === 0 && <p>No donations yet.</p>}
    </div>
  );
}

// app/donations/add/page.tsx
'use client';

import { addDonation } from '@/app/actions';
import { useRouter } from 'next/navigation';

export default function AddDonationPage() {
  const router = useRouter();

  async function handleSubmit(formData: FormData) {
    await addDonation(formData);
    // => Server Action adds donation and revalidates

    // => Redirect to live page
    router.push('/donations/live');
    // => Live page shows updated list
  }

  return (
    <div>
      <h1>Add Donation</h1>

      <form action={handleSubmit}>
        <input type="text" name="name" placeholder="Your name" required />
        <input type="number" name="amount" placeholder="Amount" required />
        <button type="submit">Donate</button>
      </form>
    </div>
  );
}
```

**Key Takeaway**: Use Server Actions with revalidatePath() for real-time updates. Simpler than WebSockets for most use cases. Page auto-refreshes every 5 seconds.

**Expected Output**: Live donations page shows current donations, auto-updates every 5 seconds. Adding donation immediately visible to all users viewing live page.

**Common Pitfalls**: Setting revalidate too low (server load), or not revalidating after mutations (users see stale data).

### Example 50: Advanced Middleware with Custom Headers

Create advanced middleware patterns for redirects, headers, and conditional logic chains.

```typescript
// middleware.ts
import { NextResponse } from "next/server";
import type { NextRequest } from "next/server";

export function middleware(request: NextRequest) {
  const { pathname } = request.nextUrl;
  const response = NextResponse.next();

  // => Pattern 1: Redirect old URLs to new structure
  if (pathname.startsWith("/old-blog")) {
    // => Permanent redirect (301)
    const newPath = pathname.replace("/old-blog", "/blog");
    return NextResponse.redirect(new URL(newPath, request.url), 301);
    // => Browser updates bookmarks, SEO passes to new URL
  }

  // => Pattern 2: Add custom headers based on path
  if (pathname.startsWith("/api/")) {
    // => API routes get CORS headers
    response.headers.set("Access-Control-Allow-Origin", "*");
    response.headers.set("Access-Control-Allow-Methods", "GET, POST, OPTIONS");
    response.headers.set("Access-Control-Allow-Headers", "Content-Type");
  }

  // => Pattern 3: Conditional redirect based on cookie
  const subscription = request.cookies.get("subscription");
  if (pathname.startsWith("/premium") && subscription?.value !== "active") {
    // => No active subscription, redirect to pricing
    return NextResponse.redirect(new URL("/pricing", request.url));
  }

  // => Pattern 4: A/B testing with cookies
  if (pathname === "/pricing") {
    const variant = request.cookies.get("pricing_variant");

    if (!variant) {
      // => Assign random variant
      const newVariant = Math.random() > 0.5 ? "a" : "b";
      response.cookies.set("pricing_variant", newVariant, {
        maxAge: 60 * 60 * 24 * 30, // => 30 days
      });

      // => Rewrite to variant page
      if (newVariant === "b") {
        return NextResponse.rewrite(new URL("/pricing-variant-b", request.url));
      }
    } else if (variant.value === "b") {
      return NextResponse.rewrite(new URL("/pricing-variant-b", request.url));
    }
  }

  // => Pattern 5: Add security headers to all responses
  response.headers.set("X-Frame-Options", "DENY");
  response.headers.set("X-Content-Type-Options", "nosniff");
  response.headers.set("Referrer-Policy", "strict-origin-when-cross-origin");
  response.headers.set("Permissions-Policy", "camera=(), microphone=(), geolocation=()");

  // => Pattern 6: Geo-based redirects (using Vercel geo headers)
  const country = request.geo?.country;
  if (pathname === "/" && country === "ID") {
    // => Indonesian visitors see Indonesian homepage
    return NextResponse.rewrite(new URL("/id", request.url));
  }

  return response;
}

export const config = {
  matcher: [
    // => Run on all paths except static files
    "/((?!_next/static|_next/image|favicon.ico|.*\\.(?:svg|png|jpg|jpeg|gif|webp)$).*)",
  ],
};
```

**Key Takeaway**: Middleware enables powerful request/response manipulation: redirects, rewrites, headers, cookies, A/B testing, geo-routing. Runs on every matching request.

**Expected Output**: Old URLs redirect to new structure. API routes get CORS headers. Premium content protected. A/B test variants assigned. Security headers added.

**Common Pitfalls**: Middleware runs on every request (keep it fast), or forgetting matcher config (runs on static files unnecessarily).

## Summary

These 25 intermediate examples cover production-ready Next.js patterns:

**Advanced Server Actions**: useFormState (validation errors), useFormStatus (loading states), progressive enhancement (works without JavaScript)

**Cache Revalidation**: Time-based (ISR with revalidate), path-based (revalidatePath), tag-based (revalidateTag)

**Route Organization**: Route groups ((folder) syntax), parallel routes (@folder), intercepting routes ((.)folder for modals)

**Advanced Forms**: Zod validation (type-safe schemas), optimistic updates (useOptimistic), React Hook Form (complex forms), Zod transforms (data normalization), file uploads (secure handling)

**Authentication**: Cookies-based (httpOnly cookies), middleware-based (protect multiple routes)

**Database Integration**: Prisma ORM (type-safe queries), error handling (try-catch, Prisma error codes)

**Client-Side Data Fetching**: SWR (caching, revalidation), TanStack Query (advanced state management)

**Pagination & Infinite Scroll**: Server-side pagination (SEO-friendly), infinite scroll (Intersection Observer)

**Search & Real-Time**: Debounced search (reduce API calls), real-time updates (Server Actions with revalidation)

**Advanced Middleware**: Custom headers, redirects, A/B testing, geo-routing, security headers

**Next**: [Advanced examples](/en/learn/software-engineering/web-platform/fe-nextjs/by-example/advanced) for performance optimization, advanced caching, streaming, and deployment patterns.
