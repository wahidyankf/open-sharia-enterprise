---
title: "Intermediate"
weight: 10000002
date: 2026-03-25T00:00:00+07:00
draft: false
description: "Master production tRPC patterns through 27 annotated examples covering nested routers, auth middleware, subscriptions, React Query integration, optimistic updates, infinite queries, batching, SSR, and error formatting"
tags: ["trpc", "typescript", "api", "tutorial", "by-example", "intermediate"]
---

This intermediate tutorial covers production tRPC patterns through 27 heavily annotated examples. Each example maintains 1-2.25 comment lines per code line. Examples build on beginner concepts—you should understand `initTRPC`, procedures, context, and basic middleware before starting.

## Prerequisites

Before starting, ensure you understand:

- tRPC beginner concepts (Examples 1-28)
- TypeScript generics and conditional types
- React hooks and React Query basics
- Async JavaScript patterns

## Group 1: Nested Routers and Middleware Composition

### Example 29: Deeply Nested Router Architecture

Production tRPC APIs mirror domain boundaries with multi-level router hierarchies.

```typescript
import { initTRPC } from "@trpc/server";
import { z } from "zod";

interface Context {
  userId: string | null;
}

const t = initTRPC.context<Context>().create();

// => Admin sub-router: procedures requiring admin role
const adminUsersRouter = t.router({
  // => Path: admin.users.list
  list: t.procedure.query(() => [
    { id: "u1", name: "Aisha", role: "admin" },
    { id: "u2", name: "Omar", role: "member" },
  ]),

  // => Path: admin.users.suspend
  suspend: t.procedure.input(z.object({ userId: z.string() })).mutation((opts) => ({
    suspended: true,
    userId: opts.input.userId,
  })),
});

// => Admin router: groups admin sub-domains
const adminRouter = t.router({
  users: adminUsersRouter, // => admin.users.*
  // => Could add: admin.billing, admin.settings, etc.
});

// => Public API router: no auth required
const publicRouter = t.router({
  status: t.procedure.query(() => ({
    api: "ok",
    timestamp: Date.now(),
  })),
});

// => Root router: top-level namespaces
const appRouter = t.router({
  public: publicRouter, // => public.status
  admin: adminRouter, // => admin.users.list, admin.users.suspend
});
// => Client: trpc.public.status.query(), trpc.admin.users.list.query()

export type AppRouter = typeof appRouter;
export { appRouter };
```

**Key Takeaway**: Nest routers three or more levels deep to mirror domain boundaries. Access paths like `admin.users.list` appear identically on client and server.

**Why It Matters**: Deep router hierarchies keep large APIs navigable. A SaaS platform might have `admin.billing.invoices.list`, `user.profile.preferences.update`, and `public.content.articles.search`—each path communicates domain context. Clients benefit from nested autocomplete: typing `trpc.admin.` surfaces only admin procedures. New team members understand the domain model by reading the router tree.

### Example 30: Middleware with Context Narrowing

Middleware can narrow context types, making TypeScript enforce invariants in protected procedures.

```typescript
import { initTRPC, TRPCError } from "@trpc/server";

// => Base context: nullable user
interface BaseContext {
  user: { id: string; role: "admin" | "member" } | null;
}

// => Narrowed context: guaranteed non-null user
interface AuthContext extends BaseContext {
  user: { id: string; role: "admin" | "member" }; // => Never null here
}

// => Admin-narrowed context: guaranteed admin user
interface AdminContext extends BaseContext {
  user: { id: string; role: "admin" }; // => Role narrowed to "admin" only
}

const t = initTRPC.context<BaseContext>().create();

// => Middleware 1: ensures user is authenticated
const requireAuth = t.middleware((opts) => {
  if (!opts.ctx.user) {
    throw new TRPCError({ code: "UNAUTHORIZED", message: "Not authenticated" });
  }
  // => TypeScript narrows: user is non-null below opts.next()
  return opts.next({ ctx: { ...opts.ctx, user: opts.ctx.user } as AuthContext });
});

// => Middleware 2: ensures user is admin (chains after requireAuth)
const requireAdmin = t.middleware((opts) => {
  const user = (opts.ctx as AuthContext).user; // => Already guaranteed non-null by requireAuth
  if (user.role !== "admin") {
    throw new TRPCError({ code: "FORBIDDEN", message: "Admin only" });
  }
  // => Narrow role: "admin" | "member" → "admin"
  return opts.next({
    ctx: { ...opts.ctx, user: { ...user, role: "admin" as const } } as AdminContext,
  });
});

// => Procedure bases: each with progressively stricter requirements
const authedProcedure = t.procedure.use(requireAuth);
// => ctx.user is AuthContext["user"] (non-null)
const adminProcedure = authedProcedure.use(requireAdmin);
// => ctx.user is AdminContext["user"] (non-null, role === "admin")

const appRouter = t.router({
  // => Only authenticated users
  myProfile: authedProcedure.query((opts) => {
    const { user } = opts.ctx as AuthContext;
    return { id: user.id, role: user.role }; // => TypeScript: user definitely exists
  }),

  // => Only admins
  allUsers: adminProcedure.query((opts) => {
    const { user } = opts.ctx as AdminContext;
    return { adminId: user.id, users: [] }; // => TypeScript: role is definitely "admin"
  }),
});

export type AppRouter = typeof appRouter;
export { appRouter };
```

**Key Takeaway**: Chain middleware to progressively narrow context types. Each middleware adds invariants that downstream code can rely on without null checks.

**Why It Matters**: Type narrowing through middleware layers eliminates repetitive defensive checks. Without it, every admin procedure would check `if (ctx.user?.role !== 'admin')`. With narrowing, the type system enforces this at the procedure base level. TypeScript turns runtime behavior into compile-time constraints—a new procedure that uses `adminProcedure` cannot accidentally skip the admin check.

### Example 31: Rate Limiting Middleware

Implement request throttling to protect procedures from abuse.

```typescript
import { initTRPC, TRPCError } from "@trpc/server";

interface Context {
  ip: string;
  userId: string | null;
}

const t = initTRPC.context<Context>().create();

// => In-memory rate limit store (production: use Redis)
const rateLimitStore = new Map<string, { count: number; windowStart: number }>();

// => Rate limit configuration
const RATE_LIMIT = {
  MAX_REQUESTS: 10, // => Max requests per window
  WINDOW_MS: 60_000, // => 1 minute window in milliseconds
};

// => Rate limiting middleware factory: configurable per procedure
function createRateLimitMiddleware(maxRequests: number) {
  return t.middleware((opts) => {
    const { ip, userId } = opts.ctx;
    // => Use userId if authenticated, IP if not - prevents auth bypass
    const key = userId ? `user:${userId}` : `ip:${ip}`;

    const now = Date.now();
    const record = rateLimitStore.get(key);

    if (!record || now - record.windowStart > RATE_LIMIT.WINDOW_MS) {
      // => New window: reset counter
      rateLimitStore.set(key, { count: 1, windowStart: now });
      // => count: 1 (first request in this window)
    } else {
      // => Existing window: increment counter
      record.count++;
      // => count: 2, 3, ..., maxRequests

      if (record.count > maxRequests) {
        // => Limit exceeded: reject request
        throw new TRPCError({
          code: "TOO_MANY_REQUESTS", // => HTTP 429
          message: `Rate limit exceeded. Try again in ${Math.ceil((RATE_LIMIT.WINDOW_MS - (now - record.windowStart)) / 1000)}s`,
        });
      }
    }

    return opts.next(); // => Under limit: proceed normally
  });
}

// => Different rate limits for different sensitivity levels
const strictRateLimit = createRateLimitMiddleware(5); // => 5 req/min (auth endpoints)
const normalRateLimit = createRateLimitMiddleware(RATE_LIMIT.MAX_REQUESTS); // => 10 req/min

const appRouter = t.router({
  // => Login: strict rate limit prevents brute force
  login: t.procedure.use(strictRateLimit).mutation(() => ({
    token: "jwt-token-here",
  })),

  // => Search: normal rate limit
  search: t.procedure.use(normalRateLimit).query(() => ({
    results: [],
  })),
});

export type AppRouter = typeof appRouter;
export { appRouter };
```

**Key Takeaway**: Rate limiting middleware uses a keyed store (user ID or IP) to count requests within a sliding window. Configure different limits for different procedure sensitivity levels.

**Why It Matters**: Rate limiting is a first-line defense against credential stuffing, data scraping, and denial-of-service attacks. Authentication endpoints need stricter limits (5 req/min) than search endpoints (100 req/min). Production systems use Redis instead of in-memory Maps for distributed rate limiting across multiple server instances. The `TOO_MANY_REQUESTS` code maps to HTTP 429, which clients can detect to show countdown timers.

## Group 2: Subscriptions

### Example 32: Basic Subscription with Observable

tRPC subscriptions enable real-time updates using server-to-client event streams.

```typescript
import { initTRPC } from "@trpc/server";
import { observable } from "@trpc/server/observable";
// => observable: creates a tRPC-compatible event stream
import { z } from "zod";

const t = initTRPC.create();

// => EventEmitter alternative: simple event bus for this example
type EventCallback = (data: { message: string; at: string }) => void;
const listeners = new Set<EventCallback>();

// => Emit function: broadcasts to all subscribers
function broadcastMessage(message: string) {
  const event = { message, at: new Date().toISOString() };
  listeners.forEach((cb) => cb(event)); // => Calls every active subscriber
}

const appRouter = t.router({
  // => Subscription procedure: returns an observable
  onMessage: t.procedure.input(z.object({ roomId: z.string() })).subscription((opts) => {
    const { roomId } = opts.input;
    // => roomId: string (could filter events by room)

    // => observable: wraps event listener lifecycle
    return observable<{ message: string; at: string }>((emit) => {
      // => emit.next(): sends an event to this subscriber
      const listener: EventCallback = (data) => {
        emit.next(data); // => Push event to connected client
        // => Client receives: { message: "Hello!", at: "2026-03-25T..." }
      };

      listeners.add(listener); // => Subscribe: start receiving events

      // => Return cleanup function: called when client disconnects
      return () => {
        listeners.delete(listener); // => Unsubscribe: stop receiving events
        console.log(`Subscriber for room ${roomId} disconnected`);
      };
    });
  }),
});

export type AppRouter = typeof appRouter;
export { appRouter };
export { broadcastMessage }; // => Export for testing/server use
```

**Key Takeaway**: Subscription procedures return `observable()` which manages subscribe/unsubscribe lifecycle. The cleanup function runs when the client disconnects.

**Why It Matters**: Subscriptions replace polling for real-time features. Instead of clients requesting updates every second (wasteful), the server pushes updates only when data changes. Chat applications, live dashboards, collaborative editors, and notification systems all use this pattern. The cleanup function prevents memory leaks when clients disconnect—missing it means dead listeners accumulate in `listeners` indefinitely.

### Example 33: Subscription with Filtering and State

Subscriptions can maintain state and filter events per subscriber.

```typescript
import { initTRPC } from "@trpc/server";
import { observable } from "@trpc/server/observable";
import { z } from "zod";

const t = initTRPC.create();

// => Event types for type-safe event bus
type PriceEvent = {
  symbol: string;
  price: number;
  change: number;
  timestamp: number;
};

// => Global event bus: stores listeners per symbol
const priceListeners = new Map<string, Set<(event: PriceEvent) => void>>();

// => Publisher function: emit price update for a symbol
function emitPriceUpdate(event: PriceEvent) {
  const symbolListeners = priceListeners.get(event.symbol);
  if (symbolListeners) {
    symbolListeners.forEach((cb) => cb(event)); // => Notify all watchers for this symbol
  }
}

const appRouter = t.router({
  // => Subscribe to price updates for a specific stock symbol
  watchPrice: t.procedure
    .input(
      z.object({
        symbol: z.string().toUpperCase(), // => Normalize: "aapl" → "AAPL"
        minChangePercent: z.number().default(0), // => Only emit if change exceeds threshold
      }),
    )
    .subscription((opts) => {
      const { symbol, minChangePercent } = opts.input;
      // => symbol: "AAPL" (normalized), minChangePercent: 0 (default)

      return observable<PriceEvent>((emit) => {
        const listener = (event: PriceEvent) => {
          // => Filter: only emit if price change exceeds threshold
          if (Math.abs(event.change) >= minChangePercent) {
            emit.next(event); // => Emit filtered event to this subscriber
          }
          // => Events below threshold are silently dropped
        };

        // => Register listener for this symbol
        if (!priceListeners.has(symbol)) {
          priceListeners.set(symbol, new Set()); // => Initialize set if first subscriber
        }
        priceListeners.get(symbol)!.add(listener);
        // => Added to symbol's listener set

        // => Cleanup: remove listener when subscriber disconnects
        return () => {
          priceListeners.get(symbol)?.delete(listener);
          if (priceListeners.get(symbol)?.size === 0) {
            priceListeners.delete(symbol); // => Remove empty symbol entry
          }
        };
      });
    }),
});

export type AppRouter = typeof appRouter;
export { appRouter };
export { emitPriceUpdate };
```

**Key Takeaway**: Filter events inside the observable callback to send only relevant updates to each subscriber. Per-subscriber filtering reduces unnecessary client processing.

**Why It Matters**: Server-side filtering in subscriptions is more efficient than sending all events and filtering on the client. A trading dashboard with 1000 subscribers watching different stocks should not send every price update to every subscriber. Filtering at the source reduces bandwidth and client CPU. The cleanup pattern with Map-based symbol groups also prevents memory leaks in long-running servers.

## Group 3: React Query Integration

### Example 34: Setting Up the tRPC React Client

Connecting tRPC to a React application requires a client setup that integrates with React Query.

```typescript
// trpc.ts - Client setup file
import { createTRPCReact } from "@trpc/react-query";
// => createTRPCReact: creates React hooks bound to your AppRouter type
import { httpBatchLink } from "@trpc/client";
// => httpBatchLink: HTTP transport that batches multiple requests
import { QueryClient, QueryClientProvider } from "@tanstack/react-query";
// => QueryClient: manages React Query's cache and request lifecycle
import type { AppRouter } from "../server/router";
// => AppRouter: server type import (type only, no runtime dependency)

// => Create typed tRPC hooks bound to AppRouter
// => All hooks (useQuery, useMutation, etc.) are fully typed from AppRouter
export const trpc = createTRPCReact<AppRouter>();
// => trpc.users.getById.useQuery({ id: 1 }) - fully typed

// => Create React Query client with sensible defaults
export const queryClient = new QueryClient({
  defaultOptions: {
    queries: {
      staleTime: 30_000, // => Data considered fresh for 30 seconds
      // => Prevents unnecessary refetches during this window
      retry: 2, // => Retry failed queries 2 times before showing error
    },
  },
});

// => Create tRPC client that knows how to send HTTP requests
export const trpcClient = trpc.createClient({
  links: [
    httpBatchLink({
      url: "http://localhost:3000/trpc", // => tRPC server endpoint
      // => Batches multiple queries into single HTTP request
      // => trpc.users.list.useQuery() + trpc.posts.list.useQuery() = 1 HTTP request
    }),
  ],
});

// => App.tsx: wrap your app with both providers
// => export function App() {
// =>   return (
// =>     <trpc.Provider client={trpcClient} queryClient={queryClient}>
// =>       <QueryClientProvider client={queryClient}>
// =>         <YourAppContent />
// =>       </QueryClientProvider>
// =>     </trpc.Provider>
// =>   );
// => }
```

**Key Takeaway**: `createTRPCReact<AppRouter>()` creates typed React hooks. Both `trpc.Provider` and `QueryClientProvider` must wrap your app.

**Why It Matters**: The client setup binds your server's type system to React's data layer. Once configured, every hook call gets full TypeScript inference from the AppRouter type. Changing a procedure's return type on the server immediately flags stale client code. The batching link is a production optimization—loading a dashboard with 5 data requirements sends 1 HTTP request instead of 5.

### Example 35: useQuery for Data Fetching

`trpc.[procedure].useQuery()` fetches data with React Query's caching, loading states, and error handling.

```typescript
// UserProfile.tsx
import { trpc } from "./trpc";

// => React component using tRPC query hook
function UserProfile({ userId }: { userId: number }) {
  // => trpc.users.getById.useQuery() - fully typed React Query hook
  // => data type inferred from server's getById return type
  const { data, isLoading, isError, error, refetch } =
    trpc.users.getById.useQuery(
      { id: userId }, // => Input: typed as { id: number }
      {
        // => React Query options
        staleTime: 60_000, // => Cache user profile for 60 seconds
        // => Prevent refetch on every component mount
        enabled: userId > 0, // => Only fetch when userId is valid
        // => enabled: false when userId is 0 or negative
      }
    );

  // => isLoading: true during initial fetch (no cached data)
  if (isLoading) return <div>Loading profile...</div>;

  // => isError: true when query throws
  // => error.message is typed from server's TRPCError
  if (isError) return <div>Error: {error.message}</div>;

  // => data is undefined until loaded (type: ReturnType<getById> | undefined)
  // => After successful load, data is the typed procedure return value
  if (!data) return <div>User not found</div>;

  return (
    <div>
      {/* => data.name: string - TypeScript knows this from AppRouter */}
      <h1>{data.name}</h1>
      <p>{data.email}</p>
      {/* => refetch: manually trigger a fresh fetch */}
      <button onClick={() => refetch()}>Refresh</button>
    </div>
  );
}

export { UserProfile };
```

**Key Takeaway**: `useQuery()` provides `data`, `isLoading`, `isError`, and `error`—all typed from the procedure's TypeScript signature.

**Why It Matters**: React Query's caching eliminates redundant network requests. If two components on the same page call `trpc.users.getById.useQuery({ id: 1 })`, only one request fires. The second component reads from cache. `staleTime` controls freshness—profile data can be stale for 60 seconds; stock prices need 0ms staleTime. The `enabled` option prevents queries until prerequisites (user ID, auth token) are ready.

### Example 36: useMutation for Write Operations

`trpc.[procedure].useMutation()` handles mutations with loading, success, and error callbacks.

```typescript
// CreatePostForm.tsx
import { useState } from "react";
import { trpc } from "./trpc";

function CreatePostForm({ onSuccess }: { onSuccess: () => void }) {
  const [title, setTitle] = useState("");
  const [content, setContent] = useState("");

  // => useMutation returns a mutate function and state
  const createPost = trpc.posts.create.useMutation({
    // => onSuccess: called after successful mutation
    onSuccess: (data) => {
      // => data: typed return value from posts.create procedure
      console.log("Created post:", data.id); // => e.g., "Created post: 42"
      setTitle(""); // => Reset form on success
      setContent("");
      onSuccess(); // => Notify parent component
    },
    // => onError: called when mutation throws TRPCError
    onError: (error) => {
      console.error("Failed:", error.message); // => e.g., "Title too short"
      // => error.data?.code: "BAD_REQUEST" | "UNAUTHORIZED" etc.
    },
  });

  const handleSubmit = (e: React.FormEvent) => {
    e.preventDefault();
    // => mutate(): triggers the mutation
    // => Input typed as posts.create procedure's input schema
    createPost.mutate({ title, content });
    // => Sends: { title: "My Post", content: "Post body..." }
  };

  return (
    <form onSubmit={handleSubmit}>
      <input value={title} onChange={(e) => setTitle(e.target.value)} />
      <textarea value={content} onChange={(e) => setContent(e.target.value)} />
      {/* => createPost.isPending: true while mutation is in-flight */}
      <button type="submit" disabled={createPost.isPending}>
        {createPost.isPending ? "Creating..." : "Create Post"}
      </button>
      {/* => Show error message if mutation failed */}
      {createPost.isError && <p>{createPost.error.message}</p>}
    </form>
  );
}

export { CreatePostForm };
```

**Key Takeaway**: `useMutation()` provides `mutate()`, `isPending`, `isError`, and `error`. Use `onSuccess` and `onError` callbacks for side effects like cache invalidation and form resets.

**Why It Matters**: The `isPending` state is essential UX—disabling the submit button prevents double-submission. The typed `data` in `onSuccess` means you can use the created resource's ID for navigation (`router.push('/posts/' + data.id)`) without any casting. Production forms combine this with React Hook Form for field-level validation and Zod schemas shared between client and server validation.

### Example 37: Cache Invalidation with useUtils

After mutations, invalidate related queries to keep the UI synchronized with server state.

```typescript
// PostList with invalidation
import { trpc } from "./trpc";

function PostList() {
  // => useUtils: access React Query utilities for cache management
  const utils = trpc.useUtils();
  // => utils: typed cache management API (invalidate, setData, prefetch)

  // => Fetch posts list
  const { data: posts } = trpc.posts.list.useQuery();
  // => posts: Post[] | undefined

  // => Delete mutation with cache invalidation
  const deletePost = trpc.posts.delete.useMutation({
    onSuccess: async () => {
      // => Invalidate posts.list cache: triggers a background refetch
      await utils.posts.list.invalidate();
      // => React Query marks posts.list as stale and refetches
      // => Component re-renders with updated list after refetch completes
    },
  });

  // => Create mutation: invalidate after creation
  const createPost = trpc.posts.create.useMutation({
    onSuccess: () => {
      // => Can invalidate specific keys or all queries under a prefix
      void utils.posts.invalidate(); // => Invalidates ALL posts.* queries
      // => Useful when multiple list views might be affected
    },
  });

  return (
    <div>
      {posts?.map((post) => (
        <div key={post.id}>
          <span>{post.title}</span>
          <button
            onClick={() => deletePost.mutate({ id: post.id })}
            disabled={deletePost.isPending}
          >
            Delete
          </button>
        </div>
      ))}
      <button onClick={() => createPost.mutate({ title: "New Post", content: "" })}>
        Add Post
      </button>
    </div>
  );
}

export { PostList };
```

**Key Takeaway**: `utils.[procedure].invalidate()` marks cached data stale, triggering background refetches. Invalidate specific procedures or entire namespaces after mutations.

**Why It Matters**: Cache invalidation is the hardest problem in distributed systems, and React Query's declarative approach simplifies it. Without invalidation, deleting a post leaves it visible in the list until a page refresh. With invalidation, the list automatically refetches after every delete. Namespace invalidation (`utils.posts.invalidate()`) is useful when an action affects multiple list views (active posts, draft posts, archived posts).

### Example 38: Optimistic Updates

Update the UI immediately before the server confirms the mutation, then roll back if it fails.

```typescript
// OptimisticTodoList.tsx
import { trpc } from "./trpc";

type Todo = { id: number; text: string; done: boolean };

function OptimisticTodoList() {
  const utils = trpc.useUtils();

  const { data: todos } = trpc.todos.list.useQuery();
  // => todos: Todo[] | undefined

  const toggleTodo = trpc.todos.toggle.useMutation({
    // => onMutate: runs BEFORE the mutation request fires
    onMutate: async ({ id, done }) => {
      // => Step 1: Cancel any in-flight refetches to avoid overwriting optimistic update
      await utils.todos.list.cancel();
      // => Prevents stale server data from overwriting our optimistic state

      // => Step 2: Save current state for rollback
      const previousTodos = utils.todos.list.getData();
      // => previousTodos: current cached Todo[] (before optimistic update)

      // => Step 3: Apply optimistic update to cache
      utils.todos.list.setData(undefined, (old) =>
        old?.map((todo) =>
          todo.id === id ? { ...todo, done } : todo
          // => Flip the specific todo's done status immediately
        )
      );

      // => Return context for rollback on error
      return { previousTodos }; // => Saved for onError
    },

    onError: (_err, _vars, context) => {
      // => Mutation failed: restore previous state
      if (context?.previousTodos) {
        utils.todos.list.setData(undefined, context.previousTodos);
        // => UI snaps back to state before optimistic update
      }
    },

    onSettled: () => {
      // => onSettled: always runs after success or error
      void utils.todos.list.invalidate(); // => Sync with authoritative server state
    },
  });

  return (
    <ul>
      {todos?.map((todo) => (
        <li key={todo.id} style={{ textDecoration: todo.done ? "line-through" : "none" }}>
          <input
            type="checkbox"
            checked={todo.done}
            onChange={(e) => toggleTodo.mutate({ id: todo.id, done: e.target.checked })}
          />
          {todo.text}
        </li>
      ))}
    </ul>
  );
}

export { OptimisticTodoList };
```

**Key Takeaway**: Optimistic updates use `onMutate` to update cache immediately, `onError` to roll back on failure, and `onSettled` to sync with server state regardless of outcome.

**Why It Matters**: Optimistic updates make interfaces feel instant. Checking a checkbox in a todo app should feel immediate, not show a spinner while waiting for the server. The rollback pattern handles the edge case where the server rejects the mutation (e.g., the todo was already deleted by another device). Production apps use optimistic updates for reactions (likes), status toggles, and any operation where immediate feedback matters more than perfect consistency.

### Example 39: Infinite Queries for Pagination

`useInfiniteQuery` handles cursor-based pagination, loading more pages on demand.

```typescript
// InfinitePosts.tsx
import { trpc } from "./trpc";

function InfinitePosts() {
  // => useInfiniteQuery: manages paginated data with accumulation
  const {
    data,
    fetchNextPage, // => Load the next page
    hasNextPage, // => Boolean: more pages available
    isFetchingNextPage, // => Boolean: currently loading next page
    isLoading,
  } = trpc.posts.list.useInfiniteQuery(
    { limit: 10 }, // => Base input: 10 posts per page
    {
      // => getNextPageParam: extract cursor for next page from last page's response
      getNextPageParam: (lastPage) => lastPage.nextCursor,
      // => lastPage.nextCursor: null when no more pages (stops pagination)
      // => Server must return nextCursor in response for this to work
    }
  );

  if (isLoading) return <div>Loading...</div>;

  // => data.pages: array of page responses, accumulated across loads
  // => data.pages[0]: first page, data.pages[1]: second page, etc.
  const allPosts = data?.pages.flatMap((page) => page.posts) ?? [];
  // => Flatten pages into single array: page1.posts + page2.posts + ...

  return (
    <div>
      {allPosts.map((post) => (
        <div key={post.id}>
          <h3>{post.title}</h3>
          <p>{post.excerpt}</p>
        </div>
      ))}

      {/* => Load More button: fetch next page */}
      {hasNextPage && (
        <button
          onClick={() => fetchNextPage()} // => Triggers next page fetch
          disabled={isFetchingNextPage}
        >
          {isFetchingNextPage ? "Loading more..." : "Load More"}
        </button>
      )}
    </div>
  );
}

export { InfinitePosts };
```

**Key Takeaway**: `useInfiniteQuery` accumulates pages in `data.pages`. `getNextPageParam` extracts the cursor from each page response. `hasNextPage` becomes false when `getNextPageParam` returns `undefined` or `null`.

**Why It Matters**: Infinite scroll is ubiquitous in modern web applications. Loading all 10,000 posts at once is impractical—cursor-based pagination loads pages on demand. The cursor approach (vs offset pagination) handles real-time data insertion without skipping or duplicating items. Social feeds, search results, and audit logs all use this pattern. `data.pages.flatMap()` is the standard way to render accumulated results.

## Group 4: Advanced Error Handling

### Example 40: Custom Error Formatter

Shape error responses for your client's exact needs by implementing a custom error formatter.

```typescript
import { initTRPC, TRPCError } from "@trpc/server";
import { ZodError } from "zod";

// => Custom error formatter: transforms errors before sending to clients
const t = initTRPC.create({
  // => errorFormatter receives the error and returns a custom shape
  errorFormatter({ shape, error }) {
    // => shape: default tRPC error shape { message, code, data }
    // => error: the original error object (TRPCError or underlying Error)

    return {
      ...shape, // => Preserve default fields: message, code, data
      data: {
        ...shape.data, // => Preserve default data: httpStatus, path, code
        // => Add Zod validation error details when applicable
        zodError:
          error.cause instanceof ZodError
            ? error.cause.flatten() // => { fieldErrors: {...}, formErrors: [...] }
            : null,
        // => null when error is not a Zod validation error
        // => { fieldErrors: { email: ["Invalid email"], name: ["Required"] } } for Zod errors
      },
    };
  },
});

const appRouter = t.router({
  createUser: t.procedure
    .input(
      require("zod").z.object({
        // => Would normally import z at top of file
        name: require("zod").z.string().min(2),
        email: require("zod").z.string().email(),
      }),
    )
    .mutation((opts) => ({
      id: 1,
      ...opts.input,
    })),
});

export type AppRouter = typeof appRouter;
export { appRouter };

// => Client receives:
// => {
// =>   message: "Input validation failed",
// =>   code: "BAD_REQUEST",
// =>   data: {
// =>     httpStatus: 400,
// =>     zodError: {
// =>       fieldErrors: { email: ["Invalid email format"] },
// =>       formErrors: []
// =>     }
// =>   }
// => }
```

**Key Takeaway**: The `errorFormatter` option in `initTRPC.create()` intercepts all errors before they reach clients. Add Zod-specific field errors for form validation integration.

**Why It Matters**: Default tRPC error shapes work but aren't always optimal for clients. React Hook Form's `setError()` needs field-level errors in `{ fieldErrors: { fieldName: ["message"] } }` format. A custom formatter provides exactly this shape. Production apps also use formatters to add error tracking IDs (for correlating client errors with server logs), strip stack traces in production, and add retry-after headers for rate limit errors.

### Example 41: Error Handling in Middleware

Middleware can catch and transform errors from downstream procedures.

```typescript
import { initTRPC, TRPCError } from "@trpc/server";

const t = initTRPC.create();

// => Error interception middleware: wraps procedure errors
const errorInterceptor = t.middleware(async (opts) => {
  try {
    const result = await opts.next(); // => Run procedure
    return result; // => Pass through successful result
  } catch (err) {
    if (err instanceof TRPCError) {
      // => TRPCError: already formatted, just log and re-throw
      console.error(`[${opts.path}] TRPCError ${err.code}: ${err.message}`);
      // => e.g., [users.getById] TRPCError NOT_FOUND: User 999 not found
      throw err; // => Re-throw with original error code
    }

    // => Unexpected error: wrap in INTERNAL_SERVER_ERROR
    // => Prevents raw error details (DB schema, file paths) from leaking
    console.error(`[${opts.path}] Unexpected error:`, err);
    // => Logged server-side with full details for debugging

    throw new TRPCError({
      code: "INTERNAL_SERVER_ERROR",
      message: "An unexpected error occurred", // => Safe message for clients
      cause: err, // => Original error preserved in server logs
    });
  }
});

const safeProcedure = t.procedure.use(errorInterceptor);

const appRouter = t.router({
  riskyQuery: safeProcedure.query(async () => {
    // => Simulate unexpected error (e.g., DB connection lost)
    throw new Error("ECONNRESET: connection reset by peer");
    // => Without interceptor: leaks DB error to client
    // => With interceptor: client sees "An unexpected error occurred"
  }),
});

export type AppRouter = typeof appRouter;
export { appRouter };
```

**Key Takeaway**: Error interception middleware wraps all downstream errors, logging details server-side while sending safe messages to clients. Re-throw `TRPCError` instances unchanged.

**Why It Matters**: Leaking raw database errors to clients is both a security issue (reveals schema names, connection strings) and a poor user experience. Middleware-based error wrapping is applied once and protects all procedures. Server logs capture the original error for debugging while clients receive actionable messages. Production tRPC apps pair this with error tracking services—the middleware sends errors to Sentry before re-throwing.

## Group 5: SSR Integration

### Example 42: Server-Side Data Fetching with createServerSideHelpers

Prefetch tRPC data on the server for faster initial page loads in Next.js Pages Router.

```typescript
// pages/users/[id].tsx (Next.js Pages Router)
import {
  createServerSideHelpers,
  type CreateServerSideHelpersOptions,
} from "@trpc/react-query/server";
import superjson from "superjson";
// => superjson: serializes Date, Map, Set (not supported by JSON.stringify)
import type { GetServerSideProps } from "next";
import { trpc } from "../../utils/trpc";
import type { AppRouter } from "../../server/router";
import { appRouter } from "../../server/router";
import { createContext } from "../../server/context";

// => GetServerSideProps: Next.js server function (runs on server, before render)
export const getServerSideProps: GetServerSideProps = async (context) => {
  // => Create server-side tRPC helpers bound to your router
  const helpers = createServerSideHelpers({
    router: appRouter, // => The actual router value (not type)
    ctx: await createContext(), // => Create context as if it were a real request
    transformer: superjson, // => Must match client's transformer configuration
  });

  const id = parseInt(context.params?.id as string);
  // => Extract user ID from URL: /users/42 → id is 42

  // => Prefetch: runs the procedure server-side and stores result in dehydrated state
  await helpers.users.getById.prefetch({ id });
  // => Data fetched server-side, serialized, sent to client as props

  return {
    props: {
      trpcState: helpers.dehydrate(), // => Serialized cache state for client hydration
      id,
    },
  };
};

// => UserPage component: reads from prefetched cache (no network request)
function UserPage({ id }: { id: number }) {
  // => Data already in cache from server prefetch - no loading state needed
  const { data } = trpc.users.getById.useQuery({ id });
  // => data: UserOutput (immediately available, no isLoading flicker)

  return (
    <div>
      <h1>{data?.name}</h1>
      <p>{data?.email}</p>
    </div>
  );
}

export default UserPage;
```

**Key Takeaway**: `createServerSideHelpers` prefetches data in `getServerSideProps`. The dehydrated state hydrates the React Query cache, so `useQuery` returns data immediately without a loading state.

**Why It Matters**: Server-side prefetching eliminates the flash of loading spinners on initial page load. Search engines index the fully rendered content. First Contentful Paint (FCP) and Largest Contentful Paint (LCP) metrics improve dramatically. Production applications with SEO requirements (blog posts, product pages, user profiles) use SSR prefetching for these performance and discoverability benefits.

### Example 43: Next.js App Router Integration Pattern

tRPC integrates with Next.js App Router using React Server Components for direct server calls.

```typescript
// app/users/[id]/page.tsx (Next.js App Router)
import { appRouter } from "../../../server/router";
import { createContext } from "../../../server/context";

// => Server Component: runs on server, can call tRPC directly
// => No HTTP request needed - direct function call
async function UserPage({ params }: { params: { id: string } }) {
  const id = parseInt(params.id);

  // => createCaller: creates a server-side caller for direct procedure calls
  // => Used in Server Components where React Query hooks are not available
  const caller = appRouter.createCaller(await createContext());
  // => caller: typed object with all router procedures

  // => Direct procedure call: no network hop, no serialization overhead
  const user = await caller.users.getById({ id });
  // => user: typed return value (throws TRPCError if not found)

  if (!user) {
    return <div>User not found</div>; // => Render not found state
  }

  return (
    <div>
      {/* => Server-rendered: HTML includes user data from the start */}
      <h1>{user.name}</h1>
      <p>{user.email}</p>
    </div>
  );
}

export default UserPage;

// => For client components that need real-time updates:
// => import { trpc } from "../../utils/trpc";
// => const { data } = trpc.users.getById.useQuery({ id });
// => Client components use hooks; server components use createCaller
```

**Key Takeaway**: In Next.js App Router Server Components, use `appRouter.createCaller()` for direct procedure calls without HTTP overhead. Client components use React Query hooks as normal.

**Why It Matters**: App Router's Server Components are a paradigm shift—they can call async data sources directly without client-side fetch waterfalls. `createCaller` lets Server Components use existing tRPC procedure logic (with validation, auth, middleware) without duplicating it. The same procedure validates inputs and checks authorization whether called from an HTTP client or directly from a Server Component. This unification is a key production advantage of tRPC.

## Group 6: Batching and Performance

### Example 44: Request Batching Configuration

tRPC's HTTP batch link combines multiple concurrent requests into a single HTTP call.

```typescript
// trpc-client.ts
import { createTRPCReact } from "@trpc/react-query";
import { httpBatchLink, splitLink, httpLink } from "@trpc/client";
// => httpBatchLink: batches multiple requests into one HTTP call
// => splitLink: routes requests to different links based on criteria
// => httpLink: sends each request individually (no batching)
import superjson from "superjson";
import type { AppRouter } from "../server/router";

export const trpc = createTRPCReact<AppRouter>();

export const trpcClient = trpc.createClient({
  links: [
    // => splitLink: route subscriptions to WebSocket, queries/mutations to HTTP
    splitLink({
      condition: (op) => op.type === "subscription",
      // => Subscriptions MUST use WebSocket (httpBatchLink doesn't support them)
      true: httpBatchLink({ url: "http://localhost:3000/trpc" }),
      // => Production: use wsLink for subscriptions
      false: httpBatchLink({
        url: "http://localhost:3000/trpc",
        // => transformer: serialize/deserialize Date, Map, Set
        transformer: superjson,
        // => maxURLLength: prevent batched GET requests from exceeding URL limits
        maxURLLength: 2048, // => Falls back to POST if URL would exceed 2048 chars
      }),
    }),
  ],
});

// => Example: this React render fires ONE HTTP request instead of three:
// => const { data: user } = trpc.users.getById.useQuery({ id: 1 });
// => const { data: posts } = trpc.posts.list.useQuery();
// => const { data: settings } = trpc.settings.get.useQuery();
// => All three queries batch into: POST /trpc?batch=1 with body [query1, query2, query3]
// => Server responds with array of results: [result1, result2, result3]
```

**Key Takeaway**: `httpBatchLink` automatically batches concurrent queries into one HTTP request. Use `splitLink` to route subscriptions to WebSocket and queries to HTTP.

**Why It Matters**: HTTP request batching is a significant performance optimization. A page that loads user profile, navigation items, notification count, and recent activity as separate queries would make 4 network round trips with `httpLink`. With `httpBatchLink`, they merge into 1 request with ~400ms latency savings on a 100ms network. Production tRPC applications consistently use batching. The `maxURLLength` guard prevents 414 URI Too Long errors when many queries batch together.

### Example 45: Query Deduplication

React Query automatically deduplicates identical concurrent queries. Understand how this interacts with tRPC.

```typescript
// QueryDeduplication.tsx
import { trpc } from "./trpc";

// => UserBadge: displays user name in a nav element
function UserBadge({ userId }: { userId: string }) {
  // => This query may deduplicate with other UserCard queries
  const { data } = trpc.users.getById.useQuery({ id: parseInt(userId) });
  // => If another component is already fetching user 1, this shares that request
  return <span>{data?.name}</span>;
}

// => UserCard: full user profile card
function UserCard({ userId }: { userId: string }) {
  // => Same query key as UserBadge when userId matches
  // => React Query deduplicates: ONE request serves both components
  const { data } = trpc.users.getById.useQuery({ id: parseInt(userId) });
  return (
    <div>
      <h2>{data?.name}</h2>
      <p>{data?.email}</p>
    </div>
  );
}

// => Page: mounts both components with the same userId
function ProfilePage({ userId }: { userId: string }) {
  return (
    <div>
      {/* => UserBadge and UserCard both call useQuery({ id: 1 }) */}
      {/* => React Query sees identical cache keys: fires ONE request */}
      {/* => Both components receive data when the single request resolves */}
      <UserBadge userId={userId} />
      <UserCard userId={userId} />
    </div>
  );
}

export { ProfilePage };

// => Cache key anatomy:
// => trpc.users.getById.useQuery({ id: 1 })
// =>   key: ["trpc", "users", "getById", { input: { id: 1 } }]
// => trpc.users.getById.useQuery({ id: 2 })
// =>   key: ["trpc", "users", "getById", { input: { id: 2 } }]
// => Different inputs = different cache entries = separate requests
```

**Key Takeaway**: React Query deduplicates queries with identical cache keys. Multiple components requesting the same tRPC procedure with the same input share one network request.

**Why It Matters**: Deduplication makes component-level data fetching practical. You can add `useQuery` to leaf components without worrying about request multiplication. A page with a header badge, profile card, and activity feed all showing the current user's name makes one request, not three. This colocation of data requirements at the component level—without performance penalties—is why React Query and tRPC complement each other so well.

## Group 7: Advanced Input Patterns

### Example 46: Transform and Preprocess Inputs

Zod's `transform` and `preprocess` enable input normalization before validation.

```typescript
import { initTRPC } from "@trpc/server";
import { z } from "zod";

const t = initTRPC.create();

const appRouter = t.router({
  // => preprocess: transform raw input BEFORE type checking
  searchByTag: t.procedure
    .input(
      z.object({
        tag: z.string().transform((s) => s.toLowerCase().trim()),
        // => "  TypeScript  " → "typescript" (before length check)
        // => .trim() removes whitespace, .toLowerCase() normalizes case
        limit: z
          .string() // => Accept string (e.g., from URL params: "?limit=20")
          .transform((s) => parseInt(s, 10)) // => "20" → 20 (parse to number)
          .pipe(z.number().positive().max(100)), // => Validate as positive number ≤ 100
        // => pipe: apply additional validation after transform
      }),
    )
    .query((opts) => {
      const { tag, limit } = opts.input;
      // => tag: string (lowercase, trimmed) - e.g., "typescript"
      // => limit: number (positive, ≤ 100) - e.g., 20
      return { tag, limit, results: [] };
    }),

  // => transform: compute derived values before procedure logic
  createSlug: t.procedure
    .input(
      z.object({
        title: z.string().transform(
          (s) =>
            s
              .toLowerCase()
              .replace(/\s+/g, "-") // => Spaces → hyphens
              .replace(/[^a-z0-9-]/g, ""), // => Remove non-alphanumeric
          // => "Hello World!" → "hello-world"
        ),
      }),
    )
    .mutation((opts) => {
      const { title } = opts.input;
      // => title is already a slug: "hello-world"
      return { slug: title }; // => Ready for URL use
    }),
});

export type AppRouter = typeof appRouter;
export { appRouter };
```

**Key Takeaway**: `.transform()` converts input values; `.pipe()` applies additional validation after transform. Use these to normalize inputs before procedure logic runs.

**Why It Matters**: Input normalization at the schema level prevents inconsistent data entering your database. Search tags stored in mixed case (`TypeScript`, `typescript`, `TYPESCRIPT`) require case-insensitive queries everywhere—normalizing at input time eliminates this complexity. URL parameter handling (`?limit=20` arrives as a string) is another common use case. Transform-then-validate ensures the normalized data meets constraints before procedures run.

### Example 47: Discriminated Union Inputs

Procedures can accept multiple input shapes via discriminated unions, handling different operation types in one endpoint.

```typescript
import { initTRPC } from "@trpc/server";
import { z } from "zod";

const t = initTRPC.create();

// => Discriminated union input: different shapes based on 'action' field
const notificationActionSchema = z.discriminatedUnion("action", [
  z.object({
    action: z.literal("mark_read"),
    notificationId: z.string(),
    // => No other fields needed to mark as read
  }),
  z.object({
    action: z.literal("mark_all_read"),
    // => No specific ID - marks everything read
    userId: z.string(),
  }),
  z.object({
    action: z.literal("delete"),
    notificationId: z.string(),
    permanent: z.boolean().default(false), // => Soft delete by default
  }),
]);

const appRouter = t.router({
  // => Single endpoint handles multiple operation types
  updateNotification: t.procedure.input(notificationActionSchema).mutation((opts) => {
    const { input } = opts;
    // => TypeScript narrows based on 'action' discriminant

    if (input.action === "mark_read") {
      // => TypeScript knows: input has notificationId (no userId, no permanent)
      return { updated: 1, id: input.notificationId };
    } else if (input.action === "mark_all_read") {
      // => TypeScript knows: input has userId (no notificationId)
      return { updated: 100, userId: input.userId };
    } else {
      // => TypeScript knows: input has notificationId and permanent
      return {
        deleted: 1,
        id: input.notificationId,
        permanent: input.permanent,
        // => permanent: false (default) unless caller specifies true
      };
    }
  }),
});

export type AppRouter = typeof appRouter;
export { appRouter };
```

**Key Takeaway**: `z.discriminatedUnion("action", [...])` creates a union where the `action` field determines the shape. TypeScript narrows to the correct variant in each branch.

**Why It Matters**: Discriminated unions enable flexible endpoints that consolidate related operations. Without them, you'd need three separate procedures (`markRead`, `markAllRead`, `deleteNotification`). With discriminated union inputs, one procedure handles all variants with full type safety in each branch. This pattern is common for batch operations, undo/redo systems, and command pattern APIs.

## Group 8: Middleware Patterns

### Example 48: Tenant Isolation Middleware

Multi-tenant applications use middleware to scope all data access to the current tenant.

```typescript
import { initTRPC, TRPCError } from "@trpc/server";
import { z } from "zod";

interface BaseContext {
  headers: Record<string, string>;
}

interface TenantContext extends BaseContext {
  tenantId: string;
  tenantPlan: "free" | "pro" | "enterprise";
}

const t = initTRPC.context<BaseContext>().create();

// => Simulated tenant lookup
const tenants: Record<string, TenantContext["tenantPlan"]> = {
  "tenant-acme": "enterprise",
  "tenant-startup": "pro",
  "tenant-demo": "free",
};

// => Tenant resolution middleware: reads tenant ID from headers
const withTenant = t.middleware((opts) => {
  const tenantId = opts.ctx.headers["x-tenant-id"]; // => Custom header for tenant
  if (!tenantId) {
    throw new TRPCError({ code: "BAD_REQUEST", message: "Missing tenant ID" });
  }

  const tenantPlan = tenants[tenantId];
  if (!tenantPlan) {
    throw new TRPCError({ code: "NOT_FOUND", message: "Unknown tenant" });
  }

  return opts.next({
    ctx: { ...opts.ctx, tenantId, tenantPlan } as TenantContext,
    // => All downstream code has access to tenantId and tenantPlan
  });
});

// => Feature gating middleware: checks plan level
const requiresPro = t.middleware((opts) => {
  const { tenantPlan } = opts.ctx as TenantContext;
  if (tenantPlan === "free") {
    throw new TRPCError({
      code: "FORBIDDEN",
      message: "This feature requires a Pro or Enterprise plan",
    });
  }
  return opts.next(); // => pro and enterprise tenants proceed
});

const tenantProcedure = t.procedure.use(withTenant);
const proProcedure = tenantProcedure.use(requiresPro);

const appRouter = t.router({
  // => Basic procedure: all tenants
  getItems: tenantProcedure.query((opts) => {
    const { tenantId } = opts.ctx as TenantContext;
    return { tenantId, items: [] }; // => Scoped to tenant
  }),

  // => Pro feature: only pro/enterprise
  exportData: proProcedure.input(z.object({ format: z.enum(["csv", "json"]) })).mutation((opts) => {
    const { tenantId } = opts.ctx as TenantContext;
    return { tenantId, format: opts.input.format, url: "https://..." };
  }),
});

export type AppRouter = typeof appRouter;
export { appRouter };
```

**Key Takeaway**: Chain tenant resolution and feature gating middleware for multi-tenant APIs. Each layer adds constraints that downstream procedures depend on.

**Why It Matters**: Multi-tenancy is the dominant architecture for B2B SaaS. Tenant isolation middleware ensures every database query includes `WHERE tenant_id = ?` without manually adding it to each procedure. Feature gating based on subscription plan is another universal requirement—middleware handles both concerns declaratively. The alternative—checking tenant and plan in every procedure—is error-prone and creates security vulnerabilities when a new procedure misses the check.

### Example 49: Audit Logging Middleware

Record all write operations for compliance and debugging.

```typescript
import { initTRPC } from "@trpc/server";
import { z } from "zod";

interface Context {
  userId: string;
  sessionId: string;
}

const t = initTRPC.context<Context>().create();

// => Audit log entry type
interface AuditEntry {
  timestamp: string;
  userId: string;
  sessionId: string;
  procedure: string;
  type: "query" | "mutation" | "subscription";
  input: unknown;
  success: boolean;
  durationMs: number;
}

// => In-memory audit log (production: write to database or append-only log)
const auditLog: AuditEntry[] = [];

// => Audit middleware: records every mutation (not queries - too verbose)
const auditMiddleware = t.middleware(async (opts) => {
  const { type, path, ctx } = opts;

  // => Only audit mutations (queries are too frequent for audit logs)
  if (type !== "mutation") {
    return opts.next(); // => Pass through queries and subscriptions unlogged
  }

  const start = Date.now();
  const result = await opts.next(); // => Execute the mutation

  const entry: AuditEntry = {
    timestamp: new Date().toISOString(),
    userId: ctx.userId,
    sessionId: ctx.sessionId,
    procedure: path, // => e.g., "users.delete"
    type,
    input: opts.getRawInput(), // => Capture the actual input for audit trail
    // => getRawInput(): returns input before Zod parsing
    success: result.ok, // => true if no error thrown
    durationMs: Date.now() - start,
  };

  auditLog.push(entry); // => Append to audit log
  return result; // => Return original result unchanged
});

const auditedProcedure = t.procedure.use(auditMiddleware);

const appRouter = t.router({
  deleteUser: auditedProcedure.input(z.object({ userId: z.string() })).mutation((opts) => {
    // => This mutation's input and outcome are automatically logged
    return { deleted: true, userId: opts.input.userId };
  }),

  getAuditLog: t.procedure.query(() => auditLog), // => Admin endpoint to read logs
});

export type AppRouter = typeof appRouter;
export { appRouter };
```

**Key Takeaway**: Audit middleware uses `type !== "mutation"` to selectively log only write operations. `opts.getRawInput()` captures the pre-validation input for complete audit records.

**Why It Matters**: Compliance requirements (GDPR, SOC 2, HIPAA) mandate audit trails for data modifications. Middleware-based auditing captures every mutation automatically—new procedures are audited from the moment they use `auditedProcedure`. Manual audit logging in each procedure is error-prone and easily forgotten. The `getRawInput()` approach captures what was actually sent, not just what passed validation, which is valuable for security investigations.

## Group 9: Practical Patterns

### Example 50: Cursor-Based Pagination on the Server

Implement the server-side cursor pagination pattern that pairs with `useInfiniteQuery`.

```typescript
import { initTRPC } from "@trpc/server";
import { z } from "zod";

const t = initTRPC.create();

// => Simulated posts database
const allPosts = Array.from({ length: 50 }, (_, i) => ({
  id: i + 1,
  title: `Post ${i + 1}`,
  excerpt: `Excerpt for post ${i + 1}`,
  createdAt: new Date(Date.now() - i * 86400000).toISOString(),
  // => Each post is 1 day older than the previous
}));

const appRouter = t.router({
  // => Cursor-based pagination: uses last item's ID as cursor
  listPosts: t.procedure
    .input(
      z.object({
        limit: z.number().min(1).max(50).default(10),
        // => cursor: the ID of the last item from the previous page
        // => undefined on the first page (start from beginning)
        cursor: z.number().optional(),
      }),
    )
    .query((opts) => {
      const { limit, cursor } = opts.input;

      // => Find starting index: after the cursor item
      const startIndex = cursor
        ? allPosts.findIndex((p) => p.id === cursor) + 1
        : // => Start AFTER the cursor item (next page)
          0;
      // => cursor is undefined: start from beginning (first page)

      // => Fetch one extra item to detect if there's a next page
      const items = allPosts.slice(startIndex, startIndex + limit + 1);
      // => Fetch limit+1 items: extra item indicates "there are more"

      let nextCursor: number | undefined;
      if (items.length > limit) {
        // => Extra item exists: more pages available
        const nextItem = items.pop(); // => Remove extra item from results
        nextCursor = nextItem?.id; // => Use its ID as the next cursor
        // => Client passes this cursor to get the next page
      }
      // => nextCursor: undefined when no more pages (last page)

      return {
        posts: items, // => Exactly 'limit' items (or fewer on last page)
        nextCursor, // => Passed to getNextPageParam in useInfiniteQuery
      };
    }),
});

export type AppRouter = typeof appRouter;
export { appRouter };
```

**Key Takeaway**: Cursor pagination fetches `limit + 1` items. If the extra item exists, there are more pages and the extra item's ID becomes `nextCursor`. Return `undefined` when on the last page.

**Why It Matters**: Offset pagination (`OFFSET 10 LIMIT 10`) degrades at scale—the database scans all skipped rows. Cursor pagination (`WHERE id > lastId LIMIT 10`) uses the indexed primary key, maintaining consistent performance at any page depth. The `limit + 1` pattern is elegant—instead of a separate COUNT query to check for more pages, you fetch one extra item. All production infinite scroll implementations use this cursor pattern.

### Example 51: Procedure Composition with Input Merging

Extend existing procedures with additional inputs using TypeScript's type system.

```typescript
import { initTRPC } from "@trpc/server";
import { z } from "zod";

const t = initTRPC.create();

// => Base pagination input: reusable across many procedures
const paginationInput = z.object({
  page: z.number().min(1).default(1), // => 1-indexed page number
  limit: z.number().min(1).max(100).default(20), // => Items per page
});

// => Base filter input: common filtering parameters
const filterInput = z.object({
  search: z.string().optional(), // => Text search query
  sortBy: z.enum(["name", "date", "relevance"]).default("date"),
  sortOrder: z.enum(["asc", "desc"]).default("desc"),
});

// => Merge schemas: combine reusable schemas into procedure-specific schemas
const usersListInput = paginationInput
  .merge(filterInput) // => Combine both base schemas
  .merge(
    z.object({
      role: z.enum(["admin", "member", "guest"]).optional(),
      // => Users-specific filter: role
    }),
  );
// => usersListInput: { page, limit, search?, sortBy, sortOrder, role? }

const appRouter = t.router({
  listUsers: t.procedure.input(usersListInput).query((opts) => {
    const { page, limit, search, sortBy, sortOrder, role } = opts.input;
    // => All fields typed and validated
    return {
      users: [], // => Would apply all filters in production
      pagination: {
        page, // => e.g., 1
        limit, // => e.g., 20
        total: 0, // => Total count from database
      },
    };
  }),

  // => Reuse pagination without filters
  listAuditLogs: t.procedure
    .input(paginationInput) // => Just pagination, no filters
    .query((opts) => {
      return { logs: [], page: opts.input.page, limit: opts.input.limit };
    }),
});

export type AppRouter = typeof appRouter;
export { appRouter };
```

**Key Takeaway**: Use `z.merge()` to combine reusable Zod schemas. Define pagination, filtering, and sorting schemas once, then compose them per procedure.

**Why It Matters**: Schema composition eliminates copy-paste across procedures. A change to the pagination schema (e.g., adding `cursor` for migration to cursor-based pagination) updates all merged schemas simultaneously. Large applications have dozens of list procedures sharing the same pagination and filter patterns. Schema composition keeps them consistent without maintenance overhead.

### Example 52: Conditional Procedure Logic Based on Context

Procedures can conditionally return different amounts of data based on context (user role, subscription tier).

```typescript
import { initTRPC } from "@trpc/server";
import { z } from "zod";

interface Context {
  userId: string | null;
  userRole: "admin" | "member" | "guest";
  subscriptionTier: "free" | "pro" | "enterprise";
}

const t = initTRPC.context<Context>().create();

const appRouter = t.router({
  // => Same procedure, different data based on caller's role/tier
  getReport: t.procedure.input(z.object({ reportId: z.string() })).query((opts) => {
    const { userId, userRole, subscriptionTier } = opts.ctx;
    const { reportId } = opts.input;

    // => Base data: everyone gets this
    const baseReport = {
      id: reportId,
      title: "Q1 2026 Analysis",
      summary: "Brief summary visible to all users",
    };

    // => Extended data: authenticated users only
    if (!userId) {
      return { ...baseReport, type: "preview" as const }; // => Unauthenticated preview
    }

    // => Pro features: pro and enterprise tiers
    const proData =
      subscriptionTier !== "free"
        ? {
            fullData: [{ metric: "revenue", value: 100000 }], // => Full dataset
            exportAvailable: true,
          }
        : {
            fullData: null, // => Hidden for free tier
            exportAvailable: false,
          };

    // => Admin extras: admin role only
    const adminData = userRole === "admin" ? { internalNotes: "Confidential admin notes", rawSql: "SELECT..." } : {};

    return {
      ...baseReport,
      ...proData,
      ...adminData,
      type: "full" as const,
    };
  }),
});

export type AppRouter = typeof appRouter;
export { appRouter };
```

**Key Takeaway**: Procedures use context fields to conditionally include data. Spread operators merge conditional data cleanly. Use `as const` on discriminant fields for TypeScript narrowing.

**Why It Matters**: Progressive data disclosure based on access level is a common SaaS pattern. Rather than separate endpoints (each requiring auth checks), one endpoint returns appropriate data for each caller. This approach is easier to test (mock different context values), easier to audit (one procedure to review), and reduces API surface area. The TypeScript `as const` on the `type` field enables client-side narrowing to differentiate preview vs full reports.

### Example 53: Procedure Middleware for Caching Hints

Middleware can set cache-control headers to enable HTTP caching for public procedures.

```typescript
import { initTRPC } from "@trpc/server";

// => Context carries the response object for header manipulation
interface Context {
  res?: {
    setHeader: (name: string, value: string) => void;
  };
}

const t = initTRPC.context<Context>().create();

// => Cache hint middleware factory: configurable TTL
function withCacheHints(maxAgeSeconds: number) {
  return t.middleware((opts) => {
    // => Set Cache-Control header BEFORE procedure runs
    opts.ctx.res?.setHeader(
      "Cache-Control",
      `public, max-age=${maxAgeSeconds}, stale-while-revalidate=${maxAgeSeconds * 2}`,
      // => max-age: serve from cache for this many seconds
      // => stale-while-revalidate: serve stale while fetching fresh in background
    );
    return opts.next(); // => Execute the actual procedure
  });
}

// => Procedure bases with different cache TTLs
const cachedShortProcedure = t.procedure.use(withCacheHints(60)); // => 1 minute
const cachedLongProcedure = t.procedure.use(withCacheHints(3600)); // => 1 hour

const appRouter = t.router({
  // => Short cache: data changes frequently (notifications, counts)
  getNotificationCount: cachedShortProcedure.query(() => ({
    count: 5, // => Cached for 60 seconds
  })),

  // => Long cache: data changes rarely (country list, categories)
  listCountries: cachedLongProcedure.query(() => ({
    countries: [
      { code: "ID", name: "Indonesia" },
      { code: "MY", name: "Malaysia" },
    ],
    // => Cached for 1 hour - countries rarely change
  })),
});

export type AppRouter = typeof appRouter;
export { appRouter };
```

**Key Takeaway**: Pass response objects through context to set headers in middleware. Cache-control headers enable CDN and browser caching for public procedures.

**Why It Matters**: HTTP caching is the most effective performance optimization available. Country lists, category trees, and configuration data that changes rarely can be cached at the CDN level, eliminating server load entirely for these requests. Middleware-based cache hints centralize the caching policy. Without this, developers must remember to set cache headers in every procedure—middleware ensures consistency.

### Example 54: Batch Mutations

Process multiple items in a single mutation to reduce round trips.

```typescript
import { initTRPC, TRPCError } from "@trpc/server";
import { z } from "zod";

const t = initTRPC.create();

// => Individual item result type
type BatchResult<T> = { success: true; item: T } | { success: false; error: string };

const appRouter = t.router({
  // => Batch create: process multiple items, return per-item results
  batchCreateTags: t.procedure
    .input(
      z.object({
        tags: z
          .array(
            z.object({
              name: z.string().min(1).max(50),
              color: z.string().regex(/^#[0-9a-fA-F]{6}$/),
            }),
          )
          .max(50), // => Max 50 tags per batch
      }),
    )
    .mutation(async (opts) => {
      const { tags } = opts.input;
      // => Process each tag independently, collecting results
      const results: BatchResult<{ id: number; name: string; color: string }>[] = await Promise.all(
        tags.map(async (tag) => {
          try {
            // => Simulate per-item processing (DB insert in production)
            await new Promise((r) => setTimeout(r, 1)); // => Simulate async work
            const id = Math.floor(Math.random() * 1000);
            return { success: true as const, item: { id, ...tag } };
            // => success: true, item contains created tag with ID
          } catch (err) {
            return {
              success: false as const,
              error: err instanceof Error ? err.message : "Unknown error",
            };
            // => Per-item failure: other items still succeed
          }
        }),
      );

      const succeeded = results.filter((r) => r.success).length;
      const failed = results.filter((r) => !r.success).length;
      // => succeeded: count of successfully created tags
      // => failed: count of failed tags (partial success allowed)

      if (succeeded === 0) {
        throw new TRPCError({
          code: "INTERNAL_SERVER_ERROR",
          message: "All batch items failed",
        });
      }

      return { results, succeeded, failed }; // => Partial success is fine
    }),
});

export type AppRouter = typeof appRouter;
export { appRouter };
```

**Key Takeaway**: Batch mutations use `Promise.all()` for parallel processing and return per-item results. Allow partial success rather than failing the entire batch on one error.

**Why It Matters**: Batch operations reduce HTTP overhead and improve throughput. Creating 50 tags with 50 individual mutations wastes 49 network round trips. A batch mutation creates all 50 in one request. The per-item result pattern is critical for user experience—if 49 tags succeed and 1 fails, the user should see which one failed and retry it, not lose all 49 successful creations.

### Example 55: Health Check and Readiness Procedures

Production APIs need health check endpoints for load balancers and monitoring.

```typescript
import { initTRPC } from "@trpc/server";

interface Context {
  db: {
    query: (sql: string) => Promise<unknown>;
  };
}

const t = initTRPC.context<Context>().create();

// => Health check result type
interface ServiceStatus {
  status: "healthy" | "degraded" | "unhealthy";
  latencyMs: number;
}

async function checkDatabase(db: Context["db"]): Promise<ServiceStatus> {
  const start = Date.now();
  try {
    await db.query("SELECT 1"); // => Minimal query to verify DB connectivity
    return { status: "healthy", latencyMs: Date.now() - start };
    // => healthy: DB responded within acceptable time
  } catch {
    return { status: "unhealthy", latencyMs: Date.now() - start };
    // => unhealthy: DB unreachable or query failed
  }
}

const appRouter = t.router({
  // => Liveness check: is the process alive?
  health: t.procedure.query(() => ({
    status: "ok" as const,
    timestamp: new Date().toISOString(),
    version: process.env.APP_VERSION ?? "unknown",
    // => APP_VERSION: set during deployment to track which version is running
  })),

  // => Readiness check: is the process ready to serve traffic?
  ready: t.procedure.query(async (opts) => {
    // => Check all dependencies
    const [database] = await Promise.all([
      checkDatabase(opts.ctx.db),
      // => Add more checks: cache, external APIs, etc.
    ]);

    // => Overall status: worst case of all checks
    const overallStatus =
      database.status === "unhealthy" ? "unhealthy" : database.status === "degraded" ? "degraded" : "healthy";

    return {
      status: overallStatus, // => "healthy" | "degraded" | "unhealthy"
      checks: { database }, // => Individual service statuses
      timestamp: new Date().toISOString(),
    };
  }),
});

export type AppRouter = typeof appRouter;
export { appRouter };
```

**Key Takeaway**: Implement separate `health` (liveness) and `ready` (readiness) endpoints. Health checks verify process liveness; readiness checks verify all dependencies are available.

**Why It Matters**: Kubernetes and load balancers require health check endpoints to manage traffic routing. Liveness probes restart unresponsive pods; readiness probes remove pods from load balancer rotation during startup or dependency failures. Using tRPC procedures for health checks ensures they go through the same auth and middleware stack, making them realistic indicators of actual request handling capability.
