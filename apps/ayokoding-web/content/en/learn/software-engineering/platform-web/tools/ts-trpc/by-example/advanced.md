---
title: "Advanced"
weight: 10000003
date: 2026-03-25T00:00:00+07:00
draft: false
description: "Master expert tRPC patterns through 25 annotated examples covering custom links, WebSocket transport, createCaller testing, integration testing, Next.js App Router, performance optimization, type inference utilities, and migration patterns"
tags: ["trpc", "typescript", "api", "tutorial", "by-example", "advanced"]
---

This advanced tutorial covers expert tRPC patterns through 25 heavily annotated examples. Each example maintains 1-2.25 comment lines per code line. Examples assume mastery of beginner and intermediate concepts—you should be comfortable with procedures, middleware, context, and React Query integration before starting.

## Prerequisites

Before starting, ensure you understand:

- All beginner and intermediate tRPC concepts (Examples 1-55)
- Advanced TypeScript (conditional types, mapped types, template literals)
- Node.js streams and event systems
- Testing patterns (unit, integration)

## Group 1: Custom Links and Transport

### Example 56: Custom Terminating Link

Links are middleware in the HTTP transport layer. A custom terminating link controls how requests reach the server.

```typescript
// customLink.ts
import type { TRPCLink } from "@trpc/client";
import { observable } from "@trpc/server/observable";
// => observable: creates event stream for link responses
import type { AppRouter } from "../server/router";

// => TRPCLink<AppRouter>: typed link bound to your router
// => Links form a chain; the last one (terminating link) sends the request
export const customFetchLink: TRPCLink<AppRouter> = () => {
  // => The link factory returns an operation handler function
  return ({ op }) => {
    // => op: { type, path, input, id } - the outgoing operation
    // => type: "query" | "mutation" | "subscription"
    // => path: "users.getById" (procedure path)

    return observable((observer) => {
      // => Build the request URL from operation path
      const url = `http://localhost:3000/trpc/${op.path}`;

      // => Custom headers: add auth, trace IDs, etc.
      const headers: Record<string, string> = {
        "Content-Type": "application/json",
        "X-Request-ID": crypto.randomUUID(), // => Unique trace ID per request
        "X-Client-Version": "2.0.0", // => Client version for server-side analytics
      };

      // => Execute the fetch with custom configuration
      fetch(url, {
        method: op.type === "query" ? "GET" : "POST",
        // => Queries use GET (cacheable), mutations use POST (side effects)
        headers,
        body: op.type !== "query" ? JSON.stringify({ input: op.input }) : undefined,
        // => Only send body for mutations and subscriptions
      })
        .then((res) => res.json())
        .then((data) => {
          observer.next({ result: { type: "data", data } });
          // => observer.next(): pass successful response downstream
          observer.complete(); // => Signal no more data
        })
        .catch((err) => {
          observer.error(err); // => Pass error downstream (to onError handlers)
        });

      // => Return cleanup function (called if subscription is cancelled)
      return () => {
        // => Could abort the fetch here with AbortController
      };
    });
  };
};
```

**Key Takeaway**: Custom links are factories returning observable-based handlers. They intercept operations before they reach the server, enabling custom transport logic.

**Why It Matters**: Custom terminating links are essential for environments where standard HTTP fetch does not suffice. React Native apps need custom certificate pinning. Embedded systems use proprietary transports. Offline-capable apps route to IndexedDB when offline and the network when connected. The observable pattern makes links composable—a retry link wraps a logging link wraps your custom link, each layer adding behavior.

### Example 57: Retry Link with Exponential Backoff

Non-terminating links intercept operations and can retry on failure before passing to the terminating link.

```typescript
// retryLink.ts
import type { TRPCLink } from "@trpc/client";
import { observable } from "@trpc/server/observable";
import type { AppRouter } from "../server/router";

// => Configuration for the retry behavior
interface RetryConfig {
  maxRetries: number; // => Maximum number of retry attempts
  baseDelayMs: number; // => Base delay in ms (doubled each retry)
  shouldRetry: (error: unknown, attempt: number) => boolean;
  // => Callback: return true to retry, false to give up
}

// => Creates a non-terminating retry link
export function createRetryLink(config: RetryConfig): TRPCLink<AppRouter> {
  return () => {
    return ({ op, next }) => {
      // => next: passes the operation to the next link in chain
      return observable((observer) => {
        let attempt = 0; // => Track current attempt number

        // => Recursive retry function
        const tryOnce = () => {
          // => Call next link (eventually reaches terminating link)
          const sub = next(op).subscribe({
            next: (value) => {
              observer.next(value); // => Success: pass result downstream
              observer.complete();
            },
            error: (err) => {
              attempt++;
              // => Check if we should retry
              if (attempt <= config.maxRetries && config.shouldRetry(err, attempt)) {
                const delay = config.baseDelayMs * Math.pow(2, attempt - 1);
                // => Exponential backoff: 100ms, 200ms, 400ms, 800ms...
                console.log(`Retry ${attempt}/${config.maxRetries} in ${delay}ms`);
                setTimeout(tryOnce, delay); // => Schedule retry after backoff delay
              } else {
                observer.error(err); // => Give up: pass error downstream
              }
            },
            complete: () => observer.complete(),
          });

          // => Return cleanup: cancel active request if link is disposed
          return () => sub.unsubscribe();
        };

        return tryOnce(); // => Start first attempt
      });
    };
  };
}

// => Usage in client setup:
// => trpc.createClient({
// =>   links: [
// =>     createRetryLink({
// =>       maxRetries: 3,
// =>       baseDelayMs: 100,
// =>       shouldRetry: (err, attempt) =>
// =>         err instanceof TRPCClientError && err.data?.code === 'INTERNAL_SERVER_ERROR'
// =>     }),
// =>     httpBatchLink({ url: 'http://localhost:3000/trpc' })
// =>   ]
// => });
```

**Key Takeaway**: Non-terminating links intercept operations and call `next(op)` to continue the chain. Retry logic wraps `next()` in a recursive function with delay between attempts.

**Why It Matters**: Network reliability is imperfect. Transient server errors, cold starts, and brief network interruptions are facts of production life. Automatic retry with exponential backoff improves perceived reliability without user intervention. The `shouldRetry` callback enables intelligent decisions: retry server errors but not client errors (`BAD_REQUEST`), retry up to 3 times for critical mutations but immediately fail for real-time subscriptions.

### Example 58: Logging Link for Request Inspection

A logging link instruments all requests for debugging and monitoring without modifying business logic.

```typescript
// loggingLink.ts
import type { TRPCLink } from "@trpc/client";
import { observable } from "@trpc/server/observable";
import type { AppRouter } from "../server/router";

// => Log entry structure for structured logging
interface RequestLog {
  requestId: string;
  type: string;
  path: string;
  startTime: number;
  endTime?: number;
  durationMs?: number;
  success?: boolean;
  errorCode?: string;
}

// => In-memory log (production: send to logging service)
const requestLogs: RequestLog[] = [];

export const loggingLink: TRPCLink<AppRouter> = () => {
  return ({ op, next }) => {
    return observable((observer) => {
      const requestId = crypto.randomUUID();
      // => Unique ID to correlate request/response log entries

      // => Create initial log entry BEFORE the request fires
      const log: RequestLog = {
        requestId,
        type: op.type, // => "query" | "mutation" | "subscription"
        path: op.path, // => e.g., "users.getById"
        startTime: performance.now(), // => High-precision start time
      };
      requestLogs.push(log); // => Record pending request

      console.log(`→ [${requestId.slice(0, 8)}] ${op.type} ${op.path}`);
      // => Example: → [a1b2c3d4] query users.getById

      return next(op).subscribe({
        next: (value) => {
          // => Request succeeded: update log with timing
          log.endTime = performance.now();
          log.durationMs = log.endTime - log.startTime;
          log.success = true;
          console.log(`✓ [${requestId.slice(0, 8)}] ${op.path} ${log.durationMs.toFixed(1)}ms`);
          // => Example: ✓ [a1b2c3d4] users.getById 12.3ms
          observer.next(value); // => Pass value unchanged
        },
        error: (err) => {
          // => Request failed: log error details
          log.endTime = performance.now();
          log.durationMs = log.endTime - log.startTime;
          log.success = false;
          log.errorCode = (err as { data?: { code?: string } }).data?.code;
          console.error(`✗ [${requestId.slice(0, 8)}] ${op.path} ${log.durationMs?.toFixed(1)}ms - ${log.errorCode}`);
          // => Example: ✗ [a1b2c3d4] users.getById 5.2ms - NOT_FOUND
          observer.error(err); // => Pass error unchanged
        },
        complete: () => observer.complete(),
      });
    });
  };
};

export { requestLogs }; // => Export for debugging/testing
```

**Key Takeaway**: Logging links intercept all operations transparently. Log before `next()` for request timing; log in `next.subscribe` callbacks for response details.

**Why It Matters**: Request-level observability is non-negotiable in production. Logging links capture timing, error rates, and path-level metrics without polluting procedure code. The structured log format enables log aggregation tools (Elasticsearch, Loki) to answer "what percentage of `users.getById` calls failed in the last hour?" Distributed tracing systems (Datadog, Honeycomb) use the `requestId` to correlate frontend logs with backend spans.

## Group 2: WebSocket Transport

### Example 59: WebSocket Link Setup for Subscriptions

WebSocket transport enables real-time subscriptions over persistent connections.

```typescript
// wsClient.ts
import { createWSClient, wsLink, splitLink, httpBatchLink } from "@trpc/client";
import { createTRPCReact } from "@trpc/react-query";
import type { AppRouter } from "../server/router";

// => Create typed tRPC React hooks
export const trpc = createTRPCReact<AppRouter>();

// => Create WebSocket client for subscription transport
const wsClient = createWSClient({
  url: "ws://localhost:3000/trpc", // => WebSocket server URL (ws:// or wss://)
  // => wss:// for production (TLS-encrypted WebSocket)

  onOpen: () => {
    console.log("WebSocket connected"); // => Connection established
  },
  onClose: () => {
    console.log("WebSocket disconnected"); // => Connection closed (cleanup, reconnect)
  },

  // => Reconnection: automatically reconnect on disconnection
  retryDelayMs: (attemptIndex) => Math.min(1000 * 2 ** attemptIndex, 30000),
  // => Exponential backoff: 1s, 2s, 4s, 8s, ..., max 30s between reconnects
});

// => tRPC client with split transport:
// => Queries and mutations → HTTP batch link
// => Subscriptions → WebSocket link
export const trpcClient = trpc.createClient({
  links: [
    splitLink({
      condition: (op) => op.type === "subscription",
      // => true: subscriptions go to WebSocket
      true: wsLink({ client: wsClient }),
      // => false: queries and mutations go to HTTP
      false: httpBatchLink({ url: "http://localhost:3000/trpc" }),
    }),
  ],
});

// => Usage in React component:
// => const { data } = trpc.messages.onNew.useSubscription(
// =>   { roomId: "room-1" },
// =>   { onData: (msg) => setMessages(prev => [...prev, msg]) }
// => );
```

**Key Takeaway**: Use `splitLink` to route subscriptions to `wsLink` and queries/mutations to `httpBatchLink`. One `createWSClient` instance manages reconnection automatically.

**Why It Matters**: Subscriptions require a persistent connection—HTTP's request-response model does not support server-push. WebSocket provides bidirectional communication over a single long-lived connection. The `splitLink` pattern is the canonical approach: don't use WebSocket for queries and mutations (they don't need persistence and HTTP caching would not work), only for subscriptions. Production WebSocket deployments use `wss://` with certificate validation and authenticate during the WebSocket handshake.

### Example 60: useSubscription React Hook

The `useSubscription` hook connects React components to real-time tRPC subscription procedures.

```typescript
// LiveChat.tsx
import { useState, useCallback } from "react";
import { trpc } from "./wsClient";

type Message = { id: string; text: string; userId: string; at: string };

function LiveChat({ roomId }: { roomId: string }) {
  const [messages, setMessages] = useState<Message[]>([]);
  const [connectionStatus, setConnectionStatus] = useState<"connecting" | "connected" | "error">(
    "connecting"
  );

  // => useSubscription: connects to a subscription procedure via WebSocket
  trpc.chat.onMessage.useSubscription(
    { roomId }, // => Input: typed from chat.onMessage procedure's input schema
    {
      // => onData: called each time the server emits an event
      onData: useCallback((message: Message) => {
        setMessages((prev) => [...prev, message]);
        // => Append new message to list; useCallback prevents re-subscription
      }, []),
      // => onError: called when subscription encounters an error
      onError: (err) => {
        console.error("Subscription error:", err.message);
        setConnectionStatus("error"); // => Update UI to show disconnected state
      },
      // => enabled: control when subscription is active
      enabled: roomId !== "", // => Don't subscribe without a valid roomId
    }
  );

  // => Mutation: send a message to the room
  const sendMessage = trpc.chat.sendMessage.useMutation();

  return (
    <div>
      <div>Status: {connectionStatus}</div>
      <div>
        {messages.map((msg) => (
          <div key={msg.id}>
            <strong>{msg.userId}</strong>: {msg.text}
            <span>{new Date(msg.at).toLocaleTimeString()}</span>
          </div>
        ))}
      </div>
      <button
        onClick={() =>
          sendMessage.mutate({ roomId, text: "Hello!" })
        }
      >
        Send
      </button>
    </div>
  );
}

export { LiveChat };
```

**Key Takeaway**: `useSubscription` accepts `onData` and `onError` callbacks. Use `useCallback` for `onData` to prevent unnecessary re-subscriptions when the parent re-renders.

**Why It Matters**: Real-time chat, live notifications, collaborative editing, and live dashboards all require subscription hooks. The `enabled` option is critical—subscribing before a room ID is available would cause errors or subscribe to the wrong room. `useCallback` on `onData` is a performance requirement: without it, React re-creates the function on every render, causing the subscription to tear down and re-establish repeatedly.

## Group 3: Testing

### Example 61: Unit Testing with createCaller

`createCaller` creates a server-side procedure caller for unit testing without HTTP overhead.

```typescript
// users.test.ts
import { describe, it, expect, beforeEach } from "vitest";
// => vitest: fast Vite-native testing framework (or use Jest equivalents)
import { appRouter } from "../server/router";
import type { AppRouter } from "../server/router";

// => Test context factory: creates mock context for testing
function createTestContext(
  overrides?: Partial<{
    userId: string | null;
    role: "admin" | "member";
  }>,
) {
  return {
    userId: overrides?.userId ?? "test-user-1", // => Default authenticated user
    role: overrides?.role ?? "member", // => Default member role
    db: {
      // => Mock database: returns predictable test data
      users: {
        findMany: async () => [{ id: 1, name: "Test User", email: "test@example.com" }],
        findById: async (id: number) => (id === 1 ? { id: 1, name: "Test User", email: "test@example.com" } : null),
        create: async (data: { name: string; email: string }) => ({
          id: 42,
          ...data,
          createdAt: "2026-03-25T00:00:00.000Z",
        }),
      },
    },
  };
}

describe("users router", () => {
  let caller: ReturnType<typeof appRouter.createCaller>;
  // => caller: typed caller bound to appRouter - full TypeScript inference

  beforeEach(() => {
    // => Create fresh caller for each test with default context
    caller = appRouter.createCaller(createTestContext());
    // => caller.users.getById({ id: 1 }) calls the actual procedure logic
    // => No HTTP, no serialization - direct function call
  });

  it("returns user by ID", async () => {
    const user = await caller.users.getById({ id: 1 });
    // => Calls getById procedure directly with context from createTestContext()
    expect(user).toEqual({
      id: 1,
      name: "Test User",
      email: "test@example.com",
    });
  });

  it("throws NOT_FOUND for missing user", async () => {
    await expect(caller.users.getById({ id: 999 })).rejects.toMatchObject({
      code: "NOT_FOUND", // => TRPCError.code matches
    });
  });

  it("admin can list all users", async () => {
    // => Override context to simulate admin
    const adminCaller = appRouter.createCaller(createTestContext({ role: "admin" }));
    const users = await adminCaller.users.list();
    expect(users).toHaveLength(1); // => Mock returns 1 user
  });
});
```

**Key Takeaway**: `createCaller(context)` creates a typed server-side caller. Tests call procedures directly with mock contexts, testing business logic without HTTP overhead.

**Why It Matters**: Fast, reliable unit tests are the foundation of confident development. `createCaller` tests the actual procedure logic—middleware, validation, business rules—without spinning up an HTTP server. Tests run in milliseconds rather than seconds. The context overrides pattern enables testing all permission scenarios (anonymous, member, admin) concisely. Every tRPC production codebase should have comprehensive `createCaller` tests for all procedures.

### Example 62: Integration Testing with HTTP

Integration tests verify the full request stack including HTTP adapters, middleware chains, and error formatting.

```typescript
// integration.test.ts
import { describe, it, expect, beforeAll, afterAll } from "vitest";
import { createHTTPServer } from "@trpc/server/adapters/standalone";
// => createHTTPServer: creates a Node.js HTTP server with tRPC adapter
import { appRouter } from "../server/router";
import { createContext } from "../server/context";
import { createTRPCProxyClient, httpBatchLink } from "@trpc/client";
import type { AppRouter } from "../server/router";

// => Server and client for integration tests
let server: ReturnType<typeof createHTTPServer>;
let client: ReturnType<typeof createTRPCProxyClient<AppRouter>>;
let baseUrl: string;

beforeAll(async () => {
  // => Start real HTTP server on a random port
  server = createHTTPServer({
    router: appRouter,
    createContext, // => Real context creation function
  });

  await new Promise<void>((resolve) => {
    server.listen(0, () => {
      // => Port 0: OS assigns random available port
      const addr = server.server.address();
      const port = typeof addr === "object" ? addr?.port : 0;
      baseUrl = `http://localhost:${port}`; // => e.g., http://localhost:54321
      resolve();
    });
  });

  // => Create typed proxy client pointing to test server
  client = createTRPCProxyClient<AppRouter>({
    links: [httpBatchLink({ url: `${baseUrl}/trpc` })],
  });
  // => client.users.getById({ id: 1 }) sends real HTTP to test server
});

afterAll(() => {
  server.server.close(); // => Stop test server after all tests complete
});

describe("integration: users", () => {
  it("GET users.getById sends real HTTP request", async () => {
    const user = await client.users.getById.query({ id: 1 });
    // => Sends actual HTTP GET to test server
    // => Full middleware chain, auth, error handling all execute
    expect(user.name).toBe("Test User");
  });

  it("validates input and returns BAD_REQUEST", async () => {
    await expect(
      client.users.getById.query({ id: -1 }), // => Invalid ID (negative)
    ).rejects.toMatchObject({
      data: { code: "BAD_REQUEST" }, // => Zod validation failed
    });
  });
});
```

**Key Takeaway**: Integration tests use `createHTTPServer` to start a real server on a random port. Tests send actual HTTP requests through the full middleware stack.

**Why It Matters**: Unit tests with `createCaller` miss HTTP-layer issues: serialization bugs, header handling, CORS configuration, and HTTP adapter behavior. Integration tests catch these issues before production. Random port assignment (`0`) prevents test flakiness from port conflicts. Running integration tests against a real server also validates that tRPC error formatting, superjson serialization, and HTTP status codes all work correctly end-to-end.

### Example 63: Mocking Context for Authorization Tests

Comprehensive authorization testing requires simulating different context states systematically.

```typescript
// auth.test.ts
import { describe, it, expect } from "vitest";
import { TRPCError } from "@trpc/server";
import { appRouter } from "../server/router";

// => Context factory with role presets for clean test organization
const contexts = {
  anonymous: () => ({ userId: null, role: null as null, permissions: [] as string[] }),
  member: () => ({ userId: "member-1", role: "member" as const, permissions: ["read"] }),
  admin: () => ({
    userId: "admin-1",
    role: "admin" as const,
    permissions: ["read", "write", "delete"],
  }),
};

describe("authorization matrix", () => {
  // => Test each permission level against protected procedures
  it("anonymous cannot access protected procedure", async () => {
    const caller = appRouter.createCaller(contexts.anonymous());
    await expect(caller.admin.getDashboard()).rejects.toMatchObject({
      code: "UNAUTHORIZED", // => Unauthenticated → UNAUTHORIZED (not FORBIDDEN)
    });
  });

  it("member cannot access admin procedure", async () => {
    const caller = appRouter.createCaller(contexts.member());
    await expect(caller.admin.getDashboard()).rejects.toMatchObject({
      code: "FORBIDDEN", // => Authenticated but wrong role → FORBIDDEN
    });
  });

  it("admin can access admin procedure", async () => {
    const caller = appRouter.createCaller(contexts.admin());
    const result = await caller.admin.getDashboard();
    // => Should succeed: no error thrown
    expect(result).toBeDefined(); // => Returned data exists
  });

  it("member can access public procedure", async () => {
    const caller = appRouter.createCaller(contexts.member());
    const result = await caller.public.getStatus();
    expect(result.status).toBe("ok"); // => Public endpoint accessible to all
  });
});

// => Test helper: verifies a procedure throws a specific TRPCError code
async function expectTRPCError(promise: Promise<unknown>, expectedCode: TRPCError["code"]) {
  await expect(promise).rejects.toMatchObject({ code: expectedCode });
  // => Combines assertion into one readable helper
}

export { expectTRPCError }; // => Reuse across test files
```

**Key Takeaway**: Define context presets for each permission level. Test the full authorization matrix: anonymous, authenticated member, and admin against each procedure.

**Why It Matters**: Authorization regressions are costly security vulnerabilities. A comprehensive test matrix ensures every protected procedure correctly rejects unauthorized access. The `UNAUTHORIZED` vs `FORBIDDEN` distinction matters—clients redirect to login for `UNAUTHORIZED` and show a "not permitted" message for `FORBIDDEN`. The `expectTRPCError` helper reduces test verbosity, making authorization test suites readable. Every permission level change on any procedure should be covered by an authorization test.

## Group 4: Next.js App Router Integration

### Example 64: tRPC Route Handler for App Router

Next.js App Router uses Route Handlers instead of Pages Router's API routes. Set up tRPC to work with them.

```typescript
// app/api/trpc/[trpc]/route.ts
import { fetchRequestHandler } from "@trpc/server/adapters/fetch";
// => fetchRequestHandler: handles tRPC over Fetch API (used by App Router)
// => Replaces createNextApiHandler (which used Node.js req/res)
import { appRouter } from "../../../../server/router";
import { createContext } from "../../../../server/context";

// => App Router Route Handler: handles all tRPC requests
// => GET: queries, POST: mutations and batched requests
const handler = (req: Request) =>
  fetchRequestHandler({
    endpoint: "/api/trpc", // => Must match the route path
    req, // => Fetch API Request (not Node.js IncomingMessage)
    router: appRouter,
    createContext: () => createContext(), // => Create context from Fetch Request
    // => In production: createContext receives req to extract headers, cookies
    onError:
      process.env.NODE_ENV === "development"
        ? ({ path, error }) => {
            console.error(`tRPC error on ${path}:`, error);
            // => Development: log errors to console for debugging
          }
        : undefined,
    // => Production: errors handled by Sentry/error tracking middleware
  });

// => Export both GET and POST handlers for App Router
export { handler as GET, handler as POST };
// => GET: queries (uses URL params for input)
// => POST: mutations and batched queries (uses request body)
```

**Key Takeaway**: App Router uses `fetchRequestHandler` with the Fetch API instead of `createNextApiHandler`. Export both `GET` and `POST` handlers from the route file.

**Why It Matters**: The migration from Pages Router API routes to App Router Route Handlers is a common Next.js upgrade task. The key difference is `fetchRequestHandler` works with the standard Fetch API `Request` object, while the older adapter used Node.js `IncomingMessage`. This makes tRPC deployable to Edge Runtime environments (Cloudflare Workers, Vercel Edge Functions) that only support the Fetch API, not Node.js APIs.

### Example 65: Server Actions with tRPC Validation

Combine Next.js Server Actions with tRPC's Zod validation for type-safe form submissions.

```typescript
// app/posts/actions.ts
"use server";
// => "use server": marks this module as Server Actions (App Router)

import { z } from "zod";
import { TRPCError } from "@trpc/server";
import { appRouter } from "../../server/router";
import { createContext } from "../../server/context";
import { revalidatePath } from "next/cache";
// => revalidatePath: invalidates Next.js cache for a URL path

// => Input schema: reused for both Server Action validation and tRPC procedure
const createPostSchema = z.object({
  title: z.string().min(1).max(200),
  content: z.string().min(1),
  published: z.boolean().default(false),
});

// => Server Action: called from Client Components via form submission
export async function createPostAction(formData: FormData) {
  // => Parse and validate form data with Zod
  const parseResult = createPostSchema.safeParse({
    title: formData.get("title"), // => Extract form field
    content: formData.get("content"),
    published: formData.get("published") === "true",
  });

  if (!parseResult.success) {
    // => Validation failed: return structured error for client
    return {
      success: false as const,
      errors: parseResult.error.flatten().fieldErrors,
      // => fieldErrors: { title: ["Required"], content: ["Too short"] }
    };
  }

  try {
    // => Call tRPC procedure through createCaller (no HTTP overhead)
    const caller = appRouter.createCaller(await createContext());
    const post = await caller.posts.create(parseResult.data);
    // => Full tRPC middleware chain runs: auth, validation, business logic

    // => Invalidate the posts list page cache
    revalidatePath("/posts"); // => Next.js will re-render /posts on next visit
    return { success: true as const, postId: post.id };
  } catch (err) {
    if (err instanceof TRPCError) {
      return { success: false as const, errors: { _root: [err.message] } };
    }
    return { success: false as const, errors: { _root: ["Unknown error"] } };
  }
}
```

**Key Takeaway**: Server Actions can use `createCaller` to invoke tRPC procedures directly, reusing validation, auth, and business logic while integrating with Next.js cache revalidation.

**Why It Matters**: Server Actions and tRPC serve overlapping but distinct purposes. Server Actions handle form submissions with progressive enhancement (work without JavaScript). tRPC handles React Query-powered data fetching and mutations. Using `createCaller` in Server Actions bridges both worlds: reuse your tRPC procedure logic without duplicating business rules or validation schemas. `revalidatePath` ensures the page reflects the mutation without a full client-side cache invalidation.

### Example 66: Shared Types Between Server and Client in Next.js

Monorepo-style type sharing enables end-to-end typing across separate packages.

```typescript
// packages/api/src/router.ts (shared package)
import { initTRPC } from "@trpc/server";
import { z } from "zod";

const t = initTRPC.create();

// => Domain types: defined once, used in both frontend and backend
export const postSchema = z.object({
  id: z.number(),
  title: z.string(),
  content: z.string(),
  authorId: z.string(),
  publishedAt: z.string().nullable(),
  tags: z.array(z.string()),
});

export type Post = z.infer<typeof postSchema>;
// => Post: { id: number; title: string; content: string; authorId: string; publishedAt: string | null; tags: string[] }

export const createPostInput = z.object({
  title: z.string().min(1).max(200),
  content: z.string(),
  tags: z.array(z.string()).default([]),
});

export type CreatePostInput = z.infer<typeof createPostInput>;
// => CreatePostInput: { title: string; content: string; tags: string[] }

export const appRouter = t.router({
  posts: t.router({
    list: t.procedure
      .output(z.array(postSchema)) // => Explicit output validates return type
      .query((): Post[] => [
        {
          id: 1,
          title: "Hello",
          content: "World",
          authorId: "user-1",
          publishedAt: null,
          tags: ["intro"],
        },
      ]),

    create: t.procedure
      .input(createPostInput)
      .output(postSchema)
      .mutation(
        (opts): Post => ({
          id: Date.now(),
          ...opts.input,
          authorId: "user-1",
          publishedAt: null,
        }),
      ),
  }),
});

export type AppRouter = typeof appRouter;
// => Export: AppRouter, Post, CreatePostInput, postSchema, createPostInput
// => Frontend imports types; backend imports the router value
```

**Key Takeaway**: Export Zod schemas, inferred types, and the `AppRouter` type from a shared package. Both client and server import from the same source—no duplication, no drift.

**Why It Matters**: In a monorepo with separate frontend and backend packages, type sharing is the foundation of type safety. Without it, teams manually duplicate types and they drift over time. With shared schemas, the `Post` type is defined once in `packages/api`—the frontend form, API handler, and database mapper all reference the same type. Renaming a field triggers TypeScript errors everywhere simultaneously. This is the "T3 Stack" pattern used by thousands of production Next.js applications.

## Group 5: Performance Optimization

### Example 67: Response Transformer with Superjson

Superjson extends JSON serialization to support Date, Map, Set, BigInt, and undefined.

```typescript
// server setup with superjson transformer
import { initTRPC } from "@trpc/server";
import superjson from "superjson";
// => superjson: transforms non-JSON types for safe serialization/deserialization

// => transformer: applied to all procedure inputs and outputs
const t = initTRPC.create({
  transformer: superjson,
  // => Serialization: Date → { json: "2026-03-25T...", meta: { values: ["Date"] } }
  // => Deserialization: reconstructed back to Date on the client
});

const appRouter = t.router({
  // => These types would fail with standard JSON.stringify()
  getComplexData: t.procedure.query(() => {
    return {
      // => Date: serialized as ISO string, deserialized as Date object
      createdAt: new Date("2026-03-25"), // => Client receives Date, not string
      // => Map: serialized as entries array, deserialized as Map
      settings: new Map([
        ["theme", "dark"], // => Client receives Map, not plain object
        ["language", "en"],
      ]),
      // => Set: serialized as array, deserialized as Set
      tags: new Set(["typescript", "trpc"]), // => Client receives Set, not array
      // => BigInt: serialized as string, deserialized as BigInt
      largeId: BigInt("9007199254740993"), // => Client receives BigInt
      // => undefined preserved (JSON.stringify drops undefined)
      optionalField: undefined, // => Client receives undefined, not missing key
    };
  }),
});

export type AppRouter = typeof appRouter;
export { appRouter };

// => Client MUST use same transformer:
// => trpc.createClient({
// =>   links: [httpBatchLink({ url: '...', transformer: superjson })]
// => })
// => Without matching transformer, Dates arrive as strings on the client
```

**Key Takeaway**: Add `transformer: superjson` to both `initTRPC.create()` and `httpBatchLink` configuration. Mismatched transformers between client and server cause deserialization bugs.

**Why It Matters**: Standard JSON loses type information: `new Date()` serializes to a string and stays a string on the client. Production applications need `Date` objects for date math, `Map` for key-value storage, and `BigInt` for large IDs (database row IDs often exceed JavaScript's `Number.MAX_SAFE_INTEGER`). Superjson handles all these transparently—procedures return native TypeScript types; clients receive native TypeScript types. The transformer configuration is a one-time setup that benefits the entire application.

### Example 68: Deferred Queries with `enabled` Flag

Control when queries fire with the `enabled` option to prevent waterfall fetches and premature requests.

```typescript
// DeferredQueries.tsx
import { useState } from "react";
import { trpc } from "./trpc";

// => Two-step fetch: user first, then user's posts (dependent queries)
function UserPostsView() {
  const [userId, setUserId] = useState<number | null>(null);
  // => userId: null until user selects from dropdown

  // => Step 1: Fetch user list - always enabled
  const { data: users } = trpc.users.list.useQuery();
  // => Fires immediately on component mount

  // => Step 2: Fetch posts - only when userId is selected
  const { data: posts, isLoading: postsLoading } = trpc.posts.byUser.useQuery(
    { userId: userId! }, // => Non-null assertion: safe because enabled checks it
    {
      enabled: userId !== null,
      // => enabled: false when userId is null - query does NOT fire
      // => enabled: true when userId is set - query fires automatically
      // => Without enabled: query would fire with userId=null → BAD_REQUEST
    }
  );

  return (
    <div>
      <select
        onChange={(e) => setUserId(parseInt(e.target.value) || null)}
        defaultValue=""
      >
        <option value="">Select a user...</option>
        {users?.map((u) => (
          <option key={u.id} value={u.id}>{u.name}</option>
        ))}
      </select>

      {userId && postsLoading && <div>Loading posts...</div>}
      {/* => Shows loading only after a user is selected */}

      {posts?.map((post) => (
        <div key={post.id}>{post.title}</div>
        // => Renders when posts arrive (after userId is set and query completes)
      ))}
    </div>
  );
}

export { UserPostsView };
```

**Key Takeaway**: Use `enabled: condition` to defer queries until prerequisites are met. The non-null assertion (`userId!`) is safe because `enabled` prevents the query from firing when `userId` is null.

**Why It Matters**: Dependent queries are ubiquitous in real applications: show posts after selecting a user, show addresses after selecting a country, show variants after selecting a product. Without `enabled`, the query fires with `null` input, triggering a validation error. The `enabled` pattern is the React Query way to express query dependencies—cleaner than conditional `useQuery` calls (which violate Rules of Hooks) and more explicit than polling.

### Example 69: Prefetching on Hover

Prefetch query data when users hover over links to reduce perceived navigation latency.

```typescript
// PrefetchLink.tsx
import { useCallback } from "react";
import { trpc } from "./trpc";
import { Link } from "react-router-dom";

// => Component: link that prefetches tRPC data on hover
function UserLink({ userId, name }: { userId: number; name: string }) {
  const utils = trpc.useUtils();
  // => utils: React Query utilities for cache manipulation

  // => Prefetch on hover: load data before the user clicks
  const handleMouseEnter = useCallback(async () => {
    // => prefetch: fetches data and stores in React Query cache
    // => Does NOT cause a re-render (unlike useQuery)
    await utils.users.getById.prefetch({ id: userId });
    // => Cache populated: when user navigates to /users/123, useQuery returns
    // => immediately from cache (no loading state)
    await utils.posts.byUser.prefetch({ userId });
    // => Prefetch related posts too: navigation renders both instantly
  }, [userId, utils]);

  return (
    <Link
      to={`/users/${userId}`}
      onMouseEnter={handleMouseEnter}
      // => Hover triggers prefetch; typical hover-to-click is 100-300ms
      // => Network request completes in that window (typical LAN/CDN: 20-50ms)
    >
      {name}
    </Link>
  );
}

export { UserLink };
```

**Key Takeaway**: `utils.[procedure].prefetch()` populates the React Query cache without triggering a re-render. Hover-based prefetching fills the cache before users click.

**Why It Matters**: Human hover-to-click time is typically 100-300ms. Network requests on good connections complete in 20-100ms. Hover-based prefetching exploits this gap to pre-warm the cache before navigation. Users experience instant page transitions instead of loading spinners. This technique is used extensively by modern web applications—Next.js's `<Link>` component uses viewport intersection for similar prefetching. Combined with tRPC's React Query integration, it requires minimal code for maximum impact.

## Group 6: Type Inference Utilities

### Example 70: Advanced Type Inference Patterns

tRPC's type utilities enable sophisticated TypeScript patterns for procedure type extraction.

```typescript
// typeUtils.ts
import type { inferRouterInputs, inferRouterOutputs } from "@trpc/server";
import type { AppRouter } from "../server/router";

// => Extract all procedure input/output types from the router
type RouterIn = inferRouterInputs<AppRouter>;
type RouterOut = inferRouterOutputs<AppRouter>;

// => Deep access: navigate nested router paths
type UserGetByIdInput = RouterIn["users"]["getById"];
// => { id: number } - exact input type for users.getById

type UserGetByIdOutput = RouterOut["users"]["getById"];
// => { id: number; name: string; email: string } - exact output type

// => Conditional type: check if a procedure exists in the router
type HasPostsCreate = "create" extends keyof RouterIn["posts"] ? true : false;
// => true: posts.create exists in the router

// => Utility: create a type-safe form submission handler type
type FormHandler<TPath extends keyof RouterIn> = {
  onSubmit: (data: RouterIn[TPath]) => Promise<RouterOut[TPath]>;
  defaultValues: Partial<RouterIn[TPath]>;
};

// => Example: typed form handler for user creation
type CreateUserFormHandler = FormHandler<"users">;
// => { onSubmit: (data: RouterIn["users"]) => Promise<RouterOut["users"]>; defaultValues: Partial<RouterIn["users"]> }

// => Template literal types: generate typed procedure paths
type QueryPaths = {
  [K in keyof RouterOut]: RouterOut[K] extends (...args: unknown[]) => unknown ? never : K;
}[keyof RouterOut];
// => Extracts all query procedure keys from the router

// => Mapped type: create loading state flags for all procedures
type LoadingStates = {
  [K in keyof RouterOut as `${string & K}Loading`]: boolean;
};
// => { usersLoading: boolean; postsLoading: boolean; ... }

type _unused = UserGetByIdInput | UserGetByIdOutput | HasPostsCreate | QueryPaths | LoadingStates; // => Suppress unused warning
export type { RouterIn, RouterOut, UserGetByIdInput, UserGetByIdOutput };
```

**Key Takeaway**: `inferRouterInputs` and `inferRouterOutputs` enable advanced TypeScript patterns: conditional types, mapped types, and template literal types built from procedure signatures.

**Why It Matters**: Type inference utilities are the foundation of tRPC's developer experience. Form libraries use `RouterIn` types for field definitions. State management stores use `RouterOut` types for cached data shapes. Generic utilities like `FormHandler<TPath>` work for any procedure without manual type duplication. In large codebases, these utilities eliminate hundreds of manually maintained types, preventing the type drift that makes maintenance expensive.

### Example 71: Procedure Type Guards and Narrowing

Use TypeScript's type system to create compile-time guarantees about procedure behavior.

```typescript
// typeGuards.ts
import type { TRPCError } from "@trpc/server";
import type { TRPCClientError } from "@trpc/client";
import type { AppRouter } from "../server/router";

// => Type guard: check if an error is a TRPCClientError with specific code
function isTRPCError(error: unknown, code?: TRPCError["code"]): error is TRPCClientError<AppRouter> {
  // => error is TRPCClientError: cast succeeds only after this guard returns true
  if (!(error instanceof Error)) return false;
  // => Must be an Error instance

  if (!("data" in error)) return false;
  // => TRPCClientError has a 'data' property

  if (code) {
    const trpcError = error as TRPCClientError<AppRouter>;
    return trpcError.data?.code === code;
    // => Check specific error code if provided
  }

  return true; // => Is a TRPCClientError but no specific code required
}

// => Usage in React error handlers:
// => catch (err) {
// =>   if (isTRPCError(err, 'UNAUTHORIZED')) {
// =>     router.push('/login');
// =>     // TypeScript knows: err is TRPCClientError<AppRouter>
// =>   } else if (isTRPCError(err, 'NOT_FOUND')) {
// =>     router.push('/404');
// =>   } else {
// =>     throw err; // Unknown error: re-throw
// =>   }
// => }

// => Type predicate for nullable procedure outputs
function isDefined<T>(value: T | null | undefined): value is T {
  return value !== null && value !== undefined;
  // => Narrows T | null | undefined → T
}

// => Usage with tRPC query results:
// => const { data: users } = trpc.users.list.useQuery();
// => const activeUsers = users?.filter(isDefined) ?? [];
// => TypeScript: activeUsers is T[] (not (T | null | undefined)[])

export { isTRPCError, isDefined };
```

**Key Takeaway**: Type guards for `TRPCClientError` enable type-safe error handling in catch blocks. The `isDefined` predicate narrows nullable arrays cleanly.

**Why It Matters**: Error handling without type guards requires `as` casts throughout. With `isTRPCError`, TypeScript narrows the error type in each branch—accessing `.data.code` is safe because the guard verified it. The `isDefined` predicate is universally useful for filtering nullable arrays that come from optional fields in procedure outputs. Both patterns reduce `as` casts, which are suppressions of TypeScript's safety guarantees.

## Group 7: Migration and Interop

### Example 72: Gradual Migration from REST to tRPC

Migrate an existing REST API to tRPC incrementally without breaking existing clients.

```typescript
// server.ts - hybrid REST + tRPC server
import express from "express";
import { createExpressMiddleware } from "@trpc/server/adapters/express";
// => createExpressMiddleware: mounts tRPC on an Express app
import { initTRPC } from "@trpc/server";
import { z } from "zod";

const app = express();
app.use(express.json());

// => EXISTING REST routes: keep working for legacy clients
// => Legacy mobile apps, third-party integrations, or gradual migration
app.get("/api/v1/users", (req, res) => {
  // => Old REST endpoint: still functions, not migrated yet
  res.json([{ id: 1, name: "Legacy user (REST)" }]);
});

app.post("/api/v1/users", (req, res) => {
  // => Old REST mutation: kept for backward compatibility
  const { name, email } = req.body;
  res.status(201).json({ id: 2, name, email });
});

// => NEW tRPC routes: new features added here; migrate old REST here gradually
const t = initTRPC.create();
const appRouter = t.router({
  users: t.router({
    // => New version of GET /api/v1/users - with validation, typing, middleware
    list: t.procedure.query(() => [{ id: 1, name: "New user (tRPC)", email: "user@example.com" }]),

    // => New feature: not available in REST API
    search: t.procedure
      .input(z.object({ query: z.string() }))
      .query((opts) => [{ id: 1, name: `Result for: ${opts.input.query}` }]),
  }),
});

export type AppRouter = typeof appRouter;

// => Mount tRPC at /trpc: coexists with REST routes
app.use(
  "/trpc",
  createExpressMiddleware({
    router: appRouter,
    createContext: () => ({}),
  }),
);
// => REST requests: /api/v1/users → Express routes
// => tRPC requests: /trpc/users.list → tRPC router

app.listen(3000, () => console.log("Hybrid server on :3000"));
```

**Key Takeaway**: Mount tRPC middleware alongside existing REST routes in Express. Both coexist on the same server—migrate procedures incrementally from REST to tRPC.

**Why It Matters**: Big-bang API rewrites fail. Gradual migration is safer: add tRPC for new features, migrate REST endpoints one by one, keep existing clients working throughout. New frontend features use tRPC from day one; old mobile apps continue hitting REST endpoints until they update. The Express adapter makes this coexistence trivial. After full migration, remove the old REST routes. This pattern is how most teams adopt tRPC in established codebases.

### Example 73: Calling tRPC from Non-React Clients

tRPC procedures are callable from vanilla TypeScript, CLI tools, and non-React frameworks.

```typescript
// vanillaClient.ts
import { createTRPCProxyClient, httpBatchLink } from "@trpc/client";
import superjson from "superjson";
import type { AppRouter } from "../server/router";

// => createTRPCProxyClient: vanilla TypeScript client without React Query
// => Use this for: scripts, CLI tools, non-React frontends, server-to-server
const client = createTRPCProxyClient<AppRouter>({
  links: [
    httpBatchLink({
      url: "http://localhost:3000/trpc",
      transformer: superjson,
      // => Headers function: adds auth token to all requests
      headers: () => ({
        authorization: `Bearer ${process.env.SERVICE_TOKEN ?? ""}`,
        // => Service token: machine-to-machine auth (not user token)
      }),
    }),
  ],
});

// => Proxy client: same type-safe API as React hooks, but Promise-based
async function runMigrationScript() {
  // => Query: returns typed data directly (not wrapped in { data, isLoading })
  const users = await client.users.list.query();
  // => users: User[] (full TypeScript inference)
  console.log(`Found ${users.length} users`);

  // => Mutation: executes write operation, returns typed result
  for (const user of users) {
    const updated = await client.users.update.mutate({
      id: user.id,
      name: user.name.trim(), // => Normalize whitespace
    });
    console.log(`Updated user ${updated.id}: ${updated.name}`);
  }
}

// => Export for use in scripts, tests, or other modules
export { client };
export { runMigrationScript };
```

**Key Takeaway**: `createTRPCProxyClient` provides a Promise-based tRPC client for vanilla TypeScript. Use it for scripts, CLI tools, and server-to-server communication.

**Why It Matters**: Not everything is a React component. Data migration scripts, cron jobs, admin CLI tools, and microservice communication all need to call tRPC procedures. The proxy client gives the same type safety and IntelliSense as React Query hooks, just without the caching layer. Server-to-server tRPC calls are particularly powerful in microservices—services communicate through typed procedure calls rather than undocumented REST endpoints.

### Example 74: OpenAPI Compatibility Layer

Generate OpenAPI specs from tRPC routers to support legacy clients and documentation.

```typescript
// openapi-adapter.ts
// => Note: requires @trpc/openapi package
// => npm install @trpc/openapi
import { initTRPC } from "@trpc/server";
import { z } from "zod";

// => @trpc/openapi extends initTRPC to add OpenAPI metadata to procedures
// => In production: import { initTRPC } from '@trpc/openapi'
const t = initTRPC.create();

// => When using @trpc/openapi, procedures get an extra .meta() chainable
// => The meta defines the REST endpoint mapping
const appRouter = t.router({
  // => With @trpc/openapi:
  // => getUser: t.procedure
  // =>   .meta({ openapi: { method: 'GET', path: '/users/{id}' } })
  // =>   .input(z.object({ id: z.number() }))
  // =>   .output(z.object({ id: z.number(), name: z.string() }))
  // =>   .query(...)

  // => Without @trpc/openapi (standard tRPC):
  getUser: t.procedure
    .input(z.object({ id: z.number() }))
    .output(z.object({ id: z.number(), name: z.string(), email: z.string() }))
    .query((opts) => ({
      id: opts.input.id,
      name: "Aisha Rahman",
      email: "aisha@example.com",
    })),
});

export type AppRouter = typeof appRouter;
export { appRouter };

// => With @trpc/openapi configured, generateOpenApiDocument() produces:
// => {
// =>   openapi: "3.0.3",
// =>   info: { title: "My API", version: "1.0.0" },
// =>   paths: {
// =>     "/users/{id}": {
// =>       get: {
// =>         parameters: [{ name: "id", in: "path", required: true, schema: { type: "integer" } }],
// =>         responses: { "200": { content: { "application/json": { schema: { ... } } } } }
// =>       }
// =>     }
// =>   }
// => }
// =>
// => This enables:
// => - Swagger UI documentation for your tRPC API
// => - REST clients (curl, Postman) to call tRPC endpoints
// => - Contract testing with existing REST test suites
// => - Third-party integrations expecting REST APIs
```

**Key Takeaway**: `@trpc/openapi` generates OpenAPI 3.0 specs from tRPC procedures, enabling REST client compatibility and Swagger UI documentation.

**Why It Matters**: Enterprise integrations frequently require OpenAPI specs. Partners, third-party tools, and legacy systems expect REST APIs. `@trpc/openapi` bridges both worlds: TypeScript-first tRPC development with an automatically generated OpenAPI spec for external consumption. You write procedures once; internal TypeScript clients use the tRPC client; external REST clients use the generated OpenAPI endpoints. Swagger UI documentation is auto-generated from the same source.

## Group 8: Expert Patterns

### Example 75: Procedure Factories for DRY CRUD

Create typed procedure factories that generate CRUD operations for any entity.

```typescript
import { initTRPC, TRPCError } from "@trpc/server";
import { z, type ZodType, type ZodObject, type ZodRawShape } from "zod";

const t = initTRPC.create();

// => Generic CRUD factory: creates standard list/getById/create/update/delete procedures
// => TEntity: the entity type; TCreateInput: input for creation
function createCrudRouter<TShape extends ZodRawShape, TEntity extends z.infer<ZodObject<TShape>>>(options: {
  entityName: string; // => For error messages: "User", "Post", etc.
  schema: ZodObject<TShape>; // => Zod schema for the entity
  createInput: ZodType; // => Schema for create input (subset of entity)
  store: Map<number, TEntity>; // => In-memory store (production: DB repository)
}) {
  const { entityName, schema, createInput, store } = options;

  return t.router({
    // => list: returns all entities
    list: t.procedure.output(z.array(schema)).query(() => Array.from(store.values())),
    // => Returns all values from the store as an array

    // => getById: returns single entity by ID
    getById: t.procedure
      .input(z.object({ id: z.number() }))
      .output(schema)
      .query((opts) => {
        const entity = store.get(opts.input.id);
        if (!entity) {
          throw new TRPCError({
            code: "NOT_FOUND",
            message: `${entityName} ${opts.input.id} not found`,
            // => Error message uses entityName: "User 42 not found"
          });
        }
        return entity; // => Found: return typed entity
      }),

    // => create: adds new entity to store
    create: t.procedure
      .input(createInput)
      .output(schema)
      .mutation((opts) => {
        const id = (store.size || 0) + 1; // => Auto-increment ID
        const entity = { id, ...opts.input } as TEntity;
        store.set(id, entity); // => Store in map
        return entity; // => Return created entity with ID
      }),
  });
}

// => Instantiate CRUD routers for each entity
const userSchema = z.object({ id: z.number(), name: z.string(), email: z.string() });
const userStore = new Map<number, z.infer<typeof userSchema>>();

const tagSchema = z.object({ id: z.number(), name: z.string(), color: z.string() });
const tagStore = new Map<number, z.infer<typeof tagSchema>>();

const appRouter = t.router({
  users: createCrudRouter({
    entityName: "User",
    schema: userSchema,
    createInput: userSchema.omit({ id: true }), // => Create input: name + email (no ID)
    store: userStore,
  }),
  tags: createCrudRouter({
    entityName: "Tag",
    schema: tagSchema,
    createInput: tagSchema.omit({ id: true }), // => Create input: name + color (no ID)
    store: tagStore,
  }),
});

export type AppRouter = typeof appRouter;
export { appRouter };
```

**Key Takeaway**: Procedure factories generate typed routers for any entity. Pass entity schemas, stores, and names to produce consistent CRUD APIs without duplication.

**Why It Matters**: Most applications have 5-20 CRUD entities. Without factories, each entity's list/getById/create/update/delete is duplicated with slight variations—a maintenance nightmare. Factories encode the CRUD pattern once, applied consistently to every entity. Adding audit logging, soft delete, or tenant scoping to the factory instantly applies to all entities. TypeScript generics ensure the factory is fully typed—`users.getById` returns `User`, `tags.getById` returns `Tag`, without any manual type annotations.

### Example 76: Event-Driven Architecture with tRPC Subscriptions

Build an event-driven system where mutations publish events consumed by subscriptions.

```typescript
import { initTRPC } from "@trpc/server";
import { observable } from "@trpc/server/observable";
import { z } from "zod";
import { EventEmitter } from "events";

const t = initTRPC.create();

// => Central event bus: mutations publish, subscriptions consume
// => EventEmitter: Node.js built-in, no external dependencies
const eventBus = new EventEmitter();
eventBus.setMaxListeners(100); // => Allow up to 100 concurrent subscribers
// => Default is 10: increase for production with many subscription clients

// => Type-safe event map: defines all events and their payloads
interface AppEvents {
  "post.created": { id: number; title: string; authorId: string };
  "post.deleted": { id: number; authorId: string };
  "user.updated": { id: string; name: string };
}

// => Type-safe emit helper: validates event name and payload
function emit<TEvent extends keyof AppEvents>(event: TEvent, payload: AppEvents[TEvent]) {
  eventBus.emit(event, payload); // => Broadcasts to all listeners for this event
}

// => Type-safe subscribe helper: creates observable from EventEmitter
function subscribe<TEvent extends keyof AppEvents>(event: TEvent) {
  return observable<AppEvents[TEvent]>((observer) => {
    const handler = (data: AppEvents[TEvent]) => observer.next(data);
    // => handler: receives event payload, forwards to tRPC subscriber

    eventBus.on(event, handler); // => Register listener
    return () => eventBus.off(event, handler); // => Cleanup: deregister on disconnect
  });
}

const appRouter = t.router({
  posts: t.router({
    // => Mutation: creates post AND emits event
    create: t.procedure.input(z.object({ title: z.string() })).mutation((opts) => {
      const post = { id: Date.now(), ...opts.input, authorId: "user-1" };
      emit("post.created", post); // => Broadcast to all subscribers
      // => All active onCreated subscriptions receive this event
      return post;
    }),

    // => Subscription: receives post creation events in real-time
    onCreated: t.procedure.subscription(() => subscribe("post.created")),
    // => Returns observable that emits { id, title, authorId } for each new post
  }),
});

export type AppRouter = typeof appRouter;
export { appRouter };
```

**Key Takeaway**: Use a central `EventEmitter` as the bridge between mutations and subscriptions. Type-safe `emit` and `subscribe` helpers prevent event name typos and payload mismatches.

**Why It Matters**: Event-driven architecture decouples mutations from their side effects. The `posts.create` mutation emits an event without knowing who is subscribed—real-time UI updates, notification triggers, search index updates, and analytics could all listen without modifying the mutation. The type-safe event map prevents entire categories of bugs: if you change `post.created` payload, TypeScript flags every subscriber that references the old shape. Production systems scale this with Redis Pub/Sub for multi-server subscriptions.

### Example 77: Middleware for Request Context Enrichment

Enrich request context with computed data that would be expensive to recalculate in each procedure.

```typescript
import { initTRPC } from "@trpc/server";
import { z } from "zod";

interface BaseContext {
  headers: Record<string, string | undefined>;
  userId: string | null;
}

interface EnrichedContext extends BaseContext {
  // => Computed once in middleware, available everywhere
  locale: string; // => Parsed from Accept-Language header
  timezone: string; // => Parsed from X-Timezone header
  featureFlags: Set<string>; // => User's enabled feature flags
  requestMetadata: {
    userAgent: string;
    ip: string;
    isBot: boolean; // => Computed from user agent
  };
}

const t = initTRPC.context<BaseContext>().create();

// => Context enrichment middleware: computes derived data once per request
const enrichContext = t.middleware(async (opts) => {
  const { headers, userId } = opts.ctx;

  // => Parse locale from Accept-Language header
  const acceptLanguage = headers["accept-language"] ?? "en-US";
  const locale = acceptLanguage.split(",")[0]?.trim() ?? "en-US";
  // => "en-US,en;q=0.9" → "en-US"

  // => Parse timezone header
  const timezone = headers["x-timezone"] ?? "UTC";
  // => Client sends timezone: "Asia/Jakarta" for Indonesian users

  // => Fetch feature flags once per request (not once per procedure)
  const featureFlags = userId
    ? await fetchUserFeatureFlags(userId) // => DB/Redis lookup
    : new Set<string>(); // => No flags for anonymous users

  // => Parse user agent for bot detection
  const userAgent = headers["user-agent"] ?? "";
  const isBot = /bot|crawler|spider/i.test(userAgent);
  // => true for Googlebot, Bingbot, etc.

  return opts.next({
    ctx: {
      ...opts.ctx,
      locale, // => "en-US"
      timezone, // => "Asia/Jakarta"
      featureFlags, // => Set<string> - all enabled flags for this user
      requestMetadata: {
        userAgent,
        ip: headers["x-forwarded-for"]?.split(",")[0] ?? "unknown",
        isBot,
      },
    } as EnrichedContext,
  });
});

// => Mock feature flag lookup
async function fetchUserFeatureFlags(userId: string): Promise<Set<string>> {
  void userId; // => Suppress unused warning (would use userId in production)
  return new Set(["new-dashboard", "beta-search"]); // => Simulated flags
}

const enrichedProcedure = t.procedure.use(enrichContext);

const appRouter = t.router({
  search: enrichedProcedure.input(z.object({ query: z.string() })).query((opts) => {
    const { locale, featureFlags, requestMetadata } = opts.ctx as EnrichedContext;
    const useBetaSearch = featureFlags.has("beta-search");
    // => Feature flag check: true for users with "beta-search" flag

    return {
      results: [],
      locale, // => Used for result language filtering
      usedBetaSearch: useBetaSearch,
      isBot: requestMetadata.isBot, // => Bots get cached results, not personalized
    };
  }),
});

export type AppRouter = typeof appRouter;
export { appRouter };
```

**Key Takeaway**: Context enrichment middleware computes expensive derived values once per request. All procedures receive this data without redundant computation.

**Why It Matters**: Without enrichment middleware, feature flag checks would require a database lookup in each procedure—N database round trips for N procedures in a request batch. With enrichment middleware, one lookup serves all procedures in the batch. Locale parsing, feature flags, and permission pre-computation all follow this pattern. Production tRPC apps typically have an `enrichContext` middleware that runs before auth middleware, pre-loading data all subsequent layers depend on.

### Example 78: Type-Safe Error Boundaries with tRPC

Create React error boundaries that handle tRPC-specific errors with type safety.

```typescript
// TRPCErrorBoundary.tsx
import { Component, type ReactNode } from "react";
import { TRPCClientError } from "@trpc/client";
import type { AppRouter } from "../server/router";

interface Props {
  children: ReactNode;
  fallback?: (error: TRPCClientError<AppRouter>) => ReactNode;
  // => fallback: render function for tRPC-specific errors
  onError?: (error: TRPCClientError<AppRouter>) => void;
  // => onError: callback for logging, analytics
}

interface State {
  error: TRPCClientError<AppRouter> | null;
}

// => Class component: required for React error boundaries (no hook equivalent)
class TRPCErrorBoundary extends Component<Props, State> {
  state: State = { error: null };

  // => getDerivedStateFromError: called when a child throws
  static getDerivedStateFromError(error: unknown): State {
    if (error instanceof TRPCClientError) {
      // => Store TRPCClientError for typed access in render
      return { error: error as TRPCClientError<AppRouter> };
    }
    // => Non-tRPC errors: re-throw for outer error boundaries
    throw error;
  }

  componentDidCatch(error: unknown): void {
    if (error instanceof TRPCClientError) {
      this.props.onError?.(error as TRPCClientError<AppRouter>);
      // => e.g., send to Sentry: Sentry.captureException(error, { extra: { code: error.data?.code } })
    }
  }

  render() {
    const { error } = this.state;
    const { children, fallback } = this.props;

    if (error) {
      const code = error.data?.code;
      // => code: "NOT_FOUND" | "UNAUTHORIZED" | "FORBIDDEN" | etc.

      // => Handle specific codes with appropriate UI
      if (code === "UNAUTHORIZED") {
        return <div>Please sign in to view this content.</div>;
      }
      if (code === "FORBIDDEN") {
        return <div>You do not have permission to view this content.</div>;
      }
      if (code === "NOT_FOUND") {
        return <div>The requested resource was not found.</div>;
      }

      // => Custom fallback for other errors
      return fallback ? fallback(error) : <div>Something went wrong: {error.message}</div>;
    }

    return children;
  }
}

export { TRPCErrorBoundary };
```

**Key Takeaway**: Error boundaries wrapping tRPC queries catch `TRPCClientError` and render appropriate UI based on `error.data?.code`. Re-throw non-tRPC errors for outer boundaries.

**Why It Matters**: Error boundaries prevent entire pages from crashing when one query fails. The semantic error codes enable contextual UI: `UNAUTHORIZED` redirects to login, `NOT_FOUND` shows a 404 component, `FORBIDDEN` shows a permission message. Without type-safe error boundaries, every component needs try/catch logic for error states. Wrapping route segments with `<TRPCErrorBoundary>` handles errors uniformly across an entire page or feature area.

### Example 79: Monorepo Package Structure for tRPC

Organize a monorepo with a shared tRPC package consumed by web and mobile frontends.

```typescript
// packages/api/src/index.ts - shared tRPC package
// => This package is imported by: apps/web, apps/mobile, apps/admin
import { initTRPC, TRPCError } from "@trpc/server";
import { z } from "zod";

// => Shared context type: all consumers create context matching this shape
export interface ApiContext {
  userId: string | null;
  tenantId: string;
  // => Each app's createContext() must return a value matching this interface
}

const t = initTRPC.context<ApiContext>().create();

// => Shared procedure bases: exported for use in sub-packages
export const publicProcedure = t.procedure;
export const authedProcedure = t.procedure.use((opts) => {
  if (!opts.ctx.userId) {
    throw new TRPCError({ code: "UNAUTHORIZED" });
  }
  return opts.next({ ctx: { ...opts.ctx, userId: opts.ctx.userId } });
});

// => Shared router: the single source of truth for all API procedures
export const appRouter = t.router({
  // => Shared procedures: same API for all frontends
  health: publicProcedure.query(() => ({ status: "ok" as const })),

  profile: authedProcedure.input(z.object({ userId: z.string().optional() })).query((opts) => ({
    id: opts.ctx.userId!,
    name: "Shared User",
    tenantId: opts.ctx.tenantId,
  })),
});

// => Export everything consumers need
export type AppRouter = typeof appRouter;
// => apps/web: import { trpc } from '@myapp/api/react' (createTRPCReact<AppRouter>())
// => apps/mobile: import { client } from '@myapp/api/native' (createTRPCProxyClient<AppRouter>())
// => apps/admin: import { caller } from '@myapp/api/server' (appRouter.createCaller())
```

**Key Takeaway**: Centralize the router, context type, and procedure bases in a shared package. Each frontend app provides its own `createContext` implementation matching the shared `ApiContext` interface.

**Why It Matters**: Monorepo architecture with shared tRPC packages is the ultimate productivity pattern for multi-frontend applications. Web, mobile, and admin apps share identical procedure signatures—the web team and mobile team cannot accidentally diverge on API contracts. Adding a new procedure to the shared package immediately provides it to all consumers with full typing. Production examples include T3 Turbo and Create JD App, both using this exact pattern for full-stack TypeScript monorepos.

### Example 80: Performance Profiling and Optimization

Identify and fix tRPC performance bottlenecks using timing data and query analysis.

```typescript
import { initTRPC } from "@trpc/server";
import { z } from "zod";

const t = initTRPC.create();

// => Performance tracker: records timing per procedure call
interface PerfRecord {
  path: string;
  type: string;
  durationMs: number;
  timestamp: number;
  slow: boolean; // => true if exceeds threshold
}

const perfRecords: PerfRecord[] = [];
const SLOW_THRESHOLD_MS = 100; // => Procedures over 100ms are "slow"

// => Profiling middleware: wraps all procedures with timing
const profilingMiddleware = t.middleware(async (opts) => {
  const start = performance.now(); // => High-resolution timer
  const result = await opts.next(); // => Execute procedure
  const durationMs = performance.now() - start;

  const record: PerfRecord = {
    path: opts.path, // => e.g., "users.getById"
    type: opts.type, // => "query" | "mutation"
    durationMs: Math.round(durationMs * 100) / 100, // => Round to 2 decimal places
    timestamp: Date.now(),
    slow: durationMs > SLOW_THRESHOLD_MS,
    // => slow: true when duration exceeds 100ms threshold
  };

  perfRecords.push(record);

  if (record.slow) {
    // => Alert on slow procedures for investigation
    console.warn(`SLOW ${record.type} ${record.path}: ${record.durationMs}ms (threshold: ${SLOW_THRESHOLD_MS}ms)`);
    // => Example: SLOW query users.getById: 245ms (threshold: 100ms)
  }

  return result; // => Return untouched result
});

const profiledProcedure = t.procedure.use(profilingMiddleware);

const appRouter = t.router({
  // => All procedures using profiledProcedure are automatically profiled
  getUsers: profiledProcedure.query(async () => {
    await new Promise((r) => setTimeout(r, 15)); // => Simulate 15ms DB query
    return [{ id: 1, name: "Aisha" }]; // => Fast: under threshold
  }),

  generateReport: profiledProcedure.query(async () => {
    await new Promise((r) => setTimeout(r, 150)); // => Simulate slow 150ms operation
    return { report: "large dataset" }; // => Slow: triggers warning
  }),

  // => Admin endpoint: view performance stats (not profiled to avoid recursion)
  perfStats: t.procedure.query(() => {
    const slowProcedures = perfRecords.filter((r) => r.slow);
    const byPath = perfRecords.reduce<Record<string, number[]>>((acc, r) => {
      (acc[r.path] ??= []).push(r.durationMs);
      return acc;
    }, {});

    // => Compute average duration per procedure
    const averages = Object.fromEntries(
      Object.entries(byPath).map(([path, durations]) => [
        path,
        Math.round(durations.reduce((a, b) => a + b, 0) / durations.length),
      ]),
    );
    // => e.g., { "users.getUsers": 15, "users.generateReport": 150 }

    return {
      totalCalls: perfRecords.length,
      slowCalls: slowProcedures.length,
      averageDurationsByPath: averages,
      // => Identify which procedures need optimization
    };
  }),
});

export type AppRouter = typeof appRouter;
export { appRouter };
```

**Key Takeaway**: Profiling middleware records timing for every procedure call. Compare against thresholds to identify slow procedures. A separate untracked `perfStats` procedure exposes aggregated data.

**Why It Matters**: You cannot optimize what you do not measure. Profiling middleware provides the timing data needed to find bottlenecks before users complain. N+1 query patterns, missing database indexes, and inefficient aggregations all appear as slow procedures. The threshold-based alerting creates an immediate feedback loop during development—exceeding 100ms triggers a console warning that the developer sees immediately. Production systems export this data to APM tools (Datadog, New Relic, Honeycomb) for SLO tracking and alerting.

---

## Summary: tRPC 95% Coverage Achieved

These 80 examples cover the following tRPC capabilities:

- **Core API** (Examples 1-6): `initTRPC`, routers, queries, mutations, Zod validation, return types
- **Context System** (Examples 7-9): Context creation, database in context, context access patterns
- **Error Handling** (Examples 10-12): `TRPCError`, error codes, Zod validation errors
- **Type Safety** (Examples 13-15): Output schemas, `inferRouterInputs`/`inferRouterOutputs`, union returns
- **Middleware** (Examples 16-20): Base procedures, logging, metrics, chaining, context augmentation
- **Async Patterns** (Examples 21-22): Async queries, async mutations with error handling
- **Input Patterns** (Examples 23-25): Nested schemas, array validation, enums and literals
- **Router Organization** (Examples 26-28): Modular composition, merging, type-safe paths
- **Advanced Middleware** (Examples 29-31): Deep nesting, context narrowing, rate limiting
- **Subscriptions** (Examples 32-33): Observable-based subscriptions, filtering
- **React Query** (Examples 34-39): Setup, useQuery, useMutation, cache invalidation, optimistic updates, infinite queries
- **Error Formatting** (Examples 40-41): Custom formatters, error interception middleware
- **SSR** (Examples 42-43): `createServerSideHelpers`, App Router Server Components
- **Performance** (Examples 44-45): Batching, deduplication
- **Advanced Input** (Examples 46-47): Transform, discriminated unions
- **Production Patterns** (Examples 48-55): Multi-tenancy, audit logging, cursor pagination, schema composition, conditional data, caching hints, batch mutations, health checks
- **Custom Links** (Examples 56-58): Terminating links, retry with backoff, logging links
- **WebSocket** (Examples 59-60): `wsLink` setup, `useSubscription` hook
- **Testing** (Examples 61-63): `createCaller` unit tests, HTTP integration tests, authorization matrices
- **Next.js Integration** (Examples 64-66): App Router handlers, Server Actions, shared type packages
- **Optimization** (Examples 67-69): Superjson, `enabled` flag, prefetch on hover
- **Type Utilities** (Examples 70-71): Advanced inference, type guards
- **Migration** (Examples 72-73): REST coexistence, vanilla TypeScript client
- **Interop** (Example 74): OpenAPI compatibility
- **Expert Patterns** (Examples 75-80): CRUD factories, event-driven architecture, context enrichment, error boundaries, monorepo structure, performance profiling

The remaining 5% covers exotic adapters, bleeding-edge features, and platform-specific configurations that arise from specialized use cases rather than core tRPC development.
