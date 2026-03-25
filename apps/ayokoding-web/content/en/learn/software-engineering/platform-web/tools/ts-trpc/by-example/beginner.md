---
title: "Beginner"
weight: 10000001
date: 2026-03-25T00:00:00+07:00
draft: false
description: "Master tRPC fundamentals through 28 annotated examples covering initTRPC, routers, query and mutation procedures, Zod validation, context, error handling, and basic middleware"
tags: ["trpc", "typescript", "api", "tutorial", "by-example", "beginner"]
---

This beginner tutorial covers fundamental tRPC concepts through 28 heavily annotated examples. Each example maintains 1-2.25 comment lines per code line to ensure deep understanding. All examples run in Node.js with `@trpc/server` and `zod` as the only dependencies.

## Prerequisites

Before starting, ensure you understand:

- TypeScript generics and type inference
- Node.js modules and async/await
- Basic REST API concepts (request, response, status codes)
- npm package installation

## Group 1: Router and Procedure Basics

### Example 1: Creating a tRPC Instance with initTRPC

tRPC starts with `initTRPC.create()` which returns builders for routers and procedures. The instance is the root of your entire type-safe API surface.

```typescript
// server.ts
import { initTRPC } from "@trpc/server"; // => Import the tRPC factory

// => initTRPC.create() bootstraps tRPC with default configuration
// => Returns an object t with .router(), .procedure, and .middleware()
const t = initTRPC.create();

// => t.router() groups named procedures into a callable router
// => Each key becomes a procedure name on the client
const appRouter = t.router({
  // => "ping" is the procedure name - client calls it as trpc.ping.query()
  ping: t.procedure
    // => .query() marks this as a read operation (GET semantics)
    .query(() => {
      // => Return value infers to string automatically
      return "pong"; // => Client receives "pong" with full TypeScript inference
    }),
});

// => Export the type only (not the value) for end-to-end type safety
// => Client imports AppRouter to get procedure signatures, inputs, and outputs
export type AppRouter = typeof appRouter;

// => Export the value for use in the HTTP adapter
export { appRouter };
```

**Key Takeaway**: `initTRPC.create()` bootstraps your tRPC instance. Export `typeof appRouter` as a type for the client to consume.

**Why It Matters**: The separation between `appRouter` (value used server-side) and `AppRouter` (type used client-side) is fundamental to tRPC's zero-codegen philosophy. Your TypeScript types flow from server to client through the type export, not through generated files. Every tRPC application starts with exactly this pattern. When you open a production tRPC codebase, this is always the first file you look at to understand the API surface.

### Example 2: Query Procedure - Reading Data

Query procedures handle read operations. They map to HTTP GET semantics—idempotent, no side effects.

```typescript
import { initTRPC } from "@trpc/server";

const t = initTRPC.create();

// => Define a simple in-memory data store for this example
const users = [
  { id: 1, name: "Aisha Rahman", email: "aisha@example.com" },
  { id: 2, name: "Omar Farooq", email: "omar@example.com" },
  { id: 3, name: "Fatima Al-Zahra", email: "fatima@example.com" },
];

const appRouter = t.router({
  // => getUsers returns the full user list
  // => No input required - returns all items
  getUsers: t.procedure.query(() => {
    // => Return type inferred as { id: number; name: string; email: string }[]
    return users; // => Returns array of 3 user objects
  }),

  // => getUserCount returns a derived value from the data
  getUserCount: t.procedure.query(() => {
    // => Computed value - derived from users array length
    return { count: users.length }; // => Returns { count: 3 }
  }),
});

export type AppRouter = typeof appRouter;
export { appRouter };
```

**Key Takeaway**: Query procedures use `.query()` for read-only operations. Return any serializable value; TypeScript infers the full return type.

**Why It Matters**: Queries are the most common procedure type in tRPC applications. Unlike REST where you manually document return shapes, tRPC infers return types automatically—the client gets full autocomplete and type checking on `data.count` or `data[0].name` without any extra work. Every dashboard, list view, and read operation in your app uses this pattern.

### Example 3: Mutation Procedure - Writing Data

Mutation procedures handle write operations. They map to HTTP POST semantics—cause side effects, not idempotent.

```typescript
import { initTRPC } from "@trpc/server";

const t = initTRPC.create();

// => Mutable in-memory store to demonstrate state changes
let todos: { id: number; text: string; done: boolean }[] = [];
let nextId = 1; // => Auto-incrementing ID counter

const appRouter = t.router({
  // => addTodo creates a new todo item
  // => .mutation() marks this as a write operation
  addTodo: t.procedure.mutation(() => {
    // => Side effect: modifies the todos array
    const newTodo = { id: nextId++, text: "New todo", done: false };
    // => nextId increments: 1 → 2 → 3 on each call
    todos.push(newTodo); // => Mutates the todos array
    return newTodo; // => Returns the created item (id: 1, text: "New todo", done: false)
  }),

  // => getTodos reads the current state
  getTodos: t.procedure.query(() => {
    return todos; // => Returns current array state
  }),
});

export type AppRouter = typeof appRouter;
export { appRouter };
```

**Key Takeaway**: Use `.mutation()` for write operations that cause side effects. Mutations can return data (like the created item) for optimistic update patterns.

**Why It Matters**: The query/mutation distinction maps directly to React Query's caching behavior on the client. Queries are cached and refetched; mutations invalidate caches. Following this convention ensures your client-side caching works correctly without extra configuration. Production tRPC apps use mutations for all create, update, and delete operations.

### Example 4: Input Validation with Zod

tRPC integrates natively with Zod for runtime input validation. The Zod schema also infers the TypeScript input type.

```typescript
import { initTRPC } from "@trpc/server";
import { z } from "zod"; // => Zod: TypeScript-first schema validation library

const t = initTRPC.create();

const appRouter = t.router({
  // => greet accepts a validated string input
  greet: t.procedure
    .input(
      // => z.object() defines the shape of the input
      z.object({
        name: z.string(), // => name must be a non-empty string
        // => TypeScript infers input type as { name: string }
      }),
    )
    .query((opts) => {
      // => opts.input is typed as { name: string } - no manual casting needed
      const { name } = opts.input; // => Destructure validated input
      return `Hello, ${name}!`; // => Returns "Hello, Aisha!" when name is "Aisha"
    }),

  // => createUser accepts multiple validated fields
  createUser: t.procedure
    .input(
      z.object({
        username: z.string().min(3), // => Username must be at least 3 characters
        age: z.number().min(0).max(120), // => Age must be 0-120
        email: z.string().email(), // => Must be valid email format
      }),
    )
    .mutation((opts) => {
      // => opts.input: { username: string; age: number; email: string }
      // => All fields are guaranteed valid by Zod before reaching this code
      return { id: 1, ...opts.input }; // => Returns created user with ID
    }),
});

export type AppRouter = typeof appRouter;
export { appRouter };
```

**Key Takeaway**: `.input(zodSchema)` validates at runtime and infers TypeScript types. Invalid inputs throw a `BAD_REQUEST` error before your procedure logic runs.

**Why It Matters**: Zod validation is the primary safety layer in tRPC. Without it, any input reaches your procedure. With it, you get runtime safety plus TypeScript inference from a single schema definition. Production tRPC applications use Zod schemas extensively - complex nested objects, optional fields, union types, and custom validators all work through this same `.input()` pattern.

### Example 5: Optional and Default Input Fields

Zod allows optional fields, default values, and nullable types. These patterns handle partial updates and optional parameters.

```typescript
import { initTRPC } from "@trpc/server";
import { z } from "zod";

const t = initTRPC.create();

const appRouter = t.router({
  // => searchUsers has optional filtering parameters
  searchUsers: t.procedure
    .input(
      z.object({
        query: z.string().optional(), // => query?: string (can be undefined)
        limit: z.number().default(10), // => limit defaults to 10 if not provided
        // => Without .default(), undefined would pass through as-is
        active: z.boolean().nullish(), // => active?: boolean | null (nullish = optional + nullable)
      }),
    )
    .query((opts) => {
      const { query, limit, active } = opts.input;
      // => query: string | undefined
      // => limit: number (always present, default 10 applied before here)
      // => active: boolean | null | undefined

      return {
        query, // => undefined if not provided
        limit, // => 10 if not provided by caller
        active, // => null if explicitly null, undefined if omitted
        results: [], // => Empty array for this example
      };
    }),
});

export type AppRouter = typeof appRouter;
export { appRouter };
```

**Key Takeaway**: Use `.optional()` for truly optional fields, `.default()` for fields with sensible defaults, and `.nullish()` for fields that can be null or undefined.

**Why It Matters**: Real API inputs are rarely all-required. Pagination parameters, filter fields, and optional metadata need nullable or optional typing. Zod's `.default()` removes the need for null-coalescing in procedure logic (`limit ?? 10`), making procedures cleaner. `.optional()` vs `.nullish()` vs `.nullable()` distinctions prevent bugs where callers send `null` expecting it to behave like `undefined`.

### Example 6: Returning Structured Data with Type Inference

tRPC infers complex return types automatically. Clients get full type safety on nested objects, arrays, and union types.

```typescript
import { initTRPC } from "@trpc/server";

const t = initTRPC.create();

// => Define complex data shapes that will be fully type-inferred
interface Product {
  id: number;
  name: string;
  price: number;
  tags: string[];
  metadata: {
    createdAt: Date;
    updatedAt: Date;
    inStock: boolean;
  };
}

const sampleProducts: Product[] = [
  {
    id: 1,
    name: "Laptop",
    price: 999.99,
    tags: ["electronics", "computing"],
    metadata: {
      createdAt: new Date("2026-01-01"),
      updatedAt: new Date("2026-03-01"),
      inStock: true,
    },
  },
];

const appRouter = t.router({
  // => getProducts returns complex nested type - fully inferred
  getProducts: t.procedure.query(() => {
    // => Return type: Product[] (full interface with nested metadata)
    // => Client sees: data[0].metadata.inStock - fully type-checked
    return sampleProducts;
  }),

  // => getProductSummary returns a transformed shape
  getProductSummary: t.procedure.query(() => {
    // => Map to simpler shape - return type inferred from mapping
    return sampleProducts.map((p) => ({
      id: p.id, // => number
      name: p.name, // => string
      priceFormatted: `$${p.price.toFixed(2)}`, // => string like "$999.99"
      tagCount: p.tags.length, // => number
    }));
    // => Client type: { id: number; name: string; priceFormatted: string; tagCount: number }[]
  }),
});

export type AppRouter = typeof appRouter;
export { appRouter };
```

**Key Takeaway**: tRPC infers complex return types automatically. Nest interfaces, arrays, and unions freely—clients get full autocomplete without any manual type definitions.

**Why It Matters**: Type inference on return values eliminates an entire category of frontend bugs. When you rename a field on the server from `createdAt` to `created_at`, TypeScript immediately flags every client usage. No manual OpenAPI spec updates, no regeneration step, no runtime surprises. This automatic contract enforcement is tRPC's most compelling production benefit.

## Group 2: Context and Typed Context

### Example 7: Creating and Using Context

Context provides per-request data (user session, database connection, request headers) to all procedures. Define it once; access everywhere.

```typescript
import { initTRPC } from "@trpc/server";

// => Context type defines what data is available in all procedures
// => Created fresh for every incoming request
interface Context {
  requestId: string; // => Unique ID for tracing/logging
  timestamp: number; // => Request arrival time
  userAgent: string; // => Browser/client identification
}

// => initTRPC.context<Context>() binds the context type to this tRPC instance
// => Now all procedures in this instance know about Context shape
const t = initTRPC.context<Context>().create();

const appRouter = t.router({
  // => opts.ctx is typed as Context - full autocomplete
  getRequestInfo: t.procedure.query((opts) => {
    const { ctx } = opts; // => ctx: Context (typed, not any)
    // => Access ctx.requestId, ctx.timestamp, ctx.userAgent safely
    return {
      requestId: ctx.requestId, // => e.g., "req-abc123"
      timestamp: ctx.timestamp, // => e.g., 1711300000000
      userAgent: ctx.userAgent, // => e.g., "Mozilla/5.0..."
    };
  }),
});

export type AppRouter = typeof appRouter;
export { appRouter };

// => Context creation function - called by the HTTP adapter for each request
// => In Express: createContext receives (req, res) from Express
export const createContext = (): Context => ({
  requestId: Math.random().toString(36).slice(2), // => Random request ID
  timestamp: Date.now(), // => Current Unix timestamp in ms
  userAgent: "test-client", // => Would come from request headers in production
});
```

**Key Takeaway**: Define a `Context` type, pass it to `initTRPC.context<Context>()`, and implement `createContext` for your HTTP adapter. All procedures receive typed context.

**Why It Matters**: Context is how tRPC procedures access request-scoped data without prop-drilling or global variables. Every production tRPC application uses context for authentication (is the user logged in?), database connections (which DB pool to use?), and tenant isolation (which organization's data to query?). The typed context eliminates `ctx as any` casts throughout your codebase.

### Example 8: Database Connection in Context

A common context pattern passes a database client per request, enabling procedures to query data without importing global singletons.

```typescript
import { initTRPC } from "@trpc/server";

// => Simulate a database client interface
// => In production: import { PrismaClient } from '@prisma/client'
interface DatabaseClient {
  users: {
    findMany: () => Promise<{ id: number; name: string }[]>;
    findById: (id: number) => Promise<{ id: number; name: string } | null>;
  };
}

// => Context carries the DB client, available in every procedure
interface Context {
  db: DatabaseClient;
}

const t = initTRPC.context<Context>().create();

// => Mock database for this example
const mockDb: DatabaseClient = {
  users: {
    findMany: async () => [
      { id: 1, name: "Aisha" },
      { id: 2, name: "Omar" },
    ],
    findById: async (id) => (id === 1 ? { id: 1, name: "Aisha" } : null),
  },
};

const appRouter = t.router({
  // => Procedure uses ctx.db - no global import needed
  listUsers: t.procedure.query(async (opts) => {
    // => opts.ctx.db is typed as DatabaseClient
    const users = await opts.ctx.db.users.findMany();
    // => users: { id: number; name: string }[]
    return users; // => Returns [{ id: 1, name: "Aisha" }, { id: 2, name: "Omar" }]
  }),
});

export type AppRouter = typeof appRouter;
export { appRouter };

// => createContext instantiates DB client per request
// => In production: use connection pooling, not new client per request
export const createContext = (): Context => ({
  db: mockDb, // => In production: return { db: new PrismaClient() } or pooled instance
});
```

**Key Takeaway**: Pass database clients through context rather than importing globals. This enables testing (swap mock DB in createContext), connection pooling, and tenant isolation.

**Why It Matters**: Global database client imports create tight coupling and make testing painful. Context-injected clients make every procedure testable—just pass a mock `db` in your test's `createContext`. Multi-tenant SaaS applications use this pattern to switch database connections based on the request's tenant header. Production tRPC with Prisma always follows this pattern.

### Example 9: Accessing Context in Procedures

Context flows into both query and mutation procedures. This example shows common context access patterns.

```typescript
import { initTRPC } from "@trpc/server";
import { z } from "zod";

interface Context {
  userId: string | null; // => null when not authenticated
  ip: string; // => Client IP address for rate limiting
  locale: string; // => e.g., "en-US" for i18n
}

const t = initTRPC.context<Context>().create();

const posts = [
  { id: 1, title: "First Post", authorId: "user-1" },
  { id: 2, title: "Second Post", authorId: "user-2" },
];

const appRouter = t.router({
  // => Query uses ctx.userId to filter results
  myPosts: t.procedure.query((opts) => {
    const { userId } = opts.ctx; // => string | null
    if (!userId) {
      return []; // => Unauthenticated users see no posts
    }
    // => Filter posts belonging to the current user
    return posts.filter((p) => p.authorId === userId);
    // => Returns posts where authorId matches userId
  }),

  // => Mutation uses multiple context fields
  createPost: t.procedure.input(z.object({ title: z.string() })).mutation((opts) => {
    const { userId, ip } = opts.ctx; // => Destructure needed fields
    const { title } = opts.input; // => Validated input

    // => Log the creation attempt with IP for audit trail
    console.log(`Create post from IP: ${ip}`); // => e.g., "Create post from IP: 127.0.0.1"

    if (!userId) {
      throw new Error("Must be authenticated"); // => Will be caught later
    }

    return { id: Date.now(), title, authorId: userId }; // => Returns new post
  }),
});

export type AppRouter = typeof appRouter;
export { appRouter };
```

**Key Takeaway**: Destructure `opts.ctx` for context fields and `opts.input` for validated inputs. Both are fully typed.

**Why It Matters**: The `opts` parameter pattern (`opts.ctx`, `opts.input`) keeps procedures readable. You always know where data comes from: context is per-request shared data, input is caller-provided validated data. Production procedures regularly use both—check context for authorization, use input for the operation's parameters.

## Group 3: Error Handling

### Example 10: Throwing TRPCError

tRPC provides `TRPCError` with standard HTTP-like error codes. Throw it to send structured errors to clients.

```typescript
import { initTRPC, TRPCError } from "@trpc/server"; // => Import TRPCError class
import { z } from "zod";

const t = initTRPC.create();

const users = new Map([
  ["user-1", { id: "user-1", name: "Aisha", role: "admin" }],
  ["user-2", { id: "user-2", name: "Omar", role: "member" }],
]);

const appRouter = t.router({
  // => getUser throws NOT_FOUND when ID doesn't exist
  getUser: t.procedure.input(z.object({ id: z.string() })).query((opts) => {
    const user = users.get(opts.input.id); // => undefined if not found

    if (!user) {
      // => TRPCError maps to HTTP 404
      // => code: "NOT_FOUND" → HTTP 404
      throw new TRPCError({
        code: "NOT_FOUND", // => Standard tRPC error code
        message: `User ${opts.input.id} not found`, // => Human-readable message
      });
    }

    return user; // => Only reached when user exists
  }),

  // => deleteUser throws FORBIDDEN for non-admin users
  deleteUser: t.procedure.input(z.object({ targetId: z.string() })).mutation((opts) => {
    const requesterId = "user-2"; // => Simulated: in real app comes from ctx.userId

    const requester = users.get(requesterId);
    if (requester?.role !== "admin") {
      // => code: "FORBIDDEN" → HTTP 403
      throw new TRPCError({
        code: "FORBIDDEN",
        message: "Only admins can delete users",
      });
    }

    users.delete(opts.input.targetId);
    return { success: true }; // => Deletion succeeded
  }),
});

export type AppRouter = typeof appRouter;
export { appRouter };
```

**Key Takeaway**: Use `TRPCError` with semantic codes (`NOT_FOUND`, `FORBIDDEN`, `UNAUTHORIZED`, `BAD_REQUEST`, `INTERNAL_SERVER_ERROR`) to communicate error semantics to clients.

**Why It Matters**: Semantic error codes enable clients to handle errors correctly. A `NOT_FOUND` triggers a 404 page; `UNAUTHORIZED` redirects to login; `FORBIDDEN` shows a permission denied message. Using generic `Error` throws instead of `TRPCError` sends opaque `INTERNAL_SERVER_ERROR` codes to clients, losing all semantic information. Production tRPC apps map every business rule violation to a specific error code.

### Example 11: TRPCError Codes Reference

Each error code maps to an HTTP status and has specific semantics. This example demonstrates all common codes.

```typescript
import { initTRPC, TRPCError } from "@trpc/server";
import { z } from "zod";

const t = initTRPC.create();

const appRouter = t.router({
  // => UNAUTHORIZED: Not logged in (401 HTTP)
  requiresLogin: t.procedure.query(() => {
    const isLoggedIn = false; // => Simulated unauthenticated state
    if (!isLoggedIn) {
      throw new TRPCError({
        code: "UNAUTHORIZED", // => "You must log in to access this"
        message: "Authentication required",
      });
    }
    return "secret data";
  }),

  // => FORBIDDEN: Logged in but lacks permission (403 HTTP)
  adminOnly: t.procedure.query(() => {
    const userRole = "member"; // => Simulated non-admin user
    if (userRole !== "admin") {
      throw new TRPCError({
        code: "FORBIDDEN", // => "You are logged in but not allowed"
        message: "Admin access required",
      });
    }
    return "admin panel data";
  }),

  // => BAD_REQUEST: Invalid input beyond Zod validation (400 HTTP)
  dateRange: t.procedure
    .input(
      z.object({
        start: z.string(),
        end: z.string(),
      }),
    )
    .query((opts) => {
      const { start, end } = opts.input;
      if (new Date(start) > new Date(end)) {
        throw new TRPCError({
          code: "BAD_REQUEST", // => Logical validation Zod can't express
          message: "Start date must be before end date",
        });
      }
      return { start, end, days: 7 }; // => Valid range response
    }),

  // => INTERNAL_SERVER_ERROR: Unexpected failures (500 HTTP)
  riskyOperation: t.procedure.mutation(async () => {
    try {
      // => Simulated operation that might fail
      const result = await Promise.resolve("ok");
      return result;
    } catch (err) {
      // => Wrap unexpected errors in INTERNAL_SERVER_ERROR
      // => Never leak raw error messages to clients in production
      throw new TRPCError({
        code: "INTERNAL_SERVER_ERROR",
        message: "Operation failed unexpectedly",
        cause: err, // => Original error preserved for server logs
      });
    }
  }),
});

export type AppRouter = typeof appRouter;
export { appRouter };
```

**Key Takeaway**: Match error codes to their semantics: `UNAUTHORIZED` for missing authentication, `FORBIDDEN` for insufficient permissions, `BAD_REQUEST` for invalid logic, `INTERNAL_SERVER_ERROR` for unexpected failures.

**Why It Matters**: Correct error codes drive correct client behavior. React Query on the client can check `error.data?.code === 'UNAUTHORIZED'` to redirect to the login page, or `error.data?.code === 'NOT_FOUND'` to show a 404 component. Using wrong codes (e.g., `BAD_REQUEST` for auth failures) breaks client error handling. Following HTTP semantics also makes your API predictable for any HTTP consumer, not just your tRPC client.

### Example 12: Handling Validation Errors from Zod

When Zod validation fails, tRPC automatically throws a `BAD_REQUEST` TRPCError with detailed field-level information. This example demonstrates what clients receive.

```typescript
import { initTRPC } from "@trpc/server";
import { z } from "zod";

const t = initTRPC.create();

// => Complex Zod schema to demonstrate validation error messages
const registrationSchema = z.object({
  username: z
    .string()
    .min(3, "Username must be at least 3 characters")
    // => Custom error message appears in the error response
    .max(20, "Username must be at most 20 characters")
    .regex(/^[a-z0-9_]+$/, "Username can only contain lowercase letters, numbers, and underscores"),

  email: z.string().email("Must be a valid email address"),
  // => .email() validates format: requires @ and valid domain

  password: z.string().min(8, "Password must be at least 8 characters"),

  age: z.number().int("Age must be a whole number").min(13, "Must be at least 13 years old"),
});

const appRouter = t.router({
  // => If input fails validation, procedure never runs
  // => tRPC automatically throws BAD_REQUEST with Zod error details
  register: t.procedure
    .input(registrationSchema) // => Validate against schema
    .mutation((opts) => {
      // => This code only runs when ALL fields pass validation
      const { username, email, age } = opts.input;
      // => username: valid string (3-20 chars, matching regex)
      // => email: valid email string
      // => age: integer >= 13
      return {
        id: 1,
        username,
        email,
        age,
        createdAt: new Date().toISOString(),
      };
    }),
});

export type AppRouter = typeof appRouter;
export { appRouter };

// => When called with invalid data, client receives:
// => { code: 'BAD_REQUEST', message: 'Input validation failed', zodErrors: [...] }
// => Each field's error message is included in zodErrors
```

**Key Takeaway**: Zod validation errors surface automatically as `BAD_REQUEST` with field-level messages. Your procedure logic only runs after all validations pass.

**Why It Matters**: Automatic Zod error formatting saves significant boilerplate. Instead of manually checking each field and constructing error responses, you declare validation rules once and get structured error responses for free. React Hook Form integration on the client can map these Zod errors directly to form field validation states, giving users precise feedback on exactly which fields failed and why.

## Group 4: Output Types and Type Safety

### Example 13: Explicit Output Types with Zod

While tRPC infers return types automatically, explicit output schemas validate your procedure's actual output and strip unexpected fields.

```typescript
import { initTRPC } from "@trpc/server";
import { z } from "zod";

const t = initTRPC.create();

// => Define the output schema - what the client will receive
const userOutputSchema = z.object({
  id: z.number(),
  name: z.string(),
  email: z.string(),
  // => Note: 'passwordHash' is NOT in output schema
  // => This prevents accidentally leaking sensitive fields
});

// => Full internal user type with sensitive fields
interface InternalUser {
  id: number;
  name: string;
  email: string;
  passwordHash: string; // => Never expose this to clients
  internalNotes: string; // => Never expose this either
}

const internalUsers: InternalUser[] = [
  {
    id: 1,
    name: "Aisha",
    email: "aisha@example.com",
    passwordHash: "bcrypt$...", // => Sensitive - must not reach client
    internalNotes: "VIP customer", // => Sensitive - must not reach client
  },
];

const appRouter = t.router({
  // => .output() adds explicit return type validation
  getUser: t.procedure
    .input(z.object({ id: z.number() }))
    .output(userOutputSchema) // => Validates and strips fields NOT in schema
    .query((opts) => {
      const user = internalUsers.find((u) => u.id === opts.input.id);
      if (!user) return null as unknown as z.infer<typeof userOutputSchema>;
      // => Even if we accidentally return passwordHash here,
      // => Zod output schema strips it before sending to client
      return user; // => passwordHash and internalNotes stripped by output schema
    }),
});

export type AppRouter = typeof appRouter;
export { appRouter };
```

**Key Takeaway**: Use `.output(schema)` to validate procedure returns and strip sensitive fields. The schema acts as a security layer between your internal data models and the API surface.

**Why It Matters**: Accidental data leakage is a serious security risk. Without output schemas, a refactored query that adds an extra field might accidentally expose passwords or PII. Output schemas create an explicit contract—any field not in the schema is stripped. Production tRPC applications use output schemas for user data, payment information, and any sensitive domain objects. It also documents the API contract clearly.

### Example 14: Inferring Input and Output Types

tRPC provides utility types to extract procedure input and output types for use in non-tRPC code.

```typescript
import { initTRPC } from "@trpc/server";
import { z } from "zod";
import type { inferRouterInputs, inferRouterOutputs } from "@trpc/server";
// => inferRouterInputs/inferRouterOutputs: utility types for extracting procedure shapes

const t = initTRPC.create();

const searchInputSchema = z.object({
  query: z.string(),
  page: z.number().default(1),
  limit: z.number().default(20),
});

const appRouter = t.router({
  searchProducts: t.procedure.input(searchInputSchema).query((opts) => {
    const { query, page, limit } = opts.input;
    // => query: string, page: number (default 1), limit: number (default 20)
    return {
      results: [{ id: 1, name: `Result for: ${query}` }],
      total: 1,
      page,
      limit,
    };
  }),
});

export type AppRouter = typeof appRouter;
export { appRouter };

// => Extract input type for a specific procedure
// => Use these types in form components, validation hooks, etc.
type RouterInputs = inferRouterInputs<AppRouter>;
// => RouterInputs["searchProducts"] = { query: string; page: number; limit: number }

type RouterOutputs = inferRouterOutputs<AppRouter>;
// => RouterOutputs["searchProducts"] = { results: {...}[]; total: number; page: number; limit: number }

// => Example: type a search form component's props
type SearchFormProps = {
  onSearch: (input: RouterInputs["searchProducts"]) => void;
  // => onSearch receives the exact procedure input type
};

// => Example: type a search results component's props
type SearchResultsProps = {
  data: RouterOutputs["searchProducts"];
  // => data has the exact shape that searchProducts returns
};

// => Both types are always in sync with the server - no manual updates needed
type _unused = SearchFormProps | SearchResultsProps; // => Suppress TS unused warning
```

**Key Takeaway**: `inferRouterInputs<AppRouter>` and `inferRouterOutputs<AppRouter>` extract procedure types for use in components, forms, and utilities outside tRPC hooks.

**Why It Matters**: These utility types close the final gap in end-to-end type safety. Component props, form handlers, and data transformers can all reference exact procedure shapes. When you change `searchProducts` to add a `category` filter, TypeScript immediately flags every consumer of `RouterInputs["searchProducts"]` that hasn't been updated. Production codebases import these types throughout the frontend codebase.

### Example 15: Union and Discriminated Union Returns

tRPC procedures can return TypeScript union types, enabling different response shapes based on conditional logic.

```typescript
import { initTRPC } from "@trpc/server";
import { z } from "zod";

const t = initTRPC.create();

// => Discriminated union: 'type' field determines shape
type ApiResponse<T> =
  | { type: "success"; data: T; requestId: string }
  | { type: "empty"; message: string; requestId: string };
// => Discriminant: 'type' field narrows the union on the client

const appRouter = t.router({
  // => Returns different shapes based on data availability
  findItem: t.procedure.input(z.object({ id: z.number() })).query((opts): ApiResponse<{ id: number; name: string }> => {
    // => Explicit return type annotation for union
    const requestId = `req-${Date.now()}`; // => Trace ID
    const item = opts.input.id === 1 ? { id: 1, name: "Found item" } : null;

    if (item) {
      // => Success branch: data field is present
      return { type: "success", data: item, requestId };
      // => Client: if (result.type === 'success') result.data.name
    } else {
      // => Empty branch: no data field
      return {
        type: "empty",
        message: "No item found",
        requestId,
      };
      // => Client: if (result.type === 'empty') result.message
    }
  }),
});

export type AppRouter = typeof appRouter;
export { appRouter };
```

**Key Takeaway**: Return discriminated unions for procedures with multiple valid response shapes. The `type` discriminant field enables safe narrowing on the client.

**Why It Matters**: Discriminated unions are more expressive than nullable returns (`T | null`). A `null` response loses information—was it not found? Empty? Deleted? A discriminated union communicates semantics. Production APIs often need multiple success cases: a search might return results, suggest alternatives, or indicate no matches—all valid, but different shapes requiring different client handling.

## Group 5: Basic Middleware

### Example 16: Creating a Reusable Base Procedure

Middleware in tRPC is applied through procedure chaining. Create a base procedure with shared logic, then extend it.

```typescript
import { initTRPC } from "@trpc/server";

interface Context {
  userId: string | null;
  requestId: string;
}

const t = initTRPC.context<Context>().create();

// => t.procedure is the base - no middleware, no context augmentation
// => Use this for public endpoints accessible without authentication
export const publicProcedure = t.procedure;
// => publicProcedure: accessible by anyone

// => .use() attaches middleware to a procedure base
// => All procedures built from protectedProcedure run this middleware first
export const protectedProcedure = t.procedure.use((opts) => {
  const { ctx } = opts; // => Access current context
  // => ctx.userId: string | null

  if (!ctx.userId) {
    // => Throw before calling opts.next() prevents the procedure from running
    throw new Error("Not authenticated"); // => In production use TRPCError UNAUTHORIZED
  }

  // => opts.next() calls the next middleware or the actual procedure
  return opts.next({
    ctx: {
      ...ctx, // => Spread existing context
      userId: ctx.userId, // => Now TypeScript knows userId is string (not null)
      // => TypeScript narrows: string | null → string
    },
  });
});
// => protectedProcedure: only runs when ctx.userId exists

const appRouter = t.router({
  // => Public - no authentication required
  getPublicData: publicProcedure.query(() => {
    return { message: "This is public" }; // => Anyone can access
  }),

  // => Protected - only authenticated users
  getPrivateData: protectedProcedure.query((opts) => {
    // => opts.ctx.userId is string here (not string | null)
    // => TypeScript guarantees this because middleware checked it
    return { message: `Hello, user ${opts.ctx.userId}` };
  }),
});

export type AppRouter = typeof appRouter;
export { appRouter };
```

**Key Takeaway**: Create named procedure bases (`publicProcedure`, `protectedProcedure`) by adding middleware with `.use()`. Procedures built from these bases automatically include that middleware.

**Why It Matters**: Named procedure bases are the central pattern for code reuse in tRPC applications. Every production app has at least `publicProcedure` and `protectedProcedure`. Auth checks, rate limiting, logging, and tenant isolation all live in procedure bases rather than being duplicated in each procedure. The TypeScript narrowing in middleware (`string | null → string`) removes null checks from every protected procedure, reducing boilerplate significantly.

### Example 17: Middleware for Logging

Middleware can add cross-cutting concerns like logging without modifying each procedure.

```typescript
import { initTRPC } from "@trpc/server";

const t = initTRPC.create();

// => Logging middleware: wraps every procedure call
const loggingMiddleware = t.middleware(async (opts) => {
  const { type, path } = opts; // => 'query' or 'mutation', procedure path like 'users.get'
  const start = Date.now(); // => Record start time for duration calculation

  console.log(`→ ${type} ${path} started`); // => Log before procedure runs
  // => Example output: → query users.getById started

  // => opts.next() executes the actual procedure
  const result = await opts.next();
  // => result.ok: boolean indicating success or failure

  const durationMs = Date.now() - start; // => Calculate elapsed time

  if (result.ok) {
    console.log(`✓ ${type} ${path} completed in ${durationMs}ms`);
    // => Example: ✓ query users.getById completed in 12ms
  } else {
    console.error(`✗ ${type} ${path} failed in ${durationMs}ms`);
    // => Example: ✗ mutation users.delete failed in 5ms
  }

  return result; // => Must return result - passes to client
});

// => Create a procedure base with logging middleware attached
const loggedProcedure = t.procedure.use(loggingMiddleware);
// => Every procedure using loggedProcedure logs start, end, and duration

const appRouter = t.router({
  // => Uses loggedProcedure - automatically logged
  getUser: loggedProcedure.query(() => {
    return { id: 1, name: "Aisha" }; // => Execution is logged automatically
  }),

  createPost: loggedProcedure.mutation(() => {
    return { id: 1, title: "New Post" }; // => Execution is logged automatically
  }),
});

export type AppRouter = typeof appRouter;
export { appRouter };
```

**Key Takeaway**: Middleware functions wrap procedure execution. `await opts.next()` runs the procedure; code before it runs before; code after it runs after. Return `result` to pass the response to the client.

**Why It Matters**: Cross-cutting concerns like logging, performance monitoring, and request tracing belong in middleware, not procedure logic. A production tRPC application typically has logging middleware capturing timing, error middleware capturing exceptions to error trackers (Sentry, Datadog), and tracing middleware injecting trace IDs. Writing this logic once in middleware rather than in each procedure reduces code by 90% and ensures consistent behavior.

### Example 18: Middleware for Request Timing and Metrics

Advanced middleware tracks performance metrics across all procedures.

```typescript
import { initTRPC } from "@trpc/server";

const t = initTRPC.create();

// => In-memory metrics store (production: use Prometheus, StatsD, etc.)
const metrics = {
  callCounts: new Map<string, number>(), // => procedure name → call count
  totalDuration: new Map<string, number>(), // => procedure name → total ms
};

// => Metrics middleware: records timing per procedure
const metricsMiddleware = t.middleware(async (opts) => {
  const key = `${opts.type}:${opts.path}`; // => e.g., "query:users.get"
  const start = performance.now(); // => High-resolution timer

  const result = await opts.next(); // => Execute the actual procedure

  const duration = performance.now() - start; // => Duration in ms with sub-ms precision

  // => Update counters atomically
  metrics.callCounts.set(key, (metrics.callCounts.get(key) ?? 0) + 1);
  // => Increment count: 0 → 1 → 2 → ...
  metrics.totalDuration.set(key, (metrics.totalDuration.get(key) ?? 0) + duration);
  // => Accumulate duration for average calculation

  return result; // => Return untouched result to client
});

const trackedProcedure = t.procedure.use(metricsMiddleware);

const appRouter = t.router({
  getProducts: trackedProcedure.query(() => {
    return [{ id: 1, name: "Widget" }]; // => Metrics recorded for this call
  }),

  // => Special procedure to read metrics (admin use)
  getMetrics: t.procedure.query(() => {
    // => Does NOT use trackedProcedure - avoid tracking the metrics endpoint
    const summary: Record<string, { calls: number; avgMs: number }> = {};
    for (const [key, count] of metrics.callCounts) {
      const total = metrics.totalDuration.get(key) ?? 0;
      summary[key] = { calls: count, avgMs: total / count };
      // => avgMs: total duration divided by call count
    }
    return summary; // => e.g., { "query:getProducts": { calls: 5, avgMs: 12.3 } }
  }),
});

export type AppRouter = typeof appRouter;
export { appRouter };
```

**Key Takeaway**: Middleware can collect metrics without affecting procedure logic. The metrics procedure itself does not use the tracked base to avoid recursive measurement.

**Why It Matters**: Performance monitoring is essential in production. Slow procedures degrade user experience; spiky procedures indicate load issues. Middleware-based metrics integrate seamlessly without touching procedure code. Production tRPC apps often combine this with distributed tracing (OpenTelemetry), sending spans to Jaeger or Honeycomb. The metrics data drives SLO alerts, capacity planning, and identifying optimization targets.

## Group 6: Procedure Chaining and Composition

### Example 19: Chaining Multiple Middleware

Multiple middleware layers chain together, each running before and after the next one.

```typescript
import { initTRPC } from "@trpc/server";

interface Context {
  userId: string | null;
  requestId: string;
}

const t = initTRPC.context<Context>().create();

// => First middleware: authentication check
const authMiddleware = t.middleware((opts) => {
  if (!opts.ctx.userId) {
    throw new Error("Authentication required"); // => Stops chain here
  }
  console.log(`[Auth] User ${opts.ctx.userId} authenticated`);
  // => Example: [Auth] User user-123 authenticated
  return opts.next(); // => Passes to next middleware in chain
});

// => Second middleware: logging (runs after auth)
const loggingMiddleware = t.middleware(async (opts) => {
  console.log(`[Log] ${opts.type}:${opts.path} starting`);
  // => Example: [Log] query:dashboard starting
  const result = await opts.next(); // => Passes to procedure
  console.log(`[Log] ${opts.type}:${opts.path} complete`);
  // => Example: [Log] query:dashboard complete
  return result;
});

// => Chain middleware: auth → logging → procedure
// => Execution order: authMiddleware → loggingMiddleware → procedure → loggingMiddleware → authMiddleware
const secureLoggedProcedure = t.procedure
  .use(authMiddleware) // => First in chain: checked first
  .use(loggingMiddleware); // => Second in chain: after auth passes

const appRouter = t.router({
  // => Full chain: auth check, then logging, then procedure
  getDashboard: secureLoggedProcedure.query((opts) => {
    // => opts.ctx.userId is string (auth guaranteed it)
    return { userId: opts.ctx.userId, data: "dashboard content" };
  }),
});

export type AppRouter = typeof appRouter;
export { appRouter };
```

**Key Takeaway**: Chain `.use()` calls to compose middleware. They execute in order, creating a pipeline. Each `await opts.next()` passes control to the next layer.

**Why It Matters**: Middleware composition enables building complex behavior from simple, testable pieces. Production applications chain authentication, authorization, rate limiting, logging, and validation middleware. Each middleware has a single responsibility and can be tested in isolation. Changing the security policy (e.g., adding IP allowlisting) means adding one middleware to the chain rather than modifying dozens of procedures.

### Example 20: Context Augmentation in Middleware

Middleware can enrich the context, adding computed data that all subsequent middleware and procedures use.

```typescript
import { initTRPC } from "@trpc/server";

interface BaseContext {
  authToken: string | null;
  requestId: string;
}

// => Augmented context: adds typed user after auth validation
interface AuthenticatedContext extends BaseContext {
  user: {
    id: string;
    name: string;
    role: "admin" | "member";
    permissions: string[];
  };
}

const t = initTRPC.context<BaseContext>().create();

// => Simulated token → user lookup
const tokenToUser: Record<string, AuthenticatedContext["user"]> = {
  "token-admin": {
    id: "user-1",
    name: "Aisha",
    role: "admin",
    permissions: ["read", "write", "delete"],
  },
  "token-member": {
    id: "user-2",
    name: "Omar",
    role: "member",
    permissions: ["read"],
  },
};

// => Auth middleware: validates token and adds user to context
const withUser = t.middleware((opts) => {
  const { authToken } = opts.ctx; // => string | null
  if (!authToken) {
    throw new Error("No auth token");
  }

  const user = tokenToUser[authToken]; // => Look up user by token
  if (!user) {
    throw new Error("Invalid auth token");
  }

  // => Return next() with augmented context
  return opts.next({
    ctx: {
      ...opts.ctx, // => Keep existing context fields
      user, // => Add resolved user object
      // => Downstream middleware and procedures now have ctx.user
    },
  });
});

// => All procedures using authedProcedure receive ctx.user
const authedProcedure = t.procedure.use(withUser);

const appRouter = t.router({
  // => ctx.user is available - user is guaranteed authenticated
  getProfile: authedProcedure.query((opts) => {
    // => TypeScript knows ctx has BaseContext + { user: ... }
    const { user } = opts.ctx as AuthenticatedContext;
    // => user: { id: string; name: string; role: "admin"|"member"; permissions: string[] }
    return {
      id: user.id,
      name: user.name,
      role: user.role,
      canWrite: user.permissions.includes("write"),
      // => admin: canWrite = true; member: canWrite = false
    };
  }),
});

export type AppRouter = typeof appRouter;
export { appRouter };
```

**Key Takeaway**: Middleware augments context by spreading existing fields and adding new ones in `opts.next({ ctx: ... })`. Subsequent layers see the enriched context type.

**Why It Matters**: Context augmentation decouples authentication from authorization. The auth middleware validates credentials and provides `ctx.user`; individual procedures check `ctx.user.permissions` without re-validating tokens. Role-based access control, subscription tier checking, and feature flags are all naturally implemented through context augmentation. Each layer enriches context without knowledge of what comes after it.

## Group 7: Async Operations

### Example 21: Async Query Procedures

Real-world procedures are almost always async. tRPC handles Promise-returning procedures natively.

```typescript
import { initTRPC } from "@trpc/server";
import { z } from "zod";

const t = initTRPC.create();

// => Simulate async database operations with artificial delay
async function fetchUserFromDb(id: number): Promise<{ id: number; name: string; email: string } | null> {
  await new Promise((resolve) => setTimeout(resolve, 10)); // => Simulate 10ms DB query
  // => In production: return await prisma.user.findUnique({ where: { id } })
  if (id === 1) {
    return { id: 1, name: "Aisha", email: "aisha@example.com" };
  }
  return null; // => User not found
}

async function fetchUserPosts(userId: number): Promise<{ id: number; title: string }[]> {
  await new Promise((resolve) => setTimeout(resolve, 5)); // => Simulate 5ms query
  if (userId === 1) {
    return [
      { id: 1, title: "First Post" },
      { id: 2, title: "Second Post" },
    ];
  }
  return []; // => No posts found
}

const appRouter = t.router({
  // => async query: awaits database results
  getUser: t.procedure.input(z.object({ id: z.number() })).query(async (opts) => {
    // => async keyword enables await inside procedure
    const user = await fetchUserFromDb(opts.input.id);
    // => user: { id, name, email } | null (resolved after ~10ms)

    if (!user) {
      throw new Error(`User ${opts.input.id} not found`);
    }

    return user; // => Returns resolved Promise value
  }),

  // => Parallel async: fetch user and posts simultaneously
  getUserWithPosts: t.procedure.input(z.object({ id: z.number() })).query(async (opts) => {
    // => Promise.all runs both queries in parallel (~10ms total, not 15ms)
    const [user, posts] = await Promise.all([fetchUserFromDb(opts.input.id), fetchUserPosts(opts.input.id)]);
    // => user and posts both resolved in parallel

    if (!user) throw new Error("User not found");

    return { user, posts }; // => { user: {...}, posts: [{...}, {...}] }
  }),
});

export type AppRouter = typeof appRouter;
export { appRouter };
```

**Key Takeaway**: Mark procedure functions as `async` to use `await`. Use `Promise.all()` for parallel operations to minimize latency.

**Why It Matters**: Every production tRPC procedure that touches a database, external API, or file system is async. The `async/await` pattern in procedures is natural TypeScript—no special tRPC adapters needed. `Promise.all()` for parallel fetching is a common optimization: fetching user data and their posts sequentially wastes ~5ms that could be eliminated by parallel execution. At scale, these optimizations compound into meaningful latency improvements.

### Example 22: Async Mutations with Error Handling

Async mutations handle operations with side effects. Combine async/await with TRPCError for robust error handling.

```typescript
import { initTRPC, TRPCError } from "@trpc/server";
import { z } from "zod";

const t = initTRPC.create();

// => Simulate async operations with potential failures
async function saveToDatabase(data: unknown): Promise<{ id: number }> {
  await new Promise((resolve) => setTimeout(resolve, 10)); // => Simulate DB write
  if (Math.random() < 0.1) {
    // => 10% chance of simulated DB error
    throw new Error("Database connection timeout");
  }
  return { id: Math.floor(Math.random() * 1000) + 1 }; // => Returns new record ID
}

async function sendEmail(to: string, subject: string): Promise<void> {
  await new Promise((resolve) => setTimeout(resolve, 5)); // => Simulate email send
  // => In production: await emailService.send({ to, subject, body })
  console.log(`Email sent to ${to}: ${subject}`);
}

const appRouter = t.router({
  // => Async mutation with comprehensive error handling
  createAccount: t.procedure
    .input(
      z.object({
        email: z.string().email(),
        name: z.string(),
      }),
    )
    .mutation(async (opts) => {
      const { email, name } = opts.input;

      try {
        // => Step 1: Save to database
        const { id } = await saveToDatabase({ email, name });
        // => id: number (e.g., 42) - new record's ID

        // => Step 2: Send welcome email (fire-and-forget acceptable here)
        await sendEmail(email, `Welcome, ${name}!`);
        // => Email queued (could also use void sendEmail(...) to not await)

        return { success: true, userId: id }; // => Return created user info
      } catch (err) {
        // => Wrap all unexpected errors in TRPCError
        // => This prevents raw error details from leaking to clients
        throw new TRPCError({
          code: "INTERNAL_SERVER_ERROR",
          message: "Account creation failed",
          cause: err, // => Original error preserved for server-side logging
        });
      }
    }),
});

export type AppRouter = typeof appRouter;
export { appRouter };
```

**Key Takeaway**: Wrap async mutation logic in try/catch and re-throw as `TRPCError`. Always catch errors from external services (databases, email providers, payment APIs) before they reach tRPC's generic error handler.

**Why It Matters**: Unhandled async errors in procedures crash requests and leak implementation details. Explicit try/catch with `TRPCError` wrapping gives you control: log the original error server-side, send a safe message to the client. Production tRPC mutations that call Stripe, SendGrid, or database writes always follow this pattern to prevent `Error: connect ECONNREFUSED` from appearing in client error messages.

## Group 8: Procedure Input Patterns

### Example 23: Complex Nested Input Schemas

Real API inputs are often deeply nested. Zod handles nested schemas with full TypeScript inference.

```typescript
import { initTRPC } from "@trpc/server";
import { z } from "zod";

const t = initTRPC.create();

// => Nested Zod schemas compose like TypeScript interfaces
const addressSchema = z.object({
  street: z.string(),
  city: z.string(),
  country: z.string().length(2), // => ISO 2-letter country code e.g. "US", "ID"
  postalCode: z.string().optional(),
});

const createOrderSchema = z.object({
  customerId: z.string().uuid(), // => Must be valid UUID format
  items: z
    .array(
      z.object({
        productId: z.string(),
        quantity: z.number().int().positive(), // => Must be positive integer
        unitPrice: z.number().positive(), // => Must be positive number
      }),
    )
    .min(1), // => Must have at least 1 item
  shippingAddress: addressSchema, // => Reuse nested schema
  billingAddress: addressSchema.optional(), // => Optional nested schema
  notes: z.string().max(500).optional(), // => Optional, max 500 chars
});

const appRouter = t.router({
  createOrder: t.procedure.input(createOrderSchema).mutation((opts) => {
    const { customerId, items, shippingAddress, billingAddress, notes } = opts.input;
    // => customerId: string (UUID format guaranteed)
    // => items: { productId: string; quantity: number; unitPrice: number }[]
    // => shippingAddress: { street: string; city: string; country: string; postalCode?: string }
    // => billingAddress: same shape | undefined
    // => notes: string | undefined

    const total = items.reduce((sum, item) => sum + item.quantity * item.unitPrice, 0);
    // => Total: sum of (quantity × unitPrice) for all items

    return {
      orderId: `ORD-${Date.now()}`,
      customerId,
      total,
      itemCount: items.length,
      shipsTo: shippingAddress.city,
      // => e.g., { orderId: "ORD-1711300000000", total: 99.99, ... }
    };
  }),
});

export type AppRouter = typeof appRouter;
export { appRouter };
```

**Key Takeaway**: Compose complex schemas by nesting Zod objects. Reusable sub-schemas (like `addressSchema`) can be shared across multiple procedures.

**Why It Matters**: E-commerce, HR systems, and logistics APIs have deeply nested data structures. Zod schemas handle this naturally with the same nesting that TypeScript interfaces use. Reusable sub-schemas (`addressSchema`) prevent duplication across billing, shipping, and warehouse procedures. The `.min(1)` and `.positive()` validators catch business rule violations before they reach database constraints.

### Example 24: Array Input Validation

tRPC with Zod validates arrays with element-level and collection-level constraints.

```typescript
import { initTRPC } from "@trpc/server";
import { z } from "zod";

const t = initTRPC.create();

const appRouter = t.router({
  // => Batch operations on arrays of IDs
  bulkDelete: t.procedure
    .input(
      z.object({
        ids: z
          .array(z.string().uuid()) // => Each element must be a UUID
          .min(1) // => At least 1 ID required
          .max(100), // => Max 100 IDs per request (prevent abuse)
      }),
    )
    .mutation((opts) => {
      const { ids } = opts.input;
      // => ids: string[] (all UUIDs, 1-100 elements)
      console.log(`Deleting ${ids.length} items`);
      // => e.g., "Deleting 5 items"
      return { deleted: ids.length }; // => Returns count of deleted items
    }),

  // => Array of validated objects with deduplication
  upsertTags: t.procedure
    .input(
      z.object({
        tags: z
          .array(
            z.object({
              name: z.string().min(1).max(50), // => 1-50 char tag name
              color: z.string().regex(/^#[0-9a-fA-F]{6}$/), // => Valid hex color
            }),
          )
          .max(20), // => Max 20 tags at once
      }),
    )
    .mutation((opts) => {
      const { tags } = opts.input;
      // => tags: { name: string; color: string }[] (0-20 items)
      // => Each tag has valid name and hex color

      // => Process tags: normalize names to lowercase
      const normalizedTags = tags.map((tag) => ({
        ...tag,
        name: tag.name.toLowerCase(), // => "TypeScript" → "typescript"
      }));

      return { upserted: normalizedTags }; // => Returns normalized tags
    }),
});

export type AppRouter = typeof appRouter;
export { appRouter };
```

**Key Takeaway**: Use `z.array()` with min/max constraints for batch operations. Combine element-level validation with collection-level limits.

**Why It Matters**: Batch operations without size limits are a denial-of-service vector. `z.array().max(100)` prevents accidental or malicious bulk operations from overwhelming your database. Element-level validation (`z.string().uuid()`) ensures each item is valid before processing begins. Production tRPC APIs for bulk import, tag management, and batch notifications use exactly this pattern.

### Example 25: Enum and Literal Input Types

Zod enums and literals constrain input to specific allowed values, preventing invalid state.

```typescript
import { initTRPC } from "@trpc/server";
import { z } from "zod";

const t = initTRPC.create();

// => Zod enum: validates against a fixed set of string values
const statusSchema = z.enum(["pending", "active", "suspended", "deleted"]);
// => Type: "pending" | "active" | "suspended" | "deleted"

// => Zod nativeEnum: uses TypeScript enum values
enum Priority {
  LOW = "low",
  MEDIUM = "medium",
  HIGH = "high",
  CRITICAL = "critical",
}
const prioritySchema = z.nativeEnum(Priority);
// => Type: Priority (same as "low" | "medium" | "high" | "critical")

const appRouter = t.router({
  // => Filter users by status enum
  getUsersByStatus: t.procedure
    .input(
      z.object({
        status: statusSchema, // => Must be one of the 4 valid statuses
        priority: prioritySchema.optional(), // => Optional priority filter
      }),
    )
    .query((opts) => {
      const { status, priority } = opts.input;
      // => status: "pending" | "active" | "suspended" | "deleted"
      // => priority: Priority | undefined

      return {
        filter: { status, priority },
        count: 0, // => Simulated result
        users: [], // => Would filter real data
      };
    }),

  // => Update status with literal union
  updateStatus: t.procedure
    .input(
      z.object({
        userId: z.string(),
        // => Literal union: only these specific transitions allowed
        newStatus: z.union([
          z.literal("active"), // => Can activate
          z.literal("suspended"), // => Can suspend
        ]),
        // => Note: "deleted" is intentional missing - use deleteUser instead
      }),
    )
    .mutation((opts) => {
      const { userId, newStatus } = opts.input;
      // => newStatus: "active" | "suspended" (only these two)
      return { userId, updatedStatus: newStatus };
    }),
});

export type AppRouter = typeof appRouter;
export { appRouter };
```

**Key Takeaway**: Use `z.enum()` for string enums and `z.union([z.literal(...)])` for constrained sets. These prevent invalid state transitions and typos.

**Why It Matters**: Restricting inputs to valid values at the API boundary prevents entire categories of bugs. Without enum validation, a client might send `status: "activ"` (typo) or `status: "banned"` (invented value), which silently fails or creates invalid database state. Enum inputs catch these errors immediately with clear messages like "Expected 'active' | 'suspended', received 'activ'". State machine transitions modeled with literal unions document allowed transitions explicitly.

## Group 9: Router Organization

### Example 26: Modular Router Composition

Large APIs split into feature-specific routers that merge into the root AppRouter.

```typescript
import { initTRPC } from "@trpc/server";
import { z } from "zod";

const t = initTRPC.create();

// => Feature router: user-related procedures
// => Would live in: src/server/routers/users.ts
const usersRouter = t.router({
  list: t.procedure.query(() => {
    return [
      { id: 1, name: "Aisha" },
      { id: 2, name: "Omar" },
    ];
  }),

  getById: t.procedure.input(z.object({ id: z.number() })).query((opts) => {
    return { id: opts.input.id, name: "Aisha" }; // => Simulated lookup
  }),
});

// => Feature router: post-related procedures
// => Would live in: src/server/routers/posts.ts
const postsRouter = t.router({
  list: t.procedure.query(() => {
    return [
      { id: 1, title: "Hello World", authorId: 1 },
      { id: 2, title: "tRPC Guide", authorId: 1 },
    ];
  }),

  create: t.procedure.input(z.object({ title: z.string(), authorId: z.number() })).mutation((opts) => {
    return { id: 3, ...opts.input }; // => Returns new post with ID
  }),
});

// => Root router: merges feature routers
// => Would live in: src/server/router.ts
const appRouter = t.router({
  users: usersRouter, // => All user procedures under "users" namespace
  posts: postsRouter, // => All post procedures under "posts" namespace
});
// => Client accesses: trpc.users.list.query(), trpc.posts.create.mutate()

export type AppRouter = typeof appRouter;
export { appRouter };
```

**Key Takeaway**: Nest feature routers under namespaced keys in the root router. Clients access them as `trpc.users.list.query()`, `trpc.posts.create.mutate()`.

**Why It Matters**: Router composition keeps codebases organized as they grow. Without namespacing, a large API becomes a flat list of hundreds of procedures. With nested routers, related procedures group together (`trpc.users.*`, `trpc.products.*`, `trpc.orders.*`). File organization mirrors the router structure: `routers/users.ts` defines `usersRouter`, cleanly separating concerns. When onboarding, developers navigate the router tree to understand the API surface.

### Example 27: Router Merging Patterns

Routers can merge procedures from multiple sources, enabling feature flag patterns and conditional registration.

```typescript
import { initTRPC } from "@trpc/server";
import { z } from "zod";

const t = initTRPC.create();

// => Core features: always available
const coreRouter = t.router({
  health: t.procedure.query(() => ({
    status: "ok",
    version: "1.0.0",
    timestamp: new Date().toISOString(),
  })),

  greet: t.procedure.input(z.object({ name: z.string() })).query((opts) => `Hello, ${opts.input.name}!`),
});

// => Analytics features: may be disabled in some environments
const analyticsRouter = t.router({
  trackEvent: t.procedure.input(z.object({ event: z.string(), properties: z.record(z.unknown()) })).mutation((opts) => {
    console.log("Track:", opts.input.event, opts.input.properties);
    // => Logs event to analytics service
    return { tracked: true };
  }),
});

// => Feature flag: analytics enabled in production only
const ANALYTICS_ENABLED = process.env.NODE_ENV === "production";
// => true in production, false in development/test

// => Conditionally include analytics router
const appRouter = t.router({
  ...coreRouter._def.procedures, // => Merge core procedures at root level
  // => _def.procedures: internal tRPC API to access procedure definitions
  ...(ANALYTICS_ENABLED ? analyticsRouter._def.procedures : {}),
  // => Analytics procedures only in production
});

export type AppRouter = typeof appRouter;
export { appRouter };
```

**Key Takeaway**: Use router spreading with `router._def.procedures` for flat merging. Conditional spreading enables feature flags at the router level.

**Why It Matters**: Feature-gated APIs are common in staged rollouts and A/B testing. Disabling analytics in development reduces noise in logs and prevents test events from polluting analytics dashboards. The spread pattern gives flexibility beyond simple namespace nesting—procedures from different routers can coexist at the same level. This pattern also enables plugin-style architectures where third-party integrations register their own routers conditionally.

### Example 28: Type-Safe Procedure Paths

tRPC procedure paths are fully typed strings. This example demonstrates how path-based features work.

```typescript
import { initTRPC } from "@trpc/server";
import { z } from "zod";
import type { inferRouterInputs, inferRouterOutputs } from "@trpc/server";

const t = initTRPC.create();

// => Build a realistic nested router structure
const appRouter = t.router({
  users: t.router({
    profile: t.router({
      // => Deeply nested procedure: path is "users.profile.get"
      get: t.procedure.input(z.object({ userId: z.string() })).query((opts) => ({
        userId: opts.input.userId,
        displayName: "Aisha Rahman",
        bio: "Software Engineer",
      })),

      update: t.procedure
        .input(
          z.object({
            userId: z.string(),
            displayName: z.string().optional(),
            bio: z.string().max(500).optional(),
          }),
        )
        .mutation((opts) => ({
          ...opts.input,
          updatedAt: new Date().toISOString(),
        })),
    }),
  }),

  posts: t.router({
    // => Path: "posts.list"
    list: t.procedure.query(() => [{ id: 1, title: "Hello" }]),
  }),
});

export type AppRouter = typeof appRouter;
export { appRouter };

// => Extract types at any depth in the router tree
type Inputs = inferRouterInputs<AppRouter>;
// => Inputs["users"]["profile"]["get"] = { userId: string }
// => Inputs["users"]["profile"]["update"] = { userId: string; displayName?: string; bio?: string }
// => Inputs["posts"]["list"] = void (no input)

type Outputs = inferRouterOutputs<AppRouter>;
// => Outputs["users"]["profile"]["get"] = { userId: string; displayName: string; bio: string }

// => These types can be used anywhere in the codebase
type ProfileData = Outputs["users"]["profile"]["get"];
// => { userId: string; displayName: string; bio: string }

type _unused = Inputs | ProfileData; // => Suppress unused warning
```

**Key Takeaway**: Deeply nested routers create typed path hierarchies. `inferRouterInputs` and `inferRouterOutputs` work at any depth to extract procedure types.

**Why It Matters**: Deep nesting mirrors your application's domain model. A user profile, user settings, and user notifications logically group under `users.*`. Type inference at any depth means frontend code can reference `Outputs["users"]["profile"]["get"]` without manually duplicating the type definition. When the server type changes, TypeScript flags every stale client reference immediately. This is the culmination of beginner-level tRPC—you now have a complete mental model of type-safe API development.
