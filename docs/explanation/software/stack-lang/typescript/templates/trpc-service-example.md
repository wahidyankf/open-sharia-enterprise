---
title: TypeScript tRPC Service Example
description: Complete tRPC server implementation with TypeScript, routers, procedures, input validation, middleware, and client usage for type-safe RPC APIs
category: template
tags:
  - typescript
  - trpc
  - rpc
  - type-safe-api
  - zod
  - procedures
  - routers
  - ts-5.0
  - ts-5.1
  - ts-5.2
  - ts-5.3
  - ts-5.4
  - ts-5.5
  - ts-5.6
  - ts-5.9
related:
  - http-server-example.md
  - service-layer-template.md
  - project-structure.md
  - ex-so-stla-ts__best-practices.md
principles:
  - simplicity-over-complexity
  - explicit-over-implicit
created: 2026-01-24
updated: 2026-01-24
---

# TypeScript tRPC Service Example

Complete tRPC server implementation with type-safe procedures, routers, and client usage.

## Table of Contents

1. [Overview](#overview)
2. [Setup](#setup)
3. [Context](#context)
4. [Routers and Procedures](#routers-and-procedures)
5. [Input Validation](#input-validation)
6. [Middleware](#middleware)
7. [Client Usage](#client-usage)
8. [Testing](#testing)

## Overview

**tRPC** provides end-to-end type safety for TypeScript applications without code generation. This example demonstrates:

- Type-safe RPC calls
- Zod schema validation
- Middleware (authentication, logging)
- Context management
- Client setup

## Setup

### Installation

```bash
npm install @trpc/server @trpc/client zod
npm install --save-dev @trpc/react-query # For React clients
```

### Project Structure

```
src/
├── infrastructure/
│   └── trpc/
│       ├── context.ts           # tRPC context
│       ├── router.ts            # App router
│       ├── routers/
│       │   ├── donationRouter.ts
│       │   └── index.ts
│       ├── middleware/
│       │   ├── auth.ts
│       │   └── logger.ts
│       ├── procedures.ts        # Base procedures
│       └── server.ts            # HTTP server setup
```

## Context

### Create Context

```typescript
// src/infrastructure/trpc/context.ts
import { CreateExpressContextOptions } from "@trpc/server/adapters/express";
import { DonationRepository } from "@/domain/repositories/DonationRepository";
import { ProcessDonationService } from "@/application/services/ProcessDonationService";
import { Logger } from "@/infrastructure/logging/Logger";

export interface Context {
  user?: {
    id: string;
    email: string;
  };
  services: {
    processDonationService: ProcessDonationService;
    createDonationService: CreateDonationService;
  };
  logger: Logger;
}

export const createContext = ({ req, res }: CreateExpressContextOptions): Context => {
  // Extract user from JWT token (if authenticated)
  const user = extractUserFromToken(req.headers.authorization);

  return {
    user,
    services: {
      processDonationService: container.get(ProcessDonationService),
      createDonationService: container.get(CreateDonationService),
    },
    logger: container.get(Logger),
  };
};

function extractUserFromToken(authHeader?: string): { id: string; email: string } | undefined {
  if (!authHeader) return undefined;

  try {
    const token = authHeader.split(" ")[1];
    const decoded = jwt.verify(token, process.env.JWT_SECRET!);
    return decoded as { id: string; email: string };
  } catch {
    return undefined;
  }
}

export type Context = Awaited<ReturnType<typeof createContext>>;
```

## Routers and Procedures

### Base Procedures

```typescript
// src/infrastructure/trpc/procedures.ts
import { initTRPC, TRPCError } from "@trpc/server";
import { Context } from "./context";

const t = initTRPC.context<Context>().create();

export const router = t.router;
export const publicProcedure = t.procedure;

// Authenticated procedure (requires user)
export const protectedProcedure = t.procedure.use(async ({ ctx, next }) => {
  if (!ctx.user) {
    throw new TRPCError({
      code: "UNAUTHORIZED",
      message: "You must be logged in",
    });
  }

  return next({
    ctx: {
      ...ctx,
      user: ctx.user, // Type-safe authenticated user
    },
  });
});
```

### Donation Router

```typescript
// src/infrastructure/trpc/routers/donationRouter.ts
import { z } from "zod";
import { router, publicProcedure, protectedProcedure } from "../procedures";
import { DonationID } from "@/domain/value-objects/DonationID";
import { Money } from "@/domain/value-objects/Money";
import { UserID } from "@/domain/value-objects/UserID";
import { TRPCError } from "@trpc/server";

export const donationRouter = router({
  // ========================================
  // Create Donation (Protected)
  // ========================================

  create: protectedProcedure
    .input(
      z.object({
        donorId: z.string().uuid(),
        recipientId: z.string().uuid(),
        amountCents: z.number().int().positive(),
        currency: z.string().length(3),
      }),
    )
    .mutation(async ({ input, ctx }) => {
      const result = await ctx.services.createDonationService.execute({
        donorId: DonorID.create(input.donorId).unwrap(),
        recipientId: RecipientID.create(input.recipientId).unwrap(),
        amount: Money.fromCents(input.amountCents, input.currency).unwrap(),
        createdBy: UserID.create(ctx.user.id).unwrap(),
      });

      if (result.isErr()) {
        throw new TRPCError({
          code: "BAD_REQUEST",
          message: result.error.message,
        });
      }

      return {
        donationId: result.value.donationId.value,
        createdAt: result.value.createdAt,
      };
    }),

  // ========================================
  // Get Donation by ID (Public)
  // ========================================

  getById: publicProcedure.input(z.object({ id: z.string().uuid() })).query(async ({ input, ctx }) => {
    const donationId = DonationID.create(input.id);
    if (donationId.isErr()) {
      throw new TRPCError({
        code: "BAD_REQUEST",
        message: "Invalid donation ID",
      });
    }

    const result = await ctx.services.getDonationService.execute({
      donationId: donationId.value,
    });

    if (result.isErr()) {
      throw new TRPCError({
        code: "NOT_FOUND",
        message: "Donation not found",
      });
    }

    return {
      id: result.value.donation.id.value,
      amount: result.value.donation.getAmount().toCents(),
      currency: result.value.donation.getAmount().getCurrency(),
      status: result.value.donation.getStatus().value,
      createdAt: result.value.donation.getCreatedAt(),
    };
  }),

  // ========================================
  // Process Donation (Protected)
  // ========================================

  process: protectedProcedure
    .input(
      z.object({
        donationId: z.string().uuid(),
      }),
    )
    .mutation(async ({ input, ctx }) => {
      const result = await ctx.services.processDonationService.execute({
        donationId: DonationID.create(input.donationId).unwrap(),
        processedBy: UserID.create(ctx.user.id).unwrap(),
      });

      if (result.isErr()) {
        if (result.error.name === "DonationNotFoundError") {
          throw new TRPCError({
            code: "NOT_FOUND",
            message: "Donation not found",
          });
        }

        throw new TRPCError({
          code: "BAD_REQUEST",
          message: result.error.message,
        });
      }

      return {
        donationId: result.value.donationId.value,
        status: result.value.status.value,
        processedAt: result.value.processedAt,
      };
    }),

  // ========================================
  // List Donations (Protected)
  // ========================================

  list: protectedProcedure
    .input(
      z.object({
        limit: z.number().min(1).max(100).default(10),
        cursor: z.string().uuid().optional(),
      }),
    )
    .query(async ({ input, ctx }) => {
      const result = await ctx.services.listDonationsService.execute({
        userId: UserID.create(ctx.user.id).unwrap(),
        limit: input.limit,
        cursor: input.cursor ? DonationID.create(input.cursor).unwrap() : undefined,
      });

      if (result.isErr()) {
        throw new TRPCError({
          code: "INTERNAL_SERVER_ERROR",
          message: "Failed to list donations",
        });
      }

      return {
        donations: result.value.donations.map((d) => ({
          id: d.id.value,
          amount: d.getAmount().toCents(),
          currency: d.getAmount().getCurrency(),
          status: d.getStatus().value,
          createdAt: d.getCreatedAt(),
        })),
        nextCursor: result.value.nextCursor?.value,
      };
    }),
});
```

### App Router

```typescript
// src/infrastructure/trpc/router.ts
import { router } from "./procedures";
import { donationRouter } from "./routers/donationRouter";
import { accountRouter } from "./routers/accountRouter";

export const appRouter = router({
  donation: donationRouter,
  account: accountRouter,
});

export type AppRouter = typeof appRouter;
```

## Input Validation

### Zod Schemas

```typescript
// src/infrastructure/trpc/schemas/donation.ts
import { z } from "zod";

export const createDonationSchema = z.object({
  donorId: z.string().uuid("Invalid donor ID"),
  recipientId: z.string().uuid("Invalid recipient ID"),
  amountCents: z.number().int().positive("Amount must be positive"),
  currency: z.string().length(3, "Currency code must be 3 characters").toUpperCase(),
  notes: z.string().max(500, "Notes too long").optional(),
});

export const processDonationSchema = z.object({
  donationId: z.string().uuid("Invalid donation ID"),
});

export const listDonationsSchema = z.object({
  limit: z.number().min(1).max(100).default(10),
  cursor: z.string().uuid().optional(),
  status: z.enum(["PENDING", "PROCESSED", "CONFIRMED", "CANCELLED"]).optional(),
});

// Reusable schemas
export const donationIdSchema = z.object({
  id: z.string().uuid(),
});
```

### Usage in Procedures

```typescript
import { createDonationSchema, donationIdSchema } from "../schemas/donation";

export const donationRouter = router({
  create: protectedProcedure.input(createDonationSchema).mutation(async ({ input, ctx }) => {
    // input is fully typed and validated!
  }),

  getById: publicProcedure.input(donationIdSchema).query(async ({ input, ctx }) => {
    // input.id is guaranteed to be a valid UUID
  }),
});
```

## Middleware

### Logging Middleware

```typescript
// src/infrastructure/trpc/middleware/logger.ts
import { middleware } from "../procedures";

export const loggerMiddleware = middleware(async ({ ctx, path, type, next }) => {
  const start = Date.now();

  ctx.logger.info("tRPC request started", {
    path,
    type,
    userId: ctx.user?.id,
  });

  const result = await next();

  const duration = Date.now() - start;

  if (result.ok) {
    ctx.logger.info("tRPC request completed", {
      path,
      type,
      duration,
      userId: ctx.user?.id,
    });
  } else {
    ctx.logger.error("tRPC request failed", {
      path,
      type,
      duration,
      error: result.error,
      userId: ctx.user?.id,
    });
  }

  return result;
});

// Apply to all procedures
export const loggedProcedure = publicProcedure.use(loggerMiddleware);
```

### Rate Limiting Middleware

```typescript
// src/infrastructure/trpc/middleware/rateLimit.ts
import { TRPCError } from "@trpc/server";
import { middleware } from "../procedures";

const rateLimits = new Map<string, { count: number; resetAt: number }>();

export const rateLimitMiddleware = (maxRequests: number, windowMs: number) => {
  return middleware(async ({ ctx, next }) => {
    const userId = ctx.user?.id || "anonymous";
    const now = Date.now();

    let limit = rateLimits.get(userId);

    if (!limit || now > limit.resetAt) {
      limit = { count: 0, resetAt: now + windowMs };
      rateLimits.set(userId, limit);
    }

    if (limit.count >= maxRequests) {
      throw new TRPCError({
        code: "TOO_MANY_REQUESTS",
        message: "Rate limit exceeded",
      });
    }

    limit.count++;

    return next();
  });
};

// Usage
export const rateLimitedProcedure = publicProcedure.use(rateLimitMiddleware(100, 60000)); // 100 requests per minute
```

## Client Usage

### Server Setup

```typescript
// src/infrastructure/trpc/server.ts
import express from "express";
import * as trpcExpress from "@trpc/server/adapters/express";
import { appRouter } from "./router";
import { createContext } from "./context";

const app = express();

app.use(
  "/trpc",
  trpcExpress.createExpressMiddleware({
    router: appRouter,
    createContext,
  }),
);

app.listen(3000, () => {
  console.log("tRPC server listening on port 3000");
});
```

### TypeScript Client

```typescript
// src/client/trpc.ts
import { createTRPCProxyClient, httpBatchLink } from "@trpc/client";
import type { AppRouter } from "@/infrastructure/trpc/router";

export const trpc = createTRPCProxyClient<AppRouter>({
  links: [
    httpBatchLink({
      url: "http://localhost:3000/trpc",
      headers() {
        return {
          authorization: `Bearer ${getAuthToken()}`,
        };
      },
    }),
  ],
});

// Usage
async function example() {
  // Create donation (fully typed!)
  const donation = await trpc.donation.create.mutate({
    donorId: "donor-123",
    recipientId: "recipient-456",
    amountCents: 10000,
    currency: "USD",
  });

  console.log("Donation created:", donation.donationId);

  // Get donation by ID
  const retrieved = await trpc.donation.getById.query({
    id: donation.donationId,
  });

  console.log("Retrieved donation:", retrieved);

  // Process donation
  const processed = await trpc.donation.process.mutate({
    donationId: donation.donationId,
  });

  console.log("Donation processed:", processed.status);
}
```

### React Client

```typescript
// src/client/hooks/trpc.ts
import { createTRPCReact } from "@trpc/react-query";
import type { AppRouter } from "@/infrastructure/trpc/router";

export const trpc = createTRPCReact<AppRouter>();

// App.tsx
import { QueryClient, QueryClientProvider } from "@tanstack/react-query";
import { httpBatchLink } from "@trpc/client";
import { trpc } from "./hooks/trpc";

const queryClient = new QueryClient();

const trpcClient = trpc.createClient({
  links: [
    httpBatchLink({
      url: "http://localhost:3000/trpc",
    }),
  ],
});

function App() {
  return (
    <trpc.Provider client={trpcClient} queryClient={queryClient}>
      <QueryClientProvider client={queryClient}>
        <DonationList />
      </QueryClientProvider>
    </trpc.Provider>
  );
}

// Component using tRPC hooks
function DonationList() {
  const { data, isLoading } = trpc.donation.list.useQuery({
    limit: 10,
  });

  const createMutation = trpc.donation.create.useMutation();

  const handleCreate = async () => {
    await createMutation.mutateAsync({
      donorId: "donor-123",
      recipientId: "recipient-456",
      amountCents: 10000,
      currency: "USD",
    });
  };

  if (isLoading) return <div>Loading...</div>;

  return (
    <div>
      <button onClick={handleCreate}>Create Donation</button>
      <ul>
        {data?.donations.map((donation) => (
          <li key={donation.id}>
            {donation.amount} {donation.currency} - {donation.status}
          </li>
        ))}
      </ul>
    </div>
  );
}
```

## Testing

### Procedure Tests

```typescript
// tests/infrastructure/trpc/routers/donationRouter.test.ts
import { describe, it, expect, vi, beforeEach } from "vitest";
import { createCallerFactory } from "@trpc/server";
import { appRouter } from "@/infrastructure/trpc/router";
import { Context } from "@/infrastructure/trpc/context";
import { Ok, Err } from "@/shared/result";

describe("donationRouter", () => {
  let mockProcessDonationService: jest.Mocked<ProcessDonationService>;
  let createCaller: ReturnType<typeof createCallerFactory<typeof appRouter>>;

  beforeEach(() => {
    mockProcessDonationService = {
      execute: vi.fn(),
    } as any;

    createCaller = createCallerFactory(appRouter);
  });

  it("should create donation successfully", async () => {
    const ctx: Context = {
      user: { id: "user-1", email: "test@example.com" },
      services: {
        createDonationService: {
          execute: vi.fn().mockResolvedValue(
            Ok({
              donationId: DonationID.create("donation-1").unwrap(),
              createdAt: new Date(),
            }),
          ),
        },
      },
      logger: console,
    } as any;

    const caller = createCaller(ctx);

    const result = await caller.donation.create({
      donorId: "donor-1",
      recipientId: "recipient-1",
      amountCents: 10000,
      currency: "USD",
    });

    expect(result.donationId).toBe("donation-1");
  });

  it("should throw UNAUTHORIZED for unauthenticated user", async () => {
    const ctx: Context = {
      user: undefined, // No user
      services: {},
      logger: console,
    } as any;

    const caller = createCaller(ctx);

    await expect(
      caller.donation.create({
        donorId: "donor-1",
        recipientId: "recipient-1",
        amountCents: 10000,
        currency: "USD",
      }),
    ).rejects.toThrow("UNAUTHORIZED");
  });
});
```

### Integration Tests

```typescript
// tests/integration/trpc/donationRouter.integration.test.ts
import { describe, it, expect, beforeAll, afterAll } from "vitest";
import { createTRPCProxyClient, httpBatchLink } from "@trpc/client";
import type { AppRouter } from "@/infrastructure/trpc/router";
import { DataSource } from "typeorm";

describe("Donation tRPC Integration", () => {
  let client: ReturnType<typeof createTRPCProxyClient<AppRouter>>;
  let dataSource: DataSource;

  beforeAll(async () => {
    dataSource = new DataSource({
      type: "postgres",
      host: "localhost",
      port: 5432,
      username: "test",
      password: "test",
      database: "test_db",
      synchronize: true,
      entities: [DonationEntity],
    });

    await dataSource.initialize();

    // Start tRPC server
    // ...

    client = createTRPCProxyClient<AppRouter>({
      links: [
        httpBatchLink({
          url: "http://localhost:3000/trpc",
          headers: {
            authorization: "Bearer test-token",
          },
        }),
      ],
    });
  });

  afterAll(async () => {
    await dataSource.destroy();
  });

  it("should create and process donation end-to-end", async () => {
    // Create donation
    const created = await client.donation.create.mutate({
      donorId: "donor-1",
      recipientId: "recipient-1",
      amountCents: 10000,
      currency: "USD",
    });

    expect(created.donationId).toBeDefined();

    // Get donation
    const retrieved = await client.donation.getById.query({
      id: created.donationId,
    });

    expect(retrieved.status).toBe("PENDING");

    // Process donation
    const processed = await client.donation.process.mutate({
      donationId: created.donationId,
    });

    expect(processed.status).toBe("PROCESSED");
  });
});
```

## Related Documentation

- [HTTP Server Example](./http-server-example.md) - Express/Fastify REST APIs
- [Service Layer Template](./service-layer-template.md) - Application services
- [Project Structure](./project-structure.md) - Hexagonal architecture
- [TypeScript Best Practices](../ex-so-stla-ts__best-practices.md) - Coding standards

---

**Last Updated**: 2026-01-24
**TypeScript Version**: 5.0+
**tRPC Version**: 10.x
**Zod Version**: 3.x
