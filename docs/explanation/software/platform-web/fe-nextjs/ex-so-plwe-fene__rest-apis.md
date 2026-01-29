---
title: Next.js REST APIs Integration
description: Comprehensive guide to working with REST APIs in Next.js including Route Handlers, external API consumption, Server Actions vs API routes, authentication, CORS, and financial API integration examples
category: explanation
tags:
  - nextjs
  - rest-api
  - route-handlers
  - typescript
  - server-actions
  - api-integration
created: 2026-01-29
updated: 2026-01-29
---

# Next.js REST APIs Integration

This document provides comprehensive guidance on working with REST APIs in Next.js applications. Next.js offers multiple approaches for API integration: Route Handlers for creating API endpoints, Server Components for consuming external APIs, and Server Actions for mutations. Understanding when to use each approach is essential for building scalable applications.

**Prerequisites**: Familiarity with [Server Components](./ex-so-plwe-fene__server-components.md), [data fetching](./ex-so-plwe-fene__data-fetching.md), and [REST API concepts](https://restfulapi.net/).

## 🎯 Route Handlers (API Routes)

Route Handlers define API endpoints in the App Router using `route.ts` files.

### Basic GET Route Handler

```typescript
// app/api/zakat/rates/route.ts
import { NextRequest, NextResponse } from "next/server";
import { db } from "@/lib/db";

export async function GET(request: NextRequest) {
  try {
    const rates = await db.zakatRate.findFirst({
      orderBy: { createdAt: "desc" },
    });

    if (!rates) {
      return NextResponse.json({ error: "Rates not found" }, { status: 404 });
    }

    return NextResponse.json(rates);
  } catch (error) {
    return NextResponse.json({ error: "Internal server error" }, { status: 500 });
  }
}
```

### POST Route Handler with Validation

```typescript
// app/api/contracts/route.ts
import { NextRequest, NextResponse } from "next/server";
import { z } from "zod";
import { db } from "@/lib/db";

const contractSchema = z.object({
  principalAmount: z.number().positive().max(1000000),
  profitMargin: z.number().min(0).max(100),
  termMonths: z.number().int().positive().max(360),
});

export async function POST(request: NextRequest) {
  try {
    const body = await request.json();
    const validatedData = contractSchema.parse(body);

    const totalAmount = validatedData.principalAmount * (1 + validatedData.profitMargin / 100);

    const contract = await db.murabahaContract.create({
      data: {
        ...validatedData,
        totalAmount,
        status: "pending",
      },
    });

    return NextResponse.json(contract, {
      status: 201,
      headers: {
        Location: `/api/contracts/${contract.id}`,
      },
    });
  } catch (error) {
    if (error instanceof z.ZodError) {
      return NextResponse.json(
        {
          error: "Validation failed",
          details: error.errors,
        },
        { status: 400 },
      );
    }

    return NextResponse.json({ error: "Internal server error" }, { status: 500 });
  }
}
```

### Dynamic Route Handler

```typescript
// app/api/contracts/[id]/route.ts
import { NextRequest, NextResponse } from "next/server";
import { db } from "@/lib/db";

interface RouteContext {
  params: { id: string };
}

export async function GET(request: NextRequest, context: RouteContext) {
  try {
    const contract = await db.murabahaContract.findUnique({
      where: { id: context.params.id },
    });

    if (!contract) {
      return NextResponse.json({ error: "Contract not found" }, { status: 404 });
    }

    return NextResponse.json(contract);
  } catch (error) {
    return NextResponse.json({ error: "Internal server error" }, { status: 500 });
  }
}

export async function PUT(request: NextRequest, context: RouteContext) {
  try {
    const body = await request.json();

    const contract = await db.murabahaContract.update({
      where: { id: context.params.id },
      data: body,
    });

    return NextResponse.json(contract);
  } catch (error) {
    return NextResponse.json({ error: "Failed to update contract" }, { status: 500 });
  }
}

export async function DELETE(request: NextRequest, context: RouteContext) {
  try {
    await db.murabahaContract.delete({
      where: { id: context.params.id },
    });

    return NextResponse.json({ success: true }, { status: 204 });
  } catch (error) {
    return NextResponse.json({ error: "Failed to delete contract" }, { status: 500 });
  }
}
```

### Request Handling

```typescript
// app/api/donations/route.ts
import { NextRequest, NextResponse } from "next/server";

export async function GET(request: NextRequest) {
  // Read search params
  const searchParams = request.nextUrl.searchParams;
  const page = Number(searchParams.get("page")) || 1;
  const limit = Number(searchParams.get("limit")) || 10;

  // Read headers
  const authorization = request.headers.get("authorization");

  // Read cookies
  const token = request.cookies.get("auth-token");

  // Fetch paginated data
  const donations = await db.donation.findMany({
    skip: (page - 1) * limit,
    take: limit,
    orderBy: { createdAt: "desc" },
  });

  const totalCount = await db.donation.count();

  // Set response headers
  const response = NextResponse.json({
    donations,
    pagination: {
      page,
      limit,
      totalCount,
      totalPages: Math.ceil(totalCount / limit),
    },
  });

  response.headers.set("X-Total-Count", String(totalCount));
  response.cookies.set("last-page", String(page));

  return response;
}
```

## 🌐 External API Consumption

### Fetching External APIs in Server Components

```typescript
// app/currency/page.tsx (Server Component)
interface ExchangeRate {
  currency: string;
  rate: number;
  lastUpdated: string;
}

export default async function CurrencyPage() {
  // Fetch external API
  const response = await fetch(
    'https://api.exchangerate.com/v1/rates/USD',
    {
      next: { revalidate: 3600 }, // Revalidate every hour
    }
  );

  if (!response.ok) {
    throw new Error('Failed to fetch exchange rates');
  }

  const data = await response.json() as ExchangeRate;

  return (
    <div>
      <h1>Currency Exchange Rates</h1>
      <p>USD to IDR: {data.rate}</p>
      <p>Last updated: {data.lastUpdated}</p>
    </div>
  );
}
```

### API Client Pattern

Create reusable API client for external services:

```typescript
// lib/api/zakat-api-client.ts
const BASE_URL = process.env.ZAKAT_API_URL || "https://api.zakat.example.com";

interface ZakatRatesResponse {
  goldNisab: number;
  silverNisab: number;
  currency: string;
}

export async function getZakatRates(currency: string = "USD"): Promise<ZakatRatesResponse> {
  const response = await fetch(`${BASE_URL}/rates?currency=${currency}`, {
    headers: {
      Authorization: `Bearer ${process.env.ZAKAT_API_KEY}`,
      "Content-Type": "application/json",
    },
    next: { revalidate: 3600 },
  });

  if (!response.ok) {
    throw new Error(`Failed to fetch zakat rates: ${response.status}`);
  }

  return response.json();
}

export async function calculateZakatRemote(
  wealth: number,
  nisab: number,
): Promise<{ zakatAmount: number; eligible: boolean }> {
  const response = await fetch(`${BASE_URL}/calculate`, {
    method: "POST",
    headers: {
      Authorization: `Bearer ${process.env.ZAKAT_API_KEY}`,
      "Content-Type": "application/json",
    },
    body: JSON.stringify({ wealth, nisab }),
    cache: "no-store", // Always fetch fresh
  });

  if (!response.ok) {
    throw new Error(`Failed to calculate zakat: ${response.status}`);
  }

  return response.json();
}

// Usage in Server Component
// const rates = await getZakatRates('USD');
```

### Error Handling for External APIs

```typescript
// lib/api/murabaha-api-client.ts
import { z } from "zod";

const contractResponseSchema = z.object({
  id: z.string(),
  principalAmount: z.number(),
  profitMargin: z.number(),
  status: z.enum(["pending", "active", "completed", "cancelled"]),
});

type ContractResponse = z.infer<typeof contractResponseSchema>;

export async function fetchContract(id: string): Promise<ContractResponse | null> {
  try {
    const response = await fetch(`https://api.murabaha.example.com/contracts/${id}`, {
      headers: {
        Authorization: `Bearer ${process.env.MURABAHA_API_KEY}`,
      },
      next: { revalidate: 60 },
    });

    if (response.status === 404) {
      return null;
    }

    if (!response.ok) {
      throw new Error(`API error: ${response.status}`);
    }

    const data = await response.json();

    // Validate response schema
    return contractResponseSchema.parse(data);
  } catch (error) {
    console.error("Failed to fetch contract:", error);
    throw error;
  }
}
```

## 🔄 Server Actions vs Route Handlers

### When to Use Server Actions

Use Server Actions for:

- Form submissions
- Mutations (create, update, delete)
- Progressive enhancement
- Direct component-to-server communication

```typescript
// app/_actions/donation-actions.ts
"use server";

import { z } from "zod";
import { revalidatePath } from "next/cache";
import { db } from "@/lib/db";

const donationSchema = z.object({
  amount: z.number().positive(),
  donorName: z.string().min(1),
  donorEmail: z.string().email(),
});

export async function createDonation(formData: FormData) {
  const validatedData = donationSchema.parse({
    amount: Number(formData.get("amount")),
    donorName: String(formData.get("donorName")),
    donorEmail: String(formData.get("donorEmail")),
  });

  const donation = await db.donation.create({
    data: validatedData,
  });

  revalidatePath("/donations");

  return { success: true, donationId: donation.id };
}
```

### When to Use Route Handlers

Use Route Handlers for:

- Public APIs consumed by external clients
- Webhooks (receiving external events)
- Custom request/response handling
- Non-form submissions
- Third-party integrations

```typescript
// app/api/webhooks/stripe/route.ts
import { NextRequest, NextResponse } from "next/server";
import Stripe from "stripe";

const stripe = new Stripe(process.env.STRIPE_SECRET_KEY!);

export async function POST(request: NextRequest) {
  const body = await request.text();
  const signature = request.headers.get("stripe-signature")!;

  try {
    const event = stripe.webhooks.constructEvent(body, signature, process.env.STRIPE_WEBHOOK_SECRET!);

    if (event.type === "payment_intent.succeeded") {
      const paymentIntent = event.data.object;
      // Handle successful payment
      await handleSuccessfulPayment(paymentIntent);
    }

    return NextResponse.json({ received: true });
  } catch (error) {
    return NextResponse.json({ error: "Webhook error" }, { status: 400 });
  }
}
```

## 🔐 Authentication and Authorization

### API Key Authentication

```typescript
// app/api/admin/contracts/route.ts
import { NextRequest, NextResponse } from "next/server";

function isAuthorized(request: NextRequest): boolean {
  const apiKey = request.headers.get("x-api-key");
  return apiKey === process.env.ADMIN_API_KEY;
}

export async function GET(request: NextRequest) {
  if (!isAuthorized(request)) {
    return NextResponse.json({ error: "Unauthorized" }, { status: 401 });
  }

  const contracts = await db.contract.findMany();
  return NextResponse.json(contracts);
}
```

### JWT Authentication

```typescript
// lib/auth/jwt.ts
import { jwtVerify, SignJWT } from "jose";

const secret = new TextEncoder().encode(process.env.JWT_SECRET);

export async function verifyToken(token: string) {
  try {
    const verified = await jwtVerify(token, secret);
    return verified.payload;
  } catch (error) {
    return null;
  }
}

// app/api/protected/route.ts
import { NextRequest, NextResponse } from "next/server";
import { verifyToken } from "@/lib/auth/jwt";

export async function GET(request: NextRequest) {
  const authHeader = request.headers.get("authorization");

  if (!authHeader?.startsWith("Bearer ")) {
    return NextResponse.json({ error: "Missing token" }, { status: 401 });
  }

  const token = authHeader.substring(7);
  const payload = await verifyToken(token);

  if (!payload) {
    return NextResponse.json({ error: "Invalid token" }, { status: 401 });
  }

  // Access protected resource
  const data = await fetchProtectedData(payload.userId as string);

  return NextResponse.json(data);
}
```

## 🌍 CORS Configuration

### Basic CORS Setup

```typescript
// app/api/public/route.ts
import { NextRequest, NextResponse } from "next/server";

export async function GET(request: NextRequest) {
  const data = { message: "Public API endpoint" };

  return NextResponse.json(data, {
    headers: {
      "Access-Control-Allow-Origin": "*",
      "Access-Control-Allow-Methods": "GET, POST, PUT, DELETE, OPTIONS",
      "Access-Control-Allow-Headers": "Content-Type, Authorization",
    },
  });
}

export async function OPTIONS(request: NextRequest) {
  return new NextResponse(null, {
    status: 200,
    headers: {
      "Access-Control-Allow-Origin": "*",
      "Access-Control-Allow-Methods": "GET, POST, PUT, DELETE, OPTIONS",
      "Access-Control-Allow-Headers": "Content-Type, Authorization",
    },
  });
}
```

### CORS Middleware Pattern

```typescript
// lib/middleware/cors.ts
import { NextResponse } from "next/server";

const allowedOrigins = ["https://example.com", "https://app.example.com"];

export function corsHeaders(origin: string | null) {
  const isAllowed = origin && allowedOrigins.includes(origin);

  return {
    "Access-Control-Allow-Origin": isAllowed ? origin : allowedOrigins[0],
    "Access-Control-Allow-Credentials": "true",
    "Access-Control-Allow-Methods": "GET, POST, PUT, DELETE, OPTIONS",
    "Access-Control-Allow-Headers": "Content-Type, Authorization",
  };
}

// app/api/contracts/route.ts
import { NextRequest, NextResponse } from "next/server";
import { corsHeaders } from "@/lib/middleware/cors";

export async function GET(request: NextRequest) {
  const origin = request.headers.get("origin");
  const contracts = await fetchContracts();

  return NextResponse.json(contracts, {
    headers: corsHeaders(origin),
  });
}
```

## ⚡ Rate Limiting

### In-Memory Rate Limiting

```typescript
// lib/rate-limit.ts
const rateLimitMap = new Map<string, { count: number; resetTime: number }>();

export function rateLimit(ip: string, limit: number = 100, windowMs: number = 60000): boolean {
  const now = Date.now();
  const record = rateLimitMap.get(ip);

  if (!record || now > record.resetTime) {
    rateLimitMap.set(ip, { count: 1, resetTime: now + windowMs });
    return true;
  }

  if (record.count >= limit) {
    return false;
  }

  record.count += 1;
  return true;
}

// app/api/zakat/calculate/route.ts
import { NextRequest, NextResponse } from "next/server";
import { rateLimit } from "@/lib/rate-limit";

export async function POST(request: NextRequest) {
  const ip = request.ip || "unknown";

  if (!rateLimit(ip, 10, 60000)) {
    return NextResponse.json({ error: "Rate limit exceeded" }, { status: 429 });
  }

  // Process request
  const result = await calculateZakat(request);
  return NextResponse.json(result);
}
```

## 💰 Financial API Integration Examples

### Zakat Calculation API

```typescript
// app/api/zakat/calculate/route.ts
import { NextRequest, NextResponse } from "next/server";
import { z } from "zod";

const zakatCalculationSchema = z.object({
  wealth: z.number().positive(),
  nisab: z.number().positive(),
  currency: z.string().length(3),
});

export async function POST(request: NextRequest) {
  try {
    const body = await request.json();
    const { wealth, nisab, currency } = zakatCalculationSchema.parse(body);

    // Business logic
    const eligible = wealth >= nisab;
    const zakatAmount = eligible ? wealth * 0.025 : 0;

    // Convert to requested currency if needed
    const convertedAmount = await convertCurrency(zakatAmount, "USD", currency);

    return NextResponse.json({
      eligible,
      zakatAmount: convertedAmount,
      currency,
      nisabThreshold: nisab,
      calculatedAt: new Date().toISOString(),
    });
  } catch (error) {
    if (error instanceof z.ZodError) {
      return NextResponse.json({ error: "Invalid input", details: error.errors }, { status: 400 });
    }

    return NextResponse.json({ error: "Calculation failed" }, { status: 500 });
  }
}
```

### Murabaha Contract API

```typescript
// app/api/murabaha/contracts/route.ts
import { NextRequest, NextResponse } from "next/server";
import { z } from "zod";
import { db } from "@/lib/db";

const contractSchema = z.object({
  principalAmount: z.number().positive().max(1000000),
  profitMargin: z.number().min(0).max(100),
  termMonths: z.number().int().positive().max(360),
  customerName: z.string().min(1),
  customerEmail: z.string().email(),
});

export async function POST(request: NextRequest) {
  try {
    const body = await request.json();
    const validatedData = contractSchema.parse(body);

    // Calculate total amount
    const totalAmount = validatedData.principalAmount * (1 + validatedData.profitMargin / 100);

    // Calculate monthly payment
    const monthlyPayment = totalAmount / validatedData.termMonths;

    // Create contract
    const contract = await db.murabahaContract.create({
      data: {
        ...validatedData,
        totalAmount,
        monthlyPayment,
        status: "pending",
        createdAt: new Date(),
      },
    });

    return NextResponse.json(
      {
        contractId: contract.id,
        principalAmount: contract.principalAmount,
        profitMargin: contract.profitMargin,
        totalAmount: contract.totalAmount,
        monthlyPayment: contract.monthlyPayment,
        termMonths: contract.termMonths,
        status: contract.status,
      },
      { status: 201 },
    );
  } catch (error) {
    if (error instanceof z.ZodError) {
      return NextResponse.json({ error: "Validation failed", details: error.errors }, { status: 400 });
    }

    return NextResponse.json({ error: "Failed to create contract" }, { status: 500 });
  }
}
```

## 🔗 Related Documentation

**Next.js Core**:

- [Data Fetching](./ex-so-plwe-fene__data-fetching.md) - Data fetching strategies
- [Server Components](./ex-so-plwe-fene__server-components.md) - Server Component patterns
- [Security](./ex-so-plwe-fene__security.md) - API security best practices
- [Best Practices](./ex-so-plwe-fene__best-practices.md) - Production standards

**Official Resources**:

- [Route Handlers](https://nextjs.org/docs/app/building-your-application/routing/route-handlers)
- [Server Actions](https://nextjs.org/docs/app/building-your-application/data-fetching/server-actions-and-mutations)

---

This comprehensive REST APIs guide covers all aspects of API integration in Next.js. Use these patterns to build secure, performant API endpoints and consume external APIs effectively in your Next.js applications.
