---
title: Next.js API Route Template
description: Production-ready template for Next.js App Router API route handlers with validation, authentication, error handling, and CORS configuration
category: explanation
tags:
  - nextjs
  - template
  - api-route
  - rest-api
  - validation
created: 2026-01-26
updated: 2026-01-26
---

# Next.js API Route Template

Production-ready template for creating type-safe Next.js App Router API route handlers with Zod validation, authentication, error handling, CORS, and rate limiting for OSE Platform.

## 📋 Template Usage

**File location**: `app/api/[endpoint]/route.ts`

**Use this template when**:

- Creating REST API endpoints
- Building backend services
- Providing data to external clients
- Integrating with third-party services

## 🎯 Basic API Route Template

```typescript
// app/api/[endpoint]/route.ts
import { NextRequest, NextResponse } from "next/server";
import { z } from "zod";

// Request validation schema
const requestSchema = z.object({
  // Define request body schema
});

// GET handler
export async function GET(request: NextRequest) {
  try {
    // Implement GET logic
    const data = { message: "Success" };

    return NextResponse.json(data, { status: 200 });
  } catch (error) {
    console.error("GET error:", error);
    return NextResponse.json({ error: "Internal server error" }, { status: 500 });
  }
}

// POST handler
export async function POST(request: NextRequest) {
  try {
    const body: unknown = await request.json();

    // Validate request body
    const validation = requestSchema.safeParse(body);

    if (!validation.success) {
      return NextResponse.json(
        {
          error: "Validation failed",
          details: validation.error.errors,
        },
        { status: 400 },
      );
    }

    const data = validation.data;

    // Implement POST logic

    return NextResponse.json({ success: true, data }, { status: 201 });
  } catch (error) {
    console.error("POST error:", error);
    return NextResponse.json({ error: "Internal server error" }, { status: 500 });
  }
}
```

## ✅ Complete CRUD API Route

```typescript
// app/api/zakat/calculations/route.ts
import { NextRequest, NextResponse } from "next/server";
import { z } from "zod";
import { auth } from "@/lib/auth";
import { db } from "@/lib/db";

// Validation schemas
const createCalculationSchema = z.object({
  wealth: z.number().positive("Wealth must be positive"),
  nisab: z.number().positive("Nisab must be positive"),
  liabilities: z.number().nonnegative("Liabilities cannot be negative").optional(),
});

const updateCalculationSchema = z.object({
  wealth: z.number().positive().optional(),
  nisab: z.number().positive().optional(),
  liabilities: z.number().nonnegative().optional(),
});

// GET /api/zakat/calculations - List all calculations for user
export async function GET(request: NextRequest) {
  try {
    const session = await auth();

    if (!session) {
      return NextResponse.json({ error: "Unauthorized" }, { status: 401 });
    }

    // Parse query parameters
    const searchParams = request.nextUrl.searchParams;
    const page = parseInt(searchParams.get("page") || "1", 10);
    const limit = parseInt(searchParams.get("limit") || "10", 10);
    const skip = (page - 1) * limit;

    // Fetch calculations
    const [calculations, total] = await Promise.all([
      db.zakatCalculation.findMany({
        where: { userId: session.user.id },
        take: limit,
        skip,
        orderBy: { createdAt: "desc" },
      }),
      db.zakatCalculation.count({
        where: { userId: session.user.id },
      }),
    ]);

    return NextResponse.json(
      {
        data: calculations,
        pagination: {
          page,
          limit,
          total,
          totalPages: Math.ceil(total / limit),
        },
      },
      { status: 200 },
    );
  } catch (error) {
    console.error("GET /api/zakat/calculations error:", error);
    return NextResponse.json({ error: "Internal server error" }, { status: 500 });
  }
}

// POST /api/zakat/calculations - Create new calculation
export async function POST(request: NextRequest) {
  try {
    const session = await auth();

    if (!session) {
      return NextResponse.json({ error: "Unauthorized" }, { status: 401 });
    }

    const body: unknown = await request.json();

    // Validate request
    const validation = createCalculationSchema.safeParse(body);

    if (!validation.success) {
      return NextResponse.json(
        {
          error: "Validation failed",
          details: validation.error.errors,
        },
        { status: 400 },
      );
    }

    const data = validation.data;

    // Calculate zakat
    const netWealth = data.wealth - (data.liabilities || 0);
    const zakatAmount = netWealth >= data.nisab ? netWealth * 0.025 : 0;

    // Create calculation
    const calculation = await db.zakatCalculation.create({
      data: {
        userId: session.user.id,
        wealth: data.wealth,
        nisab: data.nisab,
        liabilities: data.liabilities || 0,
        zakatAmount,
        eligibleForZakat: zakatAmount > 0,
      },
    });

    return NextResponse.json(calculation, { status: 201 });
  } catch (error) {
    console.error("POST /api/zakat/calculations error:", error);
    return NextResponse.json({ error: "Internal server error" }, { status: 500 });
  }
}
```

## 🔑 Dynamic Route API

```typescript
// app/api/zakat/calculations/[id]/route.ts
import { NextRequest, NextResponse } from "next/server";
import { z } from "zod";
import { auth } from "@/lib/auth";
import { db } from "@/lib/db";

interface RouteContext {
  params: {
    id: string;
  };
}

// GET /api/zakat/calculations/[id] - Get single calculation
export async function GET(request: NextRequest, { params }: RouteContext) {
  try {
    const session = await auth();

    if (!session) {
      return NextResponse.json({ error: "Unauthorized" }, { status: 401 });
    }

    const calculation = await db.zakatCalculation.findUnique({
      where: {
        id: params.id,
        userId: session.user.id, // Verify ownership
      },
    });

    if (!calculation) {
      return NextResponse.json({ error: "Calculation not found" }, { status: 404 });
    }

    return NextResponse.json(calculation, { status: 200 });
  } catch (error) {
    console.error("GET error:", error);
    return NextResponse.json({ error: "Internal server error" }, { status: 500 });
  }
}

// PUT /api/zakat/calculations/[id] - Update calculation
export async function PUT(request: NextRequest, { params }: RouteContext) {
  try {
    const session = await auth();

    if (!session) {
      return NextResponse.json({ error: "Unauthorized" }, { status: 401 });
    }

    // Verify ownership
    const existing = await db.zakatCalculation.findUnique({
      where: {
        id: params.id,
        userId: session.user.id,
      },
    });

    if (!existing) {
      return NextResponse.json({ error: "Calculation not found" }, { status: 404 });
    }

    const body: unknown = await request.json();

    const validation = updateCalculationSchema.safeParse(body);

    if (!validation.success) {
      return NextResponse.json(
        {
          error: "Validation failed",
          details: validation.error.errors,
        },
        { status: 400 },
      );
    }

    const data = validation.data;

    // Update calculation
    const updated = await db.zakatCalculation.update({
      where: { id: params.id },
      data: {
        wealth: data.wealth,
        nisab: data.nisab,
        liabilities: data.liabilities,
        updatedAt: new Date(),
      },
    });

    return NextResponse.json(updated, { status: 200 });
  } catch (error) {
    console.error("PUT error:", error);
    return NextResponse.json({ error: "Internal server error" }, { status: 500 });
  }
}

// DELETE /api/zakat/calculations/[id] - Delete calculation
export async function DELETE(request: NextRequest, { params }: RouteContext) {
  try {
    const session = await auth();

    if (!session) {
      return NextResponse.json({ error: "Unauthorized" }, { status: 401 });
    }

    // Verify ownership
    const existing = await db.zakatCalculation.findUnique({
      where: {
        id: params.id,
        userId: session.user.id,
      },
    });

    if (!existing) {
      return NextResponse.json({ error: "Calculation not found" }, { status: 404 });
    }

    await db.zakatCalculation.delete({
      where: { id: params.id },
    });

    return NextResponse.json({ success: true, message: "Calculation deleted" }, { status: 200 });
  } catch (error) {
    console.error("DELETE error:", error);
    return NextResponse.json({ error: "Internal server error" }, { status: 500 });
  }
}
```

## 🔒 API Route with Authentication Middleware

```typescript
// lib/api/middleware.ts
import { NextRequest, NextResponse } from "next/server";
import { auth } from "@/lib/auth";

export async function withAuth(
  handler: (req: NextRequest, context: any) => Promise<NextResponse>,
  requiredRole?: string,
) {
  return async (request: NextRequest, context: any) => {
    const session = await auth();

    if (!session) {
      return NextResponse.json({ error: "Unauthorized" }, { status: 401 });
    }

    if (requiredRole && session.user.role !== requiredRole) {
      return NextResponse.json({ error: "Forbidden: insufficient permissions" }, { status: 403 });
    }

    // Add session to request for handler
    (request as any).session = session;

    return handler(request, context);
  };
}
```

```typescript
// app/api/admin/users/route.ts
import { NextRequest, NextResponse } from "next/server";
import { withAuth } from "@/lib/api/middleware";
import { db } from "@/lib/db";

async function handler(request: NextRequest) {
  const session = (request as any).session;

  const users = await db.user.findMany({
    select: {
      id: true,
      name: true,
      email: true,
      role: true,
      createdAt: true,
    },
  });

  return NextResponse.json(users, { status: 200 });
}

export const GET = withAuth(handler, "ADMIN");
```

## 🌐 API Route with CORS

```typescript
// app/api/public/data/route.ts
import { NextRequest, NextResponse } from "next/server";

const corsHeaders = {
  "Access-Control-Allow-Origin": "*",
  "Access-Control-Allow-Methods": "GET, POST, PUT, DELETE, OPTIONS",
  "Access-Control-Allow-Headers": "Content-Type, Authorization",
};

// Handle preflight requests
export async function OPTIONS(request: NextRequest) {
  return NextResponse.json({}, { headers: corsHeaders });
}

export async function GET(request: NextRequest) {
  try {
    const data = { message: "Public API data" };

    return NextResponse.json(data, {
      status: 200,
      headers: corsHeaders,
    });
  } catch (error) {
    return NextResponse.json({ error: "Internal server error" }, { status: 500, headers: corsHeaders });
  }
}

export async function POST(request: NextRequest) {
  try {
    const body = await request.json();

    // Process request

    return NextResponse.json({ success: true }, { status: 201, headers: corsHeaders });
  } catch (error) {
    return NextResponse.json({ error: "Internal server error" }, { status: 500, headers: corsHeaders });
  }
}
```

## ⏱️ API Route with Rate Limiting

```typescript
// lib/rate-limit.ts
interface RateLimitStore {
  [key: string]: { count: number; resetTime: number };
}

const store: RateLimitStore = {};

export function rateLimit(
  identifier: string,
  maxRequests: number,
  windowMs: number,
): { success: boolean; remaining: number } {
  const now = Date.now();
  const record = store[identifier];

  if (!record || record.resetTime < now) {
    store[identifier] = {
      count: 1,
      resetTime: now + windowMs,
    };
    return { success: true, remaining: maxRequests - 1 };
  }

  if (record.count >= maxRequests) {
    return { success: false, remaining: 0 };
  }

  record.count++;
  return { success: true, remaining: maxRequests - record.count };
}
```

```typescript
// app/api/zakat/calculate/route.ts
import { NextRequest, NextResponse } from "next/server";
import { rateLimit } from "@/lib/rate-limit";

export async function POST(request: NextRequest) {
  try {
    const ip = request.ip || "unknown";

    // Rate limit: 10 requests per minute
    const rateLimitResult = rateLimit(ip, 10, 60 * 1000);

    if (!rateLimitResult.success) {
      return NextResponse.json(
        { error: "Too many requests. Please try again later." },
        {
          status: 429,
          headers: {
            "X-RateLimit-Limit": "10",
            "X-RateLimit-Remaining": "0",
            "Retry-After": "60",
          },
        },
      );
    }

    // Process request...

    return NextResponse.json(
      { success: true },
      {
        status: 200,
        headers: {
          "X-RateLimit-Limit": "10",
          "X-RateLimit-Remaining": String(rateLimitResult.remaining),
        },
      },
    );
  } catch (error) {
    console.error("POST error:", error);
    return NextResponse.json({ error: "Internal server error" }, { status: 500 });
  }
}
```

## 📡 API Route with External API Integration

```typescript
// app/api/exchange-rates/route.ts
import { NextRequest, NextResponse } from "next/server";

export const revalidate = 3600; // Cache for 1 hour

export async function GET(request: NextRequest) {
  try {
    const searchParams = request.nextUrl.searchParams;
    const baseCurrency = searchParams.get("base") || "USD";

    // Fetch from external API
    const response = await fetch(`https://api.exchangerate.host/latest?base=${baseCurrency}`, {
      next: { revalidate: 3600 },
    });

    if (!response.ok) {
      return NextResponse.json({ error: "Failed to fetch exchange rates" }, { status: 502 });
    }

    const data = await response.json();

    return NextResponse.json(
      {
        base: baseCurrency,
        rates: data.rates,
        lastUpdated: data.date,
      },
      { status: 200 },
    );
  } catch (error) {
    console.error("Exchange rate API error:", error);
    return NextResponse.json({ error: "Failed to fetch exchange rates" }, { status: 500 });
  }
}
```

## 📤 File Upload API Route

```typescript
// app/api/upload/route.ts
import { NextRequest, NextResponse } from "next/server";
import { writeFile, mkdir } from "fs/promises";
import path from "path";
import { randomUUID } from "crypto";
import { auth } from "@/lib/auth";

const MAX_FILE_SIZE = 5 * 1024 * 1024; // 5MB
const ALLOWED_TYPES = ["image/jpeg", "image/png", "image/webp", "application/pdf"];

export async function POST(request: NextRequest) {
  try {
    const session = await auth();

    if (!session) {
      return NextResponse.json({ error: "Unauthorized" }, { status: 401 });
    }

    const formData = await request.formData();
    const file = formData.get("file") as File | null;

    if (!file) {
      return NextResponse.json({ error: "No file provided" }, { status: 400 });
    }

    // Validate file size
    if (file.size > MAX_FILE_SIZE) {
      return NextResponse.json({ error: "File size exceeds 5MB limit" }, { status: 400 });
    }

    // Validate file type
    if (!ALLOWED_TYPES.includes(file.type)) {
      return NextResponse.json({ error: "Invalid file type. Allowed: JPEG, PNG, WebP, PDF" }, { status: 400 });
    }

    // Generate safe filename
    const ext = path.extname(file.name);
    const filename = `${randomUUID()}${ext}`;

    // Create upload directory
    const uploadDir = path.join(process.cwd(), "uploads", session.user.id);
    await mkdir(uploadDir, { recursive: true });

    // Save file
    const filepath = path.join(uploadDir, filename);
    const bytes = await file.arrayBuffer();
    await writeFile(filepath, Buffer.from(bytes));

    return NextResponse.json(
      {
        success: true,
        filename,
        size: file.size,
        type: file.type,
      },
      { status: 201 },
    );
  } catch (error) {
    console.error("Upload error:", error);
    return NextResponse.json({ error: "File upload failed" }, { status: 500 });
  }
}
```

## 📚 Best Practices

### 1. Always Validate Input

```typescript
const schema = z.object({
  field: z.string().min(1),
});

const validation = schema.safeParse(body);

if (!validation.success) {
  return NextResponse.json({ error: "Validation failed" }, { status: 400 });
}
```

### 2. Use Proper HTTP Status Codes

```typescript
200 - OK (successful GET)
201 - Created (successful POST)
400 - Bad Request (validation error)
401 - Unauthorized (authentication required)
403 - Forbidden (insufficient permissions)
404 - Not Found
429 - Too Many Requests (rate limit)
500 - Internal Server Error
```

### 3. Implement Error Handling

```typescript
try {
  // API logic
} catch (error) {
  console.error("API error:", error);
  return NextResponse.json({ error: "Internal server error" }, { status: 500 });
}
```

### 4. Verify Authentication and Authorization

```typescript
const session = await auth();

if (!session) {
  return NextResponse.json({ error: "Unauthorized" }, { status: 401 });
}

if (session.user.role !== "ADMIN") {
  return NextResponse.json({ error: "Forbidden" }, { status: 403 });
}
```

### 5. Use Type-Safe Responses

```typescript
interface ApiResponse {
  data?: any;
  error?: string;
}

return NextResponse.json<ApiResponse>({ data: result }, { status: 200 });
```

## 🔗 Related Templates

- [Page Template](./ex-so-plwe-tsnext-te__page-template.md) - Page components
- [Server Action Template](./ex-so-plwe-tsnext-te__server-action-template.md) - Form mutations
- [Layout Template](./ex-so-plwe-tsnext-te__layout-template.md) - App structure

---

This template provides comprehensive API route patterns for Next.js App Router with OSE Platform security and best practices.
