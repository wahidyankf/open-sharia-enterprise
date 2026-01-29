---
title: Next.js Security Best Practices
description: Comprehensive security guide for Next.js applications covering Server Component security, XSS prevention, CSRF protection, authentication with NextAuth.js, authorization patterns, environment variables, and API security
category: explanation
tags:
  - nextjs
  - security
  - authentication
  - authorization
  - xss
  - csrf
  - typescript
created: 2026-01-29
updated: 2026-01-29
---

# Next.js Security Best Practices

This document provides comprehensive security guidance for Next.js applications with TypeScript. Security is non-negotiable in modern web applications, especially those handling financial data or sensitive user information. Next.js provides built-in security features, but developers must understand and properly implement security best practices.

**Prerequisites**: Familiarity with [Server Components](./ex-so-plwe-fene__server-components.md), [REST APIs](./ex-so-plwe-fene__rest-apis.md), and [web security fundamentals](https://owasp.org/).

## 🛡️ Server Component Security Advantages

Server Components provide inherent security benefits by keeping sensitive operations on the server.

### Sensitive Data on Server Only

```typescript
// app/admin/dashboard/page.tsx (Server Component)
import { db } from '@/lib/db';

export default async function AdminDashboard() {
  // Sensitive query stays on server
  const apiKey = process.env.ADMIN_API_KEY; // Never exposed to client
  const sensitiveData = await db.admin.findMany({
    where: { apiKey },
  });

  // Transform data for safe display
  const safeData = sensitiveData.map((item) => ({
    id: item.id,
    name: item.name,
    // Exclude sensitive fields
  }));

  return <DashboardView data={safeData} />;
}
```

**Benefits**:

- API keys never sent to client
- Database credentials stay secure
- Business logic protected
- Sensitive algorithms hidden

### Server-Only Module Pattern

Use `server-only` package to enforce server-side execution.

```bash
npm install server-only
```

```typescript
// lib/auth/server-only-auth.ts
import "server-only";

export async function getServerSession() {
  // This code will error if imported in Client Component
  const session = await validateToken();
  return session;
}

export async function checkAdminPermissions(userId: string) {
  const user = await db.user.findUnique({
    where: { id: userId },
    select: { role: true },
  });

  return user?.role === "admin";
}
```

## 🔒 XSS Prevention

### Automatic XSS Protection with React

React automatically escapes content, preventing XSS attacks.

```typescript
// app/zakat/page.tsx
export default function ZakatPage({ searchParams }: { searchParams: { name?: string } }) {
  // React automatically escapes searchParams.name
  return (
    <div>
      <h1>Welcome, {searchParams.name}</h1>
      {/* Safe - React escapes HTML */}
    </div>
  );
}
```

### Dangerous HTML Rendering (Avoid)

Only use `dangerouslySetInnerHTML` with sanitized content.

```typescript
import DOMPurify from 'isomorphic-dompurify';

interface ArticleProps {
  content: string;
}

export function Article({ content }: ArticleProps) {
  // Sanitize HTML before rendering
  const sanitizedContent = DOMPurify.sanitize(content);

  return (
    <div
      dangerouslySetInnerHTML={{ __html: sanitizedContent }}
    />
  );
}
```

### Safe URL Handling

Validate and sanitize URLs before use.

```typescript
function isValidUrl(url: string): boolean {
  try {
    const parsed = new URL(url);
    // Only allow https protocol
    return parsed.protocol === 'https:';
  } catch {
    return false;
  }
}

export function SafeLink({ href, children }: { href: string; children: React.ReactNode }) {
  if (!isValidUrl(href)) {
    return <span>{children}</span>;
  }

  return (
    <a href={href} rel="noopener noreferrer">
      {children}
    </a>
  );
}
```

## 🔐 CSRF Protection

### Server Actions CSRF Protection

Server Actions have built-in CSRF protection through origin checking.

```typescript
// app/_actions/contract-actions.ts
"use server";

import { db } from "@/lib/db";

// CSRF protection automatic - origin validated by Next.js
export async function createContract(formData: FormData) {
  const principalAmount = Number(formData.get("principalAmount"));

  const contract = await db.contract.create({
    data: { principalAmount },
  });

  return { success: true, contractId: contract.id };
}
```

**How it works**:

- Next.js validates request origin
- Origin must match application domain
- Automatic protection for POST/PUT/DELETE
- No additional CSRF token needed

### API Route CSRF Protection

For API routes, implement CSRF token validation.

```typescript
// lib/csrf.ts
import { randomBytes } from "crypto";

export function generateCsrfToken(): string {
  return randomBytes(32).toString("hex");
}

export function validateCsrfToken(token: string, expected: string): boolean {
  return token === expected;
}

// app/api/contracts/route.ts
import { NextRequest, NextResponse } from "next/server";
import { validateCsrfToken } from "@/lib/csrf";

export async function POST(request: NextRequest) {
  const csrfToken = request.headers.get("x-csrf-token");
  const expectedToken = request.cookies.get("csrf-token")?.value;

  if (!csrfToken || !expectedToken || !validateCsrfToken(csrfToken, expectedToken)) {
    return NextResponse.json({ error: "Invalid CSRF token" }, { status: 403 });
  }

  // Process request
  const contract = await createContract(request);
  return NextResponse.json(contract);
}
```

## 🔑 Authentication

### NextAuth.js Setup (Recommended)

NextAuth.js (Auth.js v5) provides comprehensive authentication.

```bash
npm install next-auth@beta
```

```typescript
// auth.ts
import NextAuth from "next-auth";
import Credentials from "next-auth/providers/credentials";
import { z } from "zod";

const loginSchema = z.object({
  email: z.string().email(),
  password: z.string().min(8),
});

export const { handlers, signIn, signOut, auth } = NextAuth({
  providers: [
    Credentials({
      credentials: {
        email: { label: "Email", type: "email" },
        password: { label: "Password", type: "password" },
      },
      async authorize(credentials) {
        const validatedData = loginSchema.parse(credentials);

        const user = await db.user.findUnique({
          where: { email: validatedData.email },
        });

        if (!user || !(await verifyPassword(validatedData.password, user.passwordHash))) {
          return null;
        }

        return {
          id: user.id,
          email: user.email,
          name: user.name,
          role: user.role,
        };
      },
    }),
  ],
  callbacks: {
    async jwt({ token, user }) {
      if (user) {
        token.role = user.role;
      }
      return token;
    },
    async session({ session, token }) {
      if (session.user) {
        session.user.role = token.role as string;
      }
      return session;
    },
  },
});
```

### Protected Routes with Middleware

```typescript
// middleware.ts
import { auth } from "@/auth";

export default auth((req) => {
  const isLoggedIn = !!req.auth;
  const isOnDashboard = req.nextUrl.pathname.startsWith("/dashboard");

  if (isOnDashboard && !isLoggedIn) {
    return Response.redirect(new URL("/login", req.url));
  }
});

export const config = {
  matcher: ["/dashboard/:path*", "/admin/:path*"],
};
```

### Session Handling in Server Components

```typescript
// app/dashboard/page.tsx
import { auth } from '@/auth';
import { redirect } from 'next/navigation';

export default async function DashboardPage() {
  const session = await auth();

  if (!session) {
    redirect('/login');
  }

  return (
    <div>
      <h1>Welcome, {session.user.name}</h1>
      <UserDashboard userId={session.user.id} />
    </div>
  );
}
```

## 🔐 Authorization

### Role-Based Access Control

```typescript
// lib/auth/authorization.ts
import { auth } from '@/auth';

export async function requireAdmin() {
  const session = await auth();

  if (!session || session.user.role !== 'admin') {
    throw new Error('Unauthorized');
  }

  return session;
}

export async function requireRole(role: string) {
  const session = await auth();

  if (!session || session.user.role !== role) {
    throw new Error('Unauthorized');
  }

  return session;
}

// app/admin/contracts/page.tsx
import { requireAdmin } from '@/lib/auth/authorization';

export default async function AdminContractsPage() {
  await requireAdmin(); // Throws if not admin

  const allContracts = await db.contract.findMany();

  return <AdminContractsList contracts={allContracts} />;
}
```

### Resource-Level Authorization

```typescript
// app/murabaha/[id]/page.tsx
import { auth } from '@/auth';
import { notFound } from 'next/navigation';

export default async function ContractPage({ params }: { params: { id: string } }) {
  const session = await auth();

  if (!session) {
    redirect('/login');
  }

  const contract = await db.contract.findUnique({
    where: { id: params.id },
  });

  if (!contract) {
    notFound();
  }

  // Authorization check
  if (contract.userId !== session.user.id && session.user.role !== 'admin') {
    throw new Error('You do not have permission to view this contract');
  }

  return <ContractDetails contract={contract} />;
}
```

## 🔐 Environment Variables Security

### Server-Only Variables

Never prefix server-only secrets with `NEXT_PUBLIC_`.

```bash
# .env.local (server-only)
DATABASE_URL=postgresql://localhost:5432/ose
API_SECRET_KEY=super_secret_key
NEXTAUTH_SECRET=auth_secret_here

# Public variables (safe to expose)
NEXT_PUBLIC_API_URL=https://api.example.com
NEXT_PUBLIC_SITE_NAME=OSE Platform
```

### Environment Variable Validation

Validate environment variables at build time.

```typescript
// lib/env.ts
import { z } from "zod";

const envSchema = z.object({
  // Server-only
  DATABASE_URL: z.string().url(),
  API_SECRET_KEY: z.string().min(32),
  NEXTAUTH_SECRET: z.string().min(32),

  // Public
  NEXT_PUBLIC_API_URL: z.string().url(),
  NEXT_PUBLIC_SITE_NAME: z.string(),

  // Node environment
  NODE_ENV: z.enum(["development", "production", "test"]),
});

export const env = envSchema.parse(process.env);

// Usage: import { env } from '@/lib/env';
// const dbUrl = env.DATABASE_URL;
```

## 🔒 API Security

### Input Validation

Always validate API inputs with Zod or similar.

```typescript
// app/api/zakat/calculate/route.ts
import { NextRequest, NextResponse } from "next/server";
import { z } from "zod";

const zakatSchema = z.object({
  wealth: z.number().positive().max(1000000000),
  nisab: z.number().positive(),
});

export async function POST(request: NextRequest) {
  try {
    const body = await request.json();
    const validatedData = zakatSchema.parse(body);

    const result = await calculateZakat(validatedData);

    return NextResponse.json(result);
  } catch (error) {
    if (error instanceof z.ZodError) {
      return NextResponse.json(
        {
          error: "Invalid input",
          details: error.errors.map((e) => ({
            field: e.path.join("."),
            message: e.message,
          })),
        },
        { status: 400 },
      );
    }

    return NextResponse.json({ error: "Internal server error" }, { status: 500 });
  }
}
```

### Rate Limiting

Implement rate limiting to prevent abuse.

```typescript
// lib/rate-limit.ts
import { LRUCache } from "lru-cache";

type RateLimitOptions = {
  uniqueTokenPerInterval?: number;
  interval?: number;
};

export default function rateLimit(options?: RateLimitOptions) {
  const tokenCache = new LRUCache({
    max: options?.uniqueTokenPerInterval || 500,
    ttl: options?.interval || 60000,
  });

  return {
    check: (limit: number, token: string) =>
      new Promise<void>((resolve, reject) => {
        const tokenCount = (tokenCache.get(token) as number[]) || [0];
        if (tokenCount[0] === 0) {
          tokenCache.set(token, tokenCount);
        }
        tokenCount[0] += 1;

        const currentUsage = tokenCount[0];
        const isRateLimited = currentUsage >= limit;

        return isRateLimited ? reject() : resolve();
      }),
  };
}

// app/api/zakat/calculate/route.ts
import rateLimit from "@/lib/rate-limit";

const limiter = rateLimit({
  interval: 60 * 1000, // 1 minute
  uniqueTokenPerInterval: 500,
});

export async function POST(request: NextRequest) {
  const ip = request.ip || "anonymous";

  try {
    await limiter.check(10, ip); // 10 requests per minute
  } catch {
    return NextResponse.json({ error: "Rate limit exceeded" }, { status: 429 });
  }

  // Process request
}
```

### API Authentication

```typescript
// lib/api-auth.ts
import { SignJWT, jwtVerify } from "jose";

const secret = new TextEncoder().encode(process.env.JWT_SECRET);

export async function createToken(payload: object): Promise<string> {
  return new SignJWT(payload).setProtectedHeader({ alg: "HS256" }).setIssuedAt().setExpirationTime("2h").sign(secret);
}

export async function verifyToken(token: string) {
  try {
    const verified = await jwtVerify(token, secret);
    return verified.payload;
  } catch {
    return null;
  }
}

// app/api/protected/route.ts
import { NextRequest, NextResponse } from "next/server";
import { verifyToken } from "@/lib/api-auth";

export async function GET(request: NextRequest) {
  const authHeader = request.headers.get("authorization");

  if (!authHeader?.startsWith("Bearer ")) {
    return NextResponse.json({ error: "Unauthorized" }, { status: 401 });
  }

  const token = authHeader.substring(7);
  const payload = await verifyToken(token);

  if (!payload) {
    return NextResponse.json({ error: "Invalid token" }, { status: 401 });
  }

  // Access protected resource
  return NextResponse.json({ data: "Protected data" });
}
```

## 🛡️ Content Security Policy

Configure CSP headers for XSS protection.

```typescript
// next.config.ts
const ContentSecurityPolicy = `
  default-src 'self';
  script-src 'self' 'unsafe-eval' 'unsafe-inline' *.vercel-insights.com;
  style-src 'self' 'unsafe-inline';
  img-src 'self' blob: data: https:;
  font-src 'self';
  connect-src 'self' *.vercel-insights.com;
  frame-ancestors 'none';
  base-uri 'self';
  form-action 'self';
  upgrade-insecure-requests;
`;

const securityHeaders = [
  {
    key: "Content-Security-Policy",
    value: ContentSecurityPolicy.replace(/\s{2,}/g, " ").trim(),
  },
  {
    key: "X-Frame-Options",
    value: "DENY",
  },
  {
    key: "X-Content-Type-Options",
    value: "nosniff",
  },
  {
    key: "Referrer-Policy",
    value: "strict-origin-when-cross-origin",
  },
  {
    key: "Permissions-Policy",
    value: "camera=(), microphone=(), geolocation=()",
  },
];

const nextConfig = {
  async headers() {
    return [
      {
        source: "/:path*",
        headers: securityHeaders,
      },
    ];
  },
};

export default nextConfig;
```

## 🔐 Password Security

### Password Hashing

Use bcrypt for password hashing.

```bash
npm install bcryptjs
npm install -D @types/bcryptjs
```

```typescript
// lib/auth/password.ts
import bcrypt from "bcryptjs";

export async function hashPassword(password: string): Promise<string> {
  const salt = await bcrypt.genSalt(12);
  return bcrypt.hash(password, salt);
}

export async function verifyPassword(password: string, hashedPassword: string): Promise<boolean> {
  return bcrypt.compare(password, hashedPassword);
}

// app/_actions/auth-actions.ts
("use server");

import { z } from "zod";
import { hashPassword } from "@/lib/auth/password";

const registerSchema = z.object({
  email: z.string().email(),
  password: z
    .string()
    .min(8)
    .regex(/^(?=.*[a-z])(?=.*[A-Z])(?=.*\d)/),
  name: z.string().min(2),
});

export async function registerUser(formData: FormData) {
  const validatedData = registerSchema.parse({
    email: String(formData.get("email")),
    password: String(formData.get("password")),
    name: String(formData.get("name")),
  });

  const passwordHash = await hashPassword(validatedData.password);

  const user = await db.user.create({
    data: {
      email: validatedData.email,
      passwordHash,
      name: validatedData.name,
    },
  });

  return { success: true, userId: user.id };
}
```

## 🔒 SQL Injection Prevention

Use parameterized queries with Prisma.

```typescript
// ✅ SAFE - Parameterized query
export async function getContractsByStatus(status: string) {
  return db.contract.findMany({
    where: { status }, // Prisma handles sanitization
  });
}

// ❌ UNSAFE - Raw SQL without sanitization
export async function getContractsUnsafe(status: string) {
  // NEVER DO THIS
  return db.$queryRaw`SELECT * FROM contracts WHERE status = ${status}`;
}

// ✅ SAFE - Raw SQL with proper sanitization
export async function getContractsSafe(status: string) {
  return db.$queryRaw`SELECT * FROM contracts WHERE status = ${status}::text`;
}
```

## 🔐 Session Security

### Secure Cookie Configuration

```typescript
// auth.ts
export const { handlers, signIn, signOut, auth } = NextAuth({
  cookies: {
    sessionToken: {
      name: `__Secure-next-auth.session-token`,
      options: {
        httpOnly: true,
        sameSite: "lax",
        path: "/",
        secure: process.env.NODE_ENV === "production",
      },
    },
  },
  session: {
    strategy: "jwt",
    maxAge: 30 * 24 * 60 * 60, // 30 days
  },
});
```

## 📊 Security Checklist

### Pre-Deployment Security Checklist

- [ ] Environment variables properly configured (no NEXT*PUBLIC* for secrets)
- [ ] HTTPS enforced in production
- [ ] CSP headers configured
- [ ] CSRF protection enabled (Server Actions or tokens)
- [ ] Input validation on all API endpoints
- [ ] Rate limiting implemented
- [ ] Authentication required for protected routes
- [ ] Authorization checks on sensitive operations
- [ ] Passwords hashed with bcrypt (12+ rounds)
- [ ] SQL injection prevention (parameterized queries)
- [ ] XSS prevention (React escaping, sanitize HTML)
- [ ] Secure cookies (httpOnly, secure, sameSite)
- [ ] Session expiration configured
- [ ] Error messages don't leak sensitive info
- [ ] Dependencies up to date (npm audit)
- [ ] CORS configured properly
- [ ] API keys rotated regularly
- [ ] Logging configured (exclude sensitive data)

## 🔗 Related Documentation

**Next.js Core**:

- [Server Components](./ex-so-plwe-fene__server-components.md) - Server Component security advantages
- [REST APIs](./ex-so-plwe-fene__rest-apis.md) - API security patterns
- [Configuration](./ex-so-plwe-fene__configuration.md) - Security headers configuration
- [Best Practices](./ex-so-plwe-fene__best-practices.md) - Production standards

**Official Resources**:

- [Next.js Security](https://nextjs.org/docs/app/building-your-application/configuring/environment-variables)
- [OWASP Top 10](https://owasp.org/www-project-top-ten/)
- [NextAuth.js Documentation](https://authjs.dev/)

---

This comprehensive security guide covers essential security practices for Next.js applications. Security is an ongoing process - stay updated on vulnerabilities, perform regular security audits, and follow the principle of least privilege in all implementations.
