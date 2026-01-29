---
title: Next.js Configuration Guide
description: Comprehensive guide to Next.js configuration including next.config.ts, environment variables, TypeScript configuration, middleware, headers, redirects, and build optimization
category: explanation
tags:
  - nextjs
  - configuration
  - typescript
  - environment-variables
  - middleware
  - build-optimization
created: 2026-01-29
updated: 2026-01-29
---

# Next.js Configuration Guide

This document provides comprehensive guidance on configuring Next.js applications with TypeScript. Proper configuration is essential for production deployments, security, performance optimization, and development experience.

**Prerequisites**: Familiarity with [Next.js fundamentals](./README.md), [TypeScript](../../prog-lang/typescript/README.md), and [Node.js](https://nodejs.org/).

## ⚙️ next.config.ts Configuration

### Basic Configuration

Next.js 14+ supports TypeScript configuration files:

```typescript
// next.config.ts
import type { NextConfig } from "next";

const nextConfig: NextConfig = {
  // Strict mode for React
  reactStrictMode: true,

  // TypeScript strict mode
  typescript: {
    ignoreBuildErrors: false,
  },

  // ESLint configuration
  eslint: {
    ignoreDuringBuilds: false,
    dirs: ["app", "components", "lib"],
  },

  // Enable experimental features
  experimental: {
    // Partial Prerendering (preview)
    ppr: false,

    // Server Actions (stable in Next.js 14)
    serverActions: {
      bodySizeLimit: "2mb",
    },
  },
};

export default nextConfig;
```

### Image Optimization

Configure Next.js Image component:

```typescript
// next.config.ts
const nextConfig: NextConfig = {
  images: {
    // Allowed external image domains
    domains: ["api.example.com", "cdn.example.com"],

    // Or use remotePatterns (more flexible)
    remotePatterns: [
      {
        protocol: "https",
        hostname: "**.example.com",
        port: "",
        pathname: "/images/**",
      },
    ],

    // Image formats
    formats: ["image/avif", "image/webp"],

    // Device sizes for responsive images
    deviceSizes: [640, 750, 828, 1080, 1200, 1920, 2048, 3840],

    // Image sizes (layout="fill")
    imageSizes: [16, 32, 48, 64, 96, 128, 256, 384],

    // Minimum cache TTL (seconds)
    minimumCacheTTL: 60,

    // Disable image optimization (not recommended)
    unoptimized: false,
  },
};

export default nextConfig;
```

### Environment Variables

Configure environment-specific settings:

```typescript
// next.config.ts
const nextConfig: NextConfig = {
  // Expose environment variables to client (prefix required)
  env: {
    CUSTOM_KEY: process.env.CUSTOM_KEY,
  },

  // Base path for deployment
  basePath: process.env.BASE_PATH || "",

  // Asset prefix (CDN)
  assetPrefix: process.env.ASSET_PREFIX || "",
};

export default nextConfig;
```

### Headers Configuration

Add custom HTTP headers:

```typescript
// next.config.ts
const nextConfig: NextConfig = {
  async headers() {
    return [
      {
        source: "/:path*",
        headers: [
          // Security headers
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
          // CORS headers (API routes)
          {
            key: "Access-Control-Allow-Origin",
            value: process.env.ALLOWED_ORIGIN || "*",
          },
        ],
      },
      {
        source: "/api/:path*",
        headers: [
          {
            key: "Cache-Control",
            value: "no-store, max-age=0",
          },
        ],
      },
    ];
  },
};

export default nextConfig;
```

### Content Security Policy (CSP)

Configure CSP headers for security:

```typescript
// next.config.ts
const ContentSecurityPolicy = `
  default-src 'self';
  script-src 'self' 'unsafe-eval' 'unsafe-inline' *.vercel-insights.com;
  style-src 'self' 'unsafe-inline';
  img-src 'self' blob: data: https:;
  font-src 'self';
  object-src 'none';
  base-uri 'self';
  form-action 'self';
  frame-ancestors 'none';
  upgrade-insecure-requests;
`;

const securityHeaders = [
  {
    key: "Content-Security-Policy",
    value: ContentSecurityPolicy.replace(/\s{2,}/g, " ").trim(),
  },
];

const nextConfig: NextConfig = {
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

### Redirects

Configure URL redirects:

```typescript
// next.config.ts
const nextConfig: NextConfig = {
  async redirects() {
    return [
      // Permanent redirect (308)
      {
        source: "/old-zakat",
        destination: "/zakat",
        permanent: true,
      },

      // Temporary redirect (307)
      {
        source: "/maintenance",
        destination: "/under-maintenance",
        permanent: false,
      },

      // Wildcard path matching
      {
        source: "/blog/:slug",
        destination: "/news/:slug",
        permanent: true,
      },

      // Regex path matching
      {
        source: "/old-site/:path(.*)",
        destination: "/new-site/:path",
        permanent: true,
      },

      // External redirects
      {
        source: "/external",
        destination: "https://example.com",
        permanent: false,
      },
    ];
  },
};

export default nextConfig;
```

### Rewrites

Proxy requests without changing the URL:

```typescript
// next.config.ts
const nextConfig: NextConfig = {
  async rewrites() {
    return [
      // Simple rewrite
      {
        source: "/api/old-endpoint",
        destination: "/api/new-endpoint",
      },

      // External API proxy
      {
        source: "/api/contracts/:path*",
        destination: "https://api.example.com/contracts/:path*",
      },

      // Rewrites can be grouped
      {
        source: "/blog/:slug",
        destination: "/news/:slug",
      },
    ];
  },
};

export default nextConfig;
```

### Build Output

Configure build output options:

```typescript
// next.config.ts
const nextConfig: NextConfig = {
  // Output mode
  output: "standalone", // For Docker deployment

  // Or 'export' for static HTML export
  // output: 'export',

  // Trailing slashes
  trailingSlash: true,

  // Generate build ID
  generateBuildId: async () => {
    return process.env.BUILD_ID || `build-${Date.now()}`;
  },

  // Compress output
  compress: true,

  // Power settings
  poweredByHeader: false,

  // Production source maps
  productionBrowserSourceMaps: false,
};

export default nextConfig;
```

## 🔐 Environment Variables

### Environment Variable Types

Next.js supports three types of environment variables:

**1. Server-only variables** (no prefix):

```bash
# .env.local
DATABASE_URL=postgresql://localhost:5432/ose
API_SECRET_KEY=secret_key_here
NEXTAUTH_SECRET=auth_secret_here
```

**2. Public variables** (NEXT*PUBLIC* prefix):

```bash
# .env.local
NEXT_PUBLIC_API_URL=https://api.example.com
NEXT_PUBLIC_SITE_NAME=OSE Platform
NEXT_PUBLIC_GA_ID=G-XXXXXXXXXX
```

**3. Built-in variables**:

```bash
# Next.js built-in environment variables
NODE_ENV=development  # or 'production' or 'test'
NEXT_PUBLIC_VERCEL_URL=your-deployment.vercel.app
```

### Environment Files

Create environment-specific files:

```
.env                # All environments (committed to git)
.env.local          # Local overrides (ignored by git)
.env.development    # Development environment
.env.production     # Production environment
.env.test           # Test environment
```

**.env Example**:

```bash
# .env (default values, committed to git)
NEXT_PUBLIC_API_URL=https://api.oseplatform.com
NEXT_PUBLIC_SITE_NAME=OSE Platform

# Database (placeholder only)
DATABASE_URL=postgresql://localhost:5432/ose

# Authentication
NEXTAUTH_URL=http://localhost:3000
```

**.env.local Example** (never commit):

```bash
# .env.local (local development, gitignored)
DATABASE_URL=postgresql://user:password@localhost:5432/ose_dev
API_SECRET_KEY=local_secret_key
NEXTAUTH_SECRET=local_auth_secret
```

### Loading Environment Variables

Next.js automatically loads environment variables from `.env*` files:

```typescript
// app/murabaha/page.tsx (Server Component)
export default async function MurabahaPage() {
  // ✅ Server-only variable (secure)
  const databaseUrl = process.env.DATABASE_URL;

  // ✅ Public variable (accessible on client)
  const apiUrl = process.env.NEXT_PUBLIC_API_URL;

  return <div>API URL: {apiUrl}</div>;
}

// app/_components/ApiClient.tsx (Client Component)
'use client';

export function ApiClient() {
  // ✅ Public variable (accessible on client)
  const apiUrl = process.env.NEXT_PUBLIC_API_URL;

  // ❌ Server-only variable (undefined on client)
  const databaseUrl = process.env.DATABASE_URL; // undefined!

  return <div>API URL: {apiUrl}</div>;
}
```

### Environment Variable Validation

Validate environment variables at build time:

```typescript
// lib/env.ts
import { z } from "zod";

const envSchema = z.object({
  // Server-only variables
  DATABASE_URL: z.string().url(),
  API_SECRET_KEY: z.string().min(32),
  NEXTAUTH_SECRET: z.string().min(32),

  // Public variables
  NEXT_PUBLIC_API_URL: z.string().url(),
  NEXT_PUBLIC_SITE_NAME: z.string().min(1),

  // Node environment
  NODE_ENV: z.enum(["development", "production", "test"]),
});

export const env = envSchema.parse(process.env);

// Usage in app
import { env } from "@/lib/env";

const dbUrl = env.DATABASE_URL; // Type-safe and validated
```

## 📝 TypeScript Configuration

### tsconfig.json

Configure TypeScript for Next.js:

```json
{
  "compilerOptions": {
    "target": "ES2020",
    "lib": ["dom", "dom.iterable", "esnext"],
    "allowJs": true,
    "skipLibCheck": true,
    "strict": true,
    "noEmit": true,
    "esModuleInterop": true,
    "module": "esnext",
    "moduleResolution": "bundler",
    "resolveJsonModule": true,
    "isolatedModules": true,
    "jsx": "preserve",
    "incremental": true,
    "plugins": [
      {
        "name": "next"
      }
    ],
    "paths": {
      "@/*": ["./*"],
      "@/components/*": ["./components/*"],
      "@/lib/*": ["./lib/*"],
      "@/app/*": ["./app/*"]
    }
  },
  "include": ["next-env.d.ts", "**/*.ts", "**/*.tsx", ".next/types/**/*.ts"],
  "exclude": ["node_modules"]
}
```

### Path Aliases

Configure import path aliases:

```typescript
// tsconfig.json
{
  "compilerOptions": {
    "paths": {
      "@/*": ["./*"],
      "@/components/*": ["./components/*"],
      "@/lib/*": ["./lib/*"],
      "@/actions/*": ["./app/_actions/*"],
      "@/types/*": ["./lib/types/*"]
    }
  }
}

// Usage in code
import { Button } from '@/components/ui/Button';
import { db } from '@/lib/db';
import { submitZakat } from '@/actions/zakat-actions';
import type { MurabahaContract } from '@/types/contracts';
```

## 🛡️ Middleware Configuration

Middleware runs before routes are matched.

### Basic Middleware

```typescript
// middleware.ts (root of project)
import { NextRequest, NextResponse } from "next/server";

export function middleware(request: NextRequest) {
  // Add custom header
  const response = NextResponse.next();
  response.headers.set("X-Custom-Header", "custom-value");

  return response;
}

export const config = {
  matcher: [
    /*
     * Match all request paths except:
     * - _next/static (static files)
     * - _next/image (image optimization)
     * - favicon.ico (favicon file)
     */
    "/((?!_next/static|_next/image|favicon.ico).*)",
  ],
};
```

### Authentication Middleware

```typescript
// middleware.ts
import { NextRequest, NextResponse } from "next/server";

export function middleware(request: NextRequest) {
  const token = request.cookies.get("auth-token");

  // Protected routes
  if (request.nextUrl.pathname.startsWith("/dashboard")) {
    if (!token) {
      return NextResponse.redirect(new URL("/login", request.url));
    }
  }

  // Admin routes
  if (request.nextUrl.pathname.startsWith("/admin")) {
    if (!token || token.value !== "admin-token") {
      return NextResponse.redirect(new URL("/unauthorized", request.url));
    }
  }

  return NextResponse.next();
}
```

### Internationalization Middleware

```typescript
// middleware.ts
import { NextRequest, NextResponse } from "next/server";

const locales = ["en", "id", "ar"];
const defaultLocale = "en";

export function middleware(request: NextRequest) {
  // Check if there is any supported locale in the pathname
  const pathname = request.nextUrl.pathname;
  const pathnameIsMissingLocale = locales.every(
    (locale) => !pathname.startsWith(`/${locale}/`) && pathname !== `/${locale}`,
  );

  // Redirect if there is no locale
  if (pathnameIsMissingLocale) {
    const locale = request.cookies.get("locale")?.value || defaultLocale;
    return NextResponse.redirect(new URL(`/${locale}${pathname}`, request.url));
  }

  return NextResponse.next();
}
```

### Rate Limiting Middleware

```typescript
// middleware.ts
import { NextRequest, NextResponse } from "next/server";

const rateLimitMap = new Map<string, { count: number; resetTime: number }>();

export function middleware(request: NextRequest) {
  const ip = request.ip || "unknown";
  const now = Date.now();

  const rateLimit = rateLimitMap.get(ip);

  if (!rateLimit || now > rateLimit.resetTime) {
    // Reset rate limit
    rateLimitMap.set(ip, {
      count: 1,
      resetTime: now + 60000, // 1 minute
    });
  } else {
    rateLimit.count += 1;

    if (rateLimit.count > 100) {
      // Exceeded rate limit
      return NextResponse.json({ error: "Rate limit exceeded" }, { status: 429 });
    }
  }

  return NextResponse.next();
}
```

## 🚀 Build Optimization

### Bundle Analysis

Analyze bundle size:

```bash
# Install bundle analyzer
npm install @next/bundle-analyzer

# Enable in next.config.ts
```

```typescript
// next.config.ts
import bundleAnalyzer from "@next/bundle-analyzer";

const withBundleAnalyzer = bundleAnalyzer({
  enabled: process.env.ANALYZE === "true",
});

const nextConfig: NextConfig = {
  // Your config
};

export default withBundleAnalyzer(nextConfig);
```

```bash
# Run analysis
ANALYZE=true npm run build
```

### Code Splitting

Configure automatic code splitting:

```typescript
// next.config.ts
const nextConfig: NextConfig = {
  experimental: {
    optimizePackageImports: ["@mui/material", "@mui/icons-material", "lodash"],
  },
};

export default nextConfig;
```

### Webpack Configuration

Customize webpack (advanced):

```typescript
// next.config.ts
import type { Configuration } from "webpack";

const nextConfig: NextConfig = {
  webpack: (config: Configuration, { isServer }) => {
    // Add custom webpack config
    if (!isServer) {
      config.resolve = {
        ...config.resolve,
        fallback: {
          ...config.resolve?.fallback,
          fs: false,
          net: false,
          tls: false,
        },
      };
    }

    return config;
  },
};

export default nextConfig;
```

## 📊 Production Configuration Example

Complete production-ready configuration:

```typescript
// next.config.ts
import type { NextConfig } from "next";

const ContentSecurityPolicy = `
  default-src 'self';
  script-src 'self' 'unsafe-eval' 'unsafe-inline' *.vercel-insights.com;
  style-src 'self' 'unsafe-inline';
  img-src 'self' blob: data: https:;
  font-src 'self';
  object-src 'none';
  base-uri 'self';
  form-action 'self';
  frame-ancestors 'none';
  upgrade-insecure-requests;
`;

const securityHeaders = [
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
  {
    key: "Content-Security-Policy",
    value: ContentSecurityPolicy.replace(/\s{2,}/g, " ").trim(),
  },
];

const nextConfig: NextConfig = {
  // React strict mode
  reactStrictMode: true,

  // TypeScript strict mode
  typescript: {
    ignoreBuildErrors: false,
  },

  // ESLint
  eslint: {
    ignoreDuringBuilds: false,
  },

  // Image optimization
  images: {
    remotePatterns: [
      {
        protocol: "https",
        hostname: "**.oseplatform.com",
      },
    ],
    formats: ["image/avif", "image/webp"],
  },

  // Security headers
  async headers() {
    return [
      {
        source: "/:path*",
        headers: securityHeaders,
      },
    ];
  },

  // Redirects
  async redirects() {
    return [
      {
        source: "/old-path",
        destination: "/new-path",
        permanent: true,
      },
    ];
  },

  // Production optimizations
  compress: true,
  poweredByHeader: false,
  productionBrowserSourceMaps: false,

  // Output for Docker
  output: "standalone",
};

export default nextConfig;
```

## 🔗 Related Documentation

**Next.js Core**:

- [Best Practices](./ex-so-plwe-fene__best-practices.md) - Production standards
- [Security](./ex-so-plwe-fene__security.md) - Security configuration
- [Deployment](./ex-so-plwe-fene__deployment.md) - Deployment strategies
- [Performance](./ex-so-plwe-fene__performance.md) - Performance optimization

**Official Resources**:

- [Next.js Configuration](https://nextjs.org/docs/app/api-reference/next-config-js)
- [Environment Variables](https://nextjs.org/docs/app/building-your-application/configuring/environment-variables)
- [TypeScript](https://nextjs.org/docs/app/building-your-application/configuring/typescript)

---

This comprehensive configuration guide covers all aspects of Next.js configuration. Use these patterns to build secure, performant, production-ready applications with proper environment management and build optimization.
