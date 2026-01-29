---
title: Next.js Deployment Guide
description: Comprehensive deployment guide for Next.js applications covering Vercel deployment, Docker containerization, self-hosted Node.js, static export, environment variables, CI/CD, and production optimizations
category: explanation
tags:
  - nextjs
  - deployment
  - vercel
  - docker
  - cicd
  - production
  - typescript
created: 2026-01-29
updated: 2026-01-29
---

# Next.js Deployment Guide

This document provides comprehensive guidance on deploying Next.js applications to production. Next.js supports multiple deployment strategies from managed platforms like Vercel to self-hosted solutions with Docker. Understanding deployment options and best practices ensures reliable, scalable production deployments.

**Prerequisites**: Familiarity with [configuration](./ex-so-plwe-fene__configuration.md), [performance optimization](./ex-so-plwe-fene__performance.md), and [security](./ex-so-plwe-fene__security.md).

## 🚀 Deployment Strategies

### Deployment Options Comparison

| Strategy       | Complexity | Cost                  | Best For                            |
| -------------- | ---------- | --------------------- | ----------------------------------- |
| Vercel         | Low        | Free tier, paid plans | Fastest deployment, automatic CI/CD |
| Docker         | Medium     | Infrastructure costs  | Self-hosted, full control           |
| Node.js Server | Medium     | Infrastructure costs  | Custom infrastructure               |
| Static Export  | Low        | CDN costs             | Static sites, no SSR                |

## ☁️ Vercel Deployment (Recommended)

### Initial Setup

```bash
# Install Vercel CLI
npm install -g vercel

# Login to Vercel
vercel login

# Deploy to preview
vercel

# Deploy to production
vercel --prod
```

### Automatic GitHub Integration

1. **Connect Repository**:
   - Go to [vercel.com](https://vercel.com)
   - Import your GitHub repository
   - Configure project settings

2. **Automatic Deployments**:
   - Push to `main` → production deployment
   - Push to other branches → preview deployment
   - Pull requests → preview deployments with unique URLs

### Vercel Configuration

```json
// vercel.json
{
  "buildCommand": "npm run build",
  "devCommand": "npm run dev",
  "installCommand": "npm install",
  "framework": "nextjs",
  "regions": ["iad1"],
  "env": {
    "DATABASE_URL": "@database-url"
  },
  "build": {
    "env": {
      "NEXT_PUBLIC_API_URL": "https://api.oseplatform.com"
    }
  }
}
```

### Environment Variables on Vercel

```bash
# Set environment variable
vercel env add DATABASE_URL

# Options:
# - Production
# - Preview
# - Development

# Pull environment variables locally
vercel env pull .env.local
```

### Performance on Vercel

```typescript
// next.config.ts
const nextConfig = {
  // Enable image optimization
  images: {
    domains: ["api.oseplatform.com"],
    formats: ["image/avif", "image/webp"],
  },

  // Compression
  compress: true,

  // Production source maps
  productionBrowserSourceMaps: false,
};

export default nextConfig;
```

## 🐳 Docker Deployment

### Dockerfile for Next.js

```dockerfile
# Dockerfile
FROM node:20-alpine AS base

# Install dependencies only when needed
FROM base AS deps
RUN apk add --no-cache libc6-compat
WORKDIR /app

# Copy package files
COPY package.json package-lock.json ./

# Install dependencies
RUN npm ci

# Rebuild source code only when needed
FROM base AS builder
WORKDIR /app
COPY --from=deps /app/node_modules ./node_modules
COPY . .

# Environment variables for build
ARG NEXT_PUBLIC_API_URL
ENV NEXT_PUBLIC_API_URL=$NEXT_PUBLIC_API_URL

# Build Next.js application
RUN npm run build

# Production image
FROM base AS runner
WORKDIR /app

ENV NODE_ENV=production

# Create non-root user
RUN addgroup --system --gid 1001 nodejs
RUN adduser --system --uid 1001 nextjs

# Copy necessary files
COPY --from=builder /app/public ./public
COPY --from=builder --chown=nextjs:nodejs /app/.next/standalone ./
COPY --from=builder --chown=nextjs:nodejs /app/.next/static ./.next/static

USER nextjs

EXPOSE 3000

ENV PORT 3000
ENV HOSTNAME "0.0.0.0"

# Start application
CMD ["node", "server.js"]
```

### Standalone Output

```typescript
// next.config.ts
const nextConfig = {
  output: "standalone", // Creates minimal Docker-ready output
};

export default nextConfig;
```

### Docker Compose

```yaml
# docker-compose.yml
version: "3.8"

services:
  nextjs:
    build:
      context: .
      args:
        NEXT_PUBLIC_API_URL: ${NEXT_PUBLIC_API_URL}
    ports:
      - "3000:3000"
    environment:
      - DATABASE_URL=${DATABASE_URL}
      - NEXTAUTH_SECRET=${NEXTAUTH_SECRET}
      - NEXTAUTH_URL=${NEXTAUTH_URL}
    restart: unless-stopped
    depends_on:
      - postgres

  postgres:
    image: postgres:16-alpine
    environment:
      - POSTGRES_USER=${POSTGRES_USER}
      - POSTGRES_PASSWORD=${POSTGRES_PASSWORD}
      - POSTGRES_DB=${POSTGRES_DB}
    volumes:
      - postgres-data:/var/lib/postgresql/data
    restart: unless-stopped

volumes:
  postgres-data:
```

### Build and Run

```bash
# Build Docker image
docker build -t ose-nextjs:latest .

# Run container
docker run -p 3000:3000 \
  -e DATABASE_URL="postgresql://..." \
  -e NEXTAUTH_SECRET="secret" \
  ose-nextjs:latest

# With Docker Compose
docker-compose up -d
```

## 🖥️ Self-Hosted Node.js

### Production Server Setup

```typescript
// server.js
const { createServer } = require("http");
const { parse } = require("url");
const next = require("next");

const dev = process.env.NODE_ENV !== "production";
const hostname = process.env.HOSTNAME || "localhost";
const port = parseInt(process.env.PORT || "3000", 10);

const app = next({ dev, hostname, port });
const handle = app.getRequestHandler();

app.prepare().then(() => {
  createServer(async (req, res) => {
    try {
      const parsedUrl = parse(req.url, true);
      await handle(req, res, parsedUrl);
    } catch (err) {
      console.error("Error occurred handling", req.url, err);
      res.statusCode = 500;
      res.end("internal server error");
    }
  }).listen(port, (err) => {
    if (err) throw err;
    console.log(`> Ready on http://${hostname}:${port}`);
  });
});
```

### PM2 Process Manager

```bash
# Install PM2
npm install -g pm2

# Start application
pm2 start npm --name "ose-nextjs" -- start

# Start with ecosystem file
pm2 start ecosystem.config.js
```

```javascript
// ecosystem.config.js
module.exports = {
  apps: [
    {
      name: "ose-nextjs",
      script: "npm",
      args: "start",
      instances: "max",
      exec_mode: "cluster",
      env: {
        NODE_ENV: "production",
        PORT: 3000,
      },
      error_file: "./logs/err.log",
      out_file: "./logs/out.log",
      log_date_format: "YYYY-MM-DD HH:mm:ss Z",
    },
  ],
};
```

### Nginx Reverse Proxy

```nginx
# /etc/nginx/sites-available/ose-nextjs
upstream nextjs_backend {
    server localhost:3000;
}

server {
    listen 80;
    server_name oseplatform.com www.oseplatform.com;

    # Redirect to HTTPS
    return 301 https://$server_name$request_uri;
}

server {
    listen 443 ssl http2;
    server_name oseplatform.com www.oseplatform.com;

    # SSL configuration
    ssl_certificate /etc/letsencrypt/live/oseplatform.com/fullchain.pem;
    ssl_certificate_key /etc/letsencrypt/live/oseplatform.com/privkey.pem;
    ssl_protocols TLSv1.2 TLSv1.3;
    ssl_ciphers HIGH:!aNULL:!MD5;

    # Security headers
    add_header X-Frame-Options "DENY" always;
    add_header X-Content-Type-Options "nosniff" always;
    add_header Referrer-Policy "strict-origin-when-cross-origin" always;

    # Proxy settings
    location / {
        proxy_pass http://nextjs_backend;
        proxy_http_version 1.1;
        proxy_set_header Upgrade $http_upgrade;
        proxy_set_header Connection 'upgrade';
        proxy_set_header Host $host;
        proxy_set_header X-Real-IP $remote_addr;
        proxy_set_header X-Forwarded-For $proxy_add_x_forwarded_for;
        proxy_set_header X-Forwarded-Proto $scheme;
        proxy_cache_bypass $http_upgrade;
    }

    # Static file caching
    location /_next/static {
        proxy_cache STATIC;
        proxy_pass http://nextjs_backend;
        add_header Cache-Control "public, max-age=31536000, immutable";
    }
}
```

## 📦 Static Export

### Configuration for Static Export

```typescript
// next.config.ts
const nextConfig = {
  output: "export",
  trailingSlash: true,
  images: {
    unoptimized: true, // Required for static export
  },
};

export default nextConfig;
```

### Build Static Site

```bash
# Build static site
npm run build

# Output directory: out/
# Deploy to any static hosting (Netlify, Cloudflare Pages, S3)
```

### Limitations of Static Export

**Cannot use**:

- Server-Side Rendering (SSR)
- Incremental Static Regeneration (ISR)
- Server Actions
- API Routes (Route Handlers)
- Dynamic routes without `generateStaticParams`
- Image Optimization API

**Can use**:

- Static Site Generation (SSG)
- Client-Side Rendering (CSR)
- Client Components
- Static assets

## 🔄 CI/CD Pipeline

### GitHub Actions

```yaml
# .github/workflows/deploy.yml
name: Deploy Next.js

on:
  push:
    branches: [main]
  pull_request:
    branches: [main]

jobs:
  test:
    runs-on: ubuntu-latest

    steps:
      - uses: actions/checkout@v4

      - name: Setup Node.js
        uses: actions/setup-node@v4
        with:
          node-version: "20"
          cache: "npm"

      - name: Install dependencies
        run: npm ci

      - name: Run linter
        run: npm run lint

      - name: Run tests
        run: npm run test

      - name: Build application
        run: npm run build
        env:
          NEXT_PUBLIC_API_URL: ${{ secrets.NEXT_PUBLIC_API_URL }}

  deploy:
    runs-on: ubuntu-latest
    needs: test
    if: github.ref == 'refs/heads/main'

    steps:
      - uses: actions/checkout@v4

      - name: Deploy to Vercel
        uses: amondnet/vercel-action@v25
        with:
          vercel-token: ${{ secrets.VERCEL_TOKEN }}
          vercel-org-id: ${{ secrets.VERCEL_ORG_ID }}
          vercel-project-id: ${{ secrets.VERCEL_PROJECT_ID }}
          vercel-args: "--prod"
```

### Docker Registry Deployment

```yaml
# .github/workflows/docker-deploy.yml
name: Docker Deploy

on:
  push:
    branches: [main]

jobs:
  build-and-push:
    runs-on: ubuntu-latest

    steps:
      - uses: actions/checkout@v4

      - name: Set up Docker Buildx
        uses: docker/setup-buildx-action@v3

      - name: Login to Docker Hub
        uses: docker/login-action@v3
        with:
          username: ${{ secrets.DOCKER_USERNAME }}
          password: ${{ secrets.DOCKER_PASSWORD }}

      - name: Build and push
        uses: docker/build-push-action@v5
        with:
          context: .
          push: true
          tags: |
            oseplatform/nextjs:latest
            oseplatform/nextjs:${{ github.sha }}
          build-args: |
            NEXT_PUBLIC_API_URL=${{ secrets.NEXT_PUBLIC_API_URL }}
          cache-from: type=registry,ref=oseplatform/nextjs:buildcache
          cache-to: type=registry,ref=oseplatform/nextjs:buildcache,mode=max

  deploy:
    runs-on: ubuntu-latest
    needs: build-and-push

    steps:
      - name: Deploy to server
        uses: appleboy/ssh-action@v1.0.0
        with:
          host: ${{ secrets.SERVER_HOST }}
          username: ${{ secrets.SERVER_USERNAME }}
          key: ${{ secrets.SSH_PRIVATE_KEY }}
          script: |
            docker pull oseplatform/nextjs:latest
            docker stop ose-nextjs || true
            docker rm ose-nextjs || true
            docker run -d \
              --name ose-nextjs \
              -p 3000:3000 \
              -e DATABASE_URL="${{ secrets.DATABASE_URL }}" \
              -e NEXTAUTH_SECRET="${{ secrets.NEXTAUTH_SECRET }}" \
              --restart unless-stopped \
              oseplatform/nextjs:latest
```

## 🔐 Environment Variables Management

### Production Environment Variables

```bash
# .env.production
DATABASE_URL=postgresql://prod-db.example.com:5432/ose
API_SECRET_KEY=prod_secret_key
NEXTAUTH_SECRET=prod_auth_secret
NEXTAUTH_URL=https://oseplatform.com

# Public variables
NEXT_PUBLIC_API_URL=https://api.oseplatform.com
NEXT_PUBLIC_SITE_NAME=OSE Platform
```

### Environment Variable Validation

```typescript
// lib/env.ts
import { z } from "zod";

const envSchema = z.object({
  NODE_ENV: z.enum(["development", "production", "test"]),
  DATABASE_URL: z.string().url(),
  API_SECRET_KEY: z.string().min(32),
  NEXTAUTH_SECRET: z.string().min(32),
  NEXTAUTH_URL: z.string().url(),
  NEXT_PUBLIC_API_URL: z.string().url(),
  NEXT_PUBLIC_SITE_NAME: z.string(),
});

export const env = envSchema.parse(process.env);
```

## 📊 Production Monitoring

### Health Check Endpoint

```typescript
// app/api/health/route.ts
import { NextResponse } from "next/server";
import { db } from "@/lib/db";

export async function GET() {
  try {
    // Check database connection
    await db.$queryRaw`SELECT 1`;

    return NextResponse.json({
      status: "healthy",
      timestamp: new Date().toISOString(),
      uptime: process.uptime(),
    });
  } catch (error) {
    return NextResponse.json(
      {
        status: "unhealthy",
        error: "Database connection failed",
      },
      { status: 503 },
    );
  }
}
```

### Vercel Analytics

```typescript
// app/layout.tsx
import { Analytics } from '@vercel/analytics/react';
import { SpeedInsights } from '@vercel/speed-insights/next';

export default function RootLayout({ children }: { children: React.ReactNode }) {
  return (
    <html>
      <body>
        {children}
        <Analytics />
        <SpeedInsights />
      </body>
    </html>
  );
}
```

## 🔄 Zero-Downtime Deployment

### Blue-Green Deployment with Docker

```bash
#!/bin/bash
# deploy.sh

# Build new image
docker build -t ose-nextjs:green .

# Start new container (port 3001)
docker run -d \
  --name ose-nextjs-green \
  -p 3001:3000 \
  -e DATABASE_URL="$DATABASE_URL" \
  ose-nextjs:green

# Wait for health check
sleep 10
curl -f http://localhost:3001/api/health || exit 1

# Switch nginx to new container
sed -i 's/localhost:3000/localhost:3001/g' /etc/nginx/sites-available/ose-nextjs
nginx -s reload

# Stop old container
docker stop ose-nextjs-blue
docker rm ose-nextjs-blue

# Rename containers
docker rename ose-nextjs-green ose-nextjs-blue
```

## 📈 Production Optimization Checklist

### Pre-Deployment Checklist

- [ ] Environment variables configured
- [ ] Database migrations run
- [ ] Build completes without errors
- [ ] Tests passing (unit, integration, E2E)
- [ ] Security headers configured
- [ ] SSL/TLS certificates installed
- [ ] CDN configured for static assets
- [ ] Image optimization enabled
- [ ] Font optimization enabled
- [ ] Caching strategies configured
- [ ] Error tracking configured (Sentry)
- [ ] Performance monitoring enabled
- [ ] Health check endpoint working
- [ ] Backup strategy implemented
- [ ] Rollback plan documented
- [ ] Load testing completed
- [ ] Security scan completed

### Post-Deployment Checklist

- [ ] Verify deployment URL accessible
- [ ] Check health check endpoint
- [ ] Verify database connectivity
- [ ] Test critical user flows
- [ ] Check error rates in monitoring
- [ ] Verify Core Web Vitals
- [ ] Monitor server resources
- [ ] Check CDN cache hit rate
- [ ] Verify SSL certificate
- [ ] Test API endpoints
- [ ] Verify environment variables loaded
- [ ] Check log output for errors

## 🔒 Production Security

### Security Headers

```typescript
// next.config.ts
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
    key: "Strict-Transport-Security",
    value: "max-age=31536000; includeSubDomains",
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

### Rate Limiting

```typescript
// middleware.ts
import { NextRequest, NextResponse } from "next/server";
import { Ratelimit } from "@upstash/ratelimit";
import { Redis } from "@upstash/redis";

const ratelimit = new Ratelimit({
  redis: Redis.fromEnv(),
  limiter: Ratelimit.slidingWindow(10, "10 s"),
});

export async function middleware(request: NextRequest) {
  const ip = request.ip ?? "127.0.0.1";
  const { success } = await ratelimit.limit(ip);

  if (!success) {
    return NextResponse.json({ error: "Rate limit exceeded" }, { status: 429 });
  }

  return NextResponse.next();
}
```

## 🔗 Related Documentation

**Next.js Core**:

- [Configuration](./ex-so-plwe-fene__configuration.md) - Configuration for production
- [Performance](./ex-so-plwe-fene__performance.md) - Performance optimization
- [Security](./ex-so-plwe-fene__security.md) - Security best practices
- [Observability](./ex-so-plwe-fene__observability.md) - Monitoring production

**Official Resources**:

- [Vercel Deployment](https://vercel.com/docs/deployments/overview)
- [Docker Deployment](https://nextjs.org/docs/app/building-your-application/deploying#docker-image)
- [Self-Hosting](https://nextjs.org/docs/app/building-your-application/deploying#self-hosting)

---

This comprehensive deployment guide covers all production deployment strategies for Next.js applications. Choose the deployment approach that best fits your infrastructure requirements, team capabilities, and budget constraints.
