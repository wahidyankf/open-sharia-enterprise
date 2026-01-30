---
title: React Deployment
description: Comprehensive guide to deploying React applications across various platforms with production-ready configurations
category: explanation
tags:
  - react
  - deployment
  - vercel
  - netlify
  - docker
  - ci-cd
  - performance
created: 2026-01-29
updated: 2026-01-29
---

# React Deployment

Comprehensive explanation of deployment strategies, platforms, and configurations for React applications in production environments.

## 📋 Overview

Deploying React applications requires understanding build processes, hosting platforms, environment configuration, and optimization strategies. This document explains deployment concepts, platform-specific approaches, and production best practices.

## 🏗️ Build Process

### Production Build Fundamentals

React applications require compilation from JSX/TypeScript to browser-compatible JavaScript before deployment.

**Create React App (CRA)**:

```bash
npm run build
```

Creates optimized production build in `build/` directory:

- Minified JavaScript bundles
- CSS extraction and optimization
- Asset optimization (images, fonts)
- Source maps for debugging
- Service worker generation (if configured)

**Vite**:

```bash
npm run build
```

Creates optimized production build in `dist/` directory:

- ESBuild for fast bundling
- Tree-shaking and code splitting
- CSS optimization
- Asset handling and optimization

### Build Optimization Techniques

**Code Splitting**:

```javascript
// Dynamic imports for route-based code splitting
import { lazy, Suspense } from "react";

const Dashboard = lazy(() => import("./pages/Dashboard"));
const Profile = lazy(() => import("./pages/Profile"));

function App() {
  return (
    <Suspense fallback={<div>Loading...</div>}>
      <Routes>
        <Route path="/dashboard" element={<Dashboard />} />
        <Route path="/profile" element={<Profile />} />
      </Routes>
    </Suspense>
  );
}
```

**Bundle Analysis**:

```bash
# CRA with webpack-bundle-analyzer
npm install --save-dev webpack-bundle-analyzer
ANALYZE=true npm run build

# Vite with rollup-plugin-visualizer
npm install --save-dev rollup-plugin-visualizer
npm run build
```

**Tree Shaking**: Eliminate unused code through ES module imports.

```javascript
// Good - tree-shakeable
import { debounce } from "lodash-es";

// Avoid - imports entire library
import _ from "lodash";
```

## ☁️ Vercel Deployment

### Automatic Deployments

Vercel provides zero-configuration deployments for React applications with automatic preview URLs for pull requests.

**Configuration** (`vercel.json`):

```json
{
  "buildCommand": "npm run build",
  "outputDirectory": "dist",
  "devCommand": "npm run dev",
  "installCommand": "npm install",
  "framework": "vite",
  "regions": ["iad1"],
  "env": {
    "VITE_API_URL": "@api-url-production"
  },
  "build": {
    "env": {
      "VITE_FEATURE_FLAG": "true"
    }
  }
}
```

**Deployment Workflow**:

1. Push code to Git repository
2. Vercel detects changes automatically
3. Builds application in isolated environment
4. Deploys to global CDN
5. Assigns preview URL for branches
6. Production deployment on main branch merge

**Preview URLs**: Every pull request gets unique URL:

```
https://[project]-[branch]-[hash].vercel.app
```

**Custom Domains**:

```bash
# Add custom domain via CLI
vercel domains add example.com

# Configure DNS records
CNAME www.example.com cname.vercel-dns.com
```

## 🌐 Netlify Deployment

### Continuous Deployment

Netlify provides continuous deployment with built-in CI/CD, form handling, and serverless functions.

**Configuration** (`netlify.toml`):

```toml
[build]
  command = "npm run build"
  publish = "dist"
  functions = "netlify/functions"

[build.environment]
  NODE_VERSION = "24.11.1"
  VITE_API_URL = "https://api.example.com"

[[redirects]]
  from = "/*"
  to = "/index.html"
  status = 200
  force = false

[[headers]]
  for = "/*"
  [headers.values]
    X-Frame-Options = "DENY"
    X-Content-Type-Options = "nosniff"
    Referrer-Policy = "strict-origin-when-cross-origin"
    Content-Security-Policy = "default-src 'self'; script-src 'self' 'unsafe-inline'; style-src 'self' 'unsafe-inline';"

[[headers]]
  for = "/static/*"
  [headers.values]
    Cache-Control = "public, max-age=31536000, immutable"
```

### SPA Redirects

Single-page applications require server-side redirect configuration to handle client-side routing.

**Netlify redirects** (`_redirects` file in public folder):

```
/*    /index.html   200
```

**Why necessary**: When user navigates to `/dashboard` directly, server needs to serve `index.html` and let React Router handle routing.

## 🐳 Docker Deployment

### Multi-Stage Dockerfile

Optimize Docker images with multi-stage builds separating build and runtime environments.

**Complete Dockerfile**:

```dockerfile
# Stage 1: Build
FROM node:24.11.1-alpine AS builder

# Set working directory
WORKDIR /app

# Copy package files
COPY package.json package-lock.json ./

# Install dependencies
RUN npm ci --only=production && npm cache clean --force

# Copy source code
COPY . .

# Build application
RUN npm run build

# Stage 2: Production
FROM nginx:1.25-alpine

# Copy custom nginx configuration
COPY nginx.conf /etc/nginx/nginx.conf

# Copy built files from builder stage
COPY --from=builder /app/dist /usr/share/nginx/html

# Expose port
EXPOSE 80

# Health check
HEALTHCHECK --interval=30s --timeout=3s --start-period=5s --retries=3 \
  CMD wget --no-verbose --tries=1 --spider http://localhost/ || exit 1

# Start nginx
CMD ["nginx", "-g", "daemon off;"]
```

### Production Nginx Configuration

**Complete nginx.conf**:

```nginx
user nginx;
worker_processes auto;
error_log /var/log/nginx/error.log warn;
pid /var/run/nginx.pid;

events {
  worker_connections 1024;
}

http {
  include /etc/nginx/mime.types;
  default_type application/octet-stream;

  log_format main '$remote_addr - $remote_user [$time_local] "$request" '
                  '$status $body_bytes_sent "$http_referer" '
                  '"$http_user_agent" "$http_x_forwarded_for"';

  access_log /var/log/nginx/access.log main;

  sendfile on;
  tcp_nopush on;
  tcp_nodelay on;
  keepalive_timeout 65;
  types_hash_max_size 2048;

  # Gzip compression
  gzip on;
  gzip_vary on;
  gzip_proxied any;
  gzip_comp_level 6;
  gzip_types text/plain text/css text/xml text/javascript
             application/json application/javascript application/xml+rss
             application/rss+xml font/truetype font/opentype
             application/vnd.ms-fontobject image/svg+xml;

  # Security headers
  add_header X-Frame-Options "DENY" always;
  add_header X-Content-Type-Options "nosniff" always;
  add_header X-XSS-Protection "1; mode=block" always;
  add_header Referrer-Policy "strict-origin-when-cross-origin" always;

  server {
    listen 80;
    server_name localhost;
    root /usr/share/nginx/html;
    index index.html;

    # SPA routing - serve index.html for all routes
    location / {
      try_files $uri $uri/ /index.html;
    }

    # Cache static assets
    location ~* \.(js|css|png|jpg|jpeg|gif|ico|svg|woff|woff2|ttf|eot)$ {
      expires 1y;
      add_header Cache-Control "public, immutable";
    }

    # Disable caching for HTML files
    location ~* \.html$ {
      expires -1;
      add_header Cache-Control "no-store, no-cache, must-revalidate, proxy-revalidate, max-age=0";
    }

    # Health check endpoint
    location /health {
      access_log off;
      return 200 "healthy\n";
      add_header Content-Type text/plain;
    }

    # Error pages
    error_page 404 /index.html;
    error_page 500 502 503 504 /50x.html;
    location = /50x.html {
      root /usr/share/nginx/html;
    }
  }
}
```

**Build and run**:

```bash
# Build Docker image
docker build -t react-app:latest .

# Run container
docker run -d -p 8080:80 --name react-app react-app:latest

# View logs
docker logs -f react-app

# Stop container
docker stop react-app
```

## 🚀 CDN Configuration

### CloudFront Deployment

Amazon CloudFront provides global content delivery with edge caching.

**CloudFront distribution configuration**:

```json
{
  "DistributionConfig": {
    "Origins": [
      {
        "DomainName": "my-bucket.s3.amazonaws.com",
        "Id": "S3-my-bucket",
        "S3OriginConfig": {
          "OriginAccessIdentity": "origin-access-identity/cloudfront/ABCDEFG"
        }
      }
    ],
    "DefaultCacheBehavior": {
      "TargetOriginId": "S3-my-bucket",
      "ViewerProtocolPolicy": "redirect-to-https",
      "Compress": true,
      "CachePolicyId": "658327ea-f89d-4fab-a63d-7e88639e58f6",
      "ResponseHeadersPolicyId": "67f7725c-6f97-4210-82d7-5512b31e9d03"
    },
    "CustomErrorResponses": [
      {
        "ErrorCode": 403,
        "ResponseCode": 200,
        "ResponsePagePath": "/index.html",
        "ErrorCachingMinTTL": 300
      },
      {
        "ErrorCode": 404,
        "ResponseCode": 200,
        "ResponsePagePath": "/index.html",
        "ErrorCachingMinTTL": 300
      }
    ],
    "PriceClass": "PriceClass_100",
    "Enabled": true,
    "HttpVersion": "http2and3"
  }
}
```

**Cache invalidation after deployment**:

```bash
# Invalidate all files
aws cloudfront create-invalidation \
  --distribution-id E1234567890ABC \
  --paths "/*"

# Invalidate specific paths
aws cloudfront create-invalidation \
  --distribution-id E1234567890ABC \
  --paths "/index.html" "/static/js/*"
```

### Cloudflare Configuration

Cloudflare provides CDN, DDoS protection, and edge caching.

**Page Rules for SPA routing**:

1. Create Page Rule for `example.com/*`
2. Add "Forwarding URL" setting
3. Set to "301 - Permanent Redirect" or use Workers

**Cloudflare Workers** (recommended for SPA):

```javascript
addEventListener("fetch", (event) => {
  event.respondWith(handleRequest(event.request));
});

async function handleRequest(request) {
  const url = new URL(request.url);

  // Serve static assets directly
  if (url.pathname.startsWith("/static/") || url.pathname.match(/\.(js|css|png|jpg|jpeg|gif|ico|svg|woff|woff2)$/)) {
    return fetch(request);
  }

  // Serve index.html for all other routes (SPA routing)
  const modifiedRequest = new Request(new URL("/index.html", request.url), request);

  return fetch(modifiedRequest);
}
```

## ⚙️ Environment Variables

### Build-time vs Runtime Configuration

**Build-time variables**: Embedded into JavaScript bundle during build.

```bash
# .env.production
VITE_API_URL=https://api.production.com
VITE_ANALYTICS_ID=UA-12345678-1
```

**Usage in code**:

```javascript
const apiUrl = import.meta.env.VITE_API_URL;
const analyticsId = import.meta.env.VITE_ANALYTICS_ID;
```

**Limitations**:

- Cannot change without rebuild
- Exposed in client-side bundle
- Must be prefixed with `VITE_` (Vite) or `REACT_APP_` (CRA)

**Runtime configuration**: Load configuration from server at runtime.

```javascript
// public/config.js (not bundled, served as static file)
window.ENV_CONFIG = {
  API_URL: "${API_URL}",
  ANALYTICS_ID: "${ANALYTICS_ID}",
};
```

**Docker entrypoint script** (`entrypoint.sh`):

```bash
#!/bin/sh
set -e

# Replace environment variables in config.js
envsubst '${API_URL} ${ANALYTICS_ID}' < /usr/share/nginx/html/config.js.template > /usr/share/nginx/html/config.js

# Start nginx
exec nginx -g 'daemon off;'
```

**Usage in React**:

```javascript
// Access runtime config
const config = window.ENV_CONFIG;
const apiUrl = config.API_URL;
```

### Secure Environment Variables

**Never commit secrets**:

```gitignore
# .gitignore
.env
.env.local
.env.production
.env.production.local
```

**Platform-specific secret management**:

```bash
# Vercel
vercel env add VITE_API_KEY production

# Netlify
netlify env:set VITE_API_KEY "secret-value" --context production

# GitHub Actions (secrets)
# Set in repository settings → Secrets → Actions
```

## 📊 Static Site Generation

### Pre-rendering with React Static

Generate static HTML for all routes at build time.

**Configuration** (using Next.js as example):

```javascript
// next.config.js
module.exports = {
  output: "export",
  images: {
    unoptimized: true,
  },
  trailingSlash: true,
};
```

**Benefits**:

- Faster initial page load
- Better SEO (complete HTML served)
- Works without JavaScript enabled
- Reduced server load

**Limitations**:

- Build time increases with page count
- Dynamic content requires client-side fetching
- Cannot use server-side features

### Incremental Static Regeneration

Update static pages after deployment without full rebuild.

```javascript
// Next.js page with ISR
export async function getStaticProps() {
  const data = await fetchData();

  return {
    props: { data },
    revalidate: 60, // Regenerate page every 60 seconds
  };
}
```

## 🖥️ Server-Side Rendering

### Next.js SSR

Render React components on server for each request.

```javascript
// pages/dashboard.js
export async function getServerSideProps(context) {
  const userId = context.req.cookies.userId;
  const userData = await fetchUserData(userId);

  return {
    props: { userData },
  };
}

export default function Dashboard({ userData }) {
  return <div>Welcome, {userData.name}</div>;
}
```

**Benefits**:

- Personalized content on first load
- Better SEO for dynamic content
- Faster perceived performance

**Trade-offs**:

- Requires Node.js server
- Higher server costs
- Increased complexity

### Streaming SSR

Stream HTML to client as components render.

```javascript
// React 18 streaming with Suspense
import { Suspense } from "react";

function Page() {
  return (
    <Suspense fallback={<Spinner />}>
      <DataFetchingComponent />
    </Suspense>
  );
}
```

## 🔄 CI/CD Pipelines

### GitHub Actions Workflow

**Complete workflow** (`.github/workflows/deploy.yml`):

```yaml
name: Deploy React App

on:
  push:
    branches: [main]
  pull_request:
    branches: [main]

env:
  NODE_VERSION: "24.11.1"
  REGISTRY: ghcr.io
  IMAGE_NAME: ${{ github.repository }}

jobs:
  test:
    runs-on: ubuntu-latest
    steps:
      - name: Checkout code
        uses: actions/checkout@v4

      - name: Setup Node.js
        uses: actions/setup-node@v4
        with:
          node-version: ${{ env.NODE_VERSION }}
          cache: "npm"

      - name: Install dependencies
        run: npm ci

      - name: Run linter
        run: npm run lint

      - name: Run tests
        run: npm run test:ci

      - name: Run type check
        run: npm run type-check

  build:
    runs-on: ubuntu-latest
    needs: test
    steps:
      - name: Checkout code
        uses: actions/checkout@v4

      - name: Setup Node.js
        uses: actions/setup-node@v4
        with:
          node-version: ${{ env.NODE_VERSION }}
          cache: "npm"

      - name: Install dependencies
        run: npm ci

      - name: Build application
        env:
          VITE_API_URL: ${{ secrets.VITE_API_URL }}
          VITE_ANALYTICS_ID: ${{ secrets.VITE_ANALYTICS_ID }}
        run: npm run build

      - name: Upload build artifacts
        uses: actions/upload-artifact@v4
        with:
          name: build-output
          path: dist/
          retention-days: 7

  deploy:
    runs-on: ubuntu-latest
    needs: build
    if: github.ref == 'refs/heads/main'
    steps:
      - name: Checkout code
        uses: actions/checkout@v4

      - name: Download build artifacts
        uses: actions/download-artifact@v4
        with:
          name: build-output
          path: dist/

      - name: Log in to Container Registry
        uses: docker/login-action@v3
        with:
          registry: ${{ env.REGISTRY }}
          username: ${{ github.actor }}
          password: ${{ secrets.GITHUB_TOKEN }}

      - name: Extract metadata
        id: meta
        uses: docker/metadata-action@v5
        with:
          images: ${{ env.REGISTRY }}/${{ env.IMAGE_NAME }}
          tags: |
            type=ref,event=branch
            type=sha,prefix={{branch}}-
            type=raw,value=latest,enable={{is_default_branch}}

      - name: Build and push Docker image
        uses: docker/build-push-action@v5
        with:
          context: .
          push: true
          tags: ${{ steps.meta.outputs.tags }}
          labels: ${{ steps.meta.outputs.labels }}
          cache-from: type=gha
          cache-to: type=gha,mode=max

      - name: Deploy to production
        run: |
          echo "Deploying to production..."
          # Add deployment commands here
```

### GitLab CI Pipeline

**Complete configuration** (`.gitlab-ci.yml`):

```yaml
stages:
  - test
  - build
  - deploy

variables:
  NODE_VERSION: "24.11.1"
  DOCKER_DRIVER: overlay2
  DOCKER_TLS_CERTDIR: "/certs"

cache:
  key: ${CI_COMMIT_REF_SLUG}
  paths:
    - node_modules/
    - .npm/

test:
  stage: test
  image: node:${NODE_VERSION}-alpine
  before_script:
    - npm ci --cache .npm --prefer-offline
  script:
    - npm run lint
    - npm run test:ci
    - npm run type-check
  coverage: '/All files[^|]*\|[^|]*\s+([\d\.]+)/'
  artifacts:
    reports:
      coverage_report:
        coverage_format: cobertura
        path: coverage/cobertura-coverage.xml

build:
  stage: build
  image: node:${NODE_VERSION}-alpine
  needs: [test]
  before_script:
    - npm ci --cache .npm --prefer-offline
  script:
    - npm run build
  artifacts:
    paths:
      - dist/
    expire_in: 1 week

deploy:production:
  stage: deploy
  image: docker:latest
  services:
    - docker:dind
  needs: [build]
  only:
    - main
  before_script:
    - docker login -u $CI_REGISTRY_USER -p $CI_REGISTRY_PASSWORD $CI_REGISTRY
  script:
    - docker build -t $CI_REGISTRY_IMAGE:$CI_COMMIT_SHA .
    - docker tag $CI_REGISTRY_IMAGE:$CI_COMMIT_SHA $CI_REGISTRY_IMAGE:latest
    - docker push $CI_REGISTRY_IMAGE:$CI_COMMIT_SHA
    - docker push $CI_REGISTRY_IMAGE:latest
```

## 📈 Monitoring Setup

### Application Performance Monitoring

**Sentry integration**:

```javascript
// src/main.jsx
import * as Sentry from "@sentry/react";

Sentry.init({
  dsn: import.meta.env.VITE_SENTRY_DSN,
  environment: import.meta.env.MODE,
  integrations: [new Sentry.BrowserTracing(), new Sentry.Replay()],
  tracesSampleRate: 0.1,
  replaysSessionSampleRate: 0.1,
  replaysOnErrorSampleRate: 1.0,
});
```

**Web Vitals tracking**:

```javascript
// src/utils/reportWebVitals.js
import { onCLS, onFID, onFCP, onLCP, onTTFB } from "web-vitals";

function sendToAnalytics(metric) {
  // Send to analytics service
  const body = JSON.stringify(metric);
  const url = "/api/analytics";

  if (navigator.sendBeacon) {
    navigator.sendBeacon(url, body);
  } else {
    fetch(url, { body, method: "POST", keepalive: true });
  }
}

export function reportWebVitals() {
  onCLS(sendToAnalytics);
  onFID(sendToAnalytics);
  onFCP(sendToAnalytics);
  onLCP(sendToAnalytics);
  onTTFB(sendToAnalytics);
}
```

### Health Check Endpoints

```javascript
// Express server for SSR
app.get("/health", (req, res) => {
  res.status(200).json({
    status: "healthy",
    timestamp: new Date().toISOString(),
    uptime: process.uptime(),
  });
});

app.get("/readiness", async (req, res) => {
  try {
    // Check database connection
    await db.ping();

    res.status(200).json({ status: "ready" });
  } catch (error) {
    res.status(503).json({ status: "not ready", error: error.message });
  }
});
```

## 🔄 Rollback Strategies

### Quick Rollback with Vercel

```bash
# List recent deployments
vercel ls

# Rollback to previous deployment
vercel rollback [deployment-url]

# Rollback to specific deployment
vercel alias set [deployment-url] production-domain.com
```

### Docker Rollback

```bash
# Tag current production
docker tag react-app:latest react-app:backup-$(date +%Y%m%d)

# Rollback to previous version
docker pull react-app:previous-sha
docker tag react-app:previous-sha react-app:latest
docker service update --image react-app:latest react-app-service
```

### Git-based Rollback

```bash
# Revert to previous commit
git revert HEAD --no-edit
git push origin main

# Automatic redeployment triggers on push
```

## ⚡ Performance Optimization

### Compression

**Brotli compression** (preferred over gzip):

```nginx
# Enable Brotli in nginx
load_module modules/ngx_http_brotli_filter_module.so;
load_module modules/ngx_http_brotli_static_module.so;

http {
  brotli on;
  brotli_comp_level 6;
  brotli_types text/plain text/css application/json application/javascript
               text/xml application/xml application/xml+rss text/javascript;
}
```

**Compression in CDN**:

- CloudFront: Enable automatic compression in cache behavior
- Cloudflare: Enable "Auto Minify" for JS/CSS/HTML
- Vercel/Netlify: Automatic Brotli/Gzip compression

### Caching Headers

**Optimal caching strategy**:

```nginx
# Hashed assets (js/css with content hash in filename) - cache forever
location ~* \.(js|css)$ {
  expires 1y;
  add_header Cache-Control "public, immutable";
}

# Images, fonts - cache with validation
location ~* \.(png|jpg|jpeg|gif|ico|svg|woff|woff2|ttf|eot)$ {
  expires 1y;
  add_header Cache-Control "public, max-age=31536000";
}

# HTML - never cache
location ~* \.html$ {
  expires -1;
  add_header Cache-Control "no-store, no-cache, must-revalidate, proxy-revalidate, max-age=0";
}

# Service worker - must revalidate
location = /service-worker.js {
  expires 0;
  add_header Cache-Control "no-cache, must-revalidate";
}
```

**CDN caching configuration**:

```javascript
// CloudFront cache policy
{
  "MinTTL": 0,
  "DefaultTTL": 86400,
  "MaxTTL": 31536000,
  "ParametersInCacheKeyAndForwardedToOrigin": {
    "EnableAcceptEncodingGzip": true,
    "EnableAcceptEncodingBrotli": true,
    "HeadersConfig": {
      "HeaderBehavior": "none"
    },
    "CookiesConfig": {
      "CookieBehavior": "none"
    },
    "QueryStringsConfig": {
      "QueryStringBehavior": "none"
    }
  }
}
```

## 📚 Related Resources

**Related Explanations**:

- [State Management](./ex-so-plwe-fere__state-management.md) - Managing application state
- [Performance](./ex-so-plwe-fere__performance.md) - Frontend performance techniques
- [Security](./ex-so-plwe-fere__security.md) - Frontend security best practices
- [Configuration](./ex-so-plwe-fere__configuration.md) - Build and environment configuration

---

**Cross-references**: [React](./README.md), [Frontend Architecture](../README.md), [Platform Web](../../README.md)
