---
title: "React Build & Deployment"
description: Build configuration and deployment strategies for React applications
category: explanation
subcategory: platform-web
tags:
  - react
  - build
  - deployment
  - vite
  - production
related:
  - ./ex-so-plwe-tsre__best-practices.md
principles:
  - automation-over-manual
  - reproducibility
last_updated: 2026-01-25
---

# React Build & Deployment

## Quick Reference

**Navigation**: [Stack Libraries](../README.md) > [TypeScript React](./README.md) > Build & Deployment

**Related Guides**:

- [Best Practices](./ex-so-plwe-tsre__best-practices.md) - Production standards
- [Performance](./ex-so-plwe-tsre__performance.md) - Build optimization

## Overview

Building and deploying React applications for production requires configuration of build tools, environment variables, optimization, and deployment pipelines.

**Target Audience**: Developers deploying React applications to production, particularly Islamic finance platforms requiring secure, optimized builds.

**React Version**: React 18.2+ with TypeScript 5+
**Build Tool**: Vite

## Vite Configuration

### Basic Configuration

```typescript
// vite.config.ts
import { defineConfig } from "vite";
import react from "@vitejs/plugin-react";
import path from "path";

export default defineConfig({
  plugins: [react()],

  resolve: {
    alias: {
      "@": path.resolve(__dirname, "./src"),
      "@features": path.resolve(__dirname, "./src/features"),
      "@shared": path.resolve(__dirname, "./src/shared"),
      "@core": path.resolve(__dirname, "./src/core"),
    },
  },

  build: {
    outDir: "dist",
    sourcemap: true,
    rollupOptions: {
      output: {
        manualChunks: {
          "react-vendor": ["react", "react-dom", "react-router-dom"],
          "ui-vendor": ["@radix-ui/react-dialog", "@radix-ui/react-dropdown-menu"],
        },
      },
    },
  },

  server: {
    port: 3000,
    proxy: {
      "/api": {
        target: "http://localhost:8080",
        changeOrigin: true,
      },
    },
  },
});
```

### Environment Variables

```typescript
// .env.development
VITE_API_URL=http://localhost:8080/api
VITE_APP_TITLE=OSE Platform (Dev)

// .env.production
VITE_API_URL=https://api.oseplatform.com
VITE_APP_TITLE=OSE Platform

// src/config.ts
export const config = {
  apiUrl: import.meta.env.VITE_API_URL,
  appTitle: import.meta.env.VITE_APP_TITLE,
  isDevelopment: import.meta.env.DEV,
  isProduction: import.meta.env.PROD,
} as const;

// Usage
import { config } from './config';

const response = await fetch(`${config.apiUrl}/donations`);
```

### Type-Safe Environment Variables

```typescript
// src/env.d.ts
/// <reference types="vite/client" />

interface ImportMetaEnv {
  readonly VITE_API_URL: string;
  readonly VITE_APP_TITLE: string;
  readonly VITE_ENABLE_ANALYTICS: string;
}

interface ImportMeta {
  readonly env: ImportMetaEnv;
}

// src/config.ts
function getEnvVar(key: keyof ImportMetaEnv): string {
  const value = import.meta.env[key];

  if (!value) {
    throw new Error(`Missing environment variable: ${key}`);
  }

  return value;
}

export const config = {
  apiUrl: getEnvVar("VITE_API_URL"),
  appTitle: getEnvVar("VITE_APP_TITLE"),
  enableAnalytics: getEnvVar("VITE_ENABLE_ANALYTICS") === "true",
} as const;
```

## Production Builds

### Build Optimization

```typescript
// vite.config.ts
import { defineConfig } from "vite";
import react from "@vitejs/plugin-react";
import { visualizer } from "rollup-plugin-visualizer";

export default defineConfig({
  plugins: [
    react(),
    visualizer({
      // Bundle size analysis
      open: true,
      gzipSize: true,
      brotliSize: true,
    }),
  ],

  build: {
    target: "es2020",
    minify: "terser",
    terserOptions: {
      compress: {
        drop_console: true, // Remove console.log in production
        drop_debugger: true,
      },
    },
    rollupOptions: {
      output: {
        manualChunks(id) {
          // Split vendor chunks
          if (id.includes("node_modules")) {
            if (id.includes("react")) {
              return "react-vendor";
            }
            if (id.includes("@tanstack/react-query")) {
              return "query-vendor";
            }
            return "vendor";
          }
        },
      },
    },
  },
});
```

## Docker Deployment

### Multi-Stage Dockerfile

```dockerfile
# Build stage
FROM node:24-alpine AS build

WORKDIR /app

COPY package*.json ./
RUN npm ci

COPY . .
RUN npm run build

# Production stage
FROM nginx:alpine

COPY --from=build /app/dist /usr/share/nginx/html
COPY nginx.conf /etc/nginx/conf.d/default.conf

EXPOSE 80

CMD ["nginx", "-g", "daemon off;"]
```

### Nginx Configuration

```nginx
server {
    listen 80;
    server_name _;

    root /usr/share/nginx/html;
    index index.html;

    # Compression
    gzip on;
    gzip_types text/plain text/css application/json application/javascript text/xml application/xml;

    # SPA routing
    location / {
        try_files $uri $uri/ /index.html;
    }

    # API proxy
    location /api {
        proxy_pass http://backend:8080;
        proxy_set_header Host $host;
        proxy_set_header X-Real-IP $remote_addr;
    }

    # Cache static assets
    location ~* \.(js|css|png|jpg|jpeg|gif|ico|svg|woff|woff2|ttf|eot)$ {
        expires 1y;
        add_header Cache-Control "public, immutable";
    }
}
```

## Related Documentation

- **[Best Practices](./ex-so-plwe-tsre__best-practices.md)** - Production standards
- **[Performance](./ex-so-plwe-tsre__performance.md)** - Build optimization

---

**Last Updated**: 2026-01-25
