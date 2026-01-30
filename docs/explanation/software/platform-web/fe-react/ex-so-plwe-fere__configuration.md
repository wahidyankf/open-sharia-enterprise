---
title: React Configuration
description: React build configuration and environment management for enterprise applications
category: explanation
tags:
  - react
  - vite
  - typescript
  - configuration
  - build-tools
  - environment-variables
created: 2026-01-29
updated: 2026-01-29
---

# React Configuration

This document explains React build configuration and environment management for enterprise applications, covering build tools, TypeScript setup, environment variables, deployment configuration, and optimization strategies.

## 📋 Overview

Modern React applications require careful configuration across multiple layers:

- **Build Tools**: Vite for fast development and optimized production builds
- **Type Safety**: TypeScript with strict mode for compile-time error detection
- **Environment Management**: .env files for different deployment contexts
- **Code Quality**: ESLint and Prettier for consistent code standards
- **Testing**: Vitest for fast unit and integration testing
- **Deployment**: Optimization for various hosting platforms

## ⚙️ Vite Configuration

Vite provides fast development server with Hot Module Replacement (HMR) and optimized production builds.

### Basic vite.config.ts

```typescript
import { defineConfig } from "vite";
import react from "@vitejs/plugin-react";
import path from "path";

export default defineConfig({
  plugins: [react()],

  // Path aliases for clean imports
  resolve: {
    alias: {
      "@": path.resolve(__dirname, "./src"),
      "@components": path.resolve(__dirname, "./src/components"),
      "@hooks": path.resolve(__dirname, "./src/hooks"),
      "@utils": path.resolve(__dirname, "./src/utils"),
      "@types": path.resolve(__dirname, "./src/types"),
      "@assets": path.resolve(__dirname, "./src/assets"),
    },
  },

  // Development server configuration
  server: {
    port: 3000,
    strictPort: false,
    open: true,
    cors: true,
    proxy: {
      "/api": {
        target: "http://localhost:8000",
        changeOrigin: true,
        rewrite: (path) => path.replace(/^\/api/, ""),
      },
    },
  },

  // Production build configuration
  build: {
    outDir: "dist",
    sourcemap: true,
    minify: "terser",
    target: "esnext",
    chunkSizeWarningLimit: 1000,
    rollupOptions: {
      output: {
        manualChunks: {
          vendor: ["react", "react-dom"],
          router: ["react-router-dom"],
        },
      },
    },
  },

  // Preview server (for testing production builds locally)
  preview: {
    port: 4173,
    strictPort: true,
  },
});
```

### Advanced Plugin Configuration

```typescript
import { defineConfig } from "vite";
import react from "@vitejs/plugin-react";
import { visualizer } from "rollup-plugin-visualizer";
import { compression } from "vite-plugin-compression2";
import checker from "vite-plugin-checker";

export default defineConfig({
  plugins: [
    react({
      // Enable React Fast Refresh
      fastRefresh: true,
      // Babel configuration for React
      babel: {
        plugins: [["@babel/plugin-proposal-decorators", { legacy: true }]],
      },
    }),

    // Type checking in separate process
    checker({
      typescript: true,
      eslint: {
        lintCommand: 'eslint "./src/**/*.{ts,tsx}"',
      },
    }),

    // Bundle size visualization
    visualizer({
      filename: "./dist/stats.html",
      open: false,
      gzipSize: true,
      brotliSize: true,
    }),

    // Compression for production
    compression({
      algorithm: "gzip",
      exclude: [/\.(br)$/, /\.(gz)$/],
    }),
    compression({
      algorithm: "brotliCompress",
      exclude: [/\.(br)$/, /\.(gz)$/],
    }),
  ],
});
```

## 📘 TypeScript Configuration

TypeScript provides compile-time type safety and improved developer experience.

### Strict tsconfig.json

```json
{
  "compilerOptions": {
    "target": "ES2020",
    "useDefineForClassFields": true,
    "lib": ["ES2020", "DOM", "DOM.Iterable"],
    "module": "ESNext",
    "skipLibCheck": true,

    /* Bundler mode */
    "moduleResolution": "bundler",
    "allowImportingTsExtensions": true,
    "resolveJsonModule": true,
    "isolatedModules": true,
    "noEmit": true,
    "jsx": "react-jsx",

    /* Linting */
    "strict": true,
    "noUnusedLocals": true,
    "noUnusedParameters": true,
    "noFallthroughCasesInSwitch": true,
    "noUncheckedIndexedAccess": true,
    "noImplicitOverride": true,
    "allowUnusedLabels": false,
    "allowUnreachableCode": false,
    "exactOptionalPropertyTypes": true,
    "noImplicitReturns": true,

    /* Path mapping (must match vite.config.ts aliases) */
    "baseUrl": ".",
    "paths": {
      "@/*": ["./src/*"],
      "@components/*": ["./src/components/*"],
      "@hooks/*": ["./src/hooks/*"],
      "@utils/*": ["./src/utils/*"],
      "@types/*": ["./src/types/*"],
      "@assets/*": ["./src/assets/*"]
    }
  },
  "include": ["src"],
  "exclude": ["node_modules", "dist", "build"],
  "references": [{ "path": "./tsconfig.node.json" }]
}
```

### tsconfig.node.json (for Vite config files)

```json
{
  "compilerOptions": {
    "composite": true,
    "skipLibCheck": true,
    "module": "ESNext",
    "moduleResolution": "bundler",
    "allowSyntheticDefaultImports": true
  },
  "include": ["vite.config.ts"]
}
```

## 🌍 Environment Variables

Environment variables configure application behavior across different deployment contexts.

### Environment File Structure

```
.env                  # Shared defaults (committed to git)
.env.local            # Local overrides (gitignored)
.env.development      # Development-specific (committed)
.env.development.local # Local dev overrides (gitignored)
.env.production       # Production-specific (committed)
.env.production.local  # Local prod overrides (gitignored)
```

### .env Example

```bash
# API Configuration
VITE_API_BASE_URL=http://localhost:8000
VITE_API_TIMEOUT=30000

# Feature Flags
VITE_FEATURE_AUTH=true
VITE_FEATURE_ANALYTICS=false

# Environment Metadata
VITE_APP_VERSION=1.0.0
VITE_BUILD_DATE=2026-01-29
```

### .env.production Example

```bash
# Production API
VITE_API_BASE_URL=https://api.production.com
VITE_API_TIMEOUT=10000

# Feature Flags
VITE_FEATURE_AUTH=true
VITE_FEATURE_ANALYTICS=true

# Production Metadata
VITE_APP_VERSION=1.0.0
VITE_BUILD_DATE=2026-01-29
```

### Type-Safe Environment Variables

```typescript
// src/config/env.ts
interface ImportMetaEnv {
  readonly VITE_API_BASE_URL: string;
  readonly VITE_API_TIMEOUT: string;
  readonly VITE_FEATURE_AUTH: string;
  readonly VITE_FEATURE_ANALYTICS: string;
  readonly VITE_APP_VERSION: string;
  readonly VITE_BUILD_DATE: string;
}

interface ImportMeta {
  readonly env: ImportMetaEnv;
}

// Runtime configuration with validation
export const config = {
  api: {
    baseUrl: import.meta.env.VITE_API_BASE_URL,
    timeout: parseInt(import.meta.env.VITE_API_TIMEOUT, 10),
  },
  features: {
    auth: import.meta.env.VITE_FEATURE_AUTH === "true",
    analytics: import.meta.env.VITE_FEATURE_ANALYTICS === "true",
  },
  app: {
    version: import.meta.env.VITE_APP_VERSION,
    buildDate: import.meta.env.VITE_BUILD_DATE,
  },
} as const;

// Validation on app startup
export function validateConfig(): void {
  if (!config.api.baseUrl) {
    throw new Error("VITE_API_BASE_URL is required");
  }
  if (isNaN(config.api.timeout)) {
    throw new Error("VITE_API_TIMEOUT must be a number");
  }
}
```

### Usage in Components

```typescript
import { config } from '@/config/env';

export function ApiClient() {
  const fetchData = async () => {
    const response = await fetch(`${config.api.baseUrl}/data`, {
      timeout: config.api.timeout,
    });
    return response.json();
  };

  return <div>API Base URL: {config.api.baseUrl}</div>;
}
```

## 🚀 Build Optimization

### Code Splitting Strategy

```typescript
// vite.config.ts
export default defineConfig({
  build: {
    rollupOptions: {
      output: {
        manualChunks: {
          // Core dependencies
          "vendor-react": ["react", "react-dom"],
          "vendor-router": ["react-router-dom"],

          // Large UI libraries
          "vendor-ui": ["@mui/material", "@emotion/react", "@emotion/styled"],

          // Utility libraries
          "vendor-utils": ["lodash-es", "date-fns", "zod"],

          // Analytics and monitoring
          "vendor-analytics": ["@sentry/react", "web-vitals"],
        },
      },
    },
  },
});
```

### Dynamic Imports (Route-Based Code Splitting)

```typescript
// src/App.tsx
import { lazy, Suspense } from 'react';
import { BrowserRouter, Routes, Route } from 'react-router-dom';

// Lazy-loaded route components
const HomePage = lazy(() => import('@/pages/Home'));
const DashboardPage = lazy(() => import('@/pages/Dashboard'));
const SettingsPage = lazy(() => import('@/pages/Settings'));

function App() {
  return (
    <BrowserRouter>
      <Suspense fallback={<LoadingSpinner />}>
        <Routes>
          <Route path="/" element={<HomePage />} />
          <Route path="/dashboard" element={<DashboardPage />} />
          <Route path="/settings" element={<SettingsPage />} />
        </Routes>
      </Suspense>
    </BrowserRouter>
  );
}
```

### Tree Shaking

```typescript
// Ensure imports are tree-shakeable
import { debounce } from "lodash-es"; // ✅ Good
import debounce from "lodash/debounce"; // ✅ Also good
import _ from "lodash"; // ❌ Imports entire library
```

### Minification Configuration

```typescript
// vite.config.ts
export default defineConfig({
  build: {
    minify: "terser",
    terserOptions: {
      compress: {
        drop_console: true, // Remove console.log in production
        drop_debugger: true,
        pure_funcs: ["console.log", "console.info"],
      },
      format: {
        comments: false, // Remove comments
      },
    },
  },
});
```

## 🗂️ Path Aliases

Path aliases simplify imports and improve code maintainability.

### Configuring Aliases

```typescript
// vite.config.ts
import path from "path";

export default defineConfig({
  resolve: {
    alias: {
      "@": path.resolve(__dirname, "./src"),
      "@components": path.resolve(__dirname, "./src/components"),
      "@hooks": path.resolve(__dirname, "./src/hooks"),
      "@utils": path.resolve(__dirname, "./src/utils"),
      "@types": path.resolve(__dirname, "./src/types"),
      "@assets": path.resolve(__dirname, "./src/assets"),
      "@api": path.resolve(__dirname, "./src/api"),
      "@store": path.resolve(__dirname, "./src/store"),
      "@config": path.resolve(__dirname, "./src/config"),
    },
  },
});
```

### Using Aliases in Code

```typescript
// Without aliases
import { Button } from "../../../components/ui/Button";
import { useAuth } from "../../../hooks/useAuth";
import { api } from "../../../api/client";

// With aliases
import { Button } from "@components/ui/Button";
import { useAuth } from "@hooks/useAuth";
import { api } from "@api/client";
```

## 🎨 Asset Management

### Static Assets

```typescript
// vite.config.ts
export default defineConfig({
  build: {
    assetsDir: "assets",
    assetsInlineLimit: 4096, // Assets < 4KB inlined as base64
  },
  publicDir: "public", // Static files served as-is
});
```

### Importing Assets

```typescript
// Images
import logo from '@assets/logo.png'; // Build-time processed
import logoUrl from '@assets/logo.png?url'; // Get URL
import logoRaw from '@assets/logo.svg?raw'; // Get raw content

// Fonts
import '@assets/fonts/inter.woff2';

// Usage
function Header() {
  return <img src={logo} alt="Company logo" />;
}
```

### public/ Directory

```
public/
├── favicon.ico          # Served at /favicon.ico
├── robots.txt           # Served at /robots.txt
├── manifest.json        # PWA manifest
└── static/
    └── privacy.pdf      # Served at /static/privacy.pdf
```

## 🔧 Development Server

### Development Configuration

```typescript
// vite.config.ts
export default defineConfig({
  server: {
    port: 3000,
    strictPort: false, // Try next available port if 3000 is taken
    open: true, // Open browser on server start
    cors: true,
    host: true, // Listen on all addresses (0.0.0.0)

    // API proxy to avoid CORS issues
    proxy: {
      "/api": {
        target: "http://localhost:8000",
        changeOrigin: true,
        rewrite: (path) => path.replace(/^\/api/, ""),
      },
      "/auth": {
        target: "http://localhost:8001",
        changeOrigin: true,
        secure: false,
      },
    },

    // HTTPS for local development
    https: false, // Set to true and provide cert/key for HTTPS
  },

  // Hot Module Replacement (HMR)
  optimizeDeps: {
    include: ["react", "react-dom"],
  },
});
```

### Hot Reload Configuration

```typescript
// src/main.tsx
if (import.meta.hot) {
  // Accept HMR updates
  import.meta.hot.accept();

  // Cleanup on module replacement
  import.meta.hot.dispose(() => {
    console.log("Module disposed");
  });
}
```

## 📦 Production Builds

### Production Optimization

```typescript
// vite.config.ts
export default defineConfig(({ mode }) => {
  const isProd = mode === "production";

  return {
    build: {
      outDir: "dist",
      sourcemap: isProd ? false : true, // No sourcemaps in production
      minify: isProd ? "terser" : false,
      cssMinify: isProd,

      // Performance hints
      chunkSizeWarningLimit: 1000,
      reportCompressedSize: true,

      rollupOptions: {
        output: {
          // Consistent chunk naming
          entryFileNames: "assets/[name].[hash].js",
          chunkFileNames: "assets/[name].[hash].js",
          assetFileNames: "assets/[name].[hash].[ext]",
        },
      },
    },

    // Environment-specific settings
    define: {
      __DEV__: !isProd,
      __PROD__: isProd,
    },
  };
});
```

### Build Scripts

```json
{
  "scripts": {
    "dev": "vite",
    "build": "tsc && vite build",
    "build:prod": "NODE_ENV=production vite build",
    "preview": "vite preview",
    "analyze": "vite-bundle-visualizer"
  }
}
```

## ✅ ESLint and Prettier

### ESLint Configuration (.eslintrc.cjs)

```javascript
module.exports = {
  root: true,
  env: {
    browser: true,
    es2020: true,
  },
  extends: [
    "eslint:recommended",
    "plugin:@typescript-eslint/recommended",
    "plugin:react-hooks/recommended",
    "plugin:react/recommended",
    "plugin:jsx-a11y/recommended",
    "prettier", // Must be last
  ],
  ignorePatterns: ["dist", ".eslintrc.cjs"],
  parser: "@typescript-eslint/parser",
  parserOptions: {
    ecmaVersion: "latest",
    sourceType: "module",
    ecmaFeatures: {
      jsx: true,
    },
  },
  plugins: ["react-refresh", "@typescript-eslint", "jsx-a11y"],
  rules: {
    "react-refresh/only-export-components": ["warn", { allowConstantExport: true }],
    "@typescript-eslint/no-unused-vars": ["error", { argsIgnorePattern: "^_" }],
    "react/react-in-jsx-scope": "off", // Not needed with React 17+
    "react/prop-types": "off", // Using TypeScript
  },
  settings: {
    react: {
      version: "detect",
    },
  },
};
```

### Prettier Configuration (.prettierrc)

```json
{
  "semi": true,
  "trailingComma": "es5",
  "singleQuote": true,
  "printWidth": 80,
  "tabWidth": 2,
  "useTabs": false,
  "arrowParens": "always",
  "endOfLine": "lf"
}
```

### Integration Scripts

```json
{
  "scripts": {
    "lint": "eslint . --ext ts,tsx --report-unused-disable-directives --max-warnings 0",
    "lint:fix": "eslint . --ext ts,tsx --fix",
    "format": "prettier --write \"src/**/*.{ts,tsx,json,css,md}\"",
    "format:check": "prettier --check \"src/**/*.{ts,tsx,json,css,md}\""
  }
}
```

## 🧪 Testing Configuration

### Vitest Setup (vitest.config.ts)

```typescript
import { defineConfig } from "vitest/config";
import react from "@vitejs/plugin-react";
import path from "path";

export default defineConfig({
  plugins: [react()],
  test: {
    globals: true,
    environment: "jsdom",
    setupFiles: "./src/test/setup.ts",
    coverage: {
      provider: "v8",
      reporter: ["text", "json", "html"],
      exclude: ["node_modules/", "src/test/", "**/*.config.ts", "**/*.d.ts"],
    },
  },
  resolve: {
    alias: {
      "@": path.resolve(__dirname, "./src"),
      "@components": path.resolve(__dirname, "./src/components"),
      "@hooks": path.resolve(__dirname, "./src/hooks"),
      "@utils": path.resolve(__dirname, "./src/utils"),
    },
  },
});
```

### Test Setup File (src/test/setup.ts)

```typescript
import "@testing-library/jest-dom";
import { cleanup } from "@testing-library/react";
import { afterEach } from "vitest";

// Cleanup after each test
afterEach(() => {
  cleanup();
});

// Mock window.matchMedia
Object.defineProperty(window, "matchMedia", {
  writable: true,
  value: vi.fn().mockImplementation((query) => ({
    matches: false,
    media: query,
    onchange: null,
    addListener: vi.fn(),
    removeListener: vi.fn(),
    addEventListener: vi.fn(),
    removeEventListener: vi.fn(),
    dispatchEvent: vi.fn(),
  })),
});
```

### Test Scripts

```json
{
  "scripts": {
    "test": "vitest",
    "test:ui": "vitest --ui",
    "test:coverage": "vitest --coverage",
    "test:run": "vitest run"
  }
}
```

## 📦 Package Management

### package.json Structure

```json
{
  "name": "@company/app-web",
  "private": true,
  "version": "1.0.0",
  "type": "module",
  "scripts": {
    "dev": "vite",
    "build": "tsc && vite build",
    "preview": "vite preview",
    "lint": "eslint . --ext ts,tsx",
    "lint:fix": "eslint . --ext ts,tsx --fix",
    "format": "prettier --write \"src/**/*.{ts,tsx,json,css,md}\"",
    "test": "vitest",
    "test:coverage": "vitest --coverage"
  },
  "dependencies": {
    "react": "^18.2.0",
    "react-dom": "^18.2.0",
    "react-router-dom": "^6.20.0"
  },
  "devDependencies": {
    "@types/react": "^18.2.43",
    "@types/react-dom": "^18.2.17",
    "@typescript-eslint/eslint-plugin": "^6.14.0",
    "@typescript-eslint/parser": "^6.14.0",
    "@vitejs/plugin-react": "^4.2.1",
    "eslint": "^8.55.0",
    "eslint-plugin-react-hooks": "^4.6.0",
    "eslint-plugin-react-refresh": "^0.4.5",
    "prettier": "^3.1.1",
    "typescript": "^5.2.2",
    "vite": "^5.0.8",
    "vitest": "^1.0.4"
  }
}
```

### Npm Workspace (Monorepo)

```json
{
  "name": "@company/monorepo",
  "private": true,
  "workspaces": ["apps/*", "libs/*"],
  "scripts": {
    "build": "npm run build --workspaces",
    "test": "npm run test --workspaces",
    "lint": "npm run lint --workspaces"
  }
}
```

## 🌐 Deployment Configuration

### Vercel Deployment

```json
{
  "buildCommand": "npm run build",
  "outputDirectory": "dist",
  "installCommand": "npm install",
  "framework": "vite",
  "env": {
    "VITE_API_BASE_URL": "@api-base-url-prod",
    "VITE_FEATURE_AUTH": "true"
  }
}
```

### Docker Deployment

```dockerfile
# Multi-stage Dockerfile
FROM node:20-alpine AS builder

WORKDIR /app
COPY package*.json ./
RUN npm ci

COPY . .
RUN npm run build

FROM nginx:alpine

COPY --from=builder /app/dist /usr/share/nginx/html
COPY nginx.conf /etc/nginx/nginx.conf

EXPOSE 80
CMD ["nginx", "-g", "daemon off;"]
```

### nginx.conf

```nginx
server {
  listen 80;
  server_name _;

  root /usr/share/nginx/html;
  index index.html;

  # Enable gzip compression
  gzip on;
  gzip_types text/plain text/css application/json application/javascript text/xml application/xml application/xml+rss text/javascript;

  # SPA routing
  location / {
    try_files $uri $uri/ /index.html;
  }

  # Cache static assets
  location ~* \.(js|css|png|jpg|jpeg|gif|ico|svg|woff|woff2|ttf|eot)$ {
    expires 1y;
    add_header Cache-Control "public, immutable";
  }
}
```

### CDN Configuration (Cloudflare)

Key settings:

- **Caching**: Cache static assets (JS/CSS/images) for 1 year
- **Minification**: Enable Auto Minify for HTML/CSS/JS
- **Brotli**: Enable Brotli compression
- **Always Use HTTPS**: Redirect HTTP to HTTPS
- **HTTP/3**: Enable QUIC protocol

## 🔗 Related Documentation

- [React Project Structure](./ex-so-plwe-fera__project-structure.md) - File and folder organization
- [React State Management](ex-so-plwe-fere__state-management.md) - State management patterns
- [React Development Environment Setup](../../../../../../how-to/software/platform-web/fe-react/hoto-so-plwe-fera__setup-dev-environment.md) - Setting up development environment

## 📚 References

**Build Tools**:

- [Vite Documentation](https://vitejs.dev/) - Official Vite documentation
- [Rollup Documentation](https://rollupjs.org/) - Rollup bundler (used by Vite)

**TypeScript**:

- [TypeScript Handbook](https://www.typescriptlang.org/docs/handbook/intro.html) - Official TypeScript guide
- [React TypeScript Cheatsheet](https://react-typescript-cheatsheet.netlify.app/) - Community-maintained TypeScript patterns

**Code Quality**:

- [ESLint Documentation](https://eslint.org/docs/latest/) - ESLint configuration
- [Prettier Documentation](https://prettier.io/docs/en/) - Code formatting

**Testing**:

- [Vitest Documentation](https://vitest.dev/) - Vite-native testing framework
- [React Testing Library](https://testing-library.com/docs/react-testing-library/intro/) - Testing utilities

**Deployment**:

- [Vercel Documentation](https://vercel.com/docs) - Vercel deployment platform
- [Docker Documentation](https://docs.docker.com/) - Containerization
