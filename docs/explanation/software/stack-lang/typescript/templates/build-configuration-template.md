---
title: TypeScript Build Configuration Template
description: Template for tsconfig.json, ESLint 9.x flat config, Jest testing, package.json, and CI/CD configurations with complete build automation for TypeScript projects
category: template
tags:
  - typescript
  - build-automation
  - tsconfig
  - eslint
  - jest
  - ci-cd
  - github-actions
  - prettier
  - vitest
  - ts-5.0
  - ts-5.1
  - ts-5.2
  - ts-5.3
  - ts-5.4
  - ts-5.5
  - ts-5.6
  - ts-5.7
related:
  - ex-so-stla-ts__modules-and-dependencies.md
  - ex-so-stla-ts__best-practices.md
  - ex-so-stla-ts__linting-and-formatting.md
principles:
  - reproducibility
  - automation-over-manual
  - explicit-over-implicit
created: 2026-01-24
updated: 2026-01-24
---

# TypeScript Build Configuration Template

This template provides standardized build configurations for TypeScript projects using tsconfig.json, ESLint 9.x flat config, Jest/Vitest, and GitHub Actions CI/CD. The example implements an Islamic finance donation service demonstrating complete build automation.

## Table of Contents

1. [Overview](#overview)
2. [tsconfig.json Configuration](#tsconfigjson-configuration)
3. [ESLint 9.x Flat Config](#eslint-9x-flat-config)
4. [Jest Configuration](#jest-configuration)
5. [Vitest Configuration](#vitest-configuration)
6. [Package.json Scripts](#packagejson-scripts)
7. [Prettier Configuration](#prettier-configuration)
8. [GitHub Actions CI/CD](#github-actions-cicd)
9. [Docker Multi-Stage Build](#docker-multi-stage-build)
10. [Complete Example Project](#complete-example-project)
11. [Usage Guidelines](#usage-guidelines)

## Overview

### Build Automation in TypeScript

TypeScript build automation serves three primary goals:

1. **Reproducibility**: Same code produces same output across environments
2. **Consistency**: Standardized build process for all team members
3. **Automation**: Eliminate manual steps and human error

**Key components**:

- **tsconfig.json**: TypeScript compiler configuration (strict mode, paths, output)
- **ESLint 9.x**: Code quality checks with flat config format
- **Jest/Vitest**: Test framework configuration
- **package.json**: Scripts for build, test, lint automation
- **Prettier**: Code formatting enforcement
- **GitHub Actions**: CI/CD pipeline automation

### Why This Matters

**Without build automation**:

```bash
# Manual, error-prone process
tsc
npm test
eslint .
prettier --write .

# Different developers use different flags
# No version consistency
# Inconsistent formatting
```

**With build automation**:

```bash
# Standardized, reproducible
npm run build
npm test
npm run lint
npm run format

# Everyone uses same configuration
# Version info automatically injected
# Consistent output everywhere
```

## tsconfig.json Configuration

### Base Configuration

```json
{
  "compilerOptions": {
    // ========================================
    // Language and Environment
    // ========================================

    // Target ES2022 for modern Node.js (18+)
    "target": "ES2022",
    "lib": ["ES2022"],
    "module": "Node16", // Node.js ESM support
    "moduleResolution": "Node16",

    // ========================================
    // Strict Type Checking
    // ========================================

    "strict": true, // Enable all strict type-checking options
    "noImplicitAny": true, // Error on expressions with implied 'any'
    "strictNullChecks": true, // Strict null checks
    "strictFunctionTypes": true, // Strict function type checks
    "strictBindCallApply": true, // Strict bind/call/apply
    "strictPropertyInitialization": true, // Strict class property initialization
    "noImplicitThis": true, // Error when 'this' has type 'any'
    "alwaysStrict": true, // Parse in strict mode

    // ========================================
    // Additional Checks
    // ========================================

    "noUnusedLocals": true, // Error on unused local variables
    "noUnusedParameters": true, // Error on unused parameters
    "noImplicitReturns": true, // Error when not all code paths return a value
    "noFallthroughCasesInSwitch": true, // Error on fallthrough cases in switch
    "noUncheckedIndexedAccess": true, // Add undefined to unverified index access
    "noPropertyAccessFromIndexSignature": true, // Require bracket access for index signature properties
    "allowUnusedLabels": false, // Error on unused labels
    "allowUnreachableCode": false, // Error on unreachable code

    // ========================================
    // Module Resolution
    // ========================================

    "baseUrl": "./", // Base directory for module resolution
    "paths": {
      "@/*": ["src/*"], // Path mapping for imports
      "@domain/*": ["src/domain/*"],
      "@application/*": ["src/application/*"],
      "@infrastructure/*": ["src/infrastructure/*"],
      "@shared/*": ["src/shared/*"]
    },
    "resolveJsonModule": true, // Allow importing JSON files
    "esModuleInterop": true, // Enable CommonJS/ES module interoperability
    "allowSyntheticDefaultImports": true, // Allow default imports from modules with no default export

    // ========================================
    // Emit Configuration
    // ========================================

    "outDir": "./dist", // Output directory
    "rootDir": "./src", // Root directory of input files
    "declaration": true, // Generate .d.ts files
    "declarationMap": true, // Generate sourcemap for .d.ts files
    "sourceMap": true, // Generate sourcemap files
    "removeComments": false, // Keep comments in output
    "importHelpers": true, // Import helper functions from tslib
    "downlevelIteration": true, // Emit more compliant but verbose iteration code

    // ========================================
    // Interop Constraints
    // ========================================

    "isolatedModules": true, // Ensure each file can be safely transpiled
    "forceConsistentCasingInFileNames": true, // Disallow inconsistent casing in file names
    "skipLibCheck": true, // Skip type checking of declaration files

    // ========================================
    // Advanced Options
    // ========================================

    "experimentalDecorators": true, // Enable decorators (for NestJS, TypeORM)
    "emitDecoratorMetadata": true, // Emit design-type metadata (for NestJS DI)
    "incremental": true, // Enable incremental compilation
    "tsBuildInfoFile": "./dist/.tsbuildinfo" // Specify file for incremental compilation info
  },
  "include": ["src/**/*"],
  "exclude": ["node_modules", "dist", "**/*.spec.ts", "**/*.test.ts"]
}
```

### Project-Specific Configurations

**tsconfig.build.json** (Production builds):

```json
{
  "extends": "./tsconfig.json",
  "compilerOptions": {
    "sourceMap": false, // Disable sourcemaps for production
    "declaration": true, // Generate .d.ts for library distribution
    "removeComments": true // Remove comments in production
  },
  "exclude": ["node_modules", "dist", "**/*.spec.ts", "**/*.test.ts", "tests"]
}
```

**tsconfig.test.json** (Test configuration):

```json
{
  "extends": "./tsconfig.json",
  "compilerOptions": {
    "types": ["jest", "node"], // Include Jest types
    "sourceMap": true, // Enable sourcemaps for debugging tests
    "inlineSourceMap": false,
    "inlineSources": false
  },
  "include": ["src/**/*", "tests/**/*", "**/*.spec.ts", "**/*.test.ts"]
}
```

## ESLint 9.x Flat Config

### Complete eslint.config.js

```javascript
// eslint.config.js (ESLint 9.x flat config format)
import js from "@eslint/js";
import tseslint from "typescript-eslint";
import prettier from "eslint-config-prettier";

export default [
  // ========================================
  // Base JavaScript Rules
  // ========================================

  js.configs.recommended,

  // ========================================
  // TypeScript Configuration
  // ========================================

  ...tseslint.configs.recommendedTypeChecked,
  ...tseslint.configs.stylisticTypeChecked,

  {
    languageOptions: {
      parserOptions: {
        project: "./tsconfig.json",
        tsconfigRootDir: import.meta.dirname,
      },
    },

    rules: {
      // ========================================
      // TypeScript-Specific Rules
      // ========================================

      "@typescript-eslint/explicit-function-return-type": [
        "error",
        {
          allowExpressions: true,
          allowTypedFunctionExpressions: true,
          allowHigherOrderFunctions: true,
        },
      ],

      "@typescript-eslint/no-explicit-any": "error",
      "@typescript-eslint/no-unused-vars": [
        "error",
        {
          argsIgnorePattern: "^_",
          varsIgnorePattern: "^_",
          caughtErrorsIgnorePattern: "^_",
        },
      ],

      "@typescript-eslint/no-floating-promises": "error",
      "@typescript-eslint/no-misused-promises": "error",
      "@typescript-eslint/await-thenable": "error",
      "@typescript-eslint/no-unnecessary-type-assertion": "error",
      "@typescript-eslint/prefer-nullish-coalescing": "error",
      "@typescript-eslint/prefer-optional-chain": "error",
      "@typescript-eslint/strict-boolean-expressions": "error",

      // ========================================
      // Naming Conventions
      // ========================================

      "@typescript-eslint/naming-convention": [
        "error",
        {
          selector: "default",
          format: ["camelCase"],
        },
        {
          selector: "variable",
          format: ["camelCase", "UPPER_CASE", "PascalCase"],
          leadingUnderscore: "allow",
        },
        {
          selector: "parameter",
          format: ["camelCase"],
          leadingUnderscore: "allow",
        },
        {
          selector: "typeLike",
          format: ["PascalCase"],
        },
        {
          selector: "enumMember",
          format: ["UPPER_CASE"],
        },
        {
          selector: "property",
          format: null, // Allow any format for properties
        },
      ],

      // ========================================
      // Best Practices
      // ========================================

      "no-console": ["warn", { allow: ["warn", "error"] }],
      "no-debugger": "error",
      "no-alert": "error",
      eqeqeq: ["error", "always"],
      curly: ["error", "all"],
      "prefer-const": "error",
      "no-var": "error",
      "object-shorthand": ["error", "always"],
      "prefer-template": "error",
      "prefer-arrow-callback": "error",

      // ========================================
      // Code Quality
      // ========================================

      complexity: ["error", 10], // Cyclomatic complexity limit
      "max-lines-per-function": ["error", { max: 50, skipBlankLines: true }],
      "max-depth": ["error", 3],
      "max-nested-callbacks": ["error", 3],
      "max-params": ["error", 4],
    },
  },

  // ========================================
  // Prettier Integration (Disable conflicting rules)
  // ========================================

  prettier,

  // ========================================
  // Ignore Patterns
  // ========================================

  {
    ignores: ["dist/", "node_modules/", "coverage/", "*.config.js", "*.config.ts"],
  },
];
```

### .eslintignore (if needed for legacy tools)

```
dist/
node_modules/
coverage/
*.config.js
*.config.ts
```

## Jest Configuration

### Complete jest.config.js

```javascript
// jest.config.js
/** @type {import('jest').Config} */
export default {
  // ========================================
  // Basic Configuration
  // ========================================

  preset: "ts-jest",
  testEnvironment: "node",
  roots: ["<rootDir>/src", "<rootDir>/tests"],

  // ========================================
  // TypeScript Support
  // ========================================

  transform: {
    "^.+\\.tsx?$": [
      "ts-jest",
      {
        tsconfig: "tsconfig.test.json",
      },
    ],
  },

  // ========================================
  // Module Resolution
  // ========================================

  moduleNameMapper: {
    "^@/(.*)$": "<rootDir>/src/$1",
    "^@domain/(.*)$": "<rootDir>/src/domain/$1",
    "^@application/(.*)$": "<rootDir>/src/application/$1",
    "^@infrastructure/(.*)$": "<rootDir>/src/infrastructure/$1",
    "^@shared/(.*)$": "<rootDir>/src/shared/$1",
  },

  moduleFileExtensions: ["ts", "tsx", "js", "jsx", "json", "node"],

  // ========================================
  // Test Discovery
  // ========================================

  testMatch: ["**/__tests__/**/*.ts", "**/?(*.)+(spec|test).ts", "**/tests/**/*.test.ts"],

  testPathIgnorePatterns: ["/node_modules/", "/dist/"],

  // ========================================
  // Coverage Configuration
  // ========================================

  collectCoverageFrom: [
    "src/**/*.ts",
    "!src/**/*.d.ts",
    "!src/**/*.spec.ts",
    "!src/**/*.test.ts",
    "!src/**/index.ts", // Barrel exports
  ],

  coverageDirectory: "coverage",
  coverageReporters: ["text", "lcov", "html", "json"],

  coverageThresholds: {
    global: {
      branches: 80,
      functions: 80,
      lines: 80,
      statements: 80,
    },
  },

  // ========================================
  // Test Execution
  // ========================================

  verbose: true,
  clearMocks: true,
  resetMocks: true,
  restoreMocks: true,

  // ========================================
  // Setup Files
  // ========================================

  setupFilesAfterEnv: ["<rootDir>/tests/setup.ts"],

  // ========================================
  // Performance
  // ========================================

  maxWorkers: "50%", // Use 50% of available CPU cores
};
```

### tests/setup.ts

```typescript
// tests/setup.ts
// Global test setup

// Extend Jest matchers if needed
import "@testing-library/jest-dom"; // If using React Testing Library

// Set test timeout
jest.setTimeout(10000); // 10 seconds

// Mock console methods to reduce noise
global.console = {
  ...console,
  log: jest.fn(), // Mock console.log
  debug: jest.fn(), // Mock console.debug
  info: jest.fn(), // Mock console.info
  // Keep error and warn for debugging
};

// Clean up after each test
afterEach(() => {
  jest.clearAllMocks();
});
```

## Vitest Configuration

### Complete vitest.config.ts

```typescript
// vitest.config.ts
import { defineConfig } from "vitest/config";
import path from "path";

export default defineConfig({
  test: {
    // ========================================
    // Test Environment
    // ========================================

    environment: "node",
    globals: true, // Use global test APIs (describe, it, expect)

    // ========================================
    // Coverage Configuration
    // ========================================

    coverage: {
      provider: "v8", // Fast coverage with V8
      reporter: ["text", "lcov", "html", "json"],
      include: ["src/**/*.ts"],
      exclude: ["src/**/*.d.ts", "src/**/*.spec.ts", "src/**/*.test.ts", "src/**/index.ts", "node_modules", "dist"],
      thresholds: {
        lines: 80,
        functions: 80,
        branches: 80,
        statements: 80,
      },
    },

    // ========================================
    // Test Files
    // ========================================

    include: ["**/__tests__/**/*.ts", "**/?(*.)+(spec|test).ts", "tests/**/*.test.ts"],

    exclude: ["node_modules", "dist"],

    // ========================================
    // Setup Files
    // ========================================

    setupFiles: ["./tests/setup.ts"],

    // ========================================
    // Performance
    // ========================================

    pool: "threads", // Use worker threads
    poolOptions: {
      threads: {
        singleThread: false,
        maxThreads: 4,
        minThreads: 1,
      },
    },

    // ========================================
    // Test Timeout
    // ========================================

    testTimeout: 10000, // 10 seconds
  },

  // ========================================
  // Path Aliases (Match tsconfig.json)
  // ========================================

  resolve: {
    alias: {
      "@": path.resolve(__dirname, "./src"),
      "@domain": path.resolve(__dirname, "./src/domain"),
      "@application": path.resolve(__dirname, "./src/application"),
      "@infrastructure": path.resolve(__dirname, "./src/infrastructure"),
      "@shared": path.resolve(__dirname, "./src/shared"),
    },
  },
});
```

## Package.json Scripts

### Complete package.json

```json
{
  "name": "donation-service",
  "version": "1.0.0",
  "description": "Islamic finance donation service",
  "main": "dist/index.js",
  "types": "dist/index.d.ts",
  "type": "module",
  "engines": {
    "node": ">=18.0.0",
    "npm": ">=9.0.0"
  },
  "scripts": {
    "// ========================================": "",
    "// Build Scripts": "",
    "// ========================================": "",

    "build": "tsc --project tsconfig.build.json",
    "build:watch": "tsc --project tsconfig.build.json --watch",
    "clean": "rm -rf dist coverage .tsbuildinfo",
    "prebuild": "npm run clean",

    "// ========================================": "",
    "// Development Scripts": "",
    "// ========================================": "",

    "dev": "tsx watch src/index.ts",
    "start": "node dist/index.js",
    "start:dev": "tsx src/index.ts",

    "// ========================================": "",
    "// Test Scripts": "",
    "// ========================================": "",

    "test": "jest",
    "test:watch": "jest --watch",
    "test:coverage": "jest --coverage",
    "test:ci": "jest --ci --coverage --maxWorkers=2",

    "// Alternative: Vitest": "",
    "test:vitest": "vitest run",
    "test:vitest:watch": "vitest",
    "test:vitest:coverage": "vitest run --coverage",

    "// ========================================": "",
    "// Linting and Formatting Scripts": "",
    "// ========================================": "",

    "lint": "eslint .",
    "lint:fix": "eslint . --fix",
    "format": "prettier --write \"src/**/*.ts\" \"tests/**/*.ts\"",
    "format:check": "prettier --check \"src/**/*.ts\" \"tests/**/*.ts\"",

    "// ========================================": "",
    "// Type Checking": "",
    "// ========================================": "",

    "type-check": "tsc --noEmit",
    "type-check:watch": "tsc --noEmit --watch",

    "// ========================================": "",
    "// Quality Checks": "",
    "// ========================================": "",

    "check": "npm run type-check && npm run lint && npm run format:check",
    "validate": "npm run check && npm run test",

    "// ========================================": "",
    "// Pre-commit Hooks": "",
    "// ========================================": "",

    "prepare": "husky install",
    "pre-commit": "lint-staged",
    "pre-push": "npm run validate"
  },
  "dependencies": {
    "tslib": "^2.6.2"
  },
  "devDependencies": {
    "@eslint/js": "^9.15.0",
    "@types/jest": "^29.5.11",
    "@types/node": "^20.10.6",
    "eslint": "^9.15.0",
    "eslint-config-prettier": "^9.1.0",
    "husky": "^8.0.3",
    "jest": "^29.7.0",
    "lint-staged": "^15.2.0",
    "prettier": "^3.1.1",
    "ts-jest": "^29.1.1",
    "tsx": "^4.7.0",
    "typescript": "^5.7.2",
    "typescript-eslint": "^8.16.0",
    "vitest": "^1.1.0"
  },
  "lint-staged": {
    "*.ts": ["eslint --fix", "prettier --write", "jest --bail --findRelatedTests --passWithNoTests"]
  }
}
```

## Prettier Configuration

### .prettierrc.json

```json
{
  "semi": true,
  "trailingComma": "es5",
  "singleQuote": false,
  "printWidth": 100,
  "tabWidth": 2,
  "useTabs": false,
  "arrowParens": "always",
  "endOfLine": "lf",
  "bracketSpacing": true,
  "bracketSameLine": false
}
```

### .prettierignore

```
dist/
node_modules/
coverage/
*.config.js
*.config.ts
pnpm-lock.yaml
package-lock.json
```

## GitHub Actions CI/CD

### Complete .github/workflows/ci.yml

```yaml
name: CI/CD Pipeline

on:
  push:
    branches: [main, develop]
  pull_request:
    branches: [main, develop]

jobs:
  # ========================================
  # Quality Checks
  # ========================================

  lint:
    name: Lint Code
    runs-on: ubuntu-latest

    steps:
      - name: Checkout code
        uses: actions/checkout@v4

      - name: Setup Node.js
        uses: actions/setup-node@v4
        with:
          node-version: "18"
          cache: "npm"

      - name: Install dependencies
        run: npm ci

      - name: Run ESLint
        run: npm run lint

      - name: Check formatting
        run: npm run format:check

  type-check:
    name: Type Check
    runs-on: ubuntu-latest

    steps:
      - name: Checkout code
        uses: actions/checkout@v4

      - name: Setup Node.js
        uses: actions/setup-node@v4
        with:
          node-version: "18"
          cache: "npm"

      - name: Install dependencies
        run: npm ci

      - name: Run type check
        run: npm run type-check

  # ========================================
  # Test
  # ========================================

  test:
    name: Test (Node ${{ matrix.node-version }})
    runs-on: ubuntu-latest

    strategy:
      matrix:
        node-version: [18, 20]

    steps:
      - name: Checkout code
        uses: actions/checkout@v4

      - name: Setup Node.js ${{ matrix.node-version }}
        uses: actions/setup-node@v4
        with:
          node-version: ${{ matrix.node-version }}
          cache: "npm"

      - name: Install dependencies
        run: npm ci

      - name: Run tests with coverage
        run: npm run test:ci

      - name: Upload coverage to Codecov
        uses: codecov/codecov-action@v3
        if: matrix.node-version == 18
        with:
          files: ./coverage/lcov.info
          flags: unittests
          name: codecov-umbrella

  # ========================================
  # Build
  # ========================================

  build:
    name: Build
    runs-on: ubuntu-latest
    needs: [lint, type-check, test]

    steps:
      - name: Checkout code
        uses: actions/checkout@v4

      - name: Setup Node.js
        uses: actions/setup-node@v4
        with:
          node-version: "18"
          cache: "npm"

      - name: Install dependencies
        run: npm ci

      - name: Build project
        run: npm run build

      - name: Upload build artifacts
        uses: actions/upload-artifact@v3
        with:
          name: dist
          path: dist/

  # ========================================
  # Docker Build (Optional)
  # ========================================

  docker:
    name: Docker Build
    runs-on: ubuntu-latest
    needs: build
    if: github.ref == 'refs/heads/main'

    steps:
      - name: Checkout code
        uses: actions/checkout@v4

      - name: Set up Docker Buildx
        uses: docker/setup-buildx-action@v3

      - name: Login to GitHub Container Registry
        uses: docker/login-action@v3
        with:
          registry: ghcr.io
          username: ${{ github.actor }}
          password: ${{ secrets.GITHUB_TOKEN }}

      - name: Build and push Docker image
        uses: docker/build-push-action@v5
        with:
          context: .
          push: true
          tags: |
            ghcr.io/${{ github.repository }}:latest
            ghcr.io/${{ github.repository }}:${{ github.sha }}
          cache-from: type=gha
          cache-to: type=gha,mode=max
```

## Docker Multi-Stage Build

### Complete Dockerfile

```dockerfile
# ========================================
# Stage 1: Dependencies
# ========================================

FROM node:18-alpine AS deps

WORKDIR /app

# Copy package files
COPY package*.json ./

# Install production dependencies only
RUN npm ci --only=production && npm cache clean --force

# ========================================
# Stage 2: Build
# ========================================

FROM node:18-alpine AS builder

WORKDIR /app

# Copy package files
COPY package*.json ./

# Install all dependencies (including devDependencies)
RUN npm ci

# Copy source code
COPY tsconfig.json tsconfig.build.json ./
COPY src ./src

# Build TypeScript
RUN npm run build

# ========================================
# Stage 3: Runtime
# ========================================

FROM node:18-alpine AS runtime

# Set NODE_ENV
ENV NODE_ENV=production

# Create app user
RUN addgroup -g 1001 -S nodejs && \
    adduser -S nodejs -u 1001

WORKDIR /app

# Copy production dependencies from deps stage
COPY --from=deps --chown=nodejs:nodejs /app/node_modules ./node_modules

# Copy built application from builder stage
COPY --from=builder --chown=nodejs:nodejs /app/dist ./dist
COPY --from=builder --chown=nodejs:nodejs /app/package.json ./

# Switch to non-root user
USER nodejs

# Expose port
EXPOSE 3000

# Health check
HEALTHCHECK --interval=30s --timeout=3s --start-period=5s --retries=3 \
  CMD node -e "require('http').get('http://localhost:3000/health', (r) => {process.exit(r.statusCode === 200 ? 0 : 1)})"

# Start application
CMD ["node", "dist/index.js"]
```

### .dockerignore

```
node_modules/
dist/
coverage/
.git/
.github/
*.md
.eslintrc.js
.prettierrc.json
tsconfig.json
jest.config.js
vitest.config.ts
tests/
*.log
.env
.env.local
```

## Complete Example Project

### Directory Structure

```
donation-service/
├── src/
│   ├── domain/
│   │   ├── aggregates/
│   │   │   └── Donation.ts
│   │   ├── value-objects/
│   │   │   ├── Money.ts
│   │   │   └── DonationID.ts
│   │   └── repositories/
│   │       └── DonationRepository.ts
│   ├── application/
│   │   └── services/
│   │       └── ProcessDonationService.ts
│   ├── infrastructure/
│   │   ├── persistence/
│   │   │   └── TypeORMDonationRepository.ts
│   │   └── http/
│   │       └── DonationController.ts
│   ├── shared/
│   │   └── result.ts
│   └── index.ts
├── tests/
│   ├── unit/
│   │   └── domain/
│   │       └── Donation.test.ts
│   ├── integration/
│   │   └── repositories/
│   │       └── DonationRepository.integration.test.ts
│   └── setup.ts
├── .github/
│   └── workflows/
│       └── ci.yml
├── tsconfig.json
├── tsconfig.build.json
├── tsconfig.test.json
├── eslint.config.js
├── jest.config.js
├── vitest.config.ts
├── .prettierrc.json
├── .prettierignore
├── .gitignore
├── .dockerignore
├── Dockerfile
├── package.json
└── README.md
```

### package.json (Complete Example)

```json
{
  "name": "donation-service",
  "version": "1.0.0",
  "description": "Islamic finance donation processing service",
  "author": "Your Name",
  "license": "MIT",
  "main": "dist/index.js",
  "types": "dist/index.d.ts",
  "type": "module",
  "engines": {
    "node": ">=18.0.0",
    "npm": ">=9.0.0"
  },
  "scripts": {
    "build": "tsc --project tsconfig.build.json",
    "build:watch": "tsc --project tsconfig.build.json --watch",
    "clean": "rm -rf dist coverage .tsbuildinfo",
    "prebuild": "npm run clean",

    "dev": "tsx watch src/index.ts",
    "start": "node dist/index.js",
    "start:dev": "tsx src/index.ts",

    "test": "jest",
    "test:watch": "jest --watch",
    "test:coverage": "jest --coverage",
    "test:ci": "jest --ci --coverage --maxWorkers=2",

    "lint": "eslint .",
    "lint:fix": "eslint . --fix",
    "format": "prettier --write \"src/**/*.ts\" \"tests/**/*.ts\"",
    "format:check": "prettier --check \"src/**/*.ts\" \"tests/**/*.ts\"",

    "type-check": "tsc --noEmit",
    "type-check:watch": "tsc --noEmit --watch",

    "check": "npm run type-check && npm run lint && npm run format:check",
    "validate": "npm run check && npm run test",

    "prepare": "husky install"
  },
  "dependencies": {
    "tslib": "^2.6.2",
    "typeorm": "^0.3.19",
    "reflect-metadata": "^0.2.1"
  },
  "devDependencies": {
    "@eslint/js": "^9.15.0",
    "@types/jest": "^29.5.11",
    "@types/node": "^20.10.6",
    "eslint": "^9.15.0",
    "eslint-config-prettier": "^9.1.0",
    "husky": "^8.0.3",
    "jest": "^29.7.0",
    "lint-staged": "^15.2.0",
    "prettier": "^3.1.1",
    "ts-jest": "^29.1.1",
    "tsx": "^4.7.0",
    "typescript": "^5.7.2",
    "typescript-eslint": "^8.16.0"
  },
  "lint-staged": {
    "*.ts": ["eslint --fix", "prettier --write", "jest --bail --findRelatedTests --passWithNoTests"]
  }
}
```

### .gitignore

```
# Dependencies
node_modules/

# Build output
dist/
*.tsbuildinfo

# Coverage
coverage/

# Environment variables
.env
.env.local
.env.*.local

# IDE
.vscode/
.idea/
*.swp
*.swo
*~

# OS
.DS_Store
Thumbs.db

# Logs
*.log
npm-debug.log*
yarn-debug.log*
yarn-error.log*
lerna-debug.log*

# Testing
.nyc_output/

# Misc
*.pid
*.seed
*.pid.lock
```

## Usage Guidelines

### 1. Start with Strict Mode

Always enable strict mode in tsconfig.json:

```json
{
  "compilerOptions": {
    "strict": true,
    "noImplicitAny": true,
    "strictNullChecks": true
  }
}
```

### 2. Use Path Aliases

Configure path aliases for cleaner imports:

```json
// tsconfig.json
{
  "compilerOptions": {
    "baseUrl": "./",
    "paths": {
      "@/*": ["src/*"],
      "@domain/*": ["src/domain/*"]
    }
  }
}

// jest.config.js or vitest.config.ts
{
  "moduleNameMapper": {
    "^@/(.*)$": "<rootDir>/src/$1"
  }
}
```

Usage:

```typescript
// ✅ Good: Clean imports with path aliases
import { Donation } from "@domain/aggregates/Donation";
import { Result } from "@shared/result";

// ❌ Bad: Relative paths
import { Donation } from "../../../domain/aggregates/Donation";
import { Result } from "../../shared/result";
```

### 3. Separate Build Configurations

Use different tsconfig files for different purposes:

- `tsconfig.json` - Base configuration
- `tsconfig.build.json` - Production builds (no tests)
- `tsconfig.test.json` - Test configuration (includes test files)

### 4. Automate Quality Checks

Use Husky + lint-staged for pre-commit hooks:

```bash
npm install --save-dev husky lint-staged
npx husky install
npx husky add .husky/pre-commit "npx lint-staged"
```

### 5. Coverage Thresholds

Set realistic coverage thresholds:

```javascript
// jest.config.js
{
  coverageThresholds: {
    global: {
      branches: 80,
      functions: 80,
      lines: 80,
      statements: 80,
    },
  },
}
```

### 6. CI/CD Best Practices

- **Matrix builds**: Test on multiple Node versions
- **Cache dependencies**: Use `actions/cache` or built-in caching
- **Parallel jobs**: Run lint, test, build in parallel
- **Artifacts**: Upload build artifacts for deployment

### 7. Docker Best Practices

- **Multi-stage builds**: Separate deps, build, runtime
- **Non-root user**: Run as nodejs user for security
- **Health checks**: Include health check endpoint
- **Minimal image**: Use Alpine variants

## Related Documentation

- [TypeScript Modules and Dependencies](../ex-so-stla-ts__modules-and-dependencies.md) - Module system
- [TypeScript Best Practices](../ex-so-stla-ts__best-practices.md) - Coding standards
- [TypeScript Linting and Formatting](../ex-so-stla-ts__linting-and-formatting.md) - ESLint/Prettier setup
- [Repository Template](./repository-template.md) - Repository pattern
- [Service Layer Template](./service-layer-template.md) - Application services

---

**Last Updated**: 2026-01-24
**TypeScript Version**: 5.7+
**ESLint Version**: 9.x
**Jest Version**: 29.x
**Node.js Version**: 18+
