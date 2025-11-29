---
title: Monorepo Structure Reference
type: reference
category: monorepo
tags:
  - nx
  - monorepo
  - architecture
  - structure
date: 2025-11-29
---

# Monorepo Structure Reference

Complete reference for the Nx monorepo structure, folder organization, and file formats.

## Overview

This project uses **Nx** as a monorepo build system with a plugin-free "vanilla Nx" approach. The monorepo is organized into two main folders:

- `apps/` - Deployable applications
- `libs/` - Reusable libraries (flat structure with language prefixes)

## Root Structure

```
open-sharia-enterprise/
├── apps/                      # Deployable applications
├── libs/                      # Reusable libraries (flat structure)
├── docs/                      # Documentation (Diátaxis framework)
├── plans/                     # Project planning documents
├── .claude/                   # Claude Code configuration
├── .husky/                    # Git hooks
├── .nx/                       # Nx cache (gitignored)
├── node_modules/              # Dependencies (gitignored)
├── nx.json                    # Nx workspace configuration
├── tsconfig.base.json         # Base TypeScript configuration
├── package.json               # Workspace manifest with npm workspaces
├── package-lock.json          # Dependency lock file
├── .nxignore                  # Files to exclude from Nx processing
├── .gitignore                 # Git ignore rules
├── commitlint.config.js       # Commit message validation
├── CLAUDE.md                  # Claude Code guidance
└── README.md                  # Project README
```

## Apps Folder (`apps/`)

### Purpose

Contains deployable application projects (executables).

### Location

`apps/` at repository root

### Organization

Flat structure - all apps at the same level, no subdirectories.

### Naming Convention

`[domain]-[type]`

**Examples**:

- `api-gateway` - API gateway service
- `admin-dashboard` - Admin web application
- `customer-portal` - Customer-facing portal
- `payment-processor` - Payment processing service
- `demo-ts-fe` - Demo Next.js frontend (temporary)

### App Structure (Next.js Example)

```
apps/demo-ts-fe/
├── app/                       # Next.js 14+ app directory
│   ├── layout.tsx             # Root layout
│   ├── page.tsx               # Home page
│   └── globals.css            # Global styles
├── public/                    # Static assets
│   └── favicon.ico
├── .next/                     # Build output (gitignored)
├── node_modules/              # App dependencies (hoisted to root)
├── project.json               # Nx project configuration
├── tsconfig.json              # TypeScript configuration
├── next.config.ts             # Next.js configuration
├── tailwind.config.ts         # Tailwind CSS configuration
├── postcss.config.mjs         # PostCSS configuration
├── .eslintrc.json             # ESLint configuration
├── package.json               # App-specific dependencies
├── .gitignore                 # App-specific git ignores
└── README.md                  # App documentation
```

### App Structure (Express API Example)

```
apps/api-gateway/
├── src/                       # Application source code
│   ├── index.ts               # Entry point
│   ├── routes/                # API routes
│   ├── controllers/           # Request handlers
│   ├── middleware/            # Express middleware
│   └── __tests__/             # Tests
├── dist/                      # Build output (gitignored)
├── project.json               # Nx project configuration
├── tsconfig.json              # TypeScript configuration
├── tsconfig.build.json        # Build-specific TS config
├── package.json               # App-specific dependencies
└── README.md                  # App documentation
```

### App Characteristics

- **Consumers** - Apps import and use libs, don't export for reuse
- **Isolated** - Apps should NOT import from other apps
- **Deployable** - Each app is independently deployable
- **Specific** - Contains app-specific logic and configuration
- **Entry Points** - Has clear entry point (index.ts, main.ts, etc.)

## Libs Folder (`libs/`)

### Purpose

Contains reusable library packages.

### Location

`libs/` at repository root

### Organization

**Flat structure** - All libraries at the same level, no nested scopes.

### Naming Convention

`[language-prefix]-[name]`

**Language Prefixes**:

- `ts-` - TypeScript (current implementation)
- `java-` - Java (future)
- `kt-` - Kotlin (future)
- `py-` - Python (future)

**Examples**:

- `ts-utils` - TypeScript utility functions
- `ts-components` - Reusable React components
- `ts-hooks` - Custom React hooks
- `ts-api` - API client libraries
- `ts-validators` - Data validation functions
- `ts-demo-libs` - Demo TypeScript library (temporary)

### Library Structure (TypeScript)

```
libs/ts-demo-libs/
├── src/
│   ├── index.ts               # Public API (barrel export)
│   └── lib/                   # Implementation
│       ├── greet.ts           # Feature implementation
│       └── greet.test.ts      # Unit tests
├── dist/                      # Build output (gitignored)
│   ├── index.js               # Compiled JavaScript
│   ├── index.d.ts             # Type definitions
│   └── lib/                   # Compiled lib files
├── project.json               # Nx project configuration
├── tsconfig.json              # TypeScript configuration
├── tsconfig.build.json        # Build-specific TS config
├── package.json               # Library metadata and dependencies
└── README.md                  # Library documentation
```

### Library Characteristics

- **Polyglot-Ready** - Designed for multiple languages (TypeScript now, Java/Kotlin/Python future)
- **Flat Structure** - All libs at same level, no nested scopes
- **Reusable** - Designed to be imported by apps and other libs
- **Focused** - Each lib has single, clear purpose
- **Public API** - Exports controlled through `index.ts` (barrel export)
- **Testable** - Can be tested independently

### Current Scope

TypeScript libraries only. Future languages (Java, Kotlin, Python) not yet implemented.

## File Format Reference

### `project.json` (Nx Configuration)

Location: `apps/[app-name]/project.json` or `libs/[lib-name]/project.json`

**Next.js App Example**:

```json
{
  "name": "demo-ts-fe",
  "sourceRoot": "apps/demo-ts-fe",
  "projectType": "application",
  "targets": {
    "dev": {
      "executor": "nx:run-commands",
      "options": {
        "command": "next dev",
        "cwd": "apps/demo-ts-fe"
      }
    },
    "build": {
      "executor": "nx:run-commands",
      "options": {
        "command": "next build",
        "cwd": "apps/demo-ts-fe"
      },
      "outputs": ["{projectRoot}/.next"]
    },
    "serve": {
      "executor": "nx:run-commands",
      "options": {
        "command": "next start",
        "cwd": "apps/demo-ts-fe"
      },
      "dependsOn": ["build"]
    }
  }
}
```

**TypeScript Library Example**:

```json
{
  "name": "ts-demo-libs",
  "sourceRoot": "libs/ts-demo-libs/src",
  "projectType": "library",
  "targets": {
    "build": {
      "executor": "nx:run-commands",
      "options": {
        "command": "tsc -p libs/ts-demo-libs/tsconfig.build.json",
        "cwd": "."
      },
      "outputs": ["{projectRoot}/dist"]
    },
    "test": {
      "executor": "nx:run-commands",
      "options": {
        "command": "node --import tsx --test libs/ts-demo-libs/src/**/*.test.ts",
        "cwd": "."
      },
      "dependsOn": ["build"]
    }
  }
}
```

**Fields**:

- `name` - Project name (used by Nx CLI)
- `sourceRoot` - Source code location
- `projectType` - `"application"` or `"library"`
- `targets` - Nx tasks (build, test, lint, etc.)
- `executor` - Always `"nx:run-commands"` (no plugins)
- `command` - Shell command to execute
- `cwd` - Working directory for command
- `outputs` - Cache output locations
- `dependsOn` - Task dependencies

### `tsconfig.json` (TypeScript Configuration)

**App Example**:

```json
{
  "extends": "../../tsconfig.base.json",
  "compilerOptions": {
    "jsx": "preserve",
    "allowJs": true,
    "esModuleInterop": true,
    "allowSyntheticDefaultImports": true,
    "strict": true,
    "forceConsistentCasingInFileNames": true,
    "noEmit": true,
    "incremental": true,
    "module": "esnext",
    "moduleResolution": "bundler",
    "resolveJsonModule": true,
    "isolatedModules": true,
    "plugins": [
      {
        "name": "next"
      }
    ]
  },
  "include": ["**/*", ".next/types/**/*.ts"],
  "exclude": ["node_modules"]
}
```

**Library Example**:

```json
{
  "extends": "../../tsconfig.base.json",
  "compilerOptions": {
    "module": "ESNext",
    "moduleResolution": "node",
    "declaration": true
  },
  "include": ["src/**/*"],
  "exclude": ["node_modules", "dist", "**/*.test.ts"]
}
```

**Key Points**:

- Always extends `../../tsconfig.base.json`
- Workspace path mappings inherited from base config
- Project-specific options only

### `package.json`

**App Example**:

```json
{
  "name": "@open-sharia-enterprise/demo-ts-fe",
  "version": "0.1.0",
  "private": true,
  "scripts": {
    "dev": "next dev",
    "build": "next build",
    "start": "next start",
    "lint": "next lint"
  },
  "dependencies": {
    "next": "15.5.6",
    "react": "19.0.0",
    "react-dom": "19.0.0"
  },
  "devDependencies": {
    "@types/node": "^22",
    "@types/react": "^19",
    "@types/react-dom": "^19",
    "typescript": "^5"
  }
}
```

**Library Example**:

```json
{
  "name": "@open-sharia-enterprise/ts-demo-libs",
  "version": "0.1.0",
  "private": true,
  "main": "./dist/index.js",
  "types": "./dist/index.d.ts",
  "devDependencies": {
    "tsx": "^4.0.0"
  }
}
```

**Naming**:

- Scope: `@open-sharia-enterprise`
- Apps: `@open-sharia-enterprise/[app-name]`
- Libs: `@open-sharia-enterprise/[lib-name]`

## Dependency Rules

### Import Patterns

**Apps importing libs**:

```typescript
// In apps/demo-ts-fe/app/page.tsx
import { greet } from "@open-sharia-enterprise/ts-demo-libs";
```

**Libs importing other libs**:

```typescript
// In libs/ts-components/src/index.ts
import { formatDate } from "@open-sharia-enterprise/ts-utils";
```

### Rules

1. **Apps can import from any lib**
2. **Libs can import from other libs**
3. **No circular dependencies** (A → B → A is prohibited)
4. **Apps should NOT import from other apps**
5. **Language boundaries exist** (TypeScript libs can't directly import Go/Python/Rust libs)

### Monitoring Dependencies

```bash
# View full dependency graph
nx graph

# View specific project dependencies
nx graph --focus=demo-ts-fe

# View affected projects
nx affected:graph
```

## Path Mappings

Configured in `tsconfig.base.json`:

```json
{
  "compilerOptions": {
    "baseUrl": ".",
    "paths": {
      "@open-sharia-enterprise/ts-*": ["libs/ts-*/src/index.ts"]
    }
  }
}
```

**Pattern**: `@open-sharia-enterprise/[language-prefix]-[name]`

**Examples**:

- `@open-sharia-enterprise/ts-utils`
- `@open-sharia-enterprise/ts-components`
- `@open-sharia-enterprise/ts-hooks`

## Build Outputs

### Apps

- **Next.js**: `apps/[app-name]/.next/`
- **Express**: `apps/[app-name]/dist/`

### Libraries

- **TypeScript**: `libs/ts-[name]/dist/`

All build outputs are gitignored.

## Related Documentation

- [How to Add New App](../how-to/ht__add-new-app.md)
- [How to Add New Library](../how-to/ht__add-new-lib.md)
- [How to Run Nx Commands](../how-to/ht__run-nx-commands.md)
- [Nx Configuration Reference](./re__nx-configuration.md)
