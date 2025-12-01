# Apps Folder

## Purpose

The `apps/` directory contains **deployable application projects** (executables). These are the final artifacts that can be run, deployed, and served to end users.

## Naming Convention

Apps follow the naming pattern: **`[domain]-[type]`**

### Examples

- `api-gateway` - API gateway service
- `admin-dashboard` - Admin web application
- `customer-portal` - Customer-facing portal
- `payment-processor` - Payment processing service
- `demo-ts-fe` - Demo Next.js frontend app (TypeScript)

## Application Characteristics

- **Consumers** - Apps import and use libs, but don't export anything for reuse
- **Isolated** - Apps should NOT import from other apps
- **Deployable** - Each app is independently deployable
- **Specific** - Contains app-specific logic and configuration
- **Entry Points** - Has clear entry points (index.ts, main.ts, etc.)

## Required Files (TypeScript/Next.js Apps)

Each TypeScript application requires:

```
apps/[app-name]/
├── app/                     # Next.js app directory (Next.js 14+)
│   ├── page.tsx             # Home page
│   ├── layout.tsx           # Root layout
│   └── ...
├── public/                  # Static assets (if applicable)
├── next.config.js           # Next.js configuration
├── package.json             # App-specific dependencies
├── project.json             # Nx project configuration
├── tsconfig.json            # TypeScript configuration (extends workspace)
└── README.md                # App documentation
```

For other app types (Node.js backend, etc.):

```
apps/[app-name]/
├── src/                     # Application source code
│   ├── index.ts             # Entry point
│   ├── [feature]/           # Feature modules
│   └── __tests__/           # Test files
├── dist/                    # Build output (gitignored)
├── package.json             # App-specific dependencies (if any)
├── project.json             # Nx project configuration
├── tsconfig.json            # TypeScript configuration
├── tsconfig.build.json      # Build-specific TS config
└── README.md                # App documentation
```

## Nx Configuration (project.json)

Each app must have a `project.json` file with Nx configuration:

```json
{
  "name": "app-name",
  "sourceRoot": "apps/app-name",
  "projectType": "application",
  "targets": {
    "build": {
      "executor": "nx:run-commands",
      "options": {
        "command": "your build command",
        "cwd": "apps/app-name"
      },
      "outputs": ["{projectRoot}/dist"]
    },
    "serve": {
      "executor": "nx:run-commands",
      "options": {
        "command": "your serve command",
        "cwd": "apps/app-name"
      }
    }
  }
}
```

**Note**: This repository uses vanilla Nx (no plugins), so all executors use `nx:run-commands` to run standard build tools directly.

## How to Add a New App

See the how-to guide: `docs/how-to/hoto__add-new-app.md` (to be created)

## Importing from Libraries

Apps can import from any library in `libs/` using path mappings:

```typescript
import { greet } from "@open-sharia-enterprise/ts-demo-libs";
import { utils } from "@open-sharia-enterprise/ts-utils";
```

Path mappings are configured in the workspace `tsconfig.base.json` file.

## Running Apps

Use Nx commands to run apps:

```bash
# Development mode
nx dev [app-name]

# Build for production
nx build [app-name]

# Serve production build
nx serve [app-name]
```

## Language Support

Currently: **TypeScript/Next.js** apps

Future: Java, Kotlin, Python apps (each language will have language-specific structure and tooling)
