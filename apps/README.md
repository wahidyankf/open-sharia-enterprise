# Apps Folder

## Purpose

The `apps/` directory contains **deployable application projects** (executables). These are the final artifacts that can be run, deployed, and served to end users.

## Naming Convention

Apps follow the naming pattern: **`[domain]-[type]`**

### Current Apps

- `ose-platform-web` - OSE Platform website ([oseplatform.com](https://oseplatform.com)) - Hugo static site
- `ayokoding-web` - AyoKoding educational platform ([ayokoding.com](https://ayokoding.com)) - Hugo static site
- `ayokoding-cli` - AyoKoding CLI tool for navigation generation - Go application
- `rhino-cli` - Repository management CLI tools - Go application
- `organiclever_app` - OrganicLever mobile and web client - Flutter application (port 3100)
- `organiclever-be` - OrganicLever backend API - Spring Boot application (port 8100)
- `organiclever-be-e2e` - E2E tests for organiclever-be REST API - Playwright (API testing)

## Application Characteristics

- **Consumers** - Apps import and use libs, but don't export anything for reuse
- **Isolated** - Apps should NOT import from other apps
- **Deployable** - Each app is independently deployable
- **Specific** - Contains app-specific logic and configuration
- **Entry Points** - Has clear entry points (index.ts, main.ts, etc.)

## App Structure Examples

### Hugo Static Site (Current)

```
apps/ose-platform-web/
├── content/                 # Markdown content files
├── layouts/                 # Hugo templates
├── static/                  # Static assets (images, CSS, JS)
├── themes/                  # Hugo themes
├── public/                  # Build output (gitignored)
├── hugo.yaml                # Hugo configuration
├── project.json             # Nx project configuration
├── build.sh                 # Build script
├── vercel.json              # Deployment configuration
└── README.md                # App documentation
```

### Go CLI Application (Current)

```
apps/ayokoding-cli/
├── cmd/                     # CLI commands
├── internal/                # Internal packages
├── dist/                    # Build output (gitignored)
├── main.go                  # Entry point
├── go.mod                   # Go module definition
├── project.json             # Nx project configuration
└── README.md                # App documentation
```

```
apps/rhino-cli/
├── cmd/                     # CLI commands
├── internal/                # Internal packages
├── dist/                    # Build output (gitignored)
├── main.go                  # Entry point
├── go.mod                   # Go module definition
├── project.json             # Nx project configuration
└── README.md                # App documentation
```

### Flutter Application (Current)

```
apps/organiclever_app/
├── lib/                     # Dart source code
│   ├── config/              # Environment configuration
│   ├── models/              # Data models
│   ├── providers/           # State management
│   ├── services/            # API clients
│   ├── screens/             # UI screens
│   └── main.dart            # Entry point
├── test/                    # Test files
│   ├── unit/                # Unit tests
│   └── widget/              # Widget tests
├── android/                 # Android platform
├── ios/                     # iOS platform
├── web/                     # Web platform
├── linux/                   # Linux platform
├── pubspec.yaml             # Dependencies
├── project.json             # Nx configuration
└── README.md                # App documentation
```

### Spring Boot Application (Current)

```
apps/organiclever-be/
├── src/main/java/           # Java source code
│   └── com/organiclever/be/
│       ├── OrganicLeverApplication.java
│       ├── config/          # Configuration classes
│       └── controller/      # REST controllers
├── src/main/resources/      # Application config
│   ├── application.yml
│   ├── application-dev.yml
│   └── application-staging.yml
├── src/test/java/           # Test files
├── target/                  # Build output (gitignored)
├── pom.xml                  # Maven configuration
├── project.json             # Nx configuration
└── README.md                # App documentation
```

### Playwright E2E Test App (Current)

```
apps/organiclever-be-e2e/
├── playwright.config.ts         # Playwright configuration (baseURL, reporters)
├── package.json                 # Pinned @playwright/test dependency
├── tsconfig.json                # TypeScript config (extends workspace base)
├── project.json                 # Nx configuration
├── tests/
│   ├── e2e/
│   │   ├── hello/
│   │   │   └── hello.spec.ts    # Tests for GET /api/v1/hello
│   │   └── actuator/
│   │       └── health.spec.ts   # Tests for GET /actuator/health
│   └── utils/
│       └── api-helpers.ts       # Shared request utilities
└── README.md                    # App documentation
```

### Future App Types

TypeScript/Next.js, Kotlin, Python apps will have language-specific structures and tooling.

## Nx Configuration (project.json)

Each app must have a `project.json` file with Nx configuration.

**Hugo App Example** (`ose-platform-web`):

```json
{
  "name": "ose-platform-web",
  "sourceRoot": "apps/ose-platform-web",
  "projectType": "application",
  "targets": {
    "dev": {
      "executor": "nx:run-commands",
      "options": {
        "command": "hugo server --buildDrafts --buildFuture",
        "cwd": "apps/ose-platform-web"
      }
    },
    "build": {
      "executor": "nx:run-commands",
      "options": {
        "command": "bash build.sh",
        "cwd": "apps/ose-platform-web"
      },
      "outputs": ["{projectRoot}/public"]
    },
    "clean": {
      "executor": "nx:run-commands",
      "options": {
        "command": "rm -rf public resources",
        "cwd": "apps/ose-platform-web"
      }
    }
  },
  "tags": ["type:app", "platform:hugo"]
}
```

**Note**: This repository uses vanilla Nx (no plugins), so all executors use `nx:run-commands` to run standard build tools directly (Hugo, Go, etc.).

## How to Add a New App

See the how-to guide: `docs/how-to/hoto__add-new-app.md` (to be created)

## Importing from Libraries

Apps can import from any library in `libs/` using path mappings:

```typescript
// Future TypeScript apps will use path mappings like:
import { utils } from "@open-sharia-enterprise/ts-utils";
import { Button } from "@open-sharia-enterprise/ts-components";
```

Path mappings are configured in the workspace `tsconfig.base.json` file.

**Note**: Currently there are no libraries in `libs/`. Libraries will be created as shared functionality is identified.

## Running Apps

Use Nx commands to run apps:

```bash
# Development mode (Hugo sites)
nx dev ose-platform-web
nx dev ayokoding-web

# Build for production
nx build ose-platform-web
nx build ayokoding-web
nx build ayokoding-cli
nx build rhino-cli

# Run CLI applications
nx run rhino-cli

# Clean build artifacts
nx clean ose-platform-web

# Run E2E tests (backend must be running first)
nx e2e organiclever-be-e2e
```

## Language Support

Currently:

- **Hugo** (static sites) - ose-platform-web, ayokoding-web
- **Go** (CLI tools) - ayokoding-cli, rhino-cli
- **Flutter/Dart** (mobile & web) - organiclever_app
- **Java/Spring Boot** (backend API) - organiclever-be
- **TypeScript/Playwright** (E2E testing) - organiclever-be-e2e

Future: TypeScript/Next.js, Kotlin, Python apps (each language will have language-specific structure and tooling)
