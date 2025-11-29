# Technical Documentation: Nx Monorepo Initialization

## Architecture Overview

This plan establishes a monorepo architecture using **Nx** as the build system and task runner. Unlike typical Nx setups that leverage plugins for code generation and execution, this implementation uses "vanilla Nx" - relying only on the core Nx package without any official or third-party plugins.

### Monorepo Philosophy

The monorepo architecture provides several key benefits for the open-sharia-enterprise project:

- **Code Sharing** - Reusable libraries shared across multiple applications
- **Atomic Changes** - Single commits can update multiple apps and libs simultaneously
- **Unified Tooling** - Consistent build, test, and lint processes across all projects
- **Dependency Management** - Single node_modules, eliminating version conflicts
- **Efficient Builds** - Only rebuild what changed using affected detection
- **Discoverability** - All code in one repository, easy to find and navigate

### Nx Role and Scope

**Nx serves as a build system and task orchestrator**, not as a framework:

- **Task Orchestration** - Runs tasks (build, test, lint) in correct dependency order
- **Computation Caching** - Caches task results to avoid redundant work
- **Affected Detection** - Identifies which projects changed and need rebuilding
- **Dependency Graph** - Visualizes project relationships and dependencies
- **No Code Generation** - Does NOT use generators (manual project creation)
- **No Plugin Executors** - Uses `nx:run-commands` to run standard tools directly

This "vanilla Nx" approach provides full control and transparency while maintaining the power of Nx's task running capabilities.

### High-Level Architecture Diagram

```
┌─────────────────────────────────────────────────────────────────┐
│                    Nx Workspace Architecture                     │
└─────────────────────────────────────────────────────────────────┘

                       open-sharia-enterprise/
                                  │
                 ┌────────────────┼────────────────┐
                 │                │                │
                 ▼                ▼                ▼
            ┌─────────┐      ┌─────────┐    ┌───────────┐
            │  apps/  │      │  libs/  │    │   docs/   │
            └─────────┘      └─────────┘    └───────────┘
                 │                │
         ┌───────┼──────┐    ┌────┼────────────────────────┐
         │       │      │    │    │      │      │     │    │
         ▼       ▼      ▼    ▼    ▼      ▼      ▼     ▼    ▼
      app-1  app-2  app-3  shared/ feature/ data-  ui/  util/
                                           access/

┌─────────────────────────────────────────────────────────────────┐
│  Dependency Flow (Apps can use Libs, Libs can use other Libs)   │
└─────────────────────────────────────────────────────────────────┘

         Apps Layer
         ┌─────────────────────────────────┐
         │  app-1    app-2    app-3        │
         └──────────────┬──────────────────┘
                        │ imports
                        ▼
         Feature Libs Layer
         ┌─────────────────────────────────┐
         │  feature/auth  feature/payments │
         └──────────────┬──────────────────┘
                        │ imports
                        ▼
         Data Access Layer
         ┌─────────────────────────────────┐
         │  data-access/users  data-access/│
         │  transactions                   │
         └──────────────┬──────────────────┘
                        │ imports
                        ▼
         Shared/Util Layer
         ┌─────────────────────────────────┐
         │  shared/utils  util/validators  │
         │  ui/components                  │
         └─────────────────────────────────┘

Rules:
- Apps import from any lib scope
- Libs can import from libs in same or lower layer
- No circular dependencies
- Shared/util libs have minimal dependencies
```

## Technology Stack

### Core Technologies

| Technology     | Version               | Purpose                               |
| -------------- | --------------------- | ------------------------------------- |
| **Nx**         | Latest stable (^19.x) | Monorepo build system and task runner |
| **Node.js**    | 24.11.1 (LTS)         | JavaScript runtime (Volta-managed)    |
| **npm**        | 11.6.2                | Package manager (Volta-managed)       |
| **TypeScript** | ^5.x                  | Type-safe development language        |
| **Volta**      | Current               | Node/npm version management           |

### Explicitly Excluded

- **Nx Plugins** - None (`@nx/react`, `@nx/node`, etc. - explicitly prohibited)
- **Build Bundlers** - None yet (Webpack, Vite - future work)
- **Test Frameworks** - None yet (Jest, Vitest - future work, using Node.js test runner for now)
- **Linters** - Existing setup only (no new ESLint configuration)

## Monorepo Structure

### Apps Folder (`apps/`)

**Purpose**: Contains deployable application projects (executables)

**Location**: `apps/` at repository root

**Naming Convention**: `[domain]-[type]`

Examples:

- `api-gateway` - API gateway service
- `admin-dashboard` - Admin web application
- `customer-portal` - Customer-facing portal
- `payment-processor` - Payment processing service

**Standard Structure**:

```
apps/
└── [app-name]/
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

**Characteristics**:

- **Consumers** - Apps import and use libs, they don't export anything for reuse
- **Isolated** - Apps should NOT import from other apps
- **Deployable** - Each app is independently deployable
- **Specific** - Contains app-specific logic and configuration
- **Entry Points** - Has a clear entry point (index.ts, main.ts, etc.)

### Libs Folder (`libs/`)

**Purpose**: Contains reusable library packages

**Location**: `libs/` at repository root

**Naming Convention**: `[scope]/[name]`

Examples:

- `shared/utils` - Common utility functions
- `feature/auth` - Authentication feature logic
- `data-access/users` - User data access layer
- `ui/components` - Reusable UI components
- `util/validators` - Pure validation functions

**Standard Structure**:

```
libs/
└── [scope]/
    └── [name]/
        ├── src/
        │   ├── index.ts         # Public API (barrel export)
        │   ├── lib/             # Implementation
        │   │   ├── [feature].ts
        │   │   └── [feature].spec.ts
        │   └── __tests__/       # Integration tests
        ├── dist/                # Build output (gitignored)
        ├── package.json         # Lib dependencies (if any)
        ├── project.json         # Nx project configuration
        ├── tsconfig.json        # TypeScript configuration
        ├── tsconfig.build.json  # Build-specific TS config
        └── README.md            # Library documentation
```

**Characteristics**:

- **Reusable** - Libs are designed to be imported by apps and other libs
- **Focused** - Each lib has a single, clear purpose
- **Public API** - Exports controlled through index.ts (barrel exports)
- **Testable** - Can be tested independently
- **Scoped** - Organized by scope for clarity and dependency management

### Library Scopes

Scopes organize libraries by their purpose and role in the architecture:

#### 1. `shared/` - Shared Utilities and Common Code

**Purpose**: Code used across multiple features and domains

**Examples**:

- `shared/utils` - General utility functions
- `shared/constants` - Shared constants and enums
- `shared/types` - Shared TypeScript types
- `shared/config` - Configuration utilities

**Characteristics**:

- Minimal dependencies (mostly self-contained)
- No business logic
- Pure functions when possible
- Widely reusable

**Import Rules**:

- Can import: Other `shared/`, `util/`
- Should NOT import: `feature/`, `data-access/`, `ui/` (except in special cases)

#### 2. `feature/` - Feature-Specific Business Logic

**Purpose**: Encapsulates business logic for specific features

**Examples**:

- `feature/auth` - Authentication logic
- `feature/payments` - Payment processing logic
- `feature/transactions` - Transaction management
- `feature/reporting` - Reporting and analytics

**Characteristics**:

- Contains business rules and workflows
- Orchestrates data access and UI components
- Feature-focused and domain-specific
- Provides high-level APIs for apps

**Import Rules**:

- Can import: `data-access/`, `shared/`, `util/`, `ui/`
- Can import: Other `feature/` (with caution, avoid circular deps)
- Should NOT be imported by: `data-access/`, `shared/`, `util/`

#### 3. `data-access/` - Data Access Layer

**Purpose**: Handles database access, API clients, and data persistence

**Examples**:

- `data-access/users` - User CRUD operations
- `data-access/transactions` - Transaction database access
- `data-access/api-client` - External API client
- `data-access/cache` - Caching layer

**Characteristics**:

- Encapsulates data sources (databases, APIs, files)
- Provides CRUD operations and queries
- Handles data transformation and validation
- Abstracts persistence details

**Import Rules**:

- Can import: `shared/`, `util/`
- Should NOT import: `feature/`, `ui/`
- Can import: Other `data-access/` for composition

#### 4. `ui/` - Reusable UI Components

**Purpose**: Presentational components and UI utilities

**Examples**:

- `ui/components` - Shared React/Vue components
- `ui/styles` - Shared styles and themes
- `ui/icons` - Icon components
- `ui/layouts` - Layout components

**Characteristics**:

- Presentation-focused (no business logic)
- Reusable across apps
- Props-based configuration
- Framework-specific (React, Vue, etc.)

**Import Rules**:

- Can import: `shared/`, `util/`
- Should NOT import: `feature/`, `data-access/`
- Can import: Other `ui/` components

#### 5. `util/` - Pure Utility Functions

**Purpose**: Framework-agnostic, pure utility functions

**Examples**:

- `util/validators` - Validation functions
- `util/formatters` - String/number formatters
- `util/parsers` - Data parsers
- `util/math` - Mathematical utilities

**Characteristics**:

- Pure functions (no side effects)
- Framework-agnostic
- Highly testable
- No dependencies on other libs (except other `util/`)

**Import Rules**:

- Can import: Other `util/` only
- Should NOT import: Any other scope
- Should be dependency-free when possible

### Dependency Rules Summary

```
┌──────────────────────────────────────────────────────────┐
│  Library Dependency Matrix (✓ = allowed, ✗ = prohibited) │
├──────────────────────────────────────────────────────────┤
│             │ shared │ feature │ data-access │ ui │ util │
├─────────────┼────────┼─────────┼─────────────┼────┼──────┤
│ apps        │   ✓    │    ✓    │      ✓      │ ✓  │  ✓   │
│ feature     │   ✓    │    ⚠    │      ✓      │ ✓  │  ✓   │
│ data-access │   ✓    │    ✗    │      ✓      │ ✗  │  ✓   │
│ ui          │   ✓    │    ✗    │      ✗      │ ✓  │  ✓   │
│ shared      │   ✓    │    ✗    │      ✗      │ ✗  │  ✓   │
│ util        │   ✗    │    ✗    │      ✗      │ ✗  │  ✓   │
└─────────────┴────────┴─────────┴─────────────┴────┴──────┘

Legend:
✓ = Allowed and encouraged
⚠ = Allowed but use with caution (risk of circular dependencies)
✗ = Prohibited (architectural violation)

Key Rules:
1. Apps can import from any lib scope
2. feature/ can import from lower layers (data-access, ui, shared, util)
3. data-access/ can only import shared/ and util/
4. ui/ can only import shared/ and util/
5. util/ should be self-contained (only import other util/)
6. No circular dependencies allowed
```

## Configuration Files

### 1. Root `package.json`

**Location**: `/package.json`

**Purpose**: Workspace root configuration with Volta pinning and npm workspaces

**Configuration**:

```json
{
  "name": "open-sharia-enterprise",
  "version": "0.1.0",
  "private": true,
  "description": "Fintech application with monorepo architecture",
  "license": "MIT",
  "workspaces": ["apps/*", "libs/*/*"],
  "scripts": {
    "build": "nx run-many -t build",
    "test": "nx run-many -t test",
    "lint": "nx run-many -t lint",
    "affected:build": "nx affected -t build",
    "affected:test": "nx affected -t test",
    "affected:lint": "nx affected -t lint",
    "graph": "nx graph",
    "nx": "nx"
  },
  "devDependencies": {
    "nx": "^19.0.0",
    "@commitlint/cli": "^existing",
    "@commitlint/config-conventional": "^existing",
    "husky": "^existing",
    "lint-staged": "^existing",
    "prettier": "^existing",
    "typescript": "^5.0.0"
  },
  "volta": {
    "node": "24.11.1",
    "npm": "11.6.2"
  }
}
```

**Key Points**:

- `workspaces` field enables npm workspaces for dependency hoisting
- `apps/*` includes all apps
- `libs/*/*` includes all scoped libs (e.g., `libs/shared/utils`)
- Scripts provide convenient access to common Nx commands
- Volta pinning preserved from existing setup
- No Nx plugins in dependencies

### 2. `nx.json`

**Location**: `/nx.json`

**Purpose**: Nx workspace configuration

**Configuration**:

```json
{
  "$schema": "./node_modules/nx/schemas/nx-schema.json",
  "affected": {
    "defaultBase": "main"
  },
  "tasksRunnerOptions": {
    "default": {
      "runner": "nx/tasks-runners/default",
      "options": {
        "cacheableOperations": ["build", "test", "lint"]
      }
    }
  },
  "targetDefaults": {
    "build": {
      "dependsOn": ["^build"],
      "outputs": ["{projectRoot}/dist"],
      "cache": true
    },
    "test": {
      "dependsOn": ["build"],
      "cache": true
    },
    "lint": {
      "cache": true
    }
  },
  "namedInputs": {
    "default": ["{projectRoot}/**/*", "sharedGlobals"],
    "production": [
      "default",
      "!{projectRoot}/**/*.spec.ts",
      "!{projectRoot}/**/*.test.ts"
    ],
    "sharedGlobals": ["{workspaceRoot}/tsconfig.base.json"]
  },
  "generators": {},
  "plugins": []
}
```

**Key Configuration Explained**:

- **affected.defaultBase** - Use `main` branch as base for affected detection
- **tasksRunnerOptions** - Use default runner with caching for build, test, lint
- **targetDefaults.build** - Build tasks:
  - `dependsOn: ["^build"]` - Build dependencies first
  - `outputs` - Cache build outputs in `dist/`
  - `cache: true` - Enable caching
- **targetDefaults.test** - Test tasks:
  - `dependsOn: ["build"]` - Build before testing
  - `cache: true` - Enable caching
- **namedInputs** - Define what files affect cache invalidation
- **generators** - Empty (no generators used)
- **plugins** - Empty (no plugins used)

### 3. `tsconfig.base.json`

**Location**: `/tsconfig.base.json`

**Purpose**: Base TypeScript configuration with path mappings

**Configuration**:

```json
{
  "compileOnSave": false,
  "compilerOptions": {
    "rootDir": ".",
    "sourceMap": true,
    "declaration": false,
    "moduleResolution": "node",
    "emitDecoratorMetadata": true,
    "experimentalDecorators": true,
    "importHelpers": true,
    "target": "ES2022",
    "module": "ESNext",
    "lib": ["ES2022"],
    "skipLibCheck": true,
    "skipDefaultLibCheck": true,
    "strict": true,
    "esModuleInterop": true,
    "baseUrl": ".",
    "paths": {
      "@open-sharia/shared/*": ["libs/shared/*/src/index.ts"],
      "@open-sharia/feature/*": ["libs/feature/*/src/index.ts"],
      "@open-sharia/data-access/*": ["libs/data-access/*/src/index.ts"],
      "@open-sharia/ui/*": ["libs/ui/*/src/index.ts"],
      "@open-sharia/util/*": ["libs/util/*/src/index.ts"]
    }
  },
  "exclude": ["node_modules", "tmp", "dist"]
}
```

**Key Points**:

- Path mappings enable clean imports: `import { utils } from '@open-sharia/shared/utils'`
- All paths point to `src/index.ts` for controlled public API
- Shared across all projects in the workspace
- Projects extend this with their own `tsconfig.json`

### 4. Project `project.json` (App Example)

**Location**: `/apps/[app-name]/project.json`

**Purpose**: Nx configuration for an individual app

**Configuration**:

```json
{
  "name": "sample-app",
  "sourceRoot": "apps/sample-app/src",
  "projectType": "application",
  "targets": {
    "build": {
      "executor": "nx:run-commands",
      "options": {
        "command": "tsc -p apps/sample-app/tsconfig.build.json",
        "cwd": "."
      },
      "outputs": ["{projectRoot}/dist"]
    },
    "serve": {
      "executor": "nx:run-commands",
      "options": {
        "command": "node apps/sample-app/dist/index.js",
        "cwd": "."
      },
      "dependsOn": ["build"]
    },
    "test": {
      "executor": "nx:run-commands",
      "options": {
        "command": "node --test apps/sample-app/src/**/*.test.ts",
        "cwd": "."
      },
      "dependsOn": ["build"]
    }
  }
}
```

**Key Points**:

- `executor: "nx:run-commands"` - Use run-commands (no plugins)
- Build command runs `tsc` directly
- Serve command runs built output
- Test command uses Node.js built-in test runner
- Tasks specify dependencies (test depends on build)

### 5. Project `project.json` (Lib Example)

**Location**: `/libs/[scope]/[name]/project.json`

**Purpose**: Nx configuration for an individual library

**Configuration**:

```json
{
  "name": "shared-utils",
  "sourceRoot": "libs/shared/utils/src",
  "projectType": "library",
  "targets": {
    "build": {
      "executor": "nx:run-commands",
      "options": {
        "command": "tsc -p libs/shared/utils/tsconfig.build.json",
        "cwd": "."
      },
      "outputs": ["{projectRoot}/dist"]
    },
    "test": {
      "executor": "nx:run-commands",
      "options": {
        "command": "node --test libs/shared/utils/src/**/*.test.ts",
        "cwd": "."
      },
      "dependsOn": ["build"]
    },
    "lint": {
      "executor": "nx:run-commands",
      "options": {
        "command": "echo 'Linting not configured yet'",
        "cwd": "."
      }
    }
  }
}
```

**Key Points**:

- `projectType: "library"` - Identifies this as a library
- Similar structure to app configuration
- Lint target placeholder (to be configured later)
- All executors use `nx:run-commands`

### 6. `.nxignore`

**Location**: `/.nxignore`

**Purpose**: Exclude paths from Nx processing

**Configuration**:

```
# Documentation
docs/
plans/
*.md

# Git files
.git/
.github/

# IDE
.vscode/
.idea/

# Temporary
tmp/
temp/
.cache/

# CI artifacts
coverage/
.nyc_output/
```

**Key Points**:

- Excludes documentation from Nx graph
- Excludes IDE and temporary files
- Reduces Nx processing overhead

## Implementation Approach

### Phase 1: Install Nx and Base Configuration

**Goal**: Get Nx installed and configured

**Tasks**:

1. **Install Nx**

   ```bash
   npm install -D nx@latest
   ```

2. **Verify Installation**

   ```bash
   npx nx --version
   ```

3. **Create `nx.json`**
   - Configure affected detection (defaultBase: main)
   - Configure task caching (build, test, lint)
   - Configure target defaults

4. **Update `package.json`**
   - Add `workspaces` field: `["apps/*", "libs/*/*"]`
   - Add npm scripts for Nx commands
   - Verify Volta configuration remains intact

5. **Create `tsconfig.base.json`**
   - Base TypeScript configuration
   - Path mappings for all lib scopes

6. **Create `.nxignore`**
   - Exclude docs, plans, and non-code files

**Validation**:

- `npx nx --version` shows version
- `npm ls nx` shows only `nx` package (no plugins)
- Volta still works: `node --version` shows 24.11.1

### Phase 2: Create Folder Structure

**Goal**: Establish apps/ and libs/ folders with documentation

**Tasks**:

1. **Create Apps Directory**

   ```bash
   mkdir -p apps
   ```

2. **Create Apps README**
   - Document purpose: deployable applications
   - Document naming: `[domain]-[type]`
   - Document structure: required files
   - Document rules: apps don't import other apps

3. **Create Libs Directory with Scopes**

   ```bash
   mkdir -p libs/shared
   mkdir -p libs/feature
   mkdir -p libs/data-access
   mkdir -p libs/ui
   mkdir -p libs/util
   ```

4. **Create Libs README**
   - Document purpose: reusable libraries
   - Document scopes: shared, feature, data-access, ui, util
   - Document naming: `[scope]/[name]`
   - Document dependency rules
   - Document structure: required files

**Validation**:

- Directory structure matches architecture diagram
- README files are complete and accurate
- Folder structure is ready for projects

### Phase 3: Create Sample App

**Goal**: Validate app structure and configuration

**Tasks**:

1. **Create App Directory**

   ```bash
   mkdir -p apps/sample-app/src
   ```

2. **Create App Files**
   - `apps/sample-app/src/index.ts` - Entry point with simple console.log
   - `apps/sample-app/package.json` - App metadata (name, version)
   - `apps/sample-app/project.json` - Nx configuration (build, serve, test targets)
   - `apps/sample-app/tsconfig.json` - Extends tsconfig.base.json
   - `apps/sample-app/tsconfig.build.json` - Build-specific config
   - `apps/sample-app/README.md` - App documentation

3. **Test App Build**

   ```bash
   nx build sample-app
   ```

4. **Test App Serve**
   ```bash
   nx serve sample-app
   ```

**Validation**:

- Build succeeds and creates `apps/sample-app/dist/`
- Serve runs built output successfully
- App structure follows documented standards

### Phase 4: Create Sample Lib

**Goal**: Validate lib structure and cross-project imports

**Tasks**:

1. **Create Lib Directory**

   ```bash
   mkdir -p libs/shared/sample-lib/src/lib
   ```

2. **Create Lib Files**
   - `libs/shared/sample-lib/src/index.ts` - Public API (barrel export)
   - `libs/shared/sample-lib/src/lib/greet.ts` - Simple function: `export function greet(name: string) { return \`Hello, ${name}\`; }`
   - `libs/shared/sample-lib/src/lib/greet.test.ts` - Simple test
   - `libs/shared/sample-lib/package.json` - Lib metadata
   - `libs/shared/sample-lib/project.json` - Nx configuration
   - `libs/shared/sample-lib/tsconfig.json` - Extends tsconfig.base.json
   - `libs/shared/sample-lib/tsconfig.build.json` - Build-specific config
   - `libs/shared/sample-lib/README.md` - Lib documentation

3. **Test Lib Build**

   ```bash
   nx build shared-sample-lib
   ```

4. **Test Lib Tests**

   ```bash
   nx test shared-sample-lib
   ```

5. **Import Lib in Sample App**
   - Update `apps/sample-app/src/index.ts`:
     ```typescript
     import { greet } from "@open-sharia/shared/sample-lib";
     console.log(greet("World"));
     ```
   - Rebuild and run app:
     ```bash
     nx build sample-app
     nx serve sample-app
     ```

**Validation**:

- Lib builds successfully
- Lib tests pass
- App successfully imports and uses lib
- `nx graph` shows app -> lib dependency

### Phase 5: Test Nx Features

**Goal**: Validate core Nx capabilities

**Tasks**:

1. **Test Dependency Graph**

   ```bash
   nx graph
   ```

   - Verify graph shows sample-app depending on shared-sample-lib

2. **Test Task Caching**

   ```bash
   nx build sample-app
   nx build sample-app  # Should use cache
   ```

   - Second build should show "[local cache]"

3. **Test Affected Detection**

   ```bash
   # Make a change to sample-lib
   nx affected:build  # Should build lib and app
   # Make a change to app only
   nx affected:build  # Should build only app
   ```

4. **Test Run-Many**
   ```bash
   nx run-many -t build  # Build all projects
   nx run-many -t test   # Test all projects
   ```

**Validation**:

- Graph visualization works
- Caching improves performance
- Affected detection identifies changed projects correctly
- Run-many executes tasks across all projects

### Phase 6: Documentation

**Goal**: Create comprehensive documentation

**Tasks**:

1. **Update CLAUDE.md**
   - Add monorepo structure section
   - Document apps/ and libs/ folders
   - Reference how-to guides

2. **Create How-To Guides** (in `docs/how-to/`):
   - `ht__add-new-app.md` - Step-by-step app creation
   - `ht__add-new-lib.md` - Step-by-step lib creation
   - `ht__run-nx-commands.md` - Common workflows

3. **Create Reference Docs** (in `docs/reference/`):
   - `re__monorepo-structure.md` - Complete structure reference
   - `re__nx-configuration.md` - Configuration file reference

4. **Update Root README.md**
   - Add monorepo overview
   - Link to documentation

**Validation**:

- All documentation follows Diátaxis framework
- Documentation is accurate and tested
- New developers can follow guides successfully

### Phase 7: Cleanup and Final Validation

**Goal**: Remove samples and verify production readiness

**Tasks**:

1. **Decide on Sample Projects**
   - Option A: Remove sample-app and sample-lib (clean slate)
   - Option B: Keep as reference examples (rename to `example-app`, `example-lib`)

2. **Run Full Validation**
   - All builds pass
   - All tests pass
   - No linting errors
   - Git hooks work correctly

3. **Verify No Plugins**

   ```bash
   npm ls | grep @nx/
   ```

   - Should only show `nx` package

4. **Verify Compatibility**
   - Volta: `node --version` shows 24.11.1
   - Git hooks: Create test commit, verify hooks run
   - Commitlint: Test with invalid commit message

5. **Final Cleanup**
   - Remove any temporary files
   - Ensure .gitignore includes dist/ and .nx/

**Validation**:

- Workspace is clean and ready for real development
- All validation criteria met
- No errors or warnings

## Design Decisions

### Decision 1: No Nx Plugins

**Context**: Nx offers official plugins for React, Next.js, Node.js, etc. that provide generators and executors

**Decision**: Do NOT use any Nx plugins; use vanilla Nx with manual configuration

**Rationale**:

- **Full Control** - Direct control over build process and configuration
- **Simplicity** - No plugin-specific abstractions or DSL to learn
- **Transparency** - Build commands are standard tools (tsc, node)
- **Flexibility** - Easy to customize without plugin constraints
- **No Lock-In** - Not dependent on plugin update cycles
- **Easier Debugging** - Standard tools, standard error messages

**Alternatives Considered**:

- Use `@nx/node` plugin for Node.js projects - Rejected for reasons above
- Use `@nx/js` plugin for TypeScript projects - Rejected for reasons above

**Consequences**:

- ✅ Full transparency and control
- ✅ Simpler mental model
- ✅ Standard tooling
- ❌ No code generators (must create projects manually)
- ❌ No plugin-specific executors (must use run-commands)
- ❌ More manual setup required

### Decision 2: Scope-Based Library Organization

**Context**: Need to organize libs in a scalable, maintainable way

**Decision**: Use scope-based folders: `shared/`, `feature/`, `data-access/`, `ui/`, `util/`

**Rationale**:

- **Clear Purpose** - Each scope has a well-defined role
- **Dependency Rules** - Scopes enforce architectural layers
- **Scalability** - Can grow to hundreds of libs with clear organization
- **Discoverability** - Easy to find libs by their purpose
- **Convention** - Widely used pattern in Nx community

**Alternatives Considered**:

- Flat structure (`libs/[name]`) - Rejected: doesn't scale, no organization
- Domain-based (`libs/auth/`, `libs/payments/`) - Rejected: mixes concerns, unclear dependencies
- No organization - Rejected: chaos at scale

**Consequences**:

- ✅ Clear, scalable organization
- ✅ Enforced architectural layers
- ✅ Easy to find and understand libs
- ❌ Slightly longer import paths
- ❌ Requires discipline to maintain

### Decision 3: TypeScript Path Mappings

**Context**: Need clean, consistent import paths for libs

**Decision**: Use `@open-sharia/[scope]/[name]` pattern in tsconfig.base.json

**Rationale**:

- **Clean Imports** - `import { utils } from '@open-sharia/shared/utils'` is clear and readable
- **Controlled API** - All imports go through `index.ts` barrel exports
- **Refactoring** - Moving files doesn't break imports if public API stays same
- **Consistency** - Same pattern across all libs
- **Scope Prefix** - Prevents conflicts with npm packages

**Alternatives Considered**:

- Relative imports (`../../../libs/shared/utils`) - Rejected: brittle, hard to read
- No path mappings - Rejected: no control over public API
- Different prefix - Rejected: `@open-sharia` matches project name

**Consequences**:

- ✅ Clean, readable imports
- ✅ Controlled public APIs
- ✅ Easy refactoring
- ❌ Requires tsconfig.base.json maintenance
- ❌ IDE may need restart after adding new libs

### Decision 4: npm Workspaces

**Context**: Need dependency management for monorepo

**Decision**: Use npm workspaces (built into npm) with patterns `apps/*` and `libs/*/*`

**Rationale**:

- **Native Support** - Built into npm, no additional tools
- **Hoisting** - Dependencies hoisted to root node_modules
- **Performance** - Single npm install for entire workspace
- **Compatibility** - Works with Volta, existing tooling
- **Simplicity** - Standard npm, no learning curve

**Alternatives Considered**:

- pnpm workspaces - Rejected: requires pnpm (Volta manages npm)
- Yarn workspaces - Rejected: requires Yarn (Volta manages npm)
- Lerna - Rejected: outdated, npm workspaces replaced it

**Consequences**:

- ✅ Native npm feature
- ✅ Simple, well-supported
- ✅ Works with Volta
- ❌ Hoisting can occasionally cause issues (rare)
- ❌ All projects share same dependency versions (usually a benefit)

### Decision 5: Node.js Built-in Test Runner

**Context**: Need basic testing capability for validation

**Decision**: Use Node.js built-in test runner (`node --test`) for now

**Rationale**:

- **No Dependencies** - Built into Node.js 24.11.1
- **Simple** - No configuration required
- **Sufficient** - Adequate for basic unit tests
- **Temporary** - Can be replaced with Jest/Vitest later
- **Fast Setup** - No additional installation or setup

**Alternatives Considered**:

- Jest - Rejected: requires setup and configuration (future work)
- Vitest - Rejected: requires setup and configuration (future work)
- No tests - Rejected: need validation

**Consequences**:

- ✅ Zero setup testing
- ✅ Fast to get started
- ✅ Sufficient for validation
- ❌ Limited features (no mocking, coverage, etc.)
- ❌ Will likely be replaced later

### Decision 6: Sample Projects Strategy

**Context**: Need to validate setup, but should we keep samples?

**Decision**: Create samples for validation, then decide to keep or remove

**Rationale**:

- **Validation** - Samples prove setup works correctly
- **Examples** - Can serve as reference for new developers
- **Flexibility** - Can remove if they add clutter
- **Defer Decision** - Decide during implementation based on value

**Options**:

- **Option A**: Remove samples after validation (clean slate)
- **Option B**: Keep and rename to `example-app`, `example-lib` (reference)

**Consequences**:

- ✅ Validation happens regardless
- ✅ Flexibility to keep or remove
- ⚠️ Need to document decision in delivery plan

## Risks and Mitigation

### Risk 1: Team Learning Curve

**Probability**: Medium
**Impact**: Medium

**Risk**: Team unfamiliar with Nx concepts (affected, graph, caching)

**Mitigation**:

- Comprehensive documentation in how-to guides
- Sample projects as learning references
- Clear naming conventions
- Team knowledge sharing sessions
- Incremental adoption (start simple)

**Contingency**: If team struggles, provide additional training and pair programming

### Risk 2: Manual Configuration Errors

**Probability**: Medium
**Impact**: Medium

**Risk**: Without generators, manual project creation is error-prone

**Mitigation**:

- Create templates (.templates/ folder)
- Document standard project.json patterns
- Code review for all new projects
- Validation scripts to check configuration
- Clear checklists for adding projects

**Contingency**: If errors occur frequently, create scripts to automate parts of setup

### Risk 3: Build Performance Degradation

**Probability**: Low
**Impact**: High

**Risk**: As monorepo grows, builds become slow

**Mitigation**:

- Enable task caching from the start
- Use affected commands in CI/CD
- Regular dependency graph analysis
- Optimize tsconfig for incremental builds
- Monitor build times and investigate spikes

**Contingency**: If performance degrades, evaluate Nx Cloud or distributed caching

### Risk 4: Dependency Conflicts

**Probability**: Low
**Impact**: Medium

**Risk**: Shared dependencies at root cause version conflicts

**Mitigation**:

- Single package.json strategy (one version wins)
- Minimize project-specific dependencies
- Regular dependency audits
- Clear ownership of shared dependencies
- Document any version constraints

**Contingency**: If conflicts occur, use npm overrides or move to project-specific deps

### Risk 5: Circular Dependencies

**Probability**: Medium
**Impact**: Medium

**Risk**: Libs import each other creating circular dependencies

**Mitigation**:

- Clear dependency rules by scope
- Regular dependency graph reviews
- Nx can detect circular deps (configure if needed)
- Code review focuses on import patterns
- Documentation emphasizes layer architecture

**Contingency**: If circulars occur, refactor to break cycle (extract shared code)

### Risk 6: Missing Plugin Features

**Probability**: High (expected)
**Impact**: Low

**Risk**: Manual setup lacks convenience of generators and executors

**Mitigation**:

- Accept trade-off for simplicity and control
- Create templates and documentation as substitutes
- Build scripts for common tasks
- Re-evaluate plugins if pain points emerge
- Focus on value of transparency

**Contingency**: Can add specific plugins later if truly needed (decision point)

## Testing Strategy

### Unit Testing

**Scope**: Individual functions and modules within libs

**Approach**:

- Use Node.js built-in test runner for now
- Test files: `[name].test.ts` or `[name].spec.ts`
- Co-located with source: `src/lib/utils.ts` → `src/lib/utils.test.ts`
- Command: `nx test [lib-name]`

**Example**:

```typescript
// libs/shared/utils/src/lib/greet.test.ts
import { test } from "node:test";
import assert from "node:assert";
import { greet } from "./greet";

test("greet returns greeting message", () => {
  assert.strictEqual(greet("World"), "Hello, World");
});
```

### Integration Testing

**Scope**: Cross-project dependencies (app imports lib)

**Approach**:

- Create app that imports lib
- Build both projects
- Verify app can use lib functionality
- Test via `nx graph` to verify dependency

**Example**:

- Create `libs/shared/sample-lib` with `greet()` function
- Create `apps/sample-app` that imports and uses `greet()`
- Build: `nx build sample-app` (builds lib first due to dependencies)
- Verify: `nx serve sample-app` runs successfully

### System Testing

**Scope**: Entire workspace and Nx features

**Approach**:

- Test all Nx commands work correctly
- Verify caching behavior
- Verify affected detection
- Verify dependency graph

**Test Cases**:

1. **Build All** - `nx run-many -t build` succeeds
2. **Test All** - `nx run-many -t test` succeeds
3. **Caching** - Second build uses cache
4. **Affected** - Only changed projects rebuild
5. **Graph** - Dependency visualization works

### Validation Checklist

**Functional Validation**:

- [ ] Nx version displays correctly
- [ ] No Nx plugins installed (`npm ls | grep @nx`)
- [ ] Apps folder exists with documentation
- [ ] Libs folder exists with scope structure
- [ ] Sample app builds successfully
- [ ] Sample lib builds successfully
- [ ] App imports lib successfully
- [ ] Dependency graph shows relationships
- [ ] Task caching works (second build is fast)
- [ ] Affected detection identifies changes
- [ ] All npm scripts work

**Quality Validation**:

- [ ] All JSON files are valid
- [ ] TypeScript compiles without errors
- [ ] No linting errors (if configured)
- [ ] Git hooks still work (Husky, prettier, commitlint)
- [ ] Volta still manages Node.js version
- [ ] Documentation is complete and accurate

**Compatibility Validation**:

- [ ] `node --version` shows 24.11.1 (Volta)
- [ ] `npm --version` shows 11.6.2 (Volta)
- [ ] Pre-commit hook formats files with Prettier
- [ ] Commit-msg hook validates with commitlint
- [ ] Conventional commits enforced

## Deployment Considerations

**Note**: This plan focuses on workspace setup, not application deployment. However, some considerations for future deployment:

### CI/CD Integration (Future Work)

**GitHub Actions Integration**:

- Use `nx affected` to only build/test changed projects
- Cache Nx computation cache between runs
- Parallelize builds where possible

**Example Future Workflow**:

```yaml
# .github/workflows/ci.yml (future)
name: CI
on: [push, pull_request]
jobs:
  build:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v3
      - uses: actions/setup-node@v3
      - run: npm ci
      - run: npx nx affected:build --base=origin/main
      - run: npx nx affected:test --base=origin/main
```

### Build Outputs

Each project builds to its own `dist/` folder:

- `apps/[app-name]/dist/` - Deployable app artifacts
- `libs/[scope]/[name]/dist/` - Compiled library code

These can be deployed, containerized, or published as needed in future plans.
