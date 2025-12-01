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
│         Nx Polyglot Monorepo Workspace Architecture              │
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
         ┌───────┼──────┐    ┌────┴─────────────┐
         │       │      │    │ (Flat structure) │
         ▼          ▼           ▼
      demo-ts-fe  app-2  ts-demo-libs
                         ts-utils
                         ts-components

┌─────────────────────────────────────────────────────────────────┐
│  Multi-Language Support (TypeScript, Go, Python, Rust, etc.)    │
└─────────────────────────────────────────────────────────────────┘

         Apps Layer (Any language)
         ┌─────────────────────────────────────────┐
         │  app-1    app-2    app-3                │
         └──────────────┬──────────────────────────┘
                        │ imports
                        ▼
         Libs Layer (Flat, language-prefixed)
         ┌─────────────────────────────────────────┐
         │  ts-demo-libs    ts-utils               │
         │  ts-components   ts-hooks               │
         │  (Future: java-*, kt-*, py-*)           │
         └─────────────────────────────────────────┘

Rules:
- Apps can import from any lib
- Libs can import from other libs (be mindful of circular deps)
- No circular dependencies allowed
- Each language uses its own build tools via nx:run-commands
```

## Technology Stack

### Core Technologies

| Technology     | Version               | Purpose                                      |
| -------------- | --------------------- | -------------------------------------------- |
| **Nx**         | Latest stable (^19.x) | Monorepo build system and task runner        |
| **Node.js**    | 24.11.1 (LTS)         | JavaScript runtime (Volta-managed)           |
| **npm**        | 11.6.2                | Package manager (Volta-managed)              |
| **TypeScript** | ^5.x                  | Primary development language (current focus) |
| **Next.js**    | Latest stable         | React framework for frontend applications    |
| **React**      | Latest stable         | UI library for building interfaces           |
| **Java**       | Latest LTS            | Future: Backend services and enterprise apps |
| **Kotlin**     | Latest stable         | Future: Android and backend services         |
| **Python**     | Latest stable         | Future: Data processing and ML               |
| **Volta**      | Current               | Node/npm version management                  |

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

**Purpose**: Contains reusable library packages in multiple programming languages

**Location**: `libs/` at repository root

**Naming Convention**: `[lang-prefix]-[name]` (flat structure)

**Language Prefixes**:

- `ts-*` - TypeScript libraries (current implementation)
- `java-*` - Java libraries (future)
- `kt-*` - Kotlin libraries (future)
- `py-*` - Python libraries (future)

**Current Scope** (TypeScript only):

- `ts-demo-libs` - Demo TypeScript library consumed by Next.js app
- `ts-utils` - TypeScript utility functions
- `ts-components` - Reusable React components
- `ts-hooks` - Custom React hooks
- `ts-api` - API client libraries

**Future Scope** (Planned):

- `java-services` - Java backend services
- `java-utils` - Java utility libraries
- `kt-android` - Kotlin Android libraries
- `kt-backend` - Kotlin backend services
- `py-ml` - Python machine learning models
- `py-data` - Python data processing

**Standard Structure (TypeScript example)**:

```
libs/
└── ts-[name]/
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

**Standard Structure (Next.js App example)**:

```
apps/
└── demo-ts-fe/
    ├── app/                 # Next.js 14+ app directory
    │   ├── page.tsx         # Home page
    │   ├── layout.tsx       # Root layout
    │   └── ...
    ├── public/              # Static assets
    ├── next.config.js       # Next.js configuration
    ├── package.json         # App dependencies
    ├── project.json         # Nx project configuration
    ├── tsconfig.json        # TypeScript configuration
    └── README.md            # App documentation
```

**Future Structure Examples** (Java, Kotlin, Python - not implemented yet):

```
# Java library (future)
libs/java-services/
    ├── src/main/java/
    ├── src/test/java/
    ├── pom.xml or build.gradle
    ├── project.json
    └── README.md

# Kotlin library (future)
libs/kt-backend/
    ├── src/main/kotlin/
    ├── src/test/kotlin/
    ├── build.gradle.kts
    ├── project.json
    └── README.md

# Python library (future)
libs/py-ml/
    ├── src/[package]/
    ├── tests/
    ├── requirements.txt
    ├── project.json
    └── README.md
```

**Characteristics**:

- **Polyglot-Ready** - Designed to support multiple languages (TypeScript now, Java/Kotlin/Python future)
- **Flat Structure** - All libs at same level (no nested scopes)
- **Language-Specific** - Each language uses its own conventions and tools
- **Reusable** - Libs are designed to be imported by apps and other libs
- **Focused** - Each lib has a single, clear purpose
- **Public API** - Exports controlled through index.ts (TypeScript) or language-specific mechanisms
- **Testable** - Can be tested independently using language-specific test frameworks

**Current Implementation**: TypeScript libraries only (Next.js + React ecosystem)

### Dependency Guidelines for Flat Structure

With a flat library structure using language prefixes, dependency management is more flexible but requires discipline to avoid circular dependencies and maintain clean architecture.

#### General Dependency Rules

1. **Apps can import from any lib** - Applications are consumers and sit at the top of the dependency graph
2. **Libs can import from other libs** - Cross-library dependencies are allowed
3. **No circular dependencies** - Strictly prohibited (A → B → A is not allowed)
4. **Language boundaries** - TypeScript libs can't directly import Go/Python/Rust libs (use APIs or IPC instead)
5. **Keep dependencies minimal** - Each lib should have clear, focused dependencies

#### Dependency Best Practices

**For TypeScript Libraries**:

- Use TypeScript path mappings in `tsconfig.base.json`
- Import via `@open-sharia-enterprise/ts-[name]` pattern
- Declare dependencies in `package.json` if needed

**For Go Libraries**:

- Use Go modules (`go.mod`)
- Import via standard Go import paths
- Keep libraries in separate modules if they're independently versioned

**For Python Libraries**:

- Use `requirements.txt` or `pyproject.toml`
- Import via standard Python import mechanisms
- Consider using namespace packages for related libs

**For Rust Libraries**:

- Use Cargo workspace features
- Define dependencies in `Cargo.toml`
- Use workspace members for shared dependencies

#### Cross-Language Communication

When libraries in different languages need to interact:

1. **HTTP APIs** - Expose REST/GraphQL endpoints (most common)
2. **gRPC** - For high-performance cross-language RPC
3. **Message Queues** - For async communication (RabbitMQ, Kafka, etc.)
4. **Shared Data Formats** - JSON, Protobuf, MessagePack
5. **CLI Interfaces** - Call programs as subprocesses (least preferred)

#### Monitoring Dependencies

**Use Nx dependency graph**:

```bash
nx graph                    # View full dependency graph
nx affected:graph           # View affected projects
```

**Watch for**:

- Circular dependencies (Nx will detect these)
- Deep dependency chains (A → B → C → D → E suggests refactoring)
- Cross-language coupling (should be minimal and explicit)

#### Example Dependency Patterns

**Good Patterns**:

```
ts-utils ← ts-auth ← app-api
  (Simple linear dependency chain)

ts-components ← demo-ts-fe
  (Direct, minimal dependency)

ts-validators (standalone, no deps)
  (Self-contained utility)
```

**Patterns to Avoid**:

```
ts-auth ← ts-users ← ts-auth
  (Circular dependency - will fail)

ts-lib-a ← ts-lib-b ← ts-lib-c ← ts-lib-d ← ts-lib-e
  (Too deep, suggests poor separation of concerns)
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
- `libs/*` includes all libs in flat structure (e.g., `libs/ts-demo-libs`, `libs/ts-utils`)
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
      "@open-sharia-enterprise/ts-*": ["libs/ts-*/src/index.ts"]
    }
  },
  "exclude": ["node_modules", "tmp", "dist"]
}
```

**Key Points**:

- Path mappings enable clean imports: `import { demo } from '@open-sharia-enterprise/ts-demo-libs'`
- TypeScript paths point to `src/index.ts` for controlled public API
- Wildcard pattern `ts-*` matches all TypeScript libraries
- Shared across all TypeScript projects in the workspace
- Projects extend this with their own `tsconfig.json`
- Future languages (Java, Kotlin, Python) will use their own import systems

### 4. Project `project.json` (Next.js App Example)

**Location**: `/apps/demo-ts-fe/project.json`

**Purpose**: Nx configuration for the Next.js demo app

**Configuration**:

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
    "start": {
      "executor": "nx:run-commands",
      "options": {
        "command": "next start",
        "cwd": "apps/demo-ts-fe"
      },
      "dependsOn": ["build"]
    },
    "lint": {
      "executor": "nx:run-commands",
      "options": {
        "command": "next lint",
        "cwd": "apps/demo-ts-fe"
      }
    }
  }
}
```

**Key Points**:

- `executor: "nx:run-commands"` - Use run-commands (no plugins)
- Dev command runs Next.js dev server
- Build command runs Next.js production build
- Start command serves production build
- Lint command runs Next.js ESLint
- Tasks specify dependencies (start depends on build)

### 5. Project `project.json` (TypeScript Lib Example)

**Location**: `/libs/ts-[name]/project.json`

**Purpose**: Nx configuration for an individual TypeScript library

**Configuration**:

```json
{
  "name": "ts-utils",
  "sourceRoot": "libs/ts-utils/src",
  "projectType": "library",
  "targets": {
    "build": {
      "executor": "nx:run-commands",
      "options": {
        "command": "tsc -p libs/ts-utils/tsconfig.build.json",
        "cwd": "."
      },
      "outputs": ["{projectRoot}/dist"]
    },
    "test": {
      "executor": "nx:run-commands",
      "options": {
        "command": "node --test libs/ts-utils/src/**/*.test.ts",
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
   - Add `workspaces` field: `["apps/*", "libs/*"]`
   - Add npm scripts for Nx commands
   - Verify Volta configuration remains intact

5. **Create `tsconfig.base.json`**
   - Base TypeScript configuration
   - Path mappings for language-prefixed libraries

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

3. **Create Libs Directory**

   ```bash
   mkdir -p libs
   ```

4. **Create Libs README**
   - Document purpose: reusable libraries
   - Document naming: `[language-prefix]-[name]` (e.g., ts-demo-libs)
   - Document current language prefix: `ts-` (TypeScript)
   - Document future language prefixes: `java-`, `kt-`, `py-`
   - Document dependency rules
   - Document structure: required files

**Validation**:

- Directory structure matches architecture diagram
- README files are complete and accurate
- Folder structure is ready for projects

### Phase 3: Create Next.js Demo App

**Goal**: Validate Next.js app structure and configuration

**Tasks**:

1. **Initialize Next.js App**

   ```bash
   cd apps
   npx create-next-app@latest demo-ts-fe --typescript --tailwind --eslint --app --no-src-dir
   cd ..
   ```

2. **Create Nx Configuration**
   - `apps/demo-ts-fe/project.json` - Nx configuration with targets:
     - `dev`: runs `next dev`
     - `build`: runs `next build`
     - `start`: runs `next start`
     - `lint`: runs `next lint`

3. **Update TypeScript Config**
   - Ensure `apps/demo-ts-fe/tsconfig.json` extends `../../tsconfig.base.json`
   - This enables importing from `@open-sharia-enterprise/ts-*` libraries

4. **Test App Dev Server**

   ```bash
   nx dev demo-ts-fe
   ```

5. **Test App Build**
   ```bash
   nx build demo-ts-fe
   ```

**Validation**:

- Next.js app initializes successfully
- Dev server runs on http://localhost:3000
- Build succeeds and creates `.next/` directory
- App structure follows Next.js 14+ conventions (app directory)

### Phase 4: Create TypeScript Demo Library

**Goal**: Validate TypeScript library structure and cross-project imports

**Tasks**:

1. **Create Lib Directory**

   ```bash
   mkdir -p libs/ts-demo-libs/src/lib
   ```

2. **Create Lib Files**
   - `libs/ts-demo-libs/src/index.ts` - Public API (barrel export)
   - `libs/ts-demo-libs/src/lib/greet.ts` - Simple function: `export function greet(name: string) { return \`Hello, ${name}\`; }`
   - `libs/ts-demo-libs/src/lib/greet.test.ts` - Simple test using Node.js test runner
   - `libs/ts-demo-libs/package.json` - Lib metadata
   - `libs/ts-demo-libs/project.json` - Nx configuration
   - `libs/ts-demo-libs/tsconfig.json` - Extends tsconfig.base.json
   - `libs/ts-demo-libs/tsconfig.build.json` - Build-specific config
   - `libs/ts-demo-libs/README.md` - Lib documentation

3. **Test Lib Build**

   ```bash
   nx build ts-demo-libs
   ```

4. **Test Lib Tests**

   ```bash
   nx test ts-demo-libs
   ```

5. **Import Lib in Next.js App**
   - Update `apps/demo-ts-fe/app/page.tsx`:

     ```typescript
     import { greet } from "@open-sharia-enterprise/ts-demo-libs";

     export default function Home() {
       const message = greet("Next.js");
       return <div>{message}</div>;
     }
     ```

   - Rebuild and run app:
     ```bash
     nx build demo-ts-fe
     nx dev demo-ts-fe
     ```

**Validation**:

- Lib builds successfully and creates `libs/ts-demo-libs/dist/`
- Lib tests pass
- Next.js app successfully imports and uses lib
- App displays greeting message from library
- `nx graph` shows demo-ts-fe -> ts-demo-libs dependency

### Phase 5: Test Nx Features

**Goal**: Validate core Nx capabilities

**Tasks**:

1. **Test Dependency Graph**

   ```bash
   nx graph
   ```

   - Verify graph shows demo-ts-fe depending on ts-demo-libs

2. **Test Task Caching**

   ```bash
   nx build demo-ts-fe
   nx build demo-ts-fe  # Should use cache
   ```

   - Second build should show "[local cache]"

3. **Test Affected Detection**

   ```bash
   # Make a change to ts-demo-libs
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
   - `hoto__add-new-app.md` - Step-by-step app creation
   - `hoto__add-new-lib.md` - Step-by-step lib creation
   - `hoto__run-nx-commands.md` - Common workflows

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

1. **Decide on Demo Projects**
   - Option A: Remove demo-ts-fe and ts-demo-libs (clean slate for production)
   - Option B: Keep as reference examples and continue using for development

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

### Decision 2: Flat Library Organization with Language Prefixes

**Context**: Need to organize libs for a polyglot monorepo supporting multiple programming languages

**Decision**: Use flat structure with language prefixes: `libs/ts-[name]`, `libs/java-[name]`, `libs/kt-[name]`, `libs/py-[name]`

**Rationale**:

- **Polyglot Support** - Language prefix clearly identifies which language each library uses
- **Simple Structure** - Flat hierarchy is easier to understand and navigate
- **No Artificial Nesting** - Avoids forcing libraries into scope categories that may not fit
- **Future Ready** - Easy to add new languages (Java, Kotlin, Python) alongside TypeScript
- **Wildcard Mappings** - TypeScript path mappings use simple wildcard: `@open-sharia-enterprise/ts-*`
- **Clear Ownership** - Language prefix shows which build tools and conventions apply

**Alternatives Considered**:

- Scope-based folders (`shared/`, `feature/`, `data-access/`, `ui/`, `util/`) - Rejected: doesn't support multiple languages well, forces artificial categorization
- Domain-based (`libs/auth/`, `libs/payments/`) - Rejected: mixes languages, unclear which tools to use
- Nested language folders (`libs/typescript/`, `libs/java/`) - Rejected: extra nesting without benefit
- No prefix (`libs/demo-lib`) - Rejected: unclear which language, naming conflicts across languages

**Consequences**:

- ✅ Clear language identification
- ✅ Simple, flat structure
- ✅ Easy to add new languages
- ✅ Scalable to hundreds of libraries
- ✅ No forced categorization
- ❌ Slightly longer library names (due to prefix)
- ❌ Less architectural enforcement (no scope-based rules)

### Decision 3: TypeScript Path Mappings

**Context**: Need clean, consistent import paths for libs in a polyglot monorepo

**Decision**: Use `@open-sharia-enterprise/[language-prefix]-[name]` pattern with wildcard in tsconfig.base.json

**Rationale**:

- **Clean Imports** - `import { greet } from '@open-sharia-enterprise/ts-demo-libs'` is clear and readable
- **Controlled API** - All imports go through `index.ts` barrel exports
- **Language Clarity** - Prefix shows which language the library uses (ts-, java-, kt-, py-)
- **Flat Structure** - Single wildcard pattern matches all libraries: `@open-sharia-enterprise/ts-*`
- **Refactoring** - Moving files doesn't break imports if public API stays same
- **Consistency** - Same pattern across all TypeScript libs
- **Scope Prefix** - Prevents conflicts with npm packages

**Alternatives Considered**:

- Relative imports (`../../../libs/ts-demo-libs`) - Rejected: brittle, hard to read
- Nested scopes (`@open-sharia-enterprise/shared/utils`) - Rejected: doesn't support multiple languages well
- No path mappings - Rejected: no control over public API
- Different prefix - Rejected: `@open-sharia-enterprise` matches project name

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

### Decision 6: Demo Projects Strategy

**Context**: Need to validate setup with concrete projects (demo-ts-fe, ts-demo-libs)

**Decision**: Create demo projects for validation, then decide to keep or remove

**Rationale**:

- **Validation** - Demo projects prove setup works correctly
- **Examples** - Can serve as reference for new developers
- **Working Code** - Demonstrates library consumption patterns (Next.js importing TypeScript lib)
- **Flexibility** - Can remove if they add clutter
- **Defer Decision** - Decide during implementation based on value

**Options**:

- **Option A**: Remove demos after validation (clean slate for production)
- **Option B**: Keep as reference examples (demo-ts-fe and ts-demo-libs)

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

- Clear dependency rules and architectural guidelines
- Regular dependency graph reviews (`nx graph`)
- Nx can detect circular deps (configure if needed)
- Code review focuses on import patterns
- Documentation emphasizes clean architecture principles

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
// libs/ts-demo-libs/src/lib/greet.test.ts
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

- Create `libs/ts-demo-libs` with `greet()` function
- Create `apps/demo-ts-fe` (Next.js) that imports and uses `greet()`
- Build: `nx build demo-ts-fe` (builds lib first due to dependencies)
- Verify: `nx dev demo-ts-fe` runs successfully and displays greeting

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
- [ ] Libs folder exists with flat structure (no nested scopes)
- [ ] Demo Next.js app (demo-ts-fe) builds successfully
- [ ] Demo TypeScript lib (ts-demo-libs) builds successfully
- [ ] Next.js app imports lib successfully
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

## References and Sources

This plan was informed by official Nx documentation and polyglot monorepo best practices:

### Nx Documentation

- [Nx Folder Structure Best Practices](https://nx.dev/docs/concepts/decisions/folder-structure) - Official guidance on organizing Nx workspaces
- [Running Custom Commands with nx:run-commands](https://nx.dev/recipes/running-tasks/run-commands-executor) - Manual configuration without plugins
- [Nx Documentation](https://nx.dev/) - Official Nx documentation

### Polyglot Monorepo Resources

- [Using Nx to build a multilang monorepo](https://sylhare.github.io/2024/10/21/Nx-multilang-monorepo.html) - Practical guide for polyglot Nx setups
- [Managing multiple languages in a monorepo](https://graphite.com/guides/managing-multiple-languages-in-a-monorepo) - Multi-language strategies

### Architecture Patterns

- [The virtuous cycle of workspace structure | Nx Blog](https://nx.dev/blog/virtuous-cycle-of-workspace-structure) - Workspace organization principles
- [Monorepo Tools Comparison](https://monorepo.tools/) - Nx vs other monorepo tools

**Key Takeaways**:

- Nx supports polyglot monorepos via `nx:run-commands` executor
- Flat structure with clear naming (language prefixes) avoids deep nesting
- Nx recommends not nesting more than 2-3 layers deep
- Manual configuration without plugins provides full control and transparency
