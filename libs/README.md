# Libs Folder

## Purpose

The `libs/` directory contains **reusable library packages** that can be shared across multiple applications. Libraries provide shared functionality, utilities, components, and services.

## Folder Organization

**Flat Structure**: All libraries exist at the same level in `libs/` directory. No nested scopes or subdirectories.

```
libs/
├── ts-demo-libs/
├── ts-utils/
├── ts-components/
├── ts-hooks/
└── (future: java-*, kt-*, py-*)
```

## Naming Convention

Libraries follow the pattern: **`[language-prefix]-[name]`**

This flat structure with language prefixes supports a **polyglot monorepo** where libraries can be written in multiple programming languages.

### Language Prefixes

- **`ts-*`** - TypeScript libraries (current implementation)
- **`java-*`** - Java libraries (future)
- **`kt-*`** - Kotlin libraries (future)
- **`py-*`** - Python libraries (future)

### Examples

**TypeScript libraries** (current focus):

- `ts-demo-libs` - Demo TypeScript library consumed by Next.js app
- `ts-utils` - TypeScript utility functions
- `ts-components` - Reusable React components
- `ts-hooks` - Custom React hooks
- `ts-api` - API client libraries

**Future polyglot examples** (planned):

- `java-services` - Java backend services
- `java-utils` - Java utility libraries
- `kt-android` - Kotlin Android libraries
- `kt-backend` - Kotlin backend services
- `py-ml` - Python machine learning models
- `py-data` - Python data processing

## Current Implementation

**TypeScript only** - The initial implementation focuses on TypeScript libraries for the Next.js/React ecosystem. Support for Java, Kotlin, and Python will be added in future iterations.

## Library Characteristics

- **Polyglot-Ready** - Designed to support multiple languages (TypeScript now, Java/Kotlin/Python future)
- **Flat Structure** - All libs at same level (no nested scopes)
- **Language-Specific** - Each language uses its own conventions and tools
- **Reusable** - Libs are designed to be imported by apps and other libs
- **Focused** - Each lib has a single, clear purpose
- **Public API** - Exports controlled through index.ts (TypeScript) or language-specific mechanisms
- **Testable** - Can be tested independently using language-specific test frameworks

## Required Files (TypeScript Libraries)

Each TypeScript library requires:

```
libs/ts-[name]/
├── src/
│   ├── index.ts             # Public API (barrel export)
│   ├── lib/                 # Implementation
│   │   ├── [feature].ts
│   │   └── [feature].test.ts
│   └── __tests__/           # Integration tests
├── dist/                    # Build output (gitignored)
├── package.json             # Lib dependencies (if any)
├── project.json             # Nx project configuration
├── tsconfig.json            # TypeScript configuration
├── tsconfig.build.json      # Build-specific TS config
└── README.md                # Library documentation
```

## Nx Configuration (project.json)

Each library must have a `project.json` file:

```json
{
  "name": "ts-library-name",
  "sourceRoot": "libs/ts-library-name/src",
  "projectType": "library",
  "targets": {
    "build": {
      "executor": "nx:run-commands",
      "options": {
        "command": "tsc -p libs/ts-library-name/tsconfig.build.json",
        "cwd": "."
      },
      "outputs": ["{projectRoot}/dist"]
    },
    "test": {
      "executor": "nx:run-commands",
      "options": {
        "command": "node --test libs/ts-library-name/src/**/*.test.ts",
        "cwd": "."
      },
      "dependsOn": ["build"]
    }
  }
}
```

**Note**: This repository uses vanilla Nx (no plugins), so all executors use `nx:run-commands` to run standard build tools directly.

## Dependency Guidelines

### General Rules

1. **Apps can import from any lib** - Applications are consumers
2. **Libs can import from other libs** - Cross-library dependencies allowed
3. **No circular dependencies** - Strictly prohibited (A → B → A not allowed)
4. **Language boundaries** - TypeScript libs can't directly import Go/Python/Rust libs (use APIs or IPC)
5. **Keep dependencies minimal** - Each lib should have clear, focused dependencies

### Monitoring Dependencies

Use Nx dependency graph to visualize and monitor:

```bash
nx graph                    # View full dependency graph
nx affected:graph           # View affected projects
```

## How to Add a New Library

See the how-to guide: `docs/how-to/ht__add-new-lib.md` (to be created)

## Path Mappings

TypeScript libraries use workspace path mappings configured in `tsconfig.base.json`:

```json
{
  "paths": {
    "@open-sharia-enterprise/ts-*": ["libs/ts-*/src/index.ts"]
  }
}
```

This allows clean imports:

```typescript
import { greet } from "@open-sharia-enterprise/ts-demo-libs";
import { utils } from "@open-sharia-enterprise/ts-utils";
```

## Running Library Commands

Use Nx commands to work with libraries:

```bash
# Build a library
nx build ts-library-name

# Test a library
nx test ts-library-name

# Build all libraries
nx run-many -t build
```

## Future Language Support

While the current implementation focuses on TypeScript, the structure is designed to support:

- **Java**: Using Maven or Gradle, standard Java project structure
- **Kotlin**: Using Gradle, Kotlin project conventions
- **Python**: Using pip/poetry, standard Python package structure

Each language will use its own build tools via `nx:run-commands` executor, maintaining the vanilla Nx approach.
