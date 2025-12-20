---
title: Nx Configuration Reference
description: Complete reference for Nx workspace configuration files, options, and settings
category: reference
tags:
  - nx
  - configuration
  - build-system
created: 2025-11-29
updated: 2025-11-30
---

# Nx Configuration Reference

Complete reference for Nx workspace configuration files, options, and settings.

## Configuration Files Overview

| File                 | Location    | Purpose                                          |
| -------------------- | ----------- | ------------------------------------------------ |
| `nx.json`            | Root        | Workspace-wide Nx configuration                  |
| `project.json`       | Per-project | Project-specific Nx configuration                |
| `tsconfig.base.json` | Root        | Base TypeScript configuration with path mappings |
| `package.json`       | Root        | Workspace manifest with npm workspaces           |
| `.nxignore`          | Root        | Files to exclude from Nx processing              |

## `nx.json` (Workspace Configuration)

### Location

`/nx.json` (repository root)

### Complete Example

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

### Field Reference

#### `$schema`

JSON schema for validation and autocomplete.

**Value**: `"./node_modules/nx/schemas/nx-schema.json"`

#### `affected`

Configuration for affected detection.

**Fields**:

- `defaultBase` (string): Base branch for comparing changes
  - Default: `"main"`
  - Used by: `nx affected:*` commands

**Example**:

```json
{
  "affected": {
    "defaultBase": "main"
  }
}
```

#### `tasksRunnerOptions`

Configuration for task runners and caching.

**Fields**:

- `default` (object): Default task runner configuration
  - `runner` (string): Task runner to use
    - Default: `"nx/tasks-runners/default"`
  - `options` (object): Runner options
    - `cacheableOperations` (string[]): Tasks to cache
      - Examples: `["build", "test", "lint"]`

**Example**:

```json
{
  "tasksRunnerOptions": {
    "default": {
      "runner": "nx/tasks-runners/default",
      "options": {
        "cacheableOperations": ["build", "test", "lint"]
      }
    }
  }
}
```

#### `targetDefaults`

Default configuration for targets across all projects.

**Common Fields**:

- `dependsOn` (string[]): Task dependencies
  - `"^build"` - Run build on dependencies first
  - `"build"` - Run own build first
- `outputs` (string[]): Output directories to cache
  - `"{projectRoot}/dist"` - Project dist folder
  - `"{projectRoot}/.next"` - Next.js build output
- `cache` (boolean): Enable caching for this target
  - Default: `false`

**Example**:

```json
{
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
  }
}
```

**Dependency Patterns**:

- `["^build"]` - Build all dependencies first
- `["build"]` - Build self first
- `["^build", "prebuild"]` - Build dependencies, then run prebuild

#### `namedInputs`

Define sets of files that affect cache invalidation.

**Common Inputs**:

- `default` - All project files + shared globals
- `production` - All files except tests
- `sharedGlobals` - Workspace-wide files that affect all projects

**Example**:

```json
{
  "namedInputs": {
    "default": ["{projectRoot}/**/*", "sharedGlobals"],
    "production": [
      "default",
      "!{projectRoot}/**/*.spec.ts",
      "!{projectRoot}/**/*.test.ts"
    ],
    "sharedGlobals": ["{workspaceRoot}/tsconfig.base.json"]
  }
}
```

**Glob Patterns**:

- `{projectRoot}/**/*` - All files in project
- `!{projectRoot}/**/*.test.ts` - Exclude test files
- `{workspaceRoot}/tsconfig.base.json` - Workspace config file

#### `generators`

Configuration for Nx generators (not used in this project).

**Value**: `{}`

#### `plugins`

Configuration for Nx plugins (not used in this project).

**Value**: `[]`

## `project.json` (Project Configuration)

### Location

Per-project:

- `apps/[app-name]/project.json`
- `libs/[lib-name]/project.json`

### Complete Example (Hugo App)

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

### Complete Example (TypeScript Library)

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
        "command": "node --import tsx --test libs/ts-utils/src/**/*.test.ts",
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

### Field Reference

#### `name`

Project name used by Nx CLI.

**Type**: string

**Format**: For apps/libs, typically matches folder name

**Examples**:

- `"ose-platform-web"` (app)
- `"ts-utils"` (lib)

#### `sourceRoot`

Location of source code.

**Type**: string

**Format**: Relative path from workspace root

**Examples**:

- `"apps/ose-platform-web"` (app root)
- `"libs/ts-utils/src"` (lib source)

#### `projectType`

Type of project.

**Type**: string

**Values**:

- `"application"` - Deployable app
- `"library"` - Reusable lib

#### `targets`

Available Nx tasks for this project.

**Type**: object

**Structure**:

```json
{
	"[target-name]": {
		"executor": "nx:run-commands",
		"options": { ... },
		"outputs": [ ... ],
		"dependsOn": [ ... ]
	}
}
```

### Target Configuration

#### `executor`

Executor to run the target.

**Type**: string

**Value**: `"nx:run-commands"` (always - no plugins)

#### `options`

Executor options.

**Fields**:

- `command` (string): Shell command to execute
  - Required
  - Examples: `"next build"`, `"tsc -p tsconfig.json"`
- `cwd` (string): Working directory
  - Optional (defaults to workspace root)
  - Examples: `"apps/ose-platform-web"`, `"."`

**Example**:

```json
{
  "options": {
    "command": "next build",
    "cwd": "apps/ose-platform-web"
  }
}
```

#### `outputs`

Output directories to cache.

**Type**: string[]

**Format**: Glob patterns relative to workspace root

**Examples**:

- `["{projectRoot}/dist"]` - Library build output
- `["{projectRoot}/.next"]` - Next.js build output
- `["{projectRoot}/build"]` - Custom build output

**Tokens**:

- `{projectRoot}` - Project directory
- `{workspaceRoot}` - Workspace root

#### `dependsOn`

Task dependencies.

**Type**: string[]

**Format**: Array of target names

**Patterns**:

- `["^build"]` - Run `build` on all dependencies first
- `["build"]` - Run own `build` target first
- `["lint", "test"]` - Run multiple targets first

**Example**:

```json
{
  "serve": {
    "dependsOn": ["build"]
  }
}
```

## `tsconfig.base.json` (TypeScript Base Configuration)

### Location

`/tsconfig.base.json` (repository root)

### Complete Example

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

### Field Reference

#### `compilerOptions.paths`

Path mappings for TypeScript imports.

**Format**: Wildcard pattern matching library names

**Pattern**: `"@open-sharia-enterprise/ts-*": ["libs/ts-*/src/index.ts"]`

**How it works**:

- Import: `import { greet } from "@open-sharia-enterprise/ts-utils"`
- Resolves to: `libs/ts-utils/src/index.ts`

**Future languages** (not yet implemented):

```json
{
  "paths": {
    "@open-sharia-enterprise/ts-*": ["libs/ts-*/src/index.ts"],
    "@open-sharia-enterprise/java-*": ["libs/java-*/src/main/java"],
    "@open-sharia-enterprise/kt-*": ["libs/kt-*/src/main/kotlin"],
    "@open-sharia-enterprise/py-*": ["libs/py-*/src"]
  }
}
```

#### `compilerOptions.target`

ECMAScript target version.

**Value**: `"ES2022"`

**Why ES2022**: Modern features, widely supported in Node.js 24.x

#### `compilerOptions.strict`

Enable all strict type checking.

**Value**: `true`

**Enables**:

- `strictNullChecks`
- `strictFunctionTypes`
- `strictBindCallApply`
- `strictPropertyInitialization`
- `noImplicitAny`
- `noImplicitThis`
- `alwaysStrict`

## `package.json` (Workspace Manifest)

### Location

`/package.json` (repository root)

### Relevant Fields

#### `workspaces`

npm workspaces configuration.

**Type**: string[]

**Value**: `["apps/*", "libs/*"]`

**Purpose**: Enables dependency hoisting and workspace features

#### `scripts`

Nx wrapper scripts.

**Common Scripts**:

```json
{
  "scripts": {
    "build": "nx run-many -t build",
    "test": "nx run-many -t test",
    "lint": "nx run-many -t lint",
    "affected:build": "nx affected -t build",
    "affected:test": "nx affected -t test",
    "affected:lint": "nx affected -t lint",
    "graph": "nx graph",
    "nx": "nx"
  }
}
```

#### `volta`

Node.js and npm version pinning.

**Type**: object

**Fields**:

- `node` (string): Node.js version
- `npm` (string): npm version

**Example**:

```json
{
  "volta": {
    "node": "24.11.1",
    "npm": "11.6.3"
  }
}
```

## `.nxignore` (Nx Ignore File)

### Location

`/.nxignore` (repository root)

### Purpose

Exclude files and directories from Nx processing.

### Complete Example

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

### Format

- One pattern per line
- Glob patterns supported
- Comments start with `#`

## Environment Variables

### Nx Environment Variables

#### `NX_SKIP_NX_CACHE`

Skip Nx cache.

**Usage**:

```bash
NX_SKIP_NX_CACHE=true nx build ose-platform-web
```

#### `NX_DAEMON`

Enable/disable Nx daemon.

**Usage**:

```bash
NX_DAEMON=false nx build ose-platform-web
```

## Related Documentation

- [How to Add New App](../how-to/hoto__add-new-app.md)
- [How to Add New Library](../how-to/hoto__add-new-lib.md)
- [How to Run Nx Commands](../how-to/hoto__run-nx-commands.md)
- [Monorepo Structure Reference](./re__monorepo-structure.md)
