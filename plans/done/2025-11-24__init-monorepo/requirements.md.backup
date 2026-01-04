# Requirements: Nx Monorepo Initialization

## Objectives

### Primary Objectives

1. **Establish Nx as Monorepo Build System**
   - Install and configure Nx as a dev dependency
   - Set up workspace configuration without using any plugins
   - Enable core Nx features: caching, affected detection, dependency graph

2. **Create Application Workspace Structure**
   - Create `apps/` folder for deployable application projects
   - Establish naming conventions and organization patterns
   - Document how to add new applications manually

3. **Create Library Workspace Structure**
   - Create `libs/` folder with flat organization
   - Use language-prefix naming convention: `ts-`, `java-`, `kt-`, `py-`
   - Design for future polyglot support (Java, Kotlin, TypeScript, Python)
   - Current implementation: TypeScript libraries only
   - Establish dependency rules and import patterns
   - Document how to add new libraries manually

4. **Enable Efficient Task Execution**
   - Configure task runner with caching for builds, tests, and linting
   - Set up dependency-based task execution
   - Enable affected detection for optimized CI/CD

5. **Provide Clear Documentation**
   - Update CLAUDE.md with monorepo structure
   - Create how-to guides for common workflows
   - Create reference documentation for configuration

### Secondary Objectives

1. **Create Reusable Templates**
   - Provide template structures for apps and libs
   - Document template usage for consistency

2. **Validate Setup with Sample Projects**
   - Create Next.js app (`apps/demo-ts-fe`) demonstrating structure
   - Create TypeScript lib (`libs/ts-demo-libs`) demonstrating library patterns
   - Demonstrate cross-project imports (app importing lib)

## User Stories

### Story 1: Install and Configure Nx

**As a** developer
**I want** Nx installed and configured in the workspace
**So that** I can use Nx commands to manage the monorepo

**Acceptance Criteria** (Gherkin):

```gherkin
Scenario: Nx is successfully installed
  Given the workspace has a valid package.json
  When I run "npm install -D nx@latest"
  Then Nx is installed as a dev dependency
  And running "npx nx --version" returns the version number
  And no Nx plugins are installed

Scenario: Nx workspace is configured
  Given Nx is installed
  When I create nx.json configuration file
  And I configure affected detection to use "main" as default base
  And I configure task caching for build, test, and lint operations
  Then running "nx graph" generates a dependency graph
  And task caching is enabled for configured operations
```

### Story 2: Create Apps Folder Structure

**As a** developer
**I want** a dedicated `apps/` folder for applications
**So that** I can organize deployable projects separately from shared libraries

**Acceptance Criteria** (Gherkin):

```gherkin
Scenario: Apps folder is created and documented
  Given the workspace root directory exists
  When I create the "apps/" directory
  And I create "apps/README.md" documenting purpose and conventions
  Then the "apps/" directory exists at the root level
  And "apps/README.md" explains naming conventions
  And "apps/README.md" explains how to add new applications
  And "apps/README.md" explains that apps consume libs but should not be imported by other apps

Scenario: Next.js app can be added to apps folder
  Given the "apps/" directory exists
  When I create "apps/demo-ts-fe/" directory
  And I initialize Next.js with TypeScript
  And I add "project.json" with Nx configuration
  And I configure TypeScript to extend workspace config
  Then the app structure is complete and documented
  And the app can be run using "nx dev demo-ts-fe"
```

### Story 3: Create Libs Folder Structure (Flat, Multi-Language)

**As a** developer
**I want** a dedicated `libs/` folder with flat organization and language prefixes
**So that** I can create libraries in multiple languages with clear naming

**Acceptance Criteria** (Gherkin):

```gherkin
Scenario: Libs folder is created with flat structure
  Given the workspace root directory exists
  When I create the "libs/" directory
  And I create "libs/README.md" documenting organization and conventions
  Then the "libs/" directory exists at the root level
  And "libs/README.md" explains flat organization
  And "libs/README.md" explains naming convention "[language-prefix]-[name]"
  And "libs/README.md" documents planned languages (TypeScript, Java, Kotlin, Python)
  And "libs/README.md" notes current focus is TypeScript only
  And "libs/README.md" provides examples: "ts-demo-libs", "ts-utils", "ts-components"

Scenario: TypeScript library can be added to libs folder
  Given the "libs/" directory exists
  When I create "libs/ts-demo-libs/" directory
  And I add "src/index.ts" with public API exports
  And I add "src/lib/" directory for implementation
  And I add "project.json" with Nx configuration
  And I add "tsconfig.json" for TypeScript
  And I add "package.json" for lib-specific dependencies
  Then the library structure is complete and documented
  And the library can be built using "nx build ts-demo-libs"
  And the library can be imported from apps using proper path
```

### Story 4: Configure Task Execution and Caching

**As a** developer
**I want** task execution with caching enabled
**So that** builds and tests run faster by reusing previous results

**Acceptance Criteria** (Gherkin):

```gherkin
Scenario: Task caching is configured
  Given nx.json exists
  When I configure "tasksRunnerOptions" with default runner
  And I set "cacheableOperations" to ["build", "test", "lint"]
  And I configure "targetDefaults" for build and test
  Then running "nx build [project]" twice uses cache on second run
  And running "nx test [project]" twice uses cache on second run
  And cache output displays "[local cache]" or "[remote cache]"

Scenario: Task dependencies are configured
  Given nx.json exists with targetDefaults
  When I configure build target with "dependsOn": ["^build"]
  And I configure test target with "dependsOn": ["build"]
  Then building an app builds its library dependencies first
  And running tests builds the project first
  And task execution order respects dependencies
```

### Story 5: Enable Affected Detection

**As a** developer
**I want** affected detection to identify changed projects
**So that** I only build and test what has changed

**Acceptance Criteria** (Gherkin):

```gherkin
Scenario: Affected detection identifies changed projects
  Given a workspace with multiple apps and libs
  And nx.json is configured with "defaultBase": "main"
  When I make a change to a library file
  And I run "nx affected:build"
  Then only the changed library and its dependents are built
  And unchanged projects are skipped
  And the output shows which projects were affected

Scenario: Affected graph visualizes changes
  Given a workspace with multiple apps and libs
  When I make a change to a library file
  And I run "nx affected:graph"
  Then a dependency graph is generated
  And affected projects are highlighted
  And the graph shows dependency relationships
```

### Story 6: Cross-Project Imports Work Correctly

**As a** developer
**I want** apps to import from libs seamlessly
**So that** I can share code across projects

**Acceptance Criteria** (Gherkin):

```gherkin
Scenario: Next.js app imports and uses library
  Given "apps/demo-ts-fe/" exists
  And "libs/ts-demo-libs/" exists
  And ts-demo-libs exports a function "greet(name: string)"
  When I import { greet } from "@open-sharia-enterprise/ts-demo-libs" in demo-ts-fe
  And I use greet("Next.js") in the Next.js page component
  And I run "nx build demo-ts-fe"
  Then the build succeeds without errors
  And the app can use the library function
  And the dependency is shown in "nx graph"

Scenario: Circular dependencies are prevented
  Given "libs/ts-auth/" exists
  And "libs/ts-users/" exists
  When ts-auth imports from ts-users
  And I attempt to import from ts-auth in ts-users
  Then Nx detects the circular dependency
  And the build fails with a clear error message
  Or a warning is displayed (depending on configuration)
```

## Functional Requirements

### REQ-001: Nx Installation and Configuration

**Priority**: High
**User Stories**: Story 1

- Install Nx as a dev dependency (latest stable version)
- Create `nx.json` workspace configuration file
- Configure affected detection with `"defaultBase": "main"`
- Configure task runner with caching for build, test, lint operations
- Update `package.json` with workspace field and Nx scripts
- Create `.nxignore` for paths to exclude from Nx

**Acceptance Criteria** (Gherkin):

```gherkin
Scenario: Nx is properly configured
  Given all Nx configuration files are created
  When I run "npx nx --version"
  Then the Nx version number is displayed
  And no Nx plugins are listed in package.json dependencies
  And "npm ls | grep @nx" shows only "nx" package, no plugins
```

### REQ-002: Apps Folder Structure

**Priority**: High
**User Stories**: Story 2

- Create `apps/` directory at repository root
- Create `apps/README.md` documenting apps folder purpose
- Document app naming convention: `[domain]-[type]` (e.g., `api-gateway`, `admin-dashboard`)
- Document required files for each app: `src/`, `project.json`, `tsconfig.json`, `package.json`
- Establish that apps are consumers of libs and should not import other apps

**Acceptance Criteria** (Gherkin):

```gherkin
Scenario: Apps folder structure is complete
  Given the workspace root exists
  When the setup is complete
  Then "apps/" directory exists
  And "apps/README.md" documents naming conventions
  And "apps/README.md" documents required files
  And "apps/README.md" explains app characteristics
```

### REQ-003: Libs Folder Structure (Flat, Polyglot-Ready)

**Priority**: High
**User Stories**: Story 3

- Create `libs/` directory at repository root with flat structure
- Use language-prefix naming convention:
  - `ts-*` - TypeScript libraries (e.g., `ts-demo-libs`, `ts-utils`, `ts-components`)
  - `java-*` - Java libraries (future, e.g., `java-api`, `java-processor`)
  - `kt-*` - Kotlin libraries (future, e.g., `kt-services`, `kt-utils`)
  - `py-*` - Python libraries (future, e.g., `py-ml`, `py-data`)
- **Current scope**: TypeScript libraries only
- **Future scope**: Java, Kotlin, Python support
- Create `libs/README.md` documenting library organization
- Document lib naming convention: `[lang-prefix]-[name]` (e.g., `ts-demo-libs`)
- Document required files for TypeScript libs
- Design structure to support multiple languages in future

**Acceptance Criteria** (Gherkin):

```gherkin
Scenario: Libs folder structure with flat organization is complete
  Given the workspace root exists
  When the setup is complete
  Then "libs/" directory exists
  And "libs/README.md" documents flat organization
  And "libs/README.md" documents language-prefix naming convention
  And "libs/README.md" lists planned languages: TypeScript, Java, Kotlin, Python
  And "libs/README.md" notes current implementation is TypeScript only
  And "libs/README.md" provides TypeScript examples
```

### REQ-004: Manual Project Configuration

**Priority**: High
**User Stories**: Story 2, Story 3

- Each project has a `project.json` file with Nx configuration
- `project.json` uses `nx:run-commands` executor (no plugin executors)
- Build target runs TypeScript compiler directly: `tsc -p [project]/tsconfig.json`
- Test target runs Node.js test runner: `node --test [project]/src/**/*.test.ts`
- Each `project.json` specifies `sourceRoot`, `projectType`, `targets`

**Acceptance Criteria** (Gherkin):

```gherkin
Scenario: Project configuration uses run-commands executor
  Given a project exists in apps/ or libs/
  When I examine its project.json file
  Then the build target uses "executor": "nx:run-commands"
  And the build command directly invokes "tsc"
  And the test target uses "executor": "nx:run-commands"
  And the test command directly invokes "node --test"
  And no plugin executors are used
```

### REQ-005: Task Execution and Caching

**Priority**: High
**User Stories**: Story 4

- Configure `nx.json` with task runner options
- Enable caching for `build`, `test`, `lint` operations
- Configure `targetDefaults` for build and test tasks
- Build tasks depend on dependencies' build tasks: `"dependsOn": ["^build"]`
- Test tasks depend on build tasks: `"dependsOn": ["build"]`

**Acceptance Criteria** (Gherkin):

```gherkin
Scenario: Task caching improves performance
  Given a project with build and test targets
  When I run "nx build [project]" for the first time
  Then the build executes and completes
  And the output is cached
  When I run "nx build [project]" again without changes
  Then the cached result is used
  And the build completes instantly with "[local cache]" message
```

### REQ-006: Affected Detection

**Priority**: High
**User Stories**: Story 5

- Configure `nx.json` with `"affected": { "defaultBase": "main" }`
- `nx affected:build` only builds changed projects and their dependents
- `nx affected:test` only tests changed projects
- `nx affected:graph` visualizes affected projects

**Acceptance Criteria** (Gherkin):

```gherkin
Scenario: Affected commands only run for changed projects
  Given a workspace with multiple projects
  When I change a single library file
  And I run "nx affected:build"
  Then only the changed library and apps that depend on it are built
  And other projects are skipped
  And the command output lists affected projects
```

### REQ-007: Workspace Scripts

**Priority**: Medium
**User Stories**: Story 1

- Add npm scripts to root `package.json` for common Nx commands:
  - `"build": "nx run-many -t build"` - Build all projects
  - `"test": "nx run-many -t test"` - Test all projects
  - `"graph": "nx graph"` - View dependency graph
  - `"affected:build": "nx affected:build"` - Build affected
  - `"affected:test": "nx affected:test"` - Test affected

**Acceptance Criteria** (Gherkin):

```gherkin
Scenario: Npm scripts provide convenient Nx access
  Given the root package.json is configured
  When I run "npm run build"
  Then all projects are built using Nx
  When I run "npm run graph"
  Then the dependency graph opens in browser
```

### REQ-008: Documentation

**Priority**: High
**User Stories**: All stories

- Update `CLAUDE.md` with monorepo structure section
- Create `docs/how-to/hoto__add-new-app.md` - Step-by-step guide for adding apps
- Create `docs/how-to/hoto__add-new-lib.md` - Step-by-step guide for adding libs
- Create `docs/how-to/hoto__run-nx-commands.md` - Common Nx workflows
- Create `docs/reference/re__monorepo-structure.md` - Complete structure reference
- Create `docs/reference/re__nx-configuration.md` - Configuration file reference

**Acceptance Criteria** (Gherkin):

```gherkin
Scenario: Documentation is complete and accurate
  Given all documentation files are created
  When a new developer reads the documentation
  Then they can add a new app by following the how-to guide
  And they can add a new lib by following the how-to guide
  And they understand the monorepo structure from reference docs
  And they can run common Nx commands from the workflow guide
```

## Non-Functional Requirements

### REQ-NFR-001: Performance

**Goal**: Fast builds and tests through caching

- Initial build of all projects completes within reasonable time (baseline to be established)
- Cached builds complete in < 1 second
- Affected builds only rebuild changed projects and dependents
- Dependency graph generation completes in < 5 seconds

**Acceptance Criteria** (Gherkin):

```gherkin
Scenario: Build performance meets targets
  Given a project has been built once
  When I run the build again without changes
  Then the build completes in under 1 second using cache
  And the output shows "[local cache]"
```

### REQ-NFR-002: Maintainability

**Goal**: Simple, transparent configuration without plugins

- All configuration is in standard JSON files
- No plugin-specific DSL or custom executors
- Build commands are standard TypeScript/Node.js commands
- Configuration can be understood by examining JSON files

**Acceptance Criteria** (Gherkin):

```gherkin
Scenario: Configuration is transparent and maintainable
  Given the Nx workspace is configured
  When I examine project.json files
  Then all executors are "nx:run-commands"
  And all commands use standard tools (tsc, node)
  And no custom plugins are referenced
```

### REQ-NFR-003: Compatibility

**Goal**: Works with existing tooling and conventions

- Volta version pinning (Node.js 24.11.1, npm 11.6.2) remains functional
- Git hooks (Husky, lint-staged, commitlint) remain functional
- Prettier formatting continues to work
- Conventional Commits validation continues to work

**Acceptance Criteria** (Gherkin):

```gherkin
Scenario: Existing tooling remains functional
  Given the Nx monorepo is set up
  When I run "node --version"
  Then it shows "24.11.1" (Volta-managed)
  When I create a commit
  Then Prettier formats staged files
  And commitlint validates the commit message
  And Husky hooks execute correctly
```

### REQ-NFR-004: Scalability

**Goal**: Support growing number of apps and libs

- Structure supports adding new apps without limit
- Structure supports adding new libs across all scopes
- Dependency graph remains manageable and navigable
- Build times scale linearly (not exponentially) with project count

**Acceptance Criteria** (Gherkin):

```gherkin
Scenario: Structure supports growth
  Given the monorepo is set up
  When 10 apps and 20 libs are added
  Then all apps and libs can be built successfully
  And the dependency graph remains readable
  And affected detection continues to work efficiently
```

## Constraints

### Technical Constraints

1. **No Nx Plugins** - Explicitly prohibited by requirements
   - Cannot use `@nx/react`, `@nx/next`, `@nx/node`, etc.
   - Cannot use plugin generators or executors
   - Must use `nx:run-commands` executor for all tasks

2. **Node.js Version** - Locked by Volta
   - Must use Node.js 24.11.1 (LTS)
   - Must use npm 11.6.2
   - Volta configuration must remain in `package.json`

3. **Existing Git Hooks** - Must not break
   - Husky pre-commit hook must continue to work
   - Prettier formatting must continue to work
   - Commitlint validation must continue to work

4. **TypeScript** - Primary language
   - All apps and libs use TypeScript
   - Standard `tsc` compiler for builds
   - No custom build tools or bundlers (for now)

### Process Constraints

1. **Trunk-Based Development** - Work on `main` branch
   - No long-lived feature branches
   - Commit directly to `main` or short-lived branches < 2 days
   - Follow conventional commits format

2. **Documentation Standards** - Follow Diátaxis framework
   - How-to guides for procedures
   - Reference docs for structure and configuration
   - Clear, actionable writing

### Business Constraints

1. **Timeline** - Foundation must be established before app development
2. **Team Knowledge** - Team must understand monorepo concepts
3. **Incremental Adoption** - Can be implemented incrementally (sample projects first)

## Assumptions

1. **Team Buy-In** - Team agrees to monorepo architecture
2. **Manual Setup** - Team accepts manual project creation (no generators)
3. **TypeScript** - All projects will use TypeScript
4. **npm Workspaces** - npm workspaces feature is acceptable for dependency management
5. **Future Growth** - More apps and libs will be added over time

## Out of Scope

### Explicitly Excluded

1. **Nx Plugins** - Per requirements
   - No `@nx/react`, `@nx/next`, `@nx/node`, etc.
   - No plugin generators
   - No plugin executors

2. **Specific Applications** - Separate plans
   - No concrete app implementations
   - No business logic
   - Sample apps only for validation, then removed

3. **CI/CD Integration** - Future work
   - GitHub Actions workflows (future plan)
   - Deployment pipelines (future plan)
   - Automated releases (future plan)

4. **Advanced Nx Features** - May add later
   - Module federation
   - Nx Cloud and remote caching
   - Distributed task execution

5. **Build Tools Beyond TypeScript** - Future work
   - Webpack/Vite configuration
   - Bundling and optimization
   - Asset processing

6. **Testing Infrastructure** - Future work
   - Test framework setup (Jest, Vitest, etc.)
   - E2E testing framework
   - Test coverage reporting

7. **Linting and Formatting** - Keep existing
   - No new ESLint configuration
   - No new Prettier configuration
   - Keep existing tools as-is

8. **Database and Infrastructure** - Separate plans
   - Database setup
   - External services
   - Authentication systems
   - API infrastructure
