# Delivery Plan: Nx Monorepo Initialization

## Overview

**Delivery Type**: Single PR

**Git Workflow**: Work on `feat/init-monorepo` branch, merge to `main` via PR

**Summary**: Initialize Nx-based monorepo with apps/ and libs/ folder structure, manual project configuration (no plugins), and comprehensive documentation.

## Implementation Phases

### Phase 1: Nx Installation and Base Configuration

**Status**: Not Started

**Goal**: Install Nx and create workspace-level configuration files

**Implementation Steps**:

- [ ] Install Nx as dev dependency: `npm install -D nx@latest`
- [ ] Verify installation: `npx nx --version`
- [ ] Create `nx.json` with workspace configuration:
  - Configure `affected.defaultBase`: `"main"`
  - Configure `tasksRunnerOptions` with caching
  - Configure `targetDefaults` for build, test, lint
  - Add empty `generators` and `plugins` arrays
- [ ] Update `package.json`:
  - Add `workspaces`: `["apps/*", "libs/*"]`
  - Add npm scripts: build, test, lint, affected:\*, graph
  - Verify Volta pinning remains: `"node": "24.11.1"`, `"npm": "11.6.2"`
- [ ] Create `tsconfig.base.json`:
  - Base TypeScript compiler options
  - Path mappings for language-prefixed libraries: `@open-sharia/ts-*`
  - Target ES2022, strict mode enabled
- [ ] Create `.nxignore`:
  - Exclude `docs/`, `plans/`, `*.md`
  - Exclude IDE and temporary files
- [ ] Update `.gitignore`:
  - Add `dist/` to ignore build outputs
  - Add `.nx/` to ignore Nx cache

**Validation Checklist**:

- [ ] `npx nx --version` displays Nx version number
- [ ] `npm ls nx` shows only `nx` package (no plugins)
- [ ] `npm ls | grep @nx` shows no Nx plugins installed
- [ ] `node --version` shows `24.11.1` (Volta managed)
- [ ] `npm --version` shows `11.6.2` (Volta managed)
- [ ] All JSON files are valid (no syntax errors)
- [ ] TypeScript compiler options are valid
- [ ] Volta configuration intact in package.json

**Acceptance Criteria**:

- [ ] All user stories related to Nx installation pass Gherkin tests
- [ ] Nx is installed and configured without plugins
- [ ] Existing Volta and git hooks still work

---

### Phase 2: Folder Structure Setup

**Status**: Not Started

**Goal**: Create apps/ and libs/ folders with flat organization and polyglot support

**Implementation Steps**:

- [ ] Create `apps/` directory: `mkdir -p apps`
- [ ] Create `apps/README.md` documenting:
  - Purpose: deployable applications (any language)
  - Naming convention: `[domain]-[type]`
  - Required files vary by language (document for each)
  - Rule: apps don't import other apps
  - Examples: api-gateway, admin-dashboard, payment-processor
- [ ] Create `libs/` directory: `mkdir -p libs`
- [ ] Create `libs/README.md` documenting:
  - Purpose: reusable libraries (polyglot-ready, TypeScript current focus)
  - Flat structure organization (no nested scopes)
  - Naming convention: `[lang-prefix]-[name]`
  - Language prefixes: ts- (current), java-, kt-, py- (future)
  - Current implementation: TypeScript libraries only
  - Planned languages: Java, Kotlin, Python (future scope)
  - Dependency guidelines (no circular deps)
  - Required files for TypeScript libs:
    - src/index.ts, src/lib/, project.json, tsconfig.json, package.json
  - Examples: ts-demo-libs, ts-utils, ts-components, ts-hooks

**Validation Checklist**:

- [ ] `apps/` directory exists at repository root
- [ ] `apps/README.md` exists and documents conventions
- [ ] `libs/` directory exists at repository root (flat, no subdirectories yet)
- [ ] `libs/README.md` documents flat organization
- [ ] `libs/README.md` documents language prefixes (ts-, java-, kt-, py-)
- [ ] `libs/README.md` notes current scope is TypeScript only
- [ ] `libs/README.md` includes TypeScript examples
- [ ] README files follow markdown best practices
- [ ] README files include clear examples

**Acceptance Criteria**:

- [ ] All user stories related to folder structure pass Gherkin tests
- [ ] Folder structure matches architecture diagrams (flat libs/)
- [ ] Documentation is complete and accurate

---

### Phase 3: Next.js Demo App Creation

**Status**: Not Started

**Goal**: Create Next.js app (`demo-ts-fe`) to validate app structure

**Implementation Steps**:

- [ ] Create app directory: `mkdir -p apps/demo-ts-fe`
- [ ] Initialize Next.js in `apps/demo-ts-fe`:
  ```bash
  cd apps/demo-ts-fe
  npx create-next-app@latest . --typescript --app --no-src-dir --tailwind --eslint --import-alias "@/*"
  cd ../..
  ```
- [ ] Create `apps/demo-ts-fe/project.json`:
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
- [ ] Update `apps/demo-ts-fe/tsconfig.json` to extend workspace tsconfig:
  - Add `"extends": "../../tsconfig.base.json"` at the top
  - Keep Next.js-specific compiler options
- [ ] Create `apps/demo-ts-fe/README.md`:
  - Document: Next.js demo app that consumes ts-demo-libs
  - Document: `nx dev demo-ts-fe` to start dev server
  - Document: `nx build demo-ts-fe` to build for production
- [ ] Test dev server: `nx dev demo-ts-fe`
- [ ] Test build: `nx build demo-ts-fe`
- [ ] Verify Next.js runs at http://localhost:3000

**Validation Checklist**:

- [ ] App directory structure is complete
- [ ] All required Next.js files exist
- [ ] `nx dev demo-ts-fe` starts dev server
- [ ] `nx build demo-ts-fe` succeeds
- [ ] Build creates `.next/` directory
- [ ] Next.js app runs without errors
- [ ] TypeScript compiles without errors
- [ ] No Next.js warnings

**Acceptance Criteria**:

- [ ] All user stories related to app creation pass Gherkin tests
- [ ] Next.js app demonstrates correct structure
- [ ] App can be developed, built, and served successfully

---

### Phase 4: Demo TypeScript Library Creation

**Status**: Not Started

**Goal**: Create TypeScript library (`ts-demo-libs`) to validate flat lib structure

**Implementation Steps**:

- [ ] Create lib directory structure:
  - `mkdir -p libs/ts-demo-libs/src/lib`
- [ ] Create `libs/ts-demo-libs/src/lib/greet.ts`:
  ```typescript
  export function greet(name: string): string {
    return `Hello, ${name}!`;
  }
  ```
- [ ] Create `libs/ts-demo-libs/src/lib/greet.test.ts`:

  ```typescript
  import { test } from "node:test";
  import assert from "node:assert";
  import { greet } from "./greet";

  test("greet returns greeting message", () => {
    assert.strictEqual(greet("World"), "Hello, World!");
  });
  ```

- [ ] Create `libs/ts-demo-libs/src/index.ts`:
  ```typescript
  export { greet } from "./lib/greet";
  ```
- [ ] Create `libs/ts-demo-libs/package.json`:
  ```json
  {
    "name": "@open-sharia/ts-demo-libs",
    "version": "0.1.0",
    "private": true
  }
  ```
- [ ] Create `libs/ts-demo-libs/project.json`:
  - name: `"ts-demo-libs"`
  - sourceRoot: `"libs/ts-demo-libs/src"`
  - projectType: `"library"`
  - targets: build (tsc), test (node --test)
- [ ] Create `libs/ts-demo-libs/tsconfig.json`:
  - Extends `../../tsconfig.base.json`
  - Include: `["src/**/*"]`
- [ ] Create `libs/ts-demo-libs/tsconfig.build.json`:
  - Extends `./tsconfig.json`
  - compilerOptions: `outDir: "dist"`, `rootDir: "src"`
  - Exclude tests
- [ ] Create `libs/ts-demo-libs/README.md`:
  - Document lib purpose (demo library for Next.js app)
  - Document public API
  - Document how to use from Next.js app
- [ ] Test lib build: `nx build ts-demo-libs`
- [ ] Test lib tests: `nx test ts-demo-libs`
- [ ] Verify build output created in `libs/ts-demo-libs/dist/`

**Validation Checklist**:

- [ ] Lib directory structure is complete
- [ ] All required files exist and are valid
- [ ] `nx build ts-demo-libs` succeeds
- [ ] Build creates `libs/ts-demo-libs/dist/`
- [ ] `nx test ts-demo-libs` succeeds
- [ ] All tests pass
- [ ] TypeScript compiles without errors
- [ ] Public API exports correctly from index.ts

**Acceptance Criteria**:

- [ ] All user stories related to lib creation pass Gherkin tests
- [ ] Demo lib demonstrates correct flat structure
- [ ] Lib can be built and tested successfully
- [ ] Lib is ready to be consumed by Next.js app

---

### Phase 5: Cross-Project Integration

**Status**: Not Started

**Goal**: Validate Next.js app can import and use lib (cross-project dependencies)

**Implementation Steps**:

- [ ] Update `apps/demo-ts-fe/app/page.tsx` to import lib:

  ```typescript
  import { greet } from "@open-sharia/ts-demo-libs";

  export default function Home() {
    const message = greet("Next.js");
    return <div>{message}</div>;
  }
  ```

- [ ] Restart dev server: `nx dev demo-ts-fe`
  - Verify lib is accessible
  - Verify page displays "Hello, Next.js!"
- [ ] Build both projects: `nx build demo-ts-fe`
  - Verify lib is built first (dependency resolution)
  - Verify Next.js build succeeds
- [ ] Test dependency graph: `nx graph`
  - Verify demo-ts-fe -> ts-demo-libs dependency shown
- [ ] Make change to lib, rebuild app
  - Verify affected detection works

**Validation Checklist**:

- [ ] Next.js app successfully imports from lib using path mapping
- [ ] TypeScript resolves import correctly in Next.js
- [ ] `nx build demo-ts-fe` builds lib first, then app
- [ ] Next.js app displays message from lib correctly
- [ ] `nx graph` shows demo-ts-fe -> ts-demo-libs dependency
- [ ] Dependency graph visualizes correctly in browser
- [ ] No import errors or TypeScript errors
- [ ] Next.js hot reload works with lib changes

**Acceptance Criteria**:

- [ ] All user stories related to cross-project imports pass Gherkin tests
- [ ] Next.js app successfully uses lib functionality
- [ ] Dependency graph shows correct relationships
- [ ] Integration between app and lib works seamlessly

---

### Phase 6: Nx Features Validation

**Status**: Not Started

**Goal**: Validate core Nx features (caching, affected, run-many)

**Implementation Steps**:

- [ ] **Test Task Caching**:
  - Run `nx build demo-ts-fe` (first build)
  - Run `nx build demo-ts-fe` again (should use cache)
  - Verify second build shows "[local cache]"
  - Verify second build completes in < 1 second
- [ ] **Test Affected Detection**:
  - Make change to `libs/ts-demo-libs/src/lib/greet.ts`
  - Run `nx affected:build`
  - Verify both lib and app are built
  - Revert change
  - Make change to `apps/demo-ts-fe/app/page.tsx` only
  - Run `nx affected:build`
  - Verify only app is built (lib skipped)
- [ ] **Test Affected Graph**:
  - Make change to lib
  - Run `nx affected:graph`
  - Verify graph highlights affected projects
- [ ] **Test Run-Many**:
  - Run `nx run-many -t build`
  - Verify all projects build
  - Run `nx run-many -t test`
  - Verify all projects test
- [ ] **Test Dependency Resolution**:
  - Verify build order respects dependencies
  - Lib builds before app

**Validation Checklist**:

- [ ] Task caching works correctly
- [ ] Second build uses cache (< 1s)
- [ ] Cache message "[local cache]" displayed
- [ ] Affected detection identifies changed lib and dependent app
- [ ] Affected detection skips unchanged projects
- [ ] `nx affected:graph` visualizes affected projects
- [ ] `nx run-many -t build` builds all projects
- [ ] `nx run-many -t test` tests all projects
- [ ] Dependency-based build order works correctly

**Acceptance Criteria**:

- [ ] All user stories related to Nx features pass Gherkin tests
- [ ] Caching improves build performance
- [ ] Affected detection works as expected

---

### Phase 7: Documentation

**Status**: Not Started

**Goal**: Create comprehensive documentation for monorepo usage

**Implementation Steps**:

- [ ] Update `CLAUDE.md`:
  - Add "Monorepo Structure" section
  - Document apps/ and libs/ folders
  - Document flat library structure with language prefixes
  - Link to how-to guides and references
- [ ] Create `docs/how-to/ht__add-new-app.md`:
  - Step-by-step guide for creating apps
  - Include all required files
  - Provide template examples
  - Document naming conventions
- [ ] Create `docs/how-to/ht__add-new-lib.md`:
  - Step-by-step guide for creating libs
  - Include all required files
  - Explain language prefix selection (ts-, java-, kt-, py-)
  - Document dependency rules
  - Provide template examples
- [ ] Create `docs/how-to/ht__run-nx-commands.md`:
  - Common Nx commands and workflows
  - Build, test, lint, serve
  - Affected detection usage
  - Dependency graph visualization
  - Caching behavior
- [ ] Create `docs/reference/re__monorepo-structure.md`:
  - Complete structure reference
  - Folder organization
  - File structure for apps and libs
  - Configuration file formats
- [ ] Create `docs/reference/re__nx-configuration.md`:
  - nx.json configuration reference
  - project.json configuration reference
  - tsconfig.base.json path mappings
  - Task runner options
- [ ] Update root `README.md`:
  - Add monorepo overview section
  - Link to documentation
  - Quick start guide
- [ ] Verify all documentation follows Diátaxis framework:
  - How-to guides are problem-oriented
  - Reference docs are technical and comprehensive
  - Links work correctly

**Validation Checklist**:

- [ ] `CLAUDE.md` updated with monorepo section
- [ ] All how-to guides created and complete
- [ ] All reference docs created and complete
- [ ] Root README.md updated
- [ ] All documentation follows Diátaxis framework
- [ ] All links work correctly (no broken links)
- [ ] Examples are accurate and tested
- [ ] Documentation follows file naming convention
- [ ] TAB indentation used in docs/ files (Logseq compatibility)

**Acceptance Criteria**:

- [ ] All user stories related to documentation pass Gherkin tests
- [ ] New developers can follow guides to add apps and libs
- [ ] Documentation is complete and accurate

---

### Phase 8: Cleanup and Final Validation

**Status**: Not Started

**Goal**: Remove samples (or keep as examples) and verify production readiness

**Implementation Steps**:

- [ ] **Decision: Demo Projects**
  - [ ] Option A: Remove `apps/demo-ts-fe/` and `libs/ts-demo-libs/` (clean slate)
  - [ ] Option B: Keep as reference examples for Next.js + TypeScript setup
  - Document decision and rationale
- [ ] Run full build: `npm run build`
  - Verify all projects build successfully
- [ ] Run full test suite: `npm run test`
  - Verify all tests pass
- [ ] Verify no Nx plugins:
  - Run `npm ls | grep @nx`
  - Should only show `nx` package
- [ ] Verify Volta compatibility:
  - Run `node --version` → `24.11.1`
  - Run `npm --version` → `11.6.2`
- [ ] Verify git hooks still work:
  - Create test file: `echo "test" > test.txt`
  - Stage file: `git add test.txt`
  - Attempt commit with invalid message
  - Verify commitlint rejects it
  - Attempt commit with valid message: `chore: test commit`
  - Verify prettier formats staged files
  - Verify commit succeeds
  - Reset: `git reset HEAD~1 && rm test.txt`
- [ ] Verify .gitignore:
  - Ensure `dist/` is ignored
  - Ensure `.nx/` is ignored
  - Ensure `node_modules/` is ignored
- [ ] Clean up temporary files:
  - Remove any test files
  - Verify no uncommitted changes
- [ ] Final validation of all configuration files:
  - Lint all JSON files
  - Verify TypeScript configuration
  - Check for syntax errors

**Validation Checklist**:

- [ ] Decision made on sample projects (keep or remove)
- [ ] All builds succeed
- [ ] All tests pass
- [ ] No Nx plugins installed
- [ ] Volta still manages Node.js version
- [ ] Git hooks work correctly (prettier, commitlint)
- [ ] .gitignore includes all build outputs
- [ ] No temporary files remain
- [ ] All JSON files are valid
- [ ] No uncommitted changes
- [ ] Workspace is clean and ready for development

**Acceptance Criteria**:

- [ ] All final validation checks pass
- [ ] Workspace is production-ready
- [ ] All requirements met

---

## Dependencies

### Internal Dependencies

**None** - This plan is self-contained and doesn't depend on other plans.

### External Dependencies

**Existing Project Setup**:

- Node.js 24.11.1 (Volta-managed) - ✓ Already configured
- npm 11.6.2 (Volta-managed) - ✓ Already configured
- Git repository - ✓ Already initialized
- package.json - ✓ Already exists
- Husky git hooks - ✓ Already configured
- Prettier - ✓ Already configured
- Commitlint - ✓ Already configured

**Required**: None - All required tools and setup already exist

---

## Risks and Mitigation

### Risk 1: Breaking Existing Git Hooks

**Probability**: Low
**Impact**: High

**Risk**: Nx installation or package.json changes break existing Husky hooks

**Mitigation Strategy**:

- Test git hooks after each phase
- Validate prettier and commitlint still work
- Keep Volta configuration intact

**Contingency Plan**:

- If hooks break, revert package.json changes
- Debug and fix hook configuration
- Ensure npm workspaces don't conflict with hooks

---

### Risk 2: TypeScript Path Mapping Issues

**Probability**: Medium
**Impact**: Medium

**Risk**: Path mappings don't resolve correctly, imports fail

**Mitigation Strategy**:

- Test imports immediately after creating demo lib
- Verify tsconfig.base.json path mappings
- Ensure IDE recognizes path mappings (may need restart)

**Contingency Plan**:

- If imports fail, debug path mapping configuration
- Verify pattern matches lib structure: `libs/[language-prefix]-[name]/src/index.ts`
- Check for typos in language prefixes (ts-, java-, kt-, py-)

---

### Risk 3: Task Caching Not Working

**Probability**: Low
**Impact**: Medium

**Risk**: Nx caching doesn't work, builds always run fresh

**Mitigation Strategy**:

- Verify nx.json configuration is correct
- Test caching explicitly in Phase 6
- Check outputs configuration in targetDefaults

**Contingency Plan**:

- If caching fails, review nx.json configuration
- Ensure outputs paths are correct
- Check Nx version compatibility

---

### Risk 4: Sample Projects Confusion

**Probability**: Medium
**Impact**: Low

**Risk**: Keeping sample projects causes confusion (are they real or examples?)

**Mitigation Strategy**:

- Make clear decision to keep or remove
- If keeping, rename to `example-*`
- Document decision in README

**Contingency Plan**:

- Can remove samples later if they cause confusion
- Easy to recreate if needed as examples

---

## Final Validation Checklist

Before marking this plan as complete and ready for merge, verify ALL items below:

### Requirements Validation

- [ ] All user stories from requirements have been implemented
- [ ] All Gherkin acceptance criteria pass
- [ ] All functional requirements met (REQ-001 through REQ-008)
- [ ] All non-functional requirements met (REQ-NFR-001 through REQ-NFR-004)
- [ ] No requirements marked as "out of scope" were included

### Code Quality

- [ ] All builds pass: `npm run build`
- [ ] All tests pass: `npm run test`
- [ ] No TypeScript errors
- [ ] No linting errors (if linter configured)
- [ ] All JSON files are valid
- [ ] Code follows project style guidelines
- [ ] No Nx plugins installed: `npm ls | grep @nx`

### Nx Features

- [ ] `npx nx --version` displays version
- [ ] `nx build [project]` works for apps and libs
- [ ] `nx test [project]` works for apps and libs
- [ ] `nx serve [app]` works for apps
- [ ] `nx graph` generates dependency graph
- [ ] `nx affected:build` only builds changed projects
- [ ] Task caching works (second build shows "[local cache]")
- [ ] `nx run-many -t build` builds all projects
- [ ] Path mappings work: `@open-sharia/[language-prefix]-[name]`

### Folder Structure

- [ ] `apps/` directory exists with README.md
- [ ] `libs/` directory exists with README.md
- [ ] Libs use flat structure (no nested scope subdirectories)
- [ ] Demo app (demo-ts-fe) demonstrates correct structure (or removed)
- [ ] Demo lib (ts-demo-libs) demonstrates correct structure (or removed)
- [ ] All required files present in demo projects

### Configuration Files

- [ ] `nx.json` is valid and correctly configured
- [ ] `package.json` has workspaces field: `["apps/*", "libs/*"]`
- [ ] `package.json` has Nx scripts
- [ ] `package.json` preserves Volta pinning
- [ ] `tsconfig.base.json` has path mappings for language-prefixed libraries
- [ ] `.nxignore` excludes docs and non-code files
- [ ] `.gitignore` includes `dist/` and `.nx/`

### Compatibility

- [ ] `node --version` shows `24.11.1` (Volta managed)
- [ ] `npm --version` shows `11.6.2` (Volta managed)
- [ ] Husky pre-commit hook works
- [ ] Prettier formats staged files
- [ ] Commitlint validates commit messages
- [ ] Conventional commits enforced
- [ ] All existing git hooks functional

### Documentation

- [ ] `CLAUDE.md` updated with monorepo section
- [ ] `docs/how-to/ht__add-new-app.md` created
- [ ] `docs/how-to/ht__add-new-lib.md` created
- [ ] `docs/how-to/ht__run-nx-commands.md` created
- [ ] `docs/reference/re__monorepo-structure.md` created
- [ ] `docs/reference/re__nx-configuration.md` created
- [ ] Root `README.md` updated
- [ ] All documentation follows Diátaxis framework
- [ ] All links work (no broken links)
- [ ] Examples are accurate and tested
- [ ] TAB indentation used in docs/ files

### Testing and Validation

- [ ] Manual testing completed for all user flows
- [ ] Cross-project imports work correctly
- [ ] Dependency graph shows correct relationships
- [ ] Affected detection correctly identifies changed projects
- [ ] Caching improves build performance
- [ ] All validation checklists completed

## Completion Status

**Overall Status**: Not Started

**Last Updated**: 2025-11-24

**Completion Date**: _To be filled when complete_
