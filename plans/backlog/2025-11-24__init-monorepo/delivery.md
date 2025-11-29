# Delivery Plan: Nx Monorepo Initialization

## Overview

**Delivery Type**: Single PR

**Git Workflow**: Commit to `main`

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
  - Add `workspaces`: `["apps/*", "libs/*/*"]`
  - Add npm scripts: build, test, lint, affected:\*, graph
  - Verify Volta pinning remains: `"node": "24.11.1"`, `"npm": "11.6.2"`
- [ ] Create `tsconfig.base.json`:
  - Base TypeScript compiler options
  - Path mappings for all scopes: `@open-sharia/shared/*`, etc.
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

**Goal**: Create apps/ and libs/ folders with scope structure and documentation

**Implementation Steps**:

- [ ] Create `apps/` directory: `mkdir -p apps`
- [ ] Create `apps/README.md` documenting:
  - Purpose: deployable applications
  - Naming convention: `[domain]-[type]`
  - Required files: src/, project.json, tsconfig.json, package.json
  - Rule: apps don't import other apps
  - Examples: api-gateway, admin-dashboard
- [ ] Create `libs/` directory with scopes:
  - `mkdir -p libs/shared`
  - `mkdir -p libs/feature`
  - `mkdir -p libs/data-access`
  - `mkdir -p libs/ui`
  - `mkdir -p libs/util`
- [ ] Create `libs/README.md` documenting:
  - Purpose: reusable libraries
  - Naming convention: `[scope]/[name]`
  - Scope definitions: shared, feature, data-access, ui, util
  - Dependency rules matrix
  - Required files: src/index.ts, src/lib/, project.json, tsconfig.json
  - Examples: shared/utils, feature/auth

**Validation Checklist**:

- [ ] `apps/` directory exists at repository root
- [ ] `apps/README.md` exists and documents conventions
- [ ] `libs/` directory exists at repository root
- [ ] All scope directories exist: `libs/shared/`, `libs/feature/`, `libs/data-access/`, `libs/ui/`, `libs/util/`
- [ ] `libs/README.md` exists and documents scopes
- [ ] README files follow markdown best practices
- [ ] README files include clear examples

**Acceptance Criteria**:

- [ ] All user stories related to folder structure pass Gherkin tests
- [ ] Folder structure matches architecture diagrams
- [ ] Documentation is complete and accurate

---

### Phase 3: Sample App Creation

**Status**: Not Started

**Goal**: Create a working sample application to validate app structure

**Implementation Steps**:

- [ ] Create app directory structure:
  - `mkdir -p apps/sample-app/src`
- [ ] Create `apps/sample-app/src/index.ts`:
  ```typescript
  console.log("Hello from sample-app!");
  ```
- [ ] Create `apps/sample-app/package.json`:
  ```json
  {
    "name": "sample-app",
    "version": "0.1.0",
    "private": true
  }
  ```
- [ ] Create `apps/sample-app/project.json`:
  - name: `"sample-app"`
  - sourceRoot: `"apps/sample-app/src"`
  - projectType: `"application"`
  - targets: build (tsc), serve (node), test (node --test)
- [ ] Create `apps/sample-app/tsconfig.json`:
  - Extends `../../tsconfig.base.json`
  - Include: `["src/**/*"]`
- [ ] Create `apps/sample-app/tsconfig.build.json`:
  - Extends `./tsconfig.json`
  - compilerOptions: `outDir: "dist"`, `rootDir: "src"`
  - Exclude tests
- [ ] Create `apps/sample-app/README.md`:
  - Document app purpose
  - Document how to build and run
- [ ] Test app build: `nx build sample-app`
- [ ] Test app serve: `nx serve sample-app`
- [ ] Verify build output created in `apps/sample-app/dist/`

**Validation Checklist**:

- [ ] App directory structure is complete
- [ ] All required files exist and are valid
- [ ] `nx build sample-app` succeeds
- [ ] Build creates `apps/sample-app/dist/index.js`
- [ ] `nx serve sample-app` runs successfully
- [ ] App outputs "Hello from sample-app!"
- [ ] TypeScript compiles without errors
- [ ] No linting errors (if linter configured)

**Acceptance Criteria**:

- [ ] All user stories related to app creation pass Gherkin tests
- [ ] Sample app demonstrates correct structure
- [ ] App can be built and run successfully

---

### Phase 4: Sample Lib Creation

**Status**: Not Started

**Goal**: Create a working sample library to validate lib structure

**Implementation Steps**:

- [ ] Create lib directory structure:
  - `mkdir -p libs/shared/sample-lib/src/lib`
- [ ] Create `libs/shared/sample-lib/src/lib/greet.ts`:
  ```typescript
  export function greet(name: string): string {
    return `Hello, ${name}!`;
  }
  ```
- [ ] Create `libs/shared/sample-lib/src/lib/greet.test.ts`:

  ```typescript
  import { test } from "node:test";
  import assert from "node:assert";
  import { greet } from "./greet";

  test("greet returns greeting message", () => {
    assert.strictEqual(greet("World"), "Hello, World!");
  });
  ```

- [ ] Create `libs/shared/sample-lib/src/index.ts`:
  ```typescript
  export { greet } from "./lib/greet";
  ```
- [ ] Create `libs/shared/sample-lib/package.json`:
  ```json
  {
    "name": "@open-sharia/shared/sample-lib",
    "version": "0.1.0",
    "private": true
  }
  ```
- [ ] Create `libs/shared/sample-lib/project.json`:
  - name: `"shared-sample-lib"`
  - sourceRoot: `"libs/shared/sample-lib/src"`
  - projectType: `"library"`
  - targets: build (tsc), test (node --test)
- [ ] Create `libs/shared/sample-lib/tsconfig.json`:
  - Extends `../../../tsconfig.base.json`
  - Include: `["src/**/*"]`
- [ ] Create `libs/shared/sample-lib/tsconfig.build.json`:
  - Extends `./tsconfig.json`
  - compilerOptions: `outDir: "dist"`, `rootDir: "src"`
  - Exclude tests
- [ ] Create `libs/shared/sample-lib/README.md`:
  - Document lib purpose
  - Document public API
  - Document how to use
- [ ] Test lib build: `nx build shared-sample-lib`
- [ ] Test lib tests: `nx test shared-sample-lib`
- [ ] Verify build output created in `libs/shared/sample-lib/dist/`

**Validation Checklist**:

- [ ] Lib directory structure is complete
- [ ] All required files exist and are valid
- [ ] `nx build shared-sample-lib` succeeds
- [ ] Build creates `libs/shared/sample-lib/dist/`
- [ ] `nx test shared-sample-lib` succeeds
- [ ] All tests pass
- [ ] TypeScript compiles without errors
- [ ] Public API exports correctly from index.ts

**Acceptance Criteria**:

- [ ] All user stories related to lib creation pass Gherkin tests
- [ ] Sample lib demonstrates correct structure
- [ ] Lib can be built and tested successfully

---

### Phase 5: Cross-Project Integration

**Status**: Not Started

**Goal**: Validate app can import and use lib (cross-project dependencies)

**Implementation Steps**:

- [ ] Update `apps/sample-app/src/index.ts` to import lib:

  ```typescript
  import { greet } from "@open-sharia/shared/sample-lib";

  console.log(greet("World"));
  console.log("Sample app using sample lib!");
  ```

- [ ] Rebuild sample-app: `nx build sample-app`
  - Verify lib is built first (dependency resolution)
- [ ] Run sample-app: `nx serve sample-app`
  - Verify output includes "Hello, World!"
- [ ] Test dependency graph: `nx graph`
  - Verify sample-app -> shared-sample-lib dependency shown
- [ ] Make change to lib, rebuild app
  - Verify affected detection works

**Validation Checklist**:

- [ ] App successfully imports from lib using path mapping
- [ ] TypeScript resolves import correctly
- [ ] `nx build sample-app` builds lib first, then app
- [ ] `nx serve sample-app` outputs expected messages
- [ ] `nx graph` shows app -> lib dependency
- [ ] Dependency graph visualizes correctly in browser
- [ ] No import errors or TypeScript errors

**Acceptance Criteria**:

- [ ] All user stories related to cross-project imports pass Gherkin tests
- [ ] App successfully uses lib functionality
- [ ] Dependency graph shows correct relationships

---

### Phase 6: Nx Features Validation

**Status**: Not Started

**Goal**: Validate core Nx features (caching, affected, run-many)

**Implementation Steps**:

- [ ] **Test Task Caching**:
  - Run `nx build sample-app` (first build)
  - Run `nx build sample-app` again (should use cache)
  - Verify second build shows "[local cache]"
  - Verify second build completes in < 1 second
- [ ] **Test Affected Detection**:
  - Make change to `libs/shared/sample-lib/src/lib/greet.ts`
  - Run `nx affected:build`
  - Verify both lib and app are built
  - Revert change
  - Make change to `apps/sample-app/src/index.ts` only
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
  - Document library scopes
  - Link to how-to guides and references
- [ ] Create `docs/how-to/ht__add-new-app.md`:
  - Step-by-step guide for creating apps
  - Include all required files
  - Provide template examples
  - Document naming conventions
- [ ] Create `docs/how-to/ht__add-new-lib.md`:
  - Step-by-step guide for creating libs
  - Include all required files
  - Explain scope selection
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

- [ ] **Decision: Sample Projects**
  - [ ] Option A: Remove `apps/sample-app/` and `libs/shared/sample-lib/` (clean slate)
  - [ ] Option B: Keep and rename to `apps/example-app/` and `libs/shared/example-lib/` (reference)
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

- Test imports immediately after creating sample lib
- Verify tsconfig.base.json path mappings
- Ensure IDE recognizes path mappings (may need restart)

**Contingency Plan**:

- If imports fail, debug path mapping configuration
- Verify pattern matches lib structure: `libs/[scope]/[name]/src/index.ts`
- Check for typos in scope names

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
- [ ] Path mappings work: `@open-sharia/[scope]/[name]`

### Folder Structure

- [ ] `apps/` directory exists with README.md
- [ ] `libs/` directory exists with README.md
- [ ] All lib scopes exist: shared/, feature/, data-access/, ui/, util/
- [ ] Sample app demonstrates correct structure (or removed)
- [ ] Sample lib demonstrates correct structure (or removed)
- [ ] All required files present in sample projects

### Configuration Files

- [ ] `nx.json` is valid and correctly configured
- [ ] `package.json` has workspaces field
- [ ] `package.json` has Nx scripts
- [ ] `package.json` preserves Volta pinning
- [ ] `tsconfig.base.json` has path mappings for all scopes
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

---

## Next Steps (Post-Delivery)

After this plan is complete and merged, the following work can begin:

1. **Create Real Applications**
   - Use `docs/how-to/ht__add-new-app.md` guide
   - Create backend API services
   - Create frontend applications

2. **Create Shared Libraries**
   - Use `docs/how-to/ht__add-new-lib.md` guide
   - Implement common utilities
   - Build feature libraries

3. **Configure CI/CD**
   - Set up GitHub Actions
   - Use `nx affected` for optimized CI
   - Configure deployment pipelines

4. **Add Build Tools**
   - Configure bundlers (Webpack, Vite)
   - Set up test frameworks (Jest, Vitest)
   - Configure linting (ESLint)

5. **Team Onboarding**
   - Train team on monorepo workflows
   - Share documentation
   - Establish best practices

This plan delivers the **foundation** for monorepo development. All future app and lib development will build on this structure.
