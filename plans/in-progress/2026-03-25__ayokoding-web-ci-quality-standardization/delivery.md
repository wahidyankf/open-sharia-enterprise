# Delivery Plan: ayokoding-web CI and Quality Gate Standardization

## Overview

**Delivery Type**: Direct commits to `main` (small, independent changes)

**Git Workflow**: Trunk Based Development — each phase is one commit

**Phase Independence**: Phases 1–3 (CI and documentation fixes, Goals 1–4) are independently committable and can be delivered without executing Phases 4–8. Phases 4–8 (repository pattern refactor, Goals 5–6) form a cohesive refactoring that should be committed as a unit. Phase 8 (Verify and Validate) applies to whichever set of phases was most recently completed.

## Implementation Phases

### Phase 1: Fix Documentation Drift in nx-targets.md

**Goal**: Correct the stale `platform:hugo` tag for ayokoding-web in the governance doc

**Implementation Steps**:

- [ ] Open `governance/development/infra/nx-targets.md`
- [ ] In the Current Project Tags table, update ayokoding-web row from `["type:app", "platform:hugo", "domain:ayokoding"]` to `["type:app", "platform:nextjs", "lang:ts", "domain:ayokoding"]`
- [ ] Verify no other references to ayokoding-web as a Hugo site in nx-targets.md
- [ ] Commit: `docs(nx-targets): fix stale ayokoding-web tag — platform:nextjs not hugo`

### Phase 2: Add Gherkin Spec Inputs to test:quick

**Goal**: Ensure test:quick cache invalidates when BDD specs change

**Implementation Steps**:

- [ ] Open `apps/ayokoding-web/project.json`
- [ ] Add `"inputs": ["default", "{workspaceRoot}/specs/apps/ayokoding-web/**/*.feature"]` to the `test:quick` target
- [ ] Run `nx run ayokoding-web:test:quick` locally to verify it still passes
- [ ] Commit: `fix(ayokoding-web): add Gherkin spec inputs to test:quick cache`

### Phase 3: Add test:integration to Scheduled CI

**Goal**: Run integration tests in the scheduled workflow so all three test levels execute in CI

**Implementation Steps**:

- [ ] Open `.github/workflows/test-and-deploy-ayokoding-web.yml`
- [ ] Add a new `integration` job that runs `npx nx run ayokoding-web:test:integration`
- [ ] Model the job setup (checkout, Volta, Node, npm ci) after the existing `unit` job
- [ ] Update the `deploy` job's `needs` array to include `integration`
- [ ] Update the `deploy` job's `if:` condition to include `&& needs.integration.result == 'success'` alongside the existing `needs.unit.result == 'success'` and `needs.e2e.result == 'success'` checks
- [ ] Verify the `integration` job condition matches the existing `unit` job (runs on schedule and manual trigger)
- [ ] Commit: `ci(ayokoding-web): add test:integration to scheduled workflow`

### Phase 4: Introduce ContentRepository Interface and Implementations

**Goal**: Define the data access contract and provide two implementations

**Implementation Steps**:

- [ ] Create `src/server/content/repository.ts` with `ContentRepository` interface:
  - `readAllContent(): Promise<ContentMeta[]>`
  - `readFileContent(filePath: string): Promise<{ content: string; frontmatter: Record<string, unknown> }>`
- [ ] Create `src/server/content/repository-fs.ts` — `FileSystemContentRepository` wrapping current `reader.ts` functions
- [ ] Create `src/server/content/repository-memory.ts` — `InMemoryContentRepository` with Maps for fixture data
- [ ] Run `nx run ayokoding-web:typecheck` to confirm both implementations satisfy the `ContentRepository` interface
- [ ] Commit: `feat(ayokoding-web): add ContentRepository interface with fs and in-memory implementations`

### Phase 5: Refactor Content Service Layer

**Goal**: Extract business logic into `ContentService` that accepts `ContentRepository` via constructor

**Implementation Steps**:

- [ ] Create `src/server/content/service.ts` — `ContentService` class:
  - Constructor takes `ContentRepository`
  - Moves index building logic from `index.ts` (`buildContentIndex`, `buildTrees`, `computePrevNext`)
  - Moves search logic from `search-index.ts` (`buildSearchIndex`, `searchContent`)
  - Exposes: `getBySlug()`, `listChildren()`, `getTree()`, `search()`, `getIndex()`
  - Calls `parseMarkdown()` internally for `getBySlug()`
- [ ] Update `src/server/trpc/init.ts`:
  - [ ] Change `initTRPC.create(...)` to `initTRPC.context<{ contentService: ContentService }>().create(...)`
  - [ ] Instantiate `ContentService` with `FileSystemContentRepository` above the `initTRPC` call
  - [ ] Export `createTRPCContext` returning `{ contentService }`
- [ ] Update the tRPC route handler adapter (`src/app/api/trpc/[trpc]/route.ts`) to call `createTRPCContext` and pass it to the fetch handler
- [ ] Update the server-side tRPC caller (`src/lib/trpc/server.ts`) to pass a real `ContentService` context when creating the caller
- [ ] Refactor `src/server/trpc/procedures/content.ts` — delegate to `ctx.contentService` instead of importing module functions
- [ ] Refactor `src/server/trpc/procedures/search.ts` — delegate to `ctx.contentService`
- [ ] Update `src/app/sitemap.ts`, `src/app/feed.xml/route.ts`, `generateStaticParams` — use shared `ContentService` singleton
- [ ] Remove `src/server/content/index.ts` (logic moved to `service.ts`)
- [ ] Remove `src/server/content/search-index.ts` (logic moved to `service.ts`)
- [ ] Run `nx run ayokoding-web:typecheck` to verify no broken imports
- [ ] Commit: `refactor(ayokoding-web): extract ContentService with repository injection`

### Phase 6: Refactor Unit Tests to Use InMemoryContentRepository

**Goal**: Replace `vi.mock()` approach with in-memory repository, consuming same Gherkin specs

**Implementation Steps**:

- [ ] Create `test/unit/be-steps/helpers/test-service.ts` — instantiate `ContentService` with `InMemoryContentRepository` populated with fixture data
- [ ] Substantially rewrite `test/unit/be-steps/helpers/test-caller.ts`:
  - [ ] Replace the empty `createCaller({})` context with `createCaller({ contentService: new ContentService(new InMemoryContentRepository(populateFixtureData())) })`
  - [ ] Delete the entire `vi.hoisted()` block and all four `vi.mock()` calls (`@/server/content/index`, `@/server/content/reader`, `@/server/content/parser`, `@/server/content/search-index`)
  - [ ] Convert the existing `mock-content.ts` fixture data file into the `InMemoryContentRepository` fixture population function rather than deleting it
- [ ] Verify all 5 existing step files still pass: `content-api`, `search-api`, `navigation-api`, `i18n-api`, `health-check`
- [ ] Update `vitest.config.ts` coverage exclusions — remove `index.ts` and `search-index.ts`, keep `reader.ts`, `repository-fs.ts`, `parser.ts`, `types.ts`
- [ ] Run `nx run ayokoding-web:test:quick` to verify coverage threshold still passes
- [ ] Commit: `refactor(ayokoding-web): unit tests use InMemoryContentRepository instead of vi.mock`

### Phase 7: Add Integration Tests with FileSystemContentRepository

**Goal**: Add integration test suite that uses real filesystem, consuming same Gherkin specs

**Implementation Steps**:

- [ ] Add `integration` vitest project to `vitest.config.ts`:
  - `include: ["test/integration/be-steps/**/*.steps.ts"]`
  - `environment: "node"`
- [ ] Create `test/integration/be-steps/helpers/test-service.ts` — instantiate `ContentService` with `FileSystemContentRepository` pointing at real `content/` directory
- [ ] Create `test/integration/be-steps/helpers/test-caller.ts` — tRPC caller backed by real filesystem service
- [ ] Create integration step files consuming the same Gherkin specs — assertions verify structural properties (non-empty results, valid HTML, correct ordering) not specific content
- [ ] Verify `nx run ayokoding-web:test:integration` passes
- [ ] Commit: `feat(ayokoding-web): add integration tests with FileSystemContentRepository`

### Phase 8: Verify and Validate

**Goal**: Confirm all changes work together

**Implementation Steps**:

- [ ] Run `nx run ayokoding-web:test:quick` and confirm it passes
- [ ] Run `nx run ayokoding-web:test:integration` and confirm it passes
- [ ] Run `nx affected -t typecheck lint test:quick` and confirm pre-push gate passes
- [ ] Verify `nx-targets.md` renders correctly and the tag table is accurate
- [ ] Push to `main` and verify pre-push hook succeeds

## Validation Checklist

- [ ] `nx-targets.md` tag table matches `apps/ayokoding-web/project.json` tags
- [ ] `test:quick` target has explicit Gherkin spec cache inputs
- [ ] Scheduled CI workflow has unit, integration, and e2e jobs
- [ ] Deploy job depends on all three test jobs passing
- [ ] `ContentRepository` interface exists with `InMemoryContentRepository` and `FileSystemContentRepository` implementations
- [ ] `ContentService` encapsulates all business logic and is the sole entry point for content access
- [ ] Unit tests use `InMemoryContentRepository` — no `vi.mock()` on content modules
- [ ] Integration tests use `FileSystemContentRepository` against real `content/` directory — no HTTP calls
- [ ] Both unit and integration tests consume Gherkin specs from `specs/apps/ayokoding-web/be/gherkin/`
- [ ] Coverage exclusions updated in `vitest.config.ts` — confirm `service.ts` is NOT in the exclusion list and `repository-fs.ts` IS in the exclusion list; run `nx run ayokoding-web:test:quick` to confirm the 80% threshold passes
- [ ] All local quality gates pass (`nx affected -t typecheck lint test:quick`)

## Success Metrics

| Metric                        | Before                                        | After                                                           |
| ----------------------------- | --------------------------------------------- | --------------------------------------------------------------- |
| Documentation accuracy (tags) | Stale `platform:hugo`                         | Correct `platform:nextjs`                                       |
| test:quick cache correctness  | Missing Gherkin spec inputs                   | Includes spec inputs                                            |
| CI test level coverage        | 2 of 3 levels (unit, e2e)                     | 3 of 3 levels (unit, integration, e2e)                          |
| Content layer testability     | `vi.mock()` on 4 modules, 0% service coverage | Repository pattern, service logic covered by unit tests         |
| Test architecture alignment   | Diverges from demo-be pattern                 | Matches demo-be pattern (interface + two implementations + BDD) |
| Pre-push gate                 | Passing                                       | Passing (no regression)                                         |
