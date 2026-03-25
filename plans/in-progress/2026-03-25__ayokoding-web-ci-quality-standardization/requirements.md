# Requirements: ayokoding-web CI and Quality Gate Standardization

## Identified Gaps and Inconsistencies

### Gap 1: Stale Tag in nx-targets.md Documentation

The [Current Project Tags table](../../../governance/development/infra/nx-targets.md) lists ayokoding-web as `platform:hugo`, but the actual `project.json` declares `["type:app", "platform:nextjs", "lang:ts", "domain:ayokoding"]`. The documentation is stale.

**Impact**: Misleading for anyone referencing the governance doc to understand the workspace.

### Gap 2: PR Quality Gate Runs a Single Monolithic Step

The current `pr-quality-gate.yml` runs:

```yaml
- run: npx nx affected -t typecheck
- run: npx nx affected -t lint
- run: npx nx affected -t test:quick
```

This is correct and matches the pre-push hook. However, the workflow does **not** run `typecheck` and `lint` separately from `test:quick` in its naming — the GitHub Actions step names should clearly label each gate for PR reviewers to quickly identify which gate failed.

**Impact**: Low — functional but could improve developer experience in PR checks.

### Gap 3: test:integration Not in Scheduled CI

The scheduled workflow `test-and-deploy-ayokoding-web.yml` runs:

1. `unit` job: `nx run ayokoding-web:test:quick`
2. `e2e` job: BE and FE E2E tests via Playwright

But `test:integration` (vitest `--project integration`) is never run in any CI workflow. Per the [Three-Level Testing Standard](../../../governance/development/quality/three-level-testing-standard.md), integration tests should run in scheduled CI.

**Impact**: Medium — integration test regressions are only caught locally, never in CI.

### Gap 4: test:quick Cache Inputs Inconsistency

`test:unit` declares explicit cache inputs:

```json
"inputs": ["default", "{workspaceRoot}/specs/apps/ayokoding-web/**/*.feature"]
```

But `test:quick` (which runs vitest internally) does not declare its own `inputs`. It inherits workspace defaults from `nx.json` but does not explicitly include the Gherkin specs. Since `test:quick` composes `test:unit` inline (not via `dependsOn`), Nx does not automatically chain the inputs.

**Impact**: Low — cache could serve stale results if only Gherkin specs change without source changes. In practice this is rare since specs and source usually change together.

### Gap 5: Missing Mandatory Targets Documentation for ayokoding-web

The `nx-targets.md` Mandatory Targets Summary Matrix categorizes project types (API Backend, Web UI App, CLI App, etc.) but does not explicitly list ayokoding-web's expected target set. ayokoding-web is a "Web UI App" but also has tRPC API routes, making it a hybrid.

**Impact**: Low — the targets exist and work, but the governance doc should clarify the expected target set for fullstack Next.js apps.

### Gap 6: No Repository Pattern — Content Layer Untestable in Isolation

The entire server-side content layer uses direct module-level function calls with no abstraction boundary between I/O and business logic:

- `src/server/trpc/procedures/content.ts` directly imports and calls `getContentIndex()`, `getContentMeta()`, `readFileContent()`, `parseMarkdown()`
- `src/server/trpc/procedures/search.ts` directly imports `searchContent()`, `buildSearchIndex()`, `getContentIndex()`
- `src/app/sitemap.ts`, `src/app/feed.xml/route.ts`, and `generateStaticParams` also call `getContentIndex()` directly

Unit tests compensate by using `vi.mock()` on 4 modules (`reader`, `index`, `parser`, `search-index`) with hardcoded mock data in `test/unit/be-steps/helpers/test-caller.ts`. This approach:

1. **Duplicates logic** — mock implementations re-implement `listChildren`, `getContentMeta` sorting/filtering inline
2. **Excludes all content code from coverage** — the entire `src/server/content/*` and `src/server/trpc/procedures/**` are in the coverage exclusion list
3. **Prevents meaningful integration tests** — no way to swap between in-memory and real filesystem without rewiring all mocks
4. **Diverges from demo-be pattern** — demo-be apps use a `Store` interface with `MemoryStore` (unit) and real-DB store (integration), both consuming the same Gherkin specs

**Impact**: Medium — the content layer's business logic (index building, tree computation, prev/next links, search indexing) is effectively untested at the service level. Integration tests cannot verify that real markdown files are correctly read, parsed, and indexed.

## Non-Functional Requirements

1. **CI determinism**: Integration tests must be deterministic in CI. ayokoding-web uses MSW (in-process mocks), not a real database, so `test:integration` must produce consistent results across runs without external dependencies.
2. **Coverage regression prevention**: The repository refactor must not regress the 80% line coverage threshold. `service.ts` replaces the previously excluded `index.ts` and `search-index.ts` and must be covered by unit tests through `InMemoryContentRepository`.
3. **Phase atomicity**: Each delivery phase must be a self-contained commit that leaves the codebase in a passing state (typecheck, lint, and test:quick all green). No phase may leave the build broken.

## User Stories

### Story 1: Fix Documentation Drift

**As a** contributor reading governance documentation
**I want** the nx-targets.md tag table to accurately reflect ayokoding-web's actual tags
**So that** I understand the workspace structure without checking each project.json

**Acceptance Criteria**:

```gherkin
Scenario: Tag table matches actual project.json
  Given the nx-targets.md Current Project Tags table
  When I look up ayokoding-web
  Then the tags show ["type:app", "platform:nextjs", "lang:ts", "domain:ayokoding"]
  And the tags match what is declared in apps/ayokoding-web/project.json
```

### Story 2: Add test:integration to Scheduled CI

**As a** maintainer of ayokoding-web
**I want** integration tests to run in the scheduled CI workflow
**So that** regressions in database/external service interactions are caught automatically

**Acceptance Criteria**:

```gherkin
Scenario: Integration tests run in scheduled CI
  Given the test-and-deploy-ayokoding-web.yml workflow
  When the scheduled workflow runs
  Then test:integration runs as a CI job
  And the deploy job's needs array includes unit, integration, and e2e
  And the deploy job if: condition explicitly checks needs.integration.result == 'success'

Scenario: Integration test failure blocks deployment
  Given the scheduled workflow is running
  When the integration test job fails
  Then the deploy job does not execute
  And the if: condition guard prevents execution even if the needs array would otherwise allow it
```

### Story 3: Align test:quick Cache Inputs

**As a** developer changing BDD specs
**I want** test:quick cache to invalidate when Gherkin specs change
**So that** I don't get stale cached test results

**Acceptance Criteria**:

```gherkin
Scenario: Gherkin spec change invalidates test:quick cache
  Given a cached test:quick result for ayokoding-web
  When I modify a file in specs/apps/ayokoding-web/**/*.feature
  And I run nx run ayokoding-web:test:quick
  Then the cache is missed and tests re-run
```

### Story 4: Improve PR Quality Gate Step Naming

**As a** PR reviewer
**I want** each quality gate step in CI to have a clear, distinct name
**So that** I can quickly identify which gate failed when a PR check fails

**Acceptance Criteria**:

```gherkin
Scenario: PR quality gate steps are clearly labeled
  Given a PR is opened with ayokoding-web changes
  When the quality gate workflow runs
  Then I see separate named steps for typecheck, lint, and test:quick
  And each step name clearly identifies the gate it represents
```

### Story 5: Introduce ContentRepository Interface with Two Implementations

**As a** developer working on ayokoding-web's backend
**I want** a `ContentRepository` interface with `InMemoryContentRepository` and `FileSystemContentRepository` implementations
**So that** unit tests use in-memory data and integration tests use real filesystem reads, both through the same contract

**Acceptance Criteria**:

```gherkin
Scenario: ContentRepository interface defines the data access contract
  Given the ContentRepository interface
  Then it exposes a method to read all content metadata
  And it exposes a method to read a single file's content and frontmatter
  And both InMemoryContentRepository and FileSystemContentRepository implement it

Scenario: Unit tests use InMemoryContentRepository
  Given the unit test suite for content-api
  When the tests run
  Then they use InMemoryContentRepository with fixture data
  And they do not perform any filesystem reads
  And they consume Gherkin specs from specs/apps/ayokoding-web/be/gherkin/**/*.feature

Scenario: Integration tests use FileSystemContentRepository
  Given the integration test suite for content-api
  When the tests run
  Then they use FileSystemContentRepository reading from the real content/ directory
  And they do not make any HTTP calls
  And they consume the same Gherkin specs from specs/apps/ayokoding-web/be/gherkin/**/*.feature
```

### Story 6: Refactor Content Service Layer to Accept Repository via Injection

**As a** developer maintaining ayokoding-web
**I want** a `ContentService` that receives a `ContentRepository` and encapsulates business logic (index building, tree computation, search)
**So that** business logic is testable independently of the data source and all consumers use a single entry point

**Acceptance Criteria**:

```gherkin
Scenario: ContentService encapsulates business logic
  Given a ContentService initialized with a ContentRepository
  Then it exposes getBySlug, listChildren, getTree, and search operations
  And index building, tree computation, and prev/next links are handled internally
  And tRPC procedures delegate to ContentService instead of calling module functions directly

Scenario: All content consumers use ContentService
  Given the sitemap generator, RSS feed, and generateStaticParams
  When they need content data
  Then they access it through ContentService
  And no consumer directly imports reader.ts or index.ts functions

Scenario: Server-side content code is no longer excluded from coverage
  Given the vitest coverage configuration
  When test:unit runs with the repository pattern in place
  Then src/server/content/index.ts is no longer in the coverage exclusion list
  And src/server/content/search-index.ts is no longer in the coverage exclusion list
  And src/server/content/repository-fs.ts is in the coverage exclusion list (thin I/O wrapper covered by integration tests)
  And the coverage report includes service.ts in measured lines
  And the 80% line coverage threshold still passes
```
