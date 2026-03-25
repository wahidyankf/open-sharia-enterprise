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
  And the deploy job depends on unit, integration, and e2e jobs all passing

Scenario: Integration test failure blocks deployment
  Given the scheduled workflow is running
  When the integration test job fails
  Then the deploy job does not execute
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
