# Delivery Plan: ayokoding-web CI and Quality Gate Standardization

## Overview

**Delivery Type**: Direct commits to `main` (small, independent changes)

**Git Workflow**: Trunk Based Development — each phase is one commit

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
- [ ] Verify the `integration` job condition matches the existing `unit` job (runs on schedule and manual trigger)
- [ ] Commit: `ci(ayokoding-web): add test:integration to scheduled workflow`

### Phase 4: Verify and Validate

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
- [ ] All local quality gates pass (`nx affected -t typecheck lint test:quick`)

## Success Metrics

| Metric                        | Before                      | After                                  |
| ----------------------------- | --------------------------- | -------------------------------------- |
| Documentation accuracy (tags) | Stale `platform:hugo`       | Correct `platform:nextjs`              |
| test:quick cache correctness  | Missing Gherkin spec inputs | Includes spec inputs                   |
| CI test level coverage        | 2 of 3 levels (unit, e2e)   | 3 of 3 levels (unit, integration, e2e) |
| Pre-push gate                 | Passing                     | Passing (no regression)                |
