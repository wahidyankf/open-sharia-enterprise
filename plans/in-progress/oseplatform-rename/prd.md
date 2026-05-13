# PRD: Rename `oseplatform-*` → `ose-*`

## Requirements

1. All four `apps/oseplatform-*` directories renamed to `apps/ose-*` via
   `git mv` to preserve history.
2. All Nx project names in `project.json` files updated to match new dir names.
3. All references across source, docs, governance, plans, agents, skills, CI,
   and infra updated atomically in a single commit.
4. Vercel production branch renamed from `prod-oseplatform-web` to
   `prod-ose-web` (manual step — requires Vercel dashboard action).
5. Post-rename: `nx build ose-web`, `nx run ose-web:test:quick`, and
   `nx run ose-web-fe-e2e:test:e2e` all pass.

## Acceptance Criteria

```gherkin
Feature: Rename oseplatform apps to ose

  Scenario: App folders renamed
    Given the rename is applied
    When I list apps/
    Then I see ose-web/, ose-web-be-e2e/, ose-web-fe-e2e/, ose-cli/
    And oseplatform-web/, oseplatform-web-be-e2e/, oseplatform-web-fe-e2e/, oseplatform-cli/ do not exist

  Scenario: Nx build succeeds after rename
    Given the rename is applied
    When I run nx build ose-web
    Then the build exits 0

  Scenario: Unit tests pass after rename
    Given the rename is applied
    When I run nx run ose-web:test:quick
    Then all tests pass

  Scenario: No stale oseplatform references remain in tracked files
    Given the rename is applied
    When I grep "oseplatform" across all tracked files
    Then zero matches are found
    But matches on "oseplatform.com" domain references are allowed
    And archived/, generated-reports/, and the plan dir are excluded
```
