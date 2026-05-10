# PRD: Consolidate CLI Specs Under `behavior/`

## Requirements

### R1 — Spec directory structure

All three apps must have CLI Gherkin specs exclusively under `behavior/cli/`:

- `specs/apps/oseplatform/behavior/cli/gherkin/links-check.feature`
- `specs/apps/ayokoding/behavior/cli/gherkin/links-check.feature`
- `specs/apps/rhino/behavior/cli/gherkin/*.feature` (18 feature files)

No `specs/apps/oseplatform/cli/`, `specs/apps/ayokoding/cli/`, or
`specs/apps/rhino/cli/` directories remain.

### R2 — `behavior/README.md` updated

`oseplatform` and `ayokoding` `behavior/README.md` files must document the new
`cli/` perspective alongside `web/` and `api/`.

### R3 — Go test files updated

All Go test files that locate feature files via `specs/apps/*/cli/gherkin` must point
to `specs/apps/*/behavior/cli/gherkin` instead.

- `oseplatform-cli`: 2 files
- `ayokoding-cli`: 2 files
- `rhino-cli`: 36 files

### R4 — `project.json` `spec-coverage` targets updated

`spec-coverage` command string and inputs glob in each app's `project.json` must
reference the new `behavior/cli/gherkin` path.

- `apps/oseplatform-cli/project.json`
- `apps/ayokoding-cli/project.json`
- `apps/rhino-cli/project.json`

### R5 — All tests green

`test:quick`, `test:integration`, and `spec-coverage` pass for all three CLI apps
after migration.

### R6 — Documentation updated

- `apps/oseplatform-cli/README.md` — spec path reference updated
- `apps/ayokoding-cli/README.md` — spec path reference updated
- `specs/apps/rhino/README.md` — structure diagram updated
- `governance/development/infra/bdd-spec-test-mapping.md` — example path updated

## Acceptance Criteria (Gherkin)

```gherkin
Feature: CLI Specs Consolidated Under behavior/

  Scenario: No top-level cli/ directories remain
    Given the migration is complete
    When I list specs/apps/oseplatform/
    Then I see no cli/ directory
    When I list specs/apps/ayokoding/
    Then I see no cli/ directory
    When I list specs/apps/rhino/
    Then I see no cli/ directory

  Scenario Outline: CLI feature files accessible under behavior/cli/
    Given the migration is complete
    When I look in specs/apps/<app>/behavior/cli/gherkin/
    Then the expected feature files are present

    Examples:
      | app         | expected files                |
      | oseplatform | links-check.feature           |
      | ayokoding   | links-check.feature           |
      | rhino       | all 18 *.feature files        |

  Scenario Outline: behavior/README.md documents cli/ perspective
    Given the migration is complete
    When I read specs/apps/<app>/behavior/README.md
    Then it documents the cli/ child

    Examples:
      | app         |
      | oseplatform |
      | ayokoding   |

  Scenario Outline: All tests pass after migration
    Given the migration is complete
    When I run nx run <app>:test:quick
    Then exit code is 0
    When I run nx run <app>:test:integration
    Then exit code is 0
    When I run nx run <app>:spec-coverage
    Then exit code is 0

    Examples:
      | app             |
      | oseplatform-cli |
      | ayokoding-cli   |
      | rhino-cli       |
```
