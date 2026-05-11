# PRD: Consolidate CLI Specs Under `behavior/`

## Product Overview

Consolidate three CLI Gherkin spec directories (`specs/apps/oseplatform/cli/`,
`specs/apps/ayokoding/cli/`, `specs/apps/rhino/cli/`) into their respective
`behavior/cli/` subdirectories. No new features — pure structural reorganization
of existing spec files and path-string updates in Go test files and project.json
targets.

## Personas

- **Repo maintainer (engineer hat)**: executes file moves, path updates, and quality gates.
- **Executing agent (orchestrated via plan-execution workflow)**: drives delivery checklist execution step-by-step.
- **swe-golang-dev agent**: applies sed replace and edits project.json targets.

## User Stories

- As a maintainer, I want all CLI Gherkin specs under `behavior/cli/`, so that new
  interface surfaces have a predictable home without top-level sprawl.
- As the executing agent (orchestrated via plan-execution workflow), I want explicit file paths and commands per delivery step,
  so that I can execute without guessing target locations.
- As a swe-golang-dev agent, I want a single sed command covering all 36 rhino test
  files, so that path updates are atomic and complete.

## Product Scope

### In Scope

- Move `specs/apps/oseplatform/cli/` → `specs/apps/oseplatform/behavior/cli/`
- Move `specs/apps/ayokoding/cli/` → `specs/apps/ayokoding/behavior/cli/`
- Move 18 feature files from `specs/apps/rhino/cli/gherkin/` → `specs/apps/rhino/behavior/cli/gherkin/`
- Update all Go test path strings in `apps/oseplatform-cli/cmd/`, `apps/ayokoding-cli/cmd/`, and `apps/rhino-cli/cmd/`
- Update `spec-coverage` targets in `project.json` for all three apps
- Update `behavior/README.md` files for oseplatform and ayokoding to document the new `cli/` perspective
- Update app READMEs and governance docs that reference old paths

### Out of Scope

- Adding new Gherkin scenarios or modifying existing feature file content
- Changing test logic or coverage thresholds
- Migrating specs for any app not listed above (`organiclever`, `wahidyankf`, etc.)
- Updating web or API behavior specs

## Product Risks

- **spec-coverage recursion into `specs/` subfolder**: rhino's `behavior/cli/gherkin/specs/`
  contains 4 planned but unimplemented features. If spec-coverage recurses into it, the run
  will fail. Mitigation: documented in delivery step 3.13.
- **sed over-match**: the sed pattern could accidentally match unexpected files. Mitigation:
  step 3.6 verifies exactly 36 files changed.

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
- `repo-governance/development/infra/bdd-spec-test-mapping.md` — example path updated

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
