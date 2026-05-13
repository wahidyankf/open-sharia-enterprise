# PRD: Rename `oseplatform-*` → `ose-*`

## Product Overview

This is a refactoring plan, not a feature. The product change is a consistent
mechanical rename of the `oseplatform-*` identifier prefix to `ose-*` across all
tracked files in the monorepo. No user-visible behavior changes — the web app at
`oseplatform.com` continues to serve the same content. The rename affects only
developer-facing names: directory paths, Nx project names, agent filenames, CI
workflow names, and script keys.

## Personas

- **Solo maintainer**: the only developer running `nx run oseplatform-*` commands daily.
- **Plan-executor agent**: the AI agent reading and executing the delivery checklist.

## User Stories

As the solo maintainer, I want all `oseplatform-*` project identifiers renamed to
`ose-*` so that `nx run`, terminal tab titles, CI logs, and grep output are
shorter in daily development use.

As the plan-executor agent, I want a complete, self-contained delivery checklist
so that I can execute the rename atomically without manual disambiguation.

## Product Scope

**In scope**:

- All four `apps/oseplatform-*` directories and their E2E pairs
- All file content references matching the enumerated sed patterns
- Agent files (`.claude/agents/`, `.opencode/agents/`), skill dirs, CI workflows, infra dirs
- Vercel production branch rename (manual step)

**Out of scope**: See BRD `## Out of Scope`.

## Product Risks

- Incomplete sed coverage leaves stale references that break imports or CI. Mitigated by the
  Phase 2 verify grep.
- `plans/done/` historical archives contain `oseplatform` references that must NOT be rewritten.
  Mitigated by excluding `plans/done/` from the sed pass.

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
