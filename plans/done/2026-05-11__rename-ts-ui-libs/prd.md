# Product Requirements Document — Rename ts-ui Libraries to web-ui

## Product Overview

A mechanical rename of two Nx shared libraries and all their references across the `ose-public`
monorepo. No new runtime behaviour is introduced. The rename is transparent to end users of the
deployed web applications; it is visible only to developers and AI agents working inside the repo.

For the business rationale see [brd.md](./brd.md).

## Personas

| Hat                   | Concern                                                                                                                     |
| --------------------- | --------------------------------------------------------------------------------------------------------------------------- |
| **Library author**    | Lib-internal config (`project.json`, `package.json`, Storybook) correctly reflects new names                                |
| **App developer**     | All four Next.js consumer apps compile, type-check, and run with new import paths                                           |
| **Governance author** | Markdown references in governance, agents, and skills use new names                                                         |
| **AI agent**          | Agent definition files and skill files reference correct library names so future prompts generate correct import statements |

## User Stories

### US-1 — Library internals updated

As the **library author**, I want `libs/web-ui/` and `libs/web-ui-token/` to have correct Nx
project names, source roots, and Storybook configuration, so that `nx run web-ui:*` and
`nx run web-ui-token:*` targets resolve without error.

### US-2 — App consumers updated

As the **app developer**, I want all four Next.js app consumers (`organiclever-web`,
`ayokoding-web`, `wahidyankf-web`, `oseplatform-web`) to import from
`@open-sharia-enterprise/web-ui` and `@open-sharia-enterprise/web-ui-token`, so that `nx affected
-t typecheck` passes with zero errors after the rename.

### US-3 — Governance and agent docs updated

As the **governance author / AI agent**, I want every `.md` file across `repo-governance/`, `.claude/`,
and `docs/` to reference `web-ui` and `web-ui-token` instead of `ts-ui` and `ts-ui-tokens`, so
that future agent prompts suggest the correct package names.

### US-4 — Lockfile regenerated

As the **library author**, I want `package-lock.json` updated to reflect the new package names,
so that `npm install` in a clean environment resolves dependencies correctly.

## Acceptance Criteria

### AC-1 — Directory rename

```gherkin
Scenario: Library directories renamed
  Given the monorepo working tree is clean on branch worktree/ui
  When the executor runs `git mv libs/ts-ui libs/web-ui`
    And the executor runs `git mv libs/ts-ui-tokens libs/web-ui-token`
  Then `libs/ts-ui/` no longer exists in the working tree
    And `libs/ts-ui-tokens/` no longer exists in the working tree
    And `libs/web-ui/` exists and contains all previously-existing files
    And `libs/web-ui-token/` exists and contains all previously-existing files
```

### AC-2 — Lib-internal config

```gherkin
Scenario: Lib-internal project.json and package.json updated
  Given `libs/web-ui/project.json` and `libs/web-ui-token/project.json` exist
  When the executor replaces all `ts-ui` references in both project.json files
    And the executor replaces all `ts-ui-tokens` references in both project.json files
    And the executor updates `libs/web-ui/package.json` name to `@open-sharia-enterprise/web-ui`
    And the executor updates the ts-ui-tokens dependency to `@open-sharia-enterprise/web-ui-token`
    And the executor updates `libs/web-ui-token/package.json` name to `@open-sharia-enterprise/web-ui-token`
  Then `nx run web-ui:typecheck` exits 0
    And `nx run web-ui-token:typecheck` exits 0
```

### AC-3 — Storybook configuration

```gherkin
Scenario: Storybook preview imports updated
  Given `libs/web-ui/.storybook/preview.ts` imports from `@open-sharia-enterprise/ts-ui-tokens/src/...`
    And `libs/web-ui/.storybook/storybook.css` imports from `@open-sharia-enterprise/ts-ui-tokens/src/...`
  When the executor replaces all `ts-ui-tokens` with `web-ui-token` in both Storybook config files
  Then `nx run web-ui:storybook` starts without module-not-found errors
```

### AC-4 — App package.json dependencies

```gherkin
Scenario: App package.json files reference new package names
  Given `apps/organiclever-web/package.json` depends on `@open-sharia-enterprise/ts-ui`
    And `apps/wahidyankf-web/package.json` depends on `@open-sharia-enterprise/ts-ui`
    And `apps/ayokoding-web/package.json` (if present as explicit dep) depends on old names
  When the executor updates each package.json dependency name to the new identifier
  Then `npm install` resolves all dependencies without errors
    And the updated lockfile contains `@open-sharia-enterprise/web-ui` entries
    And the updated lockfile contains no `@open-sharia-enterprise/ts-ui` entries
```

### AC-5 — App next.config.ts transpilePackages

```gherkin
Scenario: transpilePackages entries renamed in Next.js configs
  Given `apps/organiclever-web/next.config.ts` lists `@open-sharia-enterprise/ts-ui` in transpilePackages
    And `apps/wahidyankf-web/next.config.ts` lists the same old names
  When the executor updates both entries to new names
  Then `nx run organiclever-web:build` succeeds
    And `nx run wahidyankf-web:build` succeeds (verified via typecheck gate as proxy)
```

### AC-6 — App project.json implicitDependencies

```gherkin
Scenario: implicitDependencies updated
  Given `apps/organiclever-web/project.json` lists `ts-ui` in `implicitDependencies`
  When the executor updates the entry to `web-ui`
  Then `nx graph` correctly shows `organiclever-web` depending on `web-ui`
```

### AC-7 — TSX and CSS source import paths

```gherkin
Scenario: All TSX and CSS files updated to new import paths
  Given ~29 organiclever-web files (28 TSX/TS + 1 CSS) import `@open-sharia-enterprise/ts-ui`
    And ~15 ayokoding-web files import `@open-sharia-enterprise/ts-ui`
    And ~12 wahidyankf-web files (7 source + 4 unit tests + 1 globals.css) import `@open-sharia-enterprise/ts-ui`
    And 4 oseplatform-web src TSX files and 5 test step files import `@open-sharia-enterprise/ts-ui`
    And all four apps have `@source "../../../../libs/ts-ui/src/**/*.{ts,tsx}"` in their globals.css
    And multiple apps have CSS `@import` of `@open-sharia-enterprise/ts-ui-tokens/src/...`
  When the executor replaces all old import paths with new ones in each file
  Then `git grep -r "ts-ui" -- apps/` returns empty output
    And `nx affected -t typecheck` exits 0 across all affected apps
```

### AC-8 — Governance and agent docs

```gherkin
Scenario: Governance markdown files reference new names
  Given 13 markdown files under `repo-governance/`, `.claude/agents/`, and `.claude/skills/`
        reference `ts-ui` or `ts-ui-tokens`
    And 4 markdown files under `specs/apps/` reference `ts-ui` or `ts-ui-tokens`
  When the executor performs a global search-and-replace in all affected markdown files
  Then `git grep -r "ts-ui" -- repo-governance/ .claude/ specs/` returns empty output
```

### AC-9 — Zero remaining ts-ui references

```gherkin
Scenario: Complete rename — no references remain
  Given all previous acceptance criteria have been satisfied
  When the executor runs `git grep -r "ts-ui" -- . ':!plans/' ':!generated-reports/' ':!archived/'`
  Then the command returns empty output (exit code 1, no matches)
```

Note: `generated-reports/` is excluded because historical audit snapshots may reference `ts-ui`
(non-executable, no build impact). `archived/` is excluded because it contains decommissioned code
not part of active builds.

### AC-10 — CI green

```gherkin
Scenario: CI passes after push
  Given the rename commits are pushed to main via the worktree branch
  When GitHub Actions runs the triggered CI workflows
  Then all CI checks complete with status "success"
    And no typecheck, lint, or test failures are reported
```

## Product Scope

### In Scope

- Mechanical rename of directory names, Nx project names, npm package names, and import strings.
- All file categories enumerated in [tech-docs.md](./tech-docs.md) §File Impact Map.

### Out of Scope

- Changing component behaviour, props, or exports.
- Adding new stories, tests, or documentation beyond what is required by the rename.
- Renaming the worktree branch or any app-level project name.

## Product Risks

| Risk                                                | AC violated | Mitigation                                                   |
| --------------------------------------------------- | ----------- | ------------------------------------------------------------ |
| Missed import in a source file                      | AC-7, AC-9  | `git grep` post-update; typecheck gate                       |
| `package-lock.json` not regenerated                 | AC-4        | Explicit `npm install` step in delivery checklist            |
| Storybook import updated but CSS import missed      | AC-3        | Both Storybook files listed explicitly in delivery checklist |
| organiclever-web `implicitDependencies` not updated | AC-6        | Explicit step in delivery checklist for `project.json`       |
