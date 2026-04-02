# Technical Documentation

## Current vs Target Structure

### Inconsistency 1: FE Gherkin Specs (Ayokoding + OSE Platform)

**Current** (flat files):

```text
specs/apps/ayokoding/fe/gherkin/accessibility.feature
specs/apps/ayokoding/fe/gherkin/content-rendering.feature
specs/apps/ayokoding/fe/gherkin/i18n.feature
specs/apps/ayokoding/fe/gherkin/navigation.feature
specs/apps/ayokoding/fe/gherkin/responsive.feature
specs/apps/ayokoding/fe/gherkin/search.feature

specs/apps/oseplatform/fe/gherkin/accessibility.feature
specs/apps/oseplatform/fe/gherkin/landing-page.feature
specs/apps/oseplatform/fe/gherkin/navigation.feature
specs/apps/oseplatform/fe/gherkin/responsive.feature
specs/apps/oseplatform/fe/gherkin/theme.feature
```

**Target** (domain subdirectories):

```text
specs/apps/ayokoding/fe/gherkin/accessibility/accessibility.feature
specs/apps/ayokoding/fe/gherkin/content-rendering/content-rendering.feature
specs/apps/ayokoding/fe/gherkin/i18n/i18n.feature
specs/apps/ayokoding/fe/gherkin/navigation/navigation.feature
specs/apps/ayokoding/fe/gherkin/responsive/responsive.feature
specs/apps/ayokoding/fe/gherkin/search/search.feature

specs/apps/oseplatform/fe/gherkin/accessibility/accessibility.feature
specs/apps/oseplatform/fe/gherkin/landing-page/landing-page.feature
specs/apps/oseplatform/fe/gherkin/navigation/navigation.feature
specs/apps/oseplatform/fe/gherkin/responsive/responsive.feature
specs/apps/oseplatform/fe/gherkin/theme/theme.feature
```

### Inconsistency 2: Go Library Specs (golang-commons + hugo-commons)

**Current** (missing `gherkin/` wrapper):

```text
specs/libs/golang-commons/testutil/capture-stdout.feature
specs/libs/golang-commons/timeutil/timestamp.feature
specs/libs/hugo-commons/links/check-links.feature
```

**Target** (with `gherkin/` wrapper):

```text
specs/libs/golang-commons/gherkin/testutil/capture-stdout.feature
specs/libs/golang-commons/gherkin/timeutil/timestamp.feature
specs/libs/hugo-commons/gherkin/links/check-links.feature
```

### Inconsistency 3: ts-ui Library Specs

**Current** (flat files under `gherkin/`):

```text
specs/libs/ts-ui/gherkin/alert.feature
specs/libs/ts-ui/gherkin/button.feature
specs/libs/ts-ui/gherkin/card.feature
specs/libs/ts-ui/gherkin/dialog.feature
specs/libs/ts-ui/gherkin/input.feature
specs/libs/ts-ui/gherkin/label.feature
```

**Target** (component subdirectories):

```text
specs/libs/ts-ui/gherkin/alert/alert.feature
specs/libs/ts-ui/gherkin/button/button.feature
specs/libs/ts-ui/gherkin/card/card.feature
specs/libs/ts-ui/gherkin/dialog/dialog.feature
specs/libs/ts-ui/gherkin/input/input.feature
specs/libs/ts-ui/gherkin/label/label.feature
```

## Target File Tree

After all three phases, the complete `specs/` directory will follow a uniform convention:
`{scope}/{name}/{layer}/gherkin/{domain}/{feature}.feature`

```text
specs/
├── apps/
│   ├── a-demo/                          # (unchanged — already consistent)
│   │   ├── be/gherkin/
│   │   │   ├── admin/admin-*.feature
│   │   │   ├── authentication/*.feature
│   │   │   ├── expenses/*.feature
│   │   │   ├── health/*.feature
│   │   │   ├── security/*.feature
│   │   │   ├── test-support/*.feature
│   │   │   ├── token-management/*.feature
│   │   │   └── user-lifecycle/*.feature
│   │   ├── c4/
│   │   ├── contracts/
│   │   └── fe/gherkin/
│   │       ├── admin/admin-panel.feature
│   │       ├── authentication/*.feature
│   │       ├── expenses/*.feature
│   │       ├── health/health-status.feature
│   │       ├── layout/accessibility.feature, responsive.feature
│   │       ├── security/security.feature
│   │       ├── token-management/tokens.feature
│   │       └── user-lifecycle/*.feature
│   │
│   ├── ayokoding/
│   │   ├── be/gherkin/                  # (unchanged)
│   │   │   ├── content-api/content-api.feature
│   │   │   ├── health/health-check.feature
│   │   │   ├── i18n/i18n-api.feature
│   │   │   ├── navigation-api/navigation-api.feature
│   │   │   └── search-api/search-api.feature
│   │   ├── build-tools/gherkin/         # (unchanged)
│   │   │   └── index-generation/index-generation.feature
│   │   ├── c4/
│   │   ├── cli/gherkin/                 # (flat — intentional)
│   │   │   └── links-check.feature
│   │   └── fe/gherkin/                  # ← PHASE 1: flat → subdirectories
│   │       ├── accessibility/accessibility.feature
│   │       ├── content-rendering/content-rendering.feature
│   │       ├── i18n/i18n.feature
│   │       ├── navigation/navigation.feature
│   │       ├── responsive/responsive.feature
│   │       └── search/search.feature
│   │
│   ├── organiclever/                    # (unchanged — already consistent)
│   │   ├── be/gherkin/
│   │   │   ├── authentication/*.feature
│   │   │   └── health/*.feature
│   │   ├── c4/
│   │   ├── contracts/
│   │   └── fe/gherkin/
│   │       ├── authentication/*.feature
│   │       └── layout/accessibility.feature
│   │
│   ├── oseplatform/
│   │   ├── be/gherkin/                  # (unchanged)
│   │   │   ├── content-retrieval/content-retrieval.feature
│   │   │   ├── health/health.feature
│   │   │   ├── rss-feed/rss-feed.feature
│   │   │   ├── search/search.feature
│   │   │   └── seo/seo.feature
│   │   ├── c4/
│   │   ├── cli/gherkin/                 # (flat — intentional)
│   │   │   └── links-check.feature
│   │   └── fe/gherkin/                  # ← PHASE 1: flat → subdirectories
│   │       ├── accessibility/accessibility.feature
│   │       ├── landing-page/landing-page.feature
│   │       ├── navigation/navigation.feature
│   │       ├── responsive/responsive.feature
│   │       └── theme/theme.feature
│   │
│   └── rhino/                           # (flat — intentional)
│       └── cli/gherkin/
│           ├── agents-sync.feature
│           ├── agents-validate-claude.feature
│           ├── ... (15 files total)
│           └── test-coverage-validate.feature
│
├── libs/
│   ├── golang-commons/
│   │   └── gherkin/                     # ← PHASE 2: added gherkin/ wrapper
│   │       ├── testutil/capture-stdout.feature
│   │       └── timeutil/timestamp.feature
│   │
│   ├── hugo-commons/
│   │   └── gherkin/                     # ← PHASE 2: added gherkin/ wrapper
│   │       └── links/check-links.feature
│   │
│   └── ts-ui/
│       └── gherkin/                     # ← PHASE 3: flat → subdirectories
│           ├── alert/alert.feature
│           ├── button/button.feature
│           ├── card/card.feature
│           ├── dialog/dialog.feature
│           ├── input/input.feature
│           └── label/label.feature
│
└── apps-labs/                           # (unchanged)
```

## Blast Radius Analysis

### Phase 1: FE Specs (Ayokoding + OSE Platform)

**Files to move**: 11 feature files (6 ayokoding + 5 oseplatform)

**References to update**:

| File                                                                | Reference Type                | Path Change                           |
| ------------------------------------------------------------------- | ----------------------------- | ------------------------------------- |
| `apps/ayokoding-web/test/unit/fe-steps/accessibility.steps.tsx`     | `loadFeature()` path          | Add `/accessibility` subdirectory     |
| `apps/ayokoding-web/test/unit/fe-steps/content-rendering.steps.tsx` | `loadFeature()` path          | Add `/content-rendering` subdirectory |
| `apps/ayokoding-web/test/unit/fe-steps/i18n.steps.tsx`              | `loadFeature()` path          | Add `/i18n` subdirectory              |
| `apps/ayokoding-web/test/unit/fe-steps/navigation.steps.tsx`        | `loadFeature()` path          | Add `/navigation` subdirectory        |
| `apps/ayokoding-web/test/unit/fe-steps/responsive.steps.tsx`        | `loadFeature()` path          | Add `/responsive` subdirectory        |
| `apps/ayokoding-web/test/unit/fe-steps/search.steps.tsx`            | `loadFeature()` path          | Add `/search` subdirectory            |
| `apps/oseplatform-web/test/unit/fe-steps/landing-page.steps.tsx`    | `loadFeature()` path          | Add `/landing-page` subdirectory      |
| `apps/oseplatform-web/test/unit/fe-steps/navigation.steps.tsx`      | `loadFeature()` path          | Add `/navigation` subdirectory        |
| `apps/oseplatform-web/test/unit/fe-steps/responsive.steps.tsx`      | `loadFeature()` path          | Add `/responsive` subdirectory        |
| `apps/oseplatform-web/test/unit/fe-steps/theme.steps.tsx`           | `loadFeature()` path          | Add `/theme` subdirectory             |
| `specs/apps/ayokoding/README.md`                                    | Structure tree                | Update FE gherkin section             |
| `specs/apps/oseplatform/README.md`                                  | Structure tree + domain table | Update FE gherkin section             |

**References that need NO changes** (already use recursive globs):

| File                                                       | Glob Pattern                                  | Why Safe                          |
| ---------------------------------------------------------- | --------------------------------------------- | --------------------------------- |
| `apps/ayokoding-web-fe-e2e/playwright.config.ts`           | `**/*.feature`                                | Recursive, matches subdirectories |
| `apps/ayokoding-web-fe-e2e/project.json` (inputs)          | `**/*.feature`                                | Recursive glob                    |
| `apps/ayokoding-web-fe-e2e/project.json` (spec-coverage)   | `specs/apps/ayokoding/fe/gherkin` (dir arg)   | Directory-level, not file-level   |
| `apps/ayokoding-web/project.json` (inputs)                 | `**/*.feature`                                | Recursive glob                    |
| `apps/oseplatform-web-fe-e2e/playwright.config.ts`         | `**/*.feature`                                | Recursive glob                    |
| `apps/oseplatform-web-fe-e2e/project.json` (inputs)        | `**/*.feature`                                | Recursive glob                    |
| `apps/oseplatform-web-fe-e2e/project.json` (spec-coverage) | `specs/apps/oseplatform/fe/gherkin` (dir arg) | Directory-level                   |
| `apps/oseplatform-web/project.json` (inputs)               | `**/*.feature`                                | Recursive glob                    |

**Note**: The `accessibility.feature` file does not have a corresponding step file in
`oseplatform-web` (it only exists in the FE E2E tests which use glob patterns). Confirm during
execution.

### Phase 2: Go Library Specs

**Files to move**: 3 feature files (2 golang-commons + 1 hugo-commons)

**References to update**:

| File                                                              | Reference Type         | Path Change               |
| ----------------------------------------------------------------- | ---------------------- | ------------------------- |
| `libs/golang-commons/testutil/capture-stdout.integration_test.go` | `filepath.Join()` path | Insert `/gherkin` segment |
| `libs/golang-commons/timeutil/timestamp.integration_test.go`      | `filepath.Join()` path | Insert `/gherkin` segment |
| `libs/hugo-commons/links/check-links.integration_test.go`         | `filepath.Join()` path | Insert `/gherkin` segment |

**References that need NO changes** (already use recursive globs):

| File                               | Glob Pattern   | Why Safe       |
| ---------------------------------- | -------------- | -------------- |
| `libs/golang-commons/project.json` | `**/*.feature` | Recursive glob |
| `libs/hugo-commons/project.json`   | `**/*.feature` | Recursive glob |

### Phase 3: ts-ui Library Specs

**Files to move**: 6 feature files

**References to update**:

| File                                                | Reference Type       | Path Change                |
| --------------------------------------------------- | -------------------- | -------------------------- |
| `libs/ts-ui/src/components/alert/alert.steps.tsx`   | `loadFeature()` path | Add `/alert` subdirectory  |
| `libs/ts-ui/src/components/button/button.steps.tsx` | `loadFeature()` path | Add `/button` subdirectory |
| `libs/ts-ui/src/components/card/card.steps.tsx`     | `loadFeature()` path | Add `/card` subdirectory   |
| `libs/ts-ui/src/components/dialog/dialog.steps.tsx` | `loadFeature()` path | Add `/dialog` subdirectory |
| `libs/ts-ui/src/components/input/input.steps.tsx`   | `loadFeature()` path | Add `/input` subdirectory  |
| `libs/ts-ui/src/components/label/label.steps.tsx`   | `loadFeature()` path | Add `/label` subdirectory  |

**References that need NO changes**:

- `libs/ts-ui/project.json` -- no `spec-coverage` target, no feature file globs in inputs

## Design Decisions

### Why Not Move CLI Specs Too?

CLI specs in `rhino/cli/gherkin/` contain 15 flat files. Moving them into subdirectories would
require updating 32 Go test files with hardcoded paths. The domain is already encoded in filenames
(`agents-sync.feature`, `env-backup.feature`). The blast radius is too high for marginal
organizational gain. Same reasoning applies to ayokoding-cli (1 file) and oseplatform-cli (1 file).

### Why `git mv` Instead of Manual Move?

`git mv` preserves file history in `git log --follow`, which matters for spec ownership tracking.
Manual mkdir + mv + git add achieves the same result but loses the explicit rename signal.

### Why Independent Commits Per Phase?

Each phase affects different projects and test suites. Independent commits allow bisecting if a
regression appears, and each commit can be verified in isolation.
