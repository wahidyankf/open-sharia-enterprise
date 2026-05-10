# PRD — wahidyankf-web DDD + New Specs Format

## Target spec tree

```
specs/apps/wahidyankf/
├── README.md
├── product/
│   └── README.md
├── system-context/
│   ├── README.md
│   └── context.md
├── containers/
│   ├── README.md
│   └── container.md
├── components/
│   ├── README.md
│   └── web/
│       ├── README.md
│       └── component-web.md
├── ddd/
│   ├── README.md
│   ├── bounded-contexts.yaml
│   ├── bounded-context-map.md
│   └── ubiquitous-language/
│       ├── README.md
│       ├── app-shell.md
│       ├── home.md
│       ├── cv.md
│       ├── personal-projects.md
│       └── search.md
└── behavior/
    ├── README.md
    └── web/
        └── gherkin/
            ├── README.md
            ├── app-shell/
            │   ├── theme.feature
            │   ├── responsive.feature
            │   └── accessibility.feature
            ├── home/
            │   └── home.feature
            ├── cv/
            │   └── cv.feature
            ├── personal-projects/
            │   └── personal-projects.feature
            └── search/
                └── search.feature
```

## Target source tree

```
apps/wahidyankf-web/src/
├── app/                              # Next.js routes (thin glue, imports from contexts/)
│   ├── page.tsx                      # delegates to home BC
│   ├── cv/page.tsx
│   └── personal-projects/page.tsx
├── contexts/
│   ├── app-shell/
│   │   └── presentation/
│   │       ├── header.tsx
│   │       ├── footer.tsx
│   │       ├── theme-toggle.tsx
│   │       ├── nav.tsx
│   │       └── ...
│   ├── home/
│   │   └── presentation/
│   │       ├── hero.tsx
│   │       └── featured.tsx
│   ├── cv/
│   │   ├── application/
│   │   │   ├── work-history.ts
│   │   │   └── skills.ts
│   │   └── presentation/
│   │       ├── timeline.tsx
│   │       └── skill-grid.tsx
│   ├── personal-projects/
│   │   ├── application/
│   │   │   ├── projects.ts
│   │   │   └── filters.ts
│   │   └── presentation/
│   │       └── project-card.tsx
│   └── search/
│       ├── application/
│       │   ├── index-builder.ts
│       │   └── score.ts
│       └── presentation/
│           ├── search-input.tsx
│           └── search-results.tsx
└── test/                             # Test utilities — colocated where possible, untouched here
```

## Bounded contexts in detail

### `app-shell`

- **Code**: `apps/wahidyankf-web/src/contexts/app-shell/`
- **Layers**: `[presentation]`
- **Owns**: site chrome — header/footer, theme toggle (dark/light), responsive layout decisions, accessibility wiring (skip links, ARIA, color-contrast tokens).
- **Gherkin**: `behavior/web/gherkin/app-shell/{theme,responsive,accessibility}.feature`
- **Key terms** (glossary): `Theme`, `Breakpoint`, `Skip link`, `ARIA landmark`, `Color token`
- **Forbidden synonyms**: "layout" (overloaded by Next.js), "wrapper" (implementation jargon)

### `home`

- **Code**: `apps/wahidyankf-web/src/contexts/home/`
- **Layers**: `[presentation]`
- **Owns**: `/` landing page — intro hero, featured-project teaser, contact links.
- **Gherkin**: `behavior/web/gherkin/home/home.feature`
- **Key terms**: `Hero`, `Featured project teaser`, `Contact link`
- **Forbidden synonyms**: "landing page" (used inside `oseplatform-web` for a different concept)

### `cv`

- **Code**: `apps/wahidyankf-web/src/contexts/cv/`
- **Layers**: `[application, presentation]`
- **Owns**: `/cv` page. `application/` holds CV data (employment timeline, skills inventory) and projection helpers. `presentation/` renders the timeline + skill grid.
- **Gherkin**: `behavior/web/gherkin/cv/cv.feature`
- **Key terms**: `Work history entry`, `Skill cluster`, `Education entry`, `Tenure`
- **Forbidden synonyms**: "resume" (the codebase consistently uses CV; cross-link with synonym lock)

### `personal-projects`

- **Code**: `apps/wahidyankf-web/src/contexts/personal-projects/`
- **Layers**: `[application, presentation]`
- **Owns**: `/personal-projects` page. `application/` holds project records + filter logic. `presentation/` renders cards.
- **Gherkin**: `behavior/web/gherkin/personal-projects/personal-projects.feature`
- **Key terms**: `Project entry`, `Tech tag`, `Filter chip`
- **Forbidden synonyms**: "portfolio" (ambiguous with the whole site)

### `search`

- **Code**: `apps/wahidyankf-web/src/contexts/search/`
- **Layers**: `[application, presentation]`
- **Owns**: cross-area search. `application/` builds the search index over home/cv/personal-projects content + scoring; `presentation/` renders search input + results dropdown.
- **Gherkin**: `behavior/web/gherkin/search/search.feature`
- **Key terms**: `Search index`, `Search query`, `Result entry`, `Score`
- **Forbidden synonyms**: "find" (informal)

## Acceptance criteria (Gherkin)

```gherkin
Feature: wahidyankf-web DDD + new specs format adoption

  Scenario: spec tree validates
    Given specs/apps/wahidyankf/ exists with the canonical five-folder layout
    When the developer runs "rhino-cli specs validate-tree wahidyankf"
    Then the command exits 0 with "0 finding(s) for \"wahidyankf\""

  Scenario: bounded-contexts.yaml is well-formed
    Given specs/apps/wahidyankf/ddd/bounded-contexts.yaml declares 5 contexts
    And every declared "code" path exists with exactly the declared layers
    And every declared "glossary" file exists
    And every declared "gherkin" directory contains at least one .feature file
    When the developer runs "rhino-cli ddd bc wahidyankf"
    Then the command exits 0 with no findings

  Scenario: glossaries are well-formed
    Given each of the 5 ubiquitous-language/*.md files exists with required frontmatter and table header
    And every backticked code identifier in each glossary's terms table is found in the BC's code path
    And every feature reference resolves to an existing .feature file
    When the developer runs "rhino-cli ddd ul wahidyankf"
    Then the command exits 0 with no findings

  Scenario: pre-push gate runs DDD validators
    Given apps/wahidyankf-web/project.json declares "ddd bc wahidyankf" and "ddd ul wahidyankf" in its test:quick commands
    When the developer runs "nx run wahidyankf-web:test:quick"
    Then both DDD validators run before vitest
    And both exit 0 before unit tests start

  Scenario: spec-coverage per BC passes
    Given the wahidyankf-web spec-coverage target points at specs/apps/wahidyankf/behavior/web/gherkin/
    When the developer runs "nx run wahidyankf-web:spec-coverage"
    Then 0 step gaps are reported for any feature under any bounded-context folder

  Scenario: legacy fe/gherkin/ folder is gone
    Given the migration is complete
    When the developer lists specs/apps/wahidyankf/
    Then no "fe/" subfolder exists
    And every legacy .feature file is reachable under behavior/web/gherkin/<bc>/

  Scenario: source code reflects bounded contexts
    Given the refactor is complete
    When the developer lists apps/wahidyankf-web/src/contexts/
    Then exactly the directories app-shell, home, cv, personal-projects, search appear
    And each subdirectory contains exactly the layers declared in bounded-contexts.yaml
```

## Personas

- **Developer** (maintainer hat) — reshapes source and spec files, runs TDD cycles, checks that validators pass.
- **Spec author** (documentation hat) — writes glossaries, bounded-context map, and per-BC READMEs.
- **Refactor executor** (delivery-checklist hat) — follows the phased delivery checklist step by step.
- **`plan-executor` agent** — reads delivery.md and executes each checkbox in order.
- **`swe-typescript-dev` agent** — performs TypeScript source moves and import updates.

## User Stories

- As a developer, I want the wahidyankf-web spec tree to follow the canonical C4 + DDD layout so that `rhino-cli ddd bc wahidyankf` can validate structural invariants automatically.
- As a developer, I want source code organized by bounded context under `src/contexts/<bc>/<layer>/` so that a rename in one BC cannot silently break another.
- As a spec author, I want per-BC Gherkin folders under `behavior/web/gherkin/<bc>/` so that the `spec-coverage` target can report step gaps per bounded context.
- As a refactor executor, I want the phased TDD delivery checklist to guide me one BC at a time so that I can verify correctness after each discrete change.

## Product Risks

- **Gherkin features moved to wrong BC subfolder** — `spec-coverage` would miss step coverage for the misplaced feature and report a false green. Mitigation: step 2.1 lists exact source → destination paths.
- **Import path churn breaks `test:quick`** — any file not updated to the new `src/contexts/` path causes a TypeScript build failure. Mitigation: phased refactor (one BC at a time) with `nx run wahidyankf-web:typecheck` after each BC.
- **`bounded-contexts.yaml` schema error** — a typo in the registry causes `rhino-cli ddd bc wahidyankf` to fail with a parse error rather than a structural finding, masking real issues. Mitigation: step 3.2 validates the registry immediately after authoring.
- **Legacy `fe/gherkin/` folder not removed** — `specs validate-tree` would report the unexpected folder. Mitigation: step 2.4 explicitly removes it.

## Non-goals

- No new UI feature added by this plan.
- No change to deployment pipeline or `prod-wahidyankf-web` branch.
- No change to the `apps/wahidyankf-web/Dockerfile`, since wahidyankf-web is Vercel-only.
- No bumping of `apps/wahidyankf-web-fe-e2e/` step files — those remain pointed at the same routes; selectors and behaviors do not change in this plan.
