# PRD — ayokoding-web DDD + New Specs Format

## Target spec tree

```
specs/apps/ayokoding/
├── README.md
├── product/
│   └── README.md
├── system-context/
│   ├── README.md
│   └── context.md                  # moved from c4/context.md
├── containers/
│   ├── README.md
│   └── container.md                # moved from c4/container.md
├── components/
│   ├── README.md
│   ├── web/
│   │   ├── README.md
│   │   └── component-web.md        # moved from c4/component-web.md
│   └── api/
│       ├── README.md
│       └── component-api.md        # moved+renamed from c4/component-be.md
├── ddd/
│   ├── README.md
│   ├── bounded-contexts.yaml
│   ├── bounded-context-map.md
│   └── ubiquitous-language/
│       ├── README.md
│       ├── app-shell.md
│       ├── content.md
│       ├── search.md
│       ├── i18n.md
│       ├── navigation.md
│       └── health.md
├── behavior/
│   ├── README.md
│   ├── web/
│   │   └── gherkin/
│   │       ├── README.md
│   │       ├── app-shell/
│   │       │   ├── responsive.feature
│   │       │   └── accessibility.feature
│   │       ├── content/
│   │       │   └── content-rendering.feature
│   │       ├── search/
│   │       │   └── search.feature
│   │       ├── i18n/
│   │       │   └── i18n.feature
│   │       └── navigation/
│   │           └── navigation.feature
│   └── api/
│       └── gherkin/
│           ├── README.md
│           ├── content/
│           │   └── content-api.feature
│           ├── search/
│           │   └── search-api.feature
│           ├── i18n/
│           │   └── i18n-api.feature
│           ├── navigation/
│           │   └── navigation-api.feature
│           └── health/
│               └── health-check.feature
├── cli/                            # UNTOUCHED — separate plan
│   └── gherkin/
│       ├── README.md
│       └── links-check.feature
└── build-tools/                    # UNTOUCHED — separate plan
    └── gherkin/
        └── index-generation/
            └── index-generation.feature
```

## Target source tree

```
apps/ayokoding-web/src/
├── app/                            # Next.js routes (thin glue)
├── middleware.ts                   # 1-line re-export from contexts/i18n/application/middleware.ts
├── contexts/
│   ├── app-shell/
│   │   └── presentation/
│   ├── content/
│   │   ├── application/            # tRPC content-api procedures
│   │   ├── infrastructure/         # filesystem / content-source adapters
│   │   └── presentation/           # rendering components
│   ├── search/
│   │   ├── application/
│   │   ├── infrastructure/
│   │   └── presentation/
│   ├── i18n/
│   │   ├── application/            # locale negotiation + middleware
│   │   ├── infrastructure/         # translation file loaders
│   │   └── presentation/           # locale switcher
│   ├── navigation/
│   │   ├── application/            # tRPC navigation-api
│   │   └── presentation/           # nav components
│   └── health/
│       └── application/            # health-check tRPC procedure
├── lib/                            # cross-cutting utils
└── test/                           # untouched
```

## Bounded contexts in detail

### `app-shell`

- **Code**: `src/contexts/app-shell/`
- **Layers**: `[presentation]`
- **Owns**: header, footer, responsive layout decisions, accessibility wiring. Note: theme + dark-mode chrome are **not** owned by app-shell here (ayokoding-web does not currently feature a theme toggle in the gherkin set; if added later, this glossary updates).
- **Gherkin**: `behavior/web/gherkin/app-shell/{responsive,accessibility}.feature`
- **Key terms**: `Breakpoint`, `Skip link`, `ARIA landmark`
- **Forbidden synonyms**: "layout" (overloaded), "wrapper" (jargon)

### `content`

- **Code**: `src/contexts/content/`
- **Layers**: `[application, infrastructure, presentation]`
- **Owns**: content rendering + tRPC content-api. `application/` exposes content-fetching procedures + DTOs. `infrastructure/` adapts filesystem reads + frontmatter parsing. `presentation/` renders article + content-list views.
- **Gherkin**: `behavior/web/gherkin/content/content-rendering.feature` (UI; **registry-canonical** path), `behavior/api/gherkin/content/content-api.feature` (HTTP; covered by `spec-coverage` only — registry limitation, see tech-docs § "Multi-perspective gherkin: workaround")
- **Key terms**: `Article`, `Content source`, `Frontmatter`, `Slug`, `Render pipeline`
- **Forbidden synonyms**: "post" (blog jargon), "page" (Next.js overloaded)

### `search`

- **Code**: `src/contexts/search/`
- **Layers**: `[application, infrastructure, presentation]`
- **Owns**: search backend (tRPC) + UI. `application/` query interpreter + scoring. `infrastructure/` index implementation. `presentation/` input + results dropdown.
- **Gherkin**: `behavior/web/gherkin/search/search.feature` (UI; **registry-canonical**), `behavior/api/gherkin/search/search-api.feature` (HTTP; covered by `spec-coverage` only — see tech-docs workaround)
- **Key terms**: `Query`, `Search index`, `Result entry`, `Score`, `Snippet`, `Locale-aware index`
- **Forbidden synonyms**: "find", "lookup"

### `i18n`

- **Code**: `src/contexts/i18n/`
- **Layers**: `[application, infrastructure, presentation]`
- **Owns**: locale switching, tRPC i18n-api, Next.js middleware (locale detection + redirect). `application/` locale negotiation logic + the middleware function. `infrastructure/` translation file loaders. `presentation/` locale switcher UI component.
- **Gherkin**: `behavior/web/gherkin/i18n/i18n.feature` (UI; **registry-canonical**), `behavior/api/gherkin/i18n/i18n-api.feature` (HTTP; covered by `spec-coverage` only — see tech-docs workaround)
- **Key terms**: `Locale`, `Default locale`, `Locale switcher`, `Translation table`, `Locale prefix`
- **Forbidden synonyms**: "language" (overloaded — language is a property of a locale), "translation" (an action; the artefact is `Translation table`)

### `navigation`

- **Code**: `src/contexts/navigation/`
- **Layers**: `[application, presentation]`
- **Owns**: top-level navigation + tRPC navigation-api (which exposes the nav structure to clients). `application/` produces the nav tree. `presentation/` renders the menu.
- **Gherkin**: `behavior/web/gherkin/navigation/navigation.feature` (UI; **registry-canonical**), `behavior/api/gherkin/navigation/navigation-api.feature` (HTTP; covered by `spec-coverage` only — see tech-docs workaround)
- **Key terms**: `Nav item`, `Nav tree`, `Active item`, `Breadcrumb`
- **Forbidden synonyms**: "menu" (UI-only word), "sidebar"

### `health`

- **Code**: `src/contexts/health/`
- **Layers**: `[application]`
- **Owns**: health-check tRPC procedure. No UI surface (no `/system/status` page in ayokoding today).
- **Gherkin**: `behavior/api/gherkin/health/health-check.feature`
- **Key terms**: `Health probe`, `UP / DOWN`
- **Forbidden synonyms**: "ping", "heartbeat"

## Acceptance criteria (Gherkin)

```gherkin
Feature: ayokoding-web DDD + new specs format adoption

  Scenario: spec tree validates
    Given specs/apps/ayokoding/ exists with the canonical five-folder layout
    When the developer runs "rhino-cli specs validate-tree ayokoding"
    Then the command exits 0 with "0 finding(s) for \"ayokoding\""
    And the legacy c4/ folder no longer exists at the spec root

  Scenario: bounded-contexts.yaml is well-formed
    Given specs/apps/ayokoding/ddd/bounded-contexts.yaml declares 6 contexts
    And every declared "code" path exists with exactly the declared layers
    When the developer runs "rhino-cli ddd bc ayokoding"
    Then the command exits 0 with no findings

  Scenario: glossaries are well-formed
    Given each of the 6 ubiquitous-language/*.md files exists with required frontmatter and table header
    When the developer runs "rhino-cli ddd ul ayokoding"
    Then the command exits 0 with no findings

  Scenario: tRPC slug is "api", not "be"
    Given the migration is complete
    When the developer lists specs/apps/ayokoding/behavior/
    Then the directory contains "web/" and "api/"
    And no "be/" subdirectory exists

  Scenario: i18n middleware is owned by the i18n BC
    Given the refactor is complete
    When the developer reads apps/ayokoding-web/src/middleware.ts
    Then the file is a one-line re-export from src/contexts/i18n/application/middleware.ts
    And the original middleware function body lives under src/contexts/i18n/application/

  Scenario: locale routing still works after middleware move
    Given the migration is complete
    When the developer requests "/" with Accept-Language "id"
    Then the response redirects to "/id"
    And when the developer requests "/" with Accept-Language "en"
    Then the response redirects to "/en"

  Scenario: pre-push gate runs DDD validators
    Given apps/ayokoding-web/project.json declares "ddd bc ayokoding" and "ddd ul ayokoding" in test:quick commands
    When the developer runs "nx run ayokoding-web:test:quick"
    Then both DDD validators run before vitest
    And both exit 0 before unit tests start

  Scenario: spec-coverage covers both perspectives via single target
    Given ayokoding-web has a single spec-coverage target running both perspectives sequentially
    When the developer runs "nx run ayokoding-web:spec-coverage"
    Then both behavior/web/gherkin and behavior/api/gherkin are validated
    And 0 step gaps are reported across all bounded-context folders in both perspectives

  Scenario: source code reflects bounded contexts
    Given the refactor is complete
    When the developer lists apps/ayokoding-web/src/contexts/
    Then exactly the directories app-shell, content, search, i18n, navigation, health appear
    And each subdirectory contains exactly the layers declared in bounded-contexts.yaml
    And src/server/ no longer exists

  Scenario: out-of-scope folders preserved
    Given the migration is complete
    When the developer lists specs/apps/ayokoding/
    Then cli/ exists with its legacy contents unchanged
    And build-tools/ exists with its legacy contents unchanged
```

## Non-goals

- No new product feature.
- No tRPC API surface change visible to clients (preserve all procedure signatures).
- No content authoring change.
- No new locale beyond English + Indonesian.
- No change to deployment pipeline or `prod-ayokoding-web` branch.
