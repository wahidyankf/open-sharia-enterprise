# PRD — oseplatform-web DDD + New Specs Format

## Target spec tree

```
specs/apps/oseplatform/
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
│       ├── landing.md
│       ├── content.md
│       ├── search.md
│       ├── rss-feed.md
│       ├── seo.md
│       └── health.md
├── behavior/
│   ├── README.md
│   ├── web/
│   │   └── gherkin/
│   │       ├── README.md
│   │       ├── app-shell/
│   │       │   ├── responsive.feature
│   │       │   ├── navigation.feature
│   │       │   ├── theme.feature
│   │       │   └── accessibility.feature
│   │       └── landing/
│   │           └── landing-page.feature
│   └── api/
│       └── gherkin/
│           ├── README.md
│           ├── health/
│           │   └── health.feature
│           ├── rss-feed/
│           │   └── rss-feed.feature
│           ├── search/
│           │   └── search.feature
│           ├── content/
│           │   └── content-retrieval.feature
│           └── seo/
│               └── seo.feature
└── cli/                            # UNTOUCHED — out of scope, separate plan
    └── gherkin/
        ├── README.md
        └── links-check.feature
```

## Target source tree

```
apps/oseplatform-web/src/
├── app/                            # Next.js routes (thin glue)
├── contexts/
│   ├── app-shell/
│   │   └── presentation/
│   ├── landing/
│   │   └── presentation/
│   ├── content/
│   │   ├── application/            # tRPC procedures
│   │   ├── infrastructure/         # filesystem / data adapters
│   │   └── presentation/           # rendering components
│   ├── search/
│   │   ├── application/
│   │   ├── infrastructure/
│   │   └── presentation/
│   ├── rss-feed/
│   │   ├── application/            # feed builder
│   │   └── infrastructure/         # XML serializer + route handler
│   ├── seo/
│   │   ├── application/            # metadata + sitemap builder
│   │   └── presentation/           # head tags
│   └── health/
│       ├── application/            # health probe + tRPC handler
│       └── presentation/           # /system/status page (if present)
├── lib/                            # cross-cutting utils — kept unless cleanly per-BC
└── test/                           # untouched
```

## Bounded contexts in detail

### `app-shell`

- **Code**: `src/contexts/app-shell/`
- **Layers**: `[presentation]`
- **Owns**: header, footer, theme toggle, navigation chrome, responsive breakpoints, accessibility wiring.
- **Gherkin**: `behavior/web/gherkin/app-shell/{responsive,navigation,theme,accessibility}.feature`
- **Key terms**: `Theme`, `Breakpoint`, `Nav item`, `Skip link`, `ARIA landmark`
- **Forbidden synonyms**: "layout" (overloaded by Next.js), "wrapper" (jargon)

### `landing`

- **Code**: `src/contexts/landing/`
- **Layers**: `[presentation]`
- **Owns**: marketing landing at `/` — hero, sections, calls-to-action.
- **Gherkin**: `behavior/web/gherkin/landing/landing-page.feature`
- **Key terms**: `Hero`, `Section block`, `CTA`
- **Forbidden synonyms**: "home" (used by `wahidyankf-web` for a different concept)

### `content`

- **Code**: `src/contexts/content/`
- **Layers**: `[application, infrastructure, presentation]`
- **Owns**: content retrieval (tRPC procedures) + rendering. `application/` holds the `content` tRPC router and DTOs. `infrastructure/` holds filesystem readers + content-source adapters. `presentation/` renders article + content-list views.
- **Gherkin**: `behavior/api/gherkin/content/content-retrieval.feature` + (eventually) UI rendering scenarios under `behavior/web/gherkin/content/`.
- **Key terms**: `Article`, `Content source`, `Slug`, `Frontmatter`, `Render pipeline`
- **Forbidden synonyms**: "post" (blog-platform jargon)

### `search`

- **Code**: `src/contexts/search/`
- **Layers**: `[application, infrastructure, presentation]`
- **Owns**: search backend (tRPC) + UI. `application/` builds the query interpreter and result scoring. `infrastructure/` owns the search index implementation. `presentation/` renders search input + results dropdown.
- **Gherkin**: `behavior/api/gherkin/search/search.feature` + (eventually) UI scenarios.
- **Key terms**: `Query`, `Search index`, `Result entry`, `Score`, `Snippet`
- **Forbidden synonyms**: "find" (informal), "lookup" (implementation jargon)

### `rss-feed`

- **Code**: `src/contexts/rss-feed/`
- **Layers**: `[application, infrastructure]`
- **Owns**: RSS feed generation route (`/rss.xml` or similar). `application/` aggregates published articles into feed entries; `infrastructure/` writes the XML stream to the response.
- **Gherkin**: `behavior/api/gherkin/rss-feed/rss-feed.feature`
- **Key terms**: `Feed`, `Feed entry`, `Channel metadata`
- **Forbidden synonyms**: "subscription" (different concept — payment subscription)

### `seo`

- **Code**: `src/contexts/seo/`
- **Layers**: `[application, presentation]`
- **Owns**: SEO metadata generation + sitemap generation. `application/` computes per-route metadata + the sitemap entry list. `presentation/` injects metadata into Next.js `<head>` via `generateMetadata`.
- **Gherkin**: `behavior/api/gherkin/seo/seo.feature`
- **Key terms**: `Meta tag`, `Sitemap entry`, `Canonical URL`, `OpenGraph card`
- **Forbidden synonyms**: "meta" alone (ambiguous with Next.js metadata API surface)

### `health`

- **Code**: `src/contexts/health/`
- **Layers**: `[application, presentation]`
- **Owns**: health probe (tRPC `health` procedure) + system-status diagnostic page (if rendered as a route).
- **Gherkin**: `behavior/api/gherkin/health/health.feature`
- **Key terms**: `Health probe`, `Status tile`, `UP / DOWN / UNKNOWN`
- **Forbidden synonyms**: "ping" (overloaded), "heartbeat" (different telemetry concept)

## Acceptance criteria (Gherkin)

```gherkin
Feature: oseplatform-web DDD + new specs format adoption

  Scenario: spec tree validates
    Given specs/apps/oseplatform/ exists with the canonical five-folder layout
    When the developer runs "rhino-cli specs validate-tree oseplatform"
    Then the command exits 0 with "0 finding(s) for \"oseplatform\""
    And the legacy c4/ folder no longer exists at the spec root

  Scenario: bounded-contexts.yaml is well-formed
    Given specs/apps/oseplatform/ddd/bounded-contexts.yaml declares 7 contexts
    And every declared "code" path exists with exactly the declared layers
    And every declared "glossary" file exists
    And every declared "gherkin" directory contains at least one .feature file
    When the developer runs "rhino-cli ddd bc oseplatform"
    Then the command exits 0 with no findings

  Scenario: glossaries are well-formed
    Given each of the 7 ubiquitous-language/*.md files exists with required frontmatter and table header
    And every backticked code identifier in each glossary's terms table is found in the BC's code path
    And every feature reference resolves to an existing .feature file
    When the developer runs "rhino-cli ddd ul oseplatform"
    Then the command exits 0 with no findings

  Scenario: tRPC slug is "api", not "be"
    Given the migration is complete
    When the developer lists specs/apps/oseplatform/behavior/
    Then the directory contains "web/" and "api/"
    And no "be/" subdirectory exists
    And specs/apps/oseplatform/components/api/component-api.md exists
    And specs/apps/oseplatform/components/be/ does not exist

  Scenario: pre-push gate runs DDD validators
    Given apps/oseplatform-web/project.json declares "ddd bc oseplatform" and "ddd ul oseplatform" in test:quick commands
    When the developer runs "nx run oseplatform-web:test:quick"
    Then both DDD validators run before vitest
    And both exit 0 before unit tests start

  Scenario: spec-coverage covers both perspectives via single target
    Given oseplatform-web has a single spec-coverage target running both perspectives sequentially
    When the developer runs "nx run oseplatform-web:spec-coverage"
    Then both behavior/web/gherkin and behavior/api/gherkin are validated
    And 0 step gaps are reported across all bounded-context folders in both perspectives

  Scenario: source code reflects bounded contexts
    Given the refactor is complete
    When the developer lists apps/oseplatform-web/src/contexts/
    Then exactly the directories app-shell, landing, content, search, rss-feed, seo, health appear
    And each subdirectory contains exactly the layers declared in bounded-contexts.yaml
    And src/server/ no longer exists as a flat parent of all server logic

  Scenario: cli specs are not touched
    Given the migration is complete
    When the developer lists specs/apps/oseplatform/cli/gherkin/
    Then the legacy file links-check.feature is unchanged
    And specs/apps/oseplatform/cli/gherkin/README.md is unchanged
```

## Personas

- **Developer** (maintainer hat) — reshapes source and spec files, runs tRPC router split, checks that validators pass.
- **Spec author** (documentation hat) — writes glossaries, bounded-context map, per-BC READMEs, and slug-vs-container note.
- **Refactor executor** (delivery-checklist hat) — follows the phased delivery checklist step by step.
- **`plan-executor` agent** — reads delivery.md and executes each checkbox in order.
- **`swe-typescript-dev` agent** — performs TypeScript source moves, tRPC router extraction, and import updates.

## User Stories

- As a developer, I want the oseplatform-web spec tree to follow the canonical C4 + DDD layout so that `rhino-cli ddd bc oseplatform` can validate structural invariants automatically.
- As a developer, I want tRPC procedures split into per-BC `application/router.ts` files under `src/contexts/<bc>/` so that a change to one BC's router cannot silently break another.
- As a spec author, I want the `be` slug renamed to `api` and documented with a slug-vs-container explanation so that future contributors understand why the slug does not map to a separate deployable.
- As a refactor executor, I want the phased TDD delivery checklist to guide me one BC at a time so that I can verify correctness after each discrete change.

## Product Risks

- **Gherkin features moved to wrong BC subfolder** — `spec-coverage` would miss step coverage for the misplaced feature and report a false green. Mitigation: steps 2.1 and 2.2 list exact source → destination paths.
- **tRPC router split breaks `be-e2e` tests** — extracting a procedure incompletely causes HTTP 500s that the E2E tests catch. Mitigation: `nx run oseplatform-web-be-e2e:test:e2e` run after each BC's router extraction (phases 4.1–4.7).
- **`be → api` slug rename missed in downstream references** — old `be/gherkin/` paths still referenced in README, step files, or project.json. Mitigation: step 7.1 explicitly updates all references.
- **`bounded-contexts.yaml` schema error** — causes `rhino-cli ddd bc oseplatform` to fail with a parse error. Mitigation: step 3.2 validates the registry immediately after authoring.

## Non-goals

- No new product feature added.
- No tRPC API surface change visible to clients (`organiclever-be-e2e` style API contract). All routes preserve their procedure signatures; only the file paths change.
- No change to `oseplatform-cli` or its specs.
- No change to deployment pipeline, `prod-oseplatform-web` branch, or domain.
