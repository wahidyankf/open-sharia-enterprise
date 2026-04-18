# PRD — Adopt wahidyankf-web

## Product Overview

`wahidyankf-web` is a personal portfolio Next.js 16 site that renders:

- A home page with About-Me, Top Skills / Languages / Frameworks (last 5
  years), Quick Links, and social links.
- A CV page with searchable, filterable, highlight-matched entries.
- A personal-projects index.
- A global theme toggle (dark / light with system preference detection).
- A site-wide search that filters list entries on the home and CV pages.
- Google Analytics and Google Tag Manager loaded via
  `@next/third-parties/google` (same package used elsewhere in the repo's
  Next.js stack).

After adoption the site is deployed via Vercel on the `prod-wahidyankf-web`
branch — identical to `ayokoding-web`, `oseplatform-web`, and
`organiclever-fe`. It ships its own unit tests, an axe-core accessibility
E2E smoke, and an `@amiceli/vitest-cucumber` Gherkin suite mirroring this
PRD's acceptance criteria.

## Personas

Personas are content-placement hats / agent roles, not external
stakeholder titles.

- **Visitor / recruiter** — arrives at `/`, wants to read the About Me,
  the top skills, and navigate to CV or projects within a few clicks.
- **Maintainer authoring content** — edits `app/data.ts` and
  `public/` assets; expects hot reload on `nx dev wahidyankf-web`.
- **Template adopter** — a future external user who forks
  `apps/wahidyankf-web/` as a starting point for their own portfolio.
  Expects every configuration file (Nx, Vitest, Playwright, Tailwind,
  oxlint, tsconfig) to be either identical to the other three Next.js
  apps or explicitly delta-documented, so their fork has zero unexplained
  bespoke behaviour.
- **AI agents consuming the file**:
  - `plan-checker` / `plan-execution-checker` — reads the functional
    requirements and Gherkin ACs to decide whether the implementation
    matches the plan.
  - `swe-typescript-dev` — reads the requirements list to know which
    routes/components must port over.
  - Future `apps-wahidyankf-web-content-maker` / `-checker` agents (not
    created in this plan) — would read the data shape and content
    expectations here.
  - Future `apps-wahidyankf-web-journal-maker` (not created in this plan)
    — will consume the app skeleton this plan lands to add `/journal`
    routes and a content pipeline.

## User Stories

- **US-1** — _As a visitor,_ I want to land on the site at `/` _so that_
  I can see a portfolio summary and quickly find the CV or projects.
- **US-2** — _As a visitor,_ I want to search across skills, languages,
  and frameworks _so that_ I can filter the list by typed text and see
  match highlights.
- **US-3** — _As a visitor,_ I want to open `/cv` and `/personal-projects`
  from the home page _so that_ I can read longer-form content.
- **US-4** — _As a visitor,_ I want to switch between light and dark
  themes _so that_ I can read comfortably regardless of system setting.
- **US-5** — _As a visitor using assistive tech,_ I want WCAG AA
  compliant contrast, keyboard navigability, and accessible ARIA roles
  _so that_ the site is usable without a mouse or with low vision.
- **US-6** — _As the maintainer,_ I want `nx dev wahidyankf-web` to start
  a local server on a unique port _so that_ it does not clash with the
  other three web apps' dev servers.
- **US-7** — _As the maintainer,_ I want `nx run wahidyankf-web:test:quick`
  to run unit tests + coverage gate in a few minutes _so that_ the
  pre-push hook stays fast.
- **US-8** — _As the maintainer,_ I want `nx run wahidyankf-web-e2e:test:e2e`
  to run a smoke + accessibility suite headlessly _so that_ regressions are
  caught automatically without manual intervention.
- **US-9** — _As the maintainer,_ I want pushing `main` → `prod-wahidyankf-web`
  to be a one-agent force-push _so that_ the deploy ritual matches the
  other three web apps.

## Functional Requirements

- **R1** — The home page (`app/page.tsx`) renders the About-Me section,
  Skills / Languages / Frameworks pills (top five-year, click-to-search),
  Quick Links to `/cv` and `/personal-projects`, and Connect-With-Me
  social buttons.
- **R2** — The CV page (`app/cv/page.tsx`) renders the full CV with search
  highlighting and the skill-click-to-filter behaviour the upstream app
  ships.
- **R3** — The personal-projects page (`app/personal-projects/page.tsx`)
  renders the upstream project list.
- **R4** — A shared `Navigation` component appears on every page (left
  sidebar on desktop, bottom nav on mobile).
- **R5** — A `ThemeToggle` controls a dark/light CSS variable theme,
  with system preference detection as the initial value.
- **R6** — A `SearchComponent` synchronises its input value with the URL
  `?search=` query string and persists across navigation between `/` and
  `/cv`.
- **R7** — A `HighlightText` component wraps matched substrings in a
  `<mark>` tag without breaking accessibility.
- **R8** — A `ScrollToTop` element appears after scrolling and returns to
  top on click.
- **R9** — Google Analytics and Google Tag Manager load through
  `@next/third-parties/google` when `NEXT_PUBLIC_GA_ID` /
  `NEXT_PUBLIC_GTM_ID` are set; the site renders correctly when they are
  absent.
- **R10** — The Nx project exposes `dev`, `build`, `start`, `typecheck`,
  `lint`, `test:unit`, `test:quick`, `spec-coverage` (mandatory per Nx
  Target Standards for all apps).
- **R11** — A sibling Nx project `wahidyankf-web-e2e` exposes `install`,
  `typecheck`, `lint`, `test:quick`, `test:e2e`, `test:e2e:ui`,
  `test:e2e:report`, `spec-coverage` matching the `organiclever-fe-e2e`
  shape.
- **R12** — A `prod-wahidyankf-web` environment branch is created
  from `main` after P5 passes, and an `apps-wahidyankf-web-deployer`
  agent supports the force-push-from-main workflow.

## Gherkin Acceptance Criteria

```gherkin
Feature: Home page renders the portfolio summary
  As a visitor
  I want to land on the home page
  So that I can see a portfolio summary

  Scenario: Home page shows About Me and skill pills
    Given I navigate to "/"
    Then the "About Me" section is visible
    And the "Top Skills Used in The Last 5 Years" section is visible
    And the "Top Programming Languages Used in The Last 5 Years" section is visible
    And the "Top Frameworks & Libraries Used in The Last 5 Years" section is visible

  Scenario: Home page exposes quick links
    Given I navigate to "/"
    When I look at the "Quick Links" section
    Then I see a link with text "View My CV" pointing to "/cv"
    And I see a link with text "Browse My Personal Projects" pointing to "/personal-projects"
```

```gherkin
Feature: Search filters list entries
  As a visitor
  I want to type a search term
  So that I can filter the displayed lists

  Scenario: Typing filters the skills list
    Given I navigate to "/"
    And the skills list contains "TypeScript"
    And the skills list contains "Haskell"
    When I type "TypeScript" into the search input
    Then the skills list shows "TypeScript"
    And the skills list does not show "Haskell"
    And the URL contains query parameter "search=TypeScript"

  Scenario: Clicking a skill pill navigates to CV with the search prefilled
    Given I navigate to "/"
    When I click the skill pill "Go"
    Then I am on "/cv"
    And the search input on "/cv" has value "Go"
```

```gherkin
Feature: CV page renders and filters
  As a visitor
  I want the CV page to render with search highlighting
  So that I can find relevant experience fast

  Scenario: CV page renders
    Given I navigate to "/cv"
    Then a heading containing "CV" is visible
    And the CV entries list is not empty

  Scenario: CV search highlights matches
    Given I navigate to "/cv?search=React"
    Then at least one CV entry contains a "<mark>" element
    And the marked text is "React"
```

```gherkin
Feature: Theme toggle persists preference
  As a visitor
  I want to switch between dark and light themes
  So that the site matches my reading preference

  Scenario: Toggling theme updates the document class
    Given I navigate to "/"
    And the document has theme "dark"
    When I click the theme toggle
    Then the document has theme "light"
```

```gherkin
Feature: Personal projects page renders the project list
  As a visitor
  I want to open the personal-projects page
  So that I can browse the maintainer's public projects

  Scenario: Personal projects page renders
    Given I navigate to "/personal-projects"
    Then a heading containing "Personal Projects" is visible
    And the personal-projects list is not empty
```

```gherkin
Feature: Accessibility AA compliance
  As a visitor using assistive tech
  I want the site to meet WCAG AA
  So that I can use the site without barriers

  Scenario: Home page has no axe-core violations
    Given I navigate to "/"
    When I run an axe-core scan with WCAG 2.1 AA rules
    Then zero violations are reported

  Scenario: CV page has no axe-core violations
    Given I navigate to "/cv"
    When I run an axe-core scan with WCAG 2.1 AA rules
    Then zero violations are reported
```

```gherkin
Feature: Quality gates on the new Nx project
  As the maintainer
  I want the standard Nx targets green
  So that pre-push and PR gates pass

  Scenario: test:quick passes with coverage
    Given the app source and unit tests are committed
    When I run "nx run wahidyankf-web:test:quick"
    Then the quality gate passes
    And code coverage meets the 80% line threshold
    And a coverage report is generated at "apps/wahidyankf-web/coverage/lcov.info"

  Scenario: spec-coverage passes
    Given all Gherkin feature files have corresponding step implementations
    When I run "nx run wahidyankf-web:spec-coverage"
    Then the spec-coverage gate passes

  Scenario: affected quality gate passes
    Given the full implementation is committed
    When I run "nx affected -t typecheck lint test:quick spec-coverage"
    Then all affected targets pass with exit code 0
```

> **Note**: This feature block describes plan-level verification criteria. It is NOT a runnable `quality-gates.feature` file — it is checked manually by `plan-execution-checker` against the final state of the implementation.

```gherkin
Feature: Production deployment wiring
  As the maintainer
  I want a production branch and deployer agent
  So that deployment matches the other three web apps

  Scenario: Production branch exists
    When I run "git branch -r"
    Then the output contains "origin/prod-wahidyankf-web"

  Scenario: Deployer agent definition exists
    When I read ".claude/agents/apps-wahidyankf-web-deployer.md"
    Then the file's frontmatter declares name "apps-wahidyankf-web-deployer"
    And the body describes a force-push from main to "prod-wahidyankf-web"
```

## Product Scope

**In scope (features)**:

- Home, CV, personal-projects pages (ported 1:1 from upstream)
- Search with URL sync, theme toggle, scroll-to-top, highlight text
- Google Analytics / GTM wiring via `@next/third-parties`
- Unit tests for every ported component + page
- Playwright-BDD E2E: smoke (all three pages reachable) + axe-core a11y

**Out of scope (features)**:

- **Personal journal route / feed / MDX pipeline** — deferred to a
  follow-up plan. This plan lands only the three upstream routes (`/`,
  `/cv`, `/personal-projects`). The app structure leaves a clear slot
  for a future `/journal` addition without refactor.
- Admin panel, CMS, or any dynamic content source beyond `app/data.ts`
- Internationalisation (site stays English-only — no Indonesian mirror)
- Server-side API routes or auth
- PWA / service worker support
- Image optimisation pipeline beyond Next.js defaults (upstream uses
  `images.unoptimized: true`; we preserve that for portability)
- External-facing "use this as a template" documentation (the app will
  function as a template structurally, but writing the adopter guide is
  a separate plan)

## Product-Level Risks

| Risk                                                                | Mitigation                                                                                                                  |
| ------------------------------------------------------------------- | --------------------------------------------------------------------------------------------------------------------------- |
| Tailwind 3 → 4 migration silently breaks the green-on-black theme   | P2 dedicates a checkbox to running `npx @tailwindcss/upgrade` and Playwright smoke in P4 renders each page at both themes   |
| React 19 + `useSearchParams` changes Suspense boundaries subtly     | Upstream `page.tsx` already wraps `HomeContent` in `<Suspense>`; port preserves that; Gherkin US-2 ACs re-verify end-to-end |
| Dev port clashes with an existing app                               | Assign `3201` (first free above `organiclever-fe`'s `3200`) and document in top-level `CLAUDE.md` under the app's section   |
| axe-core flags upstream contrast (dark background + green-400 text) | Contrast pair `#4ade80` on `#111827` measures above WCAG AA (≈ 6.4:1); if scan reports violations, adjust tokens in P4      |
| Recruiters hit the site during the deploy-branch creation cutover   | P6 creates the branch from a verified green `main`; Vercel bindings happen post-merge so no partial deploy window           |
