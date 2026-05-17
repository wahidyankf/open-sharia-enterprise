# Product Requirements Document — DDD + Hex In-the-Field (F#)

## Product Overview

A new in-the-field tutorial under
`apps/ayokoding-web/content/en/learn/software-engineering/architecture/ddd-hexagonal-in-practice/fp-in-the-field/`
that teaches production wiring of DDD aggregates through a hexagonal
F# / Giraffe / Npgsql codebase. Audience: contributors who have already
completed both `domain-driven-design-ddd/in-fp-by-example` and
`hexagonal-architecture/in-fp-by-example`. Running domain: `apps/ose-app-be`.

## Personas

Solo-maintainer hats and consuming agents.

### Persona P1 — Contributor onboarding to `ose-app-be`

A maintainer hat that has read the four existing `by-example` tracks, knows F#,
and is about to add or migrate a bounded context in `apps/ose-app-be`. Needs a
concrete wiring playbook keyed to real files in the codebase.

### Persona P2 — Self-learner consolidating DDD + hex

A reader who finished the FP by-example tracks and wants a single trail showing
how the two ideas combine in a real product. Not modifying `ose-app-be`, but
wants the dogfooded mental model.

### Persona P3 — Validation agents

- `apps-ayokoding-web-in-the-field-checker` validates structure, density,
  prerequisite statement, dogfood density.
- `apps-ayokoding-web-facts-checker` validates external F# / Giraffe / Npgsql
  claims via `docs-validating-factual-accuracy`.
- `apps-ayokoding-web-link-checker` validates intra-repo + external links.

## User Stories

### US-1 — Production wiring playbook

> **As** a contributor onboarding to `apps/ose-app-be`,
> **I want** a guide tutorial that shows me how a DDD aggregate flows through a
> Giraffe handler, an application service, a repository port, a Npgsql adapter,
> and an integration test,
> **so that** I can ship a new bounded context confidently without re-deriving
> the wiring from scratch.

### US-2 — Prerequisite-respecting depth

> **As** a self-learner who has completed the FP by-example tracks,
> **I want** a tutorial that does NOT re-teach DDD aggregates or hex ports from
> scratch,
> **so that** my reading time goes into wiring patterns, not refresher material.

### US-3 — Dogfooded snippets

> **As** a contributor reading the tutorial,
> **I want** the F# snippets to mirror real files under
> `apps/ose-app-be/src/OseAppBe/`,
> **so that** I can map a guide chapter to a real PR I would open.

### US-4 — Cross-tutorial discoverability

> **As** a reader on `domain-driven-design-ddd/in-fp-by-example` or
> `hexagonal-architecture/in-fp-by-example`,
> **I want** a clear next-step link to the in-the-field tutorial,
> **so that** I do not miss the production wiring track.

### US-5 — Quality gates

> **As** the maintainer pushing to `main`,
> **I want** the tutorial to pass markdown lint, link checker, ayokoding-web
> unit tests, and a Playwright navigation smoke,
> **so that** I trust the publish without manual eyeball review.

## Acceptance Criteria (Gherkin)

### AC-1 — Tutorial scaffolding exists

```gherkin
Scenario: Tutorial directory scaffolded under en/ architecture content
  Given the ose-public repo at HEAD of the worktree branch
  When the plan execution completes Phase 1
  Then the directory "apps/ayokoding-web/content/en/learn/software-engineering/architecture/ddd-hexagonal-in-practice/fp-in-the-field/" exists
  And it contains "_index.md", "overview.md", "beginner.md", "intermediate.md", "advanced.md", "production.md"
  And the parent directory "ddd-hexagonal-in-practice/" contains an "_index.md" describing the trail
  And every file passes "npm run lint:md"
```

### AC-2 — Prerequisite statement enforced

```gherkin
Scenario: overview.md declares the by-example prerequisite verbatim
  Given the file "ddd-hexagonal-in-practice/fp-in-the-field/overview.md"
  When apps-ayokoding-web-in-the-field-checker runs
  Then overview.md contains an explicit Prerequisites section
  And the section names both "domain-driven-design-ddd/in-fp-by-example" and "hexagonal-architecture/in-fp-by-example" as required reading
  And the section states the tutorial does NOT re-teach DDD or hex fundamentals
  And no guide body re-explains aggregate, port, adapter, bounded context, or repository pattern from scratch
  And the apps-ayokoding-web-in-the-field-checker report contains zero CRITICAL or HIGH findings on separation
```

### AC-3 — Guide count within range

```gherkin
Scenario: Guide count lands inside the in-the-field skill range
  Given the docs-creating-in-the-field-tutorials skill target of 20 to 40 guides
  When all phases 2-4 complete
  Then the combined guide count across beginner.md, intermediate.md, advanced.md, production.md is at least 20 and at most 40
  And no single difficulty page contains fewer than 4 or more than 14 guides, except production.md which may contain 2–6 guides
  And every guide follows the five-part structure (Why It Matters, Standard Library First, Production Framework, Diagram if appropriate, Trade-offs)
```

### AC-4 — Annotation density per skill

```gherkin
Scenario: Every F# code block hits the 1.0 to 2.25 annotation density target
  Given any code block in beginner.md, intermediate.md, advanced.md, or production.md
  When apps-ayokoding-web-in-the-field-checker samples the block
  Then the ratio of "// =>" annotation lines to code lines is at least 1.0 and at most 2.25
  And annotations focus on framework behavior, configuration impact, integration points, security implications, or performance characteristics
  And no annotation re-explains base F# syntax assumed by the prerequisite
```

### AC-5 — Dogfooded snippets

```gherkin
Scenario: F# snippets map to real apps/ose-app-be source
  Given the production-grade code blocks across all four difficulty pages
  When the maintainer samples 20 snippets at random
  Then at least 15 of 20 reference, mirror, or extend a real file at "apps/ose-app-be/src/OseAppBe/Domain/", "Handlers/", "Infrastructure/", "Contracts/", or "contexts/<context>/"
  And each dogfooded snippet either inline-cites the source file with a relative link or explicitly states "_New file_" / "_Intended layout — current source lives at <path>_"
```

### AC-6 — Coverage of wiring concerns

```gherkin
Scenario: Cross-cutting wiring concerns are each covered by at least one guide
  Given the topic map in tech-docs.md
  When the tutorial is complete
  Then there is at least one guide covering each of:
    | concern                                                                |
    | Bounded context as a hexagon (isolation invariants)                    |
    | Aggregate as application-service input/output through hex ports        |
    | Repository port + Npgsql adapter pair                                  |
    | Domain event publisher port + outbox or in-memory adapter              |
    | Giraffe HTTP handler as primary (driving) adapter                      |
    | Contract codegen consumed by handler (link out to organiclever-contracts) |
    | Cross-context integration via ACL adapter                              |
    | Domain event flow inside a single context                              |
    | Integration test wiring at the hex boundary (adapter swap)             |
    | Database integration test via docker-compose harness                   |
    | AI orchestration port + adapter                                        |
  And background job adapter is covered if "apps/ose-app-be" exposes one, otherwise marked _Out of scope — no current background-job port_
```

### AC-7 — Cross-tutorial backlinks

```gherkin
Scenario: Existing DDD-FP and Hex-FP tutorials surface the new in-the-field trail
  Given the existing files:
    | path                                                                                                            |
    | apps/ayokoding-web/content/en/learn/software-engineering/architecture/_index.md                                  |
    | apps/ayokoding-web/content/en/learn/software-engineering/architecture/overview.md                                |
    | apps/ayokoding-web/content/en/learn/software-engineering/architecture/domain-driven-design-ddd/_index.md         |
    | apps/ayokoding-web/content/en/learn/software-engineering/architecture/domain-driven-design-ddd/overview.md       |
    | apps/ayokoding-web/content/en/learn/software-engineering/architecture/hexagonal-architecture/_index.md            |
    | apps/ayokoding-web/content/en/learn/software-engineering/architecture/hexagonal-architecture/overview.md          |
  When Phase 5 completes
  Then each of those six files contains at least one link pointing at the new "ddd-hexagonal-in-practice/fp-in-the-field/" trail
  And the architecture root _index.md surfaces the new trail in its navigation
  And every new link passes "nx run ayokoding-web:test:quick" link validation
```

### AC-8 — Quality gates pass

```gherkin
Scenario: All authoring-time quality gates are green pre-push
  Given the worktree branch "worktrees/ddd-hex-in-the-field/"
  When the maintainer is ready to push
  Then "npm run lint:md" exits 0
  And "nx run ayokoding-web:test:quick" exits 0
  And "nx run ayokoding-web-fe-e2e:test:e2e" exits 0 with the new tutorial navigation smoke included
  And "npx nx affected -t typecheck lint test:quick spec-coverage" exits 0
  And the apps-ayokoding-web-in-the-field-checker report contains zero CRITICAL findings and zero HIGH findings
  And the apps-ayokoding-web-link-checker report contains zero CRITICAL or HIGH findings
  And the apps-ayokoding-web-facts-checker report contains zero CRITICAL or HIGH findings
```

### AC-9 — Post-push CI green

```gherkin
Scenario: GitHub Actions workflows pass after push to main
  Given the maintainer pushed the worktree branch directly to "main"
  When the GitHub Actions workflows triggered by the push complete
  Then every workflow run completes with status "success"
  And no workflow is bypassed, skipped manually, or rerun on the same SHA after intervention
```

### AC-10 — Plan archival

```gherkin
Scenario: Plan archives with completion date prefix
  Given all delivery checkboxes are ticked
  When the maintainer archives
  Then the plan folder is renamed via "git mv plans/in-progress/ddd-hex-in-the-field/ plans/done/YYYY-MM-DD__ddd-hex-in-the-field/" where YYYY-MM-DD is the completion date
  And "plans/in-progress/README.md" no longer lists the plan
  And "plans/done/README.md" lists the plan with the completion date
  And the archival commit message follows "chore(plans): move ddd-hex-in-the-field to done"
```

## Product Scope

### In Scope (Product Features)

- New trail directory `ddd-hexagonal-in-practice/fp-in-the-field/` with five
  difficulty-tier files plus `_index.md` and `overview.md`.
- 20–40 production-grade guides total across the four difficulty pages.
- Prerequisite statement on `overview.md`.
- Cross-links from existing DDD-FP and Hex-FP entry points and the architecture
  root `_index.md`.
- Navigation smoke test in `apps-ayokoding-web-fe-e2e`.
- Markdown quality gate compliance.

### Out of Scope (Product)

- Java / Kotlin / C# OOP equivalent track.
- Indonesian translation.
- Mutations to `apps/ose-app-be` source code.
- Re-explaining DDD or hex fundamentals.
- More than 40 total guides.

## Product Risks

- **Tutorial drift** if `apps/ose-app-be` evolves: mitigated by inline relative
  links to source files and a follow-up `migration-notes.md` for any divergence.
- **Annotation density slippage** when a new guide is added later without
  re-running the checker: mitigated by encoding the rule in
  `apps-ayokoding-web-in-the-field-checker`.
- **Discoverability gap** if architecture root `_index.md` is not updated:
  mitigated by AC-7 making the backlink explicit.
- **E2E breakage from path typo**: mitigated by the navigation smoke test added
  in Phase 6.
