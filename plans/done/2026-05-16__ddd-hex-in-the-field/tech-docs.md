# Technical Documentation — DDD + Hex In-the-Field (F#)

## Directory Layout

The new trail lands here (paths relative to ose-public repo root):

```text
apps/ayokoding-web/content/en/learn/software-engineering/architecture/
└── ddd-hexagonal-in-practice/
    ├── _index.md                       # Trail-level index
    └── fp-in-the-field/
        ├── _index.md                   # Tutorial-level index
        ├── overview.md                 # Includes Prerequisites section
        ├── beginner.md                 # ~4-10 guides
        ├── intermediate.md             # ~8-14 guides
        ├── advanced.md                 # ~6-10 guides
        └── production.md               # ~2-6 guides (deployment, observability,
                                        #  failure-mode wiring)
```

Sibling pattern reference: existing
`apps/ayokoding-web/content/en/learn/software-engineering/architecture/domain-driven-design-ddd/in-fp-by-example/`
already ships `_index.md`, `overview.md`, `beginner.md`, `intermediate.md`,
`advanced.md` [Repo-grounded]. The new trail adds a `production.md` tier to
hold the deployment / observability / failure-mode guides that have no
equivalent in the by-example tracks.

## Naming Convention for Individual Guides

Each guide is a top-level `##` heading inside a difficulty-tier file, named:

```text
## Guide N — <topic>
```

Where `N` is monotonically increasing across the whole tutorial (1..N across
beginner → intermediate → advanced → production), `<topic>` is a short noun
phrase. Example: `## Guide 7 — Repository port + Npgsql adapter pair`.

Rationale: matches the existing by-example convention of numbered examples in
flat difficulty files [Repo-grounded — verified against `domain-driven-design-ddd/in-fp-by-example/beginner.md` file shape].

## Running Domain Mapping — `apps/ose-app-be`

Real F# files in the BE today [Repo-grounded — verified via `find` 2026-05-16]:

| Layer (current flat)                                      | Files                                                                                          |
| --------------------------------------------------------- | ---------------------------------------------------------------------------------------------- |
| `Domain/`                                                 | `Types.fs`, `RegulatorySource.fs`, `InternalPolicy.fs`, `GapAnalysis.fs`, `AiOrchestration.fs` |
| `Handlers/`                                               | `HealthHandler.fs`                                                                             |
| `Infrastructure/`                                         | `AppDbContext.fs`, `Migrations.fs`                                                             |
| `Contracts/`                                              | `ContractWrappers.fs`                                                                          |
| `Program.fs`                                              | Composition root                                                                               |
| `contexts/<context>/{domain,application,infrastructure}/` | `.gitkeep` only — intended layout                                                              |

Each guide either (a) cites one of the populated files via inline relative link
and excerpt, or (b) marks the snippet `_New file — intended layout, scaffolding
exists at apps/ose-app-be/src/OseAppBe/contexts/<context>/<layer>/_` and adds
the gap to a follow-up `migration-notes.md` cross-link.

## Guide Topic Map (concerns → guide candidates)

Each row is a wiring concern that AC-6 requires; the right column shows likely
guide titles. Final numbering is set during Phase 2–4 authoring.

| Concern                                      | Candidate guide titles                                                                    | Suggested executor                      |
| -------------------------------------------- | ----------------------------------------------------------------------------------------- | --------------------------------------- |
| Bounded context as a hexagon                 | "One context = one hexagon: layers, isolation invariants"                                 | `apps-ayokoding-web-in-the-field-maker` |
| Aggregate as application-service IO          | "Application service signatures take and return aggregates, not DTOs"                     | `apps-ayokoding-web-in-the-field-maker` |
| Repository port + Npgsql adapter             | "Repository port in F#; Npgsql adapter behind it"                                         | `apps-ayokoding-web-in-the-field-maker` |
| Domain event publisher port + adapter        | "DomainEventPublisher port; in-memory adapter for tests; outbox adapter for prod"         | `apps-ayokoding-web-in-the-field-maker` |
| Giraffe handler as primary adapter           | "Giraffe HTTP handler translates request DTO → command → aggregate → response DTO"        | `apps-ayokoding-web-in-the-field-maker` |
| Contract codegen consumed by handler         | "Handler consumes generated contract types; reference: `organiclever-contracts`"          | `apps-ayokoding-web-in-the-field-maker` |
| Cross-context integration via ACL            | "gap-analysis consuming regulatory-source through an Anti-Corruption Layer"               | `apps-ayokoding-web-in-the-field-maker` |
| Domain event flow inside a context           | "Domain events: aggregate emits, application service publishes, handler returns"          | `apps-ayokoding-web-in-the-field-maker` |
| Integration test wiring at hex boundary      | "Swap Npgsql adapter for in-memory adapter at the application service seam"               | `apps-ayokoding-web-in-the-field-maker` |
| Database integration test via docker-compose | "End-to-end DB test using `apps/ose-app-be/docker-compose.integration.yml`"               | `apps-ayokoding-web-in-the-field-maker` |
| AI orchestration port + adapter              | "`ai-orchestration` context: port-first design, adapter swap for tests"                   | `apps-ayokoding-web-in-the-field-maker` |
| Background job adapter (conditional)         | If a job port exists in `ose-app-be`: "Background job adapter"; else marked Out of scope. | `apps-ayokoding-web-in-the-field-maker` |

## Agent Invocation Sequence

Per the maker-checker-fixer pattern in
`repo-governance/development/pattern/maker-checker-fixer.md`:

1. **Per difficulty tier** (beginner → intermediate → advanced → production):
   1. `apps-ayokoding-web-in-the-field-maker` writes guides for the tier.
   2. `apps-ayokoding-web-in-the-field-checker` validates structure, density,
      prerequisite separation, dogfood density.
   3. `apps-ayokoding-web-in-the-field-fixer` resolves checker findings.
   4. Re-run checker until zero CRITICAL/HIGH findings on two consecutive
      passes.
2. **After all tiers complete**:
   1. `apps-ayokoding-web-facts-checker` validates external F# / Giraffe /
      Npgsql claims via `docs-validating-factual-accuracy`.
   2. `apps-ayokoding-web-facts-fixer` resolves findings.
   3. `apps-ayokoding-web-link-checker` validates internal + external links.
   4. `apps-ayokoding-web-link-fixer` resolves findings.
3. **E2E coverage**:
   1. `swe-e2e-dev` adds a Playwright navigation smoke under
      `apps/ayokoding-web-fe-e2e/`.

## Dogfooding Strategy — Snippet Coupling vs Drift

Two snippet modes are allowed; mode chosen per guide:

- **Mirror mode** (preferred): inline excerpt that copies the live file at
  authoring time, with a relative link directly under the code block:
  `Source: apps/ose-app-be/src/OseAppBe/Domain/RegulatorySource.fs` (relative link, plan was authored when real codebase existed).
  Cost: drifts silently if BE changes. Mitigation: `apps-ayokoding-web-facts-checker`
  is invoked on every plan-execution and on a 6-month cadence.
- **Intended-layout mode**: when the BE has scaffolding only (e.g.
  `contexts/regulatory-source/domain/.gitkeep`), the snippet shows the
  intended file with a `_New file — intended layout, scaffolding exists at
apps/ose-app-be/src/OseAppBe/contexts/regulatory-source/domain/_` callout and
  the snippet's PR-readiness is verified by `swe-fsharp-dev`.

Both modes are explicit; bare snippets without a source citation are forbidden
and flagged HIGH by `apps-ayokoding-web-in-the-field-checker`.

## Tutorial-Wide Diagram Budget

Per `docs-creating-in-the-field-tutorials` skill guidance (10–20 diagrams,
25–50% of 20–40 guides). Required diagrams (Mermaid `flowchart LR`):

1. One-context-one-hexagon — layers, ports, adapters.
2. Aggregate-flows-through-handler — request → DTO → command → aggregate → response DTO.
3. Repository-port-and-Npgsql-adapter — interface vs implementation seam.
4. Domain-event-publisher-port — in-memory adapter vs outbox adapter.
5. Cross-context-ACL — `gap-analysis` consuming `regulatory-source`.
6. Hex-boundary-integration-test — adapter swap at the seam.
7. docker-compose-integration-flow — host ↔ pgsql container ↔ test runner.

All diagrams use the WCAG-accessible palette enumerated in the skill:
Blue `#0173B2`, Orange `#DE8F05`, Teal `#029E73`, Purple `#CC78BC`, Brown `#CA9161`.

## Cross-Linking Plan (Phase 5)

Six files receive new links to the new trail:

- `apps/ayokoding-web/content/en/learn/software-engineering/architecture/_index.md`
- `apps/ayokoding-web/content/en/learn/software-engineering/architecture/overview.md`
- `apps/ayokoding-web/content/en/learn/software-engineering/architecture/domain-driven-design-ddd/_index.md`
- `apps/ayokoding-web/content/en/learn/software-engineering/architecture/domain-driven-design-ddd/overview.md`
- `apps/ayokoding-web/content/en/learn/software-engineering/architecture/hexagonal-architecture/_index.md`
- `apps/ayokoding-web/content/en/learn/software-engineering/architecture/hexagonal-architecture/overview.md`

[All six verified to exist via Bash 2026-05-16, Repo-grounded.]

Link text pattern: `Next step (production wiring): [DDD + Hexagonal in Practice — F# in the Field](/en/learn/software-engineering/architecture/ddd-hexagonal-in-practice/fp-in-the-field)`.

## Test Strategy

Tutorial content itself is not TDD — the deliverable is Markdown. TDD-shaped
executable work in this plan:

- **E2E navigation smoke** (`apps/ayokoding-web-fe-e2e/`): Red — add failing
  spec that navigates to `/learn/software-engineering/architecture/ddd-hexagonal-in-practice/fp-in-the-field/overview`,
  expects `h1` containing "DDD + Hexagonal in Practice". Green — Phase 1
  scaffolding makes the route exist. Refactor — share the smoke pattern with
  the sibling architecture tutorials if useful.
- **Link validation**: `nx run ayokoding-web:test:quick` already runs the
  ayokoding-web link validator [Repo-grounded — documented in
  `apps/ayokoding-web/README.md` per AGENTS.md]; new links must pass.
- **Markdown lint**: `npm run lint:md` is the gate.

## Dependencies

No new npm / NuGet / cargo packages. The plan only adds Markdown. Existing
runtime dependencies referenced (read-only): Giraffe, Npgsql, Microsoft.Data.Sqlite
or equivalent in `apps/ose-app-be` per its `OseAppBe.fsproj` [Unverified — confirm
via `Read` during Phase 2 authoring before citing version numbers].

## Rollback

Pure additive change. Rollback is `git rm -r apps/ayokoding-web/content/en/learn/software-engineering/architecture/ddd-hexagonal-in-practice/`
plus reverting the six cross-link edits plus removing the new
`apps/ayokoding-web-fe-e2e/` spec. No data migration, no schema change, no
runtime dependency.
