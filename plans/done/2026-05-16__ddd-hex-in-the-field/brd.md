# Business Requirements Document — DDD + Hex In-the-Field (F#)

## Business Goal

Provide repository contributors and self-learners a single production-grade
guide tutorial showing how Domain-Driven Design aggregates wire through a
hexagonal F# / Giraffe codebase — using the live `apps/ose-app-be` as the
running, dogfooded domain rather than yet another Wlaschin order-taking recap.

## Business Impact

### Pain Points Today

- The four existing `by-example` tutorials (DDD-FP, DDD-OOP, Hex-FP, Hex-OOP)
  each teach their pattern in isolation. A contributor finishing all four still
  cannot answer: "how do I take a `RegulatorySource` aggregate, expose a create
  endpoint, persist via Npgsql, publish a domain event, and integration-test the
  whole hex boundary?" [Judgment call — based on existing tutorial structure
  reviewed at `apps/ayokoding-web/content/en/learn/software-engineering/software-architecture/`]
- Real wiring is touched only sparsely in advanced sections — not enough density
  for a contributor to copy-paste-adapt for `apps/ose-app-be` work [Unverified —
  recheck during authoring].
- `apps/ose-app-be` is mid-migration to a bounded-context layout: `contexts/`
  scaffolding (`.gitkeep`-only today) exists alongside flat `Domain/`,
  `Handlers/`, `Infrastructure/`, `Contracts/` directories that hold the actual
  F# code [Repo-grounded]. New contributors lack a guided path for completing
  the migration consistently.

### Expected Benefits

- Single tutorial trail from "I understand DDD and hex separately" to "I can
  ship a new bounded context in `ose-app-be` confidently".
- Reduced duplication: rather than writing 80 more F# examples reformulating
  known patterns, the new tutorial focuses on the ~20–40 wiring patterns the
  by-example tracks intentionally do not cover.
- Dogfooding: every snippet maps to a real file in `apps/ose-app-be`, so
  contributors see how the codebase actually fits the theory rather than a
  contrived order-taking demo.
- Onboarding ramp for the `ose-app-be` bounded-context migration: the tutorial
  doubles as a contributor playbook.

## Affected Roles (Hats)

Solo-maintainer repo — no sign-off ceremonies. The roles below are hats the
maintainer wears and agent consumers of the file.

- **Tutorial author hat** — wears it during Phase 2–4 writing; invokes the
  apps-ayokoding-web-in-the-field-maker / -checker / -fixer agents.
- **Backend engineer hat** — wears it when extracting real snippets from
  `apps/ose-app-be` and when reconciling tutorial guidance with the BE's
  in-flight `contexts/` migration.
- **Reviewer hat** — wears it during the plan-quality-gate loop and the
  apps-ayokoding-web-in-the-field-checker output.
- **Consuming agents**:
  - `apps-ayokoding-web-in-the-field-maker` (writes individual guides)
  - `apps-ayokoding-web-in-the-field-checker` (validates structure + annotation density)
  - `apps-ayokoding-web-in-the-field-fixer` (resolves checker findings)
  - `apps-ayokoding-web-link-checker` / `-link-fixer` (link integrity)
  - `apps-ayokoding-web-facts-checker` / `-facts-fixer` (verifies F# / Giraffe / Npgsql
    claims via the docs-validating-factual-accuracy skill)
  - `swe-fsharp-dev` (verifies F# snippets compile in the running codebase)
  - `swe-e2e-dev` (adds Playwright navigation smoke)

## Business-Level Success Metrics

- **Coverage parity**: the new tutorial covers, at minimum, every cross-cutting
  wiring concern listed in tech-docs.md §Guide Topic Map. [Judgment call —
  evaluated via the apps-ayokoding-web-in-the-field-checker run.]
- **Dogfood density**: ≥75% of code snippets reference, mirror, or extend a
  real file under `apps/ose-app-be/src/OseAppBe/` [Judgment call — verified by
  manual sampling during Phase 6 validation].
- **Prerequisite discipline**: zero re-teaching of DDD or hex fundamentals; the
  apps-ayokoding-web-in-the-field-checker (which embeds the
  `docs-validating-software-engineering-separation` skill (loaded by the checker agent)) reports clean.
- **Quality gates**: `nx run ayokoding-web:test:quick` passes; `npm run lint:md`
  passes; `nx run ayokoding-web-fe-e2e:test:e2e` passes (with new navigation
  smoke for the tutorial route).

## Business-Scope Non-Goals

- Not a Java / Kotlin / C# tutorial — the OOP track is a separate future plan.
- Not a `domain-driven-design-ddd` or `hexagonal-architecture` re-teaching — the
  audience prerequisite is hard.
- Not a refactor of `apps/ose-app-be` — the BE is consulted, not modified.
- Not an Indonesian translation in this plan — no `content/id/` mirror exists
  for the architecture subtree today [Repo-grounded].
- Not an 80-example sibling tutorial — explicitly user-rejected.

## Business Risks and Mitigations

| Risk                                                                                                             | Mitigation                                                                                                                                                   |
| ---------------------------------------------------------------------------------------------------------------- | ------------------------------------------------------------------------------------------------------------------------------------------------------------ |
| `apps/ose-app-be` evolves during authoring; snippets go stale.                                                   | Inline excerpt + relative link to the real source file; treat divergence as a finding for `apps-ayokoding-web-facts-checker` rather than silent drift.       |
| `contexts/` migration in `ose-app-be` lags the tutorial — tutorial describes intent the code does not yet match. | Where intent ≠ current code, mark the gap inline (`_Intended layout — current source lives at <path>_`) and add the gap to a `migration-notes.md` follow-up. |
| Scope creep into an 80-example sibling.                                                                          | Hard guide-count ceiling of 40 enforced by tech-docs.md and prd.md acceptance criteria; plan-checker flags any breach.                                       |
| Audience tries to skip the by-example prerequisite.                                                              | Explicit prerequisite statement at `overview.md`; apps-ayokoding-web-in-the-field-checker verifies separation.                                               |
| Guide annotation density drift below 1.0/code-line.                                                              | apps-ayokoding-web-in-the-field-checker enforces 1.0–2.25 density per code block; failures route to apps-ayokoding-web-in-the-field-fixer.                   |
