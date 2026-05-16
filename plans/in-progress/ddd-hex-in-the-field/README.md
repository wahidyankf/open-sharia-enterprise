# DDD + Hexagonal Architecture — In-the-Field (FP + OOP)

## Status

**Stage**: In-progress
**Created**: 2026-05-16
**Identifier**: `ddd-hex-in-the-field`
**Scope expanded 2026-05-16**: added OOP track parallel to FP track. User directive: "make sure we have both oop and fp version."
**Target tutorial paths**:

- FP: `apps/ayokoding-web/content/en/learn/software-engineering/architecture/ddd-hexagonal-in-practice/fp-in-the-field/`
- OOP: `apps/ayokoding-web/content/en/learn/software-engineering/architecture/ddd-hexagonal-in-practice/oop-in-the-field/`

**Running domains**:

- FP: `apps/ose-app-be` (F# / Giraffe / Npgsql) — four bounded contexts: `regulatory-source`, `internal-policy`, `gap-analysis`, `ai-orchestration`.
- OOP: `apps/organiclever-be` (Java 24 / Spring Boot 4) — minimal scaffold today (health controller only); heavy intended-layout mode for snippets.

## Context

The repo already ships four `by-example` tutorials at
`apps/ayokoding-web/content/en/learn/software-engineering/architecture/` [Repo-grounded]:

| Tutorial                                  | Examples (claimed by user) | Stack                                           |
| ----------------------------------------- | -------------------------- | ----------------------------------------------- |
| `domain-driven-design-ddd/fp-by-example`  | 80 [Judgment call]         | F# (Wlaschin "Domain Modeling Made Functional") |
| `domain-driven-design-ddd/oop-by-example` | 80 [Judgment call]         | Java 21+ / Kotlin / C# 12+                      |
| `hexagonal-architecture/fp-by-example`    | 80 [Judgment call]         | F#                                              |
| `hexagonal-architecture/oop-by-example`   | 80 [Judgment call]         | Java / Kotlin / C#                              |

These tutorials teach DDD (_what_ to model) and hex (_where_ to put it)
separately. Integration is touched only sparsely (DDD OOP intermediate Ex 44;
hex OOP and hex FP advanced Ex 74) [Unverified — recheck before authoring].
That is not enough for production wiring: how does an aggregate flow through a
real Giraffe route, a repository port, a Npgsql adapter, a domain event
publisher, and an integration test boundary?

User explicitly rejected building another 80-example by-example sibling: too
much duplication with both existing tracks. User chose an **in-the-field guide**
instead, focused on production wiring patterns using `apps/ose-app-be` as the
running, dogfooded domain rather than recapping Wlaschin's order-taking story.

## Scope

**In scope**:

- New in-the-field tutorial at
  `apps/ayokoding-web/content/en/learn/software-engineering/architecture/ddd-hexagonal-in-practice/fp-in-the-field/`.
- 20–40 production-grade guides per `docs-creating-in-the-field-tutorials`
  skill (1.0–2.25 annotation density, five-part guide structure, standard library
  first principle).
- Running domain = `apps/ose-app-be` F# code, dogfooded.
- Prerequisite statement at tutorial overview (audience MUST have completed
  `domain-driven-design-ddd/fp-by-example` and `hexagonal-architecture/fp-by-example`)
  per `docs-validating-software-engineering-separation` skill — no re-teaching DDD
  or hex individually.
- Cross-links from existing DDD-FP and Hex-FP `_index.md` + overview pages.
- E2E navigation smoke test in `apps-ayokoding-web-fe-e2e` for the new path.
- Markdown quality gate (Prettier + markdownlint-cli2) and link-checker pass.

**Out of scope (deferred)**:

- Kotlin / C# in-the-field tracks (OOP track is Java-only — single-language analog of the F#-only FP track).
- Changes to the four existing by-example tutorials beyond cross-links.
- Changes to `ose-app-be` code itself.
- Indonesian translation — no `content/id/` mirror exists today for the architecture
  subtree [Repo-grounded]; defer to a follow-up plan if the `ayokoding-web` content
  policy requires it.
- A combined "DDD + Hex by-example" tutorial (user explicitly ruled out).

## Documents

- [brd.md](./brd.md) — business rationale (why a wiring guide vs another 80-example).
- [prd.md](./prd.md) — product requirements + Gherkin acceptance criteria.
- [tech-docs.md](./tech-docs.md) — technical approach (directory layout, guide
  topic mapping, agent invocation sequence, dogfooding strategy).
- [delivery.md](./delivery.md) — sequential `- [ ]` checklist organised by phase.

## Approach Summary

- Phase 1 — Scaffold: create `ddd-hexagonal-in-practice/fp-in-the-field/`
  directory, `_index.md`, `overview.md` with prerequisite statement, four
  difficulty pages (`beginner.md`, `intermediate.md`, `advanced.md`, `production.md`).
- Phase 2 — Beginner guides (~6–10): one context = one hexagon, aggregate as
  application-service IO, repository port stub, Giraffe handler skeleton.
- Phase 3 — Intermediate guides (~8–14): Npgsql adapter, contract codegen
  consumed by handler, integration test wiring at the hex boundary, domain
  event publisher port.
- Phase 4 — Advanced guides (~6–10): cross-context ACL adapter, docker-compose
  integration harness, AI orchestration port + adapter, background-job adapter
  (if applicable), failure-mode patterns.
- Phase 5 — Cross-links: update both DDD-FP and Hex-FP `_index.md` + `overview.md`.
- Phase 6 — Validation: markdown lint, link checker, E2E navigation smoke,
  apps-ayokoding-web maker→checker→fixer loop.
- Phase 7 — Archival.

## Worktree

Worktree path: `worktrees/ddd-hex-in-the-field/`

Provision before execution (run from `apps/ayokoding-web` parent — i.e. the
ose-public repo root):

```bash
claude --worktree ddd-hex-in-the-field
```

ose-public repo overrides the default platform-binding worktree location and lands
worktrees at `worktrees/<name>/` in the repo root per
[Worktree Path Convention](../../../repo-governance/conventions/structure/worktree-path.md).

## Quality Gate Handoff

After authoring, the calling context should run the [plan-quality-gate workflow](../../../repo-governance/workflows/plan/plan-quality-gate.md)
(plan-checker → plan-fixer iterative loop) before plan-execution.
