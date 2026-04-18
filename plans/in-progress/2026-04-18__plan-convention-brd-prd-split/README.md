# Plan Convention: Split Requirements into BRD + PRD

**Status**: In Progress
**Created**: 2026-04-18
**Scope**: `ose-public` — governance convention + plan agents + plan workflows + plan skill + existing in-progress plan migration

## Context

The current [Plans Organization Convention](../../../governance/conventions/structure/plans.md) defines a four-document plan layout:

- `README.md` — context + navigation
- `requirements.md` — user stories, acceptance criteria (Gherkin), and business requirements lumped together
- `tech-docs.md` — architecture and implementation approach
- `delivery.md` — step-by-step checklist

Conflating business intent (why this exists, what value it creates, who benefits, what KPIs move) with product specification (user stories, Gherkin criteria, feature scope) inside a single `requirements.md` has two recurring failure modes:

1. **Business context gets crowded out.** When the reader opens `requirements.md`, user stories dominate and business impact degrades into an Overview sentence — so the author on cold re-read, or a reviewer at code review time, cannot easily answer "why are we doing this?" without hunting.
2. **Product scope gets tangled with business framing.** Engineers reading the file for Gherkin scenarios must skim past strategy sections, and product updates touch the same file as business rationale — producing noisy diffs and unclear ownership.

## Goal

Evolve the canonical plan structure from four documents to **five documents** by splitting `requirements.md` into two purpose-focused files:

- `brd.md` — **Business Requirements Document**. Business impact, goals, intent, affected roles, success metrics (business level). Content-placement container, not a sign-off artifact — code review is the only approval gate in this repo.
- `prd.md` — **Product Requirements Document**. User stories, personas, Gherkin acceptance criteria, product scope, out-of-scope items, UX requirements.

Target plan layout (multi-file default):

```
YYYY-MM-DD__project-id/
├── README.md          # Context + scope + overview (entry point)
├── brd.md             # Business requirements: impact, intent, affected roles, success metrics
├── prd.md             # Product requirements: user stories, Gherkin criteria, scope
├── tech-docs.md       # Technical design: architecture, decisions, mechanics
└── delivery.md        # Step-by-step delivery checklist
```

## Scope

### In Scope

- Update `governance/conventions/structure/plans.md` to define five-document layout, BRD/PRD content rules, and updated single-file exception criteria.
- Update four plan agents under `.claude/agents/`: `plan-maker`, `plan-checker`, `plan-fixer`, `plan-execution-checker` (the prior `plan-executor` agent was removed in a separate refactor; plan execution is now orchestrated directly by the calling context via the [plan-execution workflow](../../../governance/workflows/plan/plan-execution.md)).
- Update two plan workflows under `governance/workflows/plan/`: `plan-quality-gate.md` (completeness bullet enumerates five docs) and `plan-execution.md` (adds context-consultation note; verifies no stale `requirements.md` references).
- Update `.claude/skills/plan-creating-project-plans/SKILL.md` to reflect new structure.
- Update cross-references: `governance/development/infra/acceptance-criteria.md`, `docs/how-to/organize-work.md`, `AGENTS.md`, any README that quotes the old four-document layout.
- Sync `.claude/` → `.opencode/` via `npm run sync:claude-to-opencode`.
- Migrate the one active in-progress plan (`2026-04-16__organiclever-fe-local-first/`) from `requirements.md` → `brd.md` + `prd.md` so the repository contains zero plans using the deprecated layout.

### Out of Scope

- **Archived plans in `plans/done/`** — historical records, left as-is.
- **Parent `ose-projects` plan convention** (the sibling `plans.md` at `governance/conventions/structure/plans.md` inside the `ose-projects` parent repo) — mirrors the ose-public convention but lives in a different repo. Tracked as follow-up work, not bundled here, because updating it requires a separate parent-repo plan and this plan's Scope is ose-public only.
- **New `brd-` / `prd-` prefix naming for other documents** — this plan does not rename `tech-docs.md` or introduce further taxonomy changes.
- **Automated migration tooling** — the single active plan migrates by hand; no generator/codemod needed for one artifact.

## Approach Summary

1. **Author the convention change first** in `governance/conventions/structure/plans.md` so downstream documents have a stable referent.
2. **Cascade updates into the four plan agents**, keeping wording consistent so `plan-checker` and `plan-execution-checker` agree on what "compliant plan" means.
3. **Update the two plan workflows** (`plan-quality-gate.md`, `plan-execution.md`) so quality-gate validation and execution mechanics stay consistent with the new convention.
4. **Update the creation skill and cross-linked docs** (`AGENTS.md`, `organize-work.md`, `acceptance-criteria.md`) in the same commit set so no reference lags.
5. **Run the OpenCode sync** and verify `.opencode/` mirrors match.
6. **Migrate the one active in-progress plan** last — exercises the new structure end-to-end, and confirms the plan-execution workflow still resolves its delivery checklist correctly after the change.
7. **Run quality gates** (markdown lint, affected-project tests, plan-checker against this plan itself).
8. **Record commits and archive the plan** — commit all changes thematically by domain, then move the plan folder to `plans/done/`.

## Plan Documents

- [Requirements (BRD)](./brd.md) — business goal, impact, affected roles, success metrics, non-goals, risks
- [Requirements (PRD)](./prd.md) — user stories (Gherkin), product scope
- [Technical Documentation](./tech-docs.md) — filename rationale, affected files, migration mechanics
- [Delivery Checklist](./delivery.md) — phased step-by-step execution

> **Note**: This plan is itself authored in the **new five-document layout** (README + brd + prd + tech-docs + delivery). It serves as the canonical reference example for the convention it introduces.
