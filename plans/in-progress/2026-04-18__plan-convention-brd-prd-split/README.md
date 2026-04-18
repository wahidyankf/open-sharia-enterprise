# Plan Convention: Split Requirements into BRD + PRD

**Status**: In Progress
**Created**: 2026-04-18
**Scope**: `ose-public` — governance convention + plan agents + plan skill + existing in-progress plan migration

## Context

The current [Plans Organization Convention](../../../governance/conventions/structure/plans.md) defines a four-document plan layout:

- `README.md` — context + navigation
- `requirements.md` — user stories, acceptance criteria (Gherkin), and business requirements lumped together
- `tech-docs.md` — architecture and implementation approach
- `delivery.md` — step-by-step checklist

Conflating business intent (why this exists, what value it creates, who benefits, what KPIs move) with product specification (user stories, Gherkin criteria, feature scope) inside a single `requirements.md` has two recurring failure modes:

1. **Business context gets crowded out.** When the reader opens `requirements.md`, user stories dominate and business impact degrades into an Overview sentence — so stakeholders reviewing a plan cannot easily answer "why are we doing this?" without hunting.
2. **Product scope gets tangled with business framing.** Engineers reading the file for Gherkin scenarios must skim past strategy sections, and product updates touch the same file as business rationale — producing noisy diffs and unclear ownership.

## Goal

Evolve the canonical plan structure from four documents to **five documents** by splitting `requirements.md` into two purpose-focused files:

- `brd.md` — **Business Requirements Document**. Business impact, goals, KPIs, stakeholders, value proposition, success metrics (business level).
- `prd.md` — **Product Requirements Document**. User stories, personas, Gherkin acceptance criteria, product scope, out-of-scope items, UX requirements.

Target plan layout (multi-file default):

```
YYYY-MM-DD__project-id/
├── README.md          # Context + scope + overview (entry point)
├── brd.md             # Business requirements: impact, KPIs, stakeholders, value
├── prd.md             # Product requirements: user stories, Gherkin criteria, scope
├── tech-docs.md       # Technical design: architecture, decisions, mechanics
└── delivery.md        # Step-by-step delivery checklist
```

## Scope

### In Scope

- Update `governance/conventions/structure/plans.md` to define five-document layout, BRD/PRD content rules, and updated single-file exception criteria.
- Update five plan agents under `.claude/agents/`: `plan-maker`, `plan-checker`, `plan-fixer`, `plan-executor`, `plan-execution-checker`.
- Update `.claude/skills/plan-creating-project-plans/SKILL.md` to reflect new structure.
- Update cross-references: `governance/development/infra/acceptance-criteria.md`, `docs/how-to/organize-work.md`, `AGENTS.md`, any README that quotes the old three-file split.
- Sync `.claude/` → `.opencode/` via `npm run sync:claude-to-opencode`.
- Migrate the one active in-progress plan (`2026-04-16__organiclever-fe-local-first/`) from `requirements.md` → `brd.md` + `prd.md` so the repository contains zero plans using the deprecated layout.

### Out of Scope

- **Archived plans in `plans/done/`** — historical records, left as-is.
- **Parent `ose-projects` plan convention** (`/Users/wkf/ose-projects/governance/conventions/structure/plans.md`) — mirrors the ose-public convention but lives in a different repo. Tracked as follow-up work, not bundled here, because updating it requires a separate parent-repo plan and this plan's Scope is ose-public only.
- **New `brd-` / `prd-` prefix naming for other documents** — this plan does not rename `tech-docs.md` or introduce further taxonomy changes.
- **Automated migration tooling** — the single active plan migrates by hand; no generator/codemod needed for one artifact.

## Approach Summary

1. **Author the convention change first** in `governance/conventions/structure/plans.md` so downstream documents have a stable referent.
2. **Cascade updates into the five plan agents and the creation skill**, keeping wording consistent so `plan-checker` and `plan-execution-checker` agree on what "compliant plan" means.
3. **Update cross-linked docs** (`AGENTS.md`, `organize-work.md`, `acceptance-criteria.md`) in the same commit set so no reference lags.
4. **Run the OpenCode sync** and verify `.opencode/` mirrors match.
5. **Migrate the one active in-progress plan** last — exercises the new structure end-to-end, and confirms `plan-executor` still resolves its delivery checklist correctly after the change.
6. **Run quality gates** (markdown lint, affected-project tests, plan-checker against this plan itself).

## Plan Documents

- [Requirements (BRD)](./brd.md) — business goals, impact, success metrics
- [Requirements (PRD)](./prd.md) — user stories (Gherkin), product scope
- [Technical Documentation](./tech-docs.md) — affected files, rename strategy, migration mechanics
- [Delivery Checklist](./delivery.md) — phased step-by-step execution

> **Note**: This plan is itself authored in the **new five-document layout** (README + brd + prd + tech-docs + delivery). It serves as the canonical reference example for the convention it introduces.
