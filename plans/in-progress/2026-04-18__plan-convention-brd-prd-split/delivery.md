# Delivery Checklist

**Plan**: Plan Convention — Split Requirements into BRD + PRD
**Date**: 2026-04-18

Granular checkboxes per the [one checkbox = one action](../../../governance/conventions/structure/plans.md#granular-checklist-items-in-deliverymd) rule. Execute phases in order.

## Phase 1 — Update the canonical convention document

- [ ] Read current `governance/conventions/structure/plans.md` in full to map every section touching the four-doc layout.
- [ ] Rewrite the "Structure Decision" section to introduce the five-doc layout as the multi-file default.
- [ ] Rewrite the "Multi-File Structure" subsection to list `README.md`, `brd.md`, `prd.md`, `tech-docs.md`, `delivery.md`.
- [ ] Add a "Content-Placement Rules" subsection mirroring the rules in [tech-docs.md](./tech-docs.md#content-placement-rules) (business content → `brd.md`; product content → `prd.md`; ambiguous cross-cutting → split per convention).
- [ ] Update the Single-File Structure subsection so README sections include condensed BRD + condensed PRD coverage.
- [ ] Update the "Large Plan (Multi-File)" example to show the five-doc layout.
- [ ] Update the "Small Plan (Single-File)" example README outline to include Business rationale + Product requirements sections.
- [ ] Update the "Last Updated" footer date to 2026-04-18.
- [ ] Run `npm run lint:md` on the convention file and confirm zero violations.

## Phase 2 — Update plan agents under `.claude/agents/`

- [ ] Read `.claude/agents/plan-maker.md` and identify every mention of `requirements.md`.
- [ ] Update `plan-maker.md` to scaffold `README.md`, `brd.md`, `prd.md`, `tech-docs.md`, `delivery.md` for multi-file plans.
- [ ] Add content-placement guidance to `plan-maker.md` for `brd.md` and `prd.md`.
- [ ] Read `.claude/agents/plan-checker.md` and identify validation logic tied to `requirements.md`.
- [ ] Update `plan-checker.md` to validate presence of `brd.md` and `prd.md` in multi-file plans.
- [ ] Add a `plan-checker.md` rule to flag business content in `prd.md` and product content in `brd.md`.
- [ ] Read `.claude/agents/plan-fixer.md` and update fix instructions to move misplaced content into the correct file.
- [ ] Read `.claude/agents/plan-executor.md` and update any documentation references to the five-doc layout (behavioral change: none — still reads `delivery.md`).
- [ ] Read `.claude/agents/plan-execution-checker.md` and update acceptance-criteria validation to read from `prd.md`.
- [ ] Run `npm run lint:md` on all five updated agent files and confirm zero violations.

## Phase 3 — Update plan workflows under `governance/workflows/plan/`

- [ ] Read `governance/workflows/plan/plan-quality-gate.md` and locate the "Plan-Specific Validation" section.
- [ ] Update the completeness bullet (currently `"All required sections present (requirements, deliverables, checklists)"`) to enumerate the five canonical documents for multi-file plans (`README.md`, `brd.md`, `prd.md`, `tech-docs.md`, `delivery.md`).
- [ ] Add a clarifying note that the single-file exception still allows a single `README.md` when eligible per the convention.
- [ ] Read `governance/workflows/plan/plan-execution.md` and verify every `delivery.md` reference remains correct (no rename of `delivery.md`).
- [ ] Add a short context note in `plan-execution.md` that the executor MAY consult `brd.md` / `prd.md` / `tech-docs.md` when a delivery item is ambiguous.
- [ ] Grep `governance/workflows/plan/` for any mention of `requirements.md` and remove/update as needed.
- [ ] Run `npm run lint:md` on both workflow files and confirm zero violations.

## Phase 4 — Update skill + cross-referenced docs

- [ ] Read `.claude/skills/plan-creating-project-plans/SKILL.md` and identify layout references.
- [ ] Update `SKILL.md` to describe the five-doc layout and update any example.
- [ ] Grep repository for `requirements.md` references and enumerate every hit outside archived plans: `grep -r 'requirements\.md' governance/ docs/ AGENTS.md .claude/ --include='*.md'`.
- [ ] Update `governance/development/infra/acceptance-criteria.md` to reference `prd.md` as the canonical Gherkin location (if referenced).
- [ ] Update `docs/how-to/organize-work.md` to reflect the five-doc layout (if referenced).
- [ ] Update `AGENTS.md` plan-structure summary if it mentions the three-file split.
- [ ] Verify no stale `requirements.md` reference remains in governance/, docs/, AGENTS.md, .claude/agents/, .claude/skills/ (grep returns only historical/migration context).

## Phase 5 — Sync to OpenCode

- [ ] Run `npm run sync:claude-to-opencode` from repo root.
- [ ] Verify script exits zero.
- [ ] `git status` shows updated `.opencode/agent/plan-*.md` and `.opencode/skill/plan-creating-project-plans/SKILL.md`.
- [ ] Spot-check `.opencode/agent/plan-maker.md` matches `.claude/agents/plan-maker.md` semantically (allowing for format conversions per [CLAUDE.md dual-mode rules](../../../CLAUDE.md#dual-mode-configuration-claude-code--opencode)).

## Phase 6 — Migrate the active in-progress plan

- [ ] Read `plans/in-progress/2026-04-16__organiclever-fe-local-first/requirements.md` in full.
- [ ] Create `plans/in-progress/2026-04-16__organiclever-fe-local-first/brd.md` with business-impact content.
- [ ] Create `plans/in-progress/2026-04-16__organiclever-fe-local-first/prd.md` with user stories + Gherkin + product scope.
- [ ] Verify `wc -l` of `brd.md` + `prd.md` approximates `wc -l` of original `requirements.md` (tolerate modest cross-link overhead).
- [ ] Delete `plans/in-progress/2026-04-16__organiclever-fe-local-first/requirements.md`.
- [ ] Update that plan's `README.md` "Plan Documents" (or equivalent) section to link `brd.md` and `prd.md` instead of `requirements.md`.
- [ ] Run `npm run lint:md` on the migrated plan files.

## Phase 7 — Verification and Quality Gates

- [ ] Grep `plans/in-progress/` and `plans/backlog/` for any `requirements.md` filename → expect zero matches.
- [ ] Grep `.claude/` for `requirements.md` → expect only historical/migration context mentions.
- [ ] Grep `governance/`, `docs/`, `AGENTS.md` for `requirements.md` → expect only historical mentions.
- [ ] Grep `governance/workflows/plan/` for `requirements.md` → expect zero matches.
- [ ] Confirm `governance/conventions/structure/plans.md` contains both `brd.md` and `prd.md` strings.
- [ ] Confirm `governance/workflows/plan/plan-quality-gate.md` completeness bullet enumerates the five canonical documents.
- [ ] Run `plan-checker` against `plans/in-progress/2026-04-18__plan-convention-brd-prd-split/` (this plan) → expect zero findings.
- [ ] Run `plan-checker` against `plans/in-progress/2026-04-16__organiclever-fe-local-first/` (migrated plan) → expect zero findings.
- [ ] Run `npm run lint:md` repository-wide → expect zero violations.
- [ ] Run `nx affected -t typecheck lint test:quick spec-coverage` → expect pass (no code changes, but verify).

## Phase 8 — Plan hand-off

- [ ] Update `plans/in-progress/README.md` index to include this plan.
- [ ] Commit changes per Conventional Commits, split by domain:
  - [ ] Commit 1: `docs(governance): split plan requirements into brd + prd`
  - [ ] Commit 2: `chore(agents): update plan-* agents for brd + prd layout`
  - [ ] Commit 3: `docs(workflows): update plan workflows for brd + prd layout`
  - [ ] Commit 4: `chore(skills): update plan-creating-project-plans skill for brd + prd`
  - [ ] Commit 5: `chore(opencode): sync .opencode mirrors`
  - [ ] Commit 6: `docs(plans): migrate organiclever-fe-local-first to brd + prd layout`
- [ ] Do **NOT** push unless the user explicitly asks.
- [ ] When work is verified complete, move the plan folder to `plans/done/` and update `plans/done/README.md` + `plans/in-progress/README.md`.

## Quality Gates

All must pass before this plan moves to `plans/done/`:

1. **Markdown lint clean** — `npm run lint:md` zero violations.
2. **Zero stale references** — grep checks in Phase 7 return expected results.
3. **Agent self-consistency** — `plan-checker` reports zero findings on both this plan and the migrated plan.
4. **Workflow self-consistency** — `plan-quality-gate.md` enumerates the same five documents the agents produce/validate.
5. **OpenCode sync clean** — `.opencode/` mirrors updated, no divergence.
6. **Affected tests pass** — `nx affected -t typecheck lint test:quick spec-coverage`.

## Verification Log (fill during execution)

- [ ] Phase 1 complete — convention doc updated.
- [ ] Phase 2 complete — five agents updated.
- [ ] Phase 3 complete — two workflows updated.
- [ ] Phase 4 complete — skill + cross-refs updated.
- [ ] Phase 5 complete — OpenCode synced.
- [ ] Phase 6 complete — legacy plan migrated.
- [ ] Phase 7 complete — all quality gates pass.
- [ ] Phase 8 complete — commits recorded, plan archived.
