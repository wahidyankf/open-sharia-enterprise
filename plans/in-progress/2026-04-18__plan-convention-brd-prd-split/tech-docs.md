# Technical Documentation

**Plan**: Plan Convention — Split Requirements into BRD + PRD
**Date**: 2026-04-18

## Overview

Structural change to governance documentation and plan agent/skill behavior. No code is modified. Risk is concentrated in (a) cross-reference drift (stale mentions of `requirements.md`) and (b) agent self-consistency (checker and maker disagreeing on the canonical layout).

## Filename Decision

Adopt **`brd.md`** and **`prd.md`** (industry-standard acronyms) rather than `business-requirements.md` and `product-requirements.md`, for three reasons:

1. **Parallels existing naming**: The convention already uses short labels (`tech-docs.md`, not `technical-documentation.md`; `delivery.md`, not `delivery-checklist.md`). `brd` / `prd` fit the established cadence.
2. **Immediate recognition**: BRD and PRD are universally understood acronyms in software planning; no one opening a plan folder will misinterpret them.
3. **Shorter directory listings**: Five-document folders look tidier with short names.

Each file's H1 expands the acronym (`# Business Requirements Document (BRD)` and `# Product Requirements Document (PRD)`), so readers unfamiliar with the acronym learn it immediately.

## Content-Placement Rules

Authoritative split between `brd.md` and `prd.md`. These rules go into the convention document and agent instructions verbatim so `plan-maker` / `plan-checker` / `plan-fixer` share one definition.

### Goes in `brd.md` (business perspective)

- Business goal and rationale
- Business impact (pain points, expected benefits)
- Stakeholders and sign-off mapping
- Business-level success metrics (KPIs, adoption targets, cost/time savings)
- Business-scope Non-Goals
- Business risks and mitigations

### Goes in `prd.md` (product perspective)

- Product overview (what is being built)
- Personas
- User stories (`As a … I want … So that …`)
- Acceptance criteria in Gherkin
- Product scope (in-scope features, out-of-scope features)
- Product-level risks (UX, feature interaction)

### Ambiguous cases

When a concern is genuinely cross-cutting (e.g., "success metric" is both a KPI for sponsors and an acceptance criterion for engineering), place the **quantitative assertion** in `brd.md` and the **testable scenario** in `prd.md`, cross-linking between them. Do not duplicate the full content.

## Affected Files

### Governance (authoritative convention)

| File                                                  | Change                                                                                                                        |
| ----------------------------------------------------- | ----------------------------------------------------------------------------------------------------------------------------- |
| `governance/conventions/structure/plans.md`           | Rewrite Multi-File Structure section for five-doc layout; update Single-File Structure exception wording; update all examples |
| `governance/development/infra/acceptance-criteria.md` | Update any reference from `requirements.md` to `prd.md` as the canonical Gherkin location                                     |

### Agents (`.claude/agents/`)

| Agent                       | Change                                                                   |
| --------------------------- | ------------------------------------------------------------------------ |
| `plan-maker.md`             | Scaffold five files; content-placement guidance for brd/prd              |
| `plan-checker.md`           | Validate presence + content placement; flag misplacement                 |
| `plan-fixer.md`             | Move misplaced content into correct file on violation                    |
| `plan-executor.md`          | No behavioral change (reads `delivery.md`); update doc references        |
| `plan-execution-checker.md` | Read `prd.md` for acceptance-criteria validation (was `requirements.md`) |

### Skills (`.claude/skills/`)

| Skill                                  | Change                                                      |
| -------------------------------------- | ----------------------------------------------------------- |
| `plan-creating-project-plans/SKILL.md` | Update plan-structure reference to five-doc; update example |

### Cross-references

| File                                                      | Change                                               |
| --------------------------------------------------------- | ---------------------------------------------------- |
| `docs/how-to/organize-work.md`                            | Update any mention of `requirements.md` as plan file |
| `AGENTS.md`                                               | Update summary of plan structure (if present)        |
| Any other governance doc surfaced by repository-wide grep | Update to new naming                                 |

### OpenCode mirrors

`.opencode/agent/plan-*.md` and `.opencode/skill/plan-creating-project-plans/SKILL.md` — regenerated via `npm run sync:claude-to-opencode` after `.claude/` edits.

### Plan migration

`plans/in-progress/2026-04-16__organiclever-fe-local-first/` — split `requirements.md` into `brd.md` + `prd.md`; update `README.md` "Plan Documents" links; remove `requirements.md`.

## Migration Mechanics (organiclever-fe-local-first plan)

Step-by-step content transplant:

1. Read `requirements.md`.
2. Identify business-impact paragraphs (rationale, scope rationale, stakeholders, success metrics) → copy into new `brd.md`.
3. Identify user stories / Gherkin scenarios / product scope / personas → copy into new `prd.md`.
4. Reconcile: any content that is genuinely both (e.g., a success metric phrased as an acceptance criterion) — quantitative version goes in `brd.md`; testable scenario goes in `prd.md`, cross-linked.
5. Verify via diff: `wc -l requirements.md` vs `wc -l brd.md prd.md` should be approximately equal (minor overhead for cross-links is acceptable; large delta indicates content loss).
6. Delete `requirements.md`.
7. Update `README.md` "Plan Documents" section to link `brd.md` + `prd.md` rather than `requirements.md`.
8. Spot-check that `plan-checker` run against the migrated plan reports zero findings.

## Single-File Exception (updated)

The single-file exception remains, with updated wording to reflect the five-doc default:

- Same threshold: ≤ 1000 lines combined.
- README.md in a single-file plan MUST cover Context, Scope, **Business rationale (condensed BRD)**, **Product requirements (condensed PRD)**, Technical approach, Delivery checklist, Quality gates, Verification.
- If the author cannot comfortably fit both business rationale and product requirements into the single README without overcrowding, promote to the five-doc layout.

## Verification Strategy

### Grep invariants (post-change)

| Check                                                         | Expected result                    |
| ------------------------------------------------------------- | ---------------------------------- |
| `plans/in-progress/ plans/backlog/ -name 'requirements.md'`   | zero matches                       |
| grep "requirements.md" in `.claude/`                          | only historical/migration mentions |
| grep "requirements.md" in `governance/`, `docs/`, `AGENTS.md` | only historical mentions           |
| grep "brd.md" in `governance/conventions/structure/plans.md`  | present                            |
| grep "prd.md" in `governance/conventions/structure/plans.md`  | present                            |

### Agent round-trip test

Run through agents manually against this plan (`2026-04-18__plan-convention-brd-prd-split/`):

1. `plan-checker` → expects zero findings.
2. `plan-executor` (dry-read) → resolves `delivery.md` correctly.
3. `plan-execution-checker` (after execution) → validates against `prd.md` Gherkin.

### Markdown lint

```bash
npm run lint:md
```

All changed files pass.

### Affected tests

```bash
nx affected -t typecheck lint test:quick spec-coverage
```

No code changes in this plan, so affected graph should be minimal or empty; any failures indicate regression elsewhere and must be investigated per [ci-blocker-resolution.md](../../../governance/development/quality/ci-blocker-resolution.md).

## Rollback

If the migration or agent updates reveal a blocking defect:

1. Revert the commit(s) from this plan via `git revert`.
2. Restore the prior `requirements.md` in the migrated plan (content preserved in git history).
3. Re-open this plan in `plans/in-progress/` with adjusted approach notes.

No data loss risk — this is a documentation-structure change.

## Dependencies and Ordering

Phase ordering is strict:

1. Convention doc first — every downstream file references it.
2. Agents + skill second — they cite the convention.
3. Cross-references third — they cite both.
4. OpenCode sync fourth — mechanical mirror of `.claude/`.
5. Plan migration last — exercises the new convention end-to-end.

Quality gates run after phase 5 against the whole change set.

## Risks

| Risk                                                               | Severity | Mitigation                                                                        |
| ------------------------------------------------------------------ | -------- | --------------------------------------------------------------------------------- |
| Cross-reference drift (stale `requirements.md` mention)            | Medium   | Grep-based verification step in delivery                                          |
| Agent self-inconsistency (maker says 5-doc, checker expects 4-doc) | High     | Update agents in one commit set; run checker against this plan as acceptance test |
| Migration loses content                                            | Medium   | Line-count sanity check + manual diff; git history preserves original             |
| OpenCode sync produces divergence                                  | Low      | `npm run sync:claude-to-opencode` is idempotent; post-sync diff verified          |
| Downstream tooling references `requirements.md` by path            | Low      | Repository-wide grep at delivery Phase 3 surfaces any non-doc references          |
