---
title: Complete Repo-Rules Zero Findings
description: Drive repo-rules-quality-gate strict mode to double-zero (deterministic preflight AND AI-only) and harden the two quality-gate workflow docs against issues observed in production.
status: In Progress
created: 2026-05-12
---

# Complete Repo-Rules Zero Findings

Drive `repo-rules-quality-gate` strict-mode pass to **double-zero on both the deterministic preflight AND the AI-only findings**, then harden both `repo-rules-quality-gate.md` and `plan-quality-gate.md` workflow docs against the issues observed in production use. Nothing is deferred — every issue identified in the brief is addressed inside this single plan.

## Plan Documents

- [README.md](./README.md) (this file) — context, scope, navigation
- [brd.md](./brd.md) — business rationale: why this work matters, who is affected, success criteria, risks
- [prd.md](./prd.md) — product requirements: user stories, functional + non-functional requirements, Gherkin acceptance criteria
- [tech-docs.md](./tech-docs.md) — architecture, per-phase technical design, JSON schemas, file-impact matrix, rollback procedures
- [delivery.md](./delivery.md) — phased delivery checklist with explicit file paths, verbatim commands, concrete acceptance criteria, suggested executors

## Elevator Pitch

After the `2026-05-12__optimize-repo-rules-quality-gate-with-rhino-cli` plan landed [Repo-grounded — `plans/done/` listing verified], a baseline strict-mode run of `repo-rules-quality-gate` produces ~4479 findings — dominated by `emoji-audit` scanning legacy `archived/` and `.next/` trees, by `docs-validate-frontmatter` rejecting the actual Diátaxis `category: explanation` value, and by `agents-detect-duplication` cluster reports that need conservative skill extraction. This plan calibrates the rhino-cli audits (Phase 1) to suppress noise, hardens the two quality-gate workflow docs (Phases 2-3), fixes the residual real governance findings (Phase 4), and applies conservative parameter-rich skill extraction to eliminate agent dedup clusters (Phase 5), arriving at double-zero (Phase 6). [Judgment call: baseline count ~4479 cited from the user brief; per-category split is also from the brief and must be re-confirmed in Phase 0.]

## Phases at a Glance

- **Phase 0** — Provision worktree (user-waivable at runtime), capture baseline preflight, record per-category counts
- **Phase 1** — Calibrate rhino-cli audits (emoji skip-dirs, Diátaxis frontmatter schema, N-fence heading hierarchy, orchestrator `--exclude` propagation, `RHINO_AUDIT_NOW` docs, version bump v0.16.1)
- **Phase 2** — Harden `repo-rules-quality-gate.md` (11 issues — broken nx command, deterministic findings as visibility-only, hash-reuse env var, arg name consistency, exit-2 recovery, skip-list curation rules, observability metrics, change footer, step-0.5 numbering rationale, emoji-audit noise hatch, conventions implemented entry)
- **Phase 3** — Fix `plan-quality-gate.md` mode bug + observability (mode parameter honoring, conventions implemented entries, observability metrics, research-delegation cost, final audit report structure)
- **Phase 4** — Apply governance findings on the calibrated residual (~700 — footer markers, workflow agent refs, README index gaps, frontmatter residuals, heading-hierarchy residuals, skip-list curation)
- **Phase 5** — Conservative skill extraction for agent dedup (368 → 0 clusters with parameterized skills; behavioral-equivalence golden tests gate every batch)
- **Phase 6** — Final convergence (re-run strict mode → double-zero), document baseline in `apps/rhino-cli/README.md` v0.16.1, archive plan

## Scope

**In scope**:

- `apps/rhino-cli/internal/repo-governance/emoji_audit.go` — emoji skip-dirs
- `apps/rhino-cli/internal/docs/frontmatter.go` — Diátaxis frontmatter schema
- `apps/rhino-cli/internal/docs/heading_hierarchy.go` — N-fence support
- `apps/rhino-cli/cmd/governance_audit.go` — `--exclude` flag propagation
- `apps/rhino-cli/cmd/root.go` — version bump to 0.16.1
- `apps/rhino-cli/README.md` — version-history entry + `RHINO_AUDIT_NOW` documentation
- `repo-governance/workflows/repo/repo-rules-quality-gate.md` — 11 hardening edits
- `repo-governance/workflows/plan/plan-quality-gate.md` — mode bug + observability
- `repo-governance/` sweep — footer markers, README index gaps
- `.claude/agents/*.md` — conservative skill extraction
- `.claude/skills/<new-shared-skills>/SKILL.md` — extracted parameterized skills
- `.opencode/agents/*.md` — auto-synced after agent edits via `npm run sync:claude-to-opencode`
- `generated-reports/.known-false-positives.md` — curated skip-list entries
- `plans/in-progress/complete-repo-rules-zero-findings/` → `plans/done/YYYY-MM-DD__complete-repo-rules-zero-findings/` (final archival)

**Out of scope**:

- Executing this plan during this authoring session — the user will run `plan-quality-gate` first, then `plan-execution`.
- Committing/pushing — the calling context handles git operations.
- Extending validation to the rest of `docs/` (tutorials, how-to, reference, non-software-engineering explanation subtrees) — already scoped out of `repo-rules-quality-gate` and tracked elsewhere [Repo-grounded — see `repo-rules-quality-gate.md` lines 56-58].
- Touching `ose-infra/`, `ose-primer/`, or `ose-projects/` parent. This plan operates only inside the `ose-public` checkout.
- Migrating to the OpenCode binding for any new content — secondary binding stays auto-synced from `.claude/`.

## Risks

Detailed in [brd.md](./brd.md). Headline risks:

- **Phase 5 behavioral drift** — conservative parameterization mitigates this; behavioral-equivalence golden tests halt-on-drift.
- **Phase 1 calibration overshoot** — could mask real findings. Mitigation: every new skip-dir is justified inline; `--exclude` is opt-in.
- **Phase 2/3 workflow churn** — workflows are consumed by checker/fixer agents. Mitigation: no breaking arg renames; arg-name unification picks the existing primary form.

## Convention References

- [Plans Organization Convention](../../../repo-governance/conventions/structure/plans.md) [Repo-grounded]
- [Plan Anti-Hallucination Convention](../../../repo-governance/development/quality/plan-anti-hallucination.md) [Repo-grounded]
- [Deterministic vs AI Validation Split Convention](../../../repo-governance/conventions/structure/deterministic-vs-ai-validation-split.md) [Repo-grounded]
- [Trunk Based Development Convention](../../../repo-governance/development/workflow/trunk-based-development.md) [Repo-grounded]
- [Worktree Path Convention](../../../repo-governance/conventions/structure/worktree-path.md) [Repo-grounded]
- [Worktree Toolchain Initialization](../../../repo-governance/development/workflow/worktree-setup.md) [Repo-grounded]
- [No Date Metadata Convention](../../../repo-governance/conventions/structure/no-date-metadata.md) [Repo-grounded]
- [No "Last Updated" Convention](../../../repo-governance/conventions/structure/no-last-updated.md) [Repo-grounded]
- [Web Research Delegation Convention](../../../repo-governance/conventions/writing/web-research-delegation.md) [Repo-grounded]
