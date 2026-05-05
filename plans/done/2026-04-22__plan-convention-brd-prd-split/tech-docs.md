# Technical Documentation

**Plan**: Plan Convention — Split Requirements into BRD + PRD
**Date**: 2026-04-18

## Overview

Structural change to governance documentation and plan agent/skill behavior. This plan modifies no code. Risk concentrates in (a) cross-reference drift (stale mentions of `requirements.md`) and (b) agent self-consistency (checker and maker disagreeing on the canonical layout).

## Filename Decision

Adopt **`brd.md`** and **`prd.md`** (industry-standard acronyms) rather than `business-requirements.md` and `product-requirements.md`, for three reasons:

1. **Parallels existing naming**: The convention already uses short labels (`tech-docs.md`, not `technical-documentation.md`; `delivery.md`, not `delivery-checklist.md`). `brd` / `prd` fit the established cadence.
2. **Immediate recognition**: BRD and PRD are universally understood acronyms in software planning; no one opening a plan folder will misinterpret them.
3. **Shorter directory listings**: Five-document folders look tidier with short names.

Each file's H1 expands the acronym (`# Business Requirements Document (BRD)` and `# Product Requirements Document (PRD)`), so readers unfamiliar with the acronym learn it immediately.

## Content-Placement Rules

Authoritative split between `brd.md` and `prd.md`. These rules go into the convention document and agent instructions verbatim so `plan-maker` / `plan-checker` / `plan-fixer` share one definition.

> **Solo-maintainer framing**: BRD and PRD are **content-placement containers**, not sign-off artifacts. This repo has one maintainer and a set of AI agents; code review (the PR) is the only approval gate. The convention MUST NOT introduce sponsor sign-off, stakeholder approval ceremonies, or role-based gates.

### Goes in `brd.md` (business perspective)

- Business goal and rationale ("why are we doing this")
- Business impact (pain points, expected benefits)
- Affected roles (which hats the maintainer wears; which agents consume the file) — **not** sign-off mapping
- Business-level success metrics. The BRD does not require every claim to be data-driven — gut-based reasoning is acceptable **when the logic supports the claim**. What is NOT acceptable: fabricated numeric targets (percentages, durations, counts) presented as though they were already measured, when no baseline exists. Options when writing a benefit or success metric:
  1. **Observable fact** (preferred when available): cite a grep/git/agent-round-trip check that verifies the claim on demand (e.g., "zero plans using the deprecated layout after migration").
  2. **Cited measurement**: reference an existing dashboard, prior measurement, or external data source. For external references (industry norms, reputable blog posts, framework documentation), the author MAY delegate to [`web-research-maker`](../../../.claude/agents/web-research-maker.md) to fetch cited findings — this keeps plan context lean while still letting claims rest on real sources. **When you cite data pulled from the internet, include the data itself in the plan** (the specific number, quote, table, or short excerpt), alongside the source URL. URL-only citations are not enough: links rot, pages change, and a reader or future agent must be able to see the evidence without leaving the plan.
  3. **Qualitative reasoning**: state the structural claim plainly without a number (e.g., "file diffs become narrower per concern").
  4. **Judgment call / gut target**: allowed, but MUST be explicitly labeled as such (e.g., "_Judgment call:_ we expect review time to drop; no baseline measured"). The reader must be able to tell at a glance that this is a gut estimate, not a measured fact.
- Business-scope Non-Goals
- Business risks and mitigations

### Goes in `prd.md` (product perspective)

- Product overview (what is being built)
- Personas (hats the maintainer wears; agents that consume the file) — **not** external stakeholder roles
- User stories (`As a … I want … So that …`)
- Acceptance criteria in Gherkin
- Product scope (in-scope features, out-of-scope features)
- Product-level risks (UX, feature interaction)

### Ambiguous cases

When a concern is genuinely cross-cutting (e.g., a success criterion is both a business-level fact and a product acceptance criterion), place the **factual claim or judgment** in `brd.md` and the **testable scenario** in `prd.md`, cross-linking between them. Do not duplicate the full content. If the BRD side is a judgment call rather than a measured fact, label it as such — do not fabricate a number and pretend it was measured.

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
| `plan-execution-checker.md` | Read `prd.md` for acceptance-criteria validation (was `requirements.md`) |

### Skills (`.claude/skills/`)

| Skill                                  | Change                                                      |
| -------------------------------------- | ----------------------------------------------------------- |
| `plan-creating-project-plans/SKILL.md` | Update plan-structure reference to five-doc; update example |

### Workflows (`governance/workflows/plan/`)

| Workflow               | Change                                                                                                                                                                                                                                                                                                                                        |
| ---------------------- | --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| `plan-quality-gate.md` | Update the "Plan-Specific Validation" completeness bullet (currently `requirements, deliverables, checklists`) to enumerate the five canonical documents for multi-file plans (`README.md`, `brd.md`, `prd.md`, `tech-docs.md`, `delivery.md`) and to clarify that the single-file exception still allows a single `README.md` when eligible. |
| `plan-execution.md`    | No behavioral change — executor still reads `delivery.md`. Add a short note that the executor MAY consult `brd.md` / `prd.md` / `tech-docs.md` for context when a delivery item is ambiguous. Verify all existing `delivery.md` references remain correct.                                                                                    |

### Cross-references

| File                                                      | Change                                               |
| --------------------------------------------------------- | ---------------------------------------------------- |
| `docs/how-to/organize-work.md`                            | Update any mention of `requirements.md` as plan file |
| `AGENTS.md`                                               | Update summary of plan structure (if present)        |
| Any other governance doc surfaced by repository-wide grep | Update to new naming                                 |

### OpenCode mirrors

`.opencode/agent/plan-*.md` and `.opencode/skill/plan-creating-project-plans/SKILL.md` — regenerated via `npm run sync:claude-to-opencode` after `.claude/` edits.

### Plan migration

`plans/in-progress/2026-04-16__organiclever-web-local-first/` — split `requirements.md` into `brd.md` + `prd.md`; update `README.md` "Plan Documents" links; remove `requirements.md`.

## Migration Mechanics (organiclever-web-local-first plan)

Step-by-step content transplant:

1. Read `requirements.md`.
2. Identify business-impact paragraphs (rationale, affected roles, success metrics) → copy into new `brd.md`.
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

## Authoring Aids

The plan author MAY use the following agents and tools to validate claims while writing or revising any of the five plan documents. These aids are optional but encouraged when a claim rests on external references:

- **[`web-research-maker`](../../../.claude/agents/web-research-maker.md)** — delegate to this agent when a BRD/PRD claim should rest on an industry source, framework doc, or reputable blog post. The agent returns cited findings in a sandboxed context so the plan stays lean. Good uses: validating "BRD/PRD is an established industry pattern" language, checking canonical sections of each doc type, finding precedent for embedding Gherkin inside a PRD.
- **`WebFetch`** — in-context fetch of a specific known-authoritative URL (single-shot verification, no multi-page research). Use when the author already knows the exact URL of the source and only needs to read it once.
- **`WebSearch`** — in-context search for a narrow, single-claim query. Escalate to `web-research-maker` if verification requires 2 or more `WebSearch` calls or 3 or more `WebFetch` calls.

Use these tools to turn `Judgment call` labels into `Cited measurement` labels over time. Research findings belong inline next to the claim they support, not in a separate research log.

### Inline-evidence rule for internet citations

When a plan claim is backed by data pulled from the internet (a blog post, spec, dashboard screenshot, benchmark, survey, etc.), the plan MUST include the **evidence itself** — not just a link. At minimum, include:

- The specific figure, quote, table row, or short excerpt being cited (verbatim, in quotes).
- The source URL.
- The date the source was accessed (link-rot hedge).
- A one-sentence note on what the evidence establishes, if not obvious from the excerpt.

Rationale: URLs decay, pages are rewritten, and paywalls appear. A future reader — human or agent — must be able to verify the claim from the plan alone, without network access. This rule applies uniformly to `brd.md`, `prd.md`, `tech-docs.md`, `delivery.md`, and `README.md`.

Example (acceptable):

> Atlassian's BRD guide lists "business objectives, scope, constraints, stakeholders, assumptions" as the five canonical BRD sections ([www.atlassian.com/agile/product-management/business-requirements-document](https://www.atlassian.com/agile/product-management/business-requirements-document), accessed 2026-04-18).

Example (not acceptable):

> Per Atlassian, BRDs have five canonical sections.

(The second form forces the reader to re-fetch the page to learn what those sections are — and fails when the page is gone.)

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
2. plan-execution workflow (dry-read by calling context) → resolves `delivery.md` correctly.
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

No code changes in this plan, so affected graph should be minimal or empty; any failures indicate regression elsewhere and must be investigated per [CI Blocker Resolution Convention](../../../governance/development/quality/ci-blocker-resolution.md).

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
3. Workflows third — they cite the convention and the agents.
4. Cross-references fourth — they cite all of the above.
5. OpenCode sync fifth — mechanical mirror of `.claude/`.
6. Plan migration last — exercises the new convention end-to-end.

Quality gates run after phase 6 against the whole change set.

## Risks

| Risk                                                               | Severity | Mitigation                                                                        |
| ------------------------------------------------------------------ | -------- | --------------------------------------------------------------------------------- |
| Cross-reference drift (stale `requirements.md` mention)            | Medium   | Grep-based verification step in delivery                                          |
| Agent self-inconsistency (maker says 5-doc, checker expects 4-doc) | High     | Update agents in one commit set; run checker against this plan as acceptance test |
| Migration loses content                                            | Medium   | Line-count sanity check + manual diff; git history preserves original             |
| OpenCode sync produces divergence                                  | Low      | `npm run sync:claude-to-opencode` is idempotent; post-sync diff verified          |
| Downstream tooling references `requirements.md` by path            | Low      | Repository-wide grep at delivery Phase 3 surfaces any non-doc references          |
