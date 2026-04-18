# Business Requirements Document (BRD)

**Plan**: Plan Convention — Split Requirements into BRD + PRD
**Date**: 2026-04-18

## Business Goal

Improve plan-document legibility and review efficiency so that stakeholders (business sponsors, product managers, engineers) can each locate the information they need in a single file without skimming unrelated content.

## Business Impact

### Pain Points Addressed

| Pain Point                  | Current State                                                                 | Impact                                                                    |
| --------------------------- | ----------------------------------------------------------------------------- | ------------------------------------------------------------------------- |
| Stakeholder review friction | Business context buried inside user-story-dominated `requirements.md`         | Executives skip plan review or ask redundant "why" questions              |
| Product/business diff noise | Both concerns share one file                                                  | PRs editing user stories churn business rationale sections and vice versa |
| Ownership ambiguity         | No file boundary between "who signs off on value" vs "who signs off on scope" | Unclear accountability for approving business goals vs product spec       |
| Onboarding burden           | New contributors read a monolithic `requirements.md` to understand any plan   | Higher cost to ramp; slower first-contribution time                       |

### Expected Benefits

- **Faster stakeholder sign-off**: A dedicated `brd.md` puts business impact on the first screen a sponsor opens. Target: non-technical reviewers can answer "why are we doing this and what does success look like?" in under 90 seconds.
- **Cleaner diffs**: Separating product scope from business rationale cuts unrelated-content churn in PRs touching plans. Target: PR reviewers report reduced context-switching when reviewing plan changes.
- **Clearer ownership**: Business sponsors own `brd.md`; product owns `prd.md`; engineering owns `tech-docs.md` + `delivery.md`. Each file has a natural reviewer.
- **Convention alignment with industry norms**: BRD and PRD are widely recognized document types; mapping the repo's plan structure onto them reduces cognitive overhead for contributors coming from other organizations.

## Stakeholders

| Stakeholder                                         | Role in this change                                      | Sign-off needed                                  |
| --------------------------------------------------- | -------------------------------------------------------- | ------------------------------------------------ |
| Repository maintainer                               | Convention owner; approves governance change             | Yes                                              |
| Plan authors (future)                               | Consumers of the new layout                              | Inform via updated convention + agents           |
| `plan-maker` / `plan-checker` / `plan-fixer` agents | Must produce and validate five-doc plans                 | Updated agent files must ship in same commit set |
| `plan-executor` / `plan-execution-checker` agents   | Must locate delivery checklist correctly after the split | Regression-test on the migrated example plan     |

## Success Metrics

Business-level success criteria (product-level criteria live in [prd.md](./prd.md)):

1. **Zero plans using deprecated three-part requirements layout** after this plan merges. The one active in-progress plan is migrated; archived plans in `plans/done/` are explicitly grandfathered.
2. **Convention document is self-consistent**: every reference to `requirements.md` in governance, agents, skills, and docs is updated. Verified by grep.
3. **Agent round-trip works**: `plan-maker` produces a five-doc plan; `plan-checker` reports zero findings on it; `plan-executor` reads `delivery.md` successfully; `plan-execution-checker` validates it against `brd.md` + `prd.md`.
4. **This plan itself passes `plan-checker`** in the new five-doc layout. It is the canonical reference example.

## Non-Goals (Business)

- **Not introducing a separate sign-off gate** (e.g., BRD approval required before PRD drafting). Authors may draft BRD and PRD in any order; review cadence is a team-level concern outside this convention.
- **Not mandating BRD/PRD for single-file plans**. The single-file exception still exists for trivially small plans (see [tech-docs](./tech-docs.md) for updated criteria).
- **Not creating new agents** for business-requirement validation. Existing `plan-checker` is extended to validate both BRD and PRD presence and content.
- **Not expanding `plans/` into a product-management tool**. This remains a developer-facing planning workspace, not a replacement for external PM systems.

## Risks and Mitigations

| Risk                                                                   | Likelihood | Mitigation                                                                                    |
| ---------------------------------------------------------------------- | ---------- | --------------------------------------------------------------------------------------------- |
| Authors duplicate content across BRD and PRD                           | Medium     | Convention spells out what belongs in each; `plan-checker` flags content overlap on review    |
| "BRD" / "PRD" acronyms feel heavyweight for small plans                | Low        | Single-file exception is preserved and updated; trivial plans skip the split                  |
| Existing tooling (e.g., scripts grep-ing for `requirements.md`) breaks | Low        | Grep pass during delivery identifies all references; updates land in one commit set           |
| Migrated plan introduces regression                                    | Low        | Migration preserves content; only file boundaries change; executor re-verified post-migration |
