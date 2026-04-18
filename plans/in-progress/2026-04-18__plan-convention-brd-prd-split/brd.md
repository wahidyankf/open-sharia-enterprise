# Business Requirements Document (BRD)

**Plan**: Plan Convention — Split Requirements into BRD + PRD
**Date**: 2026-04-18

> **Scope note**: This is a single-maintainer repo operated in collaboration with AI agents. "Business requirements" here mean _author-level clarity of intent_, _reviewer-level clarity at code review_, and _agent-level validatability_ — not sponsor sign-off or stakeholder approval ceremonies. Code review is the gate; there are no separate human sign-offs.

## Business Goal

Improve plan-document legibility and review efficiency so that (a) the author, on revisiting a plan weeks later, can locate intent and rationale without re-reading the full `requirements.md`; (b) code reviewers can scan the "why" independently of the "what"; and (c) plan agents can validate each concern against the file that exclusively owns it.

## Business Impact

### Pain Points Addressed

| Pain Point                  | Current State                                                                 | Impact                                                                                              |
| --------------------------- | ----------------------------------------------------------------------------- | --------------------------------------------------------------------------------------------------- |
| Intent gets buried          | Business rationale sits under user-story sections in `requirements.md`        | Author loses context on revisit; code reviewer hunts for "why" before judging "what"                |
| Product/business diff noise | Both concerns share one file                                                  | PRs editing user stories churn business rationale sections and vice versa                           |
| Agent validation is coarse  | `plan-checker` validates one omnibus file; cannot assert misplacement cleanly | Misplaced content (business framing in scope list, scope items in business rationale) slips through |
| Onboarding / revisit burden | Revisiting a plan means parsing a monolithic `requirements.md`                | Higher cognitive load to resume work; slower re-entry after context switches                        |

### Expected Benefits

- **Faster "why" lookup**: A dedicated `brd.md` puts business impact on the first screen the author or reviewer opens. Target: answer "why are we doing this and what does success look like?" in under 90 seconds on a cold re-read.
- **Cleaner diffs**: Separating product scope from business rationale eliminates unrelated-content churn in PRs touching plans. Target: PRs touching only `prd.md` never modify business rationale and vice versa.
- **Sharper agent validation**: `plan-checker` can assert each concern against its owning file and flag misplacement as a distinct finding class, rather than validating an omnibus `requirements.md` by keyword heuristics.
- **Convention alignment with industry norms**: BRD and PRD are widely recognized document types; mapping the repo's plan structure onto them reduces cognitive overhead when reading plans authored elsewhere and when explaining the structure to future contributors or tools.

## Affected Roles

There is no human sign-off gate. The relevant roles are the maintainer wearing different hats and the agents that consume the files:

| Role                                                | Primary file(s)                         | How they consume it                                                    |
| --------------------------------------------------- | --------------------------------------- | ---------------------------------------------------------------------- |
| Maintainer (author mode)                            | All five                                | Drafts each file in its purpose                                        |
| Maintainer (reviewer mode at code review)           | `README.md` → targeted file per concern | Reviews the file relevant to the concern being raised                  |
| `plan-maker` / `plan-checker` / `plan-fixer` agents | All five                                | Produce, validate, and remediate each file per content-placement rules |
| `plan-executor` / `plan-execution-checker` agents   | `delivery.md`, `prd.md`                 | Drive execution; verify completed work against Gherkin in `prd.md`     |

Code review (the PR itself) is the approval gate. No separate ceremony exists or is introduced.

## Success Metrics

Business-level success criteria (product-level criteria live in [prd.md](./prd.md)):

1. **Zero plans using deprecated four-document layout** after this plan merges. The one active in-progress plan is migrated; archived plans in `plans/done/` are explicitly grandfathered.
2. **Convention document is self-consistent**: every reference to `requirements.md` in governance, agents, workflows, skills, and docs is updated. Verified by grep.
3. **Agent round-trip works**: `plan-maker` produces a five-doc plan; `plan-checker` reports zero findings on it; `plan-executor` reads `delivery.md` successfully; `plan-execution-checker` validates it against `prd.md` Gherkin criteria.
4. **This plan itself passes `plan-checker`** in the new five-doc layout. It is the canonical reference example.

## Non-Goals (Business)

- **Not introducing any human sign-off gate**. Code review (PR approval) is the only approval gate and is unchanged by this plan.
- **Not mandating BRD/PRD for single-file plans**. The single-file exception still exists for trivially small plans (see [tech-docs](./tech-docs.md) for updated criteria).
- **Not creating new agents** for business-requirement validation. Existing `plan-checker` is extended to validate both BRD and PRD presence and content.
- **Not expanding `plans/` into a product-management tool**. This remains a developer-facing planning workspace, not a replacement for external PM systems.
- **Not introducing role-based ownership** beyond what the content-placement rules imply. In a single-maintainer repo, ownership language collapses to "which file does this content belong in?" — nothing more.

## Risks and Mitigations

| Risk                                                                   | Likelihood | Mitigation                                                                                    |
| ---------------------------------------------------------------------- | ---------- | --------------------------------------------------------------------------------------------- |
| Authors (i.e., the maintainer) duplicate content across BRD and PRD    | Medium     | Convention spells out what belongs in each; `plan-checker` flags content overlap on review    |
| "BRD" / "PRD" acronyms feel heavyweight for small plans                | Low        | Single-file exception is preserved and updated; trivial plans skip the split                  |
| Existing tooling (e.g., scripts grep-ing for `requirements.md`) breaks | Low        | Grep pass during delivery identifies all references; updates land in one commit set           |
| Migrated plan introduces regression                                    | Low        | Migration preserves content; only file boundaries change; executor re-verified post-migration |
| BRD drifts into sponsor-ceremony framing over time                     | Low        | Convention text and `plan-checker` rules anchor BRD on intent/impact/metrics, not sign-off    |
