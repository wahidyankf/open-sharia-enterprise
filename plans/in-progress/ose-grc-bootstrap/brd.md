# BRD — ose-grc Bootstrap

## Business Goal

Stand up the four-project scaffold needed to begin building **OSE GRC** — a Sharia-compliant Governance/Risk/Compliance product whose first capability will be AI-assisted gap analysis between regulator-published rule documents and a company's internal policies.

The bootstrap is **infrastructure-only**: enable feature work to start with a healthy CI green-light, contract codegen wired, DDD/BDD scaffolding in place, and OpenRouter integration plumbing ready. Without this scaffold, every feature plan re-does the same plumbing in slightly inconsistent ways.

## Business Context

Compliance gap analysis is today a manual, expensive, error-prone activity. Compliance officers read regulator circulars, then read every internal SOP, then write a gap memo. The exercise repeats every time a regulator publishes a revision. Companies that operate across jurisdictions (e.g., Sharia banks across Indonesia, Malaysia, GCC) suffer proportional cost.

The OSE Platform vision is to **democratize Sharia-compliant enterprise tooling**; GRC is the natural Phase-2 product after OrganicLever (productivity tracker) — it targets compliance officers, internal auditors, and Sharia advisors who currently rely on bespoke services or hand-rolled spreadsheets.

LLMs (via OpenRouter, so the platform stays model-agnostic) make it feasible to do structured semantic comparison across heterogeneous document corpora at low marginal cost.

## Affected Roles

- **Compliance Officer** (primary user; future). Receives gap reports. Not affected by this bootstrap plan directly — they consume features that follow.
- **Internal Auditor** (primary user; future). Same — consumes follow-up features.
- **Sharia Advisor** (primary user; future). Reviews gap reports for Sharia-specific compliance.
- **Product Engineer** (immediate user of this plan). Gets a working dev environment with `nx dev ose-grc-web` and `nx dev ose-grc-be` after bootstrap completes. Can begin feature work the same day. _[Judgment call]: there is no measured baseline; the win is qualitative — reproducible scaffolding rather than ad-hoc setup._
- **Repo Governance Owner** (i.e., the solo maintainer). Inherits a new product line that obeys existing conventions (DDD specs, BDD specs, three-level testing, contracts codegen) — no special-casing.

## Business-Level Success Metrics

- _[Judgment call]_ After bootstrap: `nx affected -t typecheck lint test:quick spec-coverage` runs green for all four projects on a fresh clone.
- _[Judgment call]_ A subsequent feature plan adds its first BDD scenario in `specs/apps/ose-grc/behavior/{be,web}/gherkin/` and watches a single Nx target turn red→green; no infrastructure work required at the feature-plan boundary.
- The four projects appear in `AGENTS.md` apps catalog, `repo-governance/` references where applicable, and the in-progress README.
- CI workflow `test-and-deploy-ose-grc-web-development.yml` exists and is dispatchable. Initial run may skip steps (no staging branch yet) but does not error.

No revenue/customer KPIs are claimed; OSE GRC has zero users today. KPIs of that shape belong to feature plans that ship the gap-analysis flow itself.

## Business-Scope Non-Goals

- **Not a product launch.** Bootstrap delivers no user-facing capability.
- **No commitment to OpenRouter exclusivity.** OpenRouter is the bootstrap default because it abstracts model vendors. Migration to direct vendor SDKs (OpenAI, Anthropic, Bedrock) remains an open option for later plans.
- **No legal/regulatory review.** GRC will eventually require compliance review of its own claims; that conversation begins when the first gap-analysis feature ships, not here.
- **No marketing site updates.** `oseplatform-web` does not gain an OSE GRC landing page in this plan.
- **No stakeholder sign-off ceremonies.** Solo-maintainer repo (per [Plans Organization Convention](../../../repo-governance/conventions/structure/plans.md)) — BRD/PRD/tech-docs/delivery alignment IS the gate, not a meeting.

## Business Risks

| Risk                                                                              | Severity | Mitigation                                                                                                                                                       |
| --------------------------------------------------------------------------------- | -------- | ---------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| Bootstrap creates four projects that drift from organiclever's conventions, leading to maintenance tax. | Medium   | Mirror organiclever-web/be project.json shapes exactly except where F# forces a divergence (dotnet vs maven, fantomas+fsharplint vs checkstyle+pmd, AltCover vs JaCoCo). Annotate every divergence. |
| OpenRouter env vars get committed to a public repo by accident.                   | High     | `.env.example` carries placeholders only. `.gitignore` already covers `.env`. Plan-level acceptance criterion: `grep -r 'sk-or-' apps/ose-grc-*` returns zero hits. |
| .NET toolchain (dotnet SDK 10, fantomas, fsharplint, AltCover, openapi-generator-cli) is not detected by `npm run doctor`, breaking the worktree-toolchain story for future contributors. | Medium   | `apps/ose-grc-be/global.json` pins dotnet 10. `apps/ose-grc-be/dotnet-tools.json` declares fantomas + altcover + fsharplint. `npm run doctor -- --fix` exercised in delivery checklist; verifies the dotnet stack restores end-to-end on a clean worktree. |
| `ose-grc-be-e2e` (or `ose-grc-e2e` if renamed) project lands without a matching `domain:ose-grc` tag, escaping the PR quality-gate detector. | Low      | `plan-checker` runs after delivery; rhino-cli `nx affected` spot-check is in the delivery checklist; CI dry-run confirms the lang/domain gates trigger.          |
| Spec hierarchy `specs/apps/ose-grc/` collides or misaligns with existing `specs/apps/` siblings (ayokoding/organiclever/oseplatform/rhino/wahidyankf). | Low      | Mirror organiclever's tree shape 1:1. `specs/apps/ose-grc/README.md` borrows organiclever's structure section verbatim.                                          |
