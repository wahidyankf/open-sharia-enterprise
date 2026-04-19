# Business Requirements Document

## Business Goal

Document the intentional budget-adaptive model inheritance design for opus-tier agents
and extend the model selection policy to cover both Claude Code and OpenCode/GLM in a
single canonical reference. No agent files change — the existing "omit = inherit" behavior
is correct by design.

## Background and Pain Points

### Pain Point 1: Budget-Adaptive Design Is Undocumented

Opus-tier agents intentionally omit the `model` field so the agent inherits the session's
active model. This is the **correct** and **intended** behavior:

- Max / Team Premium session → inherits Opus 4.7 (high-capability output)
- Pro / Standard / API session → inherits Sonnet 4.6 (matches user's budget)

The design decision is: agents should adapt to the user's purchasing decision rather than
force a specific model tier regardless of cost. A Pro-tier user who gets Sonnet-quality
output from plan-maker is getting behavior that matches their account — not a bug.

The problem is this design is **nowhere documented**. Someone reading the policy today sees
"omit the model field" with no explanation of why. The risk: a well-intentioned developer
adds `model: opus` to an opus-tier agent to "make it more explicit", unknowingly breaking
the budget-adaptive behavior and forcing Opus charges on users who chose a lower tier.

**Model resolution chain** (official Claude Code docs, verified April 2026):
env var → caller parameter → frontmatter `model:` → session default (inherit)

Omitting the field uses step 4 (inherit). This is intentional and correct.

### Pain Point 2: OpenCode Mapping Undocumented

`model-selection.md` covers Claude Code only. The OpenCode GLM model equivalents
(`zai-coding-plan/glm-5.1`, `zai-coding-plan/glm-5-turbo`), their capability mapping, and
the 3-to-2 tier collapse (both `opus` and `sonnet` collapse to `glm-5.1` in OpenCode) are
nowhere in the policy documentation. Developers adding new agents or reviewing agent tiers
have no authoritative source for OpenCode model selection.

### Pain Point 3: Model Version References Are Stale

`model-selection.md` contains no Claude 4.x model IDs and no reference to Haiku 3
retirement (retired 2026-04-19, the day this plan was created). Stale version references
erode trust in the policy document.

### Pain Point 4: CLAUDE.md Describes the Wrong Plan Format

`CLAUDE.md` says "Default plan layout: four documents — README.md, requirements.md,
tech-docs.md, delivery.md." The actual plans convention (`governance/conventions/structure/plans.md`,
updated 2026-04-18) specifies five documents: README.md, brd.md, prd.md, tech-docs.md,
delivery.md. This discrepancy in the primary developer onboarding document actively misleads
anyone creating a new plan.

## Business Impact

| Stakeholder                  | Impact today                            | Impact after fix               |
| ---------------------------- | --------------------------------------- | ------------------------------ |
| Maintainer on Pro plan       | Opus agents run on Sonnet silently      | Correct tier guaranteed        |
| Maintainer adding new agent  | No OpenCode model docs to reference     | Clear dual-platform policy     |
| AI agents creating plans     | Follow stale 4-doc format               | Follow correct 5-doc format    |
| Any reviewer auditing agents | Cannot verify OpenCode tier from policy | Single authoritative reference |

## Affected Roles

- **Maintainer (all hats)** — directly affected by model degradation and doc stale-ness
- **`plan-maker` agent** — reads CLAUDE.md to determine plan format; currently produces wrong format
- **`agent-maker` agent** — reads model-selection.md when creating new agents; gets incomplete info
- **`repo-rules-checker` agent** — validates model selection compliance; policy gaps reduce check quality

## Business-Level Success Metrics

1. **Observable**: `for f in .claude/agents/*.md; do grep -qm1 "^model:" "$f" || echo "BLANK: $f"; done` outputs nothing (zero blank model fields)
2. **Observable**: `npm run validate:claude` exits 0 after all changes
3. **Observable**: `model-selection.md` diff contains an "OpenCode / GLM Equivalents" section and a "Current Model Versions" table
4. **Observable**: `CLAUDE.md` diff changes "four documents" to "five documents" and removes `requirements.md` from the file list
5. _Judgment call:_ agent invocations from Pro-tier sessions will produce opus-quality output for reasoning-intensive agents going forward; no baseline measured

## Business-Scope Non-Goals

- This is not a re-evaluation of which agents belong in which tier — tier assignments are correct
- This is not a rhino-cli source code change — the tooling already supports `model: opus`
- This is not an agent capability expansion — no new agents, no new features
- This does not address the GLM-5.1 capability gap vs Claude Opus 4.7 — that is a platform-level constraint outside this plan's scope

## Business Risks and Mitigations

| Risk                                                                        | Likelihood | Mitigation                                                         |
| --------------------------------------------------------------------------- | ---------- | ------------------------------------------------------------------ |
| rhino-cli test fixtures hard-code empty model and fail                      | Low        | Update fixture files (not source) if needed                        |
| An agent missed in the audit still has blank model                          | Low        | Phase 7 audit grep + `validate:claude` catch it                    |
| CLAUDE.md update conflicts with concurrent plans using old format           | Low        | This is the only in-progress plan currently creating new docs      |
| Stale model-selection.md wording surfaces in agent outputs until re-indexed | Negligible | Agents re-read files on each invocation; no caching of policy docs |
