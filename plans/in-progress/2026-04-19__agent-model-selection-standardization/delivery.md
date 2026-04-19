# Delivery Checklist

## Prerequisites

- [ ] Open `ose-public`-rooted worktree session (Scope A):
      `cd ose-public && claude --worktree agent-model-standardization`
- [ ] Run `npm install && npm run doctor -- --fix` in worktree root
- [ ] Confirm `npm run validate:claude` passes on clean branch (baseline)
- [ ] Confirm `npm run validate:sync` passes on clean branch (baseline)

---

## Phase 1: Update `governance/development/agents/model-selection.md`

- [ ] **1.1** Update Opus tier section — add "Budget-Adaptive Inheritance" block after the
      frontmatter example explaining WHY the field is omitted (inherit = matches user's
      account tier / token budget by design)
- [ ] **1.2** Add "Current Model Versions (April 2026)" table (Opus 4.7, Sonnet 4.6,
      Haiku 4.5-20251001; note Haiku 3 retired 2026-04-19)
- [ ] **1.3** Add "OpenCode / GLM Equivalents" section after "Tier Comparison Summary":
  - Mapping table: `opus`/`sonnet`/omit → `zai-coding-plan/glm-5.1`,
    `haiku` → `zai-coding-plan/glm-5-turbo`
  - 3-to-2 tier collapse explanation
  - GLM-5.1 capability context (SWE-Bench 58.4 ≈ Opus 4.6 class, not Opus 4.7)
- [ ] **1.4** Add row to "Common Mistakes" table:
  - Mistake: Adding `model: opus` to opus-tier agents
  - Problem: Bypasses budget-adaptive inheritance; forces Opus charges regardless of
    user's account tier
  - Correction: Omit the field — agent inherits session model to match user's tier
- [ ] **1.5** Update "Last Updated" date to 2026-04-19
- [ ] **1.6** Commit: `docs(model-selection): add budget-adaptive design note + opencode glm mapping`

---

## Phase 2: CLAUDE.md _(done during plan authoring)_

- [x] **2.1** Models row updated: `opus` alias added, OpenCode collapse documented,
      link to model-selection.md added _(applied 2026-04-19)_
- [ ] **2.2** Commit: `docs(CLAUDE.md): update model format differences for opus alias`

---

## Phase 3: Propagate to Related Governance Docs

Use `repo-rules-maker` to find all agent governance files that describe opus-tier model
behavior and add the budget-adaptive design note where missing.

**Files to check and update:**

- [ ] **3.1** `governance/development/agents/ai-agents.md` — find model selection
      descriptions for opus-tier agents; add budget-adaptive note
- [ ] **3.2** `governance/development/agents/best-practices.md` — check for model
      selection guidance; add budget-adaptive note if present
- [ ] **3.3** `.claude/agents/README.md` — check model tier descriptions; add
      budget-adaptive note to opus tier description
- [ ] **3.4** Commit: `docs(agents): propagate budget-adaptive model inheritance note`

---

## Phase 4: repo-rules-checker OCD Validation

Run `repo-rules-checker` in OCD mode before any commit or push.

- [ ] **4.1** Invoke `repo-rules-checker` with `strictness: ocd`
- [ ] **4.2** Review findings — fix all CRITICAL and HIGH findings
- [ ] **4.3** Re-run until two consecutive zero-finding passes (per quality gate workflow)
- [ ] **4.4** Commit any fixes: `fix(governance): repo-rules-checker ocd findings`

---

## Phase 5: Validation

- [ ] **5.1** Run `npm run validate:claude` — expect zero errors
- [ ] **5.2** Run `npm run validate:sync` — expect zero errors
- [ ] **5.3** Run `nx run rhino-cli:test:quick` — expect pass

---

## Phase 6: Sync + Final Gate

- [ ] **6.1** Run `npm run sync:dry-run` — preview OpenCode output (no changes expected
      since agent files unchanged; only governance docs changed)
- [ ] **6.2** Run `npm run sync:claude-to-opencode` — apply sync
- [ ] **6.3** Run `npm run validate:sync` — final pass
- [ ] **6.4** Push branch `worktree-agent-model-standardization` to origin
- [ ] **6.5** Open draft PR against `main` with title:
      `docs(agents): document budget-adaptive model inheritance + opencode glm mapping`
- [ ] **6.6** Move this plan to `plans/done/` after PR merges to `main`

---

## Acceptance Criteria Checklist

- [ ] `model-selection.md` Opus tier section explicitly documents omit-as-budget-adaptive
- [ ] `model-selection.md` Common Mistakes includes "Adding `model: opus` to opus-tier agents"
- [ ] `model-selection.md` has "OpenCode / GLM Equivalents" section
- [ ] `model-selection.md` has "Current Model Versions (April 2026)" table
- [ ] `CLAUDE.md` Format Differences Models row includes `opus` alias _(done)_
- [ ] Related governance docs (`ai-agents.md`, `best-practices.md`, `.claude/agents/README.md`)
      contain the budget-adaptive design note
- [ ] `repo-rules-checker` OCD passes with zero findings
- [ ] `npm run validate:claude` passes
- [ ] `npm run validate:sync` passes
- [ ] No `.claude/agents/*.md` opus-tier file has `model: opus` (inherit = correct)
