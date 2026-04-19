# Delivery Checklist

## Prerequisites

- [ ] Run `npm install` to install workspace dependencies
- [ ] Run `npm run doctor -- --fix` to converge the Go and Node toolchains (required before
      any `validate:*` or `sync:*` script can invoke the rhino-cli binary)
- [x] Running directly on `main` branch (no worktree — governance-only changes, no code)
- [x] Confirm `npm run validate:claude` passes on clean branch (baseline) _(verified 2026-04-19)_
- [x] Confirm `npm run validate:sync` passes on clean branch (baseline) _(verified 2026-04-19)_

### Commit Guidelines

- Commit changes thematically — group related changes into logically cohesive commits
- Follow Conventional Commits format: `<type>(<scope>): <description>`
- Split different domains/concerns into separate commits
- Do NOT bundle unrelated fixes into a single commit
- Example: separate `docs(model-selection): ...` from `fix(agents): ...` commits

---

## Phase 1: Update `governance/development/agents/model-selection.md` _(done)_

- [x] **1.1** Budget-Adaptive Inheritance block added to Opus tier section _(applied 2026-04-19)_
- [x] **1.2** "Current Model Versions (April 2026)" table added _(applied 2026-04-19)_
- [x] **1.3** "OpenCode / GLM Equivalents" section added with mapping table + collapse explanation _(applied 2026-04-19)_
- [x] **1.4** "Adding `model: opus` to opus-tier agents" row added to Common Mistakes _(applied 2026-04-19)_
- [x] **1.5** "Last Updated" set to 2026-04-19 _(applied 2026-04-19)_
- [x] **1.6** Committed: `docs(model-selection): add budget-adaptive inheritance note + opencode glm mapping` _(2026-04-19)_

---

## Phase 2: CLAUDE.md _(done)_

- [x] **2.1** Models row updated: `opus` alias added, OpenCode collapse documented,
      link to model-selection.md added _(applied 2026-04-19)_
- [x] **2.2** Committed: `docs(CLAUDE.md): update model format differences for opus alias` _(2026-04-19)_

---

## Phase 3: Propagate to Related Governance Docs _(done)_

- [x] **3.1** `governance/development/agents/ai-agents.md` — budget-adaptive note added to model field spec + Model Selection Guidelines _(applied 2026-04-19)_
- [x] **3.2** `governance/development/agents/best-practices.md` — budget-adaptive rationale added; plan-maker example corrected to show omit _(applied 2026-04-19)_
- [x] **3.3** `.claude/agents/README.md` — "Opus-tier agents omit `model` by design" note added _(applied 2026-04-19)_
- [x] **3.4** Committed: `docs(agents): propagate budget-adaptive model inheritance note` _(2026-04-19)_

---

## Phase 4: Agent Tier Audit — Right-Size Every Agent

Apply the `model-selection.md` decision tree to every agent in `.claude/agents/` to verify
tier assignments are correct. **Goal**: no agent over-budgeted (opus when sonnet suffices)
or under-budgeted (haiku for tasks requiring reasoning).

**Decision tree** (from model-selection.md):

- Creative reasoning / code generation / architectural decisions → **Opus (omit)**
- Rule-based validation / structured procedure / template following → **Sonnet**
- Purely mechanical / deterministic steps / no reasoning → **Haiku**

**Audit approach**:

For each agent, read its description + Model Selection Justification block. Apply the
decision tree. If the current tier doesn't match, update frontmatter + justification.

- [ ] **4.1** Glob `.claude/agents/*.md` (excluding `README.md`) — list all agents with
      current `model:` field (empty = opus-inherit, `sonnet`, `haiku`)
- [ ] **4.2** For each opus-tier agent (omit), verify it passes the first branch:
      "open-ended creative work, code generation, multi-step architectural judgment".
      Flag any that are actually rule-following or mechanical.
- [ ] **4.3** For each sonnet-tier agent, verify it passes the second branch:
      "rule application, validation, structured output". Flag any that are mechanical
      (should be haiku) or genuinely creative (should be opus).
- [ ] **4.4** For each haiku-tier agent, verify it is truly deterministic with no branching
      logic. Flag any requiring error-handling reasoning (should be sonnet).
- [ ] **4.5** Apply all tier corrections:
  - Update `model:` frontmatter field
  - Update Model Selection Justification block to match new tier
  - Do NOT change any agent's color or tools
- [ ] **4.6** Commit corrections: `fix(agents): right-size model tiers per decision tree audit`

**Special attention agents** (known borderline cases):

| Agent                               | Current | Likely correct | Reason to check                                    |
| ----------------------------------- | ------- | -------------- | -------------------------------------------------- |
| `web-research-maker`                | omit    | sonnet         | Structured research pattern; no invention needed   |
| `plan-executor`                     | omit    | sonnet         | Follows delivery checklist, not architectural work |
| `repo-ose-primer-adoption-maker`    | omit    | sonnet         | Classify + report per classifier table             |
| `repo-ose-primer-propagation-maker` | omit    | sonnet/opus    | Transform + PR creation; may need judgment         |
| `statusline-setup`                  | omit    | haiku          | Single config edit, fully deterministic            |
| `general-purpose`                   | omit    | omit           | Genuinely open-ended; opus appropriate             |

> **Note**: Confirm by reading each agent's actual description before changing. The table
> above lists candidates — the agent content is authoritative.

---

## Phase 5: repo-rules-checker OCD Validation

Run `repo-rules-checker` in OCD mode after Phase 4 agent changes.

- [ ] **5.1** Invoke `repo-rules-checker` with `strictness: ocd`
- [ ] **5.2** Review findings — fix all CRITICAL, HIGH, and MEDIUM findings
- [ ] **5.3** Re-run until two consecutive zero-finding passes (per quality gate workflow)
- [ ] **5.4** Commit any fixes: `fix(governance): repo-rules-checker ocd findings`

---

## Phase 6: Validation

- [ ] **6.1** Run `npm run validate:claude` — expect zero errors
- [ ] **6.2** Run `npm run validate:sync` — expect zero errors
- [ ] **6.3** Run `nx run rhino-cli:test:quick` — expect pass

---

## Phase 7: Sync + Final Gate

- [ ] **7.1** Run `npm run sync:dry-run` — preview OpenCode output (agents with updated
      tiers should show model ID changes in .opencode/agent/)
- [ ] **7.2** Run `npm run sync:claude-to-opencode` — apply sync
- [ ] **7.3** Run `npm run validate:sync` — final pass

### Local Quality Gates (Before Push)

- [ ] Run `npm run lint:md` — lint all markdown files (all plan files and governance docs
      are markdown; this is the relevant pre-push gate for documentation-only changes)
- [ ] Run `npm run lint:md:fix` — auto-fix any markdown violations
- [ ] Fix ALL failures found — including preexisting issues not caused by your changes

> **Important**: Fix ALL failures found during quality gates, not just those caused by your
> changes. This follows the root cause orientation principle — proactively fix preexisting
> errors encountered during work. Do not defer or mention-and-skip existing issues.

- [ ] **7.4** Push directly to `origin/main`

### Post-Push Verification

- [ ] Monitor GitHub Actions CI runs for the push to `main` — verify all checks pass
- [ ] If any CI check fails, fix immediately and push a follow-up commit
- [ ] Do NOT proceed to plan archival until CI is green

### Plan Archival

- [ ] **7.5a** Verify ALL delivery checklist items are ticked
- [ ] **7.5b** Verify ALL quality gates pass (local + CI)
- [ ] **7.5c** `git mv plans/in-progress/2026-04-19__agent-model-selection-standardization/ plans/done/`
- [ ] **7.5d** Update `plans/in-progress/README.md` — remove this plan entry
- [ ] **7.5e** Update `plans/done/README.md` — add this plan entry with completion date
- [ ] **7.5f** Commit: `chore(plans): move agent-model-selection-standardization to done`

---

## Acceptance Criteria Checklist

- [x] `model-selection.md` Opus tier section explicitly documents omit-as-budget-adaptive _(done)_
- [x] `model-selection.md` Common Mistakes includes "Adding `model: opus` to opus-tier agents" _(done)_
- [x] `model-selection.md` has "OpenCode / GLM Equivalents" section _(done)_
- [x] `model-selection.md` has "Current Model Versions (April 2026)" table _(done)_
- [x] `CLAUDE.md` Format Differences Models row includes `opus` alias _(done)_
- [x] Related governance docs (`ai-agents.md`, `best-practices.md`, `.claude/agents/README.md`)
      contain the budget-adaptive design note _(done)_
- [ ] All agents in `.claude/agents/` have correct tier assignment per decision tree (no over/under-budgeting)
- [ ] Model Selection Justification blocks match each agent's actual tier
- [ ] `repo-rules-checker` OCD passes with zero findings
- [ ] `npm run validate:claude` passes
- [ ] `npm run validate:sync` passes
- [ ] No `.claude/agents/*.md` opus-tier file has `model: opus` (inherit = correct)
