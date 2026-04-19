# Agent Model Selection Standardization

## Context

The repo has ~70 specialized AI agents split across two runtimes:

- **Claude Code** (`.claude/agents/`) — uses `model: opus|sonnet|haiku` or omit
- **OpenCode** (`.opencode/agent/`) — auto-synced via `npm run sync:claude-to-opencode`
  (rhino-cli builds `.opencode/agent/` from `.claude/agents/`)

The sync already handles model translation:

- Claude `opus` → OpenCode `zai-coding-plan/glm-5.1`
- Claude `sonnet` → OpenCode `zai-coding-plan/glm-5.1`
- Claude `haiku` → OpenCode `zai-coding-plan/glm-5-turbo`
- Claude `""` (omit) → OpenCode `zai-coding-plan/glm-5.1` (default)

## Problems Being Fixed

1. **Budget-adaptive design undocumented** — Opus-tier agents omit the `model` field so
   they inherit the session's active model, adapting to the user's account tier and token
   budget. This is intentional (Max gets Opus 4.7, Pro gets Sonnet 4.6) but nowhere
   documented. The gap risks someone adding `model: opus` to "fix" it, breaking the
   budget-adaptive behavior.
2. **OpenCode mapping undocumented** — `model-selection.md` covers Claude Code only; GLM
   model IDs, the 3-to-2 tier collapse, and GLM capability context are nowhere in policy.
3. **Model version refs stale** — no Claude 4.x IDs, no Haiku 3 retirement notice
   (retired 2026-04-19).
4. **CLAUDE.md described wrong plan format** — said "four documents / requirements.md"
   while convention specifies five. Fixed during plan authoring via `repo-rules-maker`.
5. **Benchmark data undocumented** — no cited benchmark reference in the project. Tier
   assignments in `model-selection.md` are unverifiable without external research.
   GLM-5-turbo has no standard benchmarks at all; GLM-5.1 scores are self-reported only.
   7 agents incorrectly on opus-inherit tier (rubric-bound work) and 1 agent incorrectly
   on sonnet (deterministic URL replacement).

## Scope

**Subrepo**: `ose-public` (running directly on `main` — governance-only changes, no code).
This plan is authored and executed from within the `ose-public` subrepo directly, not
orchestrated from the parent container repo. The parent-level Scope A worktree rule does
not apply here; that rule triggers only when a plan is orchestrated from the parent repo
and names `ose-public` as a target subrepo.

**Files touched**:

- `governance/development/agents/model-selection.md` — primary policy + benchmark citations _(phases 1, 6)_
- `CLAUDE.md` — plan format + model aliases _(phase 2)_
- `governance/development/agents/ai-agents.md` — budget-adaptive propagation _(phase 3)_
- `governance/development/agents/best-practices.md` — budget-adaptive propagation _(phase 3)_
- `.claude/agents/README.md` — opus-tier omit note + benchmark pointer _(phases 3, 6)_
- `docs/reference/ai-model-benchmarks.md` — new benchmark reference doc _(phase 4)_
- `.claude/agents/*.md` — 8 agents: tier corrections _(phase 5)_
- `.opencode/agent/*.md` — re-synced to reflect tier changes _(phase 9)_

**Files NOT touched**: `apps/rhino-cli/` source (no code changes needed), 62 unchanged agents.

## Approach Summary

No rhino-cli code changes needed. Fix is documentation + targeted tier corrections:

1. _(done)_ Update `model-selection.md` — budget-adaptive inheritance, OpenCode section,
   version table, Common Mistakes entry
2. _(done)_ Update `CLAUDE.md` — plan format + model aliases
3. _(done)_ Propagate budget-adaptive note to related governance docs
4. Create `docs/reference/ai-model-benchmarks.md` — cited benchmark reference for all 5 models
5. Correct 8 agent tiers (7 OMIT→SONNET, 1 SONNET→HAIKU) per benchmark-informed audit
6. Add benchmark citations to `model-selection.md` + `.claude/agents/README.md` via `repo-rules-maker`
7. Re-run sync to reflect tier changes in `.opencode/agent/`

## Plan Documents

| File                           | Purpose                                                          |
| ------------------------------ | ---------------------------------------------------------------- |
| [brd.md](./brd.md)             | Business rationale, pain points, affected roles, success metrics |
| [prd.md](./prd.md)             | User stories, Gherkin acceptance criteria, product scope         |
| [tech-docs.md](./tech-docs.md) | Sync architecture, model mapping, exact diffs, risk              |
| [delivery.md](./delivery.md)   | Phased execution checklist                                       |

## References

- Policy: [governance/development/agents/model-selection.md](../../../governance/development/agents/model-selection.md)
- Plans convention: [governance/conventions/structure/plans.md](../../../governance/conventions/structure/plans.md)
- Sync converter: [apps/rhino-cli/internal/agents/converter.go](../../../apps/rhino-cli/internal/agents/converter.go)
- Sync types: [apps/rhino-cli/internal/agents/types.go](../../../apps/rhino-cli/internal/agents/types.go)
