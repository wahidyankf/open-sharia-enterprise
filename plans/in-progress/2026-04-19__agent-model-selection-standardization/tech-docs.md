# Technical Documentation

## Current Architecture

### Claude Code → OpenCode Sync Pipeline

```
.claude/agents/*.md
        │
        ▼
rhino-cli agents sync
(apps/rhino-cli/internal/agents/converter.go)
        │
        ▼
.opencode/agent/*.md
```

The sync pipeline transforms Claude frontmatter into OpenCode YAML. Model field handling
lives in `converter.go:ConvertModel()`:

```go
func ConvertModel(claudeModel string) string {
    model := strings.TrimSpace(claudeModel)
    switch model {
    case "sonnet", "opus":
        return "zai-coding-plan/glm-5.1"
    case "haiku":
        return "zai-coding-plan/glm-5-turbo"
    default:
        // Default to most capable model.
        // "inherit" is not a valid OpenCode value.
        return "zai-coding-plan/glm-5.1"
    }
}
```

**Key observation**: `"opus"` is already an explicit case — handled identically to `"sonnet"`.
This means switching from omit to `model: opus` in Claude agents has zero impact on OpenCode
output. The only change is in the source Claude agent files and in the guarantee they provide.

### Model Tier Mapping (Full Picture)

| Claude Code alias | Claude Code model (April 2026)                     | OpenCode model ID             | GLM tier                 |
| ----------------- | -------------------------------------------------- | ----------------------------- | ------------------------ |
| `opus`            | `claude-opus-4-7` ($5/$25/MTok, 1M ctx)            | `zai-coding-plan/glm-5.1`     | 744B MoE, SWE-Bench 58.4 |
| `sonnet`          | `claude-sonnet-4-6` ($3/$15/MTok, 1M ctx)          | `zai-coding-plan/glm-5.1`     | same model               |
| `haiku`           | `claude-haiku-4-5-20251001` ($1/$5/MTok, 200k ctx) | `zai-coding-plan/glm-5-turbo` | purpose-built agentic    |
| `""` (omit)       | Inherits session model (tier-dependent)            | `zai-coding-plan/glm-5.1`     | same as opus/sonnet      |

**3-to-2 collapse**: Claude has three tiers; GLM (Z.ai Coding Plan) has two. `opus` and
`sonnet` collapse to the same GLM model because GLM-5.1 is the single top-tier option
(benchmarks ≈ Claude Opus 4.6, which is below Opus 4.7 but above Sonnet 4.6).

### ValidModels in rhino-cli

`apps/rhino-cli/internal/agents/types.go:ValidModels`:

```go
var ValidModels = map[string]bool{
    "":       true, // Empty is valid (inherits)
    "sonnet": true,
    "opus":   true,
    "haiku":  true,
}
```

All four values pass `validate:claude`. No code change needed.

### OpenCode Output Format

OpenCode agents use a different model field structure. After sync:

```yaml
# .opencode/agent/plan-maker.md (after standardization)
---
name: plan-maker
description: ...
model: zai-coding-plan/glm-5.1   # converted from model: opus
tools:
  read: true
  write: true
  ...
---
```

## Changes Required

### Change 1: `governance/development/agents/model-selection.md`

**Section updates**:

1. **Opus tier "Frontmatter" block** — change from:

   ```yaml
   # BEFORE: omit model field
   ---
   name: swe-typescript-dev
   description: ...
   tools: [Read, Write, Edit, Glob, Grep, Bash]
   color: purple
   ---
   ```

   To:

   ```yaml
   # AFTER: explicit model: opus
   ---
   name: swe-typescript-dev
   description: ...
   tools: Read, Write, Edit, Glob, Grep, Bash
   model: opus
   color: purple
   ---
   ```

2. **Opus tier "Justification" example** — change from
   "uses inherited `model: opus` (omit model field)" to
   "uses `model: opus`".

3. **New section "OpenCode / GLM Equivalents"** — insert after "Tier Comparison Summary":

   ```markdown
   ## OpenCode / GLM Equivalents

   Agents in `.claude/agents/` are auto-synced to `.opencode/agent/` by rhino-cli.
   The sync translates Claude model aliases to Zhipu AI (Z.ai) GLM model IDs.

   ### Model ID Mapping

   | Claude Code     | OpenCode                      | Capability notes                               |
   | --------------- | ----------------------------- | ---------------------------------------------- |
   | `model: opus`   | `zai-coding-plan/glm-5.1`     | 744B MoE; SWE-Bench Pro 58.4; ≈ Opus 4.6 class |
   | `model: sonnet` | `zai-coding-plan/glm-5.1`     | Same model as opus tier in OpenCode            |
   | `model: haiku`  | `zai-coding-plan/glm-5-turbo` | Purpose-built for agentic tool-calling         |
   | omit            | `zai-coding-plan/glm-5.1`     | Legacy default; prefer explicit values         |

   ### 3-to-2 Tier Collapse

   Claude Code supports three distinct tiers (Opus 4.7 > Sonnet 4.6 > Haiku 4.5).
   GLM Coding Plan has two: glm-5.1 (top) and glm-5-turbo (fast/agentic). The collapse means
   agents differentiated in Claude Code (opus vs sonnet) behave identically in OpenCode.
   This is an acceptable gap: the Claude Code tier assignment governs behavior in Claude sessions
   (the primary runtime), and OpenCode uses the best available GLM model for all non-haiku work.

   ### Why No Separate "Opus" GLM Tier

   GLM-5.1 benchmarks at SWE-Bench Pro 58.4, comparable to Claude Opus 4.6 (57.3) but below
   Opus 4.7. No GLM model currently exceeds Opus 4.7 capability. Using glm-5.1 for opus-tier
   agents is the best available option, not a perfect equivalence.
   ```

4. **Model version table** — add a new "Current Model Versions (April 2026)" table:

   ```markdown
   ## Current Model Versions (April 2026)

   | Claude Code alias | Model ID                    | Context     | Notes                      |
   | ----------------- | --------------------------- | ----------- | -------------------------- |
   | `opus`            | `claude-opus-4-7`           | 1M tokens   | Current top tier           |
   | `sonnet`          | `claude-sonnet-4-6`         | 1M tokens   | Daily driver               |
   | `haiku`           | `claude-haiku-4-5-20251001` | 200k tokens | Haiku 3 retired 2026-04-19 |
   ```

5. **Budget-adaptive design note** — add explanation under Opus tier that omitting
   `model` is intentional: the agent inherits the session's active model to match the
   user's account tier and token budget. Include warning against adding `model: opus`.

### Change 2: `CLAUDE.md`

Target line in "Format Differences":

```markdown
# BEFORE

- **Models**: Claude Code uses `sonnet`/`haiku` (or omits), OpenCode uses `zai-coding-plan/glm-5.1` (sonnet/opus/omitted), `zai-coding-plan/glm-5-turbo` (haiku)

# AFTER

- **Models**: Claude Code uses `sonnet`/`opus`/`haiku` (or omits for legacy compat); OpenCode uses `zai-coding-plan/glm-5.1` (opus/sonnet/omitted) and `zai-coding-plan/glm-5-turbo` (haiku). See [model-selection.md](./governance/development/agents/model-selection.md) for full tier mapping.
```

### Change 3: OpenCode re-sync

Run `npm run sync:claude-to-opencode` after all `.claude/agents/` changes. This is the final
step — it re-generates all `.opencode/agent/` files from updated sources. No manual edits
to `.opencode/agent/`.

## Validation Commands

```bash
# Validate Claude agent format
npm run validate:claude

# Validate sync consistency (Claude ↔ OpenCode match)
npm run validate:sync

# Dry-run sync (preview changes without writing)
npm run sync:dry-run

# Apply sync
npm run sync:claude-to-opencode

# rhino-cli unit tests (catch any fixture mismatches)
nx run rhino-cli:test:quick
```

## Risk Assessment

| Risk                                          | Likelihood | Impact | Mitigation                                  |
| --------------------------------------------- | ---------- | ------ | ------------------------------------------- |
| rhino-cli tests have fixture with empty model | Low        | Low    | Update fixture in test to use `model: opus` |
| A sub-agent silently gets wrong model today   | Medium     | Medium | This plan fixes it                          |
| Missed agent (still has empty model)          | Low        | Low    | `validate:claude` + grep audit catches it   |
| OpenCode output different from expected       | Low        | Low    | `validate:sync` catches divergence          |

## Worktree Setup

Per Scope A rules, execution MUST happen in a subrepo worktree:

```bash
cd /Users/wkf/ose-projects/ose-public
claude --worktree agent-model-standardization
# OR
git worktree add .claude/worktrees/agent-model-standardization -b worktree-agent-model-standardization origin/HEAD
```

Changes commit to branch `worktree-agent-model-standardization`, then open draft PR against
`main`.
