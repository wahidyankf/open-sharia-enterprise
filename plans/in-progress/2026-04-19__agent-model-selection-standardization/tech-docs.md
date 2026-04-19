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
# .opencode/agent/plan-maker.md (after sync — opus-tier agent with omitted model)
---
name: plan-maker
description: ...
model: zai-coding-plan/glm-5.1   # default — ConvertModel("") returns glm-5.1
tools:
  read: true
  write: true
  ...
---
```

## Changes Required

### Change 1: `governance/development/agents/model-selection.md` _(applied 2026-04-19)_

**Section updates applied**:

1. **Budget-Adaptive Inheritance block** — added after the Opus tier frontmatter example.
   Explains why omitting `model` is intentional (session inheritance adapts to user's account
   tier and token budget). Includes account-tier table (Max/Team → Opus 4.7, Pro/Standard →
   Sonnet 4.6). Includes warning: Do NOT add `model: opus`.

2. **Frontmatter example** — kept as omit (no `model` field). The surrounding explanation
   clarifies this is intentional budget-adaptive design.

3. **New section "OpenCode / GLM Equivalents"** — inserted after "Tier Comparison Summary".
   Mapping table: omit/sonnet → `zai-coding-plan/glm-5.1`, haiku → `zai-coding-plan/glm-5-turbo`.
   3-to-2 tier collapse explanation. GLM-5.1 capability context.

4. **Model version table** — "Current Model Versions (April 2026)" table added with Opus 4.7,
   Sonnet 4.6, Haiku 4.5-20251001; Haiku 3 retirement note (2026-04-19).

5. **Common Mistakes row** — "Adding `model: opus` to opus-tier agents" added. Problem:
   bypasses budget-adaptive inheritance. Correction: omit the field.

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

## Execution Context

This plan runs directly on `main` — no worktree needed (governance-only changes, no code).
The Scope declaration in `delivery.md` and `README.md` confirms this; the subrepo worktree
rule (Scope A) does not apply here.

## Dependencies

Tools required to execute the delivery plan:

- **Node.js 24.x + npm** — for `npm run validate:claude`, `npm run validate:sync`,
  `npm run sync:claude-to-opencode`. Managed by Volta; `npm install` installs the workspace.
- **Go toolchain** — required to build rhino-cli before any `npm run validate:*` or
  `npm run sync:*` script can run (these scripts call the rhino-cli binary). Install via
  `npm run doctor -- --fix`.
- **rhino-cli binary** — built from `apps/rhino-cli/` as part of `nx run rhino-cli:build`
  or implicitly by the npm scripts. `npm run doctor -- --fix` ensures Go is present.

**Setup sequence**:

```bash
npm install                  # install workspace deps
npm run doctor -- --fix      # converge Go + Node toolchains (required before validate/sync)
```

## Testing Strategy

Validation commands test that agent frontmatter parses correctly and that Claude↔OpenCode
sync is consistent:

- `npm run validate:claude` — parses every `.claude/agents/*.md` frontmatter; exit code 0
  means all model values are in `ValidModels` and no required fields are missing.
- `npm run validate:sync` — compares `.claude/agents/*.md` with `.opencode/agent/*.md`;
  exit code 0 means no drift between the two runtime configs.
- `nx run rhino-cli:test:quick` — runs rhino-cli unit tests including Gherkin BDD scenarios
  with fixture agents; catches fixture regressions when model values change.

**Pass criterion**: all three commands exit with code 0 and print zero error lines.

## Rollback

Each delivery phase has a corresponding commit. To revert:

- **Phase 1 only** (`model-selection.md`): `git revert <phase-1-sha>`
- **Phase 2 only** (`CLAUDE.md`): `git revert <phase-2-sha>`
- **Phase 3 only** (governance docs propagation): `git revert <phase-3-sha>`
- **Phase 4** (agent tier corrections): `git revert <phase-4-sha>`
- **Full rollback**: revert all four commits in reverse order

No DB migrations, no infra changes, no compiled artifacts. A revert is safe at any phase
boundary — each phase commit is self-contained and reversible.
