# Technical Documentation: Full Migration from Claude Code to OpenCode

## Architecture Overview

### Current State

```
┌─────────────────────────────────────────────────────────────┐
│                    Dual-Format Architecture                  │
├─────────────────────────────────────────────────────────────┤
│                                                               │
│  ┌─────────────────┐                                        │
│  │ .claude/agents/ │ ◄───── Source of Truth                  │
│  │   (45 agents)   │                                        │
│  └────────┬────────┘                                        │
│           │                                                 │
│           ▼ (convert-agents-to-opencode.py)                 │
│  ┌─────────────────┐                                        │
│  │.opencode/agent/ │ ◄───── Generated                        │
│  │   (45 agents)   │                                        │
│  └─────────────────┘                                        │
│                                                               │
│  ┌─────────────────────┐                                    │
│  │   .claude/skills/   │ ◄───── Shared (23 skills)           │
│  └─────────────────────┘                                    │
│                                                               │
│  ┌─────────────┐     ┌──────────────┐                       │
│  │ CLAUDE.md   │     │  AGENTS.md   │                         │
│  │  (~30k)     │     │   (~1k)      │                         │
│  └─────────────┘     └──────────────┘                         │
│                                                               │
└─────────────────────────────────────────────────────────────┘
```

### Target State

```
┌─────────────────────────────────────────────────────────────┐
│                  Single-Format Architecture                 │
├─────────────────────────────────────────────────────────────┤
│                                                               │
│  ┌─────────────────┐                                        │
│  │.opencode/agent/ │ ◄───── Source of Truth                  │
│  │   (45 agents)   │                                        │
│  └─────────────────┘                                        │
│                                                               │
│  ┌─────────────────────┐                                    │
│  │.opencode/skill/     │ ◄───── Shared (23 skills)           │
│  │   (permission-based)│                                    │
│  └─────────────────────┘                                    │
│                                                               │
│  ┌──────────────────────────────────────────────────┐        │
│  │            AGENTS.md (expanded)                  │        │
│  │        (~5k-10k lines, all guidance)            │        │
│  └──────────────────────────────────────────────────┘        │
│                                                               │
│  ┌──────────────────────────────────────────────────┐        │
│  │  Eliminated: CLAUDE.md, .claude/, scripts       │        │
│  └──────────────────────────────────────────────────┘        │
│                                                               │
└─────────────────────────────────────────────────────────────┘
```

## Agent Format Conversion

### Claude Code Format

```yaml
---
name: docs-checker
description: Validates documentation for quality and consistency
tools: Read, Grep, Glob
model: sonnet
color: "#FF6B6B"
tags: [docs, validation, checker]
---
# Agent Body
Instructions for validating documentation...
```

### OpenCode Format

```yaml
---
description: Validates documentation for quality and consistency
mode: subagent
model: zai/glm-4.7
temperature: 0.1
tools:
  read: true
  grep: true
  glob: true
  write: false
  edit: false
  bash: false
permission:
  skill:
    docs-applying-content-quality: allow
    docs-validating-factual-accuracy: allow
---
# Agent Body
Instructions for validating documentation...
```

### Field Mapping

| Claude Code   | OpenCode       | Notes                                        |
| ------------- | -------------- | -------------------------------------------- |
| `name`        | (filename)     | Agent name inferred from filename            |
| `description` | `description`  | Same content                                 |
| `tools`       | `tools.<tool>` | Comma-separated → object with boolean values |
| (omitted)     | `mode`         | Default: "subagent" (all current agents)     |
| `model`       | `model`        | Format: `zai/glm-4.7` (GLM models)           |
| (omitted)     | `temperature`  | Default: varies by agent type                |
| `color`       | (removed)      | Not used in OpenCode                         |
| `tags`        | (removed)      | Not used in OpenCode                         |
| (omitted)     | `permission`   | New: skill permissions, bash/edit control    |

### Model Alias Migration (CRITICAL)

**Issue**: All 45 OpenCode agents currently use Claude Code model aliases (`sonnet`, `opus`, `haiku`). OpenCode requires GLM model names (`zai/glm-4.7`, `zai/glm-4.7-flash`, `zai/glm-4.7-plus`, or `inherit`).

#### Current Agent State

**Example from `.opencode/agent/docs-checker.md`**:

```yaml
---
description: Validates documentation for quality and consistency
model: sonnet  ❌ Claude Code alias (not valid in OpenCode)
tools:
  read: true
  grep: true
  glob: true
  write: false
  edit: false
  bash: false
---
```

**Audit Results**: 52 agents use model aliases (grep search result)

- 48 agents use `model: sonnet`
- 4 agents use `model: haiku` (apps-ayokoding-web-title-maker, apps-ose-platform-web-deployer, etc.)
- 0 agents use `model: opus`

#### Model Alias to Full Name Mapping

| Claude Code Alias | OpenCode Model      | Notes                                        |
| ----------------- | ------------------- | -------------------------------------------- |
| `sonnet`          | `zai/glm-4.7`       | Most agents use this                         |
| `opus`            | `zai/glm-4.7-plus`  | Advanced reasoning (not currently used)      |
| `haiku`           | `zai/glm-4.7-flash` | Fast, simple tasks (deployers, title makers) |
| `inherit`         | `inherit`           | Use main conversation model (same in both)   |

#### Migration Strategy

**Phase 2: Model Field Update**

1. **Audit all agents**: `grep -r "model:" .opencode/agent/*.md`
2. **Replace aliases with GLM models**:

   ```bash
   # Replace sonnet → zai/glm-4.7
   sed -i 's/model: sonnet/model: zai\/glm-4.7/g' .opencode/agent/*.md

   # Replace haiku → zai/glm-4.7-flash
   sed -i 's/model: haiku/model: zai\/glm-4.7-flash/g' .opencode/agent/*.md

   # Replace opus → zai/glm-4.7-plus (if any)
   sed -i 's/model: opus/model: zai\/glm-4.7-plus/g' .opencode/agent/*.md
   ```

3. **Validate**: Ensure all agents have valid OpenCode model names
4. **Test**: Invoke sample agents to verify model configuration works

#### Target State

**After migration, `.opencode/agent/docs-checker.md`**:

```yaml
---
description: Validates documentation for quality and consistency
model: zai/glm-4.7  ✅ GLM model name
tools:
  read: true
  grep: true
  glob: true
  write: false
  edit: false
  bash: false
---
```

**Validation Criteria**:

- [ ] 0 agents use Claude Code model aliases
- [ ] All agents use GLM model names (`zai/glm-4.7`, `zai/glm-4.7-flash`, `zai/glm-4.7-plus`, or `inherit`)
- [ ] No references to `anthropic/claude-*` models in any agent
- [ ] All agents validate with OpenCode schema
- [ ] Sample agents invoke successfully

### Tool Permission Mapping

#### Claude Code → OpenCode

**Claude Code**:

```yaml
tools: Read, Grep, Glob, Write, Edit, Bash
```

**OpenCode**:

```yaml
tools:
  read: true
  grep: true
  glob: true
  write: true
  edit: true
  bash: true
```

#### Claude Code (omitted tools) → OpenCode

**Claude Code**:

```yaml
# tools field omitted = inherits all
```

**OpenCode**:

```yaml
# tools field omitted = inherits all
# OR explicit:
tools:
  read: true
  grep: true
  glob: true
  write: true
  edit: true
  bash: true
```

#### Permission Control (OpenCode Only)

**Read-only agent**:

```yaml
tools:
  read: true
  grep: true
  glob: true
  write: false
  edit: false
  bash: false
permission:
  edit: deny
  bash: deny
```

**Full-access agent**:

```yaml
tools:
  read: true
  write: true
  edit: true
  bash: true
  grep: true
  glob: true
permission:
  bash:
    "rm *": ask
    "git push": ask

**Loading**: Explicit permission, agent invokes via `skill()` tool

**Status**: Target state

### Migration Strategy

**Phase 1: Skills Location Migration (MAJOR CHANGE)**

**Decision**: Move skills to OpenCode standard location

**Rationale**:
- OpenCode standard is `.opencode/skill/<name>/SKILL.md`
- Fully migrating from Claude Code means deleting all Claude Code artifacts
- Clean break from Claude Code to OpenCode
- No dual maintenance burden

**Steps**:
1. Create `.opencode/skill/` directory structure
2. Move all 23 skills from `.claude/skills/` to `.opencode/skill/`
3. Update skill frontmatter to OpenCode format (remove `allowed-tools`, `model`)
4. Validate all skills load correctly

**Target State**:

After migration:
- Skills exist at `.opencode/skill/<name>/SKILL.md` (primary location)
- `.claude/skills/` DELETED (no Claude Code artifacts remain)
- OpenCode searches `.opencode/skill/` when discovering skills
- All agents have `permission.skill` frontmatter
- All 23 skills validate
```
