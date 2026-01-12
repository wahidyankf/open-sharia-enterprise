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
│  │   (46 agents)   │                                        │
│  └────────┬────────┘                                        │
│           │                                                 │
│           ▼ (convert-agents-to-opencode.py)                 │
│  ┌─────────────────┐                                        │
│  │.opencode/agent/ │ ◄───── Generated                        │
│  │   (46 agents)   │                                        │
│  └─────────────────┘                                        │
│                                                               │
│  ┌─────────────────────┐                                    │
│  │   .claude/skills/   │ ◄───── Shared (23 skills)           │
│  └─────────────────────┘                                    │
│                                                               │
│  ┌─────────────┐     ┌──────────────┐                       │
│  │ CLAUDE.md   │     │  AGENTS.md   │                         │
  │  │  (348)     │     │   (232)      │                         │
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
│  │   (46 agents)   │                                        │
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

### Model Configuration Migration (CRITICAL)

**Issue**: `.opencode/opencode.json` config currently uses Claude models (`anthropic/claude-sonnet-4-5`, `anthropic/claude-haiku-4`). The main conversation model should be updated to use GLM models instead.

#### Current Configuration

**From `.opencode/opencode.json`**:

```json
{
  "$schema": "https://opencode.ai/config.json",
  "model": "anthropic/claude-sonnet-4-5",
  "small_model": "anthropic/claude-haiku-4"
}
```

❌ Uses Claude models (not GLM)

#### Agent Model Configuration

**Important**: OpenCode agents already use correct GLM model format (`zai/glm-4.7`, `zai/glm-4.7-flash`, etc.). No changes needed to agent frontmatter.

**Example from `.opencode/agent/docs-checker.md`**:

```yaml
---
description: Validates documentation for quality and consistency
model: zai/glm-4.7  ✅ Already correct GLM format
tools:
  read: true
  grep: true
  glob: true
  write: false
  edit: false
  bash: false
---
```

**Agent Model Audit**:

```bash
$ grep -r "model:" .opencode/agent/*.md | head -5
docs-checker.md:model: zai/glm-4.7
docs-fixer.md:model: zai/glm-4.7
agent-maker.md:model: zai/glm-4.7
apps-ayokoding-web-title-maker.md:model: zai/glm-4.7-flash
apps-ose-platform-web-deployer.md:model: zai/glm-4.7-flash
```

✅ All 46 agents already use GLM model names (no Claude aliases found)

#### Model Name Reference

| GLM Model Name      | Use Case                                        |
| ------------------- | ----------------------------------------------- |
| `zai/glm-4.7`       | Advanced reasoning, deep analysis (most agents) |
| `zai/glm-4.7-flash` | Fast, simple tasks, title generation            |
| `zai/glm-4.7-plus`  | Advanced reasoning, complex orchestration       |
| `inherit`           | Use main conversation model                     |

#### Migration Strategy

**Phase 2: Config File Update**

1. **Update `.opencode/opencode.json`**:

   ```json
   {
     "$schema": "https://opencode.ai/config.json",
     "model": "zai/glm-4.7",
     "small_model": "zai/glm-4.7-flash"
   }
   ```

   Or for `inherit` (use agent-specified models):

   ```json
   {
     "$schema": "https://opencode.ai/config.json",
     "model": "inherit"
   }
   ```

2. **No agent changes needed**: Agents already use correct GLM format

3. **Validate**: Verify configuration works by invoking sample agents

#### Target Configuration

**After migration, `.opencode/opencode.json`**:

```json
{
  "$schema": "https://opencode.ai/config.json",
  "model": "zai/glm-4.7",
  "small_model": "zai/glm-4.7-flash",
  "mcp": {
    "nx-mcp": {
      "type": "local",
      "command": ["npx", "-y", "nx-mcp"],
      "enabled": true
    }
  }
}
```

**Validation Criteria**:

- [ ] `.opencode/opencode.json` uses GLM model names (not `anthropic/claude-*`)
- [ ] Config model is either `zai/glm-4.7`, `zai/glm-4.7-flash`, or `inherit`
- [ ] No agents require model field changes (already correct)
- [ ] Sample agents invoke successfully with new config
- [ ] All 46 agents validate with OpenCode schema

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

````yaml
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

### Phase 2: Skills Frontmatter Cleanup

**Current Claude Code Skill Format** (example):

```yaml
---
name: docs-applying-content-quality
description: Universal markdown content quality standards for active voice, heading hierarchy, accessibility compliance...
model: sonnet
tags: [docs, quality, validation]
---
# Skill Content
Content of the skill...
````

**Target OpenCode Skill Format** (example):

```yaml
---
description: Universal markdown content quality standards for active voice, heading hierarchy, accessibility compliance...
---
# Skill Content
Content of the skill...
```

**Changes Required**:

- Remove `name` field (skill name inferred from directory name)
- Remove `model` field (skills don't specify models)
- Remove `tags` field (not used in OpenCode skills)
- Keep `description` field (same)
- No frontmatter changes required for skill content

---

## Validation Procedures

### Agent Validation

1. **Schema Validation**
   - Verify all agents have valid OpenCode frontmatter
   - Check required fields: `description`, `model`, `tools`
   - Validate model names are GLM models (not Claude aliases)
   - Validate `tools` object structure

2. **Functional Validation**
   - Test sample agents from each family
   - Verify agent responses are correct
   - Verify no functionality regressions
   - Check agent-tool integration

3. **Permission Validation**
   - Verify read-only agents have `write: false, edit: false, bash: false`
   - Verify full-access agents have appropriate permissions
   - Check for security vulnerabilities (unrestricted bash/edit)
   - Validate `permission.skill` format

### Skills Validation

1. **Location Validation**
   - Verify `.opencode/skill/<name>/SKILL.md` structure
   - Verify all 23 skills present
   - Verify no Claude Code skill artifacts remain

2. **Format Validation**
   - Verify skill frontmatter is OpenCode-compliant
   - Remove Claude Code-specific fields (name, model, tags)
   - Verify description field present

3. **Functional Validation**
   - Test skill loading with permission-based model
   - Verify agents can access permitted skills
   - Verify denied skills are inaccessible

### Documentation Validation

1. **Completeness Validation**
   - Verify AGENTS.md contains all agent guidance
   - Verify all references updated
   - Verify no CLAUDE.md references remain

2. **Accuracy Validation**
   - Verify all examples show OpenCode format
   - Verify all paths are correct
   - Verify all counts are accurate

### Repository Cleanup Validation

1. **File Deletion Validation**
   - Verify `.claude/agents/` deleted
   - Verify `.claude/skills/` deleted
   - Verify `.claude/settings.json` deleted
   - Verify CLAUDE.md deleted
   - Verify conversion scripts deleted
   - Verify `.claude/` directory deleted entirely

2. **Reference Validation**
   - Search for "claude code" references (only historical notes should remain)
   - Search for ".claude/agents/" references (should be zero)
   - Search for "CLAUDE.md" references (should be zero)

---

## Rollback Procedure

### Pre-Migration Archive

Before starting migration, create archive of current state:

```bash
# Create archive branch
git checkout -b archive/pre-migration-claude-code

# Commit current state
git add .
git commit -m "Archive: Pre-migration state before Claude Code to OpenCode migration"

# Push to remote (optional)
git push -u origin archive/pre-migration-claude-code

# Return to main branch
git checkout main
```

### Rollback Steps

If critical issues arise after migration:

```bash
# 1. Reset to pre-migration state
git reset --hard HEAD~1  # Revert migration commit

# 2. Or restore from archive branch
git checkout archive/pre-migration-claude-code -- .
git reset HEAD

# 3. Verify restored state
ls -la .claude/agents/
ls -la .claude/skills/
ls -la .opencode/agent/

# 4. Commit rollback
git add .
git commit -m "Rollback: Reverted Claude Code to OpenCode migration due to critical issues"
```

### Rollback Triggers

Rollback if any of the following occur:

- Critical agents fail to function
- Major functionality regressions detected
- Validation tests fail after multiple fix attempts
- Migration breaks existing workflows
- User request for rollback

---

## Migration Scripts

### Current Conversion Scripts

The repository contains dual-format conversion scripts that will be **DELETED** during migration:

- `scripts/convert-agents-to-opencode.py` (14,661 bytes)
- `scripts/validate-opencode-agents.py` (11,102 bytes)
- `scripts/sync-claude-opencode.py` (8,627 bytes)

### Script Deletion Rationale

These scripts are **NOT reused** because:

1. **Migration eliminates dual-format**: No conversion needed post-migration
2. **Scripts target Claude Code → OpenCode**: After migration, source (`.claude/agents/`) will be deleted
3. **Validation becomes simpler**: Only need to validate OpenCode format (not both formats)
4. **Cleanup commitment**: Dual-format architecture is being removed completely

### Useful Logic to Extract (Optional)

Before deletion, consider extracting useful functions:

- Agent schema validation logic (from `validate-opencode-agents.py`)
- Tool permission mapping logic (from `convert-agents-to-opencode.py`)
- Agent frontmatter parsing logic

**Decision**: Delete scripts in Task 6.4 (delivery.md) unless specific extraction is needed

---

## Implementation Notes

### Migration Execution Order

1. **Phase 1**: Preparation (audit, inventory, test setup)
2. **Phase 2**: Agent migration (schema, model, permissions, testing)
3. **Phase 3**: Skills migration (location, frontmatter, validation)
4. **Phase 4**: Governance updates (checker, fixer, maker agents)
5. **Phase 5**: Documentation consolidation (AGENTS.md expansion)
6. **Phase 6**: Cleanup (delete Claude Code artifacts)
7. **Phase 7**: Final validation (comprehensive testing, manual validation)

### Risk Mitigation

- **Validation after each phase**: Catch issues early
- **Comprehensive testing**: Test sample agents before proceeding
- **Archive creation**: Preserve pre-migration state
- **Rollback procedure**: Clear steps to revert if needed
- **Incremental changes**: Small, verifiable steps

### Success Criteria

- All 46 agents work correctly in OpenCode
- All 23 skills load correctly with permission model
- All governance agents updated
- AGENTS.md comprehensive
- Zero Claude Code artifacts remain
- All validation tests pass
- Migration commit created and documented

---

## References

- [OpenCode Agent Format](https://opencode.ai/docs/agents)
- [OpenCode Skills Documentation](https://opencode.ai/docs/skills)
- [AI Agents Convention](../../../governance/development/agents/ai-agents.md)
- [Maker-Checker-Fixer Pattern](../../../governance/development/pattern/maker-checker-fixer.md)
- [Plans Organization Convention](../../../governance/conventions/project/plans-organization.md)
- Current conversion: `scripts/convert-agents-to-opencode.py`
- Current validation: `scripts/validate-opencode-agents.py`
- Migration plan: `plans/backlog/2025-01-12__claude-code-full-migration/`
