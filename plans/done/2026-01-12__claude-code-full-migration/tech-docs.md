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

### Model Configuration Verification (UPDATED)

**Status**: ✅ Verified - All 46 agents already use GLM model names

**Audit Results**:

- 38 agents use `zai/glm-4.7` (advanced reasoning, deep analysis)
- 6 agents use `zai/glm-4.5-air` (fast, lightweight tasks)
- 0 agents use Claude Code aliases (sonnet, haiku, opus)
- 0 agents use anthropic/claude-\* model names

**Model Name Reference**:

| GLM Model Name    | Use Case                          | Agent Count |
| ----------------- | --------------------------------- | ----------- |
| `zai/glm-4.7`     | Advanced reasoning, deep analysis | 38          |
| `zai/glm-4.5-air` | Fast, lightweight tasks           | 6           |
| `inherit`         | Use main conversation model       | 0           |

#### Verification Strategy

**Phase 2: Model Configuration Verification**

1. **Verify agent models**:

   ```bash
   grep -r "^model:" .opencode/agent/*.md | sort | uniq -c
   ```

2. **Verify no Claude aliases**:

   ```bash
   grep -r "^model: sonnet\|^model: haiku\|^model: opus" .opencode/agent/*.md
   # Should return empty (no matches)
   ```

3. **Verify no anthropic/claude-\* models**:

   ```bash
   grep -r "^model: anthropic/claude" .opencode/agent/*.md
   # Should return empty (no matches)
   ```

4. **Document model distribution**: Create report showing which agents use which models

**Note**: No agent model field changes required - agents already use GLM models.

#### Agent Model Configuration

**Current State**: All 46 agents already use GLM model format (verified via audit):

```bash
$ grep -r "^model:" .opencode/agent/*.md | sort | uniq -c
    38 zai/glm-4.7
     6 zai/glm-4.5-air
```

**Example from `.opencode/agent/docs-checker.md`**:

```yaml
---
description: Validates documentation for quality and consistency
model: zai/glm-4.7  ✅ Correct GLM format
tools:
  read: true
  grep: true
  glob: true
  write: false
  edit: false
  bash: false
---
```

✅ All 46 agents use GLM model names (no Claude Code aliases found)
✅ No agent model changes required (already correct)

#### Model Name Reference

| GLM Model Name      | Use Case                                        |
| ------------------- | ----------------------------------------------- |
| `zai/glm-4.7`       | Advanced reasoning, deep analysis (most agents) |
| `zai/glm-4.7-flash` | Fast, simple tasks, title generation            |
| `zai/glm-4.7-plus`  | Advanced reasoning, complex orchestration       |
| `inherit`           | Use main conversation model                     |

#### Configuration Strategy

**Phase 2: Configuration Verification**

1. **Verify all agent models use GLM names**:

   ```bash
   grep -r "^model:" .opencode/agent/*.md | sort | uniq -c
   ```

   Expected: 38x `zai/glm-4.7`, 6x `zai/glm-4.5-air`

2. **Verify no Claude Code aliases**:

   ```bash
   grep -r "^model: sonnet\|^model: haiku\|^model: opus" .opencode/agent/*.md
   ```

   Expected: No matches

3. **Document findings**: Create report in `generated-reports/model-configuration.md`

**No configuration changes required** - agents already use GLM models.

**Validation Criteria**:

- [ ] All 46 agents use GLM model names (zai/glm-4.7, zai/glm-4.5-air, or inherit)
- [ ] 0 agents use Claude Code model aliases (sonnet, haiku, opus)
- [ ] 0 agents use anthropic/claude-\* model names
- [ ] Model configuration report created
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

## Governance Agents Migration

**Agents Requiring Updates**:

1. **repo-governance-checker**:
   - Validates `.claude/agents/` format → Validate `.opencode/agent/` format only
   - Validates dual-format paths → Validate single-format paths only
   - Remove Claude Code-specific validation checks

2. **repo-governance-fixer**:
   - Modifies both `.claude/agents/` and `.opencode/agent/` → Modify `.opencode/agent/` only
   - Syncs formats → No sync needed (single format)
   - Update path references in code

3. **agent-maker**:
   - Creates agents in `.claude/agents/` → Create agents in `.opencode/agent/`
   - Uses Claude Code frontmatter → Use OpenCode frontmatter
   - Updates Claude Code README → Update OpenCode README

4. **All other agents with path references**:
   - Update any references to `.claude/agents/` → `.opencode/agent/`
   - Update any references to CLAUDE.md → AGENTS.md
   - Update any Claude Code format examples → OpenCode format examples

### Path Reference Changes

**Before**:

```yaml
# In agent body
Read .claude/agents/docs-checker.md
Validate format against governance/development/agents/ai-agents.md
```

**After**:

```yaml
# In agent body
Read .opencode/agent/docs-checker.md
Validate format against governance/development/agents/ai-agents.md (updated to OpenCode format)
```

### Dual-Format Support Removal

**Before** (in repo-governance-checker):

```python
# Validate Claude Code format
validate_claude_agent(agent_path)

# Validate OpenCode format
validate_opencode_agent(agent_path)

# Check for dual-format consistency
check_sync_state()
```

**After** (in repo-governance-checker):

```python
# Validate OpenCode format only
validate_opencode_agent(agent_path)

# Ensure no Claude Code artifacts remain
check_no_claude_code_artifacts()

# Validate OpenCode schema compliance
validate_opencode_schema()
```

### Example: repo-governance-checker Updates

**Tasks**:

1. Remove all `.claude/agents/` validation logic
2. Remove dual-format consistency checks
3. Add OpenCode schema validation
4. Add Claude Code artifact detection (fail if found)
5. Update agent README update logic

**Validation Criteria**:

- repo-governance-checker validates `.opencode/agent/` format only
- repo-governance-fixer modifies `.opencode/agent/` files only
- agent-maker creates agents in `.opencode/agent/`
- All path references updated to `.opencode/agent/`
- All examples show OpenCode format

---

## Documentation Consolidation

### Content Mapping Strategy

**CLAUDE.md → AGENTS.md Content Mapping**:

| CLAUDE.md Section   | Destination         | Action                                     |
| ------------------- | ------------------- | ------------------------------------------ |
| Project Overview    | governance docs     | Move to existing governance overview       |
| Agent Format        | AGENTS.md (expand)  | Update to OpenCode format examples         |
| Agent Invocation    | AGENTS.md (expand)  | Update to OpenCode invocation patterns     |
| Agent Tools         | AGENTS.md (expand)  | Update to OpenCode permission model        |
| Maker-Checker-Fixer | AGENTS.md (expand)  | Keep (already dual-format compatible)      |
| Skills              | AGENTS.md (expand)  | Update to OpenCode permission.skill model  |
| Model Configuration | AGENTS.md (expand)  | Update to GLM models (no migration)        |
| Session Management  | AGENTS.md (add new) | Add OpenCode-specific session management   |
| Multi-Model Usage   | AGENTS.md (add new) | Add OpenCode-specific multi-model patterns |

### Final AGENTS.md Structure

```markdown
# AI Agents Documentation

## Project Overview

[OpenCode-only project overview]

## Agent Catalog

[All 46 agents, organized by family]

## Agent Format

[OpenCode frontmatter structure, examples]

## Agent Invocation

[OpenCode invocation patterns, session management]

## Agent Tools & Permissions

[OpenCode tool access control model]

## Skills

[23 skills, permission-based loading model]

## Maker-Checker-Fixer Workflow

[OpenCode-specific workflow patterns]

## Multi-Model Usage

[OpenCode multi-model patterns]

## Agent Creation Workflow

[How to create new OpenCode agents]

## Agent Validation & Fixing

[How to validate and fix OpenCode agents]
```

### Duplicate Content Handling

**Remove from CLAUDE.md** (move to governance docs):

- Project principles → `governance/principles/`
- Development practices → `governance/development/`
- Repository architecture → `governance/explanation/`

**Merge into AGENTS.md**:

- Agent format (update to OpenCode)
- Agent invocation (update to OpenCode)
- Skills (update to permission.skill model)

**Add to AGENTS.md** (missing content):

- Session management in OpenCode
- Multi-model usage in OpenCode

### Consolidation Steps

1. **Read CLAUDE.md**: Extract all content by section
2. **Classify sections**: Agent-specific vs general guidance vs duplicate
3. **Move general content**: Add to appropriate governance docs
4. **Update agent-specific content**: Convert to OpenCode format
5. **Add missing content**: Write OpenCode-specific sections
6. **Validate AGENTS.md**: Verify all agent guidance present
7. **Delete CLAUDE.md**: Remove original file

**Validation Criteria**:

- AGENTS.md contains all agent guidance from CLAUDE.md
- AGENTS.md uses OpenCode format examples only
- All CLAUDE.md references removed from repository
- General project guidance exists in governance docs
- No duplicate content between AGENTS.md and governance docs

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
- [Plans Organization Convention](../../../governance/conventions/structure/plans.md)
- Current conversion: `scripts/convert-agents-to-opencode.py`
- Current validation: `scripts/validate-opencode-agents.py`
- Migration plan: `plans/backlog/2025-01-12__claude-code-full-migration/`
