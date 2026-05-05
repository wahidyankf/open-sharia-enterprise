# Requirements: Full Migration from Claude Code to OpenCode

## User Stories

### US-1: Agent Format Migration

**As a** repository maintainer
**I want** all AI agents defined in OpenCode format only
**So that** there's a single source of truth for agent definitions

**Acceptance Criteria**: See Scenario 1

---

### US-2: Documentation Consolidation

**As a** contributor
**I want** all AI agent guidance in AGENTS.md
**So that** I don't have to reference multiple documentation files

**Acceptance Criteria**: See Scenario 2

---

### US-3: Repository Cleanup

**As a** repository maintainer
**I want** all Claude Code-specific files and directories removed
**So that** the repository structure is clean and single-format

**Acceptance Criteria**: See Scenario 3

---

### US-4: Skills Permission-Based Loading

**As an** agent developer
**I want** skills to use OpenCode's permission-based model
**So that** skill access is explicit and secure

**Acceptance Criteria**: See Scenario 4

---

### US-5: Agent Creation Workflow Update

**As an** agent developer
**I want** agent-maker to create agents in OpenCode format
**So that** new agents follow the single-format standard

**Acceptance Criteria**: See Scenario 5

---

### US-6: Repository Governance Updates

**As a** governance checker/fixer
**I want** all validation/fix agents to work with OpenCode format
**So that** repository governance continues to function correctly

**Acceptance Criteria**: See Scenario 6

---

## Gherkin Scenarios

### Scenario 1: Agent Format Migration

**Given** the repository currently has 46 agents in `.claude/agents/` (Claude Code format)
**And** 46 agents in `.opencode/agent/` (OpenCode format, generated)
**When** the migration is executed
**Then** `.claude/agents/` directory is removed entirely
**And** `.opencode/agent/` contains all 46 agents as the source of truth
**And** each OpenCode agent has complete frontmatter (description, model, tools, permissions)
**And** each OpenCode agent has the same behavior as the previous Claude Code agent
**And** no agent functionality is lost or broken
**And** all 46 agents validate successfully with OpenCode schema

**Given** an OpenCode agent requires explicit tool permissions
**When** converting from Claude Code format
**Then** Claude Code's `tools: Read, Grep, Glob` becomes `tools: {read: true, grep: true, glob: true}`
**And** Claude Code's omitted `tools` field becomes `tools: {}` (inherits all)
**And** permissions section is added for bash/edit operations where needed

---

### Scenario 2: Documentation Consolidation

**Given** CLAUDE.md currently contains 348 lines of Claude Code-specific guidance
**And** AGENTS.md currently contains 232 lines of OpenCode-specific guidance
**When** documentation consolidation is executed
**Then** CLAUDE.md is deleted
**And** AGENTS.md is expanded to contain all relevant agent guidance
**And** AGENTS.md length is consolidated from CLAUDE.md content into existing 232 lines
**And** AGENTS.md covers: project overview, agent catalog, skills, maker-checker-fixer workflow, tool usage
**And** general project guidance is moved to existing governance docs (not duplicated)
**And** all references to CLAUDE.md in the repository are removed or updated
**And** AGENTS.md includes OpenCode-specific patterns (permissions, session management, multi-model)

**Given** the user needs to understand agent invocation
**When** reading AGENTS.md
**Then** all examples show OpenCode format invocation
**And** no examples show Claude Code format
**And** OpenCode-specific features are documented (permission.skill, tool access control)

---

### Scenario 3: Repository Cleanup

**Given** the repository currently contains Claude Code-specific artifacts
**And** `.claude/agents/` contains 45 agent files
**And** `.claude/settings.json` exists
**And** `scripts/convert-agents-to-opencode.py` exists
**And** `scripts/validate-opencode-agents.py` exists
**And** `scripts/sync-claude-opencode.py` exists
**And** CLAUDE.md exists
**When** repository cleanup is executed
**Then** `.claude/agents/` directory is removed
**And** `.claude/settings.json` is removed
**And** all conversion scripts are removed
**And** all validation scripts for dual-format are removed
**And** CLAUDE.md is removed
**And** `.claude/` contains only `skills/` directory (23 skills)
**And** no references to these removed files exist in code or documentation
**And** git history is preserved (migration commit references old paths)

**Given** a user searches for Claude Code references in the repository
**When** they run `rg "claude code" -i`
**Then** only historical migration notes appear
**And** no functional dependencies on Claude Code format remain
**And** no active code paths reference Claude Code directories

---

### Scenario 4: Skills Permission-Based Loading

**Given** 23 skills currently exist in `.claude/skills/`
**And** skills auto-load in Claude Code based on task description
**And** skills use permission-based loading in OpenCode via `permission.skill` frontmatter
**When** skills migration is executed
**Then** `.claude/skills/` directory is deleted (no Claude Code artifacts remain)
**And** all 23 skills are moved to `.opencode/skill/<name>/SKILL.md`
**And** all OpenCode agents that use skills have `permission.skill` frontmatter
**And** `permission.skill` format is:

```yaml
permission:
  skill:
    docs-applying-diataxis-framework: allow
    wow-applying-maker-checker-fixer: allow
```

**And** skills not listed in `permission.skill` are denied access
**And** all 23 skills load correctly when invoked in OpenCode
**And** skill loading behavior is tested with sample agents

**Given** an agent needs to use multiple skills
**When** the agent frontmatter is updated
**Then** all required skills are listed under `permission.skill`
**And** the format uses `skill-name: allow` syntax
**And** no other permission syntax is used for skills

---

### Scenario 5: Agent Creation Workflow Update

**Given** `agent-maker` currently creates agents in `.claude/agents/`
**And** agents are in Claude Code format
**When** agent-maker is updated
**Then** agent-maker creates agents in `.opencode/agent/`
**And** agents are in OpenCode format
**And** agent frontmatter includes OpenCode-specific fields (description, model, tools, permissions)
**And** agent body contains agent-specific logic and instructions
**And** agent-maker validates OpenCode format before writing
**And** agent-maker updates `.opencode/agent/README.md` with new agent entry

**Given** a developer wants to create a new agent
**When** they invoke agent-maker with agent name and description
**Then** the agent is created at `.opencode/agent/<agent-name>.md`
**And** the agent has correct OpenCode frontmatter
**And** the agent is immediately usable by OpenCode CLI
**And** the agent entry appears in `.opencode/agent/README.md`

---

### Scenario 6: Repository Governance Updates

**Given** `repo-governance-checker` currently validates `.claude/agents/` format
**And** `repo-governance-fixer` modifies both `.claude/agents/` and `.opencode/agent/`
**And** other checker/fixer agents reference dual-format paths
**When** governance agents are updated
**Then** `repo-governance-checker` validates `.opencode/agent/` format only
**And** `repo-governance-fixer` modifies `.opencode/agent/` only
**And** all agent paths in governance agents are updated to `.opencode/agent/`
**And** all references to dual-format support are removed
**And** validation checks ensure Claude Code artifacts don't reappear

**Given** repo-governance-checker runs validation
**When** it scans the repository
**Then** it validates OpenCode agent frontmatter format
**And** it checks for proper tool permissions
**And** it validates `permission.skill` format
**And** it ensures no `.claude/agents/` directory exists
**And** it ensures no CLAUDE.md file exists
**And** it reports any Claude Code artifacts as errors

**Given** repo-governance-fixer applies fixes
**When** it modifies agents
**Then** it updates `.opencode/agent/` files directly
**And** it does not attempt to sync to Claude Code format
**And** it uses Bash tools for `.opencode/` writes (if applicable)
**And** it validates fixes using OpenCode schema

---

## Non-Functional Requirements

### NFR-1: Zero Functionality Loss

**Requirement**: All existing agent functionality must be preserved

**Validation**:

- All 46 agents work identically before and after migration
- All maker-checker-fixer workflows function correctly
- All 23 skills load and execute correctly
- No user-facing behavior changes

---

### NFR-2: Documentation Completeness

**Requirement**: AGENTS.md must contain all necessary agent guidance

**Validation**:

- New contributors can work with agents using only AGENTS.md
- All agent patterns documented (creation, invocation, validation)
- All governance patterns documented (checking, fixing, workflows)
- No references to CLAUDE.md for agent guidance

---

### NFR-3: Backward Compatibility (Git)

**Requirement**: Git history must be preserved during migration

**Validation**:

- Migration commit message references old paths (`.claude/agents/`)
- Git metadata (created/updated timestamps) extracted and preserved
- Historical agent evolution can be traced via git blame on old paths
- Archive branch contains pre-migration state if needed

---

### NFR-4: Validation Coverage

**Requirement**: Comprehensive validation must ensure migration success

**Validation**:

- All OpenCode agents validate with schema
- All skills load with permission model
- All governance agents work with OpenCode format
- All documentation references are updated
- No Claude Code artifacts remain

---

### NFR-5: Rollback Capability

**Requirement**: Migration must be reversible in case of critical issues

**Validation**:

- Git revert can restore pre-migration state
- Rollback procedure documented in delivery checklist
- Pre-migration state archived in separate branch
- Validation fails before commit if critical issues detected

---

## Traceability Matrix

| User Story | Acceptance Criteria | Test Scenario               | Related Docs |
| ---------- | ------------------- | --------------------------- | ------------ |
| US-1       | Scenario 1          | Agent format conversion     | tech-docs.md |
| US-2       | Scenario 2          | Documentation consolidation | tech-docs.md |
| US-3       | Scenario 3          | Repository cleanup          | delivery.md  |
| US-4       | Scenario 4          | Skills permission model     | tech-docs.md |
| US-5       | Scenario 5          | Agent creation workflow     | delivery.md  |
| US-6       | Scenario 6          | Governance agents update    | delivery.md  |

---

## Dependencies

### External Dependencies

- OpenCode CLI version compatibility
- OpenCode agent schema stability
- Claude Code migration guides accuracy

### Internal Dependencies

- Completion of "agents-docs-source-of-truth" plan (if executed) would conflict with this plan
- Current `.opencode/agent/` generation scripts must be stable for initial validation
- All 23 skills must work with OpenCode permission model

### Blocking Dependencies

None - this plan can proceed independently

---

## Out of Scope

The following are explicitly out of scope for this migration:

1. **Skills format changes**: Skills moved to `.opencode/skill/<name>/SKILL.md` (OpenCode standard)
2. **Skill content migration**: No changes to skill definitions themselves (frontmatter adjusted only)
3. **OpenCode feature adoption**: Only migrating existing functionality, not adding new OpenCode-specific features
4. **Performance optimization**: Migration focused on correctness, not performance
5. **New agent creation**: Only migrating existing 46 agents
6. **Workflow re-architecture**: Maker-checker-fixer patterns preserved, only format changes
7. **Model configuration changes**: Claude Code model aliases replaced with GLM model names (sonnet→zai/glm-4.7, haiku→zai/glm-4.7-flash, opus→zai/glm-4.7-plus)
8. **MCP server configuration**: MCP tools configuration unchanged

---

## Acceptance Checklist

Complete all items to consider this migration successful:

### Functionality

- [ ] All 46 agents work correctly in OpenCode
- [ ] All 23 skills load with permission model
- [ ] All maker agents create OpenCode format agents
- [ ] All checker agents validate OpenCode format
- [ ] All fixer agents modify OpenCode format
- [ ] All workflow agents orchestrate OpenCode format agents

### Cleanup

- [ ] `.claude/agents/` removed (45 files)
- [ ] `.claude/settings.json` removed
- [ ] CLAUDE.md removed
- [ ] All conversion scripts removed
- [ ] All validation scripts for dual-format removed
- [ ] No Claude Code references remain in code/docs

### Documentation

- [ ] AGENTS.md comprehensive (5k-10k lines)
- [ ] All agent guidance in AGENTS.md
- [ ] All examples show OpenCode format
- [ ] All references updated to `.opencode/agent/`
- [ ] Contribution workflow documented

### Validation

- [ ] All OpenCode agents validate with schema
- [ ] All governance agents work with OpenCode format
- [ ] No Claude Code artifacts detected
- [ ] Rollback procedure tested
- [ ] Migration commit documented with old paths

---

## References

- [AI Agents Convention](../../../../governance/development/agents/ai-agents.md)
- [OpenCode Agent Format](https://opencode.ai/docs/agents)
- [Maker-Checker-Fixer Pattern](../../../governance/development/pattern/maker-checker-fixer.md)
- [Plans Organization Convention](../../../../governance/conventions/structure/plans.md)
- Current conversion: `scripts/convert-agents-to-opencode.py`
- Current validation: `scripts/validate-opencode-agents.py`
