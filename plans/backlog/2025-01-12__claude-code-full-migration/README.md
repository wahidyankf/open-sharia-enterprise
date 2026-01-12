# Full Migration from Claude Code to OpenCode

**Status**: Backlog
**Created**: 2025-01-12
**Priority**: High
**Complexity**: High (architectural change eliminating Claude Code dependency)

## Problem Statement

Currently, this repository maintains dual AI agent support:

- **Claude Code format**: `.claude/agents/` (45 agents) + `.claude/skills/` (23 skills) - **Source of truth**
- **OpenCode format**: `.opencode/agent/` (45 agents) - Generated from Claude Code
- **Documentation**: `CLAUDE.md` (~30,000 lines) - Claude Code-specific guidance
- **Instructions**: `AGENTS.md` (~1,000 lines) - OpenCode-specific guidance

**Key Pain Points**:

1. **Dual maintenance burden**: Primary agent definitions tied to Claude Code format
2. **Documentation duplication**: CLAUDE.md and AGENTS.md cover similar content
3. **Conversion complexity**: Scripts required to sync formats
4. **Vendor lock-in**: Core definitions coupled to Claude Code format
5. **Future tool adoption**: Adding third AI CLI tool requires new conversion logic
6. **Skills management**: `.claude/skills/` shared but loaded differently by each tool
7. **Agent invocation differences**: Skills auto-load in Claude Code vs permission-based in OpenCode
8. **Model alias issue (CRITICAL)**: All 45 agents use Claude Code model aliases (`sonnet`, `haiku`, `opus`) which are NOT valid in OpenCode. OpenCode requires GLM model names (`zai/glm-4.7`, `zai/glm-4.7-flash`, `zai/glm-4.7-plus`, or `inherit`)
9. **Governance docs outdated**: Multiple governance documents reference Claude Code format and paths, requiring comprehensive updates to OpenCode format

## Proposed Solution

**Fully migrate to OpenCode as single source of truth**, eliminating all Claude Code-specific directories and files.

### Current Architecture

```
Source: .claude/agents/ (Claude Code format, 45 agents)
   ↓ (convert-agents-to-opencode.py)
Generated: .opencode/agent/ (OpenCode format, 45 agents)

Shared: .claude/skills/ (23 skills, auto-loaded by Claude Code)
       .claude/settings.json (Claude Code settings)
       .claude/settings.local.json (local Claude Code settings)

Docs: CLAUDE.md (~30k lines, Claude Code guidance)
        AGENTS.md (~1k lines, OpenCode guidance)
        governance/development/agents/ai-agents.md (Claude Code convention)
```

### Proposed Architecture

```
Source: .opencode/agent/ (OpenCode format, 45 agents - single source)

Shared: .opencode/skill/<name>/SKILL.md (23 skills, OpenCode standard)

Docs: AGENTS.md (expanded with CLAUDE.md content, OpenCode-only)
       governance/development/agents/ai-agents.md (updated to OpenCode format)
       All governance/ docs (updated to reference .opencode/agent/)

Config: .opencode/opencode.json (OpenCode settings, replaces Claude Code settings)

 Eliminated: .claude/agents/ (45 files)
               .claude/skills/ (23 skills)
               .claude/settings.json
               .claude/settings.local.json
               CLAUDE.md
               scripts/convert-agents-to-opencode.py
               scripts/validate-opencode-agents.py
               scripts/sync-claude-opencode.py
               .claude/ (entire directory)
```

### Model Migration Strategy (CRITICAL CHANGE)

**Issue**: All 45 agents currently use Claude Code model aliases (`sonnet`, `haiku`, `opus`). User wants GLM models instead of Claude models.

**Model Mapping**:

| Claude Code Alias | OpenCode GLM Model  | Use Case                                  |
| ----------------- | ------------------- | ----------------------------------------- |
| `sonnet`          | `zai/glm-4.7`       | Advanced reasoning, deep analysis         |
| `haiku`           | `zai/glm-4.7-flash` | Fast, simple tasks, title generation      |
| `opus`            | `zai/glm-4.7-plus`  | Advanced reasoning, complex orchestration |
| `inherit`         | `inherit`           | Use main conversation model               |

**Rationale**: GLM models provide equivalent capabilities to Claude models while supporting the user's preference for non-Anthropic providers.

```

## Goals

### Primary Goals

1. **Single format**: OpenCode agent format as source of truth
2. **Eliminate Claude Code artifacts**: Remove `.claude/agents/`, CLAUDE.md, conversion scripts
3. **Simplify maintenance**: No format conversion or synchronization
4. **OpenCode-only workflow**: All AI tooling uses OpenCode exclusively
5. **Model alias migration**: Replace `sonnet`/`opus`/`haiku` with GLM model names (e.g., `zai/glm-4.7`)
6. **Governance integration**: Update all governance/docs to reference OpenCode format only

### Secondary Goals

1. **Consolidated documentation**: AGENTS.md contains all AI agent guidance
2. **Permission-based skills**: Migrate to OpenCode's `permission.skill` model (delete `.claude/skills/`)
3. **Clean repository structure**: Remove dual-format artifacts
4. **Future-proof**: Easy to adopt new AI CLI tools (no Claude Code legacy)
5. **Governance consistency**: All governance docs reference `.opencode/agent/`, AGENTS.md, not Claude Code artifacts

## Benefits

### Simplification

- **One format**: No conversion scripts or sync logic
- **One location**: Edit agents in `.opencode/agent/` directly
- **One doc**: AGENTS.md contains all agent guidance

### Maintainability

- **Single source of truth**: No dual maintenance
- **Faster iteration**: Edit once, no conversion step
- **Clear workflow**: OpenCode only, no format decisions

### Documentation Quality

- **Comprehensive**: AGENTS.md combines best of both CLAUDE.md and AGENTS.md
- **Targeted**: All content specific to OpenCode format
- **Concise**: No redundant "both formats supported" sections

### Future Flexibility

- **New tools**: Adopt new AI CLI without Claude Code legacy
- **Easier onboarding**: One format to learn
- **Clean migration path**: No multi-format complexity

## Trade-offs and Risks

### Loss of Claude Code Compatibility

**Risk**: Can no longer use Claude Code AI CLI tool

**Mitigation**:

- OpenCode is open source, actively maintained (650K monthly users)
- OpenCode supports same Claude models via API
- Repository fully committed to OpenCode architecture
- Migration decision documented in CLAUDE.md deprecation notice

### Documentation Consolidation Effort

**Risk**: Merging 30k lines of CLAUDE.md into 1k lines of AGENTS.md is complex

**Mitigation**:

- Focus on agent-specific guidance (not general project info)
- Extract project guidance to `CLAUDE.md` → governance docs
- Prioritize OpenCode-specific patterns
- Use links to existing governance docs instead of duplicating

### Skills Migration Complexity

**Risk**: Converting 23 skills from auto-load to permission-based model

**Mitigation**:

- OpenCode's `permission.skill` provides explicit control
- Move skills to `.opencode/skill/<name>/SKILL.md` (OpenCode standard)
- Delete `.claude/skills/` entirely (no Claude Code artifacts remain)
- Validate skill loading with test agents before full migration

### Git History Loss

**Risk**: Deleting `.claude/agents/` loses git blame for agent evolution

**Mitigation**:

- Migrate key git metadata (created/updated timestamps) to OpenCode frontmatter
- Document migration in commit message with references to old paths
- Archive `.claude/agents/` in separate branch if historical analysis needed
- Current `.opencode/agent/` already has history from conversion scripts

### Breaking Changes for Contributors

**Risk**: Contributors familiar with Claude Code format must relearn

**Mitigation**:

- OpenCode format is more explicit (permission-based tools)
- Comprehensive documentation in AGENTS.md
- Migration guide in delivery checklist
- Examples and patterns in governance/docs

## Success Criteria

### Agent Functionality

- [ ] All 45 agents work identically in OpenCode after migration
- [ ] All 23 skills load correctly with permission-based model
- [ ] 0 validation errors in OpenCode agents
- [ ] 0 functionality regressions vs current agent behavior
- [ ] All agent invocations use OpenCode format successfully

### Repository Cleanup

- [ ] `.claude/agents/` directory removed (45 agent files)
- [ ] `.claude/skills/` directory removed (23 skills)
- [ ] `.claude/settings.json` removed
- [ ] `.claude/settings.local.json` removed
- [ ] CLAUDE.md removed
- [ ] All conversion scripts removed (`scripts/convert-*.py`, `scripts/validate-*.py`, `scripts/sync-*.py`)
- [ ] `.claude/` directory removed entirely (no Claude Code artifacts remain)

### Documentation Quality

- [ ] AGENTS.md is comprehensive (5,000-10,000 lines, not 30k)
- [ ] AGENTS.md contains all AI agent guidance from CLAUDE.md
- [ ] All references to `.claude/agents/` updated to `.opencode/agent/`
- [ ] All references to dual-format support removed
- [ ] Contribution workflow documented (how to add/edit OpenCode agents)

### Skills Management

- [ ] All 23 skills load correctly in OpenCode
- [ ] Skills use `permission.skill` model in OpenCode agent frontmatter
- [ ] Skills exist at `.opencode/skill/<name>/SKILL.md` only
- [ ] `.claude/skills/` deleted (no Claude Code artifacts remain)
- [ ] Documentation updated to reflect permission-based loading

### Agent Creation/Updates

- [ ] `agent-maker` creates agents in `.opencode/agent/` (not `.claude/agents/`)
- [ ] `repo-governance-checker` validates `.opencode/agent/` format
- [ ] `repo-governance-fixer` modifies `.opencode/agent/` directly
- [ ] All other agents updated to use OpenCode paths

### Workflow Consistency

- [ ] All workflows use OpenCode agent format
- [ ] All documentation examples show OpenCode format
- [ ] All instructions reference OpenCode only
- [ ] No Claude Code-specific terminology remains

## Plan Structure

This plan is organized into four documents:

1. **README.md** (this file): Overview, problem, solution, risks, success criteria
2. **[requirements.md](./requirements.md)**: User stories, acceptance criteria (Gherkin format)
3. **[tech-docs.md](./tech-docs.md)**: Architecture, design decisions, implementation details
4. **[delivery.md](./delivery.md)**: Phase-by-phase execution plan, migration checklist

## Alternatives Considered

### Alternative 1: Keep Dual-Format (Status Quo)

**Approach**: `.claude/agents/` remains source, continue generating OpenCode

**Pros**:

- Already implemented and working
- Familiar to current contributors
- Lower complexity (2 locations vs 1)

**Cons**:

- Dual maintenance burden
- Conversion scripts to maintain
- CLAUDE.md/AGENTS.md duplication
- Future tools require new conversion logic

**Decision**: Rejected - doesn't solve maintainability problem

### Alternative 2: Tool-Agnostic Docs as Source

**Approach**: Move agents to `governance/agents/`, sync to both formats

**Pros**:

- Future-proof for new tools
- Documentation co-location

**Cons**:

- Adds abstraction layer (3 locations total)
- Sync script as critical path
- More complex than direct migration
- Over-engineering for single-tool future

**Decision**: Rejected - existing "agents-docs-source-of-truth" plan covers this approach

### Alternative 3: Switch to Claude Code as Single Source

**Approach**: Keep `.claude/agents/`, eliminate OpenCode

**Pros**:

- Already source of truth
- No migration needed for current agents

**Cons**:

- Proprietary (less community support)
- Vendor lock-in
- Less open development
- Limited to Anthropic models

**Decision**: Rejected - OpenCode offers better long-term flexibility

### Alternative 4: Selected - OpenCode as Single Source

**Approach**: Make `.opencode/agent/` source, eliminate Claude Code

**Pros**:

- Single source of truth
- Open source (650K monthly users)
- Multi-model support (Claude, GPT, Gemini, local models)
- Permission-based security model
- No conversion scripts
- Simplified documentation
- Future-proof for new tools

**Cons**:

- One-time migration effort
- Loss of Claude Code compatibility

**Decision**: **SELECTED** - best balance of simplicity, maintainability, and future flexibility

## Next Steps

1. **Review requirements.md**: Validate user stories and acceptance criteria
2. **Review technical.md**: Confirm architecture and design decisions
3. **Review delivery.md**: Approve execution plan
4. **Execute migration**: Follow delivery checklist phase-by-phase
5. **Validate**: Run comprehensive tests after each phase
6. **Commit**: Remove Claude Code artifacts once validation complete
7. **Update docs**: Ensure all references point to OpenCode

## References

- [OpenCode Official Site](https://opencode.ai)
- [OpenCode vs Claude Code Comparison](https://www.nxcode.io/resources/news/opencode-vs-claude-code-vs-cursor-2026)
- [Claude Code to OpenCode Migration Guide](https://medium.com/spillwave-solutions/claude-code-agents-to-opencode-agents-041f9c8e5ccd)
- [Converting Claude Subagents to OpenCode](https://claude-plugins.dev/skills/@edheltzel/dotfiles/converting-claude-subagents)
- [AI Agents Convention](../../../governance/development/agents/ai-agents.md)
- [Repository Governance Architecture](../../../governance/ex-ru__repository-governance-architecture.md)
- [Plans Organization Convention](../../../governance/conventions/project/plans-organization.md)
- Current conversion scripts: `scripts/convert-agents-to-opencode.py`, `scripts/validate-opencode-agents.py`, `scripts/sync-claude-opencode.py`
- Existing dual-format plan: `plans/backlog/2026-01-04__agents-docs-source-of-truth/` (alternative approach rejected)

```

```

```
