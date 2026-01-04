# Agent and Skill Definitions as Documentation Source of Truth

**Status**: Backlog
**Created**: 2026-01-04
**Priority**: High
**Complexity**: High (architectural change affecting 45 agents + 18 skills)

## Problem Statement

Currently, AI agent and skill definitions exist in tool-specific formats:

- **Agents**: `.claude/agents/` (Claude Code format) is source of truth, `.opencode/agent/` is generated
- **Skills**: `.claude/skills/` contains skill definitions shared by both tools
- **Coupling**: Agent definitions coupled to Claude Code format, requiring conversion for OpenCode

**Key Pain Points**:

1. **Tool lock-in**: Primary definitions tied to Claude Code format
2. **Dual maintenance burden**: Changes require thinking in Claude Code format first, then generating OpenCode
3. **Format drift risk**: As tools evolve, maintaining format compatibility becomes harder
4. **No canonical documentation**: Agent catalog exists in `.claude/agents/README.md`, not in `docs/`
5. **Future tool adoption**: Adding third tool format requires new conversion logic

## Proposed Solution

**Move agent and skill definitions to `docs/explanation/` as tool-agnostic source of truth**, then sync to both tool-specific formats.

### Current Architecture

```
Source: .claude/agents/ (Claude Code format)
   ↓ (one-way sync via convert-agents-to-opencode.py)
Generated: .opencode/agent/ (OpenCode format)

Shared: .claude/skills/ (used by both tools)
```

### Proposed Architecture

```
Source: docs/explanation/agents/ (tool-agnostic markdown)
   ↓ (sync via sync-docs-to-agents.py)
   ├→ .claude/agents/ (Claude Code format)
   └→ .opencode/agent/ (OpenCode format)

Source: docs/explanation/skills/ (tool-agnostic markdown)
   ↓ (sync via sync-docs-to-agents.py)
   └→ .claude/skills/ (used by both tools)
```

## Goals

### Primary Goals

1. **Tool-agnostic source of truth**: Agent definitions not coupled to any CLI tool format
2. **Documentation co-location**: Agents and skills live in `docs/` alongside conventions they enforce
3. **Single edit point**: Update one file in docs, sync to both tool formats automatically
4. **Future-proof**: Easy to add new tool formats without modifying source definitions

### Secondary Goals

1. **Improved traceability**: Clear path from Layer 4 (agents) documentation to implementation
2. **Reduced coupling**: Decouple agent logic from tool-specific frontmatter schemas
3. **Safer migrations**: Tool format changes only affect sync script, not source definitions

## Benefits

### Tool Agnosticism

- **Format independence**: Source definitions don't depend on Claude Code or OpenCode schemas
- **Multiple targets**: One definition syncs to N tool formats (currently 2, future: more)
- **Schema evolution**: Tool format changes isolated to sync script

### Documentation Quality

- **Co-location**: Agents live in `docs/explanation/agents/` alongside conventions they enforce
- **Canonical catalog**: `docs/explanation/agents/README.md` becomes authoritative agent index
- **Traceability**: Direct link from Layer 4 governance to implementation

### Maintainability

- **Single source**: Edit once, sync everywhere
- **Separation of concerns**: Definition (what) separate from implementation (how)
- **Validation**: One validation pass on source, multiple format validations on output

## Trade-offs and Risks

### Added Complexity

**Trade-off**: Introduces abstraction layer between definitions and implementations

- **Current**: 2 locations (`.claude/agents/`, `.opencode/agent/`)
- **Proposed**: 3 locations (`docs/explanation/agents/`, `.claude/agents/`, `.opencode/agent/`)

**Mitigation**: Sync script is authoritative - never edit tool-specific directories directly

### Sync Script as Critical Path

**Risk**: If sync script breaks, both tool formats break

**Mitigation**:

- Comprehensive validation after sync (both formats)
- Rollback plan (keep git history)
- Pre-commit hook prevents direct edits to generated directories

### Migration Effort

**Risk**: Extracting 45 agents + 18 skills is substantial one-time effort

**Mitigation**:

- Automated extraction via `repo-cli` Go application (`apps/repo-cli`)
- Phased migration with validation at each step
- Full test suite before committing changes

### Git History Preservation

**Risk**: Moving files loses git blame history for agent evolution

**Mitigation**:

- Use git metadata (first/last commit) to populate `created`/`updated` timestamps
- Document migration in commit message with references to old paths
- Consider keeping `.claude/agents/` as git history reference (archived)

### Editing Workflow Change

**Risk**: Contributors might edit `.claude/agents/` directly (muscle memory)

**Mitigation**:

- Banner in `.claude/agents/README.md`: "⚠️ DO NOT EDIT - Generated from docs/"
- Pre-commit hook warns if tool-specific directories modified
- Documentation updates (CLAUDE.md, AGENTS.md, contribution guides)

## Success Criteria

### Functional Equivalence

- [ ] All 45 agents work identically in Claude Code after migration
- [ ] All 45 agents work identically in OpenCode after migration
- [ ] All 18 skills load correctly from `.claude/skills/` after sync
- [ ] 0 validation errors in both formats
- [ ] 0 functionality regressions vs current agent behavior

### Agent Updates

- [ ] `agent-maker` creates agents in `docs/explanation/agents/content/` (not `.claude/agents/`)
- [ ] `agent-maker` uses tool-agnostic format (role instead of color)
- [ ] `wow-rules-checker` validates source (`docs/explanation/agents/`, `docs/explanation/skills/`)
- [ ] `wow-rules-fixer` does NOT modify generated directories (`.claude/agents/`, `.opencode/agent/`, `.claude/skills/`)
- [ ] Any other agents that reference agent/skill locations updated

### Documentation Quality

- [ ] `docs/explanation/agents/README.md` is comprehensive agent catalog
- [ ] `docs/explanation/skills/README.md` is comprehensive skills catalog
- [ ] Architecture documentation updated (repository governance, AI agents convention)
- [ ] CLAUDE.md and AGENTS.md reflect new source locations

### Operational Safety

- [ ] Sync script validates output before writing
- [ ] Pre-commit hook prevents direct edits to generated directories
- [ ] Rollback procedure documented and tested
- [ ] Contribution workflow documented (how to add new agent/skill)

### Traceability

- [ ] Clear mapping: docs definition → Claude Code format → OpenCode format
- [ ] Git history preserved via metadata extraction
- [ ] Migration commit references old locations for history lookup

## Plan Structure

This plan is organized into four documents:

1. **README.md** (this file): Overview, problem, solution, risks, success criteria
2. **[requirements.md](./requirements.md)**: User stories, acceptance criteria (Gherkin format)
3. **[tech-docs.md](./tech-docs.md)**: Architecture, design decisions, implementation details
4. **[delivery.md](./delivery.md)**: Phase-by-phase execution plan, migration checklist

## Alternatives Considered

### Alternative 1: Keep Current Dual-Format (Status Quo)

**Approach**: `.claude/agents/` remains source, conversion script to OpenCode

**Pros**:

- Already implemented and working
- Familiar to current contributors
- Lower complexity (2 locations vs 3)

**Cons**:

- Tool lock-in to Claude Code format
- Future tools require new conversion logic
- Agent definitions not in documentation directory

**Decision**: Rejected - doesn't solve tool lock-in problem

### Alternative 2: OpenCode as Source of Truth

**Approach**: Flip current architecture - `.opencode/agent/` is source, generate `.claude/agents/`

**Pros**:

- Still dual-format
- OpenCode is open source (less vendor lock-in)

**Cons**:

- Still tool-specific source
- Doesn't solve future tool adoption
- Requires rewriting current agents

**Decision**: Rejected - swaps lock-in target, doesn't eliminate it

### Alternative 3: YAML Metadata + Markdown Body

**Approach**: Pure YAML frontmatter in `docs/`, full markdown bodies in tool-specific directories

**Pros**:

- Smaller source files
- Metadata centralized

**Cons**:

- Agent prompt logic scattered across locations
- Still dual maintenance for prompt content
- Harder to review/edit agents holistically

**Decision**: Rejected - incomplete separation of concerns

### Alternative 4: Selected - Markdown with Tool-Agnostic Structure

**Approach**: Full markdown files in `docs/` with tool-agnostic frontmatter, sync to both formats

**Pros**:

- Complete source definition in one file
- Tool-agnostic format
- Documentation co-location
- Easy to review/edit

**Cons**:

- Added abstraction layer
- Migration effort

**Decision**: **SELECTED** - best balance of tool independence and maintainability

## Next Steps

1. **Review requirements.md**: Validate user stories and acceptance criteria
2. **Review technical.md**: Confirm architecture and design decisions
3. **Review delivery.md**: Approve execution plan
4. **Execute migration**: Follow delivery checklist phase-by-phase
5. **Validate**: Run comprehensive tests after each phase
6. **Document**: Update all references to new architecture

## References

- [AI Agents Convention](../../../docs/explanation/rules/development/agents/ex-ru-de-ag__ai-agents.md)
- [Repository Governance Architecture](../../../docs/explanation/ex-ru__repository-governance-architecture.md)
- [Plans Organization Convention](../../../docs/explanation/rules/conventions/project/ex-ru-co-pr__plans-organization.md)
- Current conversion script: `scripts/convert-agents-to-opencode.py`
- Current validation script: `scripts/validate-opencode-agents.py`
