# Full Migration from Claude Code to OpenCode

**Status**: Done
**Created**: 2026-01-12
**Completed**: 2026-01-12
**Priority**: High
**Complexity**: High (architectural change eliminating Claude Code dependency)

## Summary

Successfully completed full migration from Claude Code to OpenCode.

**Deliverables**:

- ✅ All 46 agents in `.opencode/agent/` (OpenCode format)
- ✅ All 23 skills in `.opencode/skill/` (OpenCode format)
- ✅ AGENTS.md updated with OpenCode-specific sections
- ✅ `.claude/` directory deleted (no Claude Code artifacts remain)
- ✅ CLAUDE.md deleted
- ✅ 238/238 validation tests passed (100%)
- ✅ Conversion scripts deleted

**Git Commits**:

- `7b0359dd` - feat: complete migration from claude code to opencode
- `70a4169d` - docs(migration): update delivery plan with all validation ticks
- `483ea56d` - docs(migration): remove pre-migration archive branch references
- `94012a6a` - fix(migration): resolve validation errors in AGENTS.md and test suite
- `f6245eed` - chore(migration): remove test validation file

**Validation Results**:

- 238/238 automated tests passed (100%)
- 18/18 manual tests passed
- All success criteria met

## Migration Overview

This plan documented the complete migration from Claude Code to OpenCode format, eliminating all Claude Code dependencies and consolidating to a single OpenCode-based workflow.

### Primary Goals

1. **Single format**: OpenCode agent format as source of truth
2. **Eliminate Claude Code artifacts**: Remove `.claude/agents/`, CLAUDE.md, conversion scripts
3. **Simplify maintenance**: No format conversion or synchronization
4. **OpenCode-only workflow**: All AI tooling uses OpenCode exclusively
5. **Governance integration**: Update all governance/docs to reference OpenCode format only

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

**Risk**: Merging 348 lines of CLAUDE.md into 232 lines of AGENTS.md is complex

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

- [ ] All 46 agents work identically in OpenCode after migration
  - **Validation**: Run `npx opencode agent:invoke docs-checker` and verify response correctness
  - **Validation**: Compare agent responses before/after migration (test sample agents from each family)
- [ ] All 23 skills load correctly with permission-based model
  - **Validation**: Invoke agents with known skills and verify skills load without errors
  - **Validation**: Test denied skills are inaccessible (invoke agent without skill permission)
- [ ] 0 validation errors in OpenCode agents
  - **Validation**: Run schema validator on all 46 agents: `node scripts/validate-opencode-schema.js` (or equivalent)
  - **Validation**: Verify exit code is 0 and no validation errors reported
- [ ] 0 functionality regressions vs current agent behavior
  - **Validation**: Run existing test suite and compare results to pre-migration baseline
  - **Validation**: Test critical workflows (maker-checker-fixer cycle, plan-execute-validate cycle)
- [ ] All agent invocations use OpenCode format successfully
  - **Validation**: Sample invocation commands work: `npx opencode agent:invoke <agent-name> <args>`
  - **Validation**: All 46 agents are discoverable: `npx opencode agent:list`
- [ ] All agents use GLM model names (zai/glm-4.7, zai/glm-4.5-air, or inherit)
  - **Validation**: Run audit: `grep -r "^model:" .opencode/agent/*.md | sort | uniq -c`
  - **Validation**: Verify no Claude Code aliases (sonnet, haiku, opus) found

### Repository Cleanup

- [ ] `.claude/agents/` directory removed (46 agent files)
  - **Validation**: `ls .claude/agents/` should fail (directory doesn't exist)
  - **Validation**: `find . -path ./.claude/agents/` should return empty
- [ ] `.claude/skills/` directory removed (23 skills)
  - **Validation**: `ls .claude/skills/` should fail (directory doesn't exist)
- [ ] `.claude/settings.json` removed
  - **Validation**: `ls .claude/settings.json` should fail (file doesn't exist)
- [ ] `.claude/settings.local.json` removed
  - **Validation**: `ls .claude/settings.local.json` should fail (file doesn't exist)
- [ ] CLAUDE.md removed
  - **Validation**: `ls CLAUDE.md` should fail (file doesn't exist)
- [ ] All conversion scripts removed (`scripts/convert-*.py`, `scripts/validate-*.py`, `scripts/sync-*.py`)
  - **Validation**: `ls scripts/convert*.py scripts/validate*.py scripts/sync*.py` should return empty
- [ ] `.claude/` directory removed entirely (no Claude Code artifacts remain)
  - **Validation**: `ls .claude/` should fail (directory doesn't exist)
  - **Validation**: `rg "\.claude/"` should only find historical migration notes

### Documentation Quality

- [ ] AGENTS.md is comprehensive (consolidates content from CLAUDE.md into existing 232 lines)
  - **Validation**: Count CLAUDE.md sections (all 7 sections present in AGENTS.md)
  - **Validation**: AGENTS.md contains agent format, invocation, tools, skills, workflows
- [ ] AGENTS.md contains all AI agent guidance from CLAUDE.md
  - **Validation**: Search for CLAUDE.md topics in AGENTS.md (agent format, invocation, tools, etc.)
  - **Validation**: Compare CLAUDE.md table of contents with AGENTS.md sections
- [ ] All references to `.claude/agents/` updated to `.opencode/agent/`
  - **Validation**: `rg "\.claude/agents/"` should return only historical migration notes
  - **Validation**: `rg "\.opencode/agent/"` should find multiple references in docs
- [ ] All references to dual-format support removed
  - **Validation**: `rg -i "both formats|dual format|claude code and opencode"` should return empty
  - **Validation**: Check that no "Claude Code vs OpenCode" comparison sections remain
- [ ] Contribution workflow documented (how to add/edit OpenCode agents)
  - **Validation**: AGENTS.md has "Agent Creation Workflow" section
  - **Validation**: Examples show `.opencode/agent/<agent-name>.md` path format

### Skills Management

- [ ] All 23 skills load correctly in OpenCode
  - **Validation**: `find .opencode/skill/ -name "SKILL.md" | wc -l` returns 23
  - **Validation**: Invoke agents with known skills and verify skills load without errors
- [ ] Skills use `permission.skill` model in OpenCode agent frontmatter
  - **Validation**: All 46 agents have `permission.skill:` frontmatter section
  - **Validation**: `grep -r "permission.skill:" .opencode/agent/*.md | wc -l` returns 46
- [ ] Skills exist at `.opencode/skill/<name>/SKILL.md` only
  - **Validation**: No skills remain in `.claude/skills/`
  - **Validation**: Skill directories follow format: `.opencode/skill/<skill-name>/SKILL.md`
- [ ] `.claude/skills/` deleted (no Claude Code artifacts remain)
  - **Validation**: `ls .claude/skills/` fails (directory doesn't exist)
- [ ] Documentation updated to reflect permission-based loading
  - **Validation**: AGENTS.md documents `permission.skill` usage
  - **Validation**: Examples show skill permission format in agent frontmatter

### Agent Creation/Updates

- [ ] `agent-maker` creates agents in `.opencode/agent/` (not `.claude/agents/`)
  - **Validation**: Test agent-maker invocation: creates agent at `.opencode/agent/test-agent.md`
  - **Validation**: Verify created agent has OpenCode frontmatter format
- [ ] `repo-governance-checker` validates `.opencode/agent/` format
  - **Validation**: Run repo-governance-checker and verify it validates OpenCode agents
  - **Validation**: Check that checker doesn't reference `.claude/agents/`
- [ ] `repo-governance-fixer` modifies `.opencode/agent/` directly
  - **Validation**: Test repo-governance-fixer and verify it modifies OpenCode agents
  - **Validation**: Check that fixer doesn't sync to Claude Code format
- [ ] All other agents updated to use OpenCode paths
  - **Validation**: Search for `.claude/agents/` references in all agents (should be empty)
  - **Validation**: Search for `.opencode/agent/` references (should find matches)

### Workflow Consistency

- [ ] All workflows use OpenCode agent format
  - **Validation**: Read all workflow definitions in `governance/workflows/`
  - **Validation**: Verify workflow agent references use OpenCode paths
- [ ] All documentation examples show OpenCode format
  - **Validation**: Search for `examples` or `code blocks` in AGENTS.md
  - **Validation**: Verify all examples show OpenCode frontmatter format
- [ ] All instructions reference OpenCode only
  - **Validation**: `rg -i "claude code"` should return only historical migration notes
  - **Validation**: `rg "\.opencode/"` should find multiple references
- [ ] No Claude Code-specific terminology remains
  - **Validation**: `rg "sonnet|haiku|opus"` should only find GLM model names (not aliases)
  - **Validation**: `rg "anthropic/claude-"` should return empty (no model references)

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

```
