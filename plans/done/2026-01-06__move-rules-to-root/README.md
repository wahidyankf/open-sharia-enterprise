# Move Rules Documentation to Root Directory

**Status**: Done
**Created**: 2026-01-06
**Completed**: 2026-01-10
**Priority**: High
**Complexity**: Medium (directory move + reference updates across ~150 files)

## Problem Statement

Currently, system rules and conventions exist under `governance/` and must follow Obsidian markdown formatting rules (file naming with `__` separator, linking syntax, emoji restrictions, etc.).

**Key Issues**:

1. **Obsidian constraints on system rules**: Agent-generated or human-written rules don't naturally align with Obsidian formatting requirements
2. **Unclear separation**: System rules and human-written documentation both under `docs/` makes purpose unclear
3. **Agent creation complexity**: Agents creating rules must know and follow Obsidian-specific conventions
4. **Formatting friction**: Rules documentation requires Obsidian-specific formatting that doesn't add value to system-level content

## Proposed Solution

**Move `governance/` (excluding `agents/`) to repository root as `/governance/`**

```
governance/           /governance/
├── vision/              →        vision/         (Layer 0: WHY we exist)
├── principles/         →        principles/       (Layer 1: WHY - values)
├── conventions/       →        conventions/     (Layer 2: WHAT - documentation rules)
├── development/       →        development/     (Layer 3: HOW - software practices)
├── workflows/        →        workflows/       (Layer 5: WHEN - multi-step processes)
├── ex-ru__*.md         →        ex-ru__*.md
└── README.md           →        README.md
```

**NOT moving**: `governance/agents/` - left untouched (handled by separate plan `2026-01-04__agents-docs-source-of-truth/`)

## Goals

### Primary Goals

1. **Separate system rules from documentation**: System rules use normal markdown (no Obsidian constraints)
2. **Clear directory purpose**: `/governance/` for system rules, `docs/` for human-written docs
3. **Simplify agent creation**: Agents create rules without Obsidian formatting requirements
4. **Preserve git history**: Use `git mv` for all directory moves

### Secondary Goals

1. **Zero broken links**: All references updated after move
2. **Governance accuracy**: Architecture document reflects new paths
3. **Atomic execution**: Single commit with all changes
4. **Validation integrity**: Each phase validated before proceeding

## Benefits

### Separation of Concerns

- **Clear boundary**: System rules (`/governance/`) vs. human-written docs (`docs/`)
- **Purpose-aligned**: `/governance/` for governance, `docs/` for learning
- **Format-appropriate**: Normal markdown for rules, Obsidian format for docs

### Maintainability

- **Easier agent creation**: No Obsidian constraints when agents create rules
- **Simpler updates**: Manual edits to rules don't require Obsidian knowledge
- **Better traceability**: Clear distinction between governance and documentation

### Git History

- **Preserved via git mv**: All file history maintained during move
- **Single commit**: All changes atomic (no intermediate states)
- **Clear rollback**: Revert single commit if needed

## Trade-offs and Risks

### Large Update Scope

**Trade-off**: Updating ~150 files with path references

**Mitigation**:

- Automated sed commands for simple patterns
- Manual review of complex relative links
- Validation with repo-governance-checker after updates

### Link Breakage Risk

**Risk**: Some links may break during move/update

**Mitigation**:

- Validate after each phase
- Run repo-governance-checker to detect broken links
- Manual spot-check of key documents

### Directory Structure Change

**Risk**: Team used to `governance/` path will need to learn new location

**Mitigation**:

- Update all references consistently
- Clear commit message explaining change
- Update AGENTS.md and CLAUDE.md as primary reference

## Success Criteria

### File Move Success

- [ ] All 5 directories moved to `/governance/` (vision, principles, conventions, development, workflows)
- [ ] 2 files moved to `/governance/` (ex-ru\_\_\*.md, README.md)
- [ ] `governance/` directory removed (no leftover files)
- [ ] Git shows moves as renames (history preserved)
- [ ] Zero untracked files remaining

### Reference Update Success

- [ ] Zero occurrences of `governance/` in entire repository
- [ ] All references to `/governance/` work correctly
- [ ] repo-governance-checker reports zero broken links
- [ ] Key documents (CLAUDE.md, AGENTS.md, governance) link correctly

### Documentation Success

- [ ] Governance architecture updated with `/governance/` paths for all layers
- [ ] Mermaid diagram in governance doc shows `/governance/` locations
- [ ] 45 agent files updated with new paths
- [ ] 23 skill files updated with new paths
- [ ] 1 workflow (ex-ru-wf-wo\_\_repository-rules-validation.md) updated

### Validation Success

- [ ] Phase 1 validation: All files in `/governance/` exist
- [ ] Phase 2 validation: Zero old-path references in governance doc
- [ ] Phase 3 validation: repo-governance-checker reports no broken links
- [ ] Final validation: Git diff shows only expected changes

## Plan Structure

This plan is organized into four documents:

1. **README.md** (this file): Overview, problem, solution, risks, success criteria
2. **[requirements.md](./requirements.md)**: User stories, acceptance criteria (Gherkin format)
3. **[tech-docs.md](./tech-docs.md)**: Architecture, file mapping, sed commands, validation criteria
4. **[delivery.md](./delivery.md)**: Phase-by-phase execution plan with validation gates

## Dependencies

### External Dependencies

- None (independent plan)

### Internal Dependencies

- **Separate from**: `plans/backlog/2026-01-04__agents-docs-source-of-truth/`
- `governance/agents/` NOT moved (that plan handles it)
- All other rules directories moved by this plan

## Constraints

1. **Git history preservation**: Must use `git mv` not copy+delete
2. **Zero broken links**: All references must work after move
3. **Single commit**: All changes in one atomic commit (YOLO approach - no rollback plan)
4. **Validate after each phase**: Don't proceed if phase fails validation

## Assumptions

1. repo-governance-checker agent is available and can validate links
2. Git mv commands preserve history for moved files
3. Team will adapt to new `/governance/` path
4. ~150 files require path reference updates (agents, skills, docs)
5. All docs/ content follows Obsidian rules (tutorials/, how-to/, reference/, explanation/)

## Alternatives Considered

### Alternative 1: Keep Current Structure (Status Quo)

**Approach**: Keep `governance/` with Obsidian constraints

**Pros**:

- No changes required
- Team familiar with current structure

**Cons**:

- Obsidian constraints don't add value to system rules
- Agents creating rules must know Obsidian format
- Unclear separation between system rules and docs

**Decision**: Rejected - doesn't solve Obsidian constraint problem

### Alternative 2: Move to Different Location (e.g., /system-governance/)

**Approach**: Move to `/system-governance/` instead of `/governance/`

**Pros**:

- More explicit name
- Clearer separation

**Cons**:

- Longer path to type
- Less intuitive than `/governance/`

**Decision**: Rejected - `/governance/` is concise and intuitive

### Alternative 3: Keep in docs/ but Remove Obsidian Rules

**Approach**: Keep `governance/` but exempt from Obsidian rules

**Pros**:

- Less directory change
- Still under docs/ hierarchy

**Cons**:

- Inconsistent rules application (docs/ follows Obsidian, but governance/ doesn't)
- Complex documentation (which rules apply where?)
- Confusing for contributors

**Decision**: Rejected - inconsistent rules application

### Alternative 4: Selected - Move to /governance/

**Approach**: Move to `/governance/` at repository root

**Pros**:

- Clear separation from docs/
- No Obsidian constraints
- Intuitive directory name
- Simple path
- Consistent rules application

**Cons**:

- Large update scope (~150 files)
- Team must learn new location

**Decision**: **SELECTED** - best balance of clarity, simplicity, and maintainability

## Next Steps

1. **Review requirements.md**: Validate user stories and acceptance criteria
2. **Review technical.md**: Confirm file mapping, sed commands, validation criteria
3. **Review delivery.md**: Approve execution plan and phase sequence
4. **Execute migration**: Follow delivery checklist phase-by-phase
5. **Validate**: Run validation checks after each phase
6. **Commit**: Single atomic commit with detailed message

## References

- Repository Governance Architecture (will be at `/governance/ex-ru__repository-governance-architecture.md` after move)
- [Plans Organization](../../governance/conventions/structure/plans.md)
- [Agent and Skill Definitions as Documentation Source of Truth](../2026-01-04__agents-docs-source-of-truth/) - Separate plan handling `governance/agents/`
