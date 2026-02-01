# Requirements - Rules Consolidation

## Objectives

Based on pre-plan audit findings:

1. **Fix Skills naming convention** - Rename MULTI-FILE-TEMPLATE (violation) and 10 Skills to gerund form
2. **Add References to 7 existing Skills** that are missing "References" section
3. **Create 7 new Skills** to cover all agent domains (using gerund form)
4. **Assign skills to 39 agents** that have empty `skills: []` field
5. **Fix 6 factual inaccuracies** in delivery infrastructure documentation
6. **Enhance wow\_\_rules-checker** to validate Skills coverage
7. **Create missing link-fixer agent** to complete MCF pattern for ayokoding-web links

## Functional Requirements

### FR-0: Skills Naming Convention + allowed-tools

All Skills must follow official best practices:

- Lowercase letters, numbers, and hyphens only (max 64 chars)
- Gerund form (verb + -ing) preferred: `creating-by-example-tutorials`
- `allowed-tools` frontmatter to restrict tool access when active

**Current State**: 1 violation (`MULTI-FILE-TEMPLATE`), 10 not using gerund form, 0 with allowed-tools
**Target State**: All 17 Skills use lowercase gerund form with allowed-tools

**Note**: The allowed-tools field applies to ALL 10 existing Skills (not just renamed ones) plus all 7 new Skills created in Phase 2.

### FR-1: Skills References

All Skills must have "References" section linking to authoritative convention/development docs.

**Current State**: 3 of 10 Skills have References
**Target State**: 17 of 17 Skills have References (10 renamed existing + 7 new)

### FR-2: Agent Skills Coverage

All agents must have non-empty `skills:` field (agents need skills like employees need skills).

**Current State**: 5 of 44 agents have skills
**Target State**: 44 of 44 agents have skills

### FR-3: New Skills Creation

Create Skills to cover all agent domain families (using gerund form):

| New Skill                     | Convention Source       | Target Agents       |
| ----------------------------- | ----------------------- | ------------------- |
| `applying-content-quality`    | `quality.md`            | All content agents  |
| `applying-diataxis-framework` | `diataxis-framework.md` | `docs__*` agents    |
| `creating-project-plans`      | `plans-organization.md` | `plan__*` agents    |
| `writing-readme-files`        | `readme-quality.md`     | `readme__*` agents  |
| `defining-workflows`          | `workflow-pattern.md`   | `wow__workflow__*`  |
| `developing-agents`           | `ai-agents.md`          | `agent__maker`      |
| `validating-links`            | `linking.md`            | Link-checker agents |

### FR-4: Factual Accuracy

Fix incorrect delivery infrastructure documentation:

| Document          | Current (Incorrect)             | Target (Correct)                     |
| ----------------- | ------------------------------- | ------------------------------------ |
| Architecture docs | "CLAUDE.md delivers to agents"  | "CLAUDE.md loads into Orchestrator"  |
| Architecture docs | "Skills auto-deliver to agents" | "Skills deliver via `skills:` field" |
| Agent docs        | "Agents inherit CLAUDE.md"      | "Agents have isolated contexts"      |

### FR-5: Validation Enhancement

wow\_\_rules-checker must validate:

- All agents have non-empty `skills:` field
- All referenced skills exist
- All Skills have "References" section

### FR-6: Complete MCF Pattern for Links

Create missing `apps__ayokoding-web__link-fixer` agent to complete the Maker-Checker-Fixer pattern.

**Current State**: `apps__ayokoding-web__link-checker` exists with no corresponding fixer
**Target State**: Complete checker-fixer pair for ayokoding-web link validation

**Agent Capabilities**:

- Fix broken internal links (update paths)
- Fix Hugo link format violations (add language prefix, remove .md)
- Update/remove broken external links (with user confirmation)
- Re-validate before applying fixes (confidence levels)

### FR-7: Consolidate Tutorial Documentation

Merge related tutorial convention documents to reduce duplication and improve maintainability.

**Current State**: Two separate documents with overlapping content:

- `programming-language-content.md` - Content requirements for language tutorials
- `programming-language-structure.md` - Structural organization

**Target State**: Single consolidated document with both content and structure guidance.

**Rationale**: These documents are tightly coupled and often referenced together. Consolidation reduces maintenance burden and improves discoverability.

## Non-Functional Requirements

### NFR-1: No Breaking Changes

- Skills are additive (don't change existing behavior)
- Agent functionality unchanged
- All existing workflows continue working

### NFR-2: Trunk-Based Development

- Small, frequent commits to main
- No feature branches
- Atomic commits by domain area

## Constraints

### C-1: Skills as Infrastructure

Skills remain delivery infrastructure, not governance layer:

- Skills reference conventions, do not replace them
- Skills provide action-oriented guidance from authoritative docs
- Skills enable progressive disclosure, not rule creation

### C-2: Existing Skills Pattern

New Skills must follow the same pattern as existing Skills:

- SKILL.md with proper frontmatter
- References section linking to authoritative docs
- Auto-loading description for task matching

### C-3: Skills Naming Convention

All Skills (new and renamed) must follow:

- Gerund form (verb + -ing): `creating-by-example-tutorials`, `applying-content-quality`
- Lowercase only: no uppercase letters
- Max 64 characters

### C-4: Skills allowed-tools

All Skills must have `allowed-tools` frontmatter to restrict tool access when active:

- Content creation Skills: `Read, Write, Edit, Glob, Grep`
- Validation Skills: `Read, Glob, Grep, Write, Bash`
- Link validation Skills: `Read, Glob, Grep, WebFetch, WebSearch, Write, Edit, Bash`
- Git workflow Skills: `Bash`

### C-5: Bash Tools for .claude/ Folder Edits

**CRITICAL**: All file operations in `.claude/` folders MUST use Bash tools (heredoc, sed, awk), NOT Write/Edit tools.

**Rationale**: This enables autonomous agent operation without user approval prompts. See [AI Agents Convention - Writing to .claude Folders](../../../../governance/development/agents/ai-agents.md#writing-to-claude-folders).

**Applies to**:

- Creating new Skills in `.claude/skills/`
- Creating new agents in `.claude/agents/`
- Updating Skills/agents frontmatter
- Any file modification within `.claude/` directory

**Implementation guidance**:

- Use `cat <<'EOF' > file` for creating new files
- Use `sed -i` for inline edits
- Use `awk` for complex transformations
- NEVER use Write or Edit tools for `.claude/` files

## Success Criteria

### Phase 0: Fix Skills Naming Convention + Add allowed-tools

- [ ] `MULTI-FILE-TEMPLATE` renamed to `multi-file-template`
- [ ] 10 existing Skills renamed to gerund form
- [ ] All 10 existing Skills have `allowed-tools` frontmatter
- [ ] All agent `skills:` references updated
- [ ] Skills README updated
- [ ] CLAUDE.md updated

### Phase 1: Add References to Existing Skills

- [ ] `creating-by-example-tutorials` has References section
- [ ] `assessing-criticality-confidence` has References section
- [ ] `developing-ayokoding-content` has References section
- [ ] `validating-factual-accuracy` has References section
- [ ] `writing-gherkin-criteria` has References section
- [ ] `developing-ose-content` has References section
- [ ] `practicing-trunk-based-development` has References section

### Phase 2: Create New Skills

- [ ] 7 new Skills created with gerund names and correct structure
- [ ] All new Skills have `allowed-tools` frontmatter
- [ ] All new Skills have References sections
- [ ] Skills README updated

### Phase 3: Assign Skills to Agents

- [ ] All 45 agents have non-empty `skills:` field (44 existing + 1 new)
- [ ] All referenced skills exist in `.claude/skills/`

### Phase 4: Fix Factual Inaccuracies

- [ ] `ex-ru__repository-governance-architecture.md` corrected
- [ ] `CLAUDE.md` corrected
- [ ] `.claude/skills/README.md` corrected
- [ ] `ai-agents.md` corrected

### Phase 5: Enhance Validation

- [ ] wow\_\_rules-checker validates non-empty skills
- [ ] wow\_\_rules-checker validates Skills references

### Phase 6: Create Missing Link Fixer Agent

- [ ] `apps__ayokoding-web__link-fixer` agent created
- [ ] Agent has correct frontmatter (name, description, tools, model, color, skills)
- [ ] Agent registered in `.claude/agents/README.md`
- [ ] CLAUDE.md agent list updated

### Phase 7: Consolidate Tutorial Documentation

- [ ] `programming-language-content.md` merged into `programming-language-structure.md`
- [ ] Original content file deleted or redirected
- [ ] All references updated across codebase
- [ ] Conventions README updated
- [ ] CLAUDE.md updated if referenced

## Acceptance Criteria

### Skills Naming Complete

```gherkin
Given all Skills have been renamed
When I check each Skill directory name
Then every Skill uses lowercase letters, numbers, and hyphens only
And every Skill uses gerund form (verb + -ing)
```

### Skills allowed-tools Complete

```gherkin
Given all Skills have been updated
When I check each Skill's SKILL.md frontmatter
Then every Skill has an allowed-tools field
And the allowed-tools restricts Claude to appropriate tools for that Skill's purpose
```

### Skills References Complete

```gherkin
Given all Skills have been updated
When I check each Skill
Then every Skill has a "References" section
And every reference links to an existing document
```

### Agent Skills Coverage Complete

```gherkin
Given all agents have been updated
When I check each agent's frontmatter
Then every agent has a non-empty skills: field
And every referenced skill exists in .claude/skills/
```

### Factual Accuracy Fixed

```gherkin
Given all documentation has been updated
When I review delivery infrastructure descriptions
Then all docs correctly state CLAUDE.md loads into Orchestrator
And all docs correctly state agents have isolated contexts
And all docs correctly state Skills deliver via skills: field only
```

### Validation Enhanced

```gherkin
Given wow__rules-checker has been enhanced
When I run validation
Then agents with empty skills are flagged as violations
And Skills without References are flagged as violations
```

### Link Fixer Agent Created

```gherkin
Given apps__ayokoding-web__link-fixer has been created
When I check the agent file
Then it has valid frontmatter with name, description, tools, model, color, skills
And skills field contains validating-links and assessing-criticality-confidence
And it is registered in .claude/agents/README.md
And CLAUDE.md agent list includes it
```

### Tutorial Documentation Consolidated

```gherkin
Given tutorial convention documents have been consolidated
When I check governance/conventions/tutorials/
Then programming-language-structure.md contains both content and structure guidance
And programming-language-content.md no longer exists
And all references to the old file point to the consolidated file
```

### Bash Tools for .claude/ Verified

```gherkin
Given all .claude/ file modifications in this plan
When I review the implementation
Then all file creations use heredoc syntax (cat <<'EOF')
And all file edits use sed or awk
And NO Write or Edit tools are used for .claude/ files
```
