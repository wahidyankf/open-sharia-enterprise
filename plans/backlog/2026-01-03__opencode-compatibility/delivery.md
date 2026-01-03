# Delivery Plan: OpenCode Compatibility

## Implementation Phases

This plan is divided into 6 phases, ordered by priority and dependency:

| Phase | Focus                         | Priority     | Dependencies |
| ----- | ----------------------------- | ------------ | ------------ |
| **0** | **Skill Renaming (REQUIRED)** | **CRITICAL** | **None**     |
| 1     | Skills Validation             | Must         | Phase 0      |
| 2     | Core Configuration            | Must         | Phase 1      |
| 3     | Instructions File             | Must         | Phase 2      |
| 4     | MCP Integration               | Should       | Phase 2      |
| 5     | Agent Translation             | Could        | Phase 3      |

---

## Phase 0: Skill Renaming (CRITICAL PREREQUISITE)

**Goal**: Fix skill naming to comply with both Claude Code and OpenCode specs

### Why This Is Required

**CONFIRMED**: Both Claude Code AND OpenCode require skill names matching `[a-z0-9-]+`:

- Underscores (`_`) are **NOT allowed**
- Current names like `docs__applying-content-quality` are **INVALID**
- Skills will NOT load in OpenCode until renamed
- Skills may already be working in Claude Code only due to legacy support

### Renaming Tasks

- [ ] **0.1 Create skill renaming script**

  ```bash
  #!/bin/bash
  # Rename all skill directories from underscore to hyphen format
  for dir in .claude/skills/*__*; do
    newname=$(echo "$dir" | sed 's/__/-/g')
    git mv "$dir" "$newname"
  done
  ```

- [ ] **0.2 Rename all 19 skill directories**

  | Current Name                                 | New Name                                    |
  | -------------------------------------------- | ------------------------------------------- |
  | `docs__applying-content-quality`             | `docs-applying-content-quality`             |
  | `docs__applying-diataxis-framework`          | `docs-applying-diataxis-framework`          |
  | `docs__creating-accessible-diagrams`         | `docs-creating-accessible-diagrams`         |
  | `docs__creating-by-example-tutorials`        | `docs-creating-by-example-tutorials`        |
  | `docs__validating-factual-accuracy`          | `docs-validating-factual-accuracy`          |
  | `docs__validating-links`                     | `docs-validating-links`                     |
  | `readme__writing-readme-files`               | `readme-writing-readme-files`               |
  | `plan__creating-project-plans`               | `plan-creating-project-plans`               |
  | `plan__writing-gherkin-criteria`             | `plan-writing-gherkin-criteria`             |
  | `agent__developing-agents`                   | `agent-developing-agents`                   |
  | `apps__ayokoding-web__developing-content`    | `apps-ayokoding-web-developing-content`     |
  | `apps__ose-platform-web__developing-content` | `apps-ose-platform-web-developing-content`  |
  | `wow__applying-maker-checker-fixer`          | `wow-applying-maker-checker-fixer`          |
  | `wow__assessing-criticality-confidence`      | `wow-assessing-criticality-confidence`      |
  | `wow__defining-workflows`                    | `wow-defining-workflows`                    |
  | `wow__generating-validation-reports`         | `wow-generating-validation-reports`         |
  | `wow__multi-file-template`                   | `wow-multi-file-template`                   |
  | `wow__practicing-trunk-based-development`    | `wow-practicing-trunk-based-development`    |
  | `wow__understanding-repository-architecture` | `wow-understanding-repository-architecture` |

- [ ] **0.3 Update SKILL.md `name` field in each skill**
  - Each SKILL.md must have `name:` matching directory name
  - Example: `name: docs-applying-content-quality`

- [ ] **0.4 Update all agent files**
  - Search for `skills:` field in `.claude/agents/*.md`
  - Replace underscore skill references with hyphen versions

- [ ] **0.5 Update documentation references**
  - `.claude/skills/README.md` - update skill listings
  - `CLAUDE.md` - update any skill references
  - `docs/` - update any skill references

- [ ] **0.6 Test Claude Code still works**
  - Run `claude` and verify skills load
  - Invoke a skill and verify it works
  - Document any issues

### Phase 0 Completion Criteria

```gherkin
Given all skills have been renamed with hyphens
When Claude Code lists available skills
Then all 19 skills appear with new names
And skill invocation works correctly
And no underscore skill names remain
```

---

## Phase 1: OpenCode Skills Validation

**Goal**: Confirm renamed skills work with OpenCode

### Prerequisites

- [ ] **Phase 0 completed** (skills renamed)
- [ ] Install OpenCode CLI

  ```bash
  # Option 1: npm global
  npm install -g @opencode/cli

  # Option 2: Direct download from releases
  # https://github.com/sst/opencode/releases
  ```

### Validation Tasks

- [ ] **1.1 Verify skill discovery**
  - Run `opencode` in repository root
  - Check if skills from `.claude/skills/` are detected
  - Expected: All 19 skills should appear

- [ ] **1.2 Verify skill names are valid**
  - Confirm no validation errors on startup
  - Confirm skill names match `^[a-z0-9]+(-[a-z0-9]+)*$`

- [ ] **1.3 Test skill invocation**
  - Invoke skill via OpenCode: "Use the docs-applying-content-quality skill"
  - Verify skill content loads correctly
  - Verify skill instructions are followed

- [ ] **1.4 Document any issues**
  - List any skills that don't load
  - Note error messages
  - Fix any remaining issues

### Phase 1 Completion Criteria

```gherkin
Given OpenCode is installed and running
When the skill tool lists available skills
Then all 19 skills from .claude/skills/ are listed
And skill invocation loads correct content
And skill behavior matches Claude Code
```

---

## Phase 2: Core Configuration

**Goal**: Create opencode.json with essential settings

### Tasks

- [ ] **2.1 Create opencode.json**

  ```json
  {
    "$schema": "https://opencode.ai/config.json",
    "model": "anthropic/claude-sonnet-4-5",
    "small_model": "anthropic/claude-haiku-4-5",
    "provider": {
      "anthropic": {
        "options": {
          "timeout": 600000
        }
      }
    }
  }
  ```

- [ ] **2.2 Configure tool permissions**
  - Enable all development tools
  - Set permission levels matching Claude Code defaults
  - Allow bash, write, edit without prompts

- [ ] **2.3 Configure TUI settings**
  - Set scroll speed
  - Configure diff display style
  - Disable auto-share for privacy

- [ ] **2.4 Test configuration**
  - Run `opencode` and verify model loads
  - Verify tools are available
  - Verify settings are applied

- [ ] **2.5 Add to git**
  - Commit opencode.json
  - Verify not gitignored

### Phase 2 Completion Criteria

```gherkin
Given opencode.json exists with valid configuration
When OpenCode starts in the repository
Then the model is set to anthropic/claude-sonnet-4-5
And all tools (bash, read, write, edit, grep, glob) are available
And tool permissions allow development workflow
```

---

## Phase 3: Instructions File

**Goal**: Create AGENTS.md for OpenCode project guidance

### Tasks

- [ ] **3.1 Draft AGENTS.md structure**
  - Project overview (2-3 sentences)
  - Environment setup (Volta, Node.js, npm)
  - Key conventions summary
  - File structure overview
  - Link to CLAUDE.md for details

- [ ] **3.2 Write AGENTS.md content**
  - Keep under 10KB
  - Focus on actionable guidance
  - Use clear markdown formatting
  - Include code examples where helpful

- [ ] **3.3 Configure instructions loading**
  - Add `instructions` field to opencode.json
  - Include AGENTS.md as primary
  - Optionally include key convention docs

- [ ] **3.4 Test instructions loading**
  - Start OpenCode fresh
  - Verify AGENTS.md content is recognized
  - Test that guidance is followed

- [ ] **3.5 Update README.md**
  - Add note about OpenCode support
  - Reference AGENTS.md
  - Explain dual-tool compatibility

### Phase 3 Completion Criteria

```gherkin
Given AGENTS.md exists at repository root
When OpenCode initializes
Then project instructions from AGENTS.md are loaded
And OpenCode follows the documented conventions
And Claude Code continues to work with CLAUDE.md
```

---

## Phase 4: MCP Integration

**Goal**: Configure MCP servers for OpenCode

### Tasks

- [ ] **4.1 Add Playwright MCP**

  ```json
  "mcp": {
    "playwright": {
      "type": "local",
      "command": ["npx", "@playwright/mcp@latest"],
      "enabled": true
    }
  }
  ```

- [ ] **4.2 Add Context7 MCP**

  ```json
  "mcp": {
    "context7": {
      "type": "local",
      "command": ["npx", "-y", "@context7/mcp-server"],
      "enabled": true
    }
  }
  ```

- [ ] **4.3 Test MCP connections**
  - Start OpenCode
  - Verify MCP servers are listed
  - Test browser automation tools
  - Test documentation lookup

- [ ] **4.4 Handle port conflicts**
  - If both tools run simultaneously, ensure no conflicts
  - Consider different ports if needed
  - Document any workarounds

- [ ] **4.5 Update .mcp.json if needed**
  - Ensure Claude Code MCP config still works
  - Verify no regressions

### Phase 4 Completion Criteria

```gherkin
Given MCP servers are configured in opencode.json
When OpenCode starts
Then Playwright MCP server connects successfully
And Context7 MCP server connects successfully
And browser_navigate, browser_snapshot tools work
And resolve-library-id, query-docs tools work
```

---

## Phase 5: Agent Translation (Optional)

**Goal**: Create OpenCode-specific agents for key workflows

### Tasks

- [ ] **5.1 Create .opencode/agent/ directory**

  ```bash
  mkdir -p .opencode/agent
  ```

- [ ] **5.2 Translate high-priority agents**
  - Start with 3-5 most-used agents:
    - docs-maker (from docs\_\_maker)
    - plan-executor (from plan\_\_executor)
    - rules-checker (from wow\_\_rules-checker)

- [ ] **5.3 Document translation process**
  - Create guide for translating Claude agents to OpenCode
  - Note format differences
  - Provide examples

- [ ] **5.4 Test translated agents**
  - Invoke each translated agent
  - Verify behavior matches Claude Code equivalent
  - Document any differences

- [ ] **5.5 Consider automation**
  - Evaluate if agent translation can be scripted
  - Create translation tool if beneficial

### Phase 5 Completion Criteria

```gherkin
Given translated agents exist in .opencode/agent/
When OpenCode lists available agents
Then translated agents appear and are usable
And agent behavior matches Claude Code equivalents
And documentation explains translation process
```

---

## Validation Checklist

### Pre-Completion Checks

- [ ] OpenCode starts without errors
- [ ] All 19 skills are discoverable
- [ ] Model is set to anthropic/claude-sonnet-4-5
- [ ] MCP servers connect successfully
- [ ] AGENTS.md is loaded as instructions
- [ ] Tools (bash, read, write, edit, grep, glob) work

### Regression Checks

- [ ] Claude Code still starts correctly
- [ ] Claude Code agents still load
- [ ] Claude Code skills still work
- [ ] Claude Code MCP servers still connect
- [ ] No configuration conflicts

### Documentation Checks

- [ ] README.md updated with OpenCode mention
- [ ] AGENTS.md is clear and actionable
- [ ] tech-docs.md accurately reflects implementation
- [ ] Any workarounds are documented

---

## Rollback Plan

If issues are discovered:

1. **Configuration issues**: Remove opencode.json
2. **Skills incompatibility**: Create .opencode/skill/ with compatible copies
3. **MCP conflicts**: Adjust port configuration or disable one set
4. **Instruction conflicts**: Rename AGENTS.md temporarily

---

## Post-Implementation Tasks

- [ ] Add to wow\_\_rules-checker: OpenCode configuration validation
- [ ] Create agent: opencode-compatibility-checker (if needed)
- [ ] Update CLAUDE.md with dual-tool notes (brief, link to docs)
- [ ] Create how-to guide: Using OpenCode with this repository
- [ ] Periodic validation: Test compatibility quarterly

---

## Completion Status

| Phase | Status          | Completed Date | Notes                                       |
| ----- | --------------- | -------------- | ------------------------------------------- |
| **0** | **Not Started** | -              | **CRITICAL: Skill renaming required first** |
| 1     | Not Started     | -              | Skills validation (after Phase 0)           |
| 2     | Not Started     | -              | Core configuration                          |
| 3     | Not Started     | -              | AGENTS.md                                   |
| 4     | Not Started     | -              | MCP integration                             |
| 5     | Not Started     | -              | Agent translation (optional)                |

**Overall Status**: Backlog
**Ready for Implementation**: Yes
**Critical Prerequisite**: Phase 0 (skill renaming) MUST be completed first
