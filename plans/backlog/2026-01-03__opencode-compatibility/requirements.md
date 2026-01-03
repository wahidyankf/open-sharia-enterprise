# Requirements: OpenCode Compatibility

## Objectives

### Primary Objectives

1. **Zero Breaking Changes**: Existing Claude Code functionality must remain intact
2. **Cost Optimization**: Use GLM-4.7 model (8.6x-20x cheaper than Claude Sonnet) in OpenCode
3. **Seamless Switching**: Developers can use either tool interchangeably
4. **Shared Configuration**: Maximize reuse of existing configurations where formats allow
5. **Minimal Duplication**: Avoid maintaining parallel configurations when possible

### Secondary Objectives

1. **Skills Portability Validation**: Confirm existing skills work with OpenCode
2. **Documentation Alignment**: Create AGENTS.md that complements CLAUDE.md
3. **Team Onboarding**: Enable new team members to choose their preferred tool

## User Stories

### US-01: Developer Switches to OpenCode

```gherkin
Feature: OpenCode Project Initialization

Scenario: First-time OpenCode user opens repository
  Given the developer has OpenCode installed
  And the repository has opencode.json configured with GLM-4.7
  When they run `opencode` in the project root
  Then OpenCode starts with zai/glm-4.7 model (cost-optimized, 8.6x cheaper than Claude)
  And displays project instructions from AGENTS.md
  And lists available MCP servers (playwright, context7)
  And can invoke skills by name

Scenario: OpenCode user creates content
  Given OpenCode is running in the repository
  When the developer asks to create Hugo content
  Then OpenCode loads apps__ayokoding-web__developing-content skill
  And follows the same conventions as Claude Code
  And produces identical output quality
```

### US-02: Developer Uses Both Tools

```gherkin
Feature: Dual-Tool Development

Scenario: Developer alternates between tools
  Given the repository supports both Claude Code and OpenCode
  When the developer uses Claude Code for agent workflows
  And switches to OpenCode for general coding
  Then both tools respect the same project conventions
  And MCP servers are available in both
  And skills produce consistent behavior

Scenario: Team uses mixed tools
  Given some team members prefer Claude Code
  And some team members prefer OpenCode
  When they collaborate on the same codebase
  Then CLAUDE.md guides Claude Code users
  And AGENTS.md guides OpenCode users
  And both follow identical conventions
```

### US-03: MCP Server Sharing

```gherkin
Feature: MCP Server Configuration

Scenario: Playwright MCP works in both tools
  Given Playwright MCP is configured for Claude Code
  And equivalent configuration exists for OpenCode
  When either tool invokes browser automation
  Then the same Playwright server handles requests
  And browser_navigate, browser_snapshot work identically

Scenario: Context7 documentation lookup
  Given Context7 MCP is configured for both tools
  When either tool queries library documentation
  Then Context7 returns identical results
  And resolve-library-id, query-docs work consistently
```

### US-04: Skills Compatibility

```gherkin
Feature: Cross-Platform Skills

Scenario: OpenCode discovers Claude skills
  Given the repository has 19 skills in .claude/skills/
  When OpenCode initializes
  Then it discovers all skills from .claude/skills/*/SKILL.md
  And the skill tool lists all available skills
  And skills can be invoked by name

Scenario: Skill behavior matches
  Given the skill docs__applying-content-quality exists
  When Claude Code invokes the skill
  And OpenCode invokes the same skill
  Then both receive identical skill content
  And both produce equivalent quality output
```

### US-05: Agent Renaming for Compatibility

```gherkin
Feature: Agent Naming Convention Compliance

Scenario: Rename all agents to OpenCode-compatible format
  Given the repository has 46 agents with underscore-based names
  And OpenCode requires names matching [a-z0-9-]+ (no underscores)
  When the renaming script is executed
  Then all 46 agent files are renamed from __ to - format
  And agent frontmatter name fields are updated
  And workflow agent references are updated
  And all agents load successfully in OpenCode

Scenario: Validate agent naming after rename
  Given all 46 agents have been renamed
  When a validation script checks agent names
  Then no agent names contain underscores
  And no agent names contain consecutive hyphens
  And all agent names match [a-z0-9]+(-[a-z0-9]+)* pattern
  And all agents are discoverable by both tools
```

## Functional Requirements

### FR-01: Configuration Files

| Requirement | Description                                              | Priority |
| ----------- | -------------------------------------------------------- | -------- |
| FR-01.1     | Create `opencode.json` with GLM-4.7 model (zai provider) | Must     |
| FR-01.2     | Configure GLM-4.5-air as small/fast model                | Should   |
| FR-01.3     | Configure MCP servers in opencode.json format            | Must     |
| FR-01.4     | Set tool permissions matching Claude Code defaults       | Should   |
| FR-01.5     | Add schema reference for IDE autocomplete                | Should   |

### FR-02: Instructions File

| Requirement | Description                                   | Priority |
| ----------- | --------------------------------------------- | -------- |
| FR-02.1     | Create `AGENTS.md` at repository root         | Must     |
| FR-02.2     | Include essential project conventions         | Must     |
| FR-02.3     | Reference CLAUDE.md for comprehensive details | Should   |
| FR-02.4     | Keep AGENTS.md focused and concise (<10KB)    | Should   |

### FR-03: Agent Translation

| Requirement | Description                                       | Priority |
| ----------- | ------------------------------------------------- | -------- |
| FR-03.1     | Create `.opencode/agent/` directory               | Should   |
| FR-03.2     | Translate high-priority agents to OpenCode format | Could    |
| FR-03.3     | Document agent format differences                 | Should   |
| FR-03.4     | Create agent creation guide for OpenCode          | Could    |

### FR-04: Skills and Agents Validation

| Requirement | Description                                        | Priority |
| ----------- | -------------------------------------------------- | -------- |
| FR-04.1     | Verify all 19 skills load in OpenCode              | Must     |
| FR-04.2     | Test skill invocation works correctly              | Must     |
| FR-04.3     | Document any skill compatibility issues            | Should   |
| FR-04.4     | Fix skill naming (rename 19 skills from \_\_ to -) | Must     |
| FR-04.5     | Verify all 46 agents load in OpenCode              | Must     |
| FR-04.6     | Test agent invocation works correctly              | Must     |
| FR-04.7     | Fix agent naming (rename 46 agents from \_\_ to -) | Must     |
| FR-04.8     | Update workflow agent references after renaming    | Must     |

## Non-Functional Requirements

### NFR-01: Maintainability

- Configurations should be easy to update
- Avoid duplicate content between CLAUDE.md and AGENTS.md
- Use references and links where possible

### NFR-02: Documentation

- Document which features are tool-specific
- Provide clear setup instructions for OpenCode users
- Explain the compatibility strategy in README

### NFR-03: Consistency

- Same project conventions apply regardless of tool
- Output quality should be indistinguishable
- Both tools should enforce identical standards

## Acceptance Criteria

### AC-01: OpenCode Initialization

```gherkin
Scenario: OpenCode starts successfully
  Given opencode.json exists with valid GLM-4.7 configuration
  And AGENTS.md exists with project instructions
  When the developer runs `opencode` command
  Then OpenCode initializes without errors
  And model is set to zai/glm-4.7 (cost-optimized)
  And MCP servers are listed as available
```

### AC-02: Skills and Agents Discovery

```gherkin
Scenario: All skills are discoverable
  Given 19 skills exist in .claude/skills/
  When OpenCode lists available skills
  Then all 19 skills appear in the list
  And skill descriptions match SKILL.md content
  And skills can be invoked successfully

Scenario: All agents are discoverable
  Given 46 agents exist in .claude/agents/
  And all agents have been renamed to hyphen format
  When OpenCode lists available agents
  Then all 46 agents appear in the list
  And agent descriptions match frontmatter content
  And agents can be invoked successfully
```

### AC-03: MCP Server Connection

```gherkin
Scenario: MCP servers connect
  Given opencode.json has mcp configuration
  When OpenCode starts
  Then Playwright MCP server connects
  And Context7 MCP server connects
  And browser automation tools are available
```

### AC-04: Claude Code Unaffected

```gherkin
Scenario: Claude Code still works
  Given OpenCode configuration has been added
  And all skills and agents have been renamed
  When the developer runs `claude` command
  Then Claude Code works exactly as before
  And all agents load correctly with new names
  And all skills function properly with new names
  And MCP servers connect successfully
```

### AC-05: Naming Convention Compliance

```gherkin
Scenario: All names are OpenCode-compatible
  Given renaming scripts have been executed
  When validation checks all skill and agent names
  Then no skill names contain underscores
  And no agent names contain underscores
  And no names contain consecutive hyphens
  And all names match pattern [a-z0-9]+(-[a-z0-9]+)*
  And both tools discover all 65 files (19 skills + 46 agents)

Scenario: Workflow references updated
  Given all agents have been renamed
  When workflow files reference agents
  Then all workflow agent references use new hyphenated names
  And workflows execute successfully in Claude Code
  And workflows execute successfully in OpenCode
```

## Constraints

### Technical Constraints

1. **CRITICAL - Skill Naming Incompatibility**:
   - **Both Claude Code AND OpenCode require**: `[a-z0-9-]+` (lowercase alphanumeric with hyphens ONLY)
   - **Current skill names use underscores**: `docs__applying-content-quality`
   - **Underscores (`_`) are NOT allowed** in either tool's spec
   - **Consecutive hyphens (`--`) are NOT allowed** in either tool's spec
   - **ALL 19 skills MUST be renamed** before OpenCode compatibility
   - **Correct fix**: Replace `__` with **SINGLE hyphen** `-`
   - Example: `docs__applying-content-quality` → `docs-applying-content-quality` (NOT `docs--applying-content-quality`)

2. **Agent Format**: OpenCode agents use different frontmatter schema
   - Cannot directly reuse Claude agent files
   - Must translate or create OpenCode-specific agents

3. **MCP Format**: Different JSON structures
   - Claude: `mcpServers.name.command` (string) + `args` (array)
   - OpenCode: `mcp.name.type` + `command` (array including args)

### Business Constraints

1. **No Disruption**: Existing Claude Code workflows must remain functional
2. **Team Choice**: Both tools should be equally viable options
3. **Maintenance Burden**: Minimize ongoing dual-tool maintenance effort

## Dependencies

### External Dependencies

- OpenCode CLI installed (`npm i -g @opencode/cli` or via releases)
- Node.js runtime for MCP servers
- MCP server packages (Playwright, Context7)

### Internal Dependencies

- Existing `.claude/` directory structure
- Current CLAUDE.md content
- Existing skill definitions

## Risks

### Risk-01: Skill Naming - CONFIRMED ISSUE

**Risk**: CONFIRMED - Both Claude Code AND OpenCode reject underscores in skill names
**Status**: Not a risk - this is a KNOWN REQUIREMENT
**Action Required**: Rename all 19 skills from `domain__skill-name` to `domain-skill-name`
**Impact**: High (requires renaming 19 skills, updating all references in agents and docs)

### Risk-02: Agent Translation Effort

**Risk**: Translating 40+ agents is significant effort
**Mitigation**: Start with subset; document format; consider automation
**Impact**: Low (agents are optional enhancement)

### Risk-03: Configuration Drift

**Risk**: Configurations may diverge over time
**Mitigation**: Document update procedures; use wow\_\_rules-checker
**Impact**: Medium (could cause inconsistent behavior)

### Risk-04: MCP Server Conflicts

**Risk**: Both tools might try to spawn same MCP server
**Mitigation**: Use different ports or process management
**Impact**: Low (unlikely concurrent use)
