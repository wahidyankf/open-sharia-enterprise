# Requirements: Agent and Skill Definitions as Documentation Source of Truth

## User Stories

### US-1: Agent Definition Management

**As an** AI agent developer
**I want** to define agents in a tool-agnostic format in `governance/agents/`
**So that** I can maintain agent definitions independently of CLI tool formats

**Acceptance Criteria**: See [AC-1](#ac-1-create-new-agent-definition)

---

### US-2: Skill Definition Management

**As a** documentation maintainer
**I want** to define skills in `governance/agents/skills/` with tool-agnostic format
**So that** skill knowledge packages work with both Claude Code and OpenCode CLI tools

**Acceptance Criteria**: See [AC-2](#ac-2-create-new-skill-definition)

---

### US-3: Automated Synchronization

**As a** repository maintainer
**I want** to sync tool-agnostic definitions to Claude Code and OpenCode formats automatically
**So that** both CLI tools always have up-to-date agent and skill implementations

**Acceptance Criteria**: See [AC-3](#ac-3-sync-definitions-to-tool-formats)

---

### US-4: Format Validation

**As a** quality assurance engineer
**I want** to validate both source definitions and generated formats
**So that** I can catch errors before committing changes

**Acceptance Criteria**: See [AC-4](#ac-4-validate-all-formats)

---

### US-5: Safe Editing Workflow

**As a** contributor
**I want** to be prevented from editing generated directories
**So that** I don't accidentally create drift between source and generated files

**Acceptance Criteria**: See [AC-5](#ac-5-prevent-direct-edits-to-generated-files)

---

### US-6: Migration from Current Architecture

**As a** repository maintainer
**I want** to migrate 45 existing agents and 23 skills to the new architecture
**So that** we preserve all existing functionality while moving to tool-agnostic format

**Acceptance Criteria**: See [AC-6](#ac-6-migrate-existing-agents-and-skills)

---

### US-7: Future Tool Support

**As a** platform engineer
**I want** to add new CLI tool format support by extending the sync script
**So that** agent definitions work with emerging AI coding tools

**Acceptance Criteria**: See [AC-7](#ac-7-add-new-tool-format-support)

---

### US-8: Update Meta-Agents

**As a** repository maintainer
**I want** meta-agents (agent-maker, wow-governance-checker, wow-governance-fixer) to respect new source locations
**So that** agents create/validate/fix definitions in docs source, not generated directories

**Acceptance Criteria**: See [AC-8](#ac-8-update-meta-agents-for-new-architecture)

---

## Acceptance Criteria (Gherkin Format)

### AC-1: Create New Agent Definition

```gherkin
Feature: Agent Definition Creation
  As an AI agent developer
  I want to create agent definitions in tool-agnostic format
  So that agents work with all supported CLI tools

  Background:
    Given I am in the repository root directory
    And the sync script is available at "scripts/sync-docs-to-agents.py"

  Scenario: Create new agent definition
    Given I create a new file "governance/agents/content/my-new-agent.md"
    And the file contains valid frontmatter with:
      | field       | value                          |
      | name        | my-new-agent                   |
      | description | Performs specific task         |
      | role        | writer                         |
      | model       | sonnet                         |
      | tools       | [Read, Write, Edit]            |
      | skills      | [skill-name-1, skill-name-2]   |
      | mode        | all                            |
    And the file contains agent instructions in markdown body
    When I run "python scripts/sync-docs-to-agents.py"
    Then a Claude Code format file is created at ".claude/agents/my-new-agent.md"
    And the Claude Code file contains frontmatter field "color" with value "blue"
    And the Claude Code file contains frontmatter field "tools" as array
    And the Claude Code file contains frontmatter field "skills" as array
    And an OpenCode format file is created at ".opencode/agent/my-new-agent.md"
    And the OpenCode file contains frontmatter field "tools" as boolean object
    And the OpenCode file contains "permission.skill" as dictionary
    And the OpenCode file embeds role in body "**Role**: Writer (blue)"
    And both generated files contain identical agent instructions in body

  Scenario: Validate agent definition format
    Given I have a file "governance/agents/content/test-agent.md"
    When I run "python scripts/validate-agent-definitions.py"
    Then validation passes with 0 errors
    And validation confirms all required fields present:
      | field       |
      | name        |
      | description |
      | role        |
      | model       |
      | tools       |
      | mode        |

  Scenario: Agent name matches filename
    Given I create "governance/agents/content/example-agent.md"
    And the frontmatter contains "name: different-name"
    When I run "python scripts/validate-agent-definitions.py"
    Then validation fails with error "Agent name must match filename (without .md)"
```

---

### AC-2: Create New Skill Definition

```gherkin
Feature: Skill Definition Creation
  As a documentation maintainer
  I want to create skill definitions in tool-agnostic format
  So that skills work with all CLI tools

  Background:
    Given I am in the repository root directory
    And the sync script is available

  Scenario: Create new skill definition
    Given I create directory "governance/agents/skills/example-new-skill/"
    And I create file "governance/agents/skills/example-new-skill/SKILL.md"
    And the file contains skill frontmatter with:
      | field       | value                             |
      | name        | example-new-skill                 |
      | description | Provides guidance on example task |
    And the file contains skill content in markdown body
    When I run "butler-cli skills sync"
    Then a directory is created at ".claude/skills/example-new-skill/"
    And a file is created at ".claude/skills/example-new-skill/SKILL.md"
    And a directory is created at ".opencode/skills/example-new-skill/"
    And a file is created at ".opencode/skills/example-new-skill/SKILL.md"
    And both skill files contain identical content from source

   Scenario: Skill uses kebab-case naming in name field
     Given I create directory "governance/agents/skills/valid-skill-name/"
     And I create file "governance/agents/skills/valid-skill-name/SKILL.md"
     And frontmatter contains "name: valid-skill-name"
     When I run "butler-cli skills validate"
     Then validation passes with 0 errors

   Scenario: Skill name with invalid characters fails validation
     Given I create directory "governance/agents/skills/Invalid_Skill/"
     And I create file "governance/agents/skills/Invalid_Skill/SKILL.md"
     And frontmatter contains "name: Invalid_Skill"
     When I run "butler-cli skills validate"
     Then validation fails with error "Skill name must use kebab-case (lowercase with hyphens)"
     And validation suggests "Use format: category-skill-name (e.g., docs-applying-content-quality)"
```

---

### AC-3: Sync Definitions to Tool Formats

```gherkin
Feature: Automated Synchronization
  As a repository maintainer
  I want to sync definitions to all tool formats
  So that changes propagate correctly

  Background:
    Given I have valid agent definitions in "governance/agents/content/"
    And I have valid skill definitions in "governance/agents/skills/"

    Scenario: Full sync operation
      Given I modify "governance/agents/content/docs-maker.md"
      And I update the description field
      When I run "butler-cli agents sync && butler-cli skills sync"
      Then the command processes all agent definitions
      And the command processes all skill definitions
      And the command reports sync statistics:
        | metric                     | value |
        | agents_synced              | 45    |
        | skills_synced              | 23    |
        | claude_agents_written      | 45    |
        | opencode_agents_written     | 45    |
        | claude_skills_written       | 23    |
        | opencode_skills_written     | 23    |
      And validation runs automatically after sync
      And validation passes with 0 errors

   Scenario: Incremental sync (single agent)
     Given I modify only "governance/agents/content/docs-checker.md"
     When I run "butler-cli agents sync --agent docs-checker"
     Then only "docs-checker" is synced to both formats
     And other agents are not touched
     And modification timestamps are preserved for unchanged agents

    Scenario: Sync with validation failure
      Given I have an invalid agent definition with missing "description" field
      When I run "butler-cli agents sync"
      Then sync validation fails before writing files
      And error message indicates "Missing required field: description"
      And no files are written to ".claude/agents/", ".opencode/agent/", ".claude/skills/", or ".opencode/skills/"
      And exit code is 1

  Scenario: Role to color mapping
    Given I have agent definition with "role: writer"
    When I run sync script
    Then Claude Code format contains "color: blue"
    And OpenCode format contains "**Role**: Writer (blue)" in body

    Given I have agent definition with "role: checker"
    When I run sync script
    Then Claude Code format contains "color: green"
    And OpenCode format contains "**Role**: Checker (green)" in body
```

---

### AC-4: Validate All Formats

```gherkin
Feature: Format Validation
  As a quality assurance engineer
  I want to validate all definition formats
  So that errors are caught early

   Scenario: Validate source definitions
     Given I have agent definitions in "governance/agents/content/"
     When I run "butler-cli agents validate"
     Then validation checks all required fields present
     And validation checks name matches filename
     And validation checks role is valid (writer, checker, updater, implementor, specialist)
     And validation checks tools are valid (Read, Write, Edit, MultiEdit, Glob, Grep, Bash, LS, WebFetch, WebSearch, TodoRead, TodoWrite, NotebookRead, NotebookEdit)
     And validation checks skills reference existing skill files
     And validation checks mode is valid (all, subagent, primary)
     And validation reports pass/fail for each agent

   Scenario: Validate generated Claude Code format
     Given sync has generated files in ".claude/agents/"
     When I run "butler-cli agents validate --format claude"
     Then validation checks frontmatter fields:
       | field       | requirement                  |
       | name        | matches filename             |
       | description | not empty                    |
       | tools       | array format                 |
       | model       | valid (inherit, sonnet, etc.)|
       | color       | valid (blue, green, etc.)    |
       | skills      | array format                 |
     And validation reports 0 errors for all 45 agents

    Scenario: Validate generated OpenCode format
      Given sync has generated files in ".opencode/agent/"
      When I run "butler-cli agents validate --format opencode"
      Then validation checks frontmatter fields:
        | field       | requirement                     |
        | description | not empty                       |
        | mode        | valid (all, subagent, primary)  |
        | tools       | boolean object with lowercase keys |
        | permission  | valid deny/allow/ask actions    |
      And validation checks "permission.skill" is dictionary
      And validation checks tool names are lowercase
      And validation reports 0 errors for all 45 agents
      And validation checks ".opencode/skills/" has 23 skill directories with SKILL.md files
      And validation reports 0 errors for all 23 skills

   Scenario: Cross-format consistency validation
     Given I have synced agent "docs-maker"
     When I run "butler-cli agents validate --cross-format"
     Then validation confirms both formats have identical:
       | aspect              |
       | agent instructions body |
       | tool access permissions |
       | skill references        |
       | model selection         |
     And validation reports 0 inconsistencies
```

---

### AC-5: Prevent Direct Edits to Generated Files

```gherkin
Feature: Safe Editing Workflow
  As a contributor
  I want to be prevented from editing generated files
  So that source and generated files stay in sync

  Scenario: Pre-commit hook warns on generated file changes
    Given I modify ".claude/agents/docs-maker.md" directly
    When I run "git add .claude/agents/docs-maker.md"
    And I run "git commit -m 'Update agent'"
    Then pre-commit hook displays warning:
      """
      ⚠️ WARNING: You are committing changes to generated directories.

      Modified generated files:
        - .claude/agents/docs-maker.md

       Source of truth locations:
       - Agents: governance/agents/content/
       - Skills: governance/agents/skills/

       To make changes:
       1. Edit source files in governance/agents/
       2. Run: butler-cli agents sync && butler-cli skills sync
       3. Commit both source and generated files

       To bypass this check (NOT recommended):
         git commit --no-verify
       """
    And commit is blocked (exit code 1)

  Scenario: README banner in generated directories
     Given sync has completed successfully
     When I view ".claude/agents/README.md"
     Then the file contains banner at top:
       """
       # ⚠️ DO NOT EDIT - GENERATED FILES

        **Source of truth**: `governance/agents/content/`

        Files in this directory are automatically generated by `butler-cli agents sync`.

        To modify agents:
        1. Edit source files in `governance/agents/content/`
        2. Run sync command: `butler-cli agents sync && butler-cli skills sync`
        3. Commit both source and generated files together

        Direct edits to this directory will be overwritten on next sync.
        """

  Scenario: Documentation update
     Given sync architecture is implemented
     When I view "CLAUDE.md"
     Then it contains section about agent source location:
       """
        ## AI Agents

        Agent definitions are maintained in `governance/agents/content/`
        and synced to tool-specific formats:
        - `.claude/agents/` - Claude Code format (generated)
        - `.opencode/agent/` - OpenCode format (generated)

        Skill definitions are maintained in `governance/agents/skills/`
        and synced to tool-specific formats:
        - `.claude/skills/` - Claude Code format (generated)
        - `.opencode/skills/` - OpenCode format (generated)

        To modify agents, edit source files and run: `butler-cli agents sync && butler-cli skills sync`
        """
```

---

### AC-6: Migrate Existing Agents and Skills

```gherkin
Feature: Migration from Current Architecture
  As a repository maintainer
  I want to migrate existing agents and skills
  So that we preserve functionality while adopting new architecture

  Background:
    Given I have 45 agents in ".claude/agents/"
    And I have 23 skills in ".claude/skills/"

   Scenario: Extract agents to docs format
     Given current agents use Claude Code format
     When I run "butler-cli agents extract"
     Then 45 files are created in "governance/agents/content/"
     And each file has tool-agnostic frontmatter:
       | field       | mapping                           |
       | name        | from original name field          |
       | description | from original description field   |
       | role        | from color (blue→writer, etc.)    |
       | model       | from original model field         |
       | tools       | from original tools array         |
       | skills      | from original skills array        |
       | mode        | all (or subagent for specific agents) |
     And each file has original agent body content
     And extraction command reports statistics:
       | metric           | value |
       | agents_extracted | 45    |
       | skills_extracted | 23    |
       | errors           | 0     |

  Scenario: Preserve git metadata
    Given agent "docs-maker.md" has git history
    And first commit was "2025-11-29T10:30:00+07:00"
    And last commit was "2026-01-03T15:45:00+07:00"
    When I run extraction script
    Then extracted file metadata section contains:
      """
      ## Agent Metadata

      - **Role**: Writer (blue)
      - **Created**: 2025-11-29 (from git)
      - **Last Updated**: 2026-01-03 (from git)
      """

  Scenario: Preserve skill directory structure
    Given skill exists at ".claude/skills/docs-applying-content-quality/SKILL.md"
    When I run extraction script
    Then skill directory is created at "governance/agents/skills/docs-applying-content-quality/"
    And skill file is created at "governance/agents/skills/docs-applying-content-quality/SKILL.md"
    And directory structure is preserved (folder/SKILL.md format)
    And skill content is identical

   Scenario: Verify functional equivalence after migration
     Given extraction and sync are complete
     When I run comprehensive agent tests in Claude Code
     Then all 45 agents function identically to pre-migration behavior
     When I run comprehensive agent tests in OpenCode
     Then all 45 agents function identically to pre-migration behavior
     And 0 validation errors in either format
     And all 23 skills load correctly from ".claude/skills/" in Claude Code
     And all 23 skills load correctly from ".opencode/skills/" in OpenCode
```

---

### AC-7: Add New Tool Format Support

````gherkin
Feature: Future Tool Format Support
  As a platform engineer
  I want to add new CLI tool formats
  So that agents work with emerging tools

  Scenario: Add hypothetical "AgentX" tool format
    Given a new CLI tool "AgentX" requires format ".agentx/agents/"
    And AgentX uses JSON frontmatter instead of YAML
    When I extend "scripts/sync-docs-to-agents.py" with AgentX generator
    Then sync script processes "governance/agents/content/*.md"
    And generates AgentX format at ".agentx/agents/"
    And AgentX files have JSON frontmatter:
      ```json
      {
        "name": "docs-maker",
        "description": "...",
        "tools": ["read", "write", "edit"]
      }
      ```
    And AgentX files have markdown body with agent instructions
    And sync continues to generate Claude Code and OpenCode formats
    And all three formats remain in sync

  Scenario: Validate new format
    Given AgentX format generator is added
    When I create "scripts/validate-agentx-agents.py"
    Then validation script checks AgentX-specific requirements
    And validation integrates with main sync workflow
    And sync fails if any format validation fails
````

---

### AC-8: Update Meta-Agents for New Architecture

````gherkin
Feature: Meta-Agent Updates
  As a repository maintainer
  I want meta-agents to respect new source locations
  So that agent creation/validation/fixing works with docs source

  Background:
    Given agent definitions are in "governance/agents/content/"
    And skill definitions are in "governance/agents/skills/"
    And ".claude/agents/" is generated (DO NOT EDIT)
    And ".opencode/agent/" is generated (DO NOT EDIT)
    And ".claude/skills/" is generated (DO NOT EDIT)

  Scenario: agent-maker creates new agent in docs source
    Given I invoke "agent-maker" agent
    And I request creation of new agent "test-validator"
    When agent-maker executes
     Then agent-maker creates "governance/agents/content/test-validator.md"
     And file uses tool-agnostic format:
       | field       | value                        |
       | name        | test-validator               |
       | description | Validates test files         |
       | role        | checker                      |
       | model       | sonnet                       |
       | tools       | [Read, Grep, Glob]           |
       | mode        | all                          |
     And agent-maker does NOT create ".claude/agents/test-validator.md"
     And agent-maker instructs user to run sync command
     And agent-maker provides command: "butler-cli agents sync"

  Scenario: agent-maker uses tool-agnostic format (role not color)
    Given I invoke "agent-maker" to create "docs-updater"
    And I specify it should be an updater/modifier agent
    When agent-maker generates frontmatter
    Then frontmatter contains "role: updater"
    And frontmatter does NOT contain "color: yellow"
    And frontmatter contains tool-agnostic model values (sonnet, haiku, opus)
    And frontmatter contains capitalized tool names (Read, Write, Edit)

   Scenario: wow-governance-checker validates docs source (not generated)
     Given I invoke "wow-governance-checker" for agent validation
     When wow-governance-checker executes
     Then it validates files in "governance/agents/content/"
     And it validates files in "governance/agents/skills/"
     And it does NOT validate ".claude/agents/" (generated)
     And it does NOT validate ".opencode/agent/" (generated)
     And it does NOT validate ".claude/skills/" (generated)
     And it does NOT validate ".opencode/skills/" (generated)
     And validation checks tool-agnostic format requirements:
       | check                    | description                       |
       | name matches filename    | agent-name.md has name: agent-name |
       | role is valid            | writer, checker, updater, etc.    |
       | tools are capitalized    | Read, Write (not read, write)     |
       | skills exist in docs     | governance/agents/skills/ |

   Scenario: wow-governance-checker detects edits to generated directories
     Given ".claude/agents/docs-maker.md" was modified
     When wow-governance-checker executes
     Then validation fails with error "Generated file modified"
     And error message indicates:
       """
       ❌ Generated file should not be edited directly:
         - .claude/agents/docs-maker.md

       Source of truth: governance/agents/content/docs-maker.md

        To fix:
        1. Revert changes to .claude/agents/docs-maker.md
        2. Edit governance/agents/content/docs-maker.md
        3. Run: butler-cli agents sync && butler-cli skills sync
        """

   Scenario: wow-governance-fixer does NOT modify generated directories
     Given wow-governance-checker generated audit report
     And report contains finding: "Fix description in docs-maker"
     And finding references "governance/agents/content/docs-maker.md"
     When I invoke "wow-governance-fixer" with audit report
     Then wow-governance-fixer reads finding
     And wow-governance-fixer modifies "governance/agents/content/docs-maker.md"
     And wow-governance-fixer does NOT modify ".claude/agents/docs-maker.md"
     And wow-governance-fixer does NOT modify ".opencode/agent/docs-maker.md"
     And wow-governance-fixer does NOT modify ".claude/skills/" (if skill referenced)
     And wow-governance-fixer does NOT modify ".opencode/skills/" (if skill referenced)
     And wow-governance-fixer instructs user to run sync after fix

  Scenario: wow-governance-fixer skips findings referencing generated files
    Given wow-governance-checker audit report contains finding
    And finding references ".claude/agents/test-agent.md" (generated file)
    When I invoke "wow-governance-fixer"
    Then wow-governance-fixer skips this finding
    And fixer logs warning: "Finding references generated file - skipping"
    And fixer suggests: "Edit source: governance/agents/content/test-agent.md"

   Scenario: Update agent-maker instructions for sync workflow
     Given I read "governance/agents/content/agent-maker.md"
     When I review agent instructions
     Then instructions include:
       """
       ## Agent Creation Workflow

       1. Create agent definition in `governance/agents/content/{agent-name}.md`
       2. Use tool-agnostic format:
          - `role` (not `color`)
          - Capitalized tool names (Read, Write)
          - Model: sonnet, haiku, opus, inherit
        3. Instruct user to sync:
           ```bash
           butler-cli agents sync && butler-cli skills sync
           ```
        4. Verify both formats generated:
           - .claude/agents/{agent-name}.md
           - .opencode/agent/{agent-name}.md
        5. Skills sync to both locations:
           - .claude/skills/{skill-name}/SKILL.md
           - .opencode/skills/{skill-name}/SKILL.md
        """
      And instructions warn NOT to create in ".claude/agents/" or ".claude/skills/" directly
````

---

## Non-Functional Requirements

### NFR-1: Performance

```gherkin
Scenario: Sync completes within acceptable time
  Given I have 45 agents and 23 skills to sync
  When I run full sync operation
  Then sync completes within 30 seconds
  And validation completes within 60 seconds
```

### NFR-2: Atomicity

```gherkin
Scenario: Sync is atomic (all or nothing)
  Given sync processes multiple agents
  And one agent fails validation midway
  When sync encounters validation error
  Then no files are written to any generated directory
  And filesystem state is unchanged from before sync started
  And error message clearly indicates which agent failed
```

### NFR-3: Idempotency

```gherkin
Scenario: Running sync multiple times produces identical output
  Given I have synced once successfully
  And no source files have changed
  When I run sync again
  Then generated files are byte-for-byte identical to first run
  And timestamps in git metadata section are unchanged
  And no unnecessary file writes occur
```

### NFR-4: Error Messages

```gherkin
Scenario: Clear error messages for validation failures
  Given I have agent with invalid role "invalid-role"
  When I run sync script
  Then error message indicates:
    - Exact file path with error
    - Field name with problem
    - Invalid value provided
    - Valid values expected
    - Line number in source file (if applicable)
  Example output:
    """
    ERROR: governance/agents/content/test-agent.md
    Field: role
    Invalid value: "invalid-role"
    Expected one of: writer, checker, updater, implementor, specialist
    Line: 4
    """
```

---

## In Scope (Additions)

The following are **explicitly included** beyond the core migration:

1. **Meta-agent updates**: `agent-maker`, `wow-governance-checker`, `wow-governance-fixer` updated to work with new source locations
2. **Validation of meta-agent behavior**: Test that meta-agents create/validate/fix definitions in docs source, not generated directories
3. **Pre-commit hook**: Install hook to prevent direct edits to generated directories

## Out of Scope

The following are explicitly **not** included in this plan:

1. **Agent execution runtime changes**: Agents continue to execute via Claude Code and OpenCode CLIs unchanged
2. **Skill loading mechanism**: Skills continue to load from `.claude/skills/` (generated location)
3. **Workflow definitions**: Workflows remain in `governance/workflows/` (separate concern)
4. **AGENTS.md / CLAUDE.md content**: Delivery mechanism documents updated to reference new source locations, but core content unchanged
5. **Agent capabilities**: No changes to what agents can do, only where definitions are stored
6. **Tool CLI behavior**: No modifications to Claude Code or OpenCode CLI tools themselves
7. **Other agents**: Only meta-agents (`agent-maker`, `wow-governance-*`) updated, not all 45 agents
8. **Skill creation agent**: No new `skill-maker` agent created (can be future enhancement)

---

## Dependencies

### Internal Dependencies

- Butler CLI: `apps/butler-cli/` (Go + Cobra framework for agent/sync/validate)
- Existing scripts:
  - `scripts/convert-agents-to-opencode.py` (reference for format mapping)
  - `scripts/validate-opencode-agents.py` (validation patterns)
- Documentation:
  - `governance/development/agents/ai-agents.md` (agent conventions)
  - `docs/explanation/ex-ru__repository-governance-architecture.md` (governance model)
- Git hooks: `.husky/pre-commit` (integration point)

### External Dependencies

- Go 1.24+ with Cobra, go-git, yaml.v3
- Git (for metadata extraction)
- Claude Code CLI (for testing Claude Code format)
- OpenCode CLI (for testing OpenCode format)

---

## Constraints

1. **Backward compatibility**: Migration must preserve exact functional behavior of all 45 agents and 23 skills
2. **Atomic migration**: Cannot be done incrementally (source of truth must switch completely)
3. **Zero downtime**: Both CLI tools must remain functional throughout migration
4. **Git history**: Must preserve ability to trace agent evolution through git history
5. **Manual testing**: Comprehensive manual testing required (automated tests for agents don't exist yet)

---

## Assumptions

1. Tool-agnostic format will remain stable (won't require frequent schema changes)
2. Number of CLI tools requiring support remains small (2-3, not 10+)
3. Contributors will follow documented workflow (edit docs, not generated files)
4. Pre-commit hooks are enabled in contributor environments
5. Sync script execution is fast enough for regular use (<1 minute)
