---
title: "AI Agents Convention"
description: Standards for creating and managing AI agents in the .claude/agents/ directory
category: explanation
subcategory: development
tags:
  - ai-agents
  - conventions
  - claude-code
  - development
  - standards
created: 2025-11-23
updated: 2025-12-26
---

# AI Agents Convention

This document defines the standards for creating, structuring, and managing AI agents in the `.claude/agents/` directory. All agents must follow these conventions to ensure consistency, maintainability, and proper integration with the project.

## Overview

### What are AI Agents?

AI agents in this project are specialized Claude Code assistants defined in the `.claude/agents/` directory. Each agent has:

- **Specific expertise** in a particular domain or task
- **Defined tool permissions** limiting what operations it can perform
- **Clear responsibilities** to avoid overlap with other agents
- **Integration with project conventions** through references to CLAUDE.md and convention documents
- **Unlimited token budget** - Focus on execution quality, not token usage (reliable compaction mechanism handles context management)

### Why We Need Agent Conventions

Without standards, agents can become:

- **Inconsistent** in structure and quality
- **Overlapping** in responsibilities, causing confusion
- **Insecure** through tool permission creep
- **Unmaintainable** as the project grows

This convention ensures all agents are:

- ✅ Well-structured and documented
- ✅ Single-purpose and focused
- ✅ Secure through explicit tool permissions
- ✅ Consistent with project standards

### Scope

This convention applies to:

- All agent files in `.claude/agents/`
- References to agents in `CLAUDE.md`
- Agent validation rules in `wow__rules-checker`

## Principles Respected

**REQUIRED SECTION**: All development practice documents MUST include this section to ensure traceability from practices back to foundational values.

This practice respects the following core principles:

- **[Explicit Over Implicit](../principles/software-engineering/ex-pr-se__explicit-over-implicit.md)**: Agent tool permissions are explicitly whitelisted in frontmatter (not implicitly granted). Each agent declares exactly which tools it can access. Agent responsibilities are explicitly documented, not inferred. Frontmatter fields (name, description, tools, model, color) make agent capabilities transparent and discoverable through simple grep operations.

- **[Simplicity Over Complexity](../principles/general/ex-pr-ge__simplicity-over-complexity.md)**: Agents follow single-responsibility principle - one clear, focused purpose per agent. Flat directory structure (no subdirectories). Simple naming convention (kebab-case). Standard document structure across all agents. Rather than creating "Swiss Army knife" agents with dozens of capabilities, we create focused agents that do one thing well.

- **[Automation Over Manual](../principles/software-engineering/ex-pr-se__automation-over-manual.md)**: AI agents themselves embody this principle - they automate repetitive tasks (documentation writing, validation, consistency checking, deployment) that would otherwise require manual effort. The `agent__maker` automates agent creation. `wow__rules-checker` automates validation. Agents transform manual processes into repeatable, consistent automated workflows.

## Token Budget Philosophy

**CRITICAL GUIDELINE**: When invoking agents (`.claude/agents/`) and workflows (`docs/explanation/workflows/`), **do NOT think about token budget constraints**.

### Why Unlimited Budget Mindset

- **Reliable compaction mechanism**: Claude Code handles context management through proven compaction
- **Quality is king**: Execution thoroughness matters more than token efficiency
- **No artificial constraints**: Agents should complete their work properly, not cut corners to save tokens
- **Workflow orchestration**: Multi-step workflows naturally require more tokens - this is expected and acceptable

### What This Means in Practice

When working with agents:

- ✅ **Do**: Let agents complete comprehensive validations, generate detailed reports, iterate until quality gates pass
- ✅ **Do**: Use multi-step workflows (check-fix iterations) without worrying about token usage
- ✅ **Do**: Focus on correct, thorough execution
- ❌ **Don't**: Skip steps to save tokens
- ❌ **Don't**: Reduce validation scope due to token concerns
- ❌ **Don't**: Avoid iteration loops because of token budget fears

### Compaction Handles Scale

- Large repository audits → Compaction preserves key findings
- Multi-iteration workflows → Compaction maintains essential state
- Comprehensive reports → Written to files, conversation can be compacted
- Long validation runs → Progressive writing ensures nothing is lost

**Bottom line**: Trust the compaction mechanism. Execute quality work. Token budget is effectively unlimited for agent and workflow operations.

## Conventions Implemented/Respected

**REQUIRED SECTION**: All development practice documents MUST include this section to ensure traceability from practices to documentation standards.

This practice implements/respects the following conventions:

- **[File Naming Convention](../conventions/meta/ex-co-me__file-naming.md)**: Agents follow kebab-case naming pattern (`agent-name.md`). Agent names must match frontmatter `name` field.

- **[Linking Convention](../conventions/formatting/ex-co-fo__linking.md)**: All references to conventions and other documents use relative paths with `.md` extension. Ensures GitHub-compatible markdown across all agent files.

- **[Emoji Usage Convention](../conventions/formatting/ex-co-fo__emoji.md)**: Agent prompt files MUST NOT contain emojis (forbidden location per convention). Only `.claude/agents/README.md` uses colored square emojis for categorization.

- **[Color Accessibility Convention](../conventions/formatting/ex-co-fo__color-accessibility.md)**: Agent color categorization (blue/green/yellow/purple) uses verified accessible palette for visual identification while maintaining text-based accessibility.

- **[Timestamp Format Convention](../conventions/formatting/ex-co-fo__timestamp.md)**: Report-generating agents use UTC+7 timestamps in 4-part filename pattern `{agent-family}__{uuid-chain}__{YYYY-MM-DD--HH-MM}__audit.md`.

## Agent File Structure

### Required Frontmatter

Every agent file MUST begin with YAML frontmatter containing six required fields:

```yaml
---
name: agent-name
description: Expert in X specializing in Y. Use when Z.
tools: Read, Glob, Grep
model: inherit
color: blue
skills: []
---
```

**Field Order**: Fields MUST appear in this exact order (name, description, tools, model, color, skills) for consistency and grep-ability across all agents.

**NO Comments in Frontmatter**: Agent frontmatter MUST NOT contain inline comments (# symbols in YAML). Research shows Claude Code has frontmatter parsing issues (GitHub issue #6377), and best practice for configuration files is to keep YAML clean without inline comments. Put explanations in the document body below the frontmatter code block, not as inline comments.

**Field Definitions:**

1. **`name`** (required)
   - MUST exactly match the filename (without `.md` extension)
   - Use kebab-case format
   - Should be descriptive and action-oriented
   - Examples: `doc-writer`, `wow__rules-checker`, `api-validator`

2. **`description`** (required)
   - One-line summary of when to use this agent
   - Should complete: "Use this agent when..."
   - Be specific about the agent's expertise
   - Example: `"Expert documentation writer specializing in Obsidian-optimized markdown and Diátaxis framework. Use when creating, editing, or organizing project documentation."`

3. **`tools`** (required)
   - Comma-separated list of allowed tool names
   - Explicit whitelist for security and clarity
   - Only include tools the agent needs
   - Common tools: `Read`, `Write`, `Edit`, `Glob`, `Grep`, `Bash`

4. **`model`** (required)
   - Specifies which model to use for this agent
   - Options: `inherit` (default), `haiku`, `sonnet`, or `opus`
   - Use `inherit` unless there's a specific need for a particular model
   - See "Model Selection Guidelines" below for decision criteria

5. **`color`** (required)
   - Visual categorization based on agent role
   - Options: `blue` (writers), `green` (checkers), `yellow` (updaters), `purple` (implementors)
   - Helps users quickly identify agent type
   - See "Agent Color Categorization" below for assignment guidelines

6. **`skills`** (required)
   - List of Skill names the agent references (from `.claude/skills/`)
   - Can be empty array `[]` if agent doesn't use Skills
   - Skills auto-load when agent is invoked (if task matches Skill description)
   - Enables composability and explicit knowledge dependencies
   - Example: `skills: [color-accessibility-diagrams, maker-checker-fixer-pattern]`
   - See "Agent Skills References" section below for complete details

### Optional Frontmatter Fields

In addition to the six required fields, agents may include optional metadata fields for tracking:

7. **`created`** (optional)
   - Date when the agent was first created
   - Format: `YYYY-MM-DD` (ISO 8601 date only)
   - Example: `created: 2025-11-23`
   - Helps track agent age and history

8. **`updated`** (optional)
   - Date when the agent was last modified
   - Format: `YYYY-MM-DD` (ISO 8601 date only)
   - Example: `updated: 2025-12-03`
   - Automatically updated when significant changes are made
   - Helps identify stale or recently maintained agents

**Best Practices:**

- Use both `created` and `updated` fields together for complete tracking
- Update the `updated` field whenever making substantial changes to the agent
- Use consistent date format (YYYY-MM-DD) matching the project's [Timestamp Format Convention](../conventions/formatting/ex-co-fo__timestamp.md) (date-only format)
- Place these fields after the six required fields in frontmatter
- These fields align with documentation frontmatter best practices from Hugo, Jekyll, and Front Matter CMS

**Example with optional fields:**

```yaml
---
name: agent-name
description: Expert in X specializing in Y. Use when Z.
tools: Read, Glob, Grep
model: inherit
color: blue
skills: []
created: 2025-11-23
updated: 2025-12-03
---
```

### Agent Skills References

**REQUIRED FIELD**: All agents MUST include a `skills:` frontmatter field for composability and consistency.

**Purpose:** The `skills:` field declares which Skills (knowledge packages in `.claude/skills/`) the agent leverages. This enables:

- **Composability**: Explicit declarations of knowledge dependencies
- **Consistency**: All agents follow same structure (no special cases)
- **Discoverability**: Easy to see which agents use which Skills
- **Validation**: Checkers can enforce field presence and validate references

#### Skills Field Format

The `skills` field (already defined as field 6 in Required Frontmatter above) has the following detailed characteristics:

- **Format**: YAML array of strings
- **Required**: Yes (can be empty `[]`)
- **Values**: Skill names matching folder names in `.claude/skills/`
- **Auto-loading**: Skills load when agent invoked AND task matches Skill description
- **Validation**: Referenced Skills must exist in `.claude/skills/` directory
- **Example**: `skills: [color-accessibility-diagrams, maker-checker-fixer-pattern]`

#### When to Reference Skills vs. Inline Knowledge

**Use Skills references when:**

- ✅ Knowledge is specialized and deep (e.g., accessible color palettes, Gherkin syntax)
- ✅ Knowledge is shared across multiple agents (e.g., Maker-Checker-Fixer pattern)
- ✅ Knowledge requires progressive disclosure (overview at startup, details on-demand)
- ✅ Knowledge is frequently updated (Skills centralize updates)
- ✅ Knowledge has multiple aspects (Skill can have reference.md, examples.md)

**Use inline knowledge when:**

- ✅ Knowledge is agent-specific and not shared
- ✅ Knowledge is simple and fits in a few paragraphs
- ✅ Knowledge is critical for agent's core operation (always needed)
- ✅ Knowledge is stable and rarely changes

#### Skills Field Examples

**Agent using Skills:**

```yaml
---
name: docs__maker
description: Expert documentation writer specializing in Obsidian-optimized markdown and Diátaxis framework.
tools: Read, Write, Edit, Glob, Grep
model: inherit
color: blue
skills:
  - color-accessibility-diagrams
  - maker-checker-fixer-pattern
---
```

**Agent not using Skills:**

```yaml
---
name: simple__helper
description: Simple helper agent for basic tasks.
tools: Read
model: haiku
color: green
skills: []
---
```

#### Skills Composition Pattern

Agents can reference multiple Skills that work together:

```yaml
---
name: apps__ayokoding-web__general-maker
description: Expert at creating general Hugo content for ayokoding-web (Hextra theme).
tools: Read, Write, Edit, Glob, Grep
model: sonnet
color: blue
skills:
  - hugo-ayokoding-development
  - color-accessibility-diagrams
  - factual-validation-methodology
---
```

When this agent is invoked, all three Skills auto-load if the task description matches their triggers. Skills compose seamlessly to provide comprehensive knowledge.

#### Best Practices for Skills References

1. **Minimal set**: Reference only Skills the agent actually uses
2. **Relevant Skills**: Skills should align with agent's domain
3. **Order by importance**: List most critical Skills first
4. **Keep updated**: Add/remove Skills as agent evolves
5. **Validate references**: Ensure referenced Skills exist in `.claude/skills/`

#### Skills vs. Direct Convention References

Agents can use both Skills AND direct links to convention documents:

- **Skills**: For progressive disclosure and shared knowledge (auto-loaded)
- **Direct links**: For specific, targeted guidance (always in Reference Documentation section)

**Example combining both:**

```yaml
---
name: docs__checker
description: Validates documentation quality and factual correctness.
tools: Read, Glob, Grep, Write, Bash
model: sonnet
color: green
skills:
  - maker-checker-fixer-pattern
  - criticality-confidence-system
---
```

**Reference Documentation section:**

```markdown
## Reference Documentation

**Skills**: This agent uses `maker-checker-fixer-pattern` and `criticality-confidence-system` Skills for validation workflows.

**Conventions:**

- `docs/explanation/conventions/content/ex-co-co__quality.md` - Content Quality Principles
- `docs/explanation/conventions/formatting/ex-co-fo__linking.md` - Linking Convention
```

This pattern provides both auto-loaded knowledge (Skills) and explicit references for specific requirements.

See [Skills README](../../../.claude/skills/README.md) for complete details on Skills creation, structure, and usage patterns.

### Document Structure

After frontmatter, agents should follow this structure:

```markdown
# [Agent Name] Agent

[One-paragraph introduction describing the agent's role]

## Core Expertise / Core Responsibility

[Clear statement of the agent's primary purpose and capabilities]

## [Domain-Specific Sections]

[Detailed guidelines, standards, checklists, examples specific to this agent]

## Reference Documentation

[Links to CLAUDE.md, conventions, and related documentation]
```

**Required Sections:**

1. **Title (H1)**: Must follow pattern `# [Name] Agent`
2. **Core Expertise/Responsibility (H2)**: Clear purpose statement
3. **Reference Documentation (H2)**: Links to relevant conventions and guidance

**Optional Sections:**

- Detailed guidelines
- Examples and anti-patterns
- Checklists
- Decision trees
- Troubleshooting

## Agent Naming Conventions

### File Naming

Agent files follow kebab-case naming with **optional scope prefixes** for app-specific or lib-specific agents:

**Pattern**: `[scope__]agent-name.md`

Where:

- `scope` (optional): `apps__[app-name]__`, `libs__[lib-name]__`, or `apps-labs__[app-name]__`
- `agent-name`: descriptive kebab-case identifier

```
✅ Good - General agents (no scope prefix):
- docs__maker.md
- wow__rules-checker.md
- plan__executor.md
- readme__maker.md

✅ Good - App-scoped agents:
- apps__ayokoding-web__general-maker.md
- apps__ayokoding-web__by-example-checker.md
- apps__ose-platform-web__content-maker.md
- apps__ose-platform-web__deployer.md

✅ Good - Lib-scoped agents (future):
- libs__ts-auth__validator.md
- libs__ts-payment__checker.md

✅ Good - Apps-labs scoped agents (future):
- apps-labs__prototype-x__tester.md

❌ Bad:
- DocWriter.md (PascalCase)
- doc_writer.md (snake_case)
- documentation-writer-agent.md (redundant suffix)
- ayokoding-general-maker.md (missing scope delimiter)
- apps_ayokoding-web_general-maker.md (wrong delimiter - use double underscore)
```

### Scope Prefix Guidelines

**When to use scope prefixes:**

1. **`apps__[app-name]__`** - Agent works ONLY with a specific app
   - Content creation for Hugo sites (ayokoding-web, ose-platform-web)
   - App-specific validation, deployment, structure management
   - Examples: `apps__ayokoding-web__general-maker`, `apps__ose-platform-web__deployer`

2. **`libs__[lib-name]__`** - Agent works ONLY with a specific library
   - Future use when monorepo has libraries with specific agents
   - Library-specific validation, testing, documentation
   - Examples: `libs__ts-auth__validator`, `libs__ts-payment__checker`

3. **`apps-labs__[app-name]__`** - Agent works with experimental apps
   - Future use for prototype-specific agents
   - Experimental validation, testing workflows
   - Examples: `apps-labs__prototype-x__tester`

**When NOT to use scope prefixes:**

- **General-purpose agents**: Work across entire repository (docs**maker, wow**rules-checker, plan\_\_executor)
- **Cross-cutting agents**: Apply to multiple apps/libs (readme**maker, agent**maker, wow\_\_workflow-maker)
- **Meta-agents**: Manage repository structure (docs**file-manager, wow**rules-maker)

**Scope naming rules:**

- App/lib names must match directory names exactly (e.g., `ayokoding-web` matches `apps/ayokoding-web/`)
- Use kebab-case throughout (no camelCase, PascalCase, or snake_case)
- Double underscore `__` separates scope from agent name (NOT single underscore `_`)
- Agent name after scope uses standard kebab-case patterns

### Naming Guidelines

1. **Be descriptive** - Name should indicate the agent's purpose
2. **Be concise** - Avoid unnecessary words
3. **Be action-oriented** - Use verbs when appropriate (`maker`, `checker`, `validator`, `fixer`, `deployer`)
4. **Avoid redundancy** - Don't add `-agent` suffix (implied by location)
5. **Match frontmatter** - `name` field must match filename exactly (including scope prefix)
6. **Use scope when appropriate** - Add `apps__[app-name]__` prefix for app-specific agents

### Agent Name vs Description

- **Name**: Short identifier used in file system and frontmatter (includes scope prefix if applicable)
- **Description**: Detailed explanation of when and how to use

Example - General agent:

```yaml
name: docs__maker # Short, kebab-case, no scope (general-purpose)
description: Expert documentation writer specializing in Obsidian-optimized markdown and Diátaxis framework. Use when creating, editing, or organizing project documentation. # Detailed usage guidance
```

Example - App-scoped agent:

```yaml
name: apps__ayokoding-web__general-maker # Includes scope prefix
description: Expert at creating general Hugo content for ayokoding-web (Hextra theme) following Hugo Content Convention and Content Quality Principles. # Detailed usage guidance
```

## Tool Access Patterns

Tool permissions follow the **principle of least privilege**: agents should only have access to tools they actually need.

| Pattern           | Tools                               | Use For                                       | Example              | Rationale                                                          |
| ----------------- | ----------------------------------- | --------------------------------------------- | -------------------- | ------------------------------------------------------------------ |
| **Read-Only**     | Read, Glob, Grep                    | Analysis without reports                      | (none currently)     | Pure read operations without file output                           |
| **Checker**       | Read, Glob, Grep, Write, Bash       | Validation with audit report generation       | wow\_\_rules-checker | Needs Write for reports in generated-reports/, Bash for timestamps |
| **Documentation** | Read, Write, Edit, Glob, Grep       | Creating/editing docs, managing doc structure | doc-writer           | Needs file creation/editing but no shell access                    |
| **Development**   | Read, Write, Edit, Glob, Grep, Bash | Code generation, tests, builds, deployment    | test-runner          | Requires command execution (⚠️ powerful, only when necessary)      |

### Report-Generating Agents: Mandatory Tool Requirements

**CRITICAL RULE**: Any agent that writes to `generated-reports/` directory MUST have **both Write and Bash** tools in their frontmatter.

**Tool Requirements Explained**:

- **Write tool**: Required for creating report files in `generated-reports/`
- **Bash tool**: Required for generating UTC+7 timestamps for report filenames using `TZ='Asia/Jakarta' date +"%Y-%m-%d--%H-%M"`

**Why both are mandatory**:

1. **Write** - Creates the actual report file
2. **Bash** - Generates accurate, real-time timestamps (placeholder timestamps like "00-00" are forbidden)

**Applies to these agent types**:

- All `*-checker` agents (wow**rules-checker, docs**checker, plan**checker, plan**execution-checker, etc.)
- `wow__rules-fixer` (generates fix reports)
- Any agent creating validation, audit, or verification reports

**MANDATORY REQUIREMENT FOR ALL \*-CHECKER AGENTS**:

ALL checker agents MUST write their validation/audit reports to `generated-reports/` directory. This is a hard requirement with NO EXCEPTIONS. The following checker agents are subject to this rule:

1. wow\_\_rules-checker
2. ayokoding-web-general-checker
3. ayokoding-web-by-example-checker
4. ayokoding-web-facts-checker
5. ayokoding-web-link-checker
6. ayokoding-web-structure-checker
7. ose-platform-web-content-checker
8. docs\_\_checker
9. docs\_\_tutorial-checker
10. readme\_\_checker
11. plan\_\_checker
12. plan\_\_execution-checker

**NO conversation-only output**: Checker agents MUST NOT output validation results in conversation only. All validation findings MUST be written to audit report files following the 4-part pattern `{agent-family}__{uuid-chain}__{YYYY-MM-DD--HH-MM}__audit.md`. The UUID chain enables parallel execution without file collisions.

**PROGRESSIVE WRITING REQUIREMENT**:

**CRITICAL BEHAVIORAL REQUIREMENT**: ALL \*-checker agents MUST write reports PROGRESSIVELY (continuously updating files during execution), NOT buffering findings in memory to write once at the end.

**Why this is mandatory:**

- **Context compaction survival**: During long audits, Claude Code may compact/summarize conversation context. If agent only writes at the END, file contents may be lost during compaction.
- **Real-time persistence**: File continuously updated THROUGHOUT execution ensures findings persist regardless of context compaction.
- **Behavioral, not optional**: This is a hard requirement for all checker agents.

**Implementation requirement:**

1. **Initialize file at execution start** - Create report file with header and "In Progress" status immediately
2. **Write findings progressively** - Each validated item written to file immediately after checking (not buffered)
3. **Update continuously** - Progress indicator and running totals updated throughout execution
4. **Finalize on completion** - Update status to "Complete" with final summary statistics

See [Temporary Files Convention - Progressive Writing Requirement](../infra/ex-de-in__temporary-files.md#progressive-writing-requirement-for-checker-agents) for complete details, patterns, and examples.

**Example frontmatter**:

```yaml
---
name: wow__rules-checker
description: Validates consistency between agents, CLAUDE.md, conventions, and documentation.
tools: Read, Glob, Grep, Write, Bash
model: sonnet
color: green
---
```

**Verification**: When creating or updating report-generating agents, verify both Write and Bash are present in the tools list.

See [Temporary Files Convention](../infra/ex-de-in__temporary-files.md) for complete details on report naming patterns, mandatory checker requirements, and timestamp generation.

### Writing to .claude Folders

**CRITICAL RULE**: When creating or modifying files in `.claude/` folders (especially `.claude/agents/`), agents MUST use Bash tools (heredoc, sed, awk, etc.) and NOT Write/Edit tools.

**Rationale**: Bash tools allow autonomous agent operation without requiring user approval for file operations. Write/Edit tools trigger user approval prompts, breaking autonomous workflows.

**Applies to**:

- Creating new agent files in `.claude/agents/`
- Updating existing agent files in `.claude/agents/`
- Modifying `.claude/agents/README.md`
- Any other `.claude` folder operations

**Tool patterns**:

```bash
# Create new agent file (heredoc pattern)
cat > .claude/agents/new-agent.md <<'END_HEREDOC'
---
name: new-agent
description: Agent description here
tools: Read, Glob, Grep, Bash
model: inherit
color: blue
---

# Agent content here...
END_HEREDOC

# Update existing agent file (sed/awk pattern)
sed -i '/^model:/s/inherit/sonnet/' .claude/agents/agent-name.md

# Update README.md (targeted insertion)
awk 'pattern { insert_text } { print }' .claude/agents/README.md > temp && mv temp .claude/agents/README.md
```

**Agents affected**:

- `agent__maker` - Creates new agents, already complies
- `wow__rules-maker` - Updates agents, already complies

**Verification**: Check that agents writing to `.claude/` use only Bash tool (not Write/Edit).

## Model Selection Guidelines

**Default**: Use `inherit` unless specific model capabilities are required.

### Model Selection Decision Tree

```
Start: Choosing Agent Model
    │
    ├─ Does this agent require specific model capabilities?
    │   │
    │   ├─ No → Use `model: inherit` ✅
    │   │        (Most agents should use inherit)
    │   │
    │   └─ Yes → What specific capability?
    │              │
    │              ├─ Simple, straightforward tasks → `model: haiku`
    │              │   (Pattern matching, URL validation, file checks)
    │              │
    │              ├─ Advanced reasoning/deep analysis → `model: sonnet`
    │              │   (Complex validation, cascading impacts, multi-step orchestration)
    │              │
    │              ├─ Multi-step planning and strategy → `model: sonnet`
    │              │
    │              └─ Unsure → Use `model: inherit` ✅
    │                         (Can always change later)
```

**⚠️ Important**: Document your reasoning if using a specific model. Add a comment in the agent explaining why.

## Agent Color Categorization

### Color Field (Required)

The `color` frontmatter field provides visual categorization for agents based on their **primary role**. This helps users quickly identify agent types and understand their capabilities at a glance.

**Format:**

```yaml
---
name: agent-name
description: Expert in X specializing in Y. Use when Z.
tools: Read, Glob, Grep
model: inherit
color: blue
---
```

**Field Definition:**

- **`color`** (required)
  - Values: `blue`, `green`, `yellow`, `purple`
  - Indicates the agent's primary role category
  - Used for visual identification in agent listings
  - Helps users choose the right agent type

### Color-to-Role Mapping

Agents are categorized by their **primary role** which aligns with naming suffixes and tool permissions:

| Color         | Role             | Purpose                               | Tool Pattern                            | Agents                                                                                     |
| ------------- | ---------------- | ------------------------------------- | --------------------------------------- | ------------------------------------------------------------------------------------------ |
| 🟦 **Blue**   | **Writers**      | Create new content from scratch       | Has `Write` tool                        | docs**maker<br>plan**maker<br>docs\_\_tutorial-maker                                       |
| 🟩 **Green**  | **Checkers**     | Validate and generate reports         | Has `Write`, `Bash` (no `Edit`)         | wow**rules-checker<br>plan**checker<br>docs\_\_checker                                     |
| 🟨 **Yellow** | **Updaters**     | Modify and propagate existing content | Has `Edit` (usually not `Write`)\*      | wow**rules-maker\*<br>docs**file-manager                                                   |
| 🟪 **Purple** | **Implementors** | Execute plans with full tool access   | Has `Write`, `Edit`, `Bash` (or Bash)\* | plan\_\_executor<br>docs-link-general-checker<br>ayokoding-web-link-checker<br>deployers\* |

**Edge Case Notes:**

- **\*Yellow with Write**: wow\_\_rules-maker needs Write tool to create new convention files (not just edit existing). Documented exception.
- **\*Purple Bash-only**: Deployers (ayokoding-web-deployer, ose-platform-web-deployer) only need Bash for git/deployment orchestration. Purple without Write/Edit is valid for Bash-only orchestrators.

**Color Accessibility Note**: All four colors (blue, green, yellow, purple) are from the verified accessible palette defined in [Color Accessibility Convention](../conventions/formatting/ex-co-fo__color-accessibility.md) - the master reference for all color usage in this repository. These colors meet WCAG AA standards for both light and dark modes and work for all types of color blindness (protanopia, deuteranopia, and tritanopia). See the accessibility section below for details on how agents are identified beyond color. All color-related work must reference the Color Accessibility Convention as the authoritative source.

### Why This Categorization System

This role-based categorization was chosen because it:

1. **Aligns with naming conventions** - Role suffixes (-writer, -checker, -updater, -implementor) directly map to colors
2. **Maps to tool permissions** - Clear security boundaries between read-only, edit-only, write-capable, and full-access agents
3. **Provides clear user guidance** - Users can quickly identify which category of agent they need
4. **Extensible** - New agents naturally fit into one of the four role categories
5. **Semantic consistency** - Colored square emojis (🟦🟩🟨🟪) have no pre-existing meaning in Unicode, allowing flexible assignment

### Hybrid Agents Exception

**DOCUMENTED EXCEPTION**: Link checker agents are hybrid agents that combine validation (green) with state management (purple). This is an explicit exception to the standard color-to-role mapping.

**Hybrid Link Checkers:**

- **docs-link-general-checker** - Validates documentation links + manages external-links-status.yaml cache
- **ayokoding-web-link-checker** - Validates Hugo content links + manages ayokoding-links-status.yaml cache

**Why hybrid status?**

1. **Primary role**: Link validation (checker behavior) with audit report generation
2. **State management**: Maintain persistent cache files tracking external link health over time
3. **Tool requirements**: Write tool needed ONLY for cache file management (not general content modification)
4. **Color assignment**: `purple` reflects the hybrid nature (validation + state management)

**Rationale for Write tool access:**

- Cache files (`external-links-status.yaml`, `ayokoding-links-status.yaml`) are operational metadata, NOT temporary reports
- Cache management is essential functionality, NOT general file writing capability
- Write tool is scoped specifically to designated cache file paths (explicit over implicit)
- This exception respects the Explicit Over Implicit principle by documenting the hybrid role

**Cache files are NOT temporary:**

- Location: `docs/metadata/` (docs-link-general-checker) and `apps/ayokoding-web/` (ayokoding-web-link-checker)
- Purpose: Long-term link status tracking (6-month expiry), shared across team
- Committed to git: Yes (operational metadata)
- Updated every run: Yes (including lastFullScan timestamp)

This hybrid status is intentionally documented here to maintain transparency and prevent confusion about tool permission patterns.

### Assigning Colors to New Agents

When creating a new agent, assign a color based on its **primary capability**:

**Decision Tree:**

```
Start: What is the agent's primary capability?
    │
    ├─ Creates new files/content from scratch
    │   └─> color: blue (Writer)
    │       - Must have `Write` tool
    │       - Examples: docs-writer, plan-writer
    │
    ├─ Validates/checks and generates reports
    │   └─> color: green (Checker)
    │       - Has `Write`, `Bash` (no Edit)
    │       - Write needed for audit reports in generated-reports/
    │       - Bash needed for UTC+7 timestamps
    │       - Examples: wow__rules-checker, plan__checker, docs__checker
    │       - EXCEPTION: Link checkers use purple (see Hybrid Agents above)
    │
    ├─ Modifies/updates existing content only
    │   └─> color: yellow (Updater)
    │       - Has `Edit` but NOT `Write`
    │       - Examples: wow__rules-maker, docs__file-manager
    │
    └─ Executes plans/orchestrates tasks
        └─> color: purple (Implementor)
            - Has Write, Edit, AND Bash
            - Examples: plan__executor
            - INCLUDES: Hybrid link checkers (validation + state management)
```

**Edge Cases:**

- **Agent has both Write and Edit**: Choose based on primary purpose
  - If mainly creates new content → `blue` (Writer)
  - If mainly executes plans/tasks → `purple` (Implementor)
- **Link-checkers with Write, Edit, Bash**: Use `purple` (Implementor)
  - Write tool needed for cache file updates (external-links-status.yaml)
  - Edit tool needed for fixing broken links in content
  - Bash tool needed for UTC+7 timestamps
  - Examples: docs-link-general-checker, ayokoding-web-link-checker
- **Deployers with Bash only**: Use `purple` (Implementor)
  - Execute deployment orchestration (purple's "executes plans/orchestrates tasks")
  - Don't create or edit files, only run git/deployment commands
  - Edge case: purple without Write/Edit tools (Bash-only orchestration)
  - Examples: ayokoding-web-deployer, ose-platform-web-deployer
- **Updaters with Write tool**: Investigate actual usage
  - Yellow (Updaters) should have Edit but NOT Write
  - If Write is needed for creating new convention files → keep yellow, document exception
  - If Write can be removed → remove Write to match yellow categorization
  - Example: wow\_\_rules-maker (creates new conventions, keeps Write + Edit)
- **Agent doesn't fit any category**: Consider if it should be split or if a new category is needed
- **Unsure**: Default to the most restrictive category based on tools, or omit the color field

**Accessibility Note**: All assigned colors (blue, green, yellow, purple) are verified color-blind friendly and meet WCAG accessibility standards per the [Color Accessibility Convention](../conventions/formatting/ex-co-fo__color-accessibility.md). Agents should still be identified primarily by name and role suffix, not color alone, to ensure accessibility for all users. See the Color Accessibility Convention for complete details on palette verification, testing methodology, and WCAG compliance.

### Color Accessibility for Agent Identification

**CRITICAL**: Colored square emojis (🟦🟩🟨🟪) provide visual categorization but are SUPPLEMENTARY to semantic information. Agents must be identifiable without relying on color perception.

#### Multiple Identification Methods

Agents are identified through FIVE independent methods:

| Identification Method | Example                          | Purpose                                   |
| --------------------- | -------------------------------- | ----------------------------------------- |
| **Agent Name**        | "docs\_\_maker"                  | Primary text-based identifier             |
| **Role Suffix**       | "-maker" (writer)                | Indicates category through naming pattern |
| **Emoji Shape**       | 🟦 (square)                      | Shape differentiation (not color)         |
| **Description**       | "Expert documentation writer..." | Semantic purpose statement                |
| **Color Field**       | `color: blue`                    | Text value in frontmatter                 |

**Users with color blindness can identify agents by:**

- Reading the agent name
- Recognizing the role suffix pattern (-maker, -checker, -updater, -executor)
- Seeing that the emoji is a square (shape, not color)
- Reading the description field

**Color perception is NOT required** to use agents effectively.

#### Accessible Color Palette Verification

All agent colors are from the verified accessible palette:

| Color  | Emoji | Hex Code | WCAG AA (Light) | WCAG AA (Dark) | Safe For               |
| ------ | ----- | -------- | --------------- | -------------- | ---------------------- |
| Blue   | 🟦    | #0173B2  | ✅ 4.88:1       | ✅ 4.30:1      | All types (excellent)  |
| Green  | 🟩    | #029E73  | ✅ 4.67:1       | ✅ 4.50:1      | All types (good)       |
| Yellow | 🟨    | #F1C40F  | ⚠️ 3.51:1       | ⚠️ 2.99:1      | All types (moderate)\* |
| Purple | 🟪    | #CC78BC  | ✅ 3.65:1       | ✅ 5.74:1      | All types (moderate)   |

\*Yellow emoji (#F1C40F) has slightly lower contrast but remains distinguishable because it's combined with:

- Square shape (not relying on color alone)
- Text label "Updater"
- Role suffix "-updater"

**Source**: Verified through ColorBrewer2, Paul Tol's schemes, and WCAG testing. See [Color Accessibility Convention](../conventions/formatting/ex-co-fo__color-accessibility.md) - the master reference for all color usage - for complete palette details, scientific verification sources, testing methodology, and WCAG compliance standards.

#### Why These Colors Were Chosen

1. **Protanopia & Deuteranopia (red-green blindness)**: Blue, yellow, and purple remain distinct. We avoid red and green entirely.
2. **Tritanopia (blue-yellow blindness)**: Blue appears pink, yellow appears light pink, but shape and text differentiation ensure identification.
3. **WCAG AA Compliance**: All colors meet minimum contrast requirements against both light and dark backgrounds.
4. **Cross-Platform Consistency**: Colors render consistently across GitHub, Obsidian, VS Code, and terminals.

#### Agent Identification Example

**Example agent: `docs__maker`**

```yaml
---
name: docs__maker
description: Expert documentation writer specializing in Obsidian-optimized markdown and Diátaxis framework. Use when creating, editing, or organizing project documentation.
tools: Read, Write, Edit, Glob, Grep
model: inherit
color: blue
---
```

**How users identify this agent (without seeing color):**

1. **Name**: "docs\_\_maker" (text identifier)
2. **Suffix**: "-maker" implies writer/creator role
3. **Description**: "Expert documentation writer" (semantic identifier)
4. **Emoji**: 🟦 appears as a square (shape), regardless of color perception
5. **Field**: `color: blue` is a text value in YAML

**For users with protanopia/deuteranopia**: The blue square appears as a distinct shade but is identifiable by its square shape and accompanying text.

**For users with tritanopia**: The blue square appears pinkish but is identifiable by its square shape and accompanying text.

**For users with complete color blindness (achromatopsia)**: All squares appear as different shades of gray but are identifiable by their position next to agent names and descriptions.

### Using Colors in Documentation

**Agent README Listings:**

When listing agents in `.claude/agents/README.md`, use the colored square emoji:

```markdown
### 🟦 `docs-writer.md`

Expert documentation writer specializing in Obsidian-optimized markdown and Diátaxis framework.
```

**Consistency with Emoji Convention:**

Colored square emojis follow the [Emoji Usage Convention](../conventions/formatting/ex-co-fo__emoji.md):

- Use at the start of headings for visual categorization
- Maintain semantic consistency (same color = same role across all docs)
- Avoid overuse (1 emoji per agent listing)

### Color Field Examples

**Writer Agent (Blue):**

```yaml
---
name: docs-writer
description: Expert documentation writer specializing in Obsidian-optimized markdown and Diátaxis framework. Use when creating, editing, or organizing project documentation.
tools: Read, Write, Edit, Glob, Grep
model: inherit
color: blue
---
```

**Checker Agent (Green):**

```yaml
---
name: wow__rules-checker
description: Validates consistency between agents, CLAUDE.md, conventions, and documentation. Use when checking for inconsistencies, contradictions, duplicate content, or verifying repository rule compliance.
tools: Read, Glob, Grep, Write, Bash
model: sonnet
color: green
---
```

**Updater Agent (Yellow):**

```yaml
---
name: wow__rules-maker
description: Propagates rule and convention changes across CLAUDE.md, convention docs, agents, and indices. Use when adding/modifying rules, conventions, or standards that affect multiple files.
tools: Read, Edit, Glob, Grep
model: sonnet
color: yellow
---
```

**Implementor Agent (Purple):**

```yaml
---
name: plan__executor
description: Expert at systematically implementing project plans by following delivery checklists. Reads plans from plans/ directory, executes implementation steps, runs validation, and updates checklist progress with detailed notes. Use when executing a plan created by the plan__maker agent.
tools: Read, Write, Edit, Glob, Grep, Bash
model: sonnet
color: purple
---
```

## Agent Responsibility Boundaries

### Single Responsibility Principle

Each agent should have **one clear, focused purpose**.

**✅ Good - Single Responsibility:**

```yaml
name: doc-writer
description: Expert documentation writer specializing in Obsidian-optimized markdown and Diátaxis framework. Use when creating, editing, or organizing project documentation.
```

**❌ Bad - Multiple Responsibilities:**

```yaml
name: doc-and-code-helper
description: Writes documentation, generates code, runs tests, and deploys applications.
```

### Avoiding Overlap

Before creating a new agent, check if existing agents already cover the domain:

1. **Review** `.claude/agents/` directory
2. **Check** each agent's `description` field
3. **Consider** if you can extend an existing agent
4. **Create new** only if there's no overlap

**Decision Matrix: New Agent vs Extend Existing**

| Scenario                     | Create New Agent | Extend Existing Agent |
| ---------------------------- | ---------------- | --------------------- |
| Completely different domain  | ✅ Yes           | ❌ No                 |
| Different tool requirements  | ✅ Yes           | ❌ No                 |
| Different model needs        | ✅ Yes           | ❌ No                 |
| Slight variation in workflow | ❌ No            | ✅ Yes                |
| Similar expertise area       | ❌ No            | ✅ Yes                |
| Experimental/temporary       | ⚠️ Maybe         | ✅ Prefer extending   |

### Agent Specialization vs Generalization

**Prefer specialization over generalization.**

**✅ Good - Specialized Agents:**

- `doc-writer` - Documentation only
- `wow__rules-checker` - Consistency validation only
- `test-runner` - Test execution only

**❌ Bad - Over-Generalized:**

- `helper` - Too vague, unclear purpose
- `assistant` - No specific expertise
- `general-agent` - Defeats the purpose of specialization

## Convention Referencing Standards

### Required Section: Reference Documentation

**Every agent MUST include a "Reference Documentation" section** at the end. See the [Agent Creation Template](#agent-creation-template) below for the complete format.

### Reference Categories

Organize references into clear categories:

1. **Project Guidance** - Always reference `CLAUDE.md`
2. **Agent Conventions** - Always reference this document (`ex-de__ai-agents.md`)
3. **Domain-Specific Conventions** - Reference relevant conventions
4. **Related Agents** - Cross-reference complementary agents

### Link Format

Use GitHub-compatible markdown with relative paths:

```markdown
✅ Good:

- `docs/explanation/development/agents/ex-de-ag__ai-agents.md` - AI agents convention

❌ Bad:

- [[ex-de__ai-agents]] - Obsidian wiki link (not GitHub compatible)
- `/docs/explanation/development/agents/ex-de-ag__ai-agents.md` - Absolute path
- `docs/explanation/development/agents/ex-de-ag__ai-agents` - Missing .md extension
```

See [Linking Convention](../conventions/formatting/ex-co-fo__linking.md) for details.

## Agent File Size Standards

### Size Limits by Agent Complexity

Agent files are organized into **three complexity tiers** with corresponding size guidelines. These limits balance agent capability with performance, maintainability, and clarity.

**Rationale**: Research shows LLMs follow ~150-200 instructions reliably, with quality degrading as count increases. While agents are only loaded when spawned (unlike CLAUDE.md which is universally included), keeping them focused improves effectiveness.

#### Tier 1: Simple Agents (Deployers, Specialized Operations)

**Target**: < 500 lines / < 15KB
**Warning**: 600 lines / 18KB
**Hard Limit**: 800 lines / 25KB

**Characteristics**:

- Single, straightforward responsibility
- Minimal decision logic
- Limited tool usage (typically Bash only for deployers)
- Few edge cases to handle
- Direct, linear workflows

**Examples**:

- ayokoding-web-deployer (deployment automation)
- ose-platform-web-deployer (deployment automation)

**When to use this tier**:

- Agent performs one specific operation repeatedly
- Minimal validation or error handling needed
- Clear success/failure conditions
- No complex orchestration

#### Tier 2: Standard Agents (Makers, Checkers, Validators)

**Target**: < 800 lines / < 25KB
**Warning**: 1,000 lines / 30KB
**Hard Limit**: 1,200 lines / 35KB

**Characteristics**:

- Moderate complexity with clear domain
- Multiple related responsibilities
- Comprehensive validation or creation logic
- Moderate edge case handling
- Structured workflows with phases

**Examples**:

- docs\_\_maker (documentation creation)
- docs\_\_checker (factual verification)
- docs\_\_tutorial-checker (tutorial quality validation)
- agent\_\_maker (agent creation automation)
- ayokoding-web-general-maker (general Hugo content creation)
- ayokoding-web-by-example-maker (by-example tutorial creation)
- ose-platform-web-content-maker (Hugo content creation)

**When to use this tier**:

- Agent creates or validates content
- Requires moderate decision-making
- Follows established patterns
- Handles multiple related tasks within a domain

#### Tier 3: Complex Agents (Planners, Orchestrators, Comprehensive Validators)

**Target**: < 1,200 lines / < 35KB
**Warning**: 1,500 lines / 40KB
**Hard Limit**: 1,800 lines / 50KB

**Characteristics**:

- High complexity with multiple interconnected concerns
- Advanced reasoning and pattern recognition
- Multi-step orchestration
- Extensive edge case handling
- Complex validation or planning logic
- Cross-cutting concerns

**Examples**:

- plan\_\_maker (comprehensive project planning)
- plan\_\_executor (multi-phase implementation)
- plan\_\_checker (pre-implementation validation)
- wow\_\_rules-maker (cascading updates across files)
- wow\_\_rules-checker (comprehensive consistency validation)
- docs\_\_file-manager (prefix calculation, link updates, git operations)
- swe**hugo**developer (theme development, asset pipeline, configuration)
- docs-link-general-checker (external/internal link validation with caching)

**When to use this tier**:

- Agent orchestrates multiple phases or agents
- Requires advanced reasoning
- Handles complex dependencies
- Manages cascading impacts
- Performs comprehensive validation

### Agent Categorization Reference

Quick categorization for existing agents:

| Tier                 | Agents                                                                                                                                                                                                                                                                                                                   |
| -------------------- | ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------ |
| **Tier 1: Simple**   | ayokoding-web-deployer, ose-platform-web-deployer                                                                                                                                                                                                                                                                        |
| **Tier 2: Standard** | docs**maker, docs**tutorial-maker, docs**checker, docs**tutorial-checker, readme**maker, readme**checker, agent\_\_maker, ayokoding-web-general-maker, ayokoding-web-by-example-maker, ayokoding-web-general-checker, ayokoding-web-by-example-checker, ose-platform-web-content-maker, ose-platform-web-content-checker |
| **Tier 3: Complex**  | plan**maker, plan**executor, plan**checker, plan**execution-checker, wow**rules-maker, wow**rules-checker, docs**file-manager, swe**hugo\_\_developer, docs-link-general-checker                                                                                                                                         |

### When to Condense or Split Agents

**Warning Signs (approaching limits)**:

- Agent approaching warning threshold for its tier
- Agent has multiple unrelated responsibilities
- Documentation becoming hard to navigate
- Users confused about when to use the agent

**Condensation Strategies**:

1. **Move details to conventions OR development docs (PRIMARY STRATEGY)** - **CRITICAL:** MOVE content to appropriate docs, NOT DELETE.

   **Destinations**:
   - `docs/explanation/conventions/` (content/format standards)
   - `docs/explanation/development/` (process/workflow standards)

   Create or expand documents with comprehensive details, then replace with brief summary + link. Zero content loss required.

2. **Remove redundant examples** - Keep 1-2 clear examples per pattern
3. **Consolidate similar sections** - Merge related guidelines
4. **Use tables instead of lists** - More compact for comparisons
5. **Remove "nice to have" guidance** - Focus on essential requirements

**When to split an agent**:

- Agent exceeds hard limit for its tier
- Agent has two clearly separable responsibilities
- Agent requires different tool sets for different tasks
- Users would benefit from specialized agents

**Example split scenarios**:

- Agent that both creates and validates → Split into maker + checker
- Agent handling multiple unrelated domains → Split by domain
- Agent with basic + advanced modes → Split by complexity level

### Size Checking Process

**For agent\_\_maker**:

1. After creating agent file, count lines and characters
2. Compare to tier limits based on agent type
3. Warn if approaching warning threshold
4. Suggest condensation if near limit

**For wow\_\_rules-maker**:

1. When updating agents, check file size before/after
2. If agent crosses warning threshold, notify user
3. Suggest condensation strategies

**For all agent authors**:

1. Before committing agent changes, verify size
2. If approaching limits, review for redundancy
3. Consider moving details to convention docs
4. Link to detailed docs rather than duplicating

### Agent Content Philosophy

**Focus on single responsibility**:

- Each agent should do ONE thing well
- Complex workflows should orchestrate multiple agents
- Don't create "Swiss Army knife" agents

**Detailed but targeted prompts**:

- Provide comprehensive guidance for the agent's domain
- Don't document unrelated concerns
- Link to convention docs instead of duplicating

**Avoid duplication with convention docs**:

- Convention docs are the source of truth
- Agents should reference conventions, not repeat them
- Exception: Agent-specific applications of conventions

**Balance comprehensiveness with conciseness**:

- Include essential decision logic and examples
- Remove tangential information
- Prefer structured formats (tables, checklists) over prose

## Agent Documentation Standards

### Required Elements

Every agent must include:

1. ✅ **Clear purpose statement** - What does this agent do?
2. ✅ **Core expertise/responsibility** - What is it an expert in?
3. ✅ **Usage guidelines** - When should you use this agent?
4. ✅ **Reference documentation** - Links to conventions and related docs

### Recommended Elements

Depending on complexity, consider adding:

- **Examples** - Show the agent in action
- **Anti-patterns** - What NOT to do
- **Checklists** - Step-by-step verification
- **Decision trees** - Help users make decisions
- **Troubleshooting** - Common issues and solutions

### Writing Style

Follow these guidelines when writing agent documentation:

1. **Use imperative, direct language**
   - ✅ "Use this agent when creating documentation"
   - ❌ "This agent could potentially be used for documentation tasks"

2. **Be action-oriented**
   - ✅ "Validates consistency between files"
   - ❌ "Performs validation activities"

3. **Provide concrete examples**
   - Include code snippets, file examples, command outputs
   - Show both good (✅) and bad (❌) examples

4. **Use checklists where applicable**
   - Break complex tasks into verifiable steps
   - Use `- [ ]` format for actionable items

5. **Be specific, not vague**
   - ✅ "Checks file naming against ex-co\_\_file-naming-convention.md"
   - ❌ "Validates files"

6. **Follow indentation convention**
   - Agent files are in `.claude/agents/` (outside `docs/`), so use standard markdown (spaces for indentation)
   - When agents create/edit files in `docs/`, they must use TAB indentation for nested bullets
   - YAML frontmatter always uses spaces (2 spaces per level) regardless of file location

## Information Accuracy and Verification

### Core Principle

**Verify, never assume.** All agents must prioritize factual accuracy by actively verifying information through tools (Read, Grep, Glob, WebSearch, WebFetch) rather than relying on assumptions or outdated general knowledge.

### Verification Requirements

Use appropriate tools to verify all claims:

- **Code/Implementation**: Read actual source with `Read`, search with `Grep/Glob`, quote line numbers
- **Project Conventions**: Read convention docs before referencing, quote exact sections with file:line
- **External Libraries**: Use `WebSearch/WebFetch` for current docs, cite sources with URLs and dates
- **File Structure**: Use `Glob` to verify paths exist, `Bash` to list contents, report exact paths
- **Commands**: Test all examples, verify outputs match documentation
- **Links**: Use `Glob/Grep` to confirm targets exist before creating links

### Verification Tools Matrix

| Information Type    | Primary Tool | Secondary Tool   | Required?      |
| ------------------- | ------------ | ---------------- | -------------- |
| Code implementation | Read         | Grep, Glob       | ✅ Required    |
| Project conventions | Read         | Grep             | ✅ Required    |
| File structure      | Glob         | Bash             | ✅ Required    |
| External libraries  | WebSearch    | WebFetch         | ✅ Required    |
| Official docs       | WebFetch     | WebSearch        | ✅ Required    |
| Best practices      | WebSearch    | WebFetch         | ⚠️ Recommended |
| Historical context  | WebSearch    | Read (changelog) | ⚠️ Recommended |

### When Verification is Not Possible

If information cannot be verified: (1) State the limitation explicitly, (2) Provide verification steps for the user, (3) Never present unverified information as fact.

### Agent-Specific Requirements

- **Documentation agents (doc-writer)**: Verify code examples, file paths, project structure claims, convention references, external library docs
- **Validation agents (wow\_\_rules-checker)**: Read all files before validating, provide specific line numbers, verify links and frontmatter
- **Development agents**: Read test files, verify command outputs, check error messages, confirm tool availability

### Verification Checklist for Agents

Before providing information, verify:

- [ ] Have I read the actual files being discussed?
- [ ] Have I verified file paths exist?
- [ ] Have I checked the actual code implementation?
- [ ] Have I consulted official documentation for external libraries?
- [ ] Have I provided specific line numbers and file paths?
- [ ] Have I stated clearly what I verified vs. what I assumed?
- [ ] Have I used appropriate tools (Read, Grep, Glob, WebSearch, WebFetch)?
- [ ] Have I cited sources with URLs and access dates?
- [ ] If I cannot verify, have I stated this limitation clearly?
- [ ] Have I provided steps for the user to verify themselves?

## Creating New Agents

### When to Create a New Agent

Create a new agent when:

1. ✅ **New domain or expertise** not covered by existing agents
2. ✅ **Different tool requirements** than existing agents
3. ✅ **Distinct user need** that would benefit from specialization
4. ✅ **Clear, single responsibility** that doesn't overlap

Don't create a new agent when:

1. ❌ **Existing agent can be extended** with minor modifications
2. ❌ **Responsibilities overlap** significantly with existing agents
3. ❌ **Purpose is too vague** or general
4. ❌ **Temporary or experimental** need (extend existing instead)

### Agent Creation Checklist

Before submitting a new agent, verify:

#### Frontmatter Complete

- [ ] `name` matches filename (kebab-case, no `.md`)
- [ ] `description` clearly states when to use this agent
- [ ] `tools` explicitly lists required tools only (least privilege)
- [ ] `model` set to `inherit` (or justified if specific)
- [ ] `color` assigned based on agent role (blue/green/yellow/purple) - required
- [ ] `skills` field present (can be empty `[]` or list actual Skills) - required

#### Document Structure

- [ ] H1 title follows pattern: `# [Name] Agent`
- [ ] Core responsibility/expertise clearly stated
- [ ] Detailed guidelines provided
- [ ] Reference documentation section included

#### Content Quality

- [ ] Purpose is clear and specific
- [ ] No significant overlap with existing agents
- [ ] Examples provided for usage
- [ ] Anti-patterns documented (what NOT to do)

#### Convention Compliance

- [ ] References `CLAUDE.md`
- [ ] References AI agents convention (`ex-de__ai-agents.md`)
- [ ] References relevant domain conventions
- [ ] Links use correct GitHub-compatible format

#### Information Accuracy

- [ ] Agent includes verification requirements for its domain
- [ ] Agent specifies when to use Read/Grep/Glob for verification
- [ ] Agent specifies when to use WebSearch/WebFetch for verification
- [ ] Agent emphasizes verification over assumptions
- [ ] Agent provides examples of good vs bad verification practices

#### File Size Compliance

- [ ] Agent size within appropriate tier limits (Simple: <800 lines, Standard: <1,200 lines, Complex: <1,800 lines)
- [ ] If approaching warning threshold, consider condensation strategies
- [ ] Verified no duplication with convention docs (link instead)

#### Testing

- [ ] Manually tested agent invocation
- [ ] Verified tool permissions are sufficient
- [ ] Confirmed no tool permission creep
- [ ] Verified model selection is appropriate

### Agent Template

Use this template when creating new agents:

```markdown
---
name: agent-name
description: Expert in [domain] specializing in [specific area]. Use when [specific scenario].
tools: Read, Glob, Grep
model: inherit
color: blue
skills: []
created: YYYY-MM-DD
updated: YYYY-MM-DD
---

# Agent Name Agent

You are an expert [role/domain] specializing in [specific expertise].

## Core Responsibility

Your primary job is to [clear, specific purpose statement].

## [Domain-Specific Guidelines]

[Detailed guidelines, standards, examples specific to this agent's domain]

### [Subsection as needed]

[More specific guidance]

## [Additional Sections as Needed]

- Examples
- Checklists
- Decision trees
- Anti-patterns
- Troubleshooting

## Reference Documentation

**Project Guidance:**

- `CLAUDE.md` - Primary guidance for all agents working on this project

**Agent Conventions:**

- `docs/explanation/development/agents/ex-de-ag__ai-agents.md` - AI agents convention (all agents must follow)

**[Domain-Specific Conventions]:**

- Relevant conventions for this agent's domain

**Related Agents:**

- Other complementary agents (if applicable)
```

## Relationship to CLAUDE.md

### Division of Responsibilities

**CLAUDE.md provides:**

- ✅ Project-wide guidance for ALL agents
- ✅ Project overview and context
- ✅ Environment setup (Volta, Node.js, npm)
- ✅ Git hooks and commit conventions
- ✅ High-level documentation organization
- ✅ Reference to this AI agents convention

**Individual agents provide:**

- ✅ Specialized domain expertise
- ✅ Specific task instructions
- ✅ Detailed guidelines for their area
- ✅ Examples and checklists for their domain

**This convention (ex-de\_\_ai-agents.md) provides:**

- ✅ Standards for how agents are structured
- ✅ Agent creation guidelines
- ✅ Tool and model selection criteria
- ✅ Convention referencing requirements

### CLAUDE.md Maintenance Standards

**CRITICAL:** CLAUDE.md is a navigation document, not a knowledge dump. All agents must help maintain its conciseness.

**Size Limits:**

- **Hard limit:** 40,000 characters (performance threshold - DO NOT EXCEED)
- **Target limit:** 30,000 characters (provides 25% headroom)
- **Warning threshold:** 35,000 characters (time to review and condense)

**Agent Responsibilities:**

1. **wow\_\_rules-maker:**
   - MUST check CLAUDE.md size when adding rules
   - Warn user if file exceeds 35,000 characters
   - Suggest condensation strategies (move details to convention docs)
   - Add only 2-5 line summaries to CLAUDE.md, link to detailed docs

2. **docs\_\_maker and related content agents:**
   - MUST NOT add verbose content to CLAUDE.md
   - When adding conventions, create detailed doc first, then brief CLAUDE.md summary
   - Maximum CLAUDE.md section length: 3-5 lines + link

3. **All agents:**
   - When in doubt, link to detailed docs rather than duplicate content
   - Each CLAUDE.md section should answer "what, where, why" but link to "how"
   - Comprehensive details belong in convention docs, not CLAUDE.md

### Agent Isolation and Delivery Pattern

```
Startup: CLAUDE.md ──loaded──> Orchestrator (main conversation)
Runtime: Orchestrator ──spawns──> Agents (isolated contexts)
         Skills ──delivers via skills: field──> Agents
         Conventions ──explicit references──> Agents
```

**Critical Understanding:**

1. **Agents have isolated contexts** - They do NOT inherit CLAUDE.md
2. **Skills deliver explicitly** - Only Skills listed in agent's `skills:` field are available
3. **References are explicit** - Agents link to specific conventions they need
4. **Orchestrator has CLAUDE.md** - Main conversation loads CLAUDE.md, not agents

**Rules:**

1. **Don't duplicate** - Agents should reference conventions, not repeat content
2. **Do specialize** - Agents add domain expertise through Skills and explicit knowledge
3. **Follow conventions** - All agents must comply with this convention
4. **Declare skills explicitly** - Every agent must have non-empty `skills:` field

### What Belongs Where

| Content Type      | CLAUDE.md | Individual Agent | This Convention |
| ----------------- | --------- | ---------------- | --------------- |
| Project overview  | ✅        | ❌               | ❌              |
| Environment setup | ✅        | ❌               | ❌              |
| Git/commit rules  | ✅        | ❌               | ❌              |
| Documentation org | ✅        | ❌               | ❌              |
| Agent structure   | ❌        | ❌               | ✅              |
| Agent creation    | ❌        | ❌               | ✅              |
| Domain expertise  | ❌        | ✅               | ❌              |
| Specific tasks    | ❌        | ✅               | ❌              |

## Special Cases

### Agent Directory Structure

The `.claude/agents/` directory:

- **Contains** a `README.md` file for agent index and workflow guidance
- **Contains** agent definition files (`.md` files)
- **Follows** flat structure (no subdirectories)

The `.claude/agents/README.md` file:

- Lists all available agents with descriptions
- Explains agent workflow and best practices
- Provides guidance on when to use each agent
- Follows the naming exception for README.md files (documented in [File Naming Convention](../conventions/meta/ex-co-me__file-naming.md))

### Agent Versioning

Currently, we don't version agents. If significant changes are needed:

1. **Update in place** for minor improvements
2. **Document changes** in the agent file (update metadata comment)
3. **Consider** creating a new agent if the purpose changes significantly

### Deprecating Agents

If an agent is no longer needed:

1. **Don't delete immediately** - may be referenced
2. **Add deprecation notice** at the top of the agent file
3. **Point to replacement** agent (if applicable)
4. **Remove after** confirming no references exist

## Anti-Patterns

| Anti-Pattern                     | ❌ Bad                                                              | ✅ Good                                                                                                                                                                         |
| -------------------------------- | ------------------------------------------------------------------- | ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| **Vague Description**            | `description: Helper agent for various tasks`                       | `description: Expert documentation writer specializing in Obsidian-optimized markdown and Diátaxis framework. Use when creating, editing, or organizing project documentation.` |
| **Tool Permission Creep**        | `tools: Read, Write, Edit, Glob, Grep, Bash` (for validation agent) | `tools: Read, Glob, Grep` (read-only for validation)                                                                                                                            |
| **Unnecessary Model Override**   | Using specific model without clear need                             | Use `model: inherit` unless advanced reasoning truly required; then `model: sonnet`                                                                                             |
| **Duplicating CLAUDE.md**        | Repeating entire environment setup section                          | Reference: `CLAUDE.md` - Primary guidance including environment setup                                                                                                           |
| **Missing Reference Section**    | No references to conventions or CLAUDE.md                           | Include Reference Documentation section with links to CLAUDE.md and ex-de\_\_ai-agents.md                                                                                       |
| **Overlapping Responsibilities** | `doc-writer-and-validator` (multiple responsibilities)              | Separate `doc-writer` and `doc-validator` agents                                                                                                                                |

## Validation and Compliance

### Repo-Rule-Checker Integration

The `wow__rules-checker` agent validates all agents against this convention.

**Checks performed:**

1. ✅ Frontmatter has all required fields
2. ✅ Agent `name` matches filename
3. ✅ Agent `description` provides clear usage guidance
4. ✅ Agent `tools` field lists tools explicitly
5. ✅ Agent `model` field is present and valid
6. ✅ Document structure follows standard pattern
7. ✅ Reference documentation section exists
8. ✅ References to CLAUDE.md and this convention present
9. ✅ Links use GitHub-compatible format

### Manual Verification

Before committing a new agent:

1. **Read this entire convention** - Understand all requirements
2. **Use the agent creation checklist** - Verify all items
3. **Test the agent** - Invoke it and verify behavior
4. **Review existing agents** - Ensure consistency
5. **Run wow\_\_rules-checker** - Validate compliance

## Related Documentation

- [Development Index](./README.md) - Overview of development conventions
- [Conventions Index](../conventions/README.md) - Documentation conventions
- [Color Accessibility Convention](../conventions/formatting/ex-co-fo__color-accessibility.md) - Master reference for all color usage (agent categorization, diagrams, accessibility standards)
- [File Naming Convention](../conventions/meta/ex-co-me__file-naming.md) - How to name files
- [Linking Convention](../conventions/formatting/ex-co-fo__linking.md) - How to link between files
- [Diátaxis Framework](../conventions/meta/ex-co-me__diataxis-framework.md) - Documentation organization

---

**Last Updated**: 2025-12-26
