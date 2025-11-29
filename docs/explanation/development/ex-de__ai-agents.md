---
title: "AI Agents Convention"
description: Standards for creating and managing AI agents in the .claude/agents/ directory
category: explanation
tags:
  - ai-agents
  - conventions
  - claude-code
  - development
  - standards
created: 2025-11-23
updated: 2025-11-30
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
- Agent validation rules in `repo-rules-checker`

## Agent File Structure

### Required Frontmatter

Every agent file MUST begin with YAML frontmatter containing five required fields:

```yaml
---
name: agent-name
description: Expert in X specializing in Y. Use when Z.
tools: Read, Glob, Grep
model: inherit
color: blue
---
```

**Field Order**: Fields MUST appear in this exact order (name, description, tools, model, color) for consistency and grep-ability across all agents.

**Field Definitions:**

1. **`name`** (required)
   - Must match the filename (without `.md` extension)
   - Use kebab-case format
   - Should be descriptive and action-oriented
   - Examples: `doc-writer`, `repo-rules-checker`, `api-validator`

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

Agent files follow kebab-case naming:

```
✅ Good:
- doc-writer.md
- repo-rules-checker.md
- api-validator.md
- test-runner.md

❌ Bad:
- DocWriter.md (PascalCase)
- doc_writer.md (snake_case)
- documentation-writer-agent.md (redundant suffix)
```

### Naming Guidelines

1. **Be descriptive** - Name should indicate the agent's purpose
2. **Be concise** - Avoid unnecessary words
3. **Be action-oriented** - Use verbs when appropriate (`writer`, `checker`, `validator`)
4. **Avoid redundancy** - Don't add `-agent` suffix (implied by location)
5. **Match frontmatter** - `name` field must match filename

### Agent Name vs Description

- **Name**: Short identifier used in file system and frontmatter
- **Description**: Detailed explanation of when and how to use

Example:

```yaml
name: doc-writer # Short, kebab-case
description: Expert documentation writer specializing in Obsidian-optimized markdown and Diátaxis framework. Use when creating, editing, or organizing project documentation. # Detailed usage guidance
```

## Tool Access Patterns

Tool permissions follow the **principle of least privilege**: agents should only have access to tools they actually need.

| Pattern           | Tools                               | Use For                                       | Example            | Rationale                                                     |
| ----------------- | ----------------------------------- | --------------------------------------------- | ------------------ | ------------------------------------------------------------- |
| **Read-Only**     | Read, Glob, Grep                    | Validation, analysis, checking, code review   | repo-rules-checker | Should never modify files; prevents accidental changes        |
| **Documentation** | Read, Write, Edit, Glob, Grep       | Creating/editing docs, managing doc structure | doc-writer         | Needs file creation/editing but no shell access               |
| **Development**   | Read, Write, Edit, Glob, Grep, Bash | Code generation, tests, builds, deployment    | test-runner        | Requires command execution (⚠️ powerful, only when necessary) |

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

| Color         | Role             | Purpose                               | Tool Pattern                 | Agents                                                  |
| ------------- | ---------------- | ------------------------------------- | ---------------------------- | ------------------------------------------------------- |
| 🟦 **Blue**   | **Writers**      | Create new content from scratch       | Has `Write` tool             | docs-writer<br>plan-writer<br>journal-writer            |
| 🟩 **Green**  | **Checkers**     | Validate and verify without modifying | Read-only (no Write or Edit) | repo-rules-checker<br>plan-checker                      |
| 🟨 **Yellow** | **Updaters**     | Modify and propagate existing content | Has `Edit` but not `Write`   | repo-rules-updater<br>docs-renamer<br>docs-link-checker |
| 🟪 **Purple** | **Implementors** | Execute plans with full tool access   | Has `Write`, `Edit`, `Bash`  | plan-implementor                                        |

### Why This Categorization System

This role-based categorization was chosen because it:

1. **Aligns with naming conventions** - Role suffixes (-writer, -checker, -updater, -implementor) directly map to colors
2. **Maps to tool permissions** - Clear security boundaries between read-only, edit-only, write-capable, and full-access agents
3. **Provides clear user guidance** - Users can quickly identify which category of agent they need
4. **Extensible** - New agents naturally fit into one of the four role categories
5. **Semantic consistency** - Colored square emojis (🟦🟩🟨🟪) have no pre-existing meaning in Unicode, allowing flexible assignment

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
    ├─ Validates/checks without modifying
    │   └─> color: green (Checker)
    │       - Read-only tools (no Write or Edit)
    │       - Examples: repo-rules-checker, plan-checker
    │
    ├─ Modifies/updates existing content only
    │   └─> color: yellow (Updater)
    │       - Has `Edit` but NOT `Write`
    │       - Examples: repo-rules-updater, docs-renamer, docs-link-checker
    │
    └─ Executes plans/orchestrates tasks
        └─> color: purple (Implementor)
            - Has Write, Edit, AND Bash
            - Examples: plan-implementor
```

**Edge Cases:**

- **Agent has both Write and Edit**: Choose based on primary purpose
  - If mainly creates new content → `blue` (Writer)
  - If mainly executes plans/tasks → `purple` (Implementor)
- **Agent doesn't fit any category**: Consider if it should be split or if a new category is needed
- **Unsure**: Default to the most restrictive category based on tools, or omit the color field

### Using Colors in Documentation

**Agent README Listings:**

When listing agents in `.claude/agents/README.md`, use the colored square emoji:

```markdown
### 🟦 `docs-writer.md`

Expert documentation writer specializing in Obsidian-optimized markdown and Diátaxis framework.
```

**Consistency with Emoji Convention:**

Colored square emojis follow the [Emoji Usage Convention](../conventions/ex-co__emoji-usage.md):

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
name: repo-rules-checker
description: Validates consistency between agents, CLAUDE.md, conventions, and documentation. Use when checking for inconsistencies, contradictions, duplicate content, or verifying repository rule compliance.
tools: Read, Glob, Grep
model: sonnet
color: green
---
```

**Updater Agent (Yellow):**

```yaml
---
name: repo-rules-updater
description: Propagates rule and convention changes across CLAUDE.md, convention docs, agents, and indices. Use when adding/modifying rules, conventions, or standards that affect multiple files.
tools: Read, Edit, Glob, Grep
model: sonnet
color: yellow
---
```

**Implementor Agent (Purple):**

```yaml
---
name: plan-implementor
description: Expert at systematically implementing project plans by following delivery checklists. Reads plans from plans/ directory, executes implementation steps, runs validation, and updates checklist progress with detailed notes. Use when executing a plan created by the plan-writer agent.
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
- `repo-rules-checker` - Consistency validation only
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

- `docs/explanation/development/ex-de__ai-agents.md` - AI agents convention

❌ Bad:

- [[ex-de__ai-agents]] - Obsidian wiki link (not GitHub compatible)
- `/docs/explanation/development/ex-de__ai-agents.md` - Absolute path
- `docs/explanation/development/ex-de__ai-agents` - Missing .md extension
```

See [Linking Convention](../conventions/ex-co__linking-convention.md) for details.

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
- **Validation agents (repo-rules-checker)**: Read all files before validating, provide specific line numbers, verify links and frontmatter
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

- `docs/explanation/development/ex-de__ai-agents.md` - AI agents convention (all agents must follow)

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

### Inheritance Pattern

```
CLAUDE.md (Project-wide guidance)
    ↓ (inherited by all agents)
Individual Agents (Specialized capabilities)
    ↓ (follow standards from)
ex-de__ai-agents.md (Agent structure standards)
```

**Rules:**

1. **Don't duplicate** - Agents should reference CLAUDE.md, not repeat its content
2. **Do specialize** - Agents add domain expertise on top of project guidance
3. **Follow conventions** - All agents must comply with this convention

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
- Follows the naming exception for README.md files (documented in [File Naming Convention](../conventions/ex-co__file-naming-convention.md))

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

The `repo-rules-checker` agent validates all agents against this convention.

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
5. **Run repo-rules-checker** - Validate compliance

## Related Documentation

- [Development Index](./README.md) - Overview of development conventions
- [Conventions Index](../conventions/README.md) - Documentation conventions
- [File Naming Convention](../conventions/ex-co__file-naming-convention.md) - How to name files
- [Linking Convention](../conventions/ex-co__linking-convention.md) - How to link between files
- [Diátaxis Framework](../conventions/ex-co__diataxis-framework.md) - Documentation organization

---

**Last Updated**: 2025-11-29
