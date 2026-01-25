# Claude Code Skills

This directory contains 26 skill packages that provide progressive knowledge delivery to agents. Skills bundle domain-specific conventions, standards, and best practices.

## Skill Organization

### 📚 Documentation Skills

- **docs-applying-content-quality** - Universal content quality standards (active voice, heading hierarchy, accessibility)
- **docs-applying-diataxis-framework** - Four-category documentation organization (Tutorials, How-To, Reference, Explanation)
- **docs-creating-accessible-diagrams** - Accessible Mermaid diagrams with color-blind friendly palette
- **docs-creating-by-example-tutorials** - By-example tutorial creation methodology
- **docs-validating-factual-accuracy** - Factual verification methodology with web research
- **docs-validating-links** - Link validity checking and fixing procedures

### 📋 README Skills

- **readme-writing-readme-files** - README-specific quality standards and structure

### 📝 Planning Skills

- **plan-creating-project-plans** - Project planning methodology and structure
- **plan-writing-gherkin-criteria** - Gherkin-style acceptance criteria writing

### 🤖 Agent Development Skills

- **agent-developing-agents** - Agent creation standards and best practices
- **agent-documenting-references** - Reference documentation for agent metadata
- **agent-selecting-models** - Model selection criteria (sonnet, opus, haiku)

### 🏗️ Repository Pattern Skills

- **repo-applying-maker-checker-fixer** - Three-stage quality workflow pattern
- **repo-assessing-criticality-confidence** - Criticality and confidence assessment system
- **repo-generating-validation-reports** - Progressive report writing with UUID chains

### 💻 Programming Language Skills

- **swe-programming-elixir** - Elixir coding standards quick reference
- **swe-programming-golang** - Go coding standards quick reference
- **swe-programming-java** - Java coding standards quick reference
- **swe-programming-python** - Python coding standards quick reference
- **swe-programming-typescript** - TypeScript coding standards quick reference

### 🌐 Application-Specific Skills

- **apps-ayokoding-web-developing-content** - AyoKoding bilingual content standards
- **apps-ose-platform-web-developing-content** - OSE Platform content standards

## Skill Format (SKILL.md)

Skills use markdown with YAML frontmatter:

```yaml
---
description: Brief description of what this skill provides (progressive disclosure metadata)
---

# Skill Name

## Purpose
Brief overview of skill purpose and when to use it

## Core Content
Detailed guidance, standards, examples

## References
Links to related conventions, principles, other skills
```

**Progressive Disclosure**: Skills provide ~100 token metadata (frontmatter + Purpose) for quick context, then detailed guidance <5k tokens for comprehensive understanding.

## Skill Modes: Inline vs Fork

Skills operate in two distinct modes:

### Inline Skills (Knowledge Delivery)

**Default behavior** when `context` field is omitted or set to `inline`:

- **Progressive disclosure** - Name/description at startup, full content on-demand
- **Knowledge injection** - Add standards and guidance to current conversation
- **Convention packaging** - Bundle governance knowledge for efficient consumption
- **Composition** - Multiple skills work together seamlessly
- **Universal compatibility** - Work in both main conversation and subagent contexts

**Example use cases**: Style guides, coding conventions, domain knowledge, quality standards

**REPOSITORY STANDARD**: All skills in `.claude/skills/` MUST use inline context. See [Skill Context Architecture](../../governance/development/agents/skill-context-architecture.md) for rationale.

### Fork Skills (Task Delegation)

**Delegation behavior** when `context: fork` is set with `agent` field:

- **Spawn isolated subagent contexts** - Create separate execution environments
- **Delegate specialized tasks** - Agent field specifies which agent type to use
- **Lightweight orchestration** - Skills invoke agents for focused work
- **Return results** - Subagent output returns to main conversation
- **Main conversation only** - Subagents cannot spawn other subagents (architectural constraint)

**Configuration syntax**:

```yaml
---
name: deep-research
context: fork
agent: Explore # Built-in or custom agent
---
Research $ARGUMENTS thoroughly...
```

**Example use cases**: Deep research, focused analysis, specialized exploration

**Key difference**: Inline skills inject knowledge (work everywhere), fork skills delegate tasks (main conversation only).

**Important**: Fork skills CANNOT be placed in `.claude/skills/` directory. Place them in project-specific directories and document as "main conversation only". See [Skill Context Architecture](../../governance/development/agents/skill-context-architecture.md).

## Skills vs Conventions

**Skills** (`.claude/skills/`):

- Delivery infrastructure serving agents
- Progressive knowledge format (inline) or task delegation (fork)
- Optimized for agent consumption
- References conventions as source of truth
- Service relationship with agents (not governance)

**Conventions** (`governance/conventions/`):

- Permanent governance rules (Layer 2)
- Comprehensive reference documentation
- Human and agent readable
- Single source of truth
- Govern agents and practices

Skills package conventions for efficient agent access and can orchestrate agents for specialized tasks, but don't govern them.

## Dual-Mode Operation

**Format Compatibility**: Skills use identical format for both Claude Code and OpenCode (SKILL.md with frontmatter).

**Source of Truth**: This directory (`.claude/skills/`) is the PRIMARY source.
**Sync Target**: Changes are copied to `.opencode/skill/` (SECONDARY) via automation.

**Making Changes**:

1. Edit skills in `.claude/skills/` directory
2. Run: `npm run sync:claude-to-opencode` (powered by `rhino-cli` for fast syncing)
3. Both systems stay synchronized (direct copy, no format conversion needed)

**Implementation**: Sync powered by `rhino-cli sync-agents` (~121ms total, 25-60x faster than bash)

**See**: [CLAUDE.md](../../CLAUDE.md) for complete guidance, [apps/rhino-cli/README.md](../../apps/rhino-cli/README.md) for rhino-cli details

## Best Practices

1. **Progressive Disclosure** - Keep metadata section brief (~100 tokens), detailed content <5k tokens
2. **References One Level Deep** - Link to conventions, don't create deep skill chains
3. **Zero-Context Execution** - Bundle all necessary knowledge for standalone agent use
4. **Trusted Sources Only** - Only use skills from trusted repositories (security policy)
5. **Examples Over Theory** - Provide concrete examples and checklists

## Usage in Agents

Agents reference skills in frontmatter (OpenCode format) or through natural invocation (Claude Code):

**OpenCode Example**:

```yaml
permission:
  skill:
    docs-applying-content-quality: allow
    docs-creating-accessible-diagrams: allow
```

**Claude Code**: Skills are auto-loaded based on agent description and context.

## Governance Alignment

All skills follow governance principles:

- **Documentation First** - Skills document conventions comprehensively
- **Explicit Over Implicit** - Clear guidance, no assumptions
- **Simplicity Over Complexity** - Single-purpose skills
- **Accessibility First** - Accessible content standards

**See**: [governance/principles/README.md](./README.md)

---

**Total Skills**: 21
**Format**: SKILL.md with YAML frontmatter
**Token Target**: Metadata ~100 tokens, Full content <5k tokens
**Last Updated**: 2026-01-22
