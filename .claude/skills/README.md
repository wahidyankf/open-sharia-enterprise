# Claude Code Skills

This directory contains 23 skill packages that provide progressive knowledge delivery to agents. Skills bundle domain-specific conventions, standards, and best practices.

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
- **repo-applying-fixer-workflow** - Fixer agent execution workflow
- **repo-assessing-criticality-confidence** - Criticality and confidence assessment system
- **repo-generating-validation-reports** - Progressive report writing with UUID chains
- **repo-executing-checker-workflow** - Standard checker workflow pattern

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

## Skills vs Conventions

**Skills** (`.claude/skills/`):

- Delivery infrastructure for agents
- Progressive knowledge format
- Optimized for agent consumption
- References conventions as source of truth

**Conventions** (`governance/conventions/`):

- Permanent governance rules
- Comprehensive reference documentation
- Human and agent readable
- Single source of truth

Skills package conventions for efficient agent access.

## Dual-Mode Operation

**Format Compatibility**: Skills use identical format for both Claude Code and OpenCode (SKILL.md with frontmatter).

**Source of Truth**: This directory (`.claude/skills/`) is the PRIMARY source.
**Sync Target**: Changes are copied to `.opencode/skill/` (SECONDARY) via automation.

**Making Changes**:

1. Edit skills in `.claude/skills/` directory
2. Run: `npm run sync:claude-to-opencode`
3. Both systems stay synchronized (direct copy, no format conversion needed)

**See**: [CLAUDE.md](../../CLAUDE.md) for complete guidance

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

**See**: [governance/principles/README.md](../../governance/principles/README.md)

---

**Total Skills**: 23
**Format**: SKILL.md with YAML frontmatter
**Token Target**: Metadata ~100 tokens, Full content <5k tokens
**Last Updated**: 2026-01-16
