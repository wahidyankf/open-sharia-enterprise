# AI Agents Development

Standards and conventions for creating and managing AI agents in the `.opencode/agent/` directory.

## Purpose

These standards define **HOW to develop AI agents**, covering agent file structure, naming conventions, frontmatter requirements, tool access patterns, model selection, and size limits. All agents must follow these standards.

## Scope

**✅ Belongs Here:**

- AI agent development standards
- Agent file structure and frontmatter
- Agent naming and categorization
- Tool access and security patterns
- Model selection guidelines

**❌ Does NOT Belong:**

- Why we automate (that's a principle)
- General development workflow (that's workflow/)
- Content writing standards (that's conventions/)

## Documents

- [AI Agents Convention](./ai-agents.md) - Complete standards for creating and managing AI agents including naming, file structure, frontmatter requirements, tool access patterns, and model selection

## Related Documentation

- [Development Index](../README.md) - All development practices
- [Automation Over Manual Principle](../../principles/software-engineering/automation-over-manual.md) - Why we build agents
- [Agents Index](../../../../.opencode/agent/README.md) - All available agents
- [Repository Architecture](../..repository-governance-architecture.md) - Six-layer governance model

## Principles Implemented/Respected

This set of development practices implements/respects the following core principles:

- **[Automation Over Manual](../../principles/software-engineering/automation-over-manual.md)**: AI agents automate repetitive tasks like content validation, file management, and quality checks, ensuring consistency and reducing manual effort.

- **[Explicit Over Implicit](../../principles/software-engineering/explicit-over-implicit.md)**: Agent development conventions require explicit tool permissions, clear descriptions, and defined scopes, making agent behavior transparent and predictable.

## Conventions Implemented/Respected

This set of development practices respects the following conventions:

- **[Content Quality Principles](../../conventions/content/quality.md)**: Agent frontmatter and documentation follow active voice, proper heading hierarchy, and accessibility standards.

- **[File Naming Convention](../../conventions/meta/file-naming.md)**: Agent files use kebab-case naming with explicit prefixes following repository naming standards.

---

**Last Updated**: 2026-01-01
