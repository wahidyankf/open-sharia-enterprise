---
title: Core Principles
description: Foundational principles that guide all conventions and development practices
category: explanation
subcategory: core-principles
tags:
  - core-principles
  - values
  - philosophy
  - index
created: 2025-12-15
updated: 2025-12-15
---

# Core Principles

Foundational principles that guide all conventions and development practices in the open-sharia-enterprise project. These principles represent the **why** behind our conventions and methodologies.

## 🎯 Purpose

Core principles establish the philosophical foundation for how we build software and write documentation. All conventions in `docs/explanation/conventions/` and development practices in `docs/explanation/development/` must respect and embody these principles.

```mermaid
%%{ Color Palette: Blue #0173B2, Orange #DE8F05, Teal #029E73, Purple #CC78BC, Brown #CA9161 }%%
graph TD
	A[Core Principles]
	B[Conventions]
	C[Development]
	D[Implementation]

	A --> B
	A --> C
	B --> D
	C --> D

	style A fill:#0173B2,stroke:#000000,color:#FFFFFF,stroke-width:2px
	style B fill:#DE8F05,stroke:#000000,color:#FFFFFF,stroke-width:2px
	style C fill:#029E73,stroke:#000000,color:#FFFFFF,stroke-width:2px
	style D fill:#CC78BC,stroke:#000000,color:#FFFFFF,stroke-width:2px
```

**Principle Hierarchy**:

- **Core Principles** (this section) - Foundational values that guide everything
- **Conventions** - Documentation standards embodying these principles
- **Development** - Software practices embodying these principles
- **Implementation** - Actual code and content following conventions and practices

## 📋 Principles Index

### 1. [Explicit Over Implicit](./ex-cp__explicit-over-implicit.md)

Choose explicit composition and configuration over magic, convenience, and hidden behavior. Code should be transparent and understandable.

**Key applications**:

- Explicit tool permissions in AI agents (not "all tools")
- Explicit file naming with prefixes (not "clever" abbreviations)
- Explicit frontmatter fields (not defaults)
- Explicit color hex codes (not CSS color names)

### 2. [Accessibility First](./ex-cp__accessibility-first.md)

Design for universal access from the start - WCAG compliance, color-blind friendly palettes, alt text, screen reader support. Accessibility benefits everyone.

**Key applications**:

- Color-blind friendly palette in all diagrams
- Alt text required for all images
- Proper heading hierarchy
- Semantic HTML
- WCAG AA contrast standards

### 3. [Simplicity Over Complexity](./ex-cp__simplicity-over-complexity.md)

Favor minimum viable abstraction and avoid over-engineering. Start simple and add complexity only when proven necessary.

**Key applications**:

- Flat library structure (no deep nesting)
- Single-purpose agents (not multi-role)
- Minimal frontmatter fields (only what's needed)
- Direct markdown (not complex templating)
- Convention documents (not frameworks)

### 4. [Automation Over Manual](./ex-cp__automation-over-manual.md)

Automate repetitive tasks to ensure consistency and reduce human error. Humans should focus on creative work, machines on repetitive tasks.

**Key applications**:

- Git hooks (pre-commit, commit-msg)
- AI agents (docs-checker, plan-validator)
- Prettier (code formatting)
- Commitlint (message validation)
- Link verification cache

### 5. [Progressive Disclosure](./ex-cp__progressive-disclosure.md)

Start simple and layer complexity gradually. Beginners see simple patterns, experts access advanced features when needed.

**Key applications**:

- Tutorial levels (Initial Setup → Quick Start → Beginner → Intermediate → Advanced)
- Diátaxis framework (Tutorials vs Reference)
- Documentation hierarchy (Overview → Details)
- File naming (simple prefix system)
- Convention documents (basic principles → advanced patterns)

### 6. [No Time Estimates](./ex-cp__no-time-estimates.md)

People work and learn at vastly different speeds. Focus on outcomes and deliverables, not arbitrary time constraints.

**Key applications**:

- No time estimates in tutorials
- No "X hours" in educational content
- Coverage percentages instead (depth, not duration)
- Outcomes-focused language
- Plan deliverables (not timelines)

## 🔗 How Principles Cascade

### Example: Color Accessibility Principle

**Core Principle**: Accessibility First

**Convention**: [Color Accessibility Convention](../conventions/ex-co__color-accessibility.md)

- Verified accessible palette (Blue, Orange, Teal, Purple, Brown)
- WCAG AA compliance required
- Color-blind testing mandatory

**Development**: [AI Agents Convention](../development/ex-de__ai-agents.md)

- Agent color categorization uses accessible palette
- Colored square emojis (🟦 🟩 🟨 🟪)
- Color is supplementary, not sole identifier

**Implementation**: Actual agent files

- Frontmatter `color` field uses accessible colors
- README displays colored emojis
- Text labels primary, color secondary

### Example: Explicit Over Implicit Principle

**Core Principle**: Explicit Over Implicit

**Convention**: [AI Agents Convention](../development/ex-de__ai-agents.md)

- Explicit `tools` field listing allowed tools
- No default tool access
- Security through explicit whitelisting

**Development**: [Code Quality Convention](../development/ex-de__code-quality.md)

- Explicit Prettier configuration
- Explicit git hook commands
- Explicit commit message format

**Implementation**: Agent files

```yaml
---
tools: Read, Glob, Grep # Explicit tool list
---
```

## 🧭 Using These Principles

### When Creating Conventions

Ask yourself:

- Does this convention embody our core principles?
- Which principle does it support?
- Does it create unnecessary complexity?
- Is it explicit and understandable?
- Is it accessible to all users?

### When Making Decisions

Prioritize principles in order of importance:

1. **Accessibility First** - Never compromise accessibility
2. **Explicit Over Implicit** - Clarity beats convenience
3. **Simplicity Over Complexity** - Simple solutions first
4. **Automation Over Manual** - Automate when proven repetitive
5. **Progressive Disclosure** - Support all skill levels
6. **No Time Estimates** - Focus on outcomes

### When Reviewing Changes

Check that changes:

- ✅ Respect accessibility standards
- ✅ Use explicit configuration
- ✅ Maintain simplicity
- ✅ Leverage automation appropriately
- ✅ Support progressive learning
- ✅ Avoid artificial time constraints

## 📚 Related Documentation

- [Conventions Index](../conventions/README.md) - Documentation conventions embodying these principles
- [Development Index](../development/README.md) - Development practices embodying these principles
- [Explanation Index](../README.md) - All conceptual documentation

---

**Last Updated**: 2025-12-15
