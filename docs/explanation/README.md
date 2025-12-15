---
title: Explanation
description: Conceptual documentation for open-sharia-enterprise
category: explanation
tags:
  - index
  - explanation
  - concepts
created: 2025-11-22
updated: 2025-12-15
---

# Explanation

Conceptual documentation for the open-sharia-enterprise project. These documents provide context, deep dives, and understanding of how systems work and why design decisions were made.

## 🎯 Understanding the Hierarchy

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

**Documentation Layers** (Hierarchy):

1. **[Core Principles](./principles/README.md)** (WHY) - Foundational values that **govern** everything
   - The **why** behind our decisions
   - Immutable principles that conventions and development must respect
   - Six principles: Explicit Over Implicit, Accessibility First, Simplicity Over Complexity, Automation Over Manual, Progressive Disclosure, No Time Estimates
   - **Role**: Governance layer - all lower layers must align with these principles

2. **[Conventions](./conventions/README.md)** (WHAT) - How we **write and structure documentation**
   - Documentation format, style, and organization rules
   - Markdown writing standards implementing principles
   - File naming, linking, and content quality implementing principles
   - Applies to: docs/, Hugo content, plans/, README files
   - **Role**: Documentation rules layer - implements principles in concrete WHAT standards
   - **Implemented by**: AI agents (docs-maker, docs-checker, etc.)

3. **[Development](./development/README.md)** (HOW) - How we **develop software and systems**
   - Software development practices implementing principles
   - Build processes, tooling, and workflows implementing principles
   - Testing, deployment, and code management implementing principles
   - Applies to: source code, Hugo themes/layouts, build systems, AI agents
   - **Role**: Software practices layer - implements principles in concrete HOW standards
   - **Implemented by**: AI agents (hugo-developer, plan-executor, etc.) and automation (git hooks, build tools)

4. **AI Agents** (IMPLEMENT) - Automated implementers in `.claude/agents/`
   - Enforce conventions from layer 2 (documentation rules)
   - Enforce practices from layer 3 (software standards)
   - Validate compliance and apply fixes
   - **Role**: Implementation and enforcement layer - ensures principles are followed

**Traceability**: Every rule should trace through the hierarchy: Principle (WHY) → Convention/Practice (WHAT/HOW) → Agent/Automation (IMPLEMENT). See [Core Principles](./principles/README.md) for complete traceability examples.

## 📋 Contents

- [Core Principles](./principles/README.md) - Foundational principles guiding all conventions and development
- [Conventions](./conventions/README.md) - Documentation writing and organization standards
- [Development](./development/README.md) - Software development practices and workflows
- [Software Engineering](./software-engineering/README.md) - Software engineering concepts and practices
- [Security](./information-security/README.md) - Security concepts and practices

---

**Last Updated**: 2025-12-15
