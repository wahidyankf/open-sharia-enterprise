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

**Documentation Layers**:

- **[Core Principles](./core-principles/README.md)** - Foundational values that guide everything
  - The **why** behind our decisions
  - Principles that conventions and development must respect
  - Examples: Explicit over implicit, Accessibility first, Simplicity over complexity

- **[Conventions](./conventions/README.md)** - How we **write and structure documentation**
  - Documentation format, style, and organization
  - Markdown writing standards
  - File naming, linking, and content quality
  - Applies to: docs/, Hugo content, plans/, README files

- **[Development](./development/README.md)** - How we **develop software and systems**
  - Software development practices and methodologies
  - Build processes, tooling, and workflows
  - Testing, deployment, and code management
  - Applies to: source code, Hugo themes/layouts, build systems, AI agents

## 📋 Contents

- [Core Principles](./core-principles/README.md) - Foundational principles guiding all conventions and development
- [Conventions](./conventions/README.md) - Documentation writing and organization standards
- [Development](./development/README.md) - Software development practices and workflows
- [Software Engineering](./software-engineering/README.md) - Software engineering concepts and practices
- [Security](./information-security/README.md) - Security concepts and practices

---

**Last Updated**: 2025-12-15
