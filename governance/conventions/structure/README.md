---
title: "Structure Conventions"
description: Documentation organization frameworks, file naming, and project planning structure
category: explanation
tags:
  - index
  - conventions
  - structure
  - organization
created: 2026-01-30
updated: 2026-01-30
---

# Structure Conventions

Documentation organization frameworks, file naming, and project planning structure. These conventions answer the question: **"How do I ORGANIZE documentation?"**

## Purpose

This directory contains standards for how documentation is organized, named, and structured across the repository. These conventions establish the foundational frameworks that govern documentation architecture.

## Documents

- [Diataxis Framework](./diataxis-framework.md) - Understanding the four-category documentation organization framework we use (Tutorials, How-To, Reference, Explanation). Foundational framework for all documentation structure
- [File Naming Convention](./file-naming.md) - Systematic approach to naming files with hierarchical prefixes encoding directory structure. Applies to docs/, governance/, and plans/ directories
- [Plans Organization](./plans.md) - Standards for organizing project planning documents in plans/ folder including structure (ideas.md, backlog/, in-progress/, done/), naming patterns (YYYY-MM-DD\_\_identifier/), lifecycle stages, and project identifiers

## Key Concepts

### Diataxis Categories

| Category    | Purpose              | User Need            |
| ----------- | -------------------- | -------------------- |
| Tutorials   | Learning-oriented    | "Help me learn"      |
| How-To      | Problem-solving      | "Help me do X"       |
| Reference   | Information-oriented | "Give me the facts"  |
| Explanation | Understanding        | "Help me understand" |

### File Naming Pattern

```
[prefix]__[content-identifier].md
```

Where prefix encodes the directory path (e.g., `tu__` for tutorials, `hoto__` for how-to).

### Plans Lifecycle

```
ideas.md → backlog/ → in-progress/ → done/
```

## Related Documentation

- [Writing Conventions](../writing/README.md) - Content quality standards
- [Formatting Conventions](../formatting/README.md) - Markdown syntax and visual elements
- [Tutorials Conventions](../tutorials/README.md) - Tutorial creation standards
- [Repository Governance Architecture](../../repository-governance-architecture.md) - Six-layer governance model

---

**Last Updated**: 2026-01-30
