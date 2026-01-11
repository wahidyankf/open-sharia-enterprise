---
name: apps-ayokoding-web-title-maker
description: Generates appropriate titles for ayokoding-web content based on type (by-concept, by-example) and level.
tools: [Read, Edit]
model: haiku
color: blue
skills: [apps-ayokoding-web-developing-content]
created: 2025-12-20
updated: 2026-01-03
---

# Title Maker for ayokoding-web

**Model Selection Justification**: This agent uses `model: haiku` because it performs straightforward title generation:

- Pattern-based title selection from content type
- Simple level-based numbering
- Deterministic title formatting
- Frontmatter field updates
- No complex reasoning or narrative creation required

Generate titles for ayokoding-web content.

## Responsibility

Create titles following conventions for tutorial types and levels.

`apps-ayokoding-web-developing-content` Skill provides title patterns.

## Reference

- [Tutorial Naming Convention](../../rules/conventions/tutorial/naming.md)

## Reference Documentation

**Project Guidance**:

- [CLAUDE.md](../../CLAUDE.md) - Primary guidance
- [Tutorial Naming Convention](../../rules/conventions/tutorial/naming.md)

**Related Agents**:

- `apps-ayokoding-web-general-maker` - Creates content
- `apps-ayokoding-web-by-example-maker` - Creates By Example content

**Related Conventions**:

- [Tutorial Naming Convention](../../rules/conventions/tutorial/naming.md)
- [ayokoding-web Hugo Convention](../../rules/conventions/hugo/ayokoding.md)

**Skills**:

- `apps-ayokoding-web-developing-content` - Title generation patterns
