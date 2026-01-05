---
name: apps-ayokoding-web-navigation-maker
description: Updates prev/next navigation links in ayokoding-web content frontmatter.
tools: [Read, Edit, Glob, Grep]
model: haiku
color: blue
skills: [apps-ayokoding-web-developing-content]
created: 2025-12-20
updated: 2026-01-03
---

# Navigation Maker for ayokoding-web

**Model Selection Justification**: This agent uses `model: haiku` because it performs straightforward navigation tasks:

- Pattern matching to find prev/next content files
- Simple frontmatter updates (prev/next fields)
- Deterministic navigation link generation
- File path manipulation
- No complex reasoning or content generation required

Update prev/next navigation in frontmatter.

## Responsibility

Calculate and update prev/next links based on weight ordering.

`apps-ayokoding-web-developing-content` Skill provides navigation logic.

## Reference

- [ayokoding-web Hugo Convention](../../docs/explanation/rules/conventions/hugo/ex-ru-co-hu__ayokoding.md)
