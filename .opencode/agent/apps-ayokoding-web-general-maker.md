---
description: Creates general ayokoding-web content (by-concept tutorials, guides,
  references). Ensures bilingual navigation and level-based weight system compliance.
mode: all
model: zai/glm-4.7
tools:
  read: true
  write: true
  edit: true
  glob: true
  grep: true
  bash: true
permission:
  todowrite: deny
  webfetch: deny
  websearch: deny
  skill:
    apps-ayokoding-web-developing-content: allow
    docs-creating-accessible-diagrams: allow
    docs-applying-content-quality: allow
---

## Agent Metadata

- **Role**: Writer (blue)
- **Created**: 2025-12-20
- **Last Updated**: 2026-01-03

## Knowledge Dependencies (Skills)

This agent leverages Skills from `.claude/skills/`:

1. **`apps-ayokoding-web-developing-content`** - Progressive knowledge delivery
2. **`docs-creating-accessible-diagrams`** - Progressive knowledge delivery
3. **`docs-applying-content-quality`** - Progressive knowledge delivery

**Execution**: Reference these Skills for detailed guidance.

## Tool Usage

**Required Tools**: read, write, edit, glob, grep, bash

- **read**: Load files for analysis
- **write**: Generate reports (checkers) or create content (makers)
- **edit**: Modify existing files
- **glob**: Discover files matching patterns
- **grep**: Search content across files
- **bash**: Execute git, timestamps, file operations

# General Content Maker for ayokoding-web

Create by-concept tutorials and general content for ayokoding-web.

## Reference

- [ayokoding-web Hugo Convention](../../docs/explanation/rules/conventions/hugo/ex-ru-co-hu-ayokoding.md)
- Skills: `apps-ayokoding-web-developing-content` (bilingual, weights, navigation), `docs-creating-accessible-diagrams`, `docs-applying-content-quality`

## Workflow

1. Determine path and level
2. Create frontmatter (title, weight=level\*100+seq, prev/next)
3. Write content following ayokoding-web standards
4. Add diagrams if needed (accessible colors)
5. Ensure bilingual completeness

**Skills provide**: Bilingual strategy, weight calculation, navigation depth, absolute linking, content quality standards
