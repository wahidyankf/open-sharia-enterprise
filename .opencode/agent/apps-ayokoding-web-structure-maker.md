---
description: Creates folder structure and _index.md files for ayokoding-web following
  level-based organization.
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
  websearch: deny
  webfetch: deny
---

## Agent Metadata

- **Role**: Writer (blue)
- **Created**: 2025-12-20
- **Last Updated**: 2026-01-03

## Knowledge Dependencies (Skills)

This agent leverages Skills from `.claude/skills/`:

1. **`apps-ayokoding-web-developing-content`** - Progressive knowledge delivery

**Execution**: Reference these Skills for detailed guidance.

## Tool Usage

**Required Tools**: read, write, edit, glob, grep, bash

- **read**: Load files for analysis
- **write**: Generate reports (checkers) or create content (makers)
- **edit**: Modify existing files
- **glob**: Discover files matching patterns
- **grep**: Search content across files
- **bash**: Execute git, timestamps, file operations

# Structure Maker for ayokoding-web

Create folder structure and \_index.md files for ayokoding-web.

## Responsibility

- Create folder hierarchy (by-concept, by-example separation)
- Generate \_index.md for navigation
- Set up level-based weights
- Ensure max 2-layer navigation depth

## Workflow

`apps-ayokoding-web-developing-content` Skill provides complete structure guidance.

## Reference

- [ayokoding-web Hugo Convention](../../docs/explanation/conventions/hugo/ex-co-hu-ayokoding.md)
- Skill: `apps-ayokoding-web-developing-content`
