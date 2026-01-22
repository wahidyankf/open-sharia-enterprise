---
description: Updates prev/next navigation links in ayokoding-web content frontmatter.
model: zai/glm-4.5-air
tools:
    edit: true
    glob: true
    grep: true
    read: true
skills:
    - docs-applying-content-quality
    - apps-ayokoding-web-developing-content
---

## Agent Metadata

- **Role**: Writer (blue)
- **Created**: 2025-12-20
- **Last Updated**: 2026-01-03

## Tool Usage

**Required Tools**: read, edit, glob, grep

- **read**: Load files for analysis
- **edit**: Modify existing files
- **glob**: Discover files matching patterns
- **grep**: Search content across files

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

- [ayokoding-web Hugo Convention](../../governance/conventions/hugo/ayokoding.md)

## Reference Documentation

**Project Guidance**:

- [AGENTS.md](../../CLAUDE.md) - Primary guidance
- [ayokoding-web Hugo Convention](../../governance/conventions/hugo/ayokoding.md)

**Related Agents**:

- `apps-ayokoding-web-structure-maker` - Creates folder structure
- `apps-ayokoding-web-general-maker` - Creates content

**Related Conventions**:

- [ayokoding-web Hugo Convention](../../governance/conventions/hugo/ayokoding.md)
