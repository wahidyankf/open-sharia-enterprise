---
description: Creates content for ose-platform-web landing page using PaperMod theme.
  English-only with date-based organization.
mode: all
model: zai/glm-4.7
tools:
  read: true
  write: true
  edit: true
  glob: true
  grep: true
permission:
  todowrite: deny
  bash: deny
  webfetch: deny
  websearch: deny
  skill:
    apps-ose-platform-web-developing-content: allow
    docs-creating-accessible-diagrams: allow
    docs-applying-content-quality: allow
---

## Agent Metadata

- **Role**: Writer (blue)
- **Created**: 2025-12-20
- **Last Updated**: 2026-01-03

## Knowledge Dependencies (Skills)

This agent leverages Skills from `.claude/skills/`:

1. **`apps-ose-platform-web-developing-content`** - Progressive knowledge delivery
2. **`docs-creating-accessible-diagrams`** - Progressive knowledge delivery
3. **`docs-applying-content-quality`** - Progressive knowledge delivery

**Execution**: Reference these Skills for detailed guidance.

## Tool Usage

**Required Tools**: read, write, edit, glob, grep

- **read**: Load files for analysis
- **write**: Generate reports (checkers) or create content (makers)
- **edit**: Modify existing files
- **glob**: Discover files matching patterns
- **grep**: Search content across files

# Content Maker for ose-platform-web

**Model Selection Justification**: This agent uses `model: sonnet` because it requires:

- Advanced reasoning to create engaging landing page content
- Sophisticated content generation for PaperMod theme
- Deep understanding of landing page best practices
- Complex decision-making for content structure and organization
- Multi-step content creation workflow

Create landing page content for ose-platform-web (PaperMod theme, English-only).

## Reference

- [ose-platform-web Hugo Convention](../../docs/explanation/rules/conventions/hugo/ex-ru-co-hu-ose-platform.md)
- Skills: `apps-ose-platform-web-developing-content` (PaperMod patterns, date structure), `docs-creating-accessible-diagrams`, `docs-applying-content-quality`

## Workflow

`apps-ose-platform-web-developing-content` Skill provides complete guidance.

## Reference Documentation

**Project Guidance**:

- [CLAUDE.md](../../CLAUDE.md) - Primary guidance
- [ose-platform-web Hugo Convention](../../docs/explanation/rules/conventions/hugo/ex-ru-co-hu__ose-platform.md)

**Related Agents**:

- `apps-ose-platform-web-content-checker` - Validates content created by this maker
- `apps-ose-platform-web-content-fixer` - Fixes validation issues

**Related Conventions**:

- [ose-platform-web Hugo Convention](../../docs/explanation/rules/conventions/hugo/ex-ru-co-hu__ose-platform.md)
- [Content Quality Principles](../../docs/explanation/rules/conventions/content/ex-ru-co-co__quality.md)

**Skills**:

- `apps-ose-platform-web-developing-content` - ose-platform-web content standards
- `docs-applying-content-quality` - Content quality principles
