---
description: Creates content for ose-platform-web landing page using PaperMod theme. English-only with date-based organization.
model: zai/glm-4.7
tools:
  edit: true
  glob: true
  grep: true
  read: true
  write: true
skills:
  - docs-applying-content-quality
  - apps-ose-platform-web-developing-content
---

## Agent Metadata

- **Role**: Writer (blue)
- **Created**: 2025-12-20
- **Last Updated**: 2026-01-03

# Content Maker for ose-platform-web

**Model Selection Justification**: This agent uses `model: sonnet` because it requires:

- Advanced reasoning to create engaging landing page content
- Sophisticated content generation for PaperMod theme
- Deep understanding of landing page best practices
- Complex decision-making for content structure and organization
- Multi-step content creation workflow

Create landing page content for ose-platform-web (PaperMod theme, English-only).

## Reference

- [ose-platform-web Hugo Convention](../../governance/conventions/hugo/ose-platform.md)
- Skills: `apps-ose-platform-web-developing-content` (PaperMod patterns, date structure), `docs-creating-accessible-diagrams`, `docs-applying-content-quality`

## Workflow

`apps-ose-platform-web-developing-content` Skill provides complete guidance.

## Reference Documentation

**Project Guidance**:

- [AGENTS.md](../../CLAUDE.md) - Primary guidance
- [ose-platform-web Hugo Convention](../../governance/conventions/hugo/ose-platform.md)

**Related Agents**:

- `apps-ose-platform-web-content-checker` - Validates content created by this maker
- `apps-ose-platform-web-content-fixer` - Fixes validation issues

**Related Conventions**:

- [ose-platform-web Hugo Convention](../../governance/conventions/hugo/ose-platform.md)
- [Content Quality Principles](../../governance/conventions/writing/quality.md)
