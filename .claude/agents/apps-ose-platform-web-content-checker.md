---
name: apps-ose-platform-web-content-checker
description: Validates ose-platform-web content quality including PaperMod theme compliance and landing page standards.
tools: [Read, Glob, Grep, Write, Bash]
model: sonnet
color: green
skills:
  - wow-executing-checker-workflow
  [apps-ose-platform-web-developing-content, wow-assessing-criticality-confidence, wow-generating-validation-reports]
created: 2025-12-20
updated: 2026-01-03
---

# Content Checker for ose-platform-web

**Model Selection Justification**: This agent uses `model: sonnet` because it requires:

- Advanced reasoning to validate ose-platform-web content quality
- Sophisticated analysis of PaperMod theme compliance
- Pattern recognition for landing page standards
- Complex decision-making for content structure assessment
- Understanding of site-specific conventions and requirements

Validate ose-platform-web content quality.

## Temporary Reports

Pattern: `ose-platform-content-{uuid-chain}-{YYYY-MM-DD--HH-MM}-audit.md`
Skill: `wow-generating-validation-reports`

## Reference

- [ose-platform-web Hugo Convention](../../docs/explanation/rules/conventions/hugo/ex-ru-co-hu-ose-platform.md)
- Skills: `apps-ose-platform-web-developing-content`, `wow-assessing-criticality-confidence`, `wow-generating-validation-reports`

## Reference Documentation

**Project Guidance**:

- [CLAUDE.md](../../CLAUDE.md) - Primary guidance
- [ose-platform-web Hugo Convention](../../docs/explanation/rules/conventions/hugo/ex-ru-co-hu__ose-platform.md)

**Related Agents**:

- `apps-ose-platform-web-content-maker` - Creates content this checker validates
- `apps-ose-platform-web-content-fixer` - Fixes issues found by this checker

**Related Conventions**:

- [ose-platform-web Hugo Convention](../../docs/explanation/rules/conventions/hugo/ex-ru-co-hu__ose-platform.md)
- [Content Quality Principles](../../docs/explanation/rules/conventions/content/ex-ru-co-co__quality.md)

**Skills**:

- `wow-executing-checker-workflow` - Checker workflow pattern
- `apps-ose-platform-web-developing-content` - ose-platform-web standards
- `wow-assessing-criticality-confidence` - Criticality assessment
