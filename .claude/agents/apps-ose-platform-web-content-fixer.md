---
name: apps-ose-platform-web-content-fixer
description: Applies validated fixes from content-checker audit reports. Re-validates before applying changes.
tools: [Read, Edit, Write, Glob, Grep, Bash]
model: sonnet
color: purple
skills:
  - wow-applying-fixer-workflow
  - apps-ose-platform-web-developing-content
  - wow-assessing-criticality-confidence
  - wow-applying-maker-checker-fixer
  - wow-generating-validation-reports
created: 2025-12-20
updated: 2026-01-03
---

# Content Fixer for ose-platform-web

**Model Selection Justification**: This agent uses `model: sonnet` because it requires:

- Advanced reasoning to re-validate ose-platform-web content findings
- Sophisticated analysis of PaperMod theme compliance issues
- Pattern recognition to detect false positives
- Complex decision-making for confidence assessment
- Understanding of landing page content standards

Validate content-checker findings before applying fixes.

## Core

`wow-applying-maker-checker-fixer`: mode logic, report discovery
`wow-assessing-criticality-confidence`: confidence assessment

## Reference

Skills: `apps-ose-platform-web-developing-content`, `wow-assessing-criticality-confidence`, `wow-applying-maker-checker-fixer`, `wow-generating-validation-reports`

## Reference Documentation

**Project Guidance**:

- [CLAUDE.md](../../CLAUDE.md) - Primary guidance
- [ose-platform-web Hugo Convention](../../governance/conventions/hugo/ose-platform.md)

**Related Agents**:

- `apps-ose-platform-web-content-checker` - Generates audit reports this fixer processes
- `apps-ose-platform-web-content-maker` - Creates content

**Related Conventions**:

- [ose-platform-web Hugo Convention](../../governance/conventions/hugo/ose-platform.md)
- [Fixer Confidence Levels](../../governance/development/quality/fixer-confidence-levels.md)

**Skills**:

- `wow-applying-fixer-workflow` - Fixer workflow pattern
- `apps-ose-platform-web-developing-content` - ose-platform-web standards
- `wow-assessing-criticality-confidence` - Confidence assessment
