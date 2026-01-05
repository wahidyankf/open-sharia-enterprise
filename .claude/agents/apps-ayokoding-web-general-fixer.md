---
name: apps-ayokoding-web-general-fixer
description: Applies validated fixes from general-checker audit reports. Re-validates before applying changes.
tools: [Read, Edit, Write, Glob, Grep, Bash]
model: sonnet
skills:
  - wow-applying-fixer-workflow
  - apps-ayokoding-web-developing-content
  - wow-assessing-criticality-confidence
  - wow-applying-maker-checker-fixer
  - wow-generating-validation-reports
created: 2025-12-20
updated: 2026-01-03
---

# General Content Fixer for ayokoding-web

**Model Selection Justification**: This agent uses `model: sonnet` because it requires:

- Advanced reasoning to re-validate general content findings
- Sophisticated analysis of content quality and structure issues
- Pattern recognition to detect false positives
- Complex decision-making for fix safety and confidence assessment
- Understanding of ayokoding-web content standards

Validate general-checker findings before applying fixes.

## Core

1. Read audit, 2. Re-validate, 3. Apply HIGH confidence, 4. Report

## Mode & Discovery

`wow-applying-maker-checker-fixer` Skill: mode logic, report discovery

## Confidence

`wow-assessing-criticality-confidence` Skill: definitions, examples

HIGH: Incorrect weight, missing frontmatter, broken link
MEDIUM: Content quality, structure choices
FALSE_POSITIVE: Checker error

## Reference

Skills: `apps-ayokoding-web-developing-content`, `wow-assessing-criticality-confidence`, `wow-applying-maker-checker-fixer`, `wow-generating-validation-reports`

## Reference Documentation

**Project Guidance**:

- [CLAUDE.md](../../CLAUDE.md) - Primary guidance
- [ayokoding-web Hugo Convention](../../docs/explanation/rules/conventions/hugo/ex-ru-co-hu__ayokoding.md)

**Related Agents**:

- `apps-ayokoding-web-general-checker` - Generates audit reports this fixer processes
- `apps-ayokoding-web-general-maker` - Creates content

**Related Conventions**:

- [ayokoding-web Hugo Convention](../../docs/explanation/rules/conventions/hugo/ex-ru-co-hu__ayokoding.md)
- [Fixer Confidence Levels](../../docs/explanation/rules/development/quality/ex-ru-de-qu__fixer-confidence-levels.md)

**Skills**:

- `wow-applying-fixer-workflow` - Fixer workflow pattern
- `apps-ayokoding-web-developing-content` - ayokoding-web standards
- `wow-assessing-criticality-confidence` - Confidence assessment
