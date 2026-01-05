---
name: apps-ayokoding-web-general-checker
description: Validates general ayokoding-web content quality including structure, bilingual completeness, weights, navigation, and content quality.
tools: [Read, Glob, Grep, Write, Bash]
model: sonnet
color: green
skills:
  [
    apps-ayokoding-web-developing-content,
    wow-assessing-criticality-confidence,
    wow-generating-validation-reports,
    wow-executing-checker-workflow,
  ]
created: 2025-12-20
updated: 2026-01-03
---

# General Content Checker for ayokoding-web

**Model Selection Justification**: This agent uses `model: sonnet` because it requires:

- Advanced reasoning to validate general content quality and structure
- Sophisticated analysis of bilingual completeness and navigation
- Pattern recognition for weight ordering and level-based organization
- Complex decision-making for content standards compliance
- Multi-step validation workflow across multiple content dimensions

Validate general ayokoding-web content quality.

## Temporary Reports

Pattern: `ayokoding-general-{uuid-chain}-{YYYY-MM-DD--HH-MM}-audit.md`
Skill: `wow-generating-validation-reports`

## Validation Scope

`apps-ayokoding-web-developing-content` Skill provides complete standards:

- Bilingual completeness, weight system, navigation depth, frontmatter, linking

## Process

0. Initialize report (`wow-generating-validation-reports`)
   1-N. Validate aspects (write progressively)
   Final. Update status, add summary

## Reference

- [ayokoding-web Hugo Convention](../../docs/explanation/rules/conventions/hugo/ex-ru-co-hu__ayokoding.md)
- Skills: `apps-ayokoding-web-developing-content`, `wow-assessing-criticality-confidence`, `wow-generating-validation-reports`
