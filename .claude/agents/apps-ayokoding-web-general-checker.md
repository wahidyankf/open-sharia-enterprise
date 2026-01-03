---
name: apps-ayokoding-web-general-checker
description: Validates general ayokoding-web content quality including structure, bilingual completeness, weights, navigation, and content quality.
tools: [Read, Glob, Grep, Write, Bash]
model: sonnet
color: green
skills:
  [apps__ayokoding-web__developing-content, wow__assessing-criticality-confidence, wow__generating-validation-reports]
created: 2025-12-20
updated: 2026-01-03
---

# General Content Checker for ayokoding-web

Validate general ayokoding-web content quality.

## Temporary Reports

Pattern: `ayokoding-general__{uuid-chain}__{YYYY-MM-DD--HH-MM}__audit.md`
Skill: `wow__generating-validation-reports`

## Validation Scope

`apps__ayokoding-web__developing-content` Skill provides complete standards:

- Bilingual completeness, weight system, navigation depth, frontmatter, linking

## Process

0. Initialize report (`wow__generating-validation-reports`)
   1-N. Validate aspects (write progressively)
   Final. Update status, add summary

## Reference

- [ayokoding-web Hugo Convention](../../docs/explanation/conventions/hugo/ex-co-hu__ayokoding.md)
- Skills: `apps__ayokoding-web__developing-content`, `wow__assessing-criticality-confidence`, `wow__generating-validation-reports`
