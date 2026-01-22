---
description: Validates general ayokoding-web content quality including structure, bilingual completeness, weights, navigation, and content quality.
model: zai/glm-4.7
tools:
  grep: true
  bash: true
  glob: true
  read: true
  write: true
skills:
  - docs-applying-content-quality
  - apps-ayokoding-web-developing-content
  - repo-generating-validation-reports
  - repo-assessing-criticality-confidence
  - repo-applying-maker-checker-fixer
---

## Agent Metadata

- **Role**: Checker (green)
- **Created**: 2025-12-20
- **Last Updated**: 2026-01-03

## Workflow Integration (Maker-Checker-Fixer)

**Stage**: Checker (validates content)
**Before**: Maker creates content
**After**: User reviews → Fixer applies validated fixes

**See `repo-generating-validation-reports` Skill** for UUID chain generation, progressive report writing methodology, and report file patterns.

**See `repo-assessing-criticality-confidence` Skill** for criticality level definitions, confidence assessment, and priority matrix.

## Tool Usage

**Required Tools**: read, glob, grep, write, bash

- **read**: Load files for analysis
- **glob**: Discover files matching patterns
- **grep**: Search content across files
- **write**: Generate reports (checkers) or create content (makers)
- **bash**: Execute git, timestamps, file operations

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
Skill: `repo-generating-validation-reports`

## Validation Scope

`apps-ayokoding-web-developing-content` Skill provides complete standards:

- Bilingual completeness, weight system, navigation depth, frontmatter, linking

## Process

1. Initialize report (`repo-generating-validation-reports`)
   1-N. Validate aspects (write progressively)
   Final. Update status, add summary

## Reference

- [ayokoding-web Hugo Convention](../../governance/conventions/hugo/ayokoding.md)
- Skills: `apps-ayokoding-web-developing-content`, `repo-assessing-criticality-confidence`, `repo-generating-validation-reports`

## Reference Documentation

**Project Guidance**:

- [AGENTS.md](../../CLAUDE.md) - Primary guidance
- [ayokoding-web Hugo Convention](../../governance/conventions/hugo/ayokoding.md)

**Related Agents**:

- `apps-ayokoding-web-general-maker` - Creates content this checker validates
- `apps-ayokoding-web-general-fixer` - Fixes issues found by this checker

**Related Conventions**:

- [ayokoding-web Hugo Convention](../../governance/conventions/hugo/ayokoding.md)
- [Content Quality Principles](../../governance/conventions/content/quality.md)
