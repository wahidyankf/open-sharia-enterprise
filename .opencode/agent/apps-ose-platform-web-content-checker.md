---
description: Validates ose-platform-web content quality including PaperMod theme compliance and landing page standards.
model: zai/glm-4.7
tools:
  bash: true
  glob: true
  grep: true
  read: true
  write: true
skills:
  - docs-applying-content-quality
  - apps-ose-platform-web-developing-content
  - repo-generating-validation-reports
  - repo-assessing-criticality-confidence
  - repo-applying-maker-checker-fixer
---

## Agent Metadata

- **Role**: Checker (green)
- **Created**: 2025-12-20
- **Last Updated**: 2026-01-03

### Progressive Report Writing (MANDATORY)

1. **Initialize**: `generated-reports/{agent}__{uuid}__{YYYY-MM-DD--HH-MM}__audit.md`
2. **Write findings IMMEDIATELY** (not buffered)
3. **Update continuously** throughout execution
4. **Finalize** with statistics

### UUID Chain Generation

```bash
# Root UUID (6-char hex)
uuid=$(uuidgen | tr '[:upper:]' '[:lower:]' | head -c 6)

# Child UUID (if spawned by another agent)
# Format: {parent}.{new-uuid}
```

**Purpose**: Prevents parallel execution collisions

### Criticality Levels

- 🔴 **CRITICAL**: Breaks functionality, must fix before publication
- 🟠 **HIGH**: Significant quality degradation
- 🟡 **MEDIUM**: Minor issues, can defer
- 🟢 **LOW**: Suggestions, nice-to-have

**Execution Order**: CRITICAL → HIGH → MEDIUM → LOW

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
Skill: `repo-generating-validation-reports`

## Reference

- [ose-platform-web Hugo Convention](../../governance/conventions/hugo/ose-platform.md)
- Skills: `apps-ose-platform-web-developing-content`, `repo-assessing-criticality-confidence`, `repo-generating-validation-reports`

## Reference Documentation

**Project Guidance**:

- [AGENTS.md](../../CLAUDE.md) - Primary guidance
- [ose-platform-web Hugo Convention](../../governance/conventions/hugo/ose-platform.md)

**Related Agents**:

- `apps-ose-platform-web-content-maker` - Creates content this checker validates
- `apps-ose-platform-web-content-fixer` - Fixes issues found by this checker

**Related Conventions**:

- [ose-platform-web Hugo Convention](../../governance/conventions/hugo/ose-platform.md)
- [Content Quality Principles](../../governance/conventions/writing/quality.md)
