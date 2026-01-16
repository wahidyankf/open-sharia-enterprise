# OpenCode Skills Index

> ⚠️ **AUTO-SYNCED**: This directory (`.opencode/skill/`) is automatically synced from `.claude/skills/` (source of truth).
>
> **DO NOT EDIT DIRECTLY**. To make changes:
>
> 1. Edit skills in `.claude/skills/` directory
> 2. Run: `npm run sync:claude-to-opencode`
> 3. Changes will be copied here automatically (direct copy, no conversion)
>
> **See [.claude/skills/README.md](../../.claude/skills/README.md) for primary skills documentation**.

---

## Skills Format

Skills use identical format for both Claude Code and OpenCode systems:

**SKILL.md structure**:

```yaml
---
description: Brief description for progressive disclosure
---

# Skill Name

## Purpose
When to use this skill

## Content
Detailed guidance, standards, examples

## References
Links to conventions, related skills
```

## All 23 Skills

Skills are direct copies from `.claude/skills/`. For complete documentation, see [.claude/skills/README.md](../../.claude/skills/README.md).

### Skill Categories

- **Documentation Skills** (6): docs-applying-content-quality, docs-applying-diataxis-framework, docs-creating-accessible-diagrams, docs-creating-by-example-tutorials, docs-validating-factual-accuracy, docs-validating-links
- **README Skills** (1): readme-writing-readme-files
- **Planning Skills** (2): plan-creating-project-plans, plan-writing-gherkin-criteria
- **Agent Development Skills** (3): agent-developing-agents, agent-documenting-references, agent-selecting-models
- **Repository Pattern Skills** (7): repo-applying-maker-checker-fixer, repo-applying-fixer-workflow, repo-assessing-criticality-confidence, repo-defining-workflows, repo-executing-checker-workflow, repo-generating-validation-reports, repo-practicing-trunk-based-development, repo-understanding-repository-architecture
- **Application-Specific Skills** (2): apps-ayokoding-web-developing-content, apps-ose-platform-web-developing-content
- **Frontend Skills** (1): frontend-design
- **Multi-File Template** (1): repo-multi-file-template

---

**Total Skills**: 23
**Format**: SKILL.md (identical for both Claude Code and OpenCode)
**Last Updated**: 2026-01-16
