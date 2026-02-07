# AGENTS.md

> ⚠️ **IMPORTANT**: This file documents `.opencode/` configuration which is **AUTO-GENERATED** from `.claude/` (source of truth).
>
> **To make changes**:
>
> 1. Edit agents/skills in `.claude/` directory
> 2. Run: `npm run sync:claude-to-opencode`
> 3. Changes will be synced to `.opencode/` automatically
>
> **See [CLAUDE.md](./CLAUDE.md) for primary documentation** (Claude Code configuration).

Instructions for AI agents working with this repository via OpenCode.

## Project Overview

**open-sharia-enterprise** - Enterprise platform built with Node.js, using **Nx monorepo** structure.

- **Node.js**: 24.11.1 (LTS, managed by Volta)
- **npm**: 11.6.3
- **Monorepo**: Nx with `apps/` and `libs/` structure
- **Git Workflow**: Trunk Based Development (all work on `main` branch)

## Dual-Mode Configuration

This repository maintains **dual compatibility** with both Claude Code and OpenCode:

- **`.claude/`**: Source of truth (PRIMARY) - Edit here first
- **`.opencode/`**: Auto-generated (SECONDARY) - Synced from `.claude/`

**Sync Command**: `npm run sync:claude-to-opencode`

**Format Differences**:

- **Tools**: Claude Code uses arrays `[Read, Write]`, OpenCode uses `{ read: true, write: true }`
- **Models**: Claude Code uses `sonnet`/`haiku`, OpenCode uses `zai/glm-4.7` or `inherit`
- **Skills**: Same format for both systems (SKILL.md)

# AI Agents

## Agent Organization

**55 specialized agents** organized into **7 families**:

1. **Documentation** (10 agents): `docs-maker`, `docs-checker`, `docs-fixer`, `docs-tutorial-maker`, `docs-tutorial-checker`, `docs-tutorial-fixer`, `docs-link-general-checker`, `docs-file-manager`, `docs-software-engineering-separation-checker`, `docs-software-engineering-separation-fixer`
2. **README** (3 agents): `readme-maker`, `readme-checker`, `readme-fixer`
3. **Project Planning** (5 agents): `plan-maker`, `plan-checker`, `plan-executor`, `plan-execution-checker`, `plan-fixer`
4. **Hugo Content - ayokoding-web** (17 agents): Bilingual content creators, validators, deployers
5. **Hugo Content - ose-platform-web** (4 agents): Landing page content creators, validators, deployers
6. **Software Engineering & Specialized** (8 agents): `agent-maker`, `swe-elixir-developer`, `swe-golang-developer`, `swe-hugo-developer`, `swe-java-developer`, `swe-python-developer`, `swe-typescript-developer`, `social-linkedin-post-maker`
7. **Repository Governance** (6 agents): `repo-governance-maker`, `repo-governance-checker`, `repo-governance-fixer`, `repo-workflow-maker`, `repo-workflow-checker`, `repo-workflow-fixer`

**Full agent catalog**: See [`.opencode/agent/README.md`](./.opencode/agent/README.md)

## Agent Format (OpenCode)

OpenCode agents use YAML frontmatter with boolean tool flags:

```yaml
---
description: Brief description of what the agent does
model: zai/glm-4.7 | zai/glm-4.5-air | inherit
tools:
  read: true | false
  grep: true | false
  glob: true | false
  write: true | false
  bash: true | false
  edit: true | false
permission:
  skill:
    skill-name: allow
    another-skill: allow
---
```

**Note**: This format is auto-generated from Claude Code format (tool arrays → boolean flags).

## Maker-Checker-Fixer Pattern

Three-stage quality workflow:

1. **Maker** - Creates content (tools: read, write, edit, glob, grep)
2. **Checker** - Validates content, generates audit reports (tools: read, glob, grep, write for reports)
3. **Fixer** - Applies validated fixes (tools: read, edit, glob, grep)

**Criticality Levels**: CRITICAL, HIGH, MEDIUM, LOW
**Confidence Levels**: HIGH, MEDIUM, FALSE_POSITIVE

**See**: `.opencode/skill/repo-applying-maker-checker-fixer/SKILL.md`

## Skills Integration

**28 skill packages** serve agents through two modes:

**Inline Skills** (default) - Knowledge injection:

- Progressive disclosure of conventions and standards
- Injected into current conversation context
- Examples: `docs-applying-content-quality`, `docs-applying-diataxis-framework`, `docs-creating-accessible-diagrams`

**Fork Skills** (`context: fork`) - Task delegation:

- Spawn isolated agent contexts for focused work
- Delegate specialized tasks (research, analysis, exploration)
- Return summarized results to main conversation
- Act as lightweight orchestrators

**Categories**:

- **Documentation**: `docs-applying-content-quality`, `docs-applying-diataxis-framework`, `docs-creating-accessible-diagrams`
- **Planning**: `plan-creating-project-plans`, `plan-writing-gherkin-criteria`
- **Agent Development**: `agent-developing-agents`, `agent-selecting-models`, `agent-documenting-references`
- **Repository Patterns**: `repo-applying-maker-checker-fixer`, `repo-assessing-criticality-confidence`, `repo-generating-validation-reports`
- **Application-Specific**: `apps-ayokoding-web-developing-content`, `apps-ose-platform-web-developing-content`

**Service Relationship**: Skills serve agents with knowledge and execution but don't govern them (service infrastructure, not governance layer).

**Full skills catalog**: See [`.opencode/skill/README.md`](./.opencode/skill/README.md)

## Security Policy

**Trusted Sources Only**: Only use skills from trusted repositories. All skills in this repository are maintained by the project team.

**Rationale**: Skills execute with agent permissions and can access repository content. Only load skills from verified sources.

## Governance Alignment

All agents follow 10 foundational principles:

1. **Documentation First** - Documentation is mandatory, not optional
2. **Accessibility First** - WCAG AA compliance
3. **Simplicity Over Complexity** - Minimum viable abstraction
4. **Explicit Over Implicit** - Clear tool permissions
5. **Automation Over Manual** - Automate repetitive tasks

**See**: [governance/principles/README.md](./governance/principles/README.md)

## Related Documentation

- **CLAUDE.md** (PRIMARY) - Claude Code configuration and guidance
- **.opencode/agent/README.md** - Complete agent catalog
- **.opencode/skill/README.md** - Complete skills catalog
- **governance/repository-governance-architecture.md** - Six-layer governance hierarchy

---

**Total Agents**: 55
**Total Skills**: 28
**Last Updated**: 2026-02-07
