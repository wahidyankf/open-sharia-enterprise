---
title: "Skills Infrastructure - Progressive Knowledge Delivery"
status: In Progress
created: 2026-01-02
git-workflow: Trunk Based Development (main branch)
delivery-type: Multi-Phase Plan (2 sequential phases with direct commits)
---

# Skills Infrastructure - Progressive Knowledge Delivery

## Overview

### Problem Statement

The repository faces important knowledge management opportunities:

1. **CLAUDE.md Future Growth** - Currently ~29k characters (below 30k target), but needs progressive disclosure to prevent future growth
2. **Knowledge Duplication** - Same conventions repeated across 45+ agent files
3. **Agent File Size Pressure** - Three-tier limits (800/1200/1800 lines) becoming constraining
4. **No Progressive Disclosure** - All knowledge loaded upfront, no on-demand depth
5. **Manual Knowledge Loading** - Cannot provide simple overview with optional deep-dive

### Proposed Solution: Skills as Delivery Infrastructure

Introduce **Claude Code Skills** as a **delivery mechanism** for convention and development knowledge to agents. Skills are infrastructure (like CLAUDE.md), not a governance layer.

```
Current Knowledge Delivery:
┌─────────────────────────────────────────────────────────────┐
│ L2: Conventions ──┬── CLAUDE.md (summaries) ───> L4: Agents │
│                   └── Direct refs (links) ─────> L4: Agents │
│ L3: Development ──┬── CLAUDE.md (summaries) ───> L4: Agents │
│                   └── Direct refs (links) ─────> L4: Agents │
└─────────────────────────────────────────────────────────────┘

With Skills Infrastructure:
┌─────────────────────────────────────────────────────────────┐
│ L2: Conventions ──┬── CLAUDE.md (navigation) ──> L4: Agents │
│                   ├── Skills (progressive) ────> L4: Agents │
│                   └── Direct refs (specific) ──> L4: Agents │
│ L3: Development ──┬── CLAUDE.md (navigation) ──> L4: Agents │
│                   ├── Skills (progressive) ────> L4: Agents │
│                   └── Direct refs (specific) ──> L4: Agents │
└─────────────────────────────────────────────────────────────┘
```

**Key insight**: Skills don't GOVERN agents (like Conventions do). Skills DELIVER knowledge to agents. They're infrastructure, not architecture.

**Skills** are model-invoked markdown-based knowledge packages that:

- **Auto-load based on description matching** - Claude decides when to use them
- **Enable progressive disclosure** - Name/description at startup, full content on-demand
- **Reduce CLAUDE.md size** - Move detailed knowledge to Skills, keep navigation
- **Reduce agent duplication** - Shared knowledge packaged once, referenced by many
- **Support knowledge composition** - Multiple Skills work together seamlessly
- **Portable across platforms** - Open standard (agentskills.io) works beyond Claude

### Why Infrastructure, Not a Layer?

**Governance layers** enforce rules on the layer below:

- Conventions GOVERN how documentation is written
- Development practices GOVERN how code is written
- These create obligations and constraints

**Delivery infrastructure** transports knowledge without governance:

- CLAUDE.md delivers summaries (doesn't govern agents)
- Agent files deliver prompts (doesn't govern workflows)
- Skills deliver packaged knowledge (doesn't govern agents)

**Skills don't enforce rules on agents. Skills serve agents with knowledge.**

### Goals

**Primary Objectives:**

1. **Implement progressive disclosure** via Skills to prevent future CLAUDE.md growth
2. **Create 8-10 high-value Skills** encoding critical repository knowledge
3. **Document Skills as infrastructure** in repository documentation
4. **Add required `skills:` frontmatter to all agents** for composability (can be empty `[]`)
5. **Update rules components** (wow**rules-maker, wow**rules-checker, wow\_\_rules-fixer) for Skills support
6. **Maintain backward compatibility** - agents with empty `skills: []` work correctly

**Secondary Objectives:**

1. **Establish Skills creation patterns** for future knowledge packaging
2. **Demonstrate progressive disclosure** as principle implementation

### Context

**Announcement**: Claude Code Skills launched December 2025 as new capability

**Standard**: Open standard at agentskills.io enables portability beyond Claude ecosystem

**Repository State**: 45+ agents, 24 conventions, 15 development practices

**Architecture**: Six-layer governance (unchanged) with Skills as delivery infrastructure

**Alignment**: Implements Progressive Disclosure, Automation Over Manual, Documentation First principles

## Git Workflow

**Trunk Based Development**: All work happens on `main` branch with small, frequent commits. No feature branches unless absolutely necessary.

See [Trunk Based Development Convention](../../docs/explanation/development/workflow/ex-de-wo__trunk-based-development.md) for complete details.

## Delivery Type

**Multi-Phase Plan (2 Sequential Phases)**

This plan implements Skills infrastructure through 2 sequential phases with direct commits to `main` branch:

1. **Phase 1: Foundation** - Skills directory structure, first 3 core Skills, infrastructure documentation
2. **Phase 2: Knowledge Migration & Polish** - 5-7 additional Skills, CLAUDE.md optimization, all agent updates (required `skills:` field), rules components updates, templates, final validation

**Dependencies**: Phase 2 builds on Phase 1; validation checkpoint required before starting Phase 2.

**Rationale for Multi-Phase**:

- Significant scope (8-10 Skills, infrastructure docs, all ~45 agents, rules components)
- Natural breakpoint for validation and feedback
- Phased rollout reduces risk
- Small, frequent commits to `main` with validation gates between phases

## Quick Links

- [Requirements](./requirements.md) - Detailed requirements and objectives
- [Technical Documentation](./tech-docs.md) - Architecture, design decisions, implementation approach
- [Delivery Plan](./delivery.md) - Implementation phases, validation, acceptance criteria

## Success Metrics

1. **CLAUDE.md Size**: Maintained at ≤30k characters (progressive disclosure prevents growth)
2. **Agent File Size**: Average agent size reduced by 15-25% through Skills references
3. **Agent Composability**: All ~45 agents have required `skills:` frontmatter field
4. **Rules Components**: All three rules agents (maker, checker, fixer) support Skills
5. **Knowledge Accessibility**: Critical conventions accessible via Skills
6. **Backward Compatibility**: Zero breaking changes (agents with `skills: []` work correctly)

## Context Documents

**Core Principles Implemented:**

- [Progressive Disclosure](../../docs/explanation/principles/content/ex-pr-co__progressive-disclosure.md) - Layer complexity gradually
- [Automation Over Manual](../../docs/explanation/principles/software-engineering/ex-pr-se__automation-over-manual.md) - Claude auto-loads Skills based on context
- [Documentation First](../../docs/explanation/principles/content/ex-pr-co__documentation-first.md) - Skills encode knowledge systematically
- [Explicit Over Implicit](../../docs/explanation/principles/software-engineering/ex-pr-se__explicit-over-implicit.md) - Clear Skills descriptions enable precise auto-loading

**Key Conventions:**

- [Plans Organization Convention](../../docs/explanation/conventions/project/ex-co-pr__plans-organization.md) - Multi-file structure for complex plans
- [AI Agents Convention](../../docs/explanation/development/agents/ex-de-ag__ai-agents.md) - Agent structure, frontmatter, Skills references
- [Repository Architecture](../../docs/explanation/ex__repository-governance-architecture.md) - Six-layer hierarchy (unchanged)

---

**Created**: 2026-01-02
**Status**: In Progress
**Delivery**: Multi-Phase (2 sequential phases with direct commits to main)
