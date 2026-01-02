---
title: "Skills Layer Implementation - Progressive Knowledge System"
status: Backlog
created: 2026-01-02
git-workflow: Trunk Based Development (main branch)
delivery-type: Multi-Phase Plan (4 sequential phases with direct commits)
---

# Skills Layer Implementation - Progressive Knowledge System

## Overview

### Problem Statement

The repository faces important knowledge management opportunities:

1. **CLAUDE.md Future Growth** - Currently ~29k characters (below 30k target), but needs progressive disclosure to prevent future growth
2. **Knowledge Duplication** - Same conventions repeated across 45+ agent files
3. **Agent File Size Pressure** - Three-tier limits (800/1200/1800 lines) becoming constraining
4. **No Progressive Disclosure** - All knowledge loaded upfront, no on-demand depth
5. **Manual Knowledge Loading** - Cannot provide simple overview with optional deep-dive

### Proposed Solution: Skills as Layer 4

Introduce **Claude Code Skills** as a new architectural layer between Development Practices (Layer 3) and AI Agents (Layer 5):

```
Layer 3: Development (HOW - Software Practices)
    ↓ governs
Layer 4: Skills (WHAT+HOW - Specialized Knowledge Packages)
    ↓ provides knowledge to
Layer 5: AI Agents (WHO - Atomic Executors)
```

**Skills** are model-invoked markdown-based knowledge packages that:

- **Auto-load based on description matching** - Claude decides when to use them (unlike user/workflow-invoked agents)
- **Enable progressive disclosure** - Load name/description at startup, full content only when needed
- **Reduce CLAUDE.md size** - Move detailed knowledge to Skills, keep high-level navigation
- **Eliminate agent duplication** - Agents reference Skills instead of duplicating knowledge
- **Support knowledge composition** - Multiple Skills work together seamlessly
- **Portable across platforms** - Open standard (agentskills.io) works beyond Claude

### Goals

**Primary Objectives:**

1. **Implement progressive disclosure** via Skills to prevent future CLAUDE.md growth (currently ~29k, maintain below 30k target)
2. **Create 8-12 high-value Skills** encoding critical repository knowledge
3. **Update architecture documentation** to include Layer 4
4. **Enable agent knowledge references** via Skills frontmatter
5. **Maintain backward compatibility** during migration

**Secondary Objectives:**

1. **Establish Skills creation patterns** for future knowledge packaging
2. **Document Skills governance model** within seven-layer architecture
3. **Enable community knowledge sharing** (Vision alignment: democratize Islamic enterprise)
4. **Demonstrate progressive disclosure** as principle implementation

### Context

**Announcement**: Claude Code Skills launched December 2025 as new capability

**Standard**: Open standard at agentskills.io enables portability beyond Claude ecosystem

**Repository State**: 45+ agents, 24 conventions, 15 development practices, approaching CLAUDE.md size limits

**Alignment**: Implements Progressive Disclosure, Automation Over Manual, Documentation First principles

## Git Workflow

**Trunk Based Development**: All work happens on `main` branch with small, frequent commits. No feature branches unless absolutely necessary. Use feature flags to hide incomplete work if needed.

See [Trunk Based Development Convention](../../docs/explanation/development/workflow/ex-de-wo__trunk-based-development.md) for complete details.

## Delivery Type

**Multi-Phase Plan (4 Sequential Phases)**

This plan implements Skills Layer through 4 sequential phases with direct commits to `main` branch:

1. **Phase 1: Foundation** - Skills directory structure, first 3 core Skills, basic documentation
2. **Phase 2: Knowledge Migration** - 5-9 additional Skills, CLAUDE.md reduction, agent updates
3. **Phase 3: Architecture Integration** - Layer 4 documentation, governance model, conventions
4. **Phase 4: Community & Polish** - Shariah-compliance Skills, examples, final validation

**Dependencies**: Each phase builds on the previous one; validation checkpoint required before starting next phase.

**Rationale for Multi-Phase**:

- Large scope (8-12 Skills, architecture changes, 45+ agent updates)
- Natural breakpoints for validation and feedback
- Phased rollout reduces risk of breaking existing workflows
- Small, frequent commits to `main` with validation gates between phases
- Enables iterative improvement based on early Skills usage

## Quick Links

- [Requirements](./requirements.md) - Detailed requirements and objectives
- [Technical Documentation](./tech-docs.md) - Architecture, design decisions, implementation approach
- [Delivery Plan](./delivery.md) - Implementation phases, validation, acceptance criteria

## Success Metrics

1. **CLAUDE.md Size**: Maintained at ≤30k characters (currently ~29k, progressive disclosure prevents future growth)
2. **Agent File Size**: Average agent size reduced by 15-25% through Skills references
3. **Knowledge Accessibility**: All critical conventions accessible via Skills (100% coverage)
4. **Backward Compatibility**: Zero breaking changes to existing agent workflows
5. **Community Value**: At least 2 Shariah-compliance Skills published for community use

## Context Documents

**Core Principles Implemented:**

- [Progressive Disclosure](../../docs/explanation/principles/content/ex-pr-co__progressive-disclosure.md) - Layer complexity gradually
- [Automation Over Manual](../../docs/explanation/principles/software-engineering/ex-pr-se__automation-over-manual.md) - Claude auto-loads Skills based on context
- [Documentation First](../../docs/explanation/principles/content/ex-pr-co__documentation-first.md) - Skills encode knowledge systematically
- [Explicit Over Implicit](../../docs/explanation/principles/software-engineering/ex-pr-se__explicit-over-implicit.md) - Clear Skills descriptions enable precise auto-loading

**Key Conventions:**

- [Plans Organization Convention](../../docs/explanation/conventions/project/ex-co-pr__plans-organization.md) - Multi-file structure for complex plans
- [AI Agents Convention](../../docs/explanation/development/agents/ex-de-ag__ai-agents.md) - Agent structure, frontmatter, Skills references
- [Repository Architecture](../../docs/explanation/ex__repository-governance-architecture.md) - Seven-layer hierarchy context

---

**Created**: 2026-01-02
**Status**: Backlog
**Delivery**: Multi-Phase (4 sequential phases with direct commits to main)
