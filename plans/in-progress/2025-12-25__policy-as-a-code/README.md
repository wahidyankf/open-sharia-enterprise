# Policy-as-Code Governance Layer

## Overview

This plan introduces a **Policy Layer 3.5** between Conventions/Development (Layer 2-3) and AI Agents (Layer 4) to enable machine-readable, version-controlled governance rules. This addresses the critical pain point of **3x rule duplication** across maker-checker-fixer agent families (3,350+ lines for one domain alone) while preserving the strong existing six-layer architecture.

**Status**: In Progress
**Created**: 2025-12-25
**Last Updated**: 2025-12-26
**Project Type**: Architectural Enhancement
**Estimated Complexity**: Large (1500+ lines total across 4 files)

## Recent Progress

**2025-12-26**:

- Plan moved to `in-progress/` directory after initial quality review
- Workflow family (8th family) added to Phase 4 migration scope
- Enhanced scope includes all maker-checker-fixer families plus workflow orchestration
- Repository governance architecture documentation completed (Layer 0-5 with Vision)

**Next Steps**:

- Validate plan completeness and technical accuracy via plan-quality-gate workflow
- Address any findings from quality gate validation
- Begin Phase 0 implementation upon plan approval

## Problem Statement

The current governance architecture relies on prose-based rules embedded in 34+ AI agents across 8 families (repo-rules, docs, ayokoding-web, ose-platform-web-content, readme, plan, docs-tutorial, workflow). This creates:

- **3x duplication**: Same rule embedded in maker, checker, and fixer agents
- **Inconsistency risk**: Each agent interprets prose rules differently
- **Maintenance burden**: Convention changes require updating 3-7 agent files
- **No coverage tracking**: Can't determine which rules are enforced vs. theoretical
- **Agent bloat**: Checkers range from 952-1,644 lines (approaching complexity limits)

## Proposed Solution

**Big-bang migration directly to main branch** using **embedded YAML policies** in existing convention markdown files, consumed via a **Go PolicyEngine** in new `apps/governance-cli/` app built with Cobra framework. All work follows trunk-based development with small, frequent commits to main.

### Key Benefits

- **Eliminates 3x duplication**: Single policy source consumed by all agents
- **50-58% agent size reduction**: Through policy centralization
- **Enables compliance tracking**: Which policies evaluated, which passed/failed
- **Accelerates rule changes**: Single policy update vs. 3-7 agent edits
- **Preserves architecture**: Maintains six-layer hierarchy with clear traceability

### Industry Validation

- 40% faster audit completion with compliance automation (Gartner 2025)
- 70%+ of enterprises integrate automated monitoring by 2025
- Industry standard: Open Policy Agent (OPA) with declarative language
- Best practice: Policies as software artifacts (version control, testing, CI/CD)

## Plan Structure

This plan uses **multi-file structure** due to its comprehensive scope:

1. **[README.md](./README.md)** (this file) - Overview and navigation
2. **[requirements.md](./requirements.md)** - Business context, goals, pain points, and success metrics
3. **[tech-docs.md](./tech-docs.md)** - Technical architecture, policy schema, engine implementation
4. **[delivery.md](./delivery.md)** - Big-bang migration strategy, implementation sequence, and risk mitigation

## Quick Reference

### Proposed Architecture

```
Layer 0: Vision (WHY)
    ↓ inspires
Layer 1: Principles (WHY values)
    ↓ governs
Layer 2: Conventions (WHAT docs) ──┐
Layer 3: Development (HOW software) ─┤
    ↓ both define                    │
Layer 3.5: POLICIES (machine-readable rules) ← NEW
    ↓ consumed by
Layer 4: AI Agents (WHO enforces)
    ↓ orchestrated by
Layer 5: Workflows (WHEN)
```

### Implementation Approach

Big-bang implementation with logical phases completed sequentially on main branch:

- **Phase 0**: Foundation - Policy schema, Go PolicyEngine implementation
- **Phase 1**: Pilot - repo-rules family (59% line reduction)
- **Phase 2**: docs family (45% line reduction)
- **Phase 3**: ayokoding & ose-platform families (58% reduction)
- **Phase 4**: Remaining families (readme, plan, docs-tutorial)
- **Phase 5**: Consolidation - Policy catalog, coverage analyzer, CLI

### Success Metrics

**Quantitative:**

- Agent line reduction: 50-58% average
- Policies extracted: 0 → 55-65
- Agent families migrated: 0/8 → 8/8
- False positive rate: ±5% of baseline
- Execution overhead: <5%

**Qualitative:**

- Rule change propagation: Faster (single update)
- Coverage tracking: Enabled
- Onboarding: Easier (clear rule inventory)
- Consistency: Enforced (shared validation logic)

## Related Documentation

**Current Architecture:**

- [Repository Governance Architecture](../../../docs/explanation/ex__repository-governance-architecture.md) - Six-layer hierarchy

**Conventions:**

- [Plans Organization Convention](../../../docs/explanation/conventions/ex-co__plans-organization.md) - Plan structure standards

**Development Practices:**

- [Maker-Checker-Fixer Pattern](../../../docs/explanation/development/ex-de__maker-checker-fixer-pattern.md) - Seven agent families
- [AI Agents Convention](../../../docs/explanation/development/ex-de__ai-agents.md) - Agent standards

## Next Steps

1. Review detailed requirements in [requirements.md](./requirements.md)
2. Understand technical architecture in [tech-docs.md](./tech-docs.md)
3. Study migration strategy in [delivery.md](./delivery.md)
4. Get stakeholder approval to move to `in-progress/`
5. Execute Phase 0: Foundation
