# Policy-as-Code Governance Layer

## Overview

This plan introduces a **Policy Layer 3.5** between Conventions/Development (Layer 2-3) and AI Agents (Layer 4) to enable machine-readable, version-controlled governance rules. This addresses the critical pain point of **3x rule duplication** across maker-checker-fixer agent families (3,503 lines for one domain alone) while preserving the strong existing six-layer architecture.

**Status**: In Backlog (Quality Gate Passed)
**Created**: 2025-12-25
**Last Updated**: 2026-01-05
**Project Type**: Architectural Enhancement
**Estimated Complexity**: Large (1500+ lines total across 4 files)

## Recent Progress

**2026-01-05**:

- Updated emoji usage convention to allow emojis in CLAUDE.md and agent files
  - Modified 3 files: emoji.md, CLAUDE.md, AGENTS.md
  - Rationale: Emojis enhance scannability for human-readable specifications
  - Validated via wow**rules**quality-gate (OCD mode) - 0 findings
- Committed and pushed emoji convention update (d04656f)
- Repository state: Clean, all rules compliant

**2025-12-28**:

- Validated repository health via wow**rules**quality-gate (strict mode)
  - Result: PASS (0 CRITICAL/HIGH/MEDIUM issues, 100% compliance)
  - All 6 layers properly documented with complete traceability
  - Agent name-filename matching: 100% (46/46 agents)
- Validated documentation quality via docs\_\_quality-gate (strict mode)
  - Factual accuracy: 12 claims verified, 2 MEDIUM issues fixed
  - Tutorial structure: Migration complete to ayokoding-web
  - Link validity: 103 external URLs verified, 0 broken links
- Ready for plan\_\_quality-gate workflow validation

**2025-12-27**:

- Updated agent counts: 46 agents (up from 34+) across 7 families
- Updated baseline measurements:
  - repo-rules family: 2,047 lines (Claude Code: 852, OpenCode: 1,195)
  - ayokoding-web family: 3,720 lines (Claude Code: 1,445, OpenCode: 2,275)
  - docs family: 1,867 lines (checker: 1,289, fixer: 578)
- Documented UUID chain tracking for parallel execution coordination
- Clarified bilingual non-mirroring policy for ayokoding-web (Phase 3 impact)
- Verified Go 1.24 remains current for PolicyEngine implementation

**2025-12-26**:

- Plan moved to `in-progress/` directory after initial quality review
- Repository governance architecture documentation completed (Layer 0-5 with Vision)

**Next Steps**:

- Run plan\_\_quality-gate workflow on this plan (strict mode)
- Address any findings from quality gate validation
- Begin Phase 0 implementation upon plan approval

---

## Quality Gate Status

**Last Quality Gate**: 2026-01-05
**Status**: ✅ PASSED (all MEDIUM issues resolved)
**Findings**: 10 total (0 HIGH, 0 MEDIUM, 9 MINOR)
**Issues Resolved**:

- ✅ Claude Code agent naming (6 references fixed)
- ✅ repo-rules baseline measurements (8 references fixed)
- ✅ Phase 1 targets updated (6 references fixed)
  **Remaining MINOR Issues** (9):
- Skills count mismatch
- Agent count discrepancies
- Line count minor variations
- Go version update needed
- See audit reports for details

**Audit Reports**:

- Initial: `generated-reports/plan__2026-01-05--12-27__audit.md`
- Fix Report: `generated-reports/plan__2026-01-05--12-27__fix.md`
- Re-validation: `generated-reports/plan__2026-01-05--12-30__audit.md`

## Problem Statement

The current governance architecture relies on prose-based rules embedded in 46 AI agents across 7 families (repo-rules, docs, ayokoding-web, ose-platform-web-content, readme, plan, docs-tutorial). Agents exist on both platforms:

- **Claude Code**: `.claude/agents/*.md` (46 agents, 9,299 total lines)
- **OpenCode**: `.opencode/agent/*.md` (46 agents, 10,695 total lines)
- **Identical agent names** across both platforms for consistency
- **Shared Skills infrastructure**: 18 Skills in `.claude/skills/` accessible by both platforms

This creates:

- **3x duplication**: Same rule embedded in maker, checker, and fixer agents
- **Inconsistency risk**: Each agent interprets prose rules differently
- **Maintenance burden**: Convention changes require updating 3-7 agent files
- **No coverage tracking**: Can't determine which rules are enforced vs. theoretical
- **Agent bloat**: Checkers range from 373-641 lines (OpenCode larger due to Bash tool usage patterns)

## Proposed Solution

**Big-bang migration directly to main branch** using **embedded YAML policies** in existing convention markdown files, consumed via a **Go PolicyEngine** in new `apps/governance-cli/` app built with Cobra framework. All work follows trunk-based development with small, frequent commits to main.

**Platform Compatibility**: Policies will work identically on both Claude Code and OpenCode platforms:

- Both platforms invoke `governance-cli` via Bash tool
- Both use identical tooling (Read, Write, Edit, Glob, Grep, Bash)
- Skills infrastructure remains separate from policy system (knowledge delivery, not governance)
- OpenCode's `permission.skill` controls Skills access only, not policy consumption

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

- Agent line reduction: 47-58% average
- Policies extracted: 0 → 55-60
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

- [Repository Governance Architecture](../../../docs/explanation/ex-ru__repository-governance-architecture.md) - Six-layer hierarchy

**Conventions:**

- [Plans Organization Convention](../../../rules/conventions/project/plans-organization.md) - Plan structure standards

**Development Practices:**

- [Maker-Checker-Fixer Pattern](../../../rules/development/pattern/maker-checker-fixer.md) - Seven agent families
- [AI Agents Convention](../../../rules/development/agents/ai-agents.md) - Agent standards

## Next Steps

1. Review detailed requirements in [requirements.md](./requirements.md)
2. Understand technical architecture in [tech-docs.md](./tech-docs.md)
3. Study migration strategy in [delivery.md](./delivery.md)
4. Get stakeholder approval to move to `in-progress/`
5. Execute Phase 0: Foundation
