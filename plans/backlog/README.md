# Backlog Plans

Planned projects for future implementation.

## Planned Projects

- **[Policy-as-Code Governance Layer](./2025-12-25__policy-as-a-code/)** (2025-12-25)
  - Introduce machine-readable governance rules (YAML policies) to eliminate 3x rule duplication across 46 agents
  - Target: 50-58% agent line reduction via Go PolicyEngine (Cobra CLI)
  - Status: Quality gate passed (all MEDIUM issues resolved), 9 MINOR issues remain
  - Ready for Phase 0 (Foundation) implementation

- **[Agent and Skill Definitions as Documentation Source of Truth](./2026-01-04__agents-docs-source-of-truth/)** (2026-01-04)
  - Move agent and skill definitions from tool-specific formats (.claude/agents/, .claude/skills/) to docs/explanation/ as tool-agnostic source of truth, then sync to Claude Code and OpenCode formats
  - Complexity: High (architectural change affecting 45 agents + 23 skills)
  - Status: Backlog (ready when policy-as-code governance layer completes)

## Instructions

**Quick Idea Capture**: For 1-3 liner ideas not ready for formal planning, use `../ideas.md`.

When creating a new plan:

1. Create folder: `YYYY-MM-DD__[project-identifier]/`
2. Add standard files: README.md, requirements.md, tech-docs.md, delivery.md
3. Add the plan to this list
