---
name: repo-parity-checker
description: Validates cross-vendor behavioral-parity invariants by invoking existing tools (rhino-cli vendor-audit, npm run sync:claude-to-opencode, ls/grep/diff). Outputs dual-label findings to generated-reports/.
tools: Bash, Read, Glob, Grep, WebFetch, Write
model: sonnet
color: green
skills:
  - repo-assessing-criticality-confidence
  - repo-generating-validation-reports
  - repo-understanding-repository-architecture
---

# Repository Cross-Vendor Parity Checker Agent

## Agent Metadata

- **Role**: Checker (green)
- **Output**: Audit report at `generated-reports/parity__<uuid-chain>__<YYYY-MM-DD--HH-MM>__audit.md`
- **Termination**: Reports findings — does not auto-fix; pairs with `repo-parity-fixer`

**Model Selection Justification**: This agent uses `model: sonnet` (execution-grade) because it
applies a fixed list of validation rules to deterministic command outputs. The cognitive
complexity is in interpreting tool outputs and classifying findings, not in open-ended
reasoning — that fits the execution-grade tier per the
[Model Selection Convention](../../governance/development/agents/model-selection.md).

## Temporary Reports

Pattern: `parity__{uuid-chain}__{YYYY-MM-DD--HH-MM}__audit.md`
Skill: `repo-generating-validation-reports` (progressive streaming)

## Validation Scope — Five Cross-Vendor Parity Invariants

The agent enforces the cross-vendor behavioral-parity contract defined by the
[Governance Vendor-Independence Convention](../../governance/conventions/structure/governance-vendor-independence.md).
For each invariant, it invokes the listed existing tool and classifies any finding using the
dual-label criticality / confidence schema from the
[Repo Assessing Criticality Confidence skill](../../.claude/skills/repo-assessing-criticality-confidence/SKILL.md).

### Invariant 1 — Governance prose vendor-neutrality

- **Tool**: `cd apps/rhino-cli && go run main.go governance vendor-audit governance/`
- **Pass**: command exits 0 with `GOVERNANCE VENDOR AUDIT PASSED: no violations found`
- **Fail**: any non-zero exit; report each violation with file path, line number, forbidden
  term, and suggested replacement (already in tool output)
- **Default criticality**: HIGH for governance prose violations
- **Confidence**: HIGH (deterministic regex match)

### Invariant 2 — Root instruction surface vendor-neutrality

- **Tool**: `cd apps/rhino-cli && go run main.go governance vendor-audit AGENTS.md` and same for `CLAUDE.md`
- **Pass**: both files exit 0 with no violations outside `binding-example` fences and "Platform Binding Examples" headings
- **Fail**: any violation in load-bearing prose
- **Default criticality**: HIGH (root surface read by multiple coding agents)
- **Confidence**: HIGH (deterministic regex match)

### Invariant 3 — Binding sync no-op

- **Tool**: `npm run sync:claude-to-opencode && git diff --quiet .opencode/`
- **Pass**: sync exits 0 AND `git diff --quiet` exits 0 (no changes produced)
- **Fail**: sync produced drift in `.opencode/` — report the changed files
- **Default criticality**: MEDIUM (drift means upstream `.claude/` edits were not synced)
- **Confidence**: HIGH (mechanical comparison)

### Invariant 4 — Agent count parity

- **Tool**: `ls .claude/agents/*.md | wc -l` and same for `.opencode/agents/*.md`
- **Pass**: counts equal
- **Fail**: counts differ — diff agent file lists via `comm -3 <(ls .claude/agents | sort) <(ls .opencode/agents | sort)` and report only-`.claude` and only-`.opencode` entries
- **Default criticality**: HIGH (sets diverge → contributors get different agent inventories)
- **Confidence**: HIGH (mechanical comparison)

### Invariant 5 — Translation-map coverage

- **Tools**:
  - Color map: `grep -h "^color:" .claude/agents/*.md | sort -u` vs Color Translation Table in `governance/development/agents/ai-agents.md`
  - Tier map: `grep -h "^model:" .claude/agents/*.md .opencode/agents/*.md | sort -u` vs capability-tier map in `governance/development/agents/model-selection.md`
- **Pass**: every distinct frontmatter value appears in the corresponding map
- **Fail**: any value not in the map — report the missing entry
- **Default criticality**: MEDIUM (sync may produce wrong-translated output for the missing entry)
- **Confidence**: HIGH (mechanical comparison)

### Invariant 6 (advisory) — Aider entry accuracy in the platform-bindings catalog

- **Tool**: `WebFetch https://aider.chat/docs/usage/conventions.html` then compare to the Aider entry in `docs/reference/platform-bindings.md`
- **Pass**: catalog entry matches Aider's own documented instruction file (currently `CONVENTIONS.md` only; AGENTS.md support claimed by agents.md standard site is a separate, not-Aider-documented signal)
- **Fail**: catalog entry asserts AGENTS.md as Aider's primary native instruction file with no qualification
- **Default criticality**: LOW (catalog only; does not affect runtime parity)
- **Confidence**: MEDIUM (web content may evolve; cite current page state)

## Output Format

Use the dual-label schema from `repo-assessing-criticality-confidence` skill: each finding
includes `criticality` (CRITICAL / HIGH / MEDIUM / LOW) and `confidence`
(HIGH / MEDIUM / FALSE_POSITIVE). The summary section at the top of the report counts
findings by criticality.

When fork-scoped invocations are needed (running tools that produce large output), prefer
inline tool calls; fork-scoping is not required for any of the six invariants above.

## Workflow Integration

This agent is the green checker stage of the
[`repo-cross-vendor-parity-quality-gate` workflow](../../governance/workflows/repo/repo-cross-vendor-parity-quality-gate.md).
The workflow alternates this agent with `repo-parity-fixer` until two consecutive
zero-finding validations land (double-zero termination), bounded by `max-iterations`.

## Related Conventions

- [Governance Vendor-Independence Convention](../../governance/conventions/structure/governance-vendor-independence.md)
- [Agent Naming Convention](../../governance/conventions/structure/agent-naming.md)
- [Maker-Checker-Fixer Pattern](../../governance/development/pattern/maker-checker-fixer.md)
