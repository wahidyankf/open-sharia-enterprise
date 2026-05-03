# Repo-Rules-Checker `docs/` Coverage Extension

## Context

`repo-rules-checker` today validates `governance/` fully but only two slices of `docs/` (`docs/explanation/README.md` and `docs/explanation/software-engineering/`). The rest of `docs/` (`docs/tutorials/`, `docs/how-to/`, `docs/reference/`, the non-software-engineering parts of `docs/explanation/`, and `docs/metadata/`) is invisible to the rules-governance dimension. Vendor-binding documentation in `docs/reference/platform-bindings.md` [Repo-grounded] silently drifts when `.claude/`, `.opencode/`, root-level `CLAUDE.md`, or `AGENTS.md` change without a paired update — no agent currently catches that drift.

This plan extends `repo-rules-checker` with a new validation step covering all of `docs/` for the rules-governance dimension only (file naming, frontmatter, no-date-metadata, traceability, broken cross-refs to `governance/`, and vendor-binding drift detection). Existing `docs-checker` / `docs-tutorial-checker` / `docs-link-checker` / `docs-software-engineering-separation-checker` retain their domain-specific responsibilities — this extension does not duplicate them.

## Scope

**In scope** (single-subrepo `ose-public`):

- `.claude/agents/repo-rules-checker.md` [Repo-grounded] — add a new validation step (provisional name "Step 8b: Cross-Documentation Rules Governance") covering the full `docs/` tree
- `.claude/agents/repo-rules-fixer.md` [Repo-grounded] — add fix recipes for the new finding categories surfaced by Step 8b
- `.opencode/agents/repo-rules-checker.md` [Repo-grounded] and `.opencode/agents/repo-rules-fixer.md` [Repo-grounded] — auto-synced from `.claude/` via `npm run sync:claude-to-opencode` per the dual-mode configuration in [`AGENTS.md`](../../../AGENTS.md)
- `governance/workflows/repo/repo-rules-quality-gate.md` [Repo-grounded] — update the Scope Clarification block to reflect the expanded coverage (replace the current `Skips: rest of docs/` line with full `docs/` coverage)

**Out of scope**:

- Diátaxis structure / tutorial pedagogy (stays with `docs-tutorial-checker`)
- Factual accuracy of `docs/` content (stays with `docs-checker`)
- Link reachability / dead-link detection (stays with `docs-link-checker`)
- Software-engineering separation between platforms (stays with `docs-software-engineering-separation-checker`)
- Anything in `apps/`, `libs/`, `specs/`, or `governance/` (governance is already covered)
- Any new agent or workflow file (this plan extends one existing agent, no new artifacts)

## Approach Summary

Extend `repo-rules-checker` with one new step that walks the full `docs/` tree applying the universal rules-governance dimension only, plus a dedicated vendor-binding drift check. Mirror the new finding categories in `repo-rules-fixer` so audit findings are auto-fixable where mechanical. Update the workflow scope-clarification block to advertise the new coverage so downstream readers know what the gate now catches.

The vendor-binding drift check is the headline new capability: it parses `docs/reference/platform-bindings.md` (and any future vendor-listing docs identified during execution), extracts cited binding directories / file names / agent count claims, and verifies them against the actual filesystem state on the current commit. Drift between documentation claim and filesystem reality produces a HIGH finding.

## Document Map

- [brd.md](./brd.md) — business rationale (why this matters, affected roles, success metrics, business risks)
- [prd.md](./prd.md) — product requirements (personas, user stories, Gherkin acceptance criteria, in/out scope)
- [tech-docs.md](./tech-docs.md) — technical approach (architecture, design decisions, file impact, dependencies, rollback)
- [delivery.md](./delivery.md) — worktree declaration + phased delivery checklist with execution-grade clarity
