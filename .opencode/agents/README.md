# OpenCode Agents

This directory contains the OpenCode-format mirror of agent definitions
maintained in [`.claude/agents/`](../../.claude/agents/README.md). The canonical
catalog and per-agent role descriptions live in the primary binding's README.

## Source of truth

- **Primary** (canonical): `.claude/agents/<name>.md`
- **Secondary** (auto-synced): `.opencode/agents/<name>.md`

Edit primary-binding files only. Run `npm run sync:claude-to-opencode` to
regenerate this directory. Any change made here will be overwritten on the next
sync.

## Format differences from primary binding

The mechanical translations (color names → theme tokens, model aliases →
provider model IDs, tool arrays → tool flags) are documented in
[`docs/reference/platform-bindings.md`](../../docs/reference/platform-bindings.md)
and implemented by `apps/rhino-cli/internal/agents/types.go`.

## Behavioral parity

The cross-vendor behavioral-parity contract that keeps this directory in sync
with `.claude/agents/` is documented in
[`governance/workflows/repo/`](../../governance/workflows/repo/) (the parity
quality-gate workflow lands as part of the cross-vendor-agent-parity plan).
