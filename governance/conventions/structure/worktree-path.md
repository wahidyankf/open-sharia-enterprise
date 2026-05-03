---
title: "Worktree Path Convention"
description: Defines the worktree directory structure, naming convention, and gitignore requirements for claude --worktree routing
category: explanation
subcategory: conventions
tags:
  - worktree
  - git
  - repository-structure
  - claude
  - hooks
created: 2026-05-03
---

# Worktree Path Convention

This convention establishes the worktree directory structure and routing convention for this repository, ensuring consistent worktree creation via `claude --worktree`.

## Principles Implemented/Respected

This convention implements the following core principles:

- **[Explicit Over Implicit](../../principles/software-engineering/explicit-over-implicit.md)**: Worktree paths are explicitly routed via hook rather than relying on defaults. The routing behavior is documented and reproducible.

- **[Reproducibility First](../../principles/software-engineering/reproducibility.md)**: All worktrees are created in a predictable location (`worktrees/<name>/`) with consistent naming, ensuring reliable git operations and CI/CD integration.

## Purpose

Standardize worktree creation so that `claude --worktree <name>` routes to `worktrees/<name>/` in the repository root (not the default `.claude/worktrees/`). This keeps worktrees visible at the repo root level while gitignoring both the conventional and custom paths.

## Scope

### What This Convention Covers

- **Worktree routing** — Override default `.claude/worktrees/` path to `worktrees/<name>/`
- **Hook mechanism** — `WorktreeCreate` hook implementation
- **Naming convention** — Hook file naming (kebab-case `.sh`)
- **Gitignore requirements** — Both worktree directories gitignored
- **Worktree creation pattern** — How new worktree rules should be added

### What This Convention Does NOT Cover

- **Git worktree low-level operations** — Internal git mechanics (handled by git documentation)
- **Hook development standards** — General hook development (see separate conventions)
- **Worktree naming for users** — User-facing worktree naming guidance (handled by user documentation)

## Standards

### Worktree Directory Structure

Worktrees created via `claude --worktree` MUST be placed under `worktrees/<name>/` in the repository root:

```
<repo-root>/
├── worktrees/              # Custom worktree location
│   └── <name>/             # Individual worktree directories
│       └── (worktree files)
├── .claude/
│   └── worktrees/         # Default location (gitignored, unused)
└── .gitignore              # Both paths must be gitignored
```

### Routing Mechanism

Worktree creation is routed via a `WorktreeCreate` hook:

- **Location**: `.claude/hooks/worktree-create.sh`
- **Naming**: kebab-case with `.sh` extension
- **Protocol**: reads a JSON payload from **stdin** with fields `hook_event_name`, `cwd`, `name`; prints the absolute worktree path to stdout (last line); writes any informational output to stderr; exits `0` on success (non-zero fails creation). The exact field names and stdin transport are dictated by the coding agent platform under which the hook runs — see Platform Binding Compatibility below for the binding-specific reference.
- **Behaviour**: routes the new worktree to `<repo-root>/worktrees/<name>/` instead of the default `.claude/worktrees/<name>/`.

**Hook contract:**

```bash
# Input: JSON payload on stdin, e.g.
#   {"hook_event_name":"WorktreeCreate","cwd":"/path/to/project","name":"my-feature"}
#
# Bash idiom for parsing:
INPUT=$(cat)
NAME=$(printf '%s' "$INPUT" | jq -r '.name // empty')
CWD=$(printf '%s' "$INPUT" | jq -r '.cwd // empty')

# Output: absolute path of the created worktree on stdout (last line)
echo "/path/to/repo/worktrees/$NAME"

# Exit code: 0 on success; non-zero fails worktree creation
```

### Naming Requirements

Worktree hook files MUST follow the pattern:

- **Format**: `<hook-type>.sh` (kebab-case, lowercase)
- **Example**: `worktree-create.sh` (WorktreeCreate hook type)
- **Location**: Always under `.claude/hooks/`

### Gitignore Requirements

Both worktree directories MUST be gitignored:

```gitignore
# .gitignore

# Default Claude worktree location (unused but gitignored for safety)
.claude/worktrees/

# Custom worktree location (active)
worktrees/
```

## Examples

### Correct Hook Registration

```json
// ~/.claude/settings.json (global user config)
{
  "hooks": {
    "WorktreeCreate": "/path/to/repo/.claude/hooks/worktree-create.sh"
  }
}
```

### Good Worktree Path

```
PASS: worktrees/feature-auth/
PASS: worktrees/bugfix-session-timeout/
PASS: worktrees/experiment-new-api/
```

### Bad Worktree Path

```
FAIL: .claude/worktrees/feature-auth/    # Wrong location (default)
FAIL: feature-auth/                     # Missing worktrees/ prefix
FAIL: worktrees/FeatureAuth/            # PascalCase (should be kebab-case)
```

### Hook File Naming

```
PASS: worktree-create.sh        # kebab-case + .sh extension
FAIL: worktreeCreate.sh          # camelCase
FAIL: WorktreeCreate.sh          # PascalCase
FAIL: worktree-create           # missing .sh extension
```

## Special Considerations

### Platform Binding Compatibility

```binding-example
The WorktreeCreate hook is registered in ~/.claude/settings.json. The Claude Code coding agent supports this hook event natively (see https://code.claude.com/docs/en/hooks for the field schema and transport contract); other coding agent platforms that support a `WorktreeCreate` hook with the same JSON-on-stdin contract reuse the same shell script without modification.
```

The hook script itself is platform-agnostic bash with `jq` for JSON parsing (`jq` is part of the doctor minimal toolchain per AGENTS.md), ensuring compatibility across platforms.

### Industry Convention vs. Chosen Approach

The dominant industry convention (per GitWorktree.org, Tower docs, Beej's Guide) places worktrees as **sibling directories** next to the main clone, not inside it:

```
~/projects/
├── myapp/                  # main worktree (original clone)
├── myapp-feature-auth/     # sibling worktree (outside repo)
```

This approach avoids nested-`.git` issues, keeps tools that walk up the directory tree happy, and is the most widely recommended pattern.

**Why `/worktrees/` inside the repo instead:**

- **Hook constraint**: The `WorktreeCreate` hook receives `cwd` (the project root) and resolves paths relative to it. Routing to a sibling path requires computing `..` from the repo root, which is messier and less portable across machines.
- **Dual-platform support**: A single hook registered in `~/.claude/settings.json` serves both platforms without duplication.
- **Simplicity**: Keeping worktrees inside the repo root makes `git worktree list` output scannable and keeps all repo-related state in one place.
- **Future-proofing**: If either platform adds native sibling-path support, this convention can be updated without changing the hook logic.

This is a deliberate pragmatic trade-off, not a lack of awareness of the sibling convention. Revisit if tooling problems emerge.

### Worktree Cleanup

When removing a worktree:

1. Remove the worktree directory: `rm -rf worktrees/<name>/`
2. Prune the git worktree reference: `git worktree prune`
3. Optionally remove the branch: `git branch -D worktree/<name>`

### Multiple Worktrees

The pattern supports multiple concurrent worktrees:

```
worktrees/
├── feature-auth/
├── bugfix-session-timeout/
└── experiment-new-api/
```

## Tools and Automation

Reference agents or tools that interact with this convention:

- **WorktreeCreate hook** (`.claude/hooks/worktree-create.sh`) — Routes `claude --worktree` to custom path
- **repo-rules-checker** — Validates worktree-related rules and gitignore compliance

## References

**Related Conventions:**

- [File Naming Convention](./file-naming.md) — Kebab-case file naming standards
- [Agent Naming Convention](./agent-naming.md) — Agent file naming patterns
- [Workflow Naming Convention](./workflow-naming.md) — Workflow file naming patterns

**Related Documentation:**

- [AGENTS.md](../../../AGENTS.md) — agent configuration
- [Repository Governance Architecture](../../repository-governance-architecture.md) — Six-layer governance hierarchy
