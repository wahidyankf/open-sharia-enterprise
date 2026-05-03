#!/bin/bash
# WorktreeCreate hook — overrides Claude Code's default `.claude/worktrees/` path.
#
# Protocol (per https://code.claude.com/docs/en/hooks):
#   - Input: JSON payload via STDIN, fields { "hook_event_name", "cwd", "name" }.
#   - Output: absolute path of the created worktree on stdout (last line).
#   - stderr: any informational/diagnostic output (does not interfere with the path).
#   - Exit: 0 on success; non-zero fails worktree creation.
#
# Behaviour: routes worktrees to `<repo-root>/worktrees/<name>/` in the parent repo
# of `cwd`, so they sit at the repo root rather than under `.claude/`.

set -e

# Read JSON payload from stdin.
INPUT=$(cat)

# Defensive: if Claude Code passes nothing or an empty stdin, treat as empty object.
if [ -z "$INPUT" ]; then
  INPUT='{}'
fi

# Parse with jq (present in the doctor minimal scope per AGENTS.md).
NAME=$(printf '%s' "$INPUT" | jq -r '.name // empty' 2>/dev/null || true)
CWD=$(printf '%s' "$INPUT" | jq -r '.cwd // empty' 2>/dev/null || true)

# Auto-generate a name when missing so `claude --worktree` (no positional arg) still works.
if [ -z "$NAME" ] || [ "$NAME" = "null" ]; then
  NAME="auto-$(date +%Y%m%d-%H%M%S)"
fi

# Default cwd to the current shell pwd.
if [ -z "$CWD" ] || [ "$CWD" = "null" ]; then
  CWD="$(pwd)"
fi

# Resolve the main repo root (parent of the common .git). Works whether `cwd` is the
# main checkout or sits inside an existing worktree.
GIT_COMMON_DIR=$(cd "$CWD" 2>/dev/null && git rev-parse --git-common-dir 2>/dev/null) || {
  echo "WorktreeCreate hook: not a git repo: $CWD" >&2
  exit 1
}
GIT_COMMON_DIR=$(cd "$CWD" && cd "$GIT_COMMON_DIR" && pwd)
REPO_ROOT=$(dirname "$GIT_COMMON_DIR")

# Custom path: <repo-root>/worktrees/<name>/
WORKTREE_PATH="$REPO_ROOT/worktrees/$NAME"

mkdir -p "$(dirname "$WORKTREE_PATH")"

# Create the worktree if it does not already exist. Branch follows the worktrees/<name>
# convention; existing branch is reused on rerun.
if ! git -C "$REPO_ROOT" worktree list --porcelain | grep -qF "worktree $WORKTREE_PATH"; then
  if git -C "$REPO_ROOT" show-ref --verify --quiet "refs/heads/worktree/$NAME"; then
    git -C "$REPO_ROOT" worktree add "$WORKTREE_PATH" "worktree/$NAME" >&2 2>/dev/null || true
  else
    git -C "$REPO_ROOT" worktree add "$WORKTREE_PATH" -b "worktree/$NAME" >&2 2>/dev/null || true
  fi
fi

# Final line on stdout MUST be the absolute worktree path.
printf '%s\n' "$WORKTREE_PATH"
exit 0
