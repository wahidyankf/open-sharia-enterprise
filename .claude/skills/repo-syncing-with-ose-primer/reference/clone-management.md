# Clone Management

Procedures for locating, preparing, and mutating the `ose-primer` clone that both sync-maker agents consume.

## Clone path

Sync agents resolve the primer clone path from the `OSE_PRIMER_CLONE` environment variable. Operators export this variable before invoking any sync agent:

```bash
export OSE_PRIMER_CLONE="$HOME/ose-projects/ose-primer"
```

The convention default is `$HOME/ose-projects/ose-primer` (a sibling of the `ose-public` checkout). Operators may override to any location. No committed document may encode an absolute path to a specific machine — `$OSE_PRIMER_CLONE` is the only reference allowed in governance and skill documents.

If `OSE_PRIMER_CLONE` is unset, pre-flight fails with an explicit error: `OSE_PRIMER_CLONE is not set; export it to the path of your local ose-primer clone (default: $HOME/ose-projects/ose-primer)`.

## First-time setup

When the clone does not yet exist at `$OSE_PRIMER_CLONE`, the operator runs:

```bash
mkdir -p "$(dirname "$OSE_PRIMER_CLONE")"
git clone https://github.com/wahidyankf/ose-primer.git "$OSE_PRIMER_CLONE"
```

The sync agent itself never runs `git clone`; cloning is the operator's responsibility.

## Pre-flight steps

Every invocation runs these checks, in order, before any read or write:

1. **Env-var set**: `OSE_PRIMER_CLONE` is set to a non-empty path.
2. **`.git` present**: `$OSE_PRIMER_CLONE/.git` exists and is a directory or a gitdir file.
3. **Origin remote URL check**: `git -C "$OSE_PRIMER_CLONE" remote get-url origin` returns a URL matching `github.com[:/]wahidyankf/ose-primer`. A mismatch aborts.
4. **Fetch prune**: `git -C "$OSE_PRIMER_CLONE" fetch --prune` — propagate transient network failures, but do not retry automatically.
5. **Clean tree**: `git -C "$OSE_PRIMER_CLONE" status --porcelain` returns empty. A dirty tree aborts with an explicit message telling the operator to commit, stash, or reset.
6. **On main**: `git -C "$OSE_PRIMER_CLONE" rev-parse --abbrev-ref HEAD` returns `main`. An off-main state aborts.
7. **Up to date with origin/main**: `git -C "$OSE_PRIMER_CLONE" rev-list --count origin/main..main` returns `0` AND `git -C "$OSE_PRIMER_CLONE" rev-list --count main..origin/main` returns `0`. Drift aborts with an explicit fast-forward / reset instruction.

### Escape hatch

Operators may pass `--use-clone-as-is` to skip checks 5 and 6 (useful for inspecting a local experiment). This flag is explicit, never default, and recorded in the generated report's frontmatter.

## Apply-mode mechanics (git worktrees)

`apply` mode NEVER mutates the clone's main working tree. Instead, it creates a dedicated git worktree:

**Worktree path**: `$OSE_PRIMER_CLONE/.claude/worktrees/sync-<utc-timestamp>-<short-uuid>/`

- `<utc-timestamp>`: `YYYYMMDD-HHMMSS` in UTC.
- `<short-uuid>`: first 8 chars of a freshly generated UUID.

**Branch name**: `sync/<utc-timestamp>-<short-uuid>` (tracks `origin/main`).

**Creation command**:

```bash
git -C "$OSE_PRIMER_CLONE" worktree add \
    -b "sync/<ts>-<uuid>" \
    "$OSE_PRIMER_CLONE/.claude/worktrees/sync-<ts>-<uuid>" \
    origin/main
```

### Rationale for worktrees over branch-in-place

- **Parallel safety**: Multiple apply invocations coexist without stepping on each other's working trees.
- **Clean main state**: The operator can still run local inspection commands in the main clone while the apply is in flight.
- **Cleaner failure recovery**: A failed apply leaves its worktree in place for debugging; no `git reset` on `main` is needed.
- **Gitignore inherited**: `.claude/worktrees/` is gitignored in `ose-public`, and the primer inherits the same pattern — the worktree path never leaks into commits.

### Worktree lifecycle

1. **Create** — as above; inside the apply agent, immediately before the first write.
2. **Work** — all file mutations happen inside the worktree path.
3. **Commit** — one commit per findings group, conventional-commits format, inside the worktree.
4. **Push** — `git -C <worktree> push -u origin <branch>`.
5. **PR open** — `gh pr create --draft --base main --head <branch> --title "..." --body "..."` (from worktree's cwd).
6. **Return** — agent returns worktree path, branch name, PR URL in its report.
7. **Cleanup** — the operator may prune the worktree after the PR merges:

   ```bash
   git -C "$OSE_PRIMER_CLONE" worktree remove "$OSE_PRIMER_CLONE/.claude/worktrees/sync-<ts>-<uuid>"
   ```

   On failure, the worktree is preserved so the operator can inspect it. The agent never auto-removes a worktree.

## Worktree hygiene

Pre-flight inspects `git -C "$OSE_PRIMER_CLONE" worktree list` and reports:

- **Warning** if any worktree is older than 7 days by mtime.
- **Refusal** if stale worktrees (> 7 days) exceed 5 in count; the operator must prune before a new apply runs.

Both rules protect the clone from accumulating abandoned state.

## Cleanup commands reference

```bash
git -C "$OSE_PRIMER_CLONE" worktree list                 # enumerate
git -C "$OSE_PRIMER_CLONE" worktree remove <path>         # remove a specific worktree
git -C "$OSE_PRIMER_CLONE" worktree prune                 # remove administrative files for deleted worktrees
```

The agent documents in its report the exact commands the operator should run post-merge — no hidden magic.
