---
title: "Git Identity From Global Config Convention"
description: Prohibits per-repo [user] overrides in .git/config; git author identity must come exclusively from the global git config
category: explanation
subcategory: development
tags:
  - git
  - identity
  - commits
  - security
  - reproducibility
created: 2026-05-19
---

# Git Identity From Global Config Convention

Git commit authorship must come exclusively from the developer's global git configuration
(`~/.gitconfig` or `~/.config/git/config`). No subrepo in this monorepo may contain a
`[user]` block in its `.git/config`.

## Principles Implemented/Respected

This practice respects the following core principles:

- **[Explicit Over Implicit](../../principles/software-engineering/explicit-over-implicit.md)**:
  Global git config is a deliberate, visible identity declaration that applies consistently
  across all projects. A per-repo override is an implicit, local mutation that silently
  overrides the developer's stated identity without any warning at commit time.

- **[Root Cause Orientation](../../principles/general/root-cause-orientation.md)**: Commits
  attributed to an unintended identity are a symptom of a per-repo override existing in
  `.git/config`. Removing the override at the root eliminates the class of problem entirely,
  rather than rewriting history after the fact.

- **[Reproducibility First](../../principles/software-engineering/reproducibility.md)**:
  A developer's identity must resolve consistently regardless of which subrepo they are
  working in. Implicit per-repo overrides break that consistency.

- **[Automation Over Manual](../../principles/software-engineering/automation-over-manual.md)**:
  A pre-commit hook enforces the rule automatically on every commit attempt, removing any
  dependency on manual audit of `.git/config` files.

## Conventions Implemented/Respected

This practice implements/respects the following conventions:

- **[Code Quality Convention](../quality/code.md)**: Identity enforcement is implemented as a
  Husky pre-commit hook, consistent with the automated quality gate pattern used across the
  repository.

## Background

At one point a `[user]` block was added to a subrepo's `.git/config`, setting the local
`user.name` and `user.email` to values different from the developer's global identity.
Because git resolves local config before global config, every subsequent commit in that
repository was attributed to the override identity — not the developer's real identity.
The problem went undetected across several days and hundreds of commits before anyone
noticed. History had to be rewritten to correct the attribution.

This incident illustrates a structural risk: the override mechanism is silent, requires no
confirmation, and persists until explicitly removed. The only reliable defense is a
pre-commit guard that prevents commits entirely when an override is present.

## Standards

### Standard 1: No `[user]` Section in Any Subrepo `.git/config`

The `[user]` section MUST NOT appear in `.git/config` for any subrepo tracked by this
monorepo (`ose-public`, `ose-infra`, `ose-primer`).

**Violation:**

```ini
# ose-public/.git/config  ← PROHIBITED
[user]
    name  = Some Name
    email = some@email.example
```

**Compliant state:** the `[user]` section is absent from `.git/config` entirely. Git falls
through to the developer's global `~/.gitconfig`.

This rule is **identity-agnostic**: any `[user]` value is a violation, regardless of
whether the identity it encodes happens to match the developer's real identity. The
mechanism itself is disallowed, not just incorrect values.

### Standard 2: Identity Comes From Global Git Config Only

The authoritative source for `user.name` and `user.email` in all subrepos is, in resolution
order:

1. `~/.gitconfig` — the standard per-user global config.
2. `~/.config/git/config` — the XDG-compliant alternative location.
3. System-level `/etc/gitconfig` — for shared CI environments only.

Developers who need a different identity per repository (e.g., work vs. personal projects)
MUST use `includeIf` directives in their global `~/.gitconfig` rather than editing any
subrepo's `.git/config`.

**Per-directory identity via `includeIf` (compliant approach):**

```ini
# ~/.gitconfig

[user]
    name  = Your Name
    email = personal@example.com

[includeIf "gitdir:/path/to/work-projects/"]
    path = ~/.gitconfig-work
```

```ini
# ~/.gitconfig-work

[user]
    name  = Your Name
    email = work@company.example
```

Git applies `~/.gitconfig-work` automatically for any repository whose `.git/` directory
is under `/path/to/work-projects/`. No per-repo `.git/config` edit is required.

**One-off override via environment variables (compliant approach for isolated commits):**

```bash
GIT_AUTHOR_NAME="Your Name" GIT_AUTHOR_EMAIL="other@example.com" \
  GIT_COMMITTER_NAME="Your Name" GIT_COMMITTER_EMAIL="other@example.com" \
  git commit -m "chore: one-off commit under alternate identity"
```

Environment variables override config for a single invocation without touching any
`.git/config` file.

### Standard 3: Pre-Commit Hook Enforces the Rule in `ose-public`

`ose-public` enforces Standard 1 automatically. The guard runs as the first step of the
Husky pre-commit hook so it fails fast before any other hook logic executes.

**Guard script**: `scripts/git-identity-check.sh`

The script calls `git config --local --get user.name` and `git config --local --get
user.email`. If either returns a non-empty value, the commit is aborted with a clear error
message listing the offending values and the exact commands needed to remove them:

```bash
git config --local --unset user.name
git config --local --unset user.email
git config --local --remove-section user 2>/dev/null || true
```

**Husky pre-commit hook** (`.husky/pre-commit`):

```bash
./scripts/git-identity-check.sh
CGO_ENABLED=0 go run -C apps/rhino-cli main.go git pre-commit
```

The identity check runs before `rhino-cli` to ensure an invalid identity cannot slip
through even if later hook steps succeed.

The guard is intentionally **identity-agnostic**: it does not compare against any specific
name or email. It only verifies that no local override of any kind exists.

## Examples

### PASS: Correctly configured global identity

```ini
# ~/.gitconfig — developer's global config
[user]
    name  = Developer Name
    email = developer@example.com
```

```ini
# ose-public/.git/config — no [user] section present
[core]
    repositoryformatversion = 0
    filemode = true
    bare = false
[remote "origin"]
    url = git@github.com:org/ose-public.git
    fetch = +refs/heads/*:refs/remotes/origin/*
[branch "main"]
    remote = origin
    merge = refs/heads/main
```

`git log --format="%an <%ae>"` outputs the global identity. Pre-commit hook exits 0.

### FAIL: Per-repo override present

```ini
# ose-public/.git/config — VIOLATION: [user] section present
[core]
    repositoryformatversion = 0
[user]
    name  = Test
    email = t@test.com
```

Pre-commit hook output:

```text
ERROR: Per-repo git identity override detected in /path/to/ose-public/.git/config

  [user]
      name  = Test
      email = t@test.com

Repo policy is identity-from-global-config only. To clear the override:

  git -C "/path/to/ose-public" config --local --unset user.name
  git -C "/path/to/ose-public" config --local --unset user.email
  git -C "/path/to/ose-public" config --local --remove-section user 2>/dev/null || true

Then retry your commit. The global config from ~/.gitconfig will take effect.
```

Commit is aborted. Developer removes the section and retries.

### PASS: Per-directory identity via `includeIf` (multi-identity workflow)

```ini
# ~/.gitconfig
[user]
    name  = Developer Name
    email = developer@personal.example

[includeIf "gitdir:~/work/"]
    path = ~/.gitconfig-work
```

```ini
# ~/.gitconfig-work
[user]
    name  = Developer Name
    email = developer@work.example
```

Commits inside `~/work/` use the work identity; commits outside use the personal identity.
Neither subrepo's `.git/config` contains a `[user]` section. Pre-commit hook exits 0 in
both contexts.

## Remediation

If the pre-commit hook blocks a commit due to an existing override:

```bash
# Remove name override (if present)
git config --local --unset user.name

# Remove email override (if present)
git config --local --unset user.email

# Remove the section entirely if it is now empty
git config --local --remove-section user 2>/dev/null || true
```

Verify the section is gone:

```bash
git config --local --list | grep "^user\."
# Should produce no output
```

Then retry the commit. The global `~/.gitconfig` takes effect immediately.

## Future Work: Sibling Subrepos

The enforcement described above is **currently active in `ose-public` only**. The two sibling
subrepos tracked from the `ose-projects` parent — `ose-infra` and `ose-primer` — are
expected to adopt identical guards as a future plan:

- Copy `scripts/git-identity-check.sh` into each sibling subrepo.
- Add `./scripts/git-identity-check.sh` as the first line of each sibling's
  `.husky/pre-commit`.
- Audit and remove any existing `[user]` block from each sibling's `.git/config`.

Until that plan executes, developers working in `ose-infra` or `ose-primer` must manually
verify that no `[user]` section exists in those repos' `.git/config` files:

```bash
git -C /path/to/ose-infra config --local --list | grep "^user\." || echo "clean"
git -C /path/to/ose-primer config --local --list | grep "^user\." || echo "clean"
```

## Related Documentation

- [`scripts/git-identity-check.sh`](../../../scripts/git-identity-check.sh) — Identity-agnostic
  guard that enforces this convention at commit time
- [`.husky/pre-commit`](../../../.husky/pre-commit) — Husky hook that invokes the guard as its
  first step
- [Code Quality Convention](../quality/code.md) — Git hooks and pre-commit automation
- [Commit Message Convention](./commit-messages.md) — Conventional Commits format
- [Reproducible Environments Convention](./reproducible-environments.md) — Deterministic
  development environments across machines
- [No Machine-Specific Information in Commits](../quality/no-machine-specific-commits.md) —
  Related constraint preventing machine-specific paths and credentials from entering git history
