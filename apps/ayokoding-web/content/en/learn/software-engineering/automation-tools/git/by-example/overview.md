---
title: "Overview"
weight: 10000000
date: 2026-03-20T00:00:00+07:00
draft: false
description: "An overview of the Git by-example tutorial series, explaining the structure and coverage of the annotated examples."
tags: ["git", "version-control", "tutorial", "by-example", "code-first"]
---

This series teaches Git through heavily annotated, self-contained examples. Each example demonstrates a concrete Git operation with inline annotations that explain the command, its options, and the resulting state of the repository.

## How This Series Is Organized

Examples are grouped by complexity:

- **Beginner**: Initializing a repository, staging files, committing, viewing history, and working with remotes.
- **Intermediate**: Branching, merging, rebasing, resolving conflicts, stashing, and tagging.
- **Advanced**: Interactive rebase, cherry-picking, bisect, hooks, worktrees, and submodules.

## Structure of Each Example

Every example follows a consistent five-part format:

1. **Brief Explanation** — what the operation does and why it matters (2-3 sentences)
2. **Mermaid Diagram** — visual representation of repository state, branch topology, or workflow (when appropriate)
3. **Heavily Annotated Commands** — shell commands with `# =>` comments describing the effect and resulting state
4. **Key Takeaway** — the core insight to retain from the example (1-2 sentences)
5. **Why It Matters** — production relevance and real-world impact (50-100 words)

## Examples by Level

### Beginner (Examples 1–28)

- [Example 1: Initialize a Local Repository](/en/learn/software-engineering/automation-tools/git/by-example/beginner#example-1-initialize-a-local-repository)
- [Example 2: Configure User Identity](/en/learn/software-engineering/automation-tools/git/by-example/beginner#example-2-configure-user-identity)
- [Example 3: Check Repository Status](/en/learn/software-engineering/automation-tools/git/by-example/beginner#example-3-check-repository-status)
- [Example 4: Stage Files with git add](/en/learn/software-engineering/automation-tools/git/by-example/beginner#example-4-stage-files-with-git-add)
- [Example 5: Create Your First Commit](/en/learn/software-engineering/automation-tools/git/by-example/beginner#example-5-create-your-first-commit)
- [Example 6: View Commit History with git log](/en/learn/software-engineering/automation-tools/git/by-example/beginner#example-6-view-commit-history-with-git-log)
- [Example 7: Formatted git log with Graph](/en/learn/software-engineering/automation-tools/git/by-example/beginner#example-7-formatted-git-log-with-graph)
- [Example 8: Inspect a Specific Commit with git show](/en/learn/software-engineering/automation-tools/git/by-example/beginner#example-8-inspect-a-specific-commit-with-git-show)
- [Example 9: See Unstaged and Staged Differences](/en/learn/software-engineering/automation-tools/git/by-example/beginner#example-9-see-unstaged-and-staged-differences)
- [Example 10: Create a .gitignore File](/en/learn/software-engineering/automation-tools/git/by-example/beginner#example-10-create-a-gitignore-file)
- [Example 11: Restore a File to Last Committed State](/en/learn/software-engineering/automation-tools/git/by-example/beginner#example-11-restore-a-file-to-last-committed-state)
- [Example 12: Unstage a Staged File](/en/learn/software-engineering/automation-tools/git/by-example/beginner#example-12-unstage-a-staged-file)
- [Example 13: Create and List Branches](/en/learn/software-engineering/automation-tools/git/by-example/beginner#example-13-create-and-list-branches)
- [Example 14: Switch Between Branches with git switch](/en/learn/software-engineering/automation-tools/git/by-example/beginner#example-14-switch-between-branches-with-git-switch)
- [Example 15: Create and Switch in One Command](/en/learn/software-engineering/automation-tools/git/by-example/beginner#example-15-create-and-switch-in-one-command)
- [Example 16: Clone a Remote Repository](/en/learn/software-engineering/automation-tools/git/by-example/beginner#example-16-clone-a-remote-repository)
- [Example 17: Inspect Remote Configuration](/en/learn/software-engineering/automation-tools/git/by-example/beginner#example-17-inspect-remote-configuration)
- [Example 18: Push Commits to a Remote](/en/learn/software-engineering/automation-tools/git/by-example/beginner#example-18-push-commits-to-a-remote)
- [Example 19: Fetch Remote Changes](/en/learn/software-engineering/automation-tools/git/by-example/beginner#example-19-fetch-remote-changes)
- [Example 20: Pull Remote Changes](/en/learn/software-engineering/automation-tools/git/by-example/beginner#example-20-pull-remote-changes)
- [Example 21: Understanding HEAD](/en/learn/software-engineering/automation-tools/git/by-example/beginner#example-21-understanding-head)
- [Example 22: Remove Tracked Files with git rm](/en/learn/software-engineering/automation-tools/git/by-example/beginner#example-22-remove-tracked-files-with-git-rm)
- [Example 23: Rename or Move Files with git mv](/en/learn/software-engineering/automation-tools/git/by-example/beginner#example-23-rename-or-move-files-with-git-mv)
- [Example 24: View History of a Specific File](/en/learn/software-engineering/automation-tools/git/by-example/beginner#example-24-view-history-of-a-specific-file)
- [Example 25: Delete a Branch](/en/learn/software-engineering/automation-tools/git/by-example/beginner#example-25-delete-a-branch)
- [Example 26: Fast-Forward Merge](/en/learn/software-engineering/automation-tools/git/by-example/beginner#example-26-fast-forward-merge)
- [Example 27: Detailed git status and Short Format](/en/learn/software-engineering/automation-tools/git/by-example/beginner#example-27-detailed-git-status-and-short-format)
- [Example 28: Search Through Commit History with git log Filters](/en/learn/software-engineering/automation-tools/git/by-example/beginner#example-28-search-through-commit-history-with-git-log-filters)

### Intermediate (Examples 29–57)

- [Example 29: Three-Way Merge](/en/learn/software-engineering/automation-tools/git/by-example/intermediate#example-29-three-way-merge)
- [Example 30: Merge Conflict Resolution](/en/learn/software-engineering/automation-tools/git/by-example/intermediate#example-30-merge-conflict-resolution)
- [Example 31: git rebase (Basic Linear Rebase)](/en/learn/software-engineering/automation-tools/git/by-example/intermediate#example-31-git-rebase-basic-linear-rebase)
- [Example 32: git rebase --onto (Selective Rebase)](/en/learn/software-engineering/automation-tools/git/by-example/intermediate#example-32-git-rebase---onto-selective-rebase)
- [Example 33: git stash save and pop](/en/learn/software-engineering/automation-tools/git/by-example/intermediate#example-33-git-stash-save-and-pop)
- [Example 34: git stash list, apply, and drop](/en/learn/software-engineering/automation-tools/git/by-example/intermediate#example-34-git-stash-list-apply-and-drop)
- [Example 35: git tag (Lightweight and Annotated)](/en/learn/software-engineering/automation-tools/git/by-example/intermediate#example-35-git-tag-lightweight-and-annotated)
- [Example 36: git remote (add, remove, rename)](/en/learn/software-engineering/automation-tools/git/by-example/intermediate#example-36-git-remote-add-remove-rename)
- [Example 37: git remote prune and Tracking Branches](/en/learn/software-engineering/automation-tools/git/by-example/intermediate#example-37-git-remote-prune-and-tracking-branches)
- [Example 38: git cherry-pick](/en/learn/software-engineering/automation-tools/git/by-example/intermediate#example-38-git-cherry-pick)
- [Example 39: git blame](/en/learn/software-engineering/automation-tools/git/by-example/intermediate#example-39-git-blame)
- [Example 40: git reflog](/en/learn/software-engineering/automation-tools/git/by-example/intermediate#example-40-git-reflog)
- [Example 41: git reset --soft, --mixed, --hard](/en/learn/software-engineering/automation-tools/git/by-example/intermediate#example-41-git-reset---soft---mixed---hard)
- [Example 42: git revert](/en/learn/software-engineering/automation-tools/git/by-example/intermediate#example-42-git-revert)
- [Example 43: git clean](/en/learn/software-engineering/automation-tools/git/by-example/intermediate#example-43-git-clean)
- [Example 44: .gitattributes](/en/learn/software-engineering/automation-tools/git/by-example/intermediate#example-44-gitattributes)
- [Example 45: Git Hooks (pre-commit)](/en/learn/software-engineering/automation-tools/git/by-example/intermediate#example-45-git-hooks-pre-commit)
- [Example 46: Git Hooks (commit-msg and pre-push)](/en/learn/software-engineering/automation-tools/git/by-example/intermediate#example-46-git-hooks-commit-msg-and-pre-push)
- [Example 47: git config (Local, Global, System)](/en/learn/software-engineering/automation-tools/git/by-example/intermediate#example-47-git-config-local-global-system)
- [Example 48: Git Aliases](/en/learn/software-engineering/automation-tools/git/by-example/intermediate#example-48-git-aliases)
- [Example 49: git log Advanced (--graph, --oneline, --author, date ranges)](/en/learn/software-engineering/automation-tools/git/by-example/intermediate#example-49-git-log-advanced---graph---oneline---author-date-ranges)
- [Example 50: git diff Advanced (--cached, branch comparison, stat)](/en/learn/software-engineering/automation-tools/git/by-example/intermediate#example-50-git-diff-advanced---cached-branch-comparison-stat)
- [Example 51: Merge Strategies (ours and theirs)](/en/learn/software-engineering/automation-tools/git/by-example/intermediate#example-51-merge-strategies-ours-and-theirs)
- [Example 52: Tracking Branches and Upstream Configuration](/en/learn/software-engineering/automation-tools/git/by-example/intermediate#example-52-tracking-branches-and-upstream-configuration)
- [Example 53: git log with Merge History Filtering](/en/learn/software-engineering/automation-tools/git/by-example/intermediate#example-53-git-log-with-merge-history-filtering)
- [Example 54: git diff with Word-Level and Whitespace Options](/en/learn/software-engineering/automation-tools/git/by-example/intermediate#example-54-git-diff-with-word-level-and-whitespace-options)
- [Example 55: git bisect](/en/learn/software-engineering/automation-tools/git/by-example/intermediate#example-55-git-bisect)
- [Example 56: git shortlog and Contribution Statistics](/en/learn/software-engineering/automation-tools/git/by-example/intermediate#example-56-git-shortlog-and-contribution-statistics)
- [Example 57: git archive and Bundle](/en/learn/software-engineering/automation-tools/git/by-example/intermediate#example-57-git-archive-and-bundle)

### Advanced (Examples 58–88)

- [Example 58: Interactive Rebase — Squash, Fixup, Reword, Edit, Drop](/en/learn/software-engineering/automation-tools/git/by-example/advanced#example-58-interactive-rebase--squash-fixup-reword-edit-drop)
- [Example 59: Interactive Rebase — `edit` to Amend a Mid-History Commit](/en/learn/software-engineering/automation-tools/git/by-example/advanced#example-59-interactive-rebase--edit-to-amend-a-mid-history-commit)
- [Example 60: Git Worktree — Parallel Working Directories](/en/learn/software-engineering/automation-tools/git/by-example/advanced#example-60-git-worktree--parallel-working-directories)
- [Example 61: Git Worktree — Lock, Unlock, and Move](/en/learn/software-engineering/automation-tools/git/by-example/advanced#example-61-git-worktree--lock-unlock-and-move)
- [Example 62: Git Worktree — Bare Repository Workflow](/en/learn/software-engineering/automation-tools/git/by-example/advanced#example-62-git-worktree--bare-repository-workflow)
- [Example 63: Git Worktree — Practical Multi-Task Development](/en/learn/software-engineering/automation-tools/git/by-example/advanced#example-63-git-worktree--practical-multi-task-development)
- [Example 64: Git Bisect — Manual Binary Search for a Regression](/en/learn/software-engineering/automation-tools/git/by-example/advanced#example-64-git-bisect--manual-binary-search-for-a-regression)
- [Example 65: Git Bisect — Automated Binary Search with a Test Script](/en/learn/software-engineering/automation-tools/git/by-example/advanced#example-65-git-bisect--automated-binary-search-with-a-test-script)
- [Example 66: Git Submodule — Add, Clone, Update, foreach, deinit](/en/learn/software-engineering/automation-tools/git/by-example/advanced#example-66-git-submodule--add-clone-update-foreach-deinit)
- [Example 67: Git Subtree — Embed and Split a Project](/en/learn/software-engineering/automation-tools/git/by-example/advanced#example-67-git-subtree--embed-and-split-a-project)
- [Example 68: `git filter-repo` — Rewrite Repository History](/en/learn/software-engineering/automation-tools/git/by-example/advanced#example-68-git-filter-repo--rewrite-repository-history)
- [Example 69: Git Rerere — Record and Reuse Conflict Resolutions](/en/learn/software-engineering/automation-tools/git/by-example/advanced#example-69-git-rerere--record-and-reuse-conflict-resolutions)
- [Example 70: Git Notes — Attach Metadata Without Altering Commits](/en/learn/software-engineering/automation-tools/git/by-example/advanced#example-70-git-notes--attach-metadata-without-altering-commits)
- [Example 71: Git Bundle — Offline Transfer of Repository Data](/en/learn/software-engineering/automation-tools/git/by-example/advanced#example-71-git-bundle--offline-transfer-of-repository-data)
- [Example 72: Git Archive — Export Clean Source Snapshots](/en/learn/software-engineering/automation-tools/git/by-example/advanced#example-72-git-archive--export-clean-source-snapshots)
- [Example 73: Git Sparse-Checkout — Check Out Only a Subdirectory](/en/learn/software-engineering/automation-tools/git/by-example/advanced#example-73-git-sparse-checkout--check-out-only-a-subdirectory)
- [Example 74: Git Maintenance — Automated Background Repository Optimization](/en/learn/software-engineering/automation-tools/git/by-example/advanced#example-74-git-maintenance--automated-background-repository-optimization)
- [Example 75: The Git Object Model — Blobs, Trees, Commits, Tags](/en/learn/software-engineering/automation-tools/git/by-example/advanced#example-75-the-git-object-model--blobs-trees-commits-tags)
- [Example 76: Packfiles and `git gc` — How Git Compresses Object Storage](/en/learn/software-engineering/automation-tools/git/by-example/advanced#example-76-packfiles-and-git-gc--how-git-compresses-object-storage)
- [Example 77: Shallow Clone and Partial Clone — Fast CI Checkouts](/en/learn/software-engineering/automation-tools/git/by-example/advanced#example-77-shallow-clone-and-partial-clone--fast-ci-checkouts)
- [Example 78: Git LFS — Storing Large Binary Files Outside the Object Store](/en/learn/software-engineering/automation-tools/git/by-example/advanced#example-78-git-lfs--storing-large-binary-files-outside-the-object-store)
- [Example 79: Advanced Merge Strategies — Octopus, Recursive with Options, and Ours](/en/learn/software-engineering/automation-tools/git/by-example/advanced#example-79-advanced-merge-strategies--octopus-recursive-with-options-and-ours)
- [Example 80: `git range-diff` — Compare Two Versions of a Patch Series](/en/learn/software-engineering/automation-tools/git/by-example/advanced#example-80-git-range-diff--compare-two-versions-of-a-patch-series)
- [Example 81: Signing Commits with GPG — Verified Authorship](/en/learn/software-engineering/automation-tools/git/by-example/advanced#example-81-signing-commits-with-gpg--verified-authorship)
- [Example 82: `git replace` — Non-Destructive History Grafting](/en/learn/software-engineering/automation-tools/git/by-example/advanced#example-82-git-replace--non-destructive-history-grafting)
- [Example 83: `git fsck` — Filesystem Consistency Check](/en/learn/software-engineering/automation-tools/git/by-example/advanced#example-83-git-fsck--filesystem-consistency-check)
- [Example 84: Monorepo Strategies with Git — Nx, Sparse Checkout, and CODEOWNERS](/en/learn/software-engineering/automation-tools/git/by-example/advanced#example-84-monorepo-strategies-with-git--nx-sparse-checkout-and-codeowners)
- [Example 85: Git Attributes and Custom Diff Drivers in a Monorepo](/en/learn/software-engineering/automation-tools/git/by-example/advanced#example-85-git-attributes-and-custom-diff-drivers-in-a-monorepo)
- [Example 86: Git Hooks in a Team — Husky, lint-staged, and Commit-msg Validation](/en/learn/software-engineering/automation-tools/git/by-example/advanced#example-86-git-hooks-in-a-team--husky-lint-staged-and-commit-msg-validation)
- [Example 87: `git bisect` Combined with Git Notes — Document Regression Findings](/en/learn/software-engineering/automation-tools/git/by-example/advanced#example-87-git-bisect-combined-with-git-notes--document-regression-findings)
- [Example 88: Git Reflog — Recovering Lost Commits and Branches](/en/learn/software-engineering/automation-tools/git/by-example/advanced#example-88-git-reflog--recovering-lost-commits-and-branches)
