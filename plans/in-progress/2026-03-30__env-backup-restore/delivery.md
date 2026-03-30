# Delivery Plan: .env Backup and Restore

## Phase 1: Specs and Internal Package

Write the Gherkin specs first (behavior-driven), then build the internal logic.

- [ ] **1.1** Create `specs/apps/rhino-cli/env/` directory
- [ ] **1.2** Write `specs/apps/rhino-cli/env/env-backup.feature` — all backup scenarios from
      requirements (discovery, custom dir, zero files, symlink skip, oversize skip, inside-repo
      rejection, JSON output)
- [ ] **1.3** Write `specs/apps/rhino-cli/env/env-restore.feature` — all restore scenarios from
      requirements (basic restore, custom dir, missing backup dir, JSON output, only restores .env\*
      files)
- [ ] **1.4** Update `specs/apps/rhino-cli/README.md` — add `env/` row to the structure table
- [ ] **1.5** Create `apps/rhino-cli/internal/envbackup/types.go` — `Options`, `FileEntry`,
      `Result`, `WorktreeInfo` structs
- [ ] **1.6** Create `apps/rhino-cli/internal/envbackup/worktree.go` — `detectWorktree()` function
      that reads `.git` (file vs dir), parses `gitdir:` pointer, resolves worktree name
- [ ] **1.7** Create `apps/rhino-cli/internal/envbackup/worktree_test.go` — unit tests for worktree
      detection (normal repo with `.git/` dir, worktree with `.git` file, invalid `.git` file)
- [ ] **1.8** Create `apps/rhino-cli/internal/envbackup/discover.go` — file discovery walker with
      `DefaultSkipDirs` (node_modules, dist, build, .next, \_\_pycache\_\_, target, vendor, coverage,
      generated-contracts, .gradle, .dart_tool, .cargo, bower_components, etc.), symlink detection,
      size check; uses `filepath.SkipDir` to prune entire subtrees of auto-generated dirs
- [ ] **1.9** Create `apps/rhino-cli/internal/envbackup/discover_test.go` — unit tests for
      discovery using `/tmp` fixtures: verify skip of each auto-generated dir (node_modules, dist,
      build, .next, \_\_pycache\_\_, target, vendor, coverage, generated-contracts), nested skips,
      symlink detection, oversized file detection, non-env dotfile exclusion, sort order
- [ ] **1.10** Create `apps/rhino-cli/internal/envbackup/backup.go` — backup orchestration: resolve
      dir, validate not inside repo, discover, copy; handle `--worktree-aware` namespace
- [ ] **1.11** Create `apps/rhino-cli/internal/envbackup/backup_test.go` — unit tests for backup
      flow using `/tmp` fixtures: basic backup, relative path preservation, permission preservation,
      content integrity, overwrite idempotency, inside-repo rejection, worktree-aware namespacing,
      auto-gen dir exclusion, zero-file result
- [ ] **1.12** Create `apps/rhino-cli/internal/envbackup/restore.go` — restore orchestration:
      resolve dir, validate exists, discover from backup, copy to repo; handle `--worktree-aware`
- [ ] **1.13** Create `apps/rhino-cli/internal/envbackup/restore_test.go` — unit tests for restore
      flow using `/tmp` fixtures: basic restore, dir creation, permission preservation, overwrite,
      missing backup dir error, non-env file filtering, worktree-aware namespace, zero-file result
- [ ] **1.14** Create `apps/rhino-cli/internal/envbackup/reporter.go` — text, json, markdown
      formatters (include worktree name in output when `--worktree-aware`)
- [ ] **1.15** Create `apps/rhino-cli/internal/envbackup/reporter_test.go` — unit tests for
      formatters

## Phase 2: Cobra Commands

Wire the internal package to CLI commands.

- [ ] **2.1** Create `apps/rhino-cli/cmd/env.go` — group command (`env`), register under
      `rootCmd`
- [ ] **2.2** Create `apps/rhino-cli/cmd/env_backup.go` — `env backup` subcommand with `--dir` and
      `--worktree-aware` flags, `RunE` calling `envbackup.Backup()`, output via `writeFormatted()`
- [ ] **2.3** Create `apps/rhino-cli/cmd/env_restore.go` — `env restore` subcommand with `--dir`
      and `--worktree-aware` flags, `RunE` calling `envbackup.Restore()`, output via
      `writeFormatted()`
- [ ] **2.4** Bump version in `cmd/root.go` from `0.13.0` to `0.14.0`

## Phase 3: Integration Tests

BDD integration tests consuming the Gherkin specs.

- [ ] **3.1** Create `apps/rhino-cli/cmd/env_backup.integration_test.go` — godog test runner for
      `env-backup.feature`, fixtures with temp git repo + .env files + symlinks + oversized files +
      worktree fixture (`.git` file with `gitdir:` pointer)
- [ ] **3.2** Create `apps/rhino-cli/cmd/env_restore.integration_test.go` — godog test runner for
      `env-restore.feature`, fixtures with pre-populated backup dir + worktree-namespaced backup

## Phase 4: Documentation and Validation

- [ ] **4.1** Update `apps/rhino-cli/README.md` — add `env backup` and `env restore` to the
      command table
- [ ] **4.2** Run `nx run rhino-cli:test:quick` — verify unit tests pass and coverage >=90%
- [ ] **4.3** Run `nx run rhino-cli:test:integration` — verify all Gherkin scenarios pass
- [ ] **4.4** Run `nx run rhino-cli:lint` — verify no lint issues
- [ ] **4.5** Manual smoke test: run `go run main.go env backup` and `go run main.go env restore`
      against the real repository

## Validation Checklist

- [ ] All Gherkin scenarios in `env-backup.feature` pass
- [ ] All Gherkin scenarios in `env-restore.feature` pass
- [ ] Unit test coverage >=90% for `internal/envbackup/`
- [ ] `nx run rhino-cli:test:quick` passes
- [ ] `nx run rhino-cli:lint` passes
- [ ] `specs/apps/rhino-cli/README.md` updated with `env/` entry
- [ ] `apps/rhino-cli/README.md` updated with new commands
- [ ] Version bumped to 0.14.0
- [ ] No hardcoded paths (all relative to git root or user home)
- [ ] Backup dir outside repo validated
- [ ] Symlinks skipped
- [ ] Oversized files skipped
- [ ] Auto-generated dirs (node_modules, dist, build, .next, \_\_pycache\_\_, target, vendor,
      coverage, generated-contracts, etc.) are pruned — never traversed
- [ ] Nested auto-gen dirs skipped (e.g., `apps/web/node_modules/.env`)
- [ ] Unit tests use `/tmp` fixtures (no real filesystem deps)
- [ ] Integration tests use `/tmp` fixtures with isolated temp dirs per scenario
- [ ] Worktree detection works (`.git` file vs directory)
- [ ] `--worktree-aware` namespaces backup by worktree/repo directory name
- [ ] Worktree-aware restore reads from correct namespace
- [ ] Works from a real `git worktree add` checkout (manual smoke test)
