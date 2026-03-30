# Delivery Plan: Enhanced Env Backup/Restore

## Phase 1: Specs — Add New Gherkin Scenarios

Write the Gherkin scenarios first (behavior-driven), then build the implementation.

- [ ] **1.1** Append 4 `@env-backup-confirm` scenarios to
      `specs/apps/rhino-cli/env/env-backup.feature` — confirm overwrite, decline overwrite, force
      flag skips prompt, no-conflict proceeds without prompt
- [ ] **1.2** Append 3 `@env-backup-config` scenarios to
      `specs/apps/rhino-cli/env/env-backup.feature` — include-config backs up config files,
      without flag ignores config, no config files found
- [ ] **1.3** Append 4 `@env-restore-confirm` scenarios to
      `specs/apps/rhino-cli/env/env-restore.feature` — confirm overwrite, decline overwrite, force
      flag skips prompt, no-conflict proceeds without prompt
- [ ] **1.4** Append 2 `@env-restore-config` scenarios to
      `specs/apps/rhino-cli/env/env-restore.feature` — include-config restores config files,
      without flag ignores config in backup

## Phase 1B: Rename Default Backup Directory

Rename the default backup directory from `ose-env-bkup` to `ose-open-env-backup` for clarity.

- [ ] **1B.1** Update `apps/rhino-cli/internal/envbackup/types.go`: change `DefaultBackupDir`
      from `"ose-env-bkup"` to `"ose-open-env-backup"`
- [ ] **1B.2** Update `apps/rhino-cli/cmd/env_backup.go`: update `--dir` flag help text
      (`default: ~/ose-open-env-backup`)
- [ ] **1B.3** Update `apps/rhino-cli/cmd/env_restore.go`: update `--dir` flag help text
- [ ] **1B.4** Update `apps/rhino-cli/README.md`: update default directory references
- [ ] **1B.5** Verify existing unit and integration tests still pass (they use temp dirs, not the
      default name, so no test changes expected)

## Phase 2: Internal Package — Confirmation Logic

- [ ] **2.1** Update `apps/rhino-cli/internal/envbackup/types.go`:
  - Add `Force bool`, `IncludeConfig bool`, `ConfirmFn func(existing []string) bool` to `Options`
  - Add `Source string` field to `FileEntry` (with `json:"source,omitempty"`)
  - Add `Cancelled bool` field to `Result` (with `json:"cancelled,omitempty"`)
- [ ] **2.2** Create `apps/rhino-cli/internal/envbackup/confirm.go`:
  - `FindExisting(entries []FileEntry, destRoot string) []string` — check which destination
    files already exist
  - `DefaultConfirmFn(r io.Reader, w io.Writer) func([]string) bool` — read stdin, print
    conflict list, return true only for `y`/`yes` (case-insensitive)
- [ ] **2.3** Create `apps/rhino-cli/internal/envbackup/confirm_test.go`:
  - Test `FindExisting` with mix of existing and non-existing files
  - Test `FindExisting` skips entries with `Skipped: true`
  - Test `DefaultConfirmFn` with inputs: `"y\n"`, `"Y\n"`, `"yes\n"`, `"YES\n"`, `"Yes\n"`
    (all confirm)
  - Test `DefaultConfirmFn` with inputs: `"n\n"`, `"N\n"`, `"\n"`, `"anything\n"`, `"no\n"`
    (all decline)
  - Test `DefaultConfirmFn` writes conflict list to writer

## Phase 3: Internal Package — Config Discovery

- [ ] **3.1** Create `apps/rhino-cli/internal/envbackup/config.go`:
  - `ConfigPattern` struct (`RelPath`, `Description`, `Category`)
  - `DefaultConfigPatterns` package-level variable (14 patterns from FR-5)
  - `DiscoverConfig(repoRoot string, patterns []ConfigPattern, maxSize int64) ([]FileEntry, error)`
    — check each pattern path with `os.Lstat`, apply symlink and size checks, set
    `Source: "config"` on each entry
- [ ] **3.2** Create `apps/rhino-cli/internal/envbackup/config_test.go`:
  - Test `DiscoverConfig` finds existing config files in `/tmp` fixture
  - Test `DiscoverConfig` silently skips missing patterns
  - Test `DiscoverConfig` skips symlinked config files with warning
  - Test `DiscoverConfig` skips oversized config files with warning
  - Test `DiscoverConfig` sets `Source: "config"` on all entries
  - Test `DiscoverConfig` returns sorted entries
  - Test `DefaultConfigPatterns` has expected length (14)

## Phase 4: Internal Package — Integrate into Backup/Restore

- [ ] **4.1** Modify `apps/rhino-cli/internal/envbackup/backup.go`:
  - After `Discover()`, if `opts.IncludeConfig`, call `DiscoverConfig()` and merge entries
  - After computing `destRoot`, if `!opts.Force && opts.ConfirmFn != nil`, call
    `FindExisting(entries, destRoot)` → if non-empty, call `opts.ConfirmFn` → if false,
    return `Result{Cancelled: true}`
  - Set `Source: "env"` on entries from `Discover()` (only when `IncludeConfig` is true,
    for clarity in mixed output)
- [ ] **4.2** Modify `apps/rhino-cli/internal/envbackup/restore.go`:
  - After discovering backup files, if `opts.IncludeConfig`, also discover config-pattern
    files in backup and merge
  - Add same confirmation check before copying
- [ ] **4.3** Extend `apps/rhino-cli/internal/envbackup/backup_test.go`:
  - Test: backup with `Force: true` overwrites without calling `ConfirmFn`
  - Test: backup with `ConfirmFn` returning `true` proceeds normally
  - Test: backup with `ConfirmFn` returning `false` returns `Cancelled: true` result
  - Test: backup with `ConfirmFn` and no existing destinations skips confirmation
  - Test: backup with `IncludeConfig: true` includes config files
  - Test: backup with `IncludeConfig: false` excludes config files
- [ ] **4.4** Extend `apps/rhino-cli/internal/envbackup/restore_test.go`:
  - Test: restore with `Force: true` overwrites without calling `ConfirmFn`
  - Test: restore with `ConfirmFn` returning `true` proceeds normally
  - Test: restore with `ConfirmFn` returning `false` returns `Cancelled: true` result
  - Test: restore with `IncludeConfig: true` includes config files
  - Test: restore with `IncludeConfig: false` excludes config files

## Phase 5: Reporter Updates

- [ ] **5.1** Modify `apps/rhino-cli/internal/envbackup/reporter.go`:
  - `FormatText`: show `[config]` tag next to config file paths; show "cancelled" message
    when `Result.Cancelled` is true; include config count in summary when present
  - `FormatJSON`: include `"source"` field on entries (via existing `json:"source,omitempty"`);
    include `"cancelled"` field when true
  - `FormatMarkdown`: show source column in table when config files are present; show
    cancelled row
- [ ] **5.2** Extend `apps/rhino-cli/internal/envbackup/reporter_test.go`:
  - Test text format with config files (shows `[config]` tag)
  - Test text format with cancelled result
  - Test JSON format with `"source": "config"` field
  - Test JSON format with `"cancelled": true` field
  - Test markdown format with source column

## Phase 6: Cobra Commands — Wire New Flags

- [ ] **6.1** Modify `apps/rhino-cli/cmd/env_backup.go`:
  - Add `--force` / `-f` flag (BoolVarP)
  - Add `--include-config` flag (BoolVar)
  - In `runEnvBackup`: compute effective force (flag || non-text output || non-TTY stdin),
    set `opts.Force`, `opts.IncludeConfig`
  - If not force, set `opts.ConfirmFn = confirmFn(os.Stdin, cmd.OutOrStderr())`
  - Update `Long` description and `Example` to document new flags
- [ ] **6.2** Modify `apps/rhino-cli/cmd/env_restore.go`:
  - Add `--force` / `-f` flag (BoolVarP)
  - Add `--include-config` flag (BoolVar)
  - In `runEnvRestore`: same force computation and ConfirmFn wiring
  - Update `Long` description and `Example`
- [ ] **6.3** Update `apps/rhino-cli/cmd/testable.go`:
  - Add `confirmFn = envbackup.DefaultConfirmFn` function variable
- [ ] **6.4** Bump version in `cmd/root.go` from `0.14.0` to `0.15.0`

## Phase 7: Cmd-Layer Unit Tests (Godog + Mocked Dependencies)

- [ ] **7.1** Update `apps/rhino-cli/cmd/steps_common_test.go`:
  - Add step regex constants for confirm scenarios (confirms overwrite, declines overwrite,
    with --force, no confirmation prompt shown, backup/restore cancelled)
  - Add step regex constants for config scenarios (with --include-config, config file
    copied/not copied, no known config files)
- [ ] **7.2** Extend `apps/rhino-cli/cmd/env_backup_test.go`:
  - Add step definitions for `@env-backup-confirm` scenarios — mock `envBackupFn` to return
    `Cancelled: true` or normal result depending on `ConfirmFn` mock; mock `confirmFn`
  - Add step definitions for `@env-backup-config` scenarios — mock `envBackupFn` to return
    results with/without config entries based on `IncludeConfig` option
  - Add non-BDD tests: `TestEnvBackupCmd_ForceFlag`, `TestEnvBackupCmd_IncludeConfigFlag`
- [ ] **7.3** Extend `apps/rhino-cli/cmd/env_restore_test.go`:
  - Add step definitions for `@env-restore-confirm` scenarios
  - Add step definitions for `@env-restore-config` scenarios
  - Add non-BDD tests: `TestEnvRestoreCmd_ForceFlag`, `TestEnvRestoreCmd_IncludeConfigFlag`

## Phase 8: Integration Tests (Godog + Real Filesystem)

- [ ] **8.1** Extend `apps/rhino-cli/cmd/env_backup.integration_test.go`:
  - Add `@env-backup-confirm` step implementations with real `/tmp` fixtures:
    - Create backup dir with pre-existing files → inject `strings.NewReader("y\n")` as
      confirm reader → verify overwrite
    - Create backup dir with pre-existing files → inject `strings.NewReader("n\n")` →
      verify files unchanged
    - Use `--force` flag → verify no prompt, files overwritten
    - Fresh backup dir → verify no prompt triggered
  - Add `@env-backup-config` step implementations:
    - Create `.claude/settings.local.json` in temp repo → backup with `--include-config` →
      verify config file in backup dir
    - Backup without `--include-config` → verify config file not in backup dir
- [ ] **8.2** Extend `apps/rhino-cli/cmd/env_restore.integration_test.go`:
  - Add `@env-restore-confirm` step implementations (mirror of backup confirm)
  - Add `@env-restore-config` step implementations:
    - Create config file in backup dir → restore with `--include-config` → verify config
      file in repo
    - Restore without `--include-config` → verify config file not restored

## Phase 9: Documentation and Validation

- [ ] **9.1** Update `apps/rhino-cli/README.md` — document `--force`, `--include-config` flags,
      config file patterns list
- [ ] **9.2** Run `nx run rhino-cli:test:quick` — verify unit tests pass and coverage >=90%
- [ ] **9.3** Run `nx run rhino-cli:test:integration` — verify all Gherkin scenarios pass at
      integration level
- [ ] **9.4** Run `nx run rhino-cli:lint` — verify no lint issues

## Phase 10: Manual Smoke Tests

Run each command manually against the real repository to verify end-to-end behavior.

- [ ] **10.1** Fresh backup (no existing backup dir):

  ```bash
  rm -rf ~/ose-open-env-backup
  cd /path/to/open-sharia-enterprise
  go run apps/rhino-cli/main.go env backup
  # Expected: no prompt, files backed up, exit 0
  ```

- [ ] **10.2** Repeat backup (existing backup dir with files):

  ```bash
  go run apps/rhino-cli/main.go env backup
  # Expected: prompt "N file(s) already exist in backup. Overwrite? [y/N]"
  # Type "y" → files overwritten
  ```

- [ ] **10.3** Decline backup overwrite:

  ```bash
  go run apps/rhino-cli/main.go env backup
  # Expected: prompt appears
  # Press Enter (empty input) → "Backup cancelled."
  ```

- [ ] **10.4** Force backup:

  ```bash
  go run apps/rhino-cli/main.go env backup --force
  # Expected: no prompt, files overwritten
  ```

- [ ] **10.5** JSON output implies force:

  ```bash
  go run apps/rhino-cli/main.go env backup -o json
  # Expected: no prompt, JSON output
  ```

- [ ] **10.6** Restore with confirmation:

  ```bash
  go run apps/rhino-cli/main.go env restore
  # Expected: prompt if .env files exist in repo
  # Type "y" → files restored
  ```

- [ ] **10.7** Restore with --force:

  ```bash
  go run apps/rhino-cli/main.go env restore --force
  # Expected: no prompt, files restored
  ```

- [ ] **10.8** Config backup:

  ```bash
  go run apps/rhino-cli/main.go env backup --include-config --force
  # Expected: .env files AND config files backed up
  # Verify: ls ~/ose-open-env-backup/.claude/settings.local.json
  ```

- [ ] **10.9** Config restore:

  ```bash
  # Temporarily rename local config to test restore
  mv .claude/settings.local.json .claude/settings.local.json.test-bak
  go run apps/rhino-cli/main.go env restore --include-config --force
  # Expected: config file restored
  # Verify: diff .claude/settings.local.json .claude/settings.local.json.test-bak
  # Clean up: mv .claude/settings.local.json.test-bak .claude/settings.local.json
  ```

- [ ] **10.10** Config backup with no config files:

  ```bash
  # In a repo with no config files (e.g., a temp dir)
  mkdir -p /tmp/test-repo && cd /tmp/test-repo && git init
  echo "FOO=bar" > .env
  go run /path/to/apps/rhino-cli/main.go env backup --include-config --force --dir /tmp/test-bkup
  # Expected: only .env backed up, no errors about missing config files
  rm -rf /tmp/test-repo /tmp/test-bkup
  ```

## Validation Checklist

- [ ] All new Gherkin scenarios in `env-backup.feature` pass at **both** unit and integration levels
- [ ] All new Gherkin scenarios in `env-restore.feature` pass at **both** unit and integration levels
- [ ] All existing Gherkin scenarios still pass (no regression)
- [ ] Unit test coverage >=90% for `internal/envbackup/`
- [ ] Overall rhino-cli coverage >=90% (`nx run rhino-cli:test:quick`)
- [ ] `nx run rhino-cli:lint` passes
- [ ] `apps/rhino-cli/README.md` updated with new flags
- [ ] Default backup directory renamed from `ose-env-bkup` to `ose-open-env-backup`
- [ ] Version bumped to 0.15.0
- [ ] `testable.go` updated with `confirmFn` function variable
- [ ] `steps_common_test.go` updated with confirm + config step constants
- [ ] `--force` flag works on both `env backup` and `env restore`
- [ ] `--include-config` flag works on both `env backup` and `env restore`
- [ ] JSON/markdown output implies force (no prompt)
- [ ] Non-TTY stdin implies force (no prompt)
- [ ] Confirmation prompt defaults to "No" (empty input declines)
- [ ] Cancelled backup/restore exits 0 with descriptive message
- [ ] Config files preserve relative paths and permissions
- [ ] Config files respect worktree-aware namespacing
- [ ] Config files respect symlink and size limits
- [ ] Missing config patterns are silently skipped (no error)
- [ ] `Source` field appears in JSON output only when `--include-config` is used
- [ ] Manual smoke tests 10.1 through 10.10 pass
