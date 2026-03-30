@env-backup
Feature: Env file backup

  As a developer
  I want rhino-cli env backup to copy all .env files outside the repository
  So that environment configuration is preserved safely before destructive operations

  @env-backup
  Scenario: Backup discovers and copies all .env files
    Given a git repository containing .env files at the root and in app subdirectories
    When the developer runs rhino-cli env backup
    Then the command exits successfully
    And each .env file is copied to the backup directory preserving its relative path
    And the output lists each backed-up file

  @env-backup
  Scenario: Backup with custom directory
    Given a git repository containing a .env file at the root
    When the developer runs rhino-cli env backup with --dir pointing to a directory outside the repository
    Then the command exits successfully
    And the .env file is copied to the specified directory preserving its relative path

  @env-backup
  Scenario: Backup rejects a directory inside the repository
    Given a git repository containing a .env file at the root
    When the developer runs rhino-cli env backup with --dir pointing to a path inside the git root
    Then the command exits with a failure code
    And the output warns that the backup directory must be outside the repository

  @env-backup
  Scenario: Symlinks and oversized files are skipped
    Given a git repository containing a symlinked .env file, a .env file larger than 1 MB, and a regular .env file
    When the developer runs rhino-cli env backup
    Then the command exits successfully
    And the symlinked .env file is skipped with a warning
    And the oversized .env file is skipped with a warning
    And the regular .env file is copied to the backup directory

  @env-backup
  Scenario: Backup with zero .env files
    Given a git repository containing no .env files
    When the developer runs rhino-cli env backup
    Then the command exits successfully
    And the output reports that zero files were backed up

  @env-backup
  Scenario: JSON output for backup
    Given a git repository containing a .env file at the root
    When the developer runs rhino-cli env backup with --output json
    Then the command exits successfully
    And the output is valid JSON
    And the JSON includes the direction, backup directory, list of files, copied count, and skipped count

  @env-backup
  Scenario: Env files inside auto-generated directories are not discovered
    Given a git repository containing .env files inside node_modules, dist, build, .next, __pycache__, target, vendor, coverage, and generated-contracts directories
    When the developer runs rhino-cli env backup
    Then the command exits successfully
    And none of the .env files inside auto-generated directories are backed up

  @env-backup
  Scenario: Env files inside nested auto-generated directories are not discovered
    Given a git repository where apps/web/node_modules contains a .env file and apps/web contains a .env.local file
    When the developer runs rhino-cli env backup
    Then the command exits successfully
    And only apps/web/.env.local is copied to the backup directory
    And the .env file inside apps/web/node_modules is not backed up

  @env-backup
  Scenario: Backup works in a git worktree
    Given a git worktree containing a .env file at its root
    When the developer runs rhino-cli env backup
    Then the command exits successfully
    And the .env file is copied to the backup directory with a flat structure

  @env-backup
  Scenario: Worktree-aware backup namespaces by worktree name
    Given a git worktree named "feature-branch" containing a .env file at its root
    When the developer runs rhino-cli env backup with --worktree-aware
    Then the command exits successfully
    And the .env file is copied under a feature-branch subdirectory inside the backup directory

  @env-backup
  Scenario: Main repo with worktree-aware uses repository directory name
    Given the main git repository named "open-sharia-enterprise" containing a .env file at its root
    When the developer runs rhino-cli env backup with --worktree-aware
    Then the command exits successfully
    And the .env file is copied under an open-sharia-enterprise subdirectory inside the backup directory
