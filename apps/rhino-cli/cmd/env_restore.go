package cmd

import (
	"fmt"
	"path/filepath"

	"github.com/spf13/cobra"
	"github.com/wahidyankf/open-sharia-enterprise/apps/rhino-cli/internal/envbackup"
)

var envRestoreDir string
var envRestoreWorktreeAware bool

var envRestoreCmd = &cobra.Command{
	Use:   "restore",
	Short: "Restore .env files from a backup",
	Long: `Copy previously backed-up .env* files from the backup directory back to
their original repository paths. Only files whose basename starts with
".env" are restored; other files in the backup are ignored.`,
	Example: `  # Restore from default directory ~/ose-env-bkup
  rhino-cli env restore

  # Restore from a custom directory
  rhino-cli env restore --dir /tmp/my-env-backup

  # Restore from worktree-namespaced backup
  rhino-cli env restore --worktree-aware

  # JSON output
  rhino-cli env restore -o json`,
	Args:          cobra.NoArgs,
	SilenceErrors: true,
	RunE:          runEnvRestore,
}

func init() {
	envCmd.AddCommand(envRestoreCmd)
	envRestoreCmd.Flags().StringVar(&envRestoreDir, "dir", "", "backup source directory (default: ~/ose-env-bkup)")
	envRestoreCmd.Flags().BoolVar(&envRestoreWorktreeAware, "worktree-aware", false, "read from worktree-namespaced backup")
}

func runEnvRestore(cmd *cobra.Command, _ []string) error {
	repoRoot, err := findGitRoot()
	if err != nil {
		return fmt.Errorf("failed to find git repository root: %w", err)
	}

	backupDir := envRestoreDir
	if backupDir == "" {
		home, err := envbackup.ExpandTilde("~")
		if err != nil {
			return fmt.Errorf("cannot determine home directory: %w", err)
		}
		backupDir = filepath.Join(home, envbackup.DefaultBackupDir)
	} else {
		backupDir, err = envbackup.ExpandTilde(backupDir)
		if err != nil {
			return fmt.Errorf("invalid backup directory: %w", err)
		}
		backupDir, err = filepath.Abs(backupDir)
		if err != nil {
			return fmt.Errorf("cannot resolve backup directory: %w", err)
		}
	}

	opts := envbackup.Options{
		RepoRoot:      repoRoot,
		BackupDir:     backupDir,
		MaxSize:       envbackup.DefaultMaxSize,
		WorktreeAware: envRestoreWorktreeAware,
	}

	if envRestoreWorktreeAware {
		info, err := envbackup.DetectWorktree(repoRoot)
		if err != nil {
			return fmt.Errorf("worktree detection failed: %w", err)
		}
		opts.WorktreeName = info.WorktreeName
	}

	result, err := envRestoreFn(opts)
	if err != nil {
		return fmt.Errorf("env restore failed: %w", err)
	}

	return writeFormatted(cmd, output, verbose, quiet, outputFuncs{
		text:     func(v, q bool) string { return envbackup.FormatText(result, v, q) },
		json:     func() (string, error) { return envbackup.FormatJSON(result) },
		markdown: func() string { return envbackup.FormatMarkdown(result) },
	})
}
