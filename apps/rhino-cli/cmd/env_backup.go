package cmd

import (
	"fmt"
	"path/filepath"

	"github.com/spf13/cobra"
	"github.com/wahidyankf/open-sharia-enterprise/apps/rhino-cli/internal/envbackup"
)

var envBackupDir string
var envBackupWorktreeAware bool

var envBackupCmd = &cobra.Command{
	Use:   "backup",
	Short: "Back up .env files from the repository",
	Long: `Recursively find all .env* files in the repository and copy them to a
backup directory, preserving the relative directory structure.

Auto-generated directories (node_modules, dist, build, .next, etc.) are
skipped. Symlinks and files larger than 1 MB are skipped with a warning.`,
	Example: `  # Back up to default directory ~/ose-env-bkup
  rhino-cli env backup

  # Back up to a custom directory
  rhino-cli env backup --dir /tmp/my-env-backup

  # Namespace backup by worktree/repo name
  rhino-cli env backup --worktree-aware

  # JSON output
  rhino-cli env backup -o json`,
	Args:          cobra.NoArgs,
	SilenceErrors: true,
	RunE:          runEnvBackup,
}

func init() {
	envCmd.AddCommand(envBackupCmd)
	envBackupCmd.Flags().StringVar(&envBackupDir, "dir", "", "backup directory (default: ~/ose-env-bkup)")
	envBackupCmd.Flags().BoolVar(&envBackupWorktreeAware, "worktree-aware", false, "namespace backup by worktree/repo directory name")
}

func runEnvBackup(cmd *cobra.Command, _ []string) error {
	repoRoot, err := findGitRoot()
	if err != nil {
		return fmt.Errorf("failed to find git repository root: %w", err)
	}

	backupDir := envBackupDir
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
		SkipDirs:      envbackup.DefaultSkipDirs,
		MaxSize:       envbackup.DefaultMaxSize,
		WorktreeAware: envBackupWorktreeAware,
	}

	if envBackupWorktreeAware {
		info, err := envbackup.DetectWorktree(repoRoot)
		if err != nil {
			return fmt.Errorf("worktree detection failed: %w", err)
		}
		opts.WorktreeName = info.WorktreeName
	}

	result, err := envBackupFn(opts)
	if err != nil {
		return fmt.Errorf("env backup failed: %w", err)
	}

	return writeFormatted(cmd, output, verbose, quiet, outputFuncs{
		text:     func(v, q bool) string { return envbackup.FormatText(result, v, q) },
		json:     func() (string, error) { return envbackup.FormatJSON(result) },
		markdown: func() string { return envbackup.FormatMarkdown(result) },
	})
}
