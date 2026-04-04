package cmd

import (
	"fmt"
	"io/fs"
	"os"
	"path/filepath"

	"github.com/spf13/cobra"
)

var envInitForce bool

var envInitCmd = &cobra.Command{
	Use:   "init",
	Short: "Create .env files from .env.example templates",
	Long: `Finds all .env.example files in infra/dev/ and copies them to .env
in the same directory. Existing .env files are not overwritten unless --force is used.

This is useful for fresh setups where no rhino-cli env backup exists.`,
	Args:          cobra.NoArgs,
	SilenceErrors: true,
	RunE:          runEnvInit,
}

func init() {
	envCmd.AddCommand(envInitCmd)
	envInitCmd.Flags().BoolVar(&envInitForce, "force", false, "Overwrite existing .env files")
}

// envInitWalkDir is overridable for testing.
var envInitWalkDir = filepath.WalkDir

// envInitReadFile is overridable for testing.
var envInitReadFile = os.ReadFile

// envInitWriteFile is overridable for testing.
var envInitWriteFile = os.WriteFile

// envInitStat is overridable for testing.
var envInitStat = os.Stat

func runEnvInit(cmd *cobra.Command, _ []string) error {
	repoRoot, err := findGitRoot()
	if err != nil {
		return fmt.Errorf("failed to find git repository root: %w", err)
	}

	infraDevDir := filepath.Join(repoRoot, "infra", "dev")

	var created, skipped int
	var errs []string

	walkErr := envInitWalkDir(infraDevDir, func(path string, d fs.DirEntry, err error) error {
		if err != nil {
			return err
		}
		if d.IsDir() {
			return nil
		}
		if d.Name() != ".env.example" {
			return nil
		}

		dir := filepath.Dir(path)
		envPath := filepath.Join(dir, ".env")
		relPath, _ := filepath.Rel(repoRoot, envPath)

		if !envInitForce {
			if _, statErr := envInitStat(envPath); statErr == nil {
				fmt.Fprintf(cmd.OutOrStdout(), "Skipped: %s (already exists, use --force to overwrite)\n", relPath)
				skipped++
				return nil
			}
		}

		data, readErr := envInitReadFile(path)
		if readErr != nil {
			errs = append(errs, fmt.Sprintf("failed to read %s: %v", path, readErr))
			return nil
		}

		if writeErr := envInitWriteFile(envPath, data, 0o644); writeErr != nil {
			errs = append(errs, fmt.Sprintf("failed to write %s: %v", envPath, writeErr))
			return nil
		}

		fmt.Fprintf(cmd.OutOrStdout(), "Created: %s (from %s)\n", relPath, filepath.Base(path))
		created++
		return nil
	})

	if walkErr != nil {
		return fmt.Errorf("failed to walk infra/dev/: %w", walkErr)
	}

	fmt.Fprintf(cmd.OutOrStdout(), "\nSummary: %d created, %d skipped\n", created, skipped)

	for _, e := range errs {
		fmt.Fprintf(cmd.ErrOrStderr(), "Error: %s\n", e)
	}

	return nil
}
