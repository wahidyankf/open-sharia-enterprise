package cmd

import (
	"fmt"
	"os"
	"path/filepath"

	"github.com/spf13/cobra"
)

// findGitRoot finds the root directory of the git repository by walking up from the current directory.
func findGitRoot() (string, error) {
	dir, err := os.Getwd()
	if err != nil {
		return "", err
	}

	// Walk up the directory tree looking for .git
	for {
		gitDir := filepath.Join(dir, ".git")
		if _, err := os.Stat(gitDir); err == nil {
			return dir, nil
		}

		// Move up one directory
		parent := filepath.Dir(dir)
		if parent == dir {
			// Reached root without finding .git
			return "", fmt.Errorf(".git directory not found")
		}
		dir = parent
	}
}

// outputFuncs holds the three formatting callbacks for a command's output.
type outputFuncs struct {
	text     func(verbose, quiet bool) string
	json     func() (string, error)
	markdown func() string
}

// writeFormatted selects the right formatter, writes to cmd.OutOrStdout(), and returns any error.
func writeFormatted(cmd *cobra.Command, format string, verbose, quiet bool, f outputFuncs) error {
	var out string
	switch format {
	case "json":
		var err error
		out, err = f.json()
		if err != nil {
			return fmt.Errorf("failed to format JSON: %w", err)
		}
	case "markdown":
		out = f.markdown()
	default:
		out = f.text(verbose, quiet)
	}
	_, _ = fmt.Fprint(cmd.OutOrStdout(), out)
	return nil
}
