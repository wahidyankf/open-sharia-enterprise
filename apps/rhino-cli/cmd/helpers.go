package cmd

import (
	"fmt"
	"path/filepath"
)

// findGitRoot finds the root directory of the git repository by walking up from the current directory.
func findGitRoot() (string, error) {
	dir, err := osGetwd()
	if err != nil {
		return "", err
	}

	// Walk up the directory tree looking for .git
	for {
		gitDir := filepath.Join(dir, ".git")
		if _, err := osStat(gitDir); err == nil {
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
// It is used by writeFormattedV2 in output.go.
type outputFuncs struct {
	text     func(verbose, quiet bool) string
	json     func() (string, error)
	markdown func() string
}
