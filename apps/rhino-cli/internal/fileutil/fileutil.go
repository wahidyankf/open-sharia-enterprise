package fileutil

import (
	"os"
	"os/exec"
	"path/filepath"
	"strings"
)

// WalkMarkdownDirs walks dirs under repoRoot collecting .md files,
// then appends any root-level .md files. Skips non-existent dirs silently.
func WalkMarkdownDirs(repoRoot string, dirs []string) ([]string, error) {
	var files []string

	for _, dir := range dirs {
		dirPath := filepath.Join(repoRoot, dir)

		if _, err := os.Stat(dirPath); os.IsNotExist(err) {
			continue
		}

		err := filepath.Walk(dirPath, func(path string, info os.FileInfo, err error) error {
			if err != nil {
				return err
			}
			if !info.IsDir() && strings.HasSuffix(path, ".md") {
				files = append(files, path)
			}
			return nil
		})
		if err != nil {
			return nil, err
		}
	}

	// Add root-level .md files
	rootMatches, err := filepath.Glob(filepath.Join(repoRoot, "*.md"))
	if err != nil {
		return nil, err
	}
	files = append(files, rootMatches...)

	return files, nil
}

// GetStagedFiles returns all staged file paths (relative to repoRoot) from git.
func GetStagedFiles(repoRoot string) ([]string, error) {
	cmd := exec.Command("git", "diff", "--cached", "--name-only", "--diff-filter=ACM")
	cmd.Dir = repoRoot
	output, err := cmd.Output()
	if err != nil {
		return nil, err
	}

	var files []string
	lines := strings.Split(strings.TrimSpace(string(output)), "\n")
	for _, line := range lines {
		if line != "" {
			files = append(files, line)
		}
	}
	return files, nil
}
