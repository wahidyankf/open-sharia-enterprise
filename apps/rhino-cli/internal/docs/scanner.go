package docs

import (
	"os"
	"os/exec"
	"path/filepath"
	"strings"
)

// GetDocsFiles returns a list of files in docs/ to validate based on options.
func GetDocsFiles(opts ValidationOptions) ([]string, error) {
	if opts.StagedOnly {
		return getStagedDocsFiles(opts.RepoRoot)
	}
	return getAllDocsFiles(opts.RepoRoot)
}

// getStagedDocsFiles returns staged files in docs/ from git.
func getStagedDocsFiles(repoRoot string) ([]string, error) {
	cmd := exec.Command("git", "diff", "--cached", "--name-only", "--diff-filter=ACM")
	cmd.Dir = repoRoot
	output, err := cmd.Output()
	if err != nil {
		return nil, err
	}

	var files []string
	lines := strings.Split(strings.TrimSpace(string(output)), "\n")
	for _, line := range lines {
		if line == "" {
			continue
		}
		// Only include files in docs/ directory
		if !strings.HasPrefix(line, "docs/") {
			continue
		}
		// Include all file types (not just .md) since naming applies to images too
		files = append(files, filepath.Join(repoRoot, line))
	}
	return files, nil
}

// getAllDocsFiles returns all files in docs/ directory.
func getAllDocsFiles(repoRoot string) ([]string, error) {
	docsDir := filepath.Join(repoRoot, "docs")

	// Check if docs directory exists
	if _, err := os.Stat(docsDir); os.IsNotExist(err) {
		return nil, nil
	}

	var files []string
	err := filepath.Walk(docsDir, func(path string, info os.FileInfo, err error) error {
		if err != nil {
			return nil // Skip errors
		}
		if info.IsDir() {
			return nil
		}
		files = append(files, path)
		return nil
	})
	if err != nil {
		return nil, err
	}

	return files, nil
}

// GetRelativePath returns the relative path from repo root.
func GetRelativePath(filePath, repoRoot string) (string, error) {
	return filepath.Rel(repoRoot, filePath)
}
