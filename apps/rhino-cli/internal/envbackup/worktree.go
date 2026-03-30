package envbackup

import (
	"fmt"
	"os"
	"path/filepath"
	"strings"
)

// WorktreeInfo holds information about the git worktree or repository.
type WorktreeInfo struct {
	// IsWorktree is true when repoRoot/.git is a file (linked worktree).
	IsWorktree bool
	// WorktreeName is the basename of repoRoot in both worktree and normal-repo cases.
	WorktreeName string
}

// DetectWorktree inspects repoRoot to determine whether it is a linked git worktree
// or a normal repository. In both cases WorktreeName is set to filepath.Base(repoRoot).
func DetectWorktree(repoRoot string) (*WorktreeInfo, error) {
	gitPath := filepath.Join(repoRoot, ".git")

	info, err := os.Lstat(gitPath)
	if err != nil {
		if os.IsNotExist(err) {
			return nil, fmt.Errorf("no .git found at %s", repoRoot)
		}
		return nil, fmt.Errorf("stat .git: %w", err)
	}

	name := filepath.Base(repoRoot)

	// Normal repository: .git is a directory.
	if info.IsDir() {
		return &WorktreeInfo{
			IsWorktree:   false,
			WorktreeName: name,
		}, nil
	}

	// Linked worktree: .git is a file containing "gitdir: <path>".
	data, err := os.ReadFile(gitPath)
	if err != nil {
		return nil, fmt.Errorf("read .git file: %w", err)
	}
	line := strings.TrimSpace(string(data))
	if !strings.HasPrefix(line, "gitdir:") {
		return nil, fmt.Errorf(".git file does not start with 'gitdir:' (got: %q)", line)
	}

	return &WorktreeInfo{
		IsWorktree:   true,
		WorktreeName: name,
	}, nil
}
