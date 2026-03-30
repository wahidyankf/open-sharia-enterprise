package envbackup

import (
	"fmt"
	"os"
	"path/filepath"
	"strings"
)

// Restore copies .env* files from opts.BackupDir back to opts.RepoRoot,
// recreating the original directory structure. The source backup directory
// must already exist.
func Restore(opts Options) (*Result, error) {
	if opts.MaxSize <= 0 {
		opts.MaxSize = DefaultMaxSize
	}

	// Expand tilde in BackupDir.
	backupDir, err := ExpandTilde(opts.BackupDir)
	if err != nil {
		return nil, fmt.Errorf("expand backup dir: %w", err)
	}
	opts.BackupDir = backupDir

	// Determine the effective source root (worktree-aware namespacing).
	srcRoot := opts.BackupDir
	if opts.WorktreeAware && opts.WorktreeName != "" {
		srcRoot = filepath.Join(opts.BackupDir, opts.WorktreeName)
	}

	// Validate source dir exists.
	if _, err := os.Stat(srcRoot); err != nil {
		if os.IsNotExist(err) {
			return nil, fmt.Errorf("backup dir does not exist: %s", srcRoot)
		}
		return nil, fmt.Errorf("stat backup dir: %w", err)
	}

	// Discover .env* files in the backup dir. We do not need the standard skip
	// dirs for restore — only skip ".git" to avoid restoring git internals if
	// the backup was accidentally created inside a repo.
	discoverOpts := Options{
		RepoRoot: srcRoot,
		SkipDirs: []string{".git"},
		MaxSize:  opts.MaxSize,
	}

	entries, err := Discover(discoverOpts)
	if err != nil {
		return nil, fmt.Errorf("discover backup files: %w", err)
	}

	result := &Result{
		Direction:    "restore",
		Dir:          opts.BackupDir,
		WorktreeName: opts.WorktreeName,
	}

	for _, e := range entries {
		// Only restore files whose basename starts with ".env".
		base := filepath.Base(e.RelPath)
		if !strings.HasPrefix(base, ".env") {
			continue
		}

		result.Files = append(result.Files, e)

		if e.Skipped {
			result.Skipped++
			continue
		}

		dst := filepath.Join(opts.RepoRoot, e.RelPath)
		if err := os.MkdirAll(filepath.Dir(dst), 0o750); err != nil {
			result.Errors = append(result.Errors, fmt.Sprintf("mkdir for %s: %v", e.RelPath, err))
			result.Skipped++
			continue
		}

		if err := copyFile(e.AbsPath, dst); err != nil {
			result.Errors = append(result.Errors, fmt.Sprintf("copy %s: %v", e.RelPath, err))
			result.Skipped++
			continue
		}
		result.Copied++
	}

	return result, nil
}
