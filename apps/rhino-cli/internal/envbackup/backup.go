package envbackup

import (
	"fmt"
	"io"
	"os"
	"path/filepath"
	"sort"
	"strings"
)

// Backup discovers all .env files under opts.RepoRoot and copies them to
// opts.BackupDir, preserving the relative directory structure and file permissions.
// The backup directory must not be inside the repo root.
func Backup(opts Options) (*Result, error) {
	if opts.MaxSize <= 0 {
		opts.MaxSize = DefaultMaxSize
	}
	if len(opts.SkipDirs) == 0 {
		opts.SkipDirs = DefaultSkipDirs
	}

	// Expand tilde in BackupDir.
	backupDir, err := ExpandTilde(opts.BackupDir)
	if err != nil {
		return nil, fmt.Errorf("expand backup dir: %w", err)
	}
	opts.BackupDir = backupDir

	// Reject backup dirs that are inside the repo root.
	if isInsideRepo(opts.BackupDir, opts.RepoRoot) {
		return nil, fmt.Errorf("backup dir %s is inside repo root %s; choose a directory outside the repo", opts.BackupDir, opts.RepoRoot)
	}

	entries, err := Discover(opts)
	if err != nil {
		return nil, fmt.Errorf("discover env files: %w", err)
	}

	// Set Source: "env" on discovered entries when IncludeConfig is active (for clarity).
	if opts.IncludeConfig {
		for i := range entries {
			if entries[i].Source == "" {
				entries[i].Source = "env"
			}
		}
	}

	// Config discovery.
	if opts.IncludeConfig {
		configEntries, err := DiscoverConfig(opts.RepoRoot, DefaultConfigPatterns, opts.MaxSize)
		if err != nil {
			return nil, fmt.Errorf("discover config files: %w", err)
		}
		entries = append(entries, configEntries...)
		sort.Slice(entries, func(i, j int) bool { return entries[i].RelPath < entries[j].RelPath })
	}

	// Determine the effective destination root (worktree-aware namespacing).
	destRoot := opts.BackupDir
	if opts.WorktreeAware && opts.WorktreeName != "" {
		destRoot = filepath.Join(opts.BackupDir, opts.WorktreeName)
	}

	// Confirmation check.
	if !opts.Force && opts.ConfirmFn != nil {
		existing := FindExisting(entries, destRoot)
		if len(existing) > 0 {
			if !opts.ConfirmFn(existing) {
				return &Result{Direction: "backup", Dir: opts.BackupDir, Cancelled: true}, nil
			}
		}
	}

	if err := os.MkdirAll(destRoot, 0o750); err != nil {
		return nil, fmt.Errorf("create backup dir: %w", err)
	}

	result := &Result{
		Direction:    "backup",
		Dir:          opts.BackupDir,
		Files:        entries,
		WorktreeName: opts.WorktreeName,
	}

	for i := range entries {
		e := &entries[i]
		if e.Skipped {
			result.Skipped++
			continue
		}

		dst := filepath.Join(destRoot, e.RelPath)
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

// copyFile copies src to dst, preserving the source file permissions.
// dst is truncated if it already exists.
func copyFile(src, dst string) error {
	fi, err := os.Lstat(src)
	if err != nil {
		return fmt.Errorf("lstat src: %w", err)
	}

	in, err := os.Open(src)
	if err != nil {
		return fmt.Errorf("open src: %w", err)
	}
	defer func() { _ = in.Close() }()

	out, err := os.OpenFile(dst, os.O_CREATE|os.O_WRONLY|os.O_TRUNC, fi.Mode().Perm())
	if err != nil {
		return fmt.Errorf("open dst: %w", err)
	}
	defer func() { _ = out.Close() }()

	if _, err := io.Copy(out, in); err != nil {
		return fmt.Errorf("copy data: %w", err)
	}
	return nil
}

// isInsideRepo reports whether backupDir is inside (or equal to) repoRoot.
func isInsideRepo(backupDir, repoRoot string) bool {
	rel, err := filepath.Rel(repoRoot, backupDir)
	if err != nil {
		return false
	}
	// If the relative path starts with ".." it is outside; also reject "." (equal).
	return !strings.HasPrefix(rel, "..")
}

// ExpandTilde replaces a leading "~" with the current user's home directory.
func ExpandTilde(path string) (string, error) {
	if !strings.HasPrefix(path, "~") {
		return path, nil
	}
	home, err := os.UserHomeDir()
	if err != nil {
		return "", fmt.Errorf("get home dir: %w", err)
	}
	return filepath.Join(home, path[1:]), nil
}
