package envbackup

import (
	"fmt"
	"os"
	"path/filepath"
	"sort"
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

	// Config discovery from backup dir.
	if opts.IncludeConfig {
		// Set Source: "env" on discovered entries for clarity.
		for i := range entries {
			if entries[i].Source == "" {
				entries[i].Source = "env"
			}
		}
		configEntries, err := DiscoverConfig(srcRoot, DefaultConfigPatterns, opts.MaxSize)
		if err != nil {
			return nil, fmt.Errorf("discover config files: %w", err)
		}
		entries = append(entries, configEntries...)
		sort.Slice(entries, func(i, j int) bool { return entries[i].RelPath < entries[j].RelPath })
	}

	// Confirmation check.
	if !opts.Force && opts.ConfirmFn != nil {
		// Build a filtered list of entries that will actually be restored.
		var restoreEntries []FileEntry
		for _, e := range entries {
			base := filepath.Base(e.RelPath)
			if e.Source == "config" || strings.HasPrefix(base, ".env") {
				restoreEntries = append(restoreEntries, e)
			}
		}
		existing := FindExisting(restoreEntries, opts.RepoRoot)
		if len(existing) > 0 {
			if !opts.ConfirmFn(existing) {
				return &Result{Direction: "restore", Dir: opts.BackupDir, Cancelled: true}, nil
			}
		}
	}

	result := &Result{
		Direction:    "restore",
		Dir:          opts.BackupDir,
		WorktreeName: opts.WorktreeName,
	}

	for _, e := range entries {
		// Only restore files whose basename starts with ".env" OR config files.
		base := filepath.Base(e.RelPath)
		if e.Source != "config" && !strings.HasPrefix(base, ".env") {
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
