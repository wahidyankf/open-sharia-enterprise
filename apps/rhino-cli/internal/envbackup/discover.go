package envbackup

import (
	"fmt"
	"io/fs"
	"os"
	"path/filepath"
	"sort"
	"strings"
)

// Discover walks RepoRoot and returns all .env* files found, including those that
// were skipped due to being symlinks or exceeding MaxSize. Results are sorted by
// RelPath for deterministic ordering.
func Discover(opts Options) ([]FileEntry, error) {
	if opts.MaxSize <= 0 {
		opts.MaxSize = DefaultMaxSize
	}

	skipSet := make(map[string]bool, len(opts.SkipDirs))
	for _, d := range opts.SkipDirs {
		skipSet[d] = true
	}

	var entries []FileEntry

	err := filepath.WalkDir(opts.RepoRoot, func(path string, d fs.DirEntry, err error) error {
		if err != nil {
			return err
		}

		base := filepath.Base(path)

		// Handle directories.
		if d.IsDir() {
			// Never skip the root itself.
			if path == opts.RepoRoot {
				return nil
			}
			// Skip hidden directories (basename starts with ".").
			if strings.HasPrefix(base, ".") {
				return filepath.SkipDir
			}
			// Skip dirs in the skip set.
			if skipSet[base] {
				return filepath.SkipDir
			}
			return nil
		}

		// Only process files whose basename starts with ".env".
		if !strings.HasPrefix(base, ".env") {
			return nil
		}

		relPath, err := filepath.Rel(opts.RepoRoot, path)
		if err != nil {
			return fmt.Errorf("compute relative path for %s: %w", path, err)
		}

		// Use Lstat to detect symlinks without following them.
		fi, err := os.Lstat(path)
		if err != nil {
			return fmt.Errorf("lstat %s: %w", path, err)
		}

		if fi.Mode()&os.ModeSymlink != 0 {
			entries = append(entries, FileEntry{
				RelPath: relPath,
				AbsPath: path,
				Skipped: true,
				Reason:  "symlink",
			})
			return nil
		}

		size := fi.Size()
		if size > opts.MaxSize {
			entries = append(entries, FileEntry{
				RelPath: relPath,
				AbsPath: path,
				Size:    size,
				Skipped: true,
				Reason:  "exceeds 1 MB",
			})
			return nil
		}

		entries = append(entries, FileEntry{
			RelPath: relPath,
			AbsPath: path,
			Size:    size,
		})
		return nil
	})
	if err != nil {
		return nil, fmt.Errorf("walk %s: %w", opts.RepoRoot, err)
	}

	sort.Slice(entries, func(i, j int) bool {
		return entries[i].RelPath < entries[j].RelPath
	})

	return entries, nil
}
