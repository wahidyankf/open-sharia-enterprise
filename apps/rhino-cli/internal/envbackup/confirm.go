package envbackup

import (
	"bufio"
	"fmt"
	"io"
	"os"
	"path/filepath"
	"strings"
)

// FindExisting returns the subset of entries whose destination paths already
// exist on disk. For backup, destRoot is the backup directory; for restore,
// destRoot is the repo root.
func FindExisting(entries []FileEntry, destRoot string) []string {
	var existing []string
	for _, e := range entries {
		if e.Skipped {
			continue
		}
		dst := filepath.Join(destRoot, e.RelPath)
		if _, err := os.Stat(dst); err == nil {
			existing = append(existing, e.RelPath)
		}
	}
	return existing
}

// DefaultConfirmFn returns a confirmation function that reads from the given
// reader (typically os.Stdin). It prints the list of conflicting files and
// prompts with [y/N]. Returns true only for affirmative answers.
func DefaultConfirmFn(r io.Reader, w io.Writer) func(existing []string) bool {
	return func(existing []string) bool {
		_, _ = fmt.Fprintf(w, "%d file(s) already exist. Overwrite? [y/N]\n", len(existing))
		for _, p := range existing {
			_, _ = fmt.Fprintf(w, "  - %s\n", p)
		}
		scanner := bufio.NewScanner(r)
		if scanner.Scan() {
			answer := strings.TrimSpace(scanner.Text())
			switch strings.ToLower(answer) {
			case "y", "yes":
				return true
			}
		}
		return false
	}
}
