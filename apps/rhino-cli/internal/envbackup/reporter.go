package envbackup

import (
	"encoding/json"
	"fmt"
	"path/filepath"
	"strings"
)

// FormatText renders a human-readable summary of the result.
//
//   - quiet: print only the one-line summary.
//   - verbose: print all files (including skipped ones with reasons).
//   - default: print only the copied files.
func FormatText(r *Result, verbose, quiet bool) string {
	var sb strings.Builder

	if !quiet {
		// Per-file lines.
		for _, f := range r.Files {
			if f.Skipped {
				if verbose {
					fmt.Fprintf(&sb, "  SKIPPED  %s  (%s)\n", f.RelPath, f.Reason)
				}
				continue
			}
			fmt.Fprintf(&sb, "  %s  %s\n", strings.ToUpper(r.Direction), f.RelPath)
		}

		// Non-fatal warnings.
		for _, e := range r.Errors {
			fmt.Fprintf(&sb, "  WARNING  %s\n", e)
		}
	}

	// Summary line.
	label := r.Direction
	if label == "" {
		label = "processed"
	}
	fmt.Fprintf(&sb, "%s complete: %d file(s) %s, %d skipped",
		capitalize(label), r.Copied, label+"d", r.Skipped)

	if r.WorktreeName != "" {
		fmt.Fprintf(&sb, "  [worktree: %s]", r.WorktreeName)
	}
	fmt.Fprintln(&sb)

	return sb.String()
}

// jsonResult is the serialisable form of Result for JSON output.
type jsonResult struct {
	Direction    string          `json:"direction"`
	Dir          string          `json:"dir"`
	Files        []jsonFileEntry `json:"files"`
	Copied       int             `json:"copied"`
	Skipped      int             `json:"skipped"`
	Errors       []string        `json:"errors,omitempty"`
	WorktreeName string          `json:"worktreeName,omitempty"`
}

type jsonFileEntry struct {
	RelPath string `json:"relPath"`
	Size    int64  `json:"size,omitempty"`
	Skipped bool   `json:"skipped,omitempty"`
	Reason  string `json:"reason,omitempty"`
}

// FormatJSON serialises the result to a JSON string.
func FormatJSON(r *Result) (string, error) {
	files := make([]jsonFileEntry, len(r.Files))
	for i, f := range r.Files {
		files[i] = jsonFileEntry{
			RelPath: f.RelPath,
			Size:    f.Size,
			Skipped: f.Skipped,
			Reason:  f.Reason,
		}
	}

	out := jsonResult{
		Direction:    r.Direction,
		Dir:          r.Dir,
		Files:        files,
		Copied:       r.Copied,
		Skipped:      r.Skipped,
		Errors:       r.Errors,
		WorktreeName: r.WorktreeName,
	}

	b, err := json.MarshalIndent(out, "", "  ")
	if err != nil {
		return "", fmt.Errorf("marshal result: %w", err)
	}
	return string(b), nil
}

// FormatMarkdown renders the result as a Markdown table.
func FormatMarkdown(r *Result) string {
	var sb strings.Builder

	action := capitalize(r.Direction)
	fmt.Fprintf(&sb, "## %s Report\n\n", action)
	fmt.Fprintf(&sb, "**Directory**: `%s`\n\n", r.Dir)
	fmt.Fprintf(&sb, "**Copied**: %d | **Skipped**: %d\n\n", r.Copied, r.Skipped)

	if r.WorktreeName != "" {
		fmt.Fprintf(&sb, "**Worktree**: `%s`\n\n", r.WorktreeName)
	}

	if len(r.Files) == 0 {
		fmt.Fprintln(&sb, "_No .env files found._")
		return sb.String()
	}

	fmt.Fprintln(&sb, "| File | Size (bytes) | Status | Reason |")
	fmt.Fprintln(&sb, "|------|-------------|--------|--------|")

	for _, f := range r.Files {
		status := "copied"
		reason := ""
		if f.Skipped {
			status = "skipped"
			reason = f.Reason
		}
		// Normalise path separators for display.
		displayPath := filepath.ToSlash(f.RelPath)
		fmt.Fprintf(&sb, "| `%s` | %d | %s | %s |\n", displayPath, f.Size, status, reason)
	}

	if len(r.Errors) > 0 {
		fmt.Fprintln(&sb, "\n### Warnings")
		for _, e := range r.Errors {
			fmt.Fprintf(&sb, "- %s\n", e)
		}
	}

	return sb.String()
}

// capitalize returns s with the first rune upper-cased.
func capitalize(s string) string {
	if s == "" {
		return s
	}
	return strings.ToUpper(s[:1]) + s[1:]
}
