package repogovernance

import (
	"bufio"
	"errors"
	"fmt"
	"io/fs"
	"os"
	"path/filepath"
	"sort"
	"strings"
)

// EmojiFinding describes a single emoji codepoint found in a file that the
// emoji convention forbids from containing emoji.
type EmojiFinding struct {
	// File is the path to the offending file.
	File string
	// Line is the 1-based line number where the codepoint appears.
	Line int
	// Column is the 1-based rune index within the line where the codepoint appears.
	Column int
	// Codepoint is the offending rune rendered as "U+XXXX".
	Codepoint string
	// Severity classifies the finding (currently always "high").
	Severity string
}

// emojiForbiddenExtensions enumerates the file suffixes covered by the emoji
// convention's hard ban (config files and source code). Any file whose path
// ends with one of these is scanned for emoji codepoints.
var emojiForbiddenExtensions = []string{
	".json",
	".yaml",
	".yml",
	".toml",
	".go",
	".ts",
	".tsx",
	".js",
	".jsx",
	".py",
	".java",
	".kt",
	".rs",
	".fs",
	".cs",
	".dart",
	".exs",
	".ex",
	".clj",
}

// emojiSkipDirs is the set of directory names skipped during the walk.
// These hold vendored or generated content that should not be linted.
var emojiSkipDirs = map[string]bool{
	"node_modules":           true,
	".git":                   true,
	".next":                  true,
	"dist":                   true,
	"build":                  true,
	"target":                 true,
	"generated":              true,
	"generated-contracts":    true,
	"generated-sources":      true,
	"generated-test-sources": true,
	"generated-reports":      true,
}

// AuditEmoji walks each path in paths and returns every emoji-codepoint
// finding in forbidden file types. The returned slice is sorted stably by
// (File, Line, Column). An error is returned only for I/O failures that
// prevent the scan from completing for a given path; per-file findings are
// not errors and are returned in the slice.
func AuditEmoji(paths []string) ([]EmojiFinding, error) {
	if len(paths) == 0 {
		return nil, errors.New("at least one path is required")
	}

	var findings []EmojiFinding
	for _, root := range paths {
		files := walkEmojiPaths(root)
		more, err := scanEmojiFiles(files)
		if err != nil {
			return nil, err
		}
		findings = append(findings, more...)
	}

	sort.SliceStable(findings, func(i, j int) bool {
		if findings[i].File != findings[j].File {
			return findings[i].File < findings[j].File
		}
		if findings[i].Line != findings[j].Line {
			return findings[i].Line < findings[j].Line
		}
		return findings[i].Column < findings[j].Column
	})
	return findings, nil
}

// walkEmojiPaths walks a single root and returns every file path whose
// extension is in emojiForbiddenExtensions, skipping the directories named
// in emojiSkipDirs. A missing root yields no paths and no error.
func walkEmojiPaths(root string) []string {
	var files []string
	_ = filepath.WalkDir(root, func(path string, d fs.DirEntry, err error) error {
		if err != nil {
			if os.IsNotExist(err) {
				return filepath.SkipAll
			}
			return err
		}
		if d.IsDir() {
			if emojiSkipDirs[d.Name()] {
				return filepath.SkipDir
			}
			return nil
		}
		if hasForbiddenEmojiExtension(d.Name()) {
			files = append(files, path)
		}
		return nil
	})
	sort.Strings(files)
	return files
}

// hasForbiddenEmojiExtension reports whether name's extension is one of the
// file types covered by the emoji ban.
func hasForbiddenEmojiExtension(name string) bool {
	lower := strings.ToLower(name)
	for _, ext := range emojiForbiddenExtensions {
		if strings.HasSuffix(lower, ext) {
			return true
		}
	}
	return false
}

// scanEmojiFiles reads each file and returns every emoji finding it contains.
func scanEmojiFiles(paths []string) ([]EmojiFinding, error) {
	var findings []EmojiFinding
	for _, p := range paths {
		more, err := scanEmojiFile(p)
		if err != nil {
			return nil, fmt.Errorf("scan %s: %w", p, err)
		}
		findings = append(findings, more...)
	}
	return findings, nil
}

// scanEmojiFile reads a single file line-by-line and reports every emoji
// codepoint it contains.
func scanEmojiFile(path string) ([]EmojiFinding, error) {
	f, err := os.Open(path) //nolint:gosec // trusted repo path
	if err != nil {
		return nil, err
	}
	defer func() { _ = f.Close() }()

	var findings []EmojiFinding
	scanner := bufio.NewScanner(f)
	// Allow long lines (minified JSON, etc.) up to 32 MiB per line.
	scanner.Buffer(make([]byte, 0, 64*1024), 32*1024*1024)
	lineNum := 0
	for scanner.Scan() {
		lineNum++
		line := scanner.Text()
		col := 0
		for _, r := range line {
			col++
			if isEmojiRune(r) {
				findings = append(findings, EmojiFinding{
					File:      path,
					Line:      lineNum,
					Column:    col,
					Codepoint: formatCodepoint(r),
					Severity:  "high",
				})
			}
		}
	}
	if err := scanner.Err(); err != nil {
		return nil, err
	}
	return findings, nil
}

// isEmojiRune reports whether r is an emoji codepoint that the emoji
// convention forbids inside source and config files. Regular CJK, Arabic,
// Hebrew, Cyrillic, and other natural-language scripts are NOT emoji and
// return false.
func isEmojiRune(r rune) bool {
	switch {
	// Misc Technical (subset overlapping with emoji presentation).
	case r >= 0x2300 && r <= 0x23FF:
		return true
	// Misc Symbols and Dingbats — covers U+2705 (heavy check mark),
	// U+274C (cross mark), U+26A0 (warning sign), and related glyphs.
	case r >= 0x2600 && r <= 0x27BF:
		return true
	// Emoji selectors and joiners.
	case r == 0x200D, r == 0xFE0F:
		return true
	// Supplemental Symbols and Pictographs plus the full Miscellaneous
	// Symbols and Pictographs block where most modern emoji live
	// (U+1F300-U+1FAFF and surrounding ranges).
	case r >= 0x1F000 && r <= 0x1FFFF:
		return true
	}
	return false
}

// formatCodepoint renders r as a stable "U+XXXX" string. Codepoints below
// 0x10000 are zero-padded to four hex digits; higher codepoints expand
// naturally (e.g., U+1F600).
func formatCodepoint(r rune) string {
	if r <= 0xFFFF {
		return fmt.Sprintf("U+%04X", r)
	}
	return fmt.Sprintf("U+%X", r)
}
