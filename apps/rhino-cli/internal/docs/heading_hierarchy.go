// Heading-hierarchy validator for the rhino-cli `docs` subcommand group.
//
// This file lives in the same `docs` package as `naming.go` and reuses the
// shared `skipDirs` set declared there.
//
// The enforced rules match the [Content Quality Convention] under
// `repo-governance/`:
//
//  1. Every markdown file MUST have exactly one H1 heading.
//  2. Heading levels must not skip: each non-H1 heading's level must be at
//     most (previous-heading-level + 1).
//
// Lines inside fenced code blocks (triple backtick or triple tilde) are
// ignored for heading detection. Files with zero headings are accepted
// (the rules above only fire when a heading is found).
//
// [Content Quality Convention]: ../../../../repo-governance/conventions/writing/quality.md

package docs

import (
	"bufio"
	"fmt"
	"io"
	"io/fs"
	"os"
	"path/filepath"
	"sort"
	"strings"
)

// DocsHeadingFinding describes a single heading-hierarchy violation in a
// documentation tree scanned by ValidateDocsHeadingHierarchy.
type DocsHeadingFinding struct {
	// File is the path to the offending markdown file.
	File string
	// Line is the 1-based line number where the violation was detected.
	// Zero is used for whole-file findings (e.g., "missing H1").
	Line int
	// Severity classifies the finding (currently always "high").
	Severity string
	// Kind is a stable machine-readable code for the violation kind.
	// One of: "missing-h1", "duplicate-h1", "skipped-level".
	Kind string
	// Message is the human-readable description of the violation.
	Message string
}

// ValidateDocsHeadingHierarchy walks every path in `paths`, scanning each
// `.md` file and reporting heading-hierarchy violations. The returned slice
// is sorted first by File then by Line for stable output.
//
// An error is returned only for I/O failures that prevent the walk from
// completing; per-file violations are returned in the slice, not as errors.
// Non-existent paths are treated as empty (skipped) for resilience.
func ValidateDocsHeadingHierarchy(paths []string) ([]DocsHeadingFinding, error) {
	if len(paths) == 0 {
		return nil, fmt.Errorf("at least one path is required")
	}

	var findings []DocsHeadingFinding
	for _, root := range paths {
		more, err := walkHeadingHierarchyPath(root)
		if err != nil {
			return nil, err
		}
		findings = append(findings, more...)
	}

	sort.SliceStable(findings, func(i, j int) bool {
		if findings[i].File != findings[j].File {
			return findings[i].File < findings[j].File
		}
		return findings[i].Line < findings[j].Line
	})
	return findings, nil
}

// walkHeadingHierarchyPath walks a single root and returns every
// heading-hierarchy violation inside it. A missing root yields no findings
// and no error. `skipDirs` (declared in naming.go) is honored.
func walkHeadingHierarchyPath(root string) ([]DocsHeadingFinding, error) {
	var findings []DocsHeadingFinding
	err := filepath.WalkDir(root, func(path string, d fs.DirEntry, walkErr error) error {
		if walkErr != nil {
			if os.IsNotExist(walkErr) {
				return filepath.SkipAll
			}
			return walkErr
		}
		if d.IsDir() {
			if _, skip := skipDirs[d.Name()]; skip {
				return filepath.SkipDir
			}
			return nil
		}
		if !strings.HasSuffix(d.Name(), ".md") {
			return nil
		}
		more, err := scanFileHeadingHierarchy(path)
		if err != nil {
			return err
		}
		findings = append(findings, more...)
		return nil
	})
	if err != nil {
		return nil, fmt.Errorf("walk %s: %w", root, err)
	}
	return findings, nil
}

// heading carries the parsed information about a single heading line.
type heading struct {
	line  int
	level int
}

// scanFileHeadingHierarchy reads a single markdown file and reports any
// heading-hierarchy violations. Headings inside fenced code blocks (``` or
// ~~~) are ignored.
func scanFileHeadingHierarchy(path string) ([]DocsHeadingFinding, error) {
	f, err := os.Open(path)
	if err != nil {
		return nil, fmt.Errorf("open %s: %w", path, err)
	}
	defer func() { _ = f.Close() }()

	headings, err := collectHeadings(f)
	if err != nil {
		return nil, fmt.Errorf("read %s: %w", path, err)
	}

	return analyzeHeadings(path, headings), nil
}

// collectHeadings reads a markdown source line-by-line and returns the
// ordered list of headings, skipping anything inside a fenced code block.
// Empty input returns an empty slice with no error.
func collectHeadings(r io.Reader) ([]heading, error) {
	scanner := bufio.NewScanner(r)
	// Allow long lines (default 64 KiB token limit is fine for prose but
	// some prose files contain very long single lines — bump to 1 MiB).
	scanner.Buffer(make([]byte, 0, 64*1024), 1024*1024)

	var headings []heading
	inFence := false
	fenceMarker := "" // tracks "```" or "~~~" so a mismatched closer is ignored.
	lineNum := 0
	for scanner.Scan() {
		lineNum++
		line := scanner.Text()
		trimmed := strings.TrimLeft(line, " \t")
		if isFenceLine(trimmed) {
			marker := fenceMarkerOf(trimmed)
			switch {
			case !inFence:
				inFence = true
				fenceMarker = marker
			case marker == fenceMarker:
				inFence = false
				fenceMarker = ""
			}
			continue
		}
		if inFence {
			continue
		}
		if level, ok := parseHeadingLevel(trimmed); ok {
			headings = append(headings, heading{line: lineNum, level: level})
		}
	}
	if err := scanner.Err(); err != nil {
		return nil, err
	}
	return headings, nil
}

// isFenceLine reports whether the given trimmed-leading-whitespace line
// opens or closes a fenced code block. A fence is at least three consecutive
// backticks or tildes at the start of the line (info-string optional).
func isFenceLine(s string) bool {
	if strings.HasPrefix(s, "```") {
		return true
	}
	if strings.HasPrefix(s, "~~~") {
		return true
	}
	return false
}

// fenceMarkerOf returns the canonical marker token for a fence line ("```"
// or "~~~"). Pre-condition: isFenceLine(s) == true.
func fenceMarkerOf(s string) string {
	if strings.HasPrefix(s, "```") {
		return "```"
	}
	return "~~~"
}

// parseHeadingLevel returns the heading level (1-6) for a markdown ATX
// heading line and `true`, or (0, false) for non-heading lines. The input
// must already have leading whitespace stripped.
//
// A heading is at least one consecutive `#` followed by a space and at
// least one non-space character (e.g. `# Title`). Markdown ATX rules allow
// 1-6 leading `#` chars; we cap at 6.
func parseHeadingLevel(s string) (int, bool) {
	if len(s) == 0 || s[0] != '#' {
		return 0, false
	}
	level := 0
	for level < len(s) && s[level] == '#' {
		level++
	}
	if level < 1 || level > 6 {
		return 0, false
	}
	// Must be followed by a space, then non-empty content. A bare `#`
	// (no content) or `#hashtag` (no space) is not a heading.
	if level >= len(s) {
		return 0, false
	}
	if s[level] != ' ' && s[level] != '\t' {
		return 0, false
	}
	rest := strings.TrimSpace(s[level+1:])
	if rest == "" {
		return 0, false
	}
	return level, true
}

// analyzeHeadings applies the heading-hierarchy rules to an ordered slice
// of headings and returns any violations.
//
// Rules:
//   - If at least one heading is present, exactly one of them must be H1.
//     Zero H1 with non-zero headings, or more than one H1, both fire
//     findings.
//   - Each heading's level must be at most (previous-heading-level + 1).
//     The first heading sets the baseline; subsequent ones are compared
//     against it.
//
// Files with no headings at all (e.g. a single-line markdown file) are
// accepted with zero findings.
func analyzeHeadings(file string, headings []heading) []DocsHeadingFinding {
	if len(headings) == 0 {
		return nil
	}

	var findings []DocsHeadingFinding

	// Rule 1: exactly one H1.
	h1Count := 0
	var firstH1Line, secondH1Line int
	for _, h := range headings {
		if h.level == 1 {
			h1Count++
			switch h1Count {
			case 1:
				firstH1Line = h.line
			case 2:
				secondH1Line = h.line
			}
		}
	}
	switch {
	case h1Count == 0:
		findings = append(findings, DocsHeadingFinding{
			File:     file,
			Line:     headings[0].line,
			Severity: "high",
			Kind:     "missing-h1",
			Message:  "markdown file has no H1 heading; every documented file must have exactly one H1",
		})
	case h1Count >= 2:
		findings = append(findings, DocsHeadingFinding{
			File:     file,
			Line:     secondH1Line,
			Severity: "high",
			Kind:     "duplicate-h1",
			Message: fmt.Sprintf(
				"markdown file has %d H1 headings (first at line %d); every file must have exactly one H1",
				h1Count, firstH1Line,
			),
		})
	}

	// Rule 2: no skipped levels.
	for i := 1; i < len(headings); i++ {
		prev := headings[i-1].level
		cur := headings[i].level
		if cur > prev+1 {
			findings = append(findings, DocsHeadingFinding{
				File:     file,
				Line:     headings[i].line,
				Severity: "high",
				Kind:     "skipped-level",
				Message: fmt.Sprintf(
					"H%d heading follows H%d, skipping H%d; heading levels must not skip",
					cur, prev, prev+1,
				),
			})
		}
	}

	return findings
}
