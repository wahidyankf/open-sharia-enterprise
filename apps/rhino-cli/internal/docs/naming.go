// Package docs provides documentation-focused validators consumed by the
// rhino-cli `docs` subcommand group. The validators are pure with respect to
// the filesystem: callers supply paths and the validator returns findings.
//
// This file implements the docs filename-naming validator. The enforced rule
// matches the [File Naming Convention] under `repo-governance/`:
//
//	basename matches ^[a-z0-9-]+\.md$
//
// `README.md` is exempt regardless of placement. Additional exemption globs
// may be supplied by the caller (matched against the basename only, via
// `filepath.Match`).
//
// [File Naming Convention]: ../../../../repo-governance/conventions/structure/file-naming.md
package docs

import (
	"fmt"
	"io/fs"
	"os"
	"path/filepath"
	"regexp"
	"sort"
	"strings"
)

// DocsNamingFinding describes a single filename-naming violation in a
// documentation tree scanned by ValidateDocsNaming.
type DocsNamingFinding struct {
	// File is the path to the offending markdown file.
	File string
	// Severity classifies the finding (currently always "high").
	Severity string
	// Message is the human-readable description of the violation.
	Message string
}

// kebabCaseRe is the canonical lowercase-kebab-case pattern enforced on every
// markdown basename. The pattern intentionally rejects underscores, uppercase
// letters, and any character outside [a-z0-9-].
var kebabCaseRe = regexp.MustCompile(`^[a-z0-9-]+\.md$`)

// skipDirs is the set of directory names walked-into but skipped wholesale
// because they hold third-party content or build artifacts.
var skipDirs = map[string]struct{}{
	"node_modules": {},
	".git":         {},
	".next":        {},
	"dist":         {},
	"build":        {},
	"target":       {},
}

// ValidateDocsNaming walks every path in `paths`, scanning each `.md` file and
// reporting basenames that violate the lowercase-kebab-case rule. `README.md`
// is exempt regardless of placement. Each entry in `exemptGlobs` is matched
// against the basename via [filepath.Match]; matching basenames are exempt as
// well. The returned slice is sorted by File for stable output.
//
// An error is returned only for I/O failures that prevent the walk from
// completing; per-file violations are returned in the slice, not as errors.
// Non-existent paths are treated as empty (skipped) for resilience.
func ValidateDocsNaming(paths []string, exemptGlobs []string) ([]DocsNamingFinding, error) {
	if len(paths) == 0 {
		return nil, fmt.Errorf("at least one path is required")
	}

	// Validate the glob patterns upfront so a malformed pattern surfaces a
	// helpful error before we begin walking.
	for _, pat := range exemptGlobs {
		if _, err := filepath.Match(pat, "x"); err != nil {
			return nil, fmt.Errorf("invalid exempt glob %q: %w", pat, err)
		}
	}

	var findings []DocsNamingFinding
	for _, root := range paths {
		more, err := walkNamingPath(root, exemptGlobs)
		if err != nil {
			return nil, err
		}
		findings = append(findings, more...)
	}

	sort.SliceStable(findings, func(i, j int) bool {
		return findings[i].File < findings[j].File
	})
	return findings, nil
}

// walkNamingPath walks a single root and returns every naming violation
// inside it. A missing root yields no findings and no error.
func walkNamingPath(root string, exemptGlobs []string) ([]DocsNamingFinding, error) {
	var findings []DocsNamingFinding
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
		base := d.Name()
		if !strings.HasSuffix(base, ".md") {
			return nil
		}
		if isNamingExempt(base, exemptGlobs) {
			return nil
		}
		if !kebabCaseRe.MatchString(base) {
			findings = append(findings, DocsNamingFinding{
				File:     path,
				Severity: "high",
				Message: fmt.Sprintf(
					"filename %q violates lowercase-kebab-case rule (^[a-z0-9-]+\\.md$); rename to lowercase-kebab-case or add an exemption",
					base,
				),
			})
		}
		return nil
	})
	if err != nil {
		return nil, fmt.Errorf("walk %s: %w", root, err)
	}
	return findings, nil
}

// isNamingExempt reports whether `basename` is exempt from the kebab-case
// rule. `README.md` is exempt unconditionally. Any caller-supplied glob that
// matches the basename also exempts it.
func isNamingExempt(basename string, exemptGlobs []string) bool {
	if basename == "README.md" {
		return true
	}
	for _, pat := range exemptGlobs {
		if matched, _ := filepath.Match(pat, basename); matched {
			return true
		}
	}
	return false
}
