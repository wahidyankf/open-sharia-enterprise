// Package docs provides documentation-focused validators consumed by the
// rhino-cli `docs` subcommand group.
//
// This file implements the docs frontmatter validator. The validator enforces
// two area-specific schemas:
//
//   - Software-engineering docs (under `docs/explanation/software-engineering/`)
//     require `title`, `description`, `category` (must equal "software"),
//     `subcategory`, and a non-empty `tags` list. Each violation is a
//     fail-level finding.
//   - Governance docs (under `repo-governance/conventions/`,
//     `repo-governance/principles/`, `repo-governance/development/`,
//     `repo-governance/workflows/`) require only `title`. `description` is
//     recommended; missing or empty `description` emits a warn-level finding
//     that is reported but does NOT flip the command's exit code.
//
// Files that fall outside both areas are not subject to any required-field
// rules and pass automatically.
package docs

import (
	"fmt"
	"io/fs"
	"os"
	"path/filepath"
	"sort"
	"strings"

	"gopkg.in/yaml.v3"
)

// DocsFrontmatterFinding describes a single frontmatter validation problem in
// a markdown file scanned by ValidateDocsFrontmatter.
type DocsFrontmatterFinding struct {
	// File is the path to the offending markdown file.
	File string
	// Severity is "fail" for required-field violations and "warn" for
	// recommended-field violations. Only "fail" findings flip the command
	// exit code; "warn" findings are reported only.
	Severity string
	// Kind is a stable machine-readable identifier of the violation kind
	// (e.g. "missing-title", "wrong-category-value").
	Kind string
	// Message is the human-readable description of the violation.
	Message string
}

// Finding severity constants.
const (
	severityFail = "fail"
	severityWarn = "warn"
)

// Finding kind constants.
const (
	kindInvalidYAML        = "invalid-yaml"
	kindMissingFrontmatter = "missing-frontmatter"
	kindMissingTitle       = "missing-title"
	kindMissingDescription = "missing-description"
	kindMissingCategory    = "missing-category"
	kindMissingSubcategory = "missing-subcategory"
	kindMissingTags        = "missing-tags"
	kindWrongCategoryValue = "wrong-category-value"
)

// softwareDocPrefix is the path segment that identifies a software-engineering
// doc subject to the strict required-field schema.
const softwareDocPrefix = "docs/explanation/software-engineering/"

// governanceDocPrefixes lists the path segments that identify governance docs
// subject to the lighter required-field schema (title required; description
// recommended).
var governanceDocPrefixes = []string{
	"repo-governance/conventions/",
	"repo-governance/principles/",
	"repo-governance/development/",
	"repo-governance/workflows/",
}

// frontmatterSkipDirs is the set of directory names walked-into but skipped
// wholesale because they hold third-party content or build artifacts.
var frontmatterSkipDirs = map[string]struct{}{
	"node_modules": {},
	".git":         {},
	".next":        {},
	"dist":         {},
	"build":        {},
	"target":       {},
}

// ValidateDocsFrontmatter walks each path in paths, scanning every .md file for
// frontmatter that violates the area-appropriate schema. Findings are returned
// sorted by (File, Kind) for stable output.
//
// An error is returned only for I/O failures that prevent the walk from
// completing; per-file violations are returned in the slice, not as errors.
// Non-existent paths are treated as empty (skipped) for resilience.
func ValidateDocsFrontmatter(paths []string) ([]DocsFrontmatterFinding, error) {
	if len(paths) == 0 {
		return nil, fmt.Errorf("at least one path is required")
	}

	var findings []DocsFrontmatterFinding
	for _, root := range paths {
		more, err := walkFrontmatterPath(root)
		if err != nil {
			return nil, err
		}
		findings = append(findings, more...)
	}

	sort.SliceStable(findings, func(i, j int) bool {
		if findings[i].File == findings[j].File {
			return findings[i].Kind < findings[j].Kind
		}
		return findings[i].File < findings[j].File
	})
	return findings, nil
}

// walkFrontmatterPath walks a single root and returns every frontmatter
// finding inside it. A missing root yields no findings and no error.
func walkFrontmatterPath(root string) ([]DocsFrontmatterFinding, error) {
	var findings []DocsFrontmatterFinding
	err := filepath.WalkDir(root, func(path string, d fs.DirEntry, walkErr error) error {
		if walkErr != nil {
			if os.IsNotExist(walkErr) {
				return filepath.SkipAll
			}
			return walkErr
		}
		if d.IsDir() {
			if _, skip := frontmatterSkipDirs[d.Name()]; skip {
				return filepath.SkipDir
			}
			return nil
		}
		if !strings.HasSuffix(d.Name(), ".md") {
			return nil
		}
		area := classifyDocArea(path)
		if area == areaUnknown {
			return nil
		}
		fileFindings, err := scanFrontmatterFile(path, area)
		if err != nil {
			return err
		}
		findings = append(findings, fileFindings...)
		return nil
	})
	if err != nil {
		return nil, fmt.Errorf("walk %s: %w", root, err)
	}
	return findings, nil
}

// docArea identifies the schema family that applies to a given file path.
type docArea int

const (
	areaUnknown docArea = iota
	areaSoftware
	areaGovernance
)

// classifyDocArea returns the schema family that applies to path. A file that
// belongs to neither known area returns areaUnknown and is skipped.
func classifyDocArea(path string) docArea {
	slashed := filepath.ToSlash(path)
	if strings.Contains(slashed, softwareDocPrefix) {
		return areaSoftware
	}
	for _, prefix := range governanceDocPrefixes {
		if strings.Contains(slashed, prefix) {
			return areaGovernance
		}
	}
	return areaUnknown
}

// scanFrontmatterFile reads the file at path, extracts and parses its YAML
// frontmatter, and returns every finding that applies to the file's area.
func scanFrontmatterFile(path string, area docArea) ([]DocsFrontmatterFinding, error) {
	data, err := os.ReadFile(path) //nolint:gosec // trusted repo path
	if err != nil {
		return nil, fmt.Errorf("read %s: %w", path, err)
	}
	frontmatter, ok := extractFrontmatter(string(data))
	if !ok {
		return []DocsFrontmatterFinding{{
			File:     path,
			Severity: severityFail,
			Kind:     kindMissingFrontmatter,
			Message:  "file has no YAML frontmatter (delimited by `---` fences)",
		}}, nil
	}
	var parsed map[string]any
	if err := yaml.Unmarshal([]byte(frontmatter), &parsed); err != nil {
		return []DocsFrontmatterFinding{{
			File:     path,
			Severity: severityFail,
			Kind:     kindInvalidYAML,
			Message:  fmt.Sprintf("frontmatter is not valid YAML: %v", err),
		}}, nil
	}
	switch area {
	case areaSoftware:
		return validateSoftwareSchema(path, parsed), nil
	case areaGovernance:
		return validateGovernanceSchema(path, parsed), nil
	case areaUnknown:
		// Caller already filtered out areaUnknown via classifyDocArea before
		// calling us; this case exists only to satisfy exhaustive switch
		// linting.
		return nil, nil
	default:
		return nil, nil
	}
}

// extractFrontmatter returns the raw YAML text between the leading and closing
// `---` fences. The second return value is false when the file has no
// frontmatter block (either no leading fence or no closing fence).
func extractFrontmatter(content string) (string, bool) {
	lines := strings.Split(content, "\n")
	if len(lines) == 0 || strings.TrimSpace(lines[0]) != "---" {
		return "", false
	}
	for i := 1; i < len(lines); i++ {
		if strings.TrimSpace(lines[i]) == "---" {
			return strings.Join(lines[1:i], "\n"), true
		}
	}
	return "", false
}

// validateSoftwareSchema enforces the strict required-field schema for
// software-engineering docs.
func validateSoftwareSchema(path string, fm map[string]any) []DocsFrontmatterFinding {
	var findings []DocsFrontmatterFinding
	if !hasNonEmptyString(fm, "title") {
		findings = append(findings, DocsFrontmatterFinding{
			File:     path,
			Severity: severityFail,
			Kind:     kindMissingTitle,
			Message:  `required field "title" is missing or empty`,
		})
	}
	if !hasNonEmptyString(fm, "description") {
		findings = append(findings, DocsFrontmatterFinding{
			File:     path,
			Severity: severityFail,
			Kind:     kindMissingDescription,
			Message:  `required field "description" is missing or empty`,
		})
	}
	if !hasNonEmptyString(fm, "category") {
		findings = append(findings, DocsFrontmatterFinding{
			File:     path,
			Severity: severityFail,
			Kind:     kindMissingCategory,
			Message:  `required field "category" is missing or empty`,
		})
	} else if v := stringValue(fm["category"]); v != "software" {
		findings = append(findings, DocsFrontmatterFinding{
			File:     path,
			Severity: severityFail,
			Kind:     kindWrongCategoryValue,
			Message:  fmt.Sprintf(`field "category" must equal "software"; found %q`, v),
		})
	}
	if !hasNonEmptyString(fm, "subcategory") {
		findings = append(findings, DocsFrontmatterFinding{
			File:     path,
			Severity: severityFail,
			Kind:     kindMissingSubcategory,
			Message:  `required field "subcategory" is missing or empty`,
		})
	}
	if !hasNonEmptyList(fm, "tags") {
		findings = append(findings, DocsFrontmatterFinding{
			File:     path,
			Severity: severityFail,
			Kind:     kindMissingTags,
			Message:  `required field "tags" must be a non-empty list`,
		})
	}
	return findings
}

// validateGovernanceSchema enforces the lighter required-field schema for
// governance docs: title required, description recommended (warn-only).
func validateGovernanceSchema(path string, fm map[string]any) []DocsFrontmatterFinding {
	var findings []DocsFrontmatterFinding
	if !hasNonEmptyString(fm, "title") {
		findings = append(findings, DocsFrontmatterFinding{
			File:     path,
			Severity: severityFail,
			Kind:     kindMissingTitle,
			Message:  `required field "title" is missing or empty`,
		})
	}
	if !hasNonEmptyString(fm, "description") {
		findings = append(findings, DocsFrontmatterFinding{
			File:     path,
			Severity: severityWarn,
			Kind:     kindMissingDescription,
			Message:  `recommended field "description" is missing or empty`,
		})
	}
	return findings
}

// hasNonEmptyString reports whether the given map contains a string-valued key
// with non-whitespace content.
func hasNonEmptyString(fm map[string]any, key string) bool {
	v, ok := fm[key]
	if !ok {
		return false
	}
	s := stringValue(v)
	return strings.TrimSpace(s) != ""
}

// stringValue coerces a YAML-unmarshalled value to its string representation.
// Non-string values render via fmt.Sprint so the caller can include them in
// human-readable messages without panicking.
func stringValue(v any) string {
	if v == nil {
		return ""
	}
	if s, ok := v.(string); ok {
		return s
	}
	return fmt.Sprint(v)
}

// hasNonEmptyList reports whether the given map contains a list-valued key
// with at least one element. YAML scalar lists unmarshal to []any.
func hasNonEmptyList(fm map[string]any, key string) bool {
	v, ok := fm[key]
	if !ok {
		return false
	}
	list, ok := v.([]any)
	if !ok {
		return false
	}
	return len(list) > 0
}

// HasFailFindings reports whether the given finding slice contains at least
// one fail-severity entry. Callers use it to decide the command exit code:
// warn-only findings are reported but do not flip exit to 1.
func HasFailFindings(findings []DocsFrontmatterFinding) bool {
	for _, f := range findings {
		if f.Severity == severityFail {
			return true
		}
	}
	return false
}
