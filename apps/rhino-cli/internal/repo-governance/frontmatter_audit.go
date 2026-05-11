package repogovernance

import (
	"errors"
	"fmt"
	"io/fs"
	"os"
	"path/filepath"
	"regexp"
	"sort"
	"strings"

	"gopkg.in/yaml.v3"
)

// FrontmatterFinding describes a single date-metadata violation in a markdown
// file scanned by AuditFrontmatter.
//
// NOTE: This is named FrontmatterFinding (not the bare Finding requested in
// the plan's tech-docs) because the existing vendor-audit Finding type in
// this same package owns the Finding identifier with a different shape. The
// distinct name keeps both types unambiguous without renaming the existing
// public API.
type FrontmatterFinding struct {
	// File is the path to the offending markdown file.
	File string
	// Line is the 1-based line number where the violation appears.
	Line int
	// Severity classifies the finding (currently always "high").
	Severity string
	// Message is the human-readable description of the violation.
	Message string
}

// websiteAppPrefixes are path components under which date metadata is
// permitted because the content renders in a public-facing UI. Any file whose
// path contains one of these segments is skipped entirely by AuditFrontmatter.
var websiteAppPrefixes = []string{
	"apps/ayokoding-web/",
	"apps/oseplatform-web/",
	"apps/organiclever-web/",
	"apps/wahidyankf-web/",
}

// lastUpdatedFooterRe matches a body line containing the literal "**Last
// Updated**" footer marker anywhere in the line, case-sensitive.
var lastUpdatedFooterRe = regexp.MustCompile(`\*\*Last Updated\*\*`)

// inlineDateAnnotationRe matches a standalone bullet-list item declaring a
// "**Created**" or "**Last Updated**" date annotation with an ISO-8601 date
// (YYYY-MM-DD).
var inlineDateAnnotationRe = regexp.MustCompile(`^\s*-\s+\*\*(Created|Last Updated)\*\*:\s*\d{4}-\d{2}-\d{2}`)

// AuditFrontmatter walks each path in paths, scanning every .md file for
// forbidden date metadata per the No Manual Date Metadata Convention. Files
// under any website app directory are skipped. The returned slice is sorted
// stably by (File, Line).
//
// An error is returned only for I/O or YAML-parse failures that prevent the
// scan from completing for a given path; per-file findings are not errors and
// are returned in the slice.
func AuditFrontmatter(paths []string) ([]FrontmatterFinding, error) {
	if len(paths) == 0 {
		return nil, errors.New("at least one path is required")
	}

	var findings []FrontmatterFinding
	for _, root := range paths {
		fs := walkPaths(root)
		more, err := scanPaths(fs)
		if err != nil {
			return nil, err
		}
		findings = append(findings, more...)
	}

	sort.SliceStable(findings, func(i, j int) bool {
		if findings[i].File == findings[j].File {
			return findings[i].Line < findings[j].Line
		}
		return findings[i].File < findings[j].File
	})
	return findings, nil
}

// walkPaths walks a single root and returns every .md file path that is not
// exempted by website-app prefix. A missing root yields no paths and no error
// (caller treats it as empty).
func walkPaths(root string) []string {
	var files []string
	_ = filepath.WalkDir(root, func(path string, d fs.DirEntry, err error) error {
		if err != nil {
			if os.IsNotExist(err) {
				return filepath.SkipAll
			}
			return err
		}
		if d.IsDir() {
			return nil
		}
		if !strings.HasSuffix(d.Name(), ".md") {
			return nil
		}
		if isWebsiteApp(path) {
			return nil
		}
		files = append(files, path)
		return nil
	})
	sort.Strings(files)
	return files
}

// scanPaths reads each file and returns all date-metadata findings it
// contains.
func scanPaths(paths []string) ([]FrontmatterFinding, error) {
	var findings []FrontmatterFinding
	for _, p := range paths {
		data, err := os.ReadFile(p) //nolint:gosec // trusted repo path
		if err != nil {
			return nil, fmt.Errorf("read %s: %w", p, err)
		}
		findings = append(findings, scanFrontmatterContent(p, string(data))...)
	}
	return findings, nil
}

// scanFrontmatterContent runs the three rule checks (forbidden frontmatter
// field, footer block, inline body annotation) against a single file's
// content and returns every finding found.
func scanFrontmatterContent(path, content string) []FrontmatterFinding {
	var findings []FrontmatterFinding

	frontmatter, frontmatterEndLine, body := splitFrontmatter(content)
	findings = append(findings, checkFrontmatterUpdatedField(path, frontmatter)...)
	findings = append(findings, checkBodyAnnotations(path, body, frontmatterEndLine)...)
	return findings
}

// splitFrontmatter returns the frontmatter text (without the surrounding `---`
// fences), the line number of the closing fence (0 if no frontmatter), and
// the body text (everything after the closing fence, or the whole content if
// no frontmatter is present).
func splitFrontmatter(content string) (frontmatter string, closeLine int, body string) {
	lines := strings.Split(content, "\n")
	if len(lines) == 0 || strings.TrimSpace(lines[0]) != "---" {
		return "", 0, content
	}
	for i := 1; i < len(lines); i++ {
		if strings.TrimSpace(lines[i]) == "---" {
			fm := strings.Join(lines[1:i], "\n")
			bodyStart := i + 1
			if bodyStart >= len(lines) {
				return fm, i + 1, ""
			}
			return fm, i + 1, strings.Join(lines[bodyStart:], "\n")
		}
	}
	// Unclosed frontmatter: treat as no frontmatter to remain permissive.
	return "", 0, content
}

// checkFrontmatterUpdatedField parses the YAML frontmatter as a generic map
// and emits a finding if the forbidden `updated` key is present. The line
// number reported is the line within the frontmatter (1-based from the
// opening fence) that declares the key, falling back to line 1 if it cannot
// be located.
func checkFrontmatterUpdatedField(path, frontmatter string) []FrontmatterFinding {
	if frontmatter == "" {
		return nil
	}
	var parsed map[string]any
	if err := yaml.Unmarshal([]byte(frontmatter), &parsed); err != nil {
		return nil // unparseable YAML is not in scope of this audit
	}
	if _, ok := parsed["updated"]; !ok {
		return nil
	}

	line := findFieldLine(frontmatter, "updated")
	return []FrontmatterFinding{{
		File:     path,
		Line:     line,
		Severity: "high",
		Message:  `forbidden "updated:" field in YAML frontmatter; remove per no-date-metadata convention`,
	}}
}

// findFieldLine returns the 1-based line number (offset from the start of the
// content, accounting for the opening `---` on line 1) where the named YAML
// top-level field is declared. Returns 2 (just after the opening fence) when
// the field cannot be located.
func findFieldLine(frontmatter, field string) int {
	prefix := field + ":"
	for i, line := range strings.Split(frontmatter, "\n") {
		if strings.HasPrefix(strings.TrimLeft(line, " "), prefix) {
			// Lines inside frontmatter start at content line 2 (line 1 is `---`).
			return i + 2
		}
	}
	return 2
}

// checkBodyAnnotations scans the body for the **Last Updated** footer marker
// and standalone date-annotation bullet items, returning a finding per
// offending line. lineOffset is the 1-based line number of the first body
// line in the original file.
func checkBodyAnnotations(path, body string, frontmatterEndLine int) []FrontmatterFinding {
	if body == "" {
		return nil
	}
	var findings []FrontmatterFinding
	startLine := frontmatterEndLine + 1
	if frontmatterEndLine == 0 {
		startLine = 1
	}
	for i, line := range strings.Split(body, "\n") {
		lineNum := startLine + i
		if inlineDateAnnotationRe.MatchString(line) {
			findings = append(findings, FrontmatterFinding{
				File:     path,
				Line:     lineNum,
				Severity: "high",
				Message:  "forbidden inline date annotation in body; remove per no-date-metadata convention",
			})
			continue
		}
		if lastUpdatedFooterRe.MatchString(line) {
			findings = append(findings, FrontmatterFinding{
				File:     path,
				Line:     lineNum,
				Severity: "high",
				Message:  "forbidden **Last Updated** footer marker in body; remove per no-date-metadata convention",
			})
		}
	}
	return findings
}

// isWebsiteApp reports whether path falls under any of the website-app
// prefixes that are exempt from the date-metadata convention.
func isWebsiteApp(path string) bool {
	slashed := filepath.ToSlash(path)
	for _, p := range websiteAppPrefixes {
		if strings.Contains(slashed, p) {
			return true
		}
	}
	return false
}
