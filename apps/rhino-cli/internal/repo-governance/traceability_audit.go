package repogovernance

import (
	"bufio"
	"bytes"
	"fmt"
	"io/fs"
	"os"
	"path/filepath"
	"regexp"
	"sort"
	"strings"
)

// TraceabilityFinding describes a single traceability violation detected by
// the governance traceability audit. The audit reports four kinds of finding:
//   - principle missing "## Vision Supported"
//   - convention missing "## Principles Implemented/Respected"
//   - development doc missing "## Principles Implemented/Respected"
//   - development doc missing "## Conventions Implemented/Respected"
//   - workflow with no `.claude/agents/<name>.md` reference in its body
//
// Path is the absolute filesystem path of the offending file. Line is the
// 1-based line number when the finding pinpoints a specific position (always
// 1 for "missing section" findings — the file itself is the failure). Kind is
// a stable machine-readable identifier. Message is the human-readable summary.
type TraceabilityFinding struct {
	// Path is the absolute path of the audited file.
	Path string
	// Line is the 1-based line number associated with the finding (1 when the
	// finding refers to the entire file rather than a specific line).
	Line int
	// Kind is the stable machine-readable finding category.
	Kind string
	// Message is the human-readable description of the violation.
	Message string
}

// traceability finding kinds.
const (
	// TraceabilityMissingVisionSupported indicates a principle file lacks the
	// "## Vision Supported" heading.
	TraceabilityMissingVisionSupported = "missing-vision-supported"
	// TraceabilityMissingPrinciplesImplemented indicates a convention or
	// development doc lacks the "## Principles Implemented/Respected" heading.
	TraceabilityMissingPrinciplesImplemented = "missing-principles-implemented"
	// TraceabilityMissingConventionsImplemented indicates a development doc
	// lacks the "## Conventions Implemented/Respected" heading.
	TraceabilityMissingConventionsImplemented = "missing-conventions-implemented"
	// TraceabilityMissingAgentReference indicates a workflow file does not
	// reference any `.claude/agents/<name>.md` agent definition.
	TraceabilityMissingAgentReference = "missing-agent-reference"
)

// Pre-compiled regexes for the audit. Each heading regex is anchored at line
// start so it does not match references to the heading text inside paragraphs.
var (
	visionSupportedRe          = regexp.MustCompile(`(?m)^##\s+Vision Supported\s*$`)
	principlesImplementedRe    = regexp.MustCompile(`(?m)^##\s+Principles Implemented/Respected\s*$`)
	conventionsImplementedRe   = regexp.MustCompile(`(?m)^##\s+Conventions Implemented/Respected\s*$`)
	claudeAgentReferenceRegexp = regexp.MustCompile(`\.claude/agents/[a-z0-9-]+\.md`)
)

// AuditTraceability walks the four governance subtrees rooted at repoRoot and
// returns every traceability finding. The walk uses these rules:
//
//   - repo-governance/principles/**/*.md — every non-README file must contain
//     a "## Vision Supported" H2 heading.
//   - repo-governance/conventions/**/*.md — every non-README file must contain
//     a "## Principles Implemented/Respected" H2 heading.
//   - repo-governance/development/**/*.md — every non-README file must contain
//     BOTH "## Principles Implemented/Respected" AND
//     "## Conventions Implemented/Respected" H2 headings.
//   - repo-governance/workflows/**/*.md — every non-README file must contain
//     at least one line matching `\.claude/agents/[a-z0-9-]+\.md`.
//
// `README.md` files are always exempt — they are index pages, not subject to
// the per-layer traceability schema. A missing subtree is treated as empty,
// not as an error.
//
// Findings are sorted by Path then Line for deterministic output.
func AuditTraceability(repoRoot string) ([]TraceabilityFinding, error) {
	var findings []TraceabilityFinding

	pf, err := auditPrinciples(filepath.Join(repoRoot, "repo-governance", "principles"))
	if err != nil {
		return nil, err
	}
	findings = append(findings, pf...)

	cf, err := auditConventions(filepath.Join(repoRoot, "repo-governance", "conventions"))
	if err != nil {
		return nil, err
	}
	findings = append(findings, cf...)

	df, err := auditDevelopment(filepath.Join(repoRoot, "repo-governance", "development"))
	if err != nil {
		return nil, err
	}
	findings = append(findings, df...)

	wf, err := auditWorkflows(filepath.Join(repoRoot, "repo-governance", "workflows"))
	if err != nil {
		return nil, err
	}
	findings = append(findings, wf...)

	sort.SliceStable(findings, func(i, j int) bool {
		if findings[i].Path == findings[j].Path {
			return findings[i].Line < findings[j].Line
		}
		return findings[i].Path < findings[j].Path
	})
	return findings, nil
}

// auditPrinciples scans each non-README .md file in root for the
// "## Vision Supported" H2 heading.
func auditPrinciples(root string) ([]TraceabilityFinding, error) {
	files, err := listGovernanceMarkdown(root)
	if err != nil {
		return nil, err
	}
	var findings []TraceabilityFinding
	for _, path := range files {
		data, err := os.ReadFile(path) //nolint:gosec // trusted repo path
		if err != nil {
			return nil, fmt.Errorf("read %s: %w", path, err)
		}
		if !visionSupportedRe.Match(data) {
			findings = append(findings, TraceabilityFinding{
				Path:    path,
				Line:    1,
				Kind:    TraceabilityMissingVisionSupported,
				Message: `principle is missing required "## Vision Supported" heading`,
			})
		}
	}
	return findings, nil
}

// auditConventions scans each non-README .md file in root for the
// "## Principles Implemented/Respected" H2 heading.
func auditConventions(root string) ([]TraceabilityFinding, error) {
	files, err := listGovernanceMarkdown(root)
	if err != nil {
		return nil, err
	}
	var findings []TraceabilityFinding
	for _, path := range files {
		data, err := os.ReadFile(path) //nolint:gosec // trusted repo path
		if err != nil {
			return nil, fmt.Errorf("read %s: %w", path, err)
		}
		if !principlesImplementedRe.Match(data) {
			findings = append(findings, TraceabilityFinding{
				Path:    path,
				Line:    1,
				Kind:    TraceabilityMissingPrinciplesImplemented,
				Message: `convention is missing required "## Principles Implemented/Respected" heading`,
			})
		}
	}
	return findings, nil
}

// auditDevelopment scans each non-README .md file in root for BOTH the
// "## Principles Implemented/Respected" AND "## Conventions Implemented/Respected"
// H2 headings.
func auditDevelopment(root string) ([]TraceabilityFinding, error) {
	files, err := listGovernanceMarkdown(root)
	if err != nil {
		return nil, err
	}
	var findings []TraceabilityFinding
	for _, path := range files {
		data, err := os.ReadFile(path) //nolint:gosec // trusted repo path
		if err != nil {
			return nil, fmt.Errorf("read %s: %w", path, err)
		}
		if !principlesImplementedRe.Match(data) {
			findings = append(findings, TraceabilityFinding{
				Path:    path,
				Line:    1,
				Kind:    TraceabilityMissingPrinciplesImplemented,
				Message: `development doc is missing required "## Principles Implemented/Respected" heading`,
			})
		}
		if !conventionsImplementedRe.Match(data) {
			findings = append(findings, TraceabilityFinding{
				Path:    path,
				Line:    1,
				Kind:    TraceabilityMissingConventionsImplemented,
				Message: `development doc is missing required "## Conventions Implemented/Respected" heading`,
			})
		}
	}
	return findings, nil
}

// traceabilityMetaExempt holds workflow paths (relative to the workflows root)
// that are reference documentation about the workflow system rather than
// executable orchestration workflows. They are exempt from the agent-reference
// requirement because they describe how to write workflows, not workflows that
// invoke agents.
var traceabilityMetaExempt = map[string]bool{
	"meta/execution-modes.md":     true,
	"meta/workflow-identifier.md": true,
}

// auditWorkflows scans each non-README .md file in root for at least one
// reference matching `.claude/agents/<name>.md`. Files listed in
// traceabilityMetaExempt are skipped.
func auditWorkflows(root string) ([]TraceabilityFinding, error) {
	files, err := listGovernanceMarkdown(root)
	if err != nil {
		return nil, err
	}
	var findings []TraceabilityFinding
	for _, path := range files {
		rel, _ := filepath.Rel(root, path)
		rel = filepath.ToSlash(rel)
		if traceabilityMetaExempt[rel] {
			continue
		}
		data, err := os.ReadFile(path) //nolint:gosec // trusted repo path
		if err != nil {
			return nil, fmt.Errorf("read %s: %w", path, err)
		}
		if !claudeAgentReferenceRegexp.Match(data) {
			line := firstNonEmptyLine(data)
			findings = append(findings, TraceabilityFinding{
				Path:    path,
				Line:    line,
				Kind:    TraceabilityMissingAgentReference,
				Message: `workflow does not reference any .claude/agents/<name>.md file`,
			})
		}
	}
	return findings, nil
}

// firstNonEmptyLine returns the 1-based line number of the first non-empty
// line in data, or 1 if the file is empty. Used so workflow "missing agent
// reference" findings still point at a meaningful position even though the
// failure is whole-file.
func firstNonEmptyLine(data []byte) int {
	scanner := bufio.NewScanner(bytes.NewReader(data))
	line := 0
	for scanner.Scan() {
		line++
		if strings.TrimSpace(scanner.Text()) != "" {
			return line
		}
	}
	return 1
}

// listGovernanceMarkdown walks root recursively and returns the sorted list of
// `.md` files excluding any file named `README.md`. A missing root yields an
// empty slice, not an error.
func listGovernanceMarkdown(root string) ([]string, error) {
	var files []string
	err := filepath.WalkDir(root, func(path string, d fs.DirEntry, err error) error {
		if err != nil {
			if os.IsNotExist(err) {
				return filepath.SkipAll
			}
			return err
		}
		if d.IsDir() {
			return nil
		}
		name := d.Name()
		if name == "README.md" {
			return nil
		}
		if !strings.HasSuffix(name, ".md") {
			return nil
		}
		files = append(files, path)
		return nil
	})
	if err != nil {
		return nil, err
	}
	sort.Strings(files)
	return files, nil
}
