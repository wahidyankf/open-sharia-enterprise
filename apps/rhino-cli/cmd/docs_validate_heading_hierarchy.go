package cmd

import (
	"encoding/json"
	"fmt"
	"path/filepath"
	"strings"

	"github.com/spf13/cobra"
	"github.com/wahidyankf/ose-public/apps/rhino-cli/internal/cliout"
	"github.com/wahidyankf/ose-public/apps/rhino-cli/internal/docs"
)

// docsValidateHeadingHierarchyFn is the test-mockable entrypoint for the
// docs heading-hierarchy validator. Tests assign a stub to short-circuit
// filesystem walking; production code delegates to the internal package.
var docsValidateHeadingHierarchyFn = docs.ValidateDocsHeadingHierarchy

// docsValidateHeadingHierarchyDefaultPaths is the repo-relative path set
// scanned when the caller passes no positional args.
var docsValidateHeadingHierarchyDefaultPaths = []string{
	"docs/",
	"repo-governance/",
}

// docsValidateHeadingHierarchySchema is the JSON envelope schema
// identifier emitted by the --output json branch of this command.
const docsValidateHeadingHierarchySchema = "rhino-cli/docs-validate-heading-hierarchy/v1"

var docsValidateHeadingHierarchyCmd = &cobra.Command{
	Use:   "validate-heading-hierarchy [path...]",
	Short: "Validate markdown heading hierarchy (exactly one H1, no skipped levels)",
	Long: `Walk every .md file under the given paths and report violations of the
heading hierarchy rule from the Content Quality Convention:

  1. Each file must have exactly one H1 heading.
  2. Heading levels must not skip: H2 cannot be followed directly by H4,
     for example. Each heading's level must be at most (previous + 1).

Lines inside fenced code blocks (triple-backtick or triple-tilde) are
ignored for heading detection.

When no positional arguments are supplied, the command scans docs/ and
repo-governance/ relative to the git root.

Exit codes:
  0 — clean (no findings)
  1 — at least one finding present
  2 — invocation error (cannot find git root, I/O failure, etc.)`,
	Example: `  # Audit default paths
  rhino-cli docs validate-heading-hierarchy

  # Audit a single explicit path
  rhino-cli docs validate-heading-hierarchy docs/

  # JSON envelope output
  rhino-cli docs validate-heading-hierarchy -o json`,
	SilenceErrors: true,
	RunE:          runDocsValidateHeadingHierarchy,
}

func init() {
	docsCmd.AddCommand(docsValidateHeadingHierarchyCmd)
}

func runDocsValidateHeadingHierarchy(cmd *cobra.Command, args []string) error {
	repoRoot, err := findGitRoot()
	if err != nil {
		return fmt.Errorf("failed to find git repository root: %w", err)
	}

	relPaths := resolveDocsValidateHeadingHierarchyPaths(args)
	fullPaths := make([]string, 0, len(relPaths))
	for _, p := range relPaths {
		fullPaths = append(fullPaths, filepath.Join(repoRoot, p))
	}

	findings, err := docsValidateHeadingHierarchyFn(fullPaths)
	if err != nil {
		return fmt.Errorf("docs validate-heading-hierarchy failed: %w", err)
	}

	if err := writeFormattedV2(cmd, verbose, quiet, outputFuncs{
		text:     func(_, _ bool) string { return formatDocsValidateHeadingHierarchyText(findings) },
		json:     func() (string, error) { return formatDocsValidateHeadingHierarchyJSON(findings) },
		markdown: func() string { return formatDocsValidateHeadingHierarchyMarkdown(findings) },
	}); err != nil {
		return err
	}

	if len(findings) > 0 {
		return fmt.Errorf("%d docs heading hierarchy finding(s) found", len(findings))
	}
	return nil
}

// resolveDocsValidateHeadingHierarchyPaths picks the active scan path list,
// preferring positional args, falling back to the default set.
func resolveDocsValidateHeadingHierarchyPaths(args []string) []string {
	if len(args) > 0 {
		return args
	}
	return docsValidateHeadingHierarchyDefaultPaths
}

// formatDocsValidateHeadingHierarchyText returns a human-readable rendering
// of the findings.
func formatDocsValidateHeadingHierarchyText(findings []docs.DocsHeadingFinding) string {
	if len(findings) == 0 {
		return "DOCS HEADING HIERARCHY VALIDATION PASSED: no heading hierarchy violations found\n"
	}
	var sb strings.Builder
	fmt.Fprintf(&sb, "DOCS HEADING HIERARCHY VALIDATION FAILED: %d violation(s) found\n", len(findings))
	for _, f := range findings {
		fmt.Fprintf(&sb, "  %s:%d  [%s]  [%s]  %s\n", f.File, f.Line, f.Severity, f.Kind, f.Message)
	}
	return sb.String()
}

// formatDocsValidateHeadingHierarchyJSON returns the canonical JSON
// envelope for findings. The envelope schema is
// rhino-cli/docs-validate-heading-hierarchy/v1.
func formatDocsValidateHeadingHierarchyJSON(findings []docs.DocsHeadingFinding) (string, error) {
	type jsonFinding struct {
		File     string `json:"file"`
		Line     int    `json:"line"`
		Severity string `json:"severity"`
		Kind     string `json:"kind"`
		Message  string `json:"message"`
	}
	jf := make([]jsonFinding, 0, len(findings))
	for _, f := range findings {
		jf = append(jf, jsonFinding{
			File:     f.File,
			Line:     f.Line,
			Severity: f.Severity,
			Kind:     f.Kind,
			Message:  f.Message,
		})
	}
	status := "passed"
	if len(findings) > 0 {
		status = "failed"
	}
	env := cliout.Envelope[[]jsonFinding]{
		Schema: docsValidateHeadingHierarchySchema,
		Status: status,
		Result: jf,
	}
	data, err := json.MarshalIndent(env, "", "  ")
	if err != nil {
		return "", err
	}
	return string(data) + "\n", nil
}

// formatDocsValidateHeadingHierarchyMarkdown returns a markdown table of
// findings.
func formatDocsValidateHeadingHierarchyMarkdown(findings []docs.DocsHeadingFinding) string {
	if len(findings) == 0 {
		return "## Docs Heading Hierarchy Validation\n\n**PASSED**: no heading hierarchy violations found\n"
	}
	var sb strings.Builder
	fmt.Fprintf(&sb, "## Docs Heading Hierarchy Validation\n\n**FAILED**: %d violation(s) found\n\n", len(findings))
	sb.WriteString("| File | Line | Severity | Kind | Message |\n")
	sb.WriteString("|------|------|----------|------|---------|\n")
	for _, f := range findings {
		fmt.Fprintf(&sb, "| %s | %d | %s | %s | %s |\n", f.File, f.Line, f.Severity, f.Kind, f.Message)
	}
	return sb.String()
}
