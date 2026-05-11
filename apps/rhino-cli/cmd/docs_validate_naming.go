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

// docsValidateNamingFn is the test-mockable entrypoint for the docs
// filename-naming validator. Tests assign a stub to short-circuit filesystem
// walking; production code delegates to the internal package.
var docsValidateNamingFn = docs.ValidateDocsNaming

// docsValidateNamingExempts holds the values supplied via repeatable
// `--exempt <glob>` flags. Each entry is a filepath.Match-style glob compared
// against the basename only.
var docsValidateNamingExempts []string

// docsValidateNamingDefaultPaths is the repo-relative path set scanned when
// the caller passes neither positional args nor flags.
var docsValidateNamingDefaultPaths = []string{
	"docs/",
	"repo-governance/",
}

// docsValidateNamingSchema is the JSON envelope schema identifier emitted by
// the --output json branch of this command.
const docsValidateNamingSchema = "rhino-cli/docs-validate-naming/v1"

var docsValidateNamingCmd = &cobra.Command{
	Use:   "validate-naming [path...]",
	Short: "Validate markdown filenames against the lowercase-kebab-case rule",
	Long: `Walk every .md file under the given paths and report basenames that
violate the File Naming Convention's lowercase-kebab-case rule:

  ^[a-z0-9-]+\.md$

Exemptions:
  - README.md is exempt at any depth (it is the canonical directory index).
  - Additional exemption globs may be supplied via repeatable --exempt; each
    pattern is matched against the basename only via filepath.Match (e.g.
    --exempt 'AGENTS.md' or --exempt '[A-Z]*.md').

When neither positional arguments nor --exempt overrides are supplied, the
command scans docs/ and repo-governance/ relative to the git root.

Exit codes:
  0 — clean (no findings)
  1 — at least one finding present
  2 — invocation error (cannot find git root, invalid glob, I/O failure, etc.)`,
	Example: `  # Audit default paths
  rhino-cli docs validate-naming

  # Audit a single explicit path
  rhino-cli docs validate-naming docs/

  # Audit with extra exempt globs
  rhino-cli docs validate-naming --exempt 'AGENTS.md' --exempt 'CLAUDE.md'

  # JSON envelope output
  rhino-cli docs validate-naming -o json`,
	SilenceErrors: true,
	RunE:          runDocsValidateNaming,
}

func init() {
	docsValidateNamingCmd.Flags().StringArrayVar(
		&docsValidateNamingExempts,
		"exempt",
		nil,
		"basename glob to exempt from the kebab-case rule (repeatable)",
	)
	docsCmd.AddCommand(docsValidateNamingCmd)
}

func runDocsValidateNaming(cmd *cobra.Command, args []string) error {
	repoRoot, err := findGitRoot()
	if err != nil {
		return fmt.Errorf("failed to find git repository root: %w", err)
	}

	relPaths := resolveDocsValidateNamingPaths(args)
	fullPaths := make([]string, 0, len(relPaths))
	for _, p := range relPaths {
		fullPaths = append(fullPaths, filepath.Join(repoRoot, p))
	}

	findings, err := docsValidateNamingFn(fullPaths, docsValidateNamingExempts)
	if err != nil {
		return fmt.Errorf("docs validate-naming failed: %w", err)
	}

	if err := writeFormattedV2(cmd, verbose, quiet, outputFuncs{
		text:     func(_, _ bool) string { return formatDocsValidateNamingText(findings) },
		json:     func() (string, error) { return formatDocsValidateNamingJSON(findings) },
		markdown: func() string { return formatDocsValidateNamingMarkdown(findings) },
	}); err != nil {
		return err
	}

	if len(findings) > 0 {
		return fmt.Errorf("%d docs naming finding(s) found", len(findings))
	}
	return nil
}

// resolveDocsValidateNamingPaths picks the active scan path list, preferring
// positional args, falling back to the default set.
func resolveDocsValidateNamingPaths(args []string) []string {
	if len(args) > 0 {
		return args
	}
	return docsValidateNamingDefaultPaths
}

// formatDocsValidateNamingText returns a human-readable rendering of findings.
func formatDocsValidateNamingText(findings []docs.DocsNamingFinding) string {
	if len(findings) == 0 {
		return "DOCS NAMING VALIDATION PASSED: no naming violations found\n"
	}
	var sb strings.Builder
	fmt.Fprintf(&sb, "DOCS NAMING VALIDATION FAILED: %d violation(s) found\n", len(findings))
	for _, f := range findings {
		fmt.Fprintf(&sb, "  %s  [%s]  %s\n", f.File, f.Severity, f.Message)
	}
	return sb.String()
}

// formatDocsValidateNamingJSON returns the canonical JSON envelope for
// findings. The envelope schema is rhino-cli/docs-validate-naming/v1.
func formatDocsValidateNamingJSON(findings []docs.DocsNamingFinding) (string, error) {
	type jsonFinding struct {
		File     string `json:"file"`
		Severity string `json:"severity"`
		Message  string `json:"message"`
	}
	jf := make([]jsonFinding, 0, len(findings))
	for _, f := range findings {
		jf = append(jf, jsonFinding{File: f.File, Severity: f.Severity, Message: f.Message})
	}
	status := "passed"
	if len(findings) > 0 {
		status = "failed"
	}
	env := cliout.Envelope[[]jsonFinding]{
		Schema: docsValidateNamingSchema,
		Status: status,
		Result: jf,
	}
	data, err := json.MarshalIndent(env, "", "  ")
	if err != nil {
		return "", err
	}
	return string(data) + "\n", nil
}

// formatDocsValidateNamingMarkdown returns a markdown table of findings.
func formatDocsValidateNamingMarkdown(findings []docs.DocsNamingFinding) string {
	if len(findings) == 0 {
		return "## Docs Filename Naming Validation\n\n**PASSED**: no naming violations found\n"
	}
	var sb strings.Builder
	fmt.Fprintf(&sb, "## Docs Filename Naming Validation\n\n**FAILED**: %d violation(s) found\n\n", len(findings))
	sb.WriteString("| File | Severity | Message |\n")
	sb.WriteString("|------|----------|---------|\n")
	for _, f := range findings {
		fmt.Fprintf(&sb, "| %s | %s | %s |\n", f.File, f.Severity, f.Message)
	}
	return sb.String()
}
