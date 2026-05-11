package cmd

import (
	"encoding/json"
	"fmt"
	"path/filepath"
	"strings"

	"github.com/spf13/cobra"
	"github.com/wahidyankf/ose-public/apps/rhino-cli/internal/cliout"
	governance "github.com/wahidyankf/ose-public/apps/rhino-cli/internal/repo-governance"
)

// frontmatterAuditFn is the test-mockable entrypoint for the frontmatter audit.
var frontmatterAuditFn = governance.AuditFrontmatter

// frontmatterAuditPaths holds the --path repeatable flag values.
var frontmatterAuditPaths []string

// frontmatterAuditDefaultPaths is the set of repo-relative paths scanned when
// the caller does not pass --path explicitly.
var frontmatterAuditDefaultPaths = []string{
	"repo-governance/",
	"docs/explanation/software-engineering/",
	".claude/agents/",
	".claude/skills/",
	"plans/",
}

// frontmatterAuditSchema is the JSON envelope schema identifier for this
// command's JSON output.
const frontmatterAuditSchema = "rhino-cli/frontmatter-audit/v1"

var governanceFrontmatterAuditCmd = &cobra.Command{
	Use:   "frontmatter-audit [path...]",
	Short: "Audit markdown files for forbidden manual date metadata",
	Long: `Walk every .md file under the given paths and report violations of the
No Manual Date Metadata Convention:

  - Forbidden 'updated:' field in YAML frontmatter
  - Forbidden '**Last Updated**' footer marker in the document body
  - Forbidden standalone inline date annotations such as
    '- **Created**: YYYY-MM-DD' or '- **Last Updated**: YYYY-MM-DD' in the body

Files whose path falls under any website app directory
(apps/ayokoding-web/, apps/oseplatform-web/, apps/organiclever-web/,
apps/wahidyankf-web/) are exempt from the rules and silently skipped.

When neither positional arguments nor --path flags are supplied, the command
scans the canonical governance paths: repo-governance/,
docs/explanation/software-engineering/, .claude/agents/, .claude/skills/,
and plans/.

Exit codes:
  0 — clean (no findings)
  1 — at least one finding present
  2 — invocation error (cannot find git root, I/O failure, etc.)`,
	Example: `  # Audit the default governance paths
  rhino-cli repo-governance frontmatter-audit

  # Audit a single path explicitly
  rhino-cli repo-governance frontmatter-audit repo-governance/

  # Audit multiple paths via repeatable --path flag
  rhino-cli repo-governance frontmatter-audit --path repo-governance/ --path docs/

  # Emit JSON envelope output
  rhino-cli repo-governance frontmatter-audit -o json`,
	SilenceErrors: true,
	RunE:          runGovernanceFrontmatterAudit,
}

func init() {
	governanceFrontmatterAuditCmd.Flags().StringArrayVarP(
		&frontmatterAuditPaths,
		"path",
		"p",
		nil,
		"path to scan (repeatable; relative to git root)",
	)
	repoGovernanceCmd.AddCommand(governanceFrontmatterAuditCmd)
}

func runGovernanceFrontmatterAudit(cmd *cobra.Command, args []string) error {
	repoRoot, err := findGitRoot()
	if err != nil {
		return fmt.Errorf("failed to find git repository root: %w", err)
	}

	relPaths := resolveFrontmatterAuditPaths(args, frontmatterAuditPaths)
	fullPaths := make([]string, 0, len(relPaths))
	for _, p := range relPaths {
		fullPaths = append(fullPaths, filepath.Join(repoRoot, p))
	}

	findings, err := frontmatterAuditFn(fullPaths)
	if err != nil {
		return fmt.Errorf("frontmatter audit failed: %w", err)
	}

	if err := writeFormattedV2(cmd, verbose, quiet, outputFuncs{
		text:     func(_, _ bool) string { return formatFrontmatterAuditText(findings) },
		json:     func() (string, error) { return formatFrontmatterAuditJSON(findings) },
		markdown: func() string { return formatFrontmatterAuditMarkdown(findings) },
	}); err != nil {
		return err
	}

	if len(findings) > 0 {
		return fmt.Errorf("%d frontmatter finding(s) found", len(findings))
	}
	// Reset the flag slice so successive invocations (especially in tests) do
	// not accumulate values across calls.
	frontmatterAuditPaths = nil
	return nil
}

// resolveFrontmatterAuditPaths picks the active scan path list, preferring
// positional args, then --path flags, then the default set.
func resolveFrontmatterAuditPaths(args, flagPaths []string) []string {
	switch {
	case len(args) > 0:
		return args
	case len(flagPaths) > 0:
		return flagPaths
	default:
		return frontmatterAuditDefaultPaths
	}
}

// formatFrontmatterAuditText returns the human-readable rendering of
// findings.
func formatFrontmatterAuditText(findings []governance.FrontmatterFinding) string {
	if len(findings) == 0 {
		return "FRONTMATTER AUDIT PASSED: no date-metadata violations found\n"
	}
	var sb strings.Builder
	fmt.Fprintf(&sb, "FRONTMATTER AUDIT FAILED: %d violation(s) found\n", len(findings))
	for _, f := range findings {
		fmt.Fprintf(&sb, "  %s:%d  [%s]  %s\n", f.File, f.Line, f.Severity, f.Message)
	}
	return sb.String()
}

// formatFrontmatterAuditJSON returns the canonical JSON envelope for findings.
func formatFrontmatterAuditJSON(findings []governance.FrontmatterFinding) (string, error) {
	type jsonFinding struct {
		File     string `json:"file"`
		Line     int    `json:"line"`
		Severity string `json:"severity"`
		Message  string `json:"message"`
	}
	jf := make([]jsonFinding, 0, len(findings))
	for _, f := range findings {
		jf = append(jf, jsonFinding{File: f.File, Line: f.Line, Severity: f.Severity, Message: f.Message})
	}
	status := "passed"
	if len(findings) > 0 {
		status = "failed"
	}
	env := cliout.Envelope[[]jsonFinding]{
		Schema: frontmatterAuditSchema,
		Status: status,
		Result: jf,
	}
	data, err := json.MarshalIndent(env, "", "  ")
	if err != nil {
		return "", err
	}
	return string(data) + "\n", nil
}

// formatFrontmatterAuditMarkdown returns a markdown table of findings.
func formatFrontmatterAuditMarkdown(findings []governance.FrontmatterFinding) string {
	if len(findings) == 0 {
		return "## Governance Frontmatter Audit\n\n**PASSED**: no date-metadata violations found\n"
	}
	var sb strings.Builder
	fmt.Fprintf(&sb, "## Governance Frontmatter Audit\n\n**FAILED**: %d violation(s) found\n\n", len(findings))
	sb.WriteString("| File | Line | Severity | Message |\n")
	sb.WriteString("|------|------|----------|---------|\n")
	for _, f := range findings {
		fmt.Fprintf(&sb, "| %s | %d | %s | %s |\n", f.File, f.Line, f.Severity, f.Message)
	}
	return sb.String()
}
