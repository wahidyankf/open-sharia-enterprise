package cmd

import (
	"encoding/json"
	"fmt"
	"io"
	"path/filepath"
	"strings"

	"github.com/spf13/cobra"
	"github.com/wahidyankf/ose-public/apps/rhino-cli/internal/cliout"
	governance "github.com/wahidyankf/ose-public/apps/rhino-cli/internal/repo-governance"
)

// readmeIndexAuditFn is the test-mockable entrypoint for the README index audit.
var readmeIndexAuditFn = governance.AuditReadmeIndex

// readmeIndexAuditExcludes holds the --exclude repeatable flag values.
var readmeIndexAuditExcludes []string

// readmeIndexAuditDefaultPaths is the set of repo-relative paths scanned when
// the caller does not pass positional arguments.
var readmeIndexAuditDefaultPaths = []string{
	"repo-governance/",
	".claude/agents/",
	".claude/skills/",
	"docs/explanation/software-engineering/",
}

// readmeIndexAuditSchema is the JSON envelope schema identifier for this
// command's JSON output.
const readmeIndexAuditSchema = "rhino-cli/readme-index-audit/v1"

// readmeIndexAuditPayload is the inner JSON envelope payload — a list of
// findings rendered with stable field ordering.
type readmeIndexAuditPayload struct {
	Findings []readmeIndexAuditFinding `json:"findings"`
}

// readmeIndexAuditFinding is the JSON-rendered finding shape (mirrors the
// internal ReadmeIndexFinding with json tags).
type readmeIndexAuditFinding struct {
	File     string `json:"file"`
	Severity string `json:"severity"`
	Kind     string `json:"kind"`
	Message  string `json:"message"`
}

var governanceReadmeIndexAuditCmd = &cobra.Command{
	Use:   "readme-index-audit [path...]",
	Short: "Audit directory README.md indexes against the sibling markdown files",
	Long: `Walk every README.md file under the given paths and verify that its link
listings cover every sibling markdown file and immediate subdirectory README.

For each README.md found, the audit compares the README's relative markdown
link targets against:

  - the actual *.md files alongside the README in the same directory
    (excluding the README itself), and
  - the immediate subdirectories that contain their own README.md.

Two kinds of findings are reported:

  - "orphan" — a sibling .md file exists on disk but is not linked from the
    README.
  - "ghost"  — the README links to a .md target that is not present on disk.

When neither positional arguments nor --exclude flags exclude the path, the
command scans these canonical paths: repo-governance/, .claude/agents/,
.claude/skills/, docs/explanation/software-engineering/.

Exit codes:
  0 — clean (no findings)
  1 — at least one finding present
  2 — invocation error (cannot find git root, I/O failure, etc.)`,
	Example: `  # Audit the default governance paths
  rhino-cli repo-governance readme-index-audit

  # Audit a single path explicitly
  rhino-cli repo-governance readme-index-audit repo-governance/

  # Exclude generated or scratch directories with a repeatable glob
  rhino-cli repo-governance readme-index-audit --exclude "*.tmp.md" --exclude scratch

  # Emit JSON envelope output
  rhino-cli repo-governance readme-index-audit -o json`,
	SilenceErrors: true,
	RunE:          runGovernanceReadmeIndexAudit,
}

func init() {
	governanceReadmeIndexAuditCmd.Flags().StringArrayVar(
		&readmeIndexAuditExcludes,
		"exclude",
		nil,
		"glob to exclude from audit (repeatable)",
	)
	repoGovernanceCmd.AddCommand(governanceReadmeIndexAuditCmd)
}

// runGovernanceReadmeIndexAudit resolves the scan paths, runs the audit, and
// dispatches the formatted output. It returns a non-nil error when one or
// more findings are present so the caller exits 1.
func runGovernanceReadmeIndexAudit(cmd *cobra.Command, args []string) error {
	repoRoot, err := findGitRoot()
	if err != nil {
		return fmt.Errorf("failed to find git repository root: %w", err)
	}

	relPaths := resolveReadmeIndexAuditPaths(args)
	fullPaths := make([]string, 0, len(relPaths))
	for _, p := range relPaths {
		fullPaths = append(fullPaths, filepath.Join(repoRoot, p))
	}

	findings, err := readmeIndexAuditFn(fullPaths, readmeIndexAuditExcludes)
	if err != nil {
		return fmt.Errorf("readme-index audit failed: %w", err)
	}

	dispatcher := cliout.NewDispatcher(
		writeReadmeIndexAuditText,
		writeReadmeIndexAuditJSON,
		writeReadmeIndexAuditMarkdown,
	)
	if err := dispatcher.Write(cmd.OutOrStdout(), outputFormat, findings); err != nil {
		return fmt.Errorf("write output: %w", err)
	}

	// Reset the flag slice so successive invocations (especially in tests) do
	// not accumulate values across calls.
	defer func() { readmeIndexAuditExcludes = nil }()

	if len(findings) > 0 {
		return fmt.Errorf("%d readme-index finding(s) found", len(findings))
	}
	return nil
}

// resolveReadmeIndexAuditPaths picks the active scan path list, preferring
// positional args, then the default set.
func resolveReadmeIndexAuditPaths(args []string) []string {
	if len(args) > 0 {
		return args
	}
	return readmeIndexAuditDefaultPaths
}

// writeReadmeIndexAuditText renders findings as a human-readable summary.
func writeReadmeIndexAuditText(w io.Writer, findings []governance.ReadmeIndexFinding) error {
	if len(findings) == 0 {
		_, err := fmt.Fprint(w, "README INDEX AUDIT PASSED: no orphan or ghost references found\n")
		return err
	}
	var sb strings.Builder
	fmt.Fprintf(&sb, "README INDEX AUDIT FAILED: %d finding(s)\n", len(findings))
	for _, f := range findings {
		fmt.Fprintf(&sb, "  %s  [%s/%s]  %s\n", f.File, f.Severity, f.Kind, f.Message)
	}
	_, err := fmt.Fprint(w, sb.String())
	return err
}

// writeReadmeIndexAuditJSON renders findings as a canonical JSON envelope.
func writeReadmeIndexAuditJSON(w io.Writer, findings []governance.ReadmeIndexFinding) error {
	payload := readmeIndexAuditPayload{Findings: make([]readmeIndexAuditFinding, 0, len(findings))}
	for _, f := range findings {
		payload.Findings = append(payload.Findings, readmeIndexAuditFinding{
			File:     f.File,
			Severity: f.Severity,
			Kind:     f.Kind,
			Message:  f.Message,
		})
	}
	status := "passed"
	if len(findings) > 0 {
		status = "failed"
	}
	env := cliout.Envelope[readmeIndexAuditPayload]{
		Schema: readmeIndexAuditSchema,
		Status: status,
		Result: payload,
	}
	data, err := json.MarshalIndent(env, "", "  ")
	if err != nil {
		return fmt.Errorf("marshal envelope: %w", err)
	}
	if _, err := w.Write(data); err != nil {
		return err
	}
	_, err = w.Write([]byte("\n"))
	return err
}

// writeReadmeIndexAuditMarkdown renders findings as a markdown summary table.
func writeReadmeIndexAuditMarkdown(w io.Writer, findings []governance.ReadmeIndexFinding) error {
	if len(findings) == 0 {
		_, err := fmt.Fprint(w, "## README Index Audit\n\n**PASSED**: no orphan or ghost references found\n")
		return err
	}
	var sb strings.Builder
	fmt.Fprintf(&sb, "## README Index Audit\n\n**FAILED**: %d finding(s)\n\n", len(findings))
	sb.WriteString("| File | Severity | Kind | Message |\n")
	sb.WriteString("|------|----------|------|---------|\n")
	for _, f := range findings {
		fmt.Fprintf(&sb, "| %s | %s | %s | %s |\n", f.File, f.Severity, f.Kind, f.Message)
	}
	_, err := fmt.Fprint(w, sb.String())
	return err
}
