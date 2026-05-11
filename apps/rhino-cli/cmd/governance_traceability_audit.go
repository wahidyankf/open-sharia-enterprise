package cmd

import (
	"encoding/json"
	"fmt"
	"io"
	"strings"

	"github.com/spf13/cobra"
	"github.com/wahidyankf/ose-public/apps/rhino-cli/internal/cliout"
	governance "github.com/wahidyankf/ose-public/apps/rhino-cli/internal/repo-governance"
)

// traceabilityAuditSchema is the canonical JSON schema identifier for the
// repo-governance traceability-audit command.
const traceabilityAuditSchema = "rhino-cli/traceability-audit/v1"

// traceabilityAuditFn is the test-mockable entrypoint for the traceability
// audit. Production code wires it to governance.AuditTraceability; unit tests
// substitute deterministic stubs.
var traceabilityAuditFn = governance.AuditTraceability

var governanceTraceabilityAuditCmd = &cobra.Command{
	Use:   "traceability-audit",
	Short: "Audit governance documents for required traceability sections",
	Long: `Walk the four governance subtrees under repo-governance/ and verify that every
non-README markdown file carries the traceability sections required for its layer:

  - repo-governance/principles/**/*.md   must contain "## Vision Supported"
  - repo-governance/conventions/**/*.md  must contain "## Principles Implemented/Respected"
  - repo-governance/development/**/*.md  must contain BOTH
                                         "## Principles Implemented/Respected" and
                                         "## Conventions Implemented/Respected"
  - repo-governance/workflows/**/*.md    must reference at least one
                                         .claude/agents/<name>.md agent definition

README.md files are exempt — they are index pages, not subject to the
per-layer traceability schema.

Exits 0 when all checks pass and 1 when one or more findings are reported.`,
	Example: `  # Run the traceability audit against the current repository
  rhino-cli repo-governance traceability-audit

  # Emit JSON envelope
  rhino-cli repo-governance traceability-audit -o json

  # Emit markdown summary
  rhino-cli repo-governance traceability-audit -o markdown`,
	SilenceErrors: true,
	SilenceUsage:  true,
	RunE:          runGovernanceTraceabilityAudit,
}

func init() {
	repoGovernanceCmd.AddCommand(governanceTraceabilityAuditCmd)
}

// runGovernanceTraceabilityAudit locates the repository root, runs the audit,
// dispatches output through cliout.Dispatcher, and returns a non-nil error
// when any finding is reported (driving exit code 1).
func runGovernanceTraceabilityAudit(cmd *cobra.Command, _ []string) error {
	repoRoot, err := findGitRoot()
	if err != nil {
		return fmt.Errorf("failed to find git repository root: %w", err)
	}

	findings, err := traceabilityAuditFn(repoRoot)
	if err != nil {
		return fmt.Errorf("traceability audit failed: %w", err)
	}

	dispatcher := cliout.NewDispatcher(
		writeTraceabilityAuditText,
		writeTraceabilityAuditJSON,
		writeTraceabilityAuditMarkdown,
	)
	if err := dispatcher.Write(cmd.OutOrStdout(), outputFormat, findings); err != nil {
		return fmt.Errorf("write output: %w", err)
	}

	if len(findings) > 0 {
		return fmt.Errorf("%d traceability finding(s) reported", len(findings))
	}
	return nil
}

// writeTraceabilityAuditText writes the plain-text representation of the
// findings to w.
func writeTraceabilityAuditText(w io.Writer, findings []governance.TraceabilityFinding) error {
	if len(findings) == 0 {
		_, err := fmt.Fprintln(w, "TRACEABILITY AUDIT PASSED: zero findings")
		return err
	}
	var sb strings.Builder
	fmt.Fprintf(&sb, "TRACEABILITY AUDIT FAILED: %d finding(s) reported\n", len(findings))
	for _, f := range findings {
		fmt.Fprintf(&sb, "  %s:%d  %s  %s\n", f.Path, f.Line, f.Kind, f.Message)
	}
	_, err := fmt.Fprint(w, sb.String())
	return err
}

// traceabilityAuditJSONFinding is the per-finding JSON representation used
// inside the envelope's Result array. Field tags fix the wire format.
type traceabilityAuditJSONFinding struct {
	Path    string `json:"path"`
	Line    int    `json:"line"`
	Kind    string `json:"kind"`
	Message string `json:"message"`
}

// traceabilityAuditJSONResult is the inner payload for the JSON envelope.
type traceabilityAuditJSONResult struct {
	Status   string                         `json:"status"`
	Count    int                            `json:"count"`
	Findings []traceabilityAuditJSONFinding `json:"findings"`
}

// writeTraceabilityAuditJSON writes a cliout.Envelope-wrapped JSON document
// to w. The envelope enforces canonical key ordering of schema, status, result.
func writeTraceabilityAuditJSON(w io.Writer, findings []governance.TraceabilityFinding) error {
	status := "passed"
	if len(findings) > 0 {
		status = "failed"
	}
	jf := make([]traceabilityAuditJSONFinding, 0, len(findings))
	for _, f := range findings {
		jf = append(jf, traceabilityAuditJSONFinding{
			Path:    f.Path,
			Line:    f.Line,
			Kind:    f.Kind,
			Message: f.Message,
		})
	}
	env := cliout.Envelope[traceabilityAuditJSONResult]{
		Schema: traceabilityAuditSchema,
		Status: status,
		Result: traceabilityAuditJSONResult{
			Status:   status,
			Count:    len(findings),
			Findings: jf,
		},
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

// writeTraceabilityAuditMarkdown writes a markdown summary table to w.
func writeTraceabilityAuditMarkdown(w io.Writer, findings []governance.TraceabilityFinding) error {
	if len(findings) == 0 {
		_, err := fmt.Fprint(w, "## Traceability Audit\n\n**PASSED**: zero findings\n")
		return err
	}
	var sb strings.Builder
	fmt.Fprintf(&sb, "## Traceability Audit\n\n**FAILED**: %d finding(s) reported\n\n", len(findings))
	sb.WriteString("| File | Line | Kind | Message |\n")
	sb.WriteString("|------|------|------|---------|\n")
	for _, f := range findings {
		fmt.Fprintf(&sb, "| %s | %d | %s | %s |\n", f.Path, f.Line, f.Kind, f.Message)
	}
	_, err := fmt.Fprint(w, sb.String())
	return err
}
