package cmd

import (
	"encoding/json"
	"fmt"
	"io"
	"path/filepath"

	"github.com/spf13/cobra"
	"github.com/wahidyankf/ose-public/apps/rhino-cli/internal/cliout"
	governance "github.com/wahidyankf/ose-public/apps/rhino-cli/internal/repo-governance"
)

// agentsMdSizeSchema is the canonical JSON schema identifier for this command.
const agentsMdSizeSchema = "rhino-cli/agents-md-size/v1"

// agentsMdSizeFn is the test-mockable entrypoint for the AGENTS.md size check.
var agentsMdSizeFn = governance.CheckAgentsMdSize

var agentsMdSizeCmd = &cobra.Command{
	Use:   "agents-md-size",
	Short: "Audit AGENTS.md size against the 30/35/40 KB thresholds",
	Long: `Measure the byte size of the repository root AGENTS.md and classify it against the
documented thresholds:

  - "ok"   when size <= 30000 bytes (within target)
  - "warn" when 30000 < size <= 35000 bytes (over target)
  - "warn" when 35000 < size <= 40000 bytes (over warning threshold)
  - "fail" when size > 40000 bytes (over hard limit)

The command exits 0 for "ok" and "warn" classifications and exits 1 for "fail".`,
	Example: `  # Audit the repository root AGENTS.md
  rhino-cli repo-governance agents-md-size

  # Emit JSON envelope
  rhino-cli repo-governance agents-md-size -o json

  # Emit markdown
  rhino-cli repo-governance agents-md-size -o markdown`,
	SilenceErrors: true,
	RunE:          runAgentsMdSize,
}

func init() {
	repoGovernanceCmd.AddCommand(agentsMdSizeCmd)
}

// runAgentsMdSize locates the repository root, audits AGENTS.md, dispatches the
// output through the cliout.Dispatcher, and returns a non-nil error only when
// the classification is "fail".
func runAgentsMdSize(cmd *cobra.Command, _ []string) error {
	repoRoot, err := findGitRoot()
	if err != nil {
		return fmt.Errorf("failed to find git repository root: %w", err)
	}

	path := filepath.Join(repoRoot, "AGENTS.md")
	finding, err := agentsMdSizeFn(path)
	if err != nil {
		return fmt.Errorf("agents-md-size audit failed: %w", err)
	}

	dispatcher := cliout.NewDispatcher(
		writeAgentsMdSizeText,
		writeAgentsMdSizeJSON,
		writeAgentsMdSizeMarkdown,
	)
	if err := dispatcher.Write(cmd.OutOrStdout(), outputFormat, finding); err != nil {
		return fmt.Errorf("write output: %w", err)
	}

	if finding.Severity == "fail" {
		return fmt.Errorf("AGENTS.md exceeds hard limit (%d bytes)", finding.Size)
	}
	return nil
}

// writeAgentsMdSizeText writes the plain-text representation of an
// AgentsMdSizeFinding to w.
func writeAgentsMdSizeText(w io.Writer, f governance.AgentsMdSizeFinding) error {
	_, err := fmt.Fprintf(w, "AGENTS.MD SIZE: %s — %s\n", statusLabelForSeverity(f.Severity), f.Message)
	return err
}

// agentsMdSizeJSONResult is the inner payload for the JSON envelope. It carries
// the per-finding fields with explicit JSON tags so the wire format is stable.
type agentsMdSizeJSONResult struct {
	File     string `json:"file"`
	Size     int64  `json:"size"`
	Severity string `json:"severity"`
	Message  string `json:"message"`
}

// writeAgentsMdSizeJSON writes a cliout.Envelope-wrapped JSON document to w.
// The envelope enforces canonical key ordering of schema, status, result.
func writeAgentsMdSizeJSON(w io.Writer, f governance.AgentsMdSizeFinding) error {
	env := cliout.Envelope[agentsMdSizeJSONResult]{
		Schema: agentsMdSizeSchema,
		Status: f.Severity,
		Result: agentsMdSizeJSONResult{
			File:     f.File,
			Size:     f.Size,
			Severity: f.Severity,
			Message:  f.Message,
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

// writeAgentsMdSizeMarkdown writes a markdown summary table to w.
func writeAgentsMdSizeMarkdown(w io.Writer, f governance.AgentsMdSizeFinding) error {
	_, err := fmt.Fprintf(w, `## AGENTS.md Size Audit

**Status**: %s

| File | Size (bytes) | Severity | Message |
|------|--------------|----------|---------|
| %s | %d | %s | %s |
`, statusLabelForSeverity(f.Severity), f.File, f.Size, f.Severity, f.Message)
	return err
}

// statusLabelForSeverity maps the internal severity code to a human-readable
// label used in text and markdown output.
func statusLabelForSeverity(severity string) string {
	switch severity {
	case "ok":
		return "PASS"
	case "warn":
		return "WARN"
	case "fail":
		return "FAIL"
	default:
		return severity
	}
}
