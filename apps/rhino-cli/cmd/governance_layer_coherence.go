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

// layerCoherenceSchema is the canonical JSON schema identifier for the
// repo-governance layer-coherence command.
const layerCoherenceSchema = "rhino-cli/layer-coherence/v1"

// layerCoherenceFn is the test-mockable entrypoint for the layer-coherence
// audit. Production code wires it to governance.AuditLayerCoherence; unit tests
// substitute deterministic stubs.
var layerCoherenceFn = governance.AuditLayerCoherence

var governanceLayerCoherenceCmd = &cobra.Command{
	Use:   "layer-coherence",
	Short: "Audit governance docs for layer numbering and naming coherence",
	Long: `Read the two governance index documents and verify that they agree on the
layer numbering and names:

  - repo-governance/repository-governance-architecture.md
  - repo-governance/README.md

Extract every layer declaration of the form **Layer N: Name** (bold span,
canonical in the README index) or ## Layer N: Name (...) (H2 heading,
canonical in the architecture document) and report:

  - intra-file conflicts (same layer number declared with two different names
    inside one file)
  - cross-file number mismatches (a layer number declared in one file is
    absent from the other)
  - cross-file name mismatches (both files declare the same layer number but
    disagree on its name)
  - numbering gaps (the union of declared numbers is not contiguous from 0
    to max)
  - missing docs (one of the required input files cannot be read)

Exits 0 when no findings are reported and 1 when one or more findings exist.`,
	Example: `  # Run the layer-coherence audit against the current repository
  rhino-cli repo-governance layer-coherence

  # Emit JSON envelope
  rhino-cli repo-governance layer-coherence -o json

  # Emit markdown summary
  rhino-cli repo-governance layer-coherence -o markdown`,
	SilenceErrors: true,
	SilenceUsage:  true,
	RunE:          runGovernanceLayerCoherence,
}

func init() {
	repoGovernanceCmd.AddCommand(governanceLayerCoherenceCmd)
}

// runGovernanceLayerCoherence locates the repository root, runs the audit,
// dispatches output through cliout.Dispatcher, and returns a non-nil error
// when any finding is reported (driving exit code 1).
func runGovernanceLayerCoherence(cmd *cobra.Command, _ []string) error {
	repoRoot, err := findGitRoot()
	if err != nil {
		return fmt.Errorf("failed to find git repository root: %w", err)
	}

	findings, err := layerCoherenceFn(repoRoot)
	if err != nil {
		return fmt.Errorf("layer-coherence audit failed: %w", err)
	}

	dispatcher := cliout.NewDispatcher(
		writeLayerCoherenceText,
		writeLayerCoherenceJSON,
		writeLayerCoherenceMarkdown,
	)
	if err := dispatcher.Write(cmd.OutOrStdout(), outputFormat, findings); err != nil {
		return fmt.Errorf("write output: %w", err)
	}

	if len(findings) > 0 {
		return fmt.Errorf("%d layer-coherence finding(s) reported", len(findings))
	}
	return nil
}

// writeLayerCoherenceText writes the plain-text representation of the findings
// to w.
func writeLayerCoherenceText(w io.Writer, findings []governance.LayerCoherenceFinding) error {
	if len(findings) == 0 {
		_, err := fmt.Fprintln(w, "LAYER COHERENCE AUDIT PASSED: zero findings")
		return err
	}
	var sb strings.Builder
	fmt.Fprintf(&sb, "LAYER COHERENCE AUDIT FAILED: %d finding(s) reported\n", len(findings))
	for _, f := range findings {
		fmt.Fprintf(&sb, "  %s  [%s]  %s — %s\n", f.File, f.Severity, f.Kind, f.Message)
	}
	_, err := fmt.Fprint(w, sb.String())
	return err
}

// layerCoherenceJSONFinding is the per-finding JSON representation used inside
// the envelope's Result array. Field tags fix the wire format.
type layerCoherenceJSONFinding struct {
	File     string `json:"file"`
	Severity string `json:"severity"`
	Kind     string `json:"kind"`
	Message  string `json:"message"`
}

// layerCoherenceJSONResult is the inner payload for the JSON envelope.
type layerCoherenceJSONResult struct {
	Status   string                      `json:"status"`
	Count    int                         `json:"count"`
	Findings []layerCoherenceJSONFinding `json:"findings"`
}

// writeLayerCoherenceJSON writes a cliout.Envelope-wrapped JSON document to w.
// The envelope enforces canonical key ordering of schema, status, result.
func writeLayerCoherenceJSON(w io.Writer, findings []governance.LayerCoherenceFinding) error {
	status := "passed"
	if len(findings) > 0 {
		status = "failed"
	}
	jf := make([]layerCoherenceJSONFinding, 0, len(findings))
	for _, f := range findings {
		jf = append(jf, layerCoherenceJSONFinding{
			File:     f.File,
			Severity: f.Severity,
			Kind:     f.Kind,
			Message:  f.Message,
		})
	}
	env := cliout.Envelope[layerCoherenceJSONResult]{
		Schema: layerCoherenceSchema,
		Status: status,
		Result: layerCoherenceJSONResult{
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

// writeLayerCoherenceMarkdown writes a markdown summary table to w.
func writeLayerCoherenceMarkdown(w io.Writer, findings []governance.LayerCoherenceFinding) error {
	if len(findings) == 0 {
		_, err := fmt.Fprint(w, "## Layer Coherence Audit\n\n**PASSED**: zero findings\n")
		return err
	}
	var sb strings.Builder
	fmt.Fprintf(&sb, "## Layer Coherence Audit\n\n**FAILED**: %d finding(s) reported\n\n", len(findings))
	sb.WriteString("| File | Severity | Kind | Message |\n")
	sb.WriteString("|------|----------|------|---------|\n")
	for _, f := range findings {
		fmt.Fprintf(&sb, "| %s | %s | %s | %s |\n", f.File, f.Severity, f.Kind, f.Message)
	}
	_, err := fmt.Fprint(w, sb.String())
	return err
}
