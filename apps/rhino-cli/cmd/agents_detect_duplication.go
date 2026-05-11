package cmd

import (
	"encoding/json"
	"fmt"
	"io"
	"strings"

	"github.com/spf13/cobra"
	"github.com/wahidyankf/ose-public/apps/rhino-cli/internal/agents"
	"github.com/wahidyankf/ose-public/apps/rhino-cli/internal/cliout"
)

// agentsDetectDuplicationSchema is the canonical JSON schema identifier emitted
// by the --output json branch of this command.
const agentsDetectDuplicationSchema = "rhino-cli/agents-detect-duplication/v1"

// agentsDetectDuplicationFn is the test-mockable entrypoint for the
// agent/skill duplication detector. Tests assign a stub to short-circuit
// the filesystem walk; production code delegates to the internal package.
var agentsDetectDuplicationFn = agents.DetectDuplication

var agentsDetectDuplicationCmd = &cobra.Command{
	Use:   "detect-duplication",
	Short: "Detect verbatim duplication across .claude/agents/*.md and .claude/skills/*/SKILL.md",
	Long: `Scan every .claude/agents/*.md and .claude/skills/*/SKILL.md file for 10-line
verbatim duplication clusters. The detector strips YAML frontmatter, normalizes
whitespace, then SHA-256s every overlapping 10-line window. Windows that appear
in two or more distinct files are reported as clusters.

Heading-only and whitespace-only windows are excluded — they would otherwise
generate noise without indicating real prose duplication.

Exit codes:
  0 — no clusters detected
  1 — at least one duplication cluster present`,
	Example: `  # Scan the repository
  rhino-cli agents detect-duplication

  # Emit JSON envelope
  rhino-cli agents detect-duplication -o json

  # Emit markdown
  rhino-cli agents detect-duplication -o markdown`,
	SilenceErrors: true,
	RunE:          runAgentsDetectDuplication,
}

func init() {
	agentsCmd.AddCommand(agentsDetectDuplicationCmd)
}

// runAgentsDetectDuplication locates the repository root, invokes the detector,
// routes output through the cliout.Dispatcher, and returns a non-nil error
// only when at least one duplication cluster is present.
func runAgentsDetectDuplication(cmd *cobra.Command, _ []string) error {
	repoRoot, err := findGitRoot()
	if err != nil {
		return fmt.Errorf("failed to find git repository root: %w", err)
	}

	findings, err := agentsDetectDuplicationFn(repoRoot)
	if err != nil {
		return fmt.Errorf("agents detect-duplication failed: %w", err)
	}

	dispatcher := cliout.NewDispatcher(
		writeAgentsDetectDuplicationText,
		writeAgentsDetectDuplicationJSON,
		writeAgentsDetectDuplicationMarkdown,
	)
	if err := dispatcher.Write(cmd.OutOrStdout(), outputFormat, findings); err != nil {
		return fmt.Errorf("write output: %w", err)
	}

	if len(findings) > 0 {
		return fmt.Errorf("%d duplication cluster(s) detected", len(findings))
	}
	return nil
}

// writeAgentsDetectDuplicationText writes a plain-text rendering of the
// duplication findings to w.
func writeAgentsDetectDuplicationText(w io.Writer, findings []agents.DuplicationFinding) error {
	if len(findings) == 0 {
		_, err := fmt.Fprintln(w, "AGENTS DUPLICATION VALIDATION PASSED: 0 clusters")
		return err
	}
	var sb strings.Builder
	fmt.Fprintf(&sb, "AGENTS DUPLICATION VALIDATION FAILED: %d cluster(s)\n", len(findings))
	for _, f := range findings {
		fmt.Fprintf(&sb, "  [%s] %s (window=%d)\n", f.Severity, f.Message, f.WindowSize)
		for i, p := range f.Files {
			fmt.Fprintf(&sb, "    - %s:%d\n", p, f.StartLines[i])
		}
	}
	_, err := w.Write([]byte(sb.String()))
	return err
}

// agentsDetectDuplicationJSONFinding is the wire-format projection of a
// DuplicationFinding emitted under the JSON envelope.
type agentsDetectDuplicationJSONFinding struct {
	Files      []string `json:"files"`
	StartLines []int    `json:"start_lines"`
	WindowSize int      `json:"window_size"`
	Severity   string   `json:"severity"`
	Message    string   `json:"message"`
}

// writeAgentsDetectDuplicationJSON writes a cliout.Envelope-wrapped JSON
// document to w. The envelope enforces canonical key ordering of schema,
// status, result.
func writeAgentsDetectDuplicationJSON(w io.Writer, findings []agents.DuplicationFinding) error {
	out := make([]agentsDetectDuplicationJSONFinding, 0, len(findings))
	for _, f := range findings {
		out = append(out, agentsDetectDuplicationJSONFinding{
			Files:      f.Files,
			StartLines: f.StartLines,
			WindowSize: f.WindowSize,
			Severity:   f.Severity,
			Message:    f.Message,
		})
	}
	status := "passed"
	if len(findings) > 0 {
		status = "failed"
	}
	env := cliout.Envelope[[]agentsDetectDuplicationJSONFinding]{
		Schema: agentsDetectDuplicationSchema,
		Status: status,
		Result: out,
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

// writeAgentsDetectDuplicationMarkdown writes a markdown summary to w.
func writeAgentsDetectDuplicationMarkdown(w io.Writer, findings []agents.DuplicationFinding) error {
	var sb strings.Builder
	sb.WriteString("## Agents Duplication Detection\n\n")
	if len(findings) == 0 {
		sb.WriteString("**PASSED**: 0 duplication clusters detected\n")
		_, err := w.Write([]byte(sb.String()))
		return err
	}
	fmt.Fprintf(&sb, "**FAILED**: %d duplication cluster(s) detected\n\n", len(findings))
	sb.WriteString("| Severity | Window | Files | Start Lines | Message |\n")
	sb.WriteString("|----------|--------|-------|-------------|---------|\n")
	for _, f := range findings {
		// Build per-row file/line summaries.
		var files, starts []string
		for i, p := range f.Files {
			files = append(files, p)
			starts = append(starts, fmt.Sprintf("%d", f.StartLines[i]))
		}
		fmt.Fprintf(&sb, "| %s | %d | %s | %s | %s |\n",
			f.Severity, f.WindowSize,
			strings.Join(files, "<br>"),
			strings.Join(starts, "<br>"),
			f.Message,
		)
	}
	_, err := w.Write([]byte(sb.String()))
	return err
}
