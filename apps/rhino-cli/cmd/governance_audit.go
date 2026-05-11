package cmd

import (
	"encoding/json"
	"fmt"
	"io"
	"os"
	"strings"
	"time"

	"github.com/spf13/cobra"
	"github.com/wahidyankf/ose-public/apps/rhino-cli/internal/cliout"
	governance "github.com/wahidyankf/ose-public/apps/rhino-cli/internal/repo-governance"
)

// governanceAuditSkip holds the --skip <category> repeatable flag values.
var governanceAuditSkip []string

// governanceAuditIncludeOnly holds the --include-category <name> repeatable flag values.
var governanceAuditIncludeOnly []string

// runAuditFn is the test-mockable entrypoint for the orchestrator. Tests
// assign a stub to short-circuit the per-category filesystem walks;
// production code delegates to the internal package.
var runAuditFn = governance.RunAudit

var governanceAuditCmd = &cobra.Command{
	Use:   "audit",
	Short: "Run all 11 deterministic governance audits and emit a single JSON envelope",
	Long: `Invoke every deterministic governance audit in fixed order, aggregate the
findings into a single canonical JSON envelope, and apply the
generated-reports/.known-false-positives.md skip list. The envelope schema is
` + "`rhino-cli/repo-governance-audit/v1`" + ` and the output is byte-deterministic
when run twice against the same repository contents.

The orchestrator runs these categories in order:

  1.  agents-md-size
  2.  frontmatter-audit
  3.  traceability-audit
  4.  license-audit
  5.  readme-index-audit
  6.  emoji-audit
  7.  layer-coherence
  8.  docs-validate-naming
  9.  docs-validate-frontmatter
  10. docs-validate-heading-hierarchy
  11. agents-detect-duplication

Exit codes:
  0 — total_findings == 0
  1 — at least one finding present
  2 — invocation error (cannot find git root, I/O failure, etc.)`,
	Example: `  # Run the full audit
  rhino-cli repo-governance audit

  # Emit JSON envelope (the AI-checker preflight contract)
  rhino-cli repo-governance audit -o json

  # Run only one category
  rhino-cli repo-governance audit --include-category agents-md-size

  # Skip two categories
  rhino-cli repo-governance audit --skip agents-detect-duplication --skip emoji-audit`,
	SilenceErrors: true,
	SilenceUsage:  true,
	RunE:          runGovernanceAudit,
}

func init() {
	governanceAuditCmd.Flags().StringArrayVar(
		&governanceAuditSkip,
		"skip",
		nil,
		"category to skip (repeatable); see --help for the canonical list",
	)
	governanceAuditCmd.Flags().StringArrayVar(
		&governanceAuditIncludeOnly,
		"include-category",
		nil,
		"category to include (repeatable); when set, only listed categories run",
	)
	repoGovernanceCmd.AddCommand(governanceAuditCmd)
}

// runGovernanceAudit locates the repository root, runs the orchestrator,
// dispatches the output through cliout.Dispatcher, and returns a non-nil
// error when total_findings is greater than zero (driving exit code 1).
// Invocation errors (e.g., missing git root) drive exit code 2 via the
// rootCmd error path.
func runGovernanceAudit(cmd *cobra.Command, _ []string) error {
	repoRoot, err := findGitRoot()
	if err != nil {
		return fmt.Errorf("failed to find git repository root: %w", err)
	}

	opts := governance.AuditOptions{
		RepoRoot:    repoRoot,
		Skip:        governanceAuditSkip,
		IncludeOnly: governanceAuditIncludeOnly,
	}
	// RHINO_AUDIT_NOW (RFC3339) pins ran_at for byte-deterministic regression
	// testing. Empty/unset → default to time.Now in the orchestrator.
	if v := os.Getenv("RHINO_AUDIT_NOW"); v != "" {
		if parsed, perr := time.Parse(time.RFC3339, v); perr == nil {
			opts.Now = func() time.Time { return parsed }
		}
	}
	envelope, err := runAuditFn(opts)
	if err != nil {
		return fmt.Errorf("repo-governance audit failed: %w", err)
	}

	dispatcher := cliout.NewDispatcher(
		writeGovernanceAuditText,
		writeGovernanceAuditJSON,
		writeGovernanceAuditMarkdown,
	)
	if err := dispatcher.Write(cmd.OutOrStdout(), outputFormat, envelope); err != nil {
		// Reset flag state before returning so subsequent invocations in
		// tests start with a clean slate.
		governanceAuditSkip = nil
		governanceAuditIncludeOnly = nil
		return fmt.Errorf("write output: %w", err)
	}

	governanceAuditSkip = nil
	governanceAuditIncludeOnly = nil

	if envelope.Result.TotalFindings > 0 {
		return fmt.Errorf("%d governance finding(s) reported across %d categor(ies)",
			envelope.Result.TotalFindings, len(envelope.Result.Categories))
	}
	return nil
}

// writeGovernanceAuditText writes a plain-text summary of the envelope to w.
func writeGovernanceAuditText(w io.Writer, env governance.AuditEnvelope) error {
	var sb strings.Builder
	if env.Result.TotalFindings == 0 {
		fmt.Fprintf(&sb, "GOVERNANCE AUDIT PASSED: 0 findings across %d categories (git_sha=%s, ran_at=%s)\n",
			len(env.Result.Categories), env.Result.GitSHA, env.Result.RanAt)
	} else {
		fmt.Fprintf(&sb, "GOVERNANCE AUDIT FAILED: %d finding(s) across %d categories (git_sha=%s, ran_at=%s)\n",
			env.Result.TotalFindings, len(env.Result.Categories), env.Result.GitSHA, env.Result.RanAt)
	}
	for _, c := range env.Result.Categories {
		state := "PASS"
		if !c.Passed {
			state = "FAIL"
		}
		fmt.Fprintf(&sb, "  [%s] %-32s %d finding(s)\n", state, c.Name, len(c.Findings))
		for _, f := range c.Findings {
			line := f.File
			if f.Line > 0 {
				line = fmt.Sprintf("%s:%d", f.File, f.Line)
			}
			fmt.Fprintf(&sb, "         %s  %s\n", line, f.Message)
		}
	}
	if len(env.Result.SkippedFalsePositives) > 0 {
		fmt.Fprintf(&sb, "  %d skipped false-positive(s)\n", len(env.Result.SkippedFalsePositives))
	}
	_, err := w.Write([]byte(sb.String()))
	return err
}

// writeGovernanceAuditJSON writes the canonical JSON envelope to w. The
// envelope's MarshalJSON enforces key ordering; we use MarshalIndent for
// human-readable two-space indentation.
func writeGovernanceAuditJSON(w io.Writer, env governance.AuditEnvelope) error {
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

// writeGovernanceAuditMarkdown writes a markdown summary of the envelope.
func writeGovernanceAuditMarkdown(w io.Writer, env governance.AuditEnvelope) error {
	var sb strings.Builder
	sb.WriteString("## Governance Audit\n\n")
	if env.Result.TotalFindings == 0 {
		fmt.Fprintf(&sb, "**PASSED**: 0 findings across %d categories (git_sha=`%s`, ran_at=`%s`)\n\n",
			len(env.Result.Categories), env.Result.GitSHA, env.Result.RanAt)
	} else {
		fmt.Fprintf(&sb, "**FAILED**: %d finding(s) across %d categories (git_sha=`%s`, ran_at=`%s`)\n\n",
			env.Result.TotalFindings, len(env.Result.Categories), env.Result.GitSHA, env.Result.RanAt)
	}
	sb.WriteString("| Category | Status | Findings |\n")
	sb.WriteString("|----------|--------|---------:|\n")
	for _, c := range env.Result.Categories {
		state := "PASS"
		if !c.Passed {
			state = "FAIL"
		}
		fmt.Fprintf(&sb, "| %s | %s | %d |\n", c.Name, state, len(c.Findings))
	}
	if len(env.Result.SkippedFalsePositives) > 0 {
		fmt.Fprintf(&sb, "\n_%d skipped false-positive(s)._\n", len(env.Result.SkippedFalsePositives))
	}
	_, err := w.Write([]byte(sb.String()))
	return err
}
