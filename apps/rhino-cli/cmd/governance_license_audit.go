package cmd

import (
	"bytes"
	"encoding/json"
	"fmt"
	"strings"

	"github.com/spf13/cobra"
	"github.com/wahidyankf/ose-public/apps/rhino-cli/internal/cliout"
	governance "github.com/wahidyankf/ose-public/apps/rhino-cli/internal/repo-governance"
)

// licenseAuditFn is the test-mockable entrypoint for the license audit.
var licenseAuditFn = governance.AuditLicense

// licenseAuditEnvelope is the canonical JSON payload shape for the
// license-audit command. The envelope uses the shared cliout.Envelope[T]
// generic wrapper so future orchestrator integration can re-use the
// canonical schema/status/result key ordering.
type licenseAuditEnvelope struct {
	TotalFindings int                         `json:"total_findings"`
	Findings      []governance.LicenseFinding `json:"findings"`
}

var governanceLicenseAuditCmd = &cobra.Command{
	Use:   "license-audit",
	Short: "Verify per-directory LICENSE files match the licensing convention",
	Long: `Verify that every product app, library, and specs directory carries a
LICENSE file matching the per-directory licensing convention.

The audit checks three properties:
  - Every required directory has a LICENSE file (apps/<name>/, libs/<name>/, specs/)
  - The LICENSE file's first non-empty line resolves to a known SPDX identifier
  - LICENSING-NOTICE.md rows agree with the on-disk LICENSE identifier

Exemptions follow the licensing convention:
  - apps/rhino-cli (internal CLI tool, inherits root LICENSE)
  - apps/*-e2e (Playwright e2e suites, inherit root LICENSE)
  - LICENSING-NOTICE.md rows for paths outside apps/, libs/, specs/

Exits with code 1 if any findings are reported, 0 if clean.`,
	Example: `  # Audit the current repository
  rhino-cli repo-governance license-audit

  # Output as JSON for tooling consumption
  rhino-cli repo-governance license-audit -o json

  # Markdown output for PR comments
  rhino-cli repo-governance license-audit -o markdown`,
	SilenceErrors: true,
	RunE:          runGovernanceLicenseAudit,
}

func init() {
	repoGovernanceCmd.AddCommand(governanceLicenseAuditCmd)
}

func runGovernanceLicenseAudit(cmd *cobra.Command, _ []string) error {
	repoRoot, err := findGitRoot()
	if err != nil {
		return fmt.Errorf("failed to find git repository root: %w", err)
	}

	findings, err := licenseAuditFn(repoRoot)
	if err != nil {
		return fmt.Errorf("license audit failed: %w", err)
	}

	if err := writeFormattedV2(cmd, verbose, quiet, outputFuncs{
		text:     func(_, q bool) string { return formatLicenseAuditText(findings, q) },
		json:     func() (string, error) { return formatLicenseAuditJSON(findings) },
		markdown: func() string { return formatLicenseAuditMarkdown(findings) },
	}); err != nil {
		return err
	}

	if len(findings) > 0 {
		return fmt.Errorf("%d license finding(s) found", len(findings))
	}
	return nil
}

// formatLicenseAuditText formats the findings as human-readable text.
func formatLicenseAuditText(findings []governance.LicenseFinding, quiet bool) string {
	if len(findings) == 0 {
		if quiet {
			return ""
		}
		return "LICENSE AUDIT PASSED: no findings\n"
	}
	var sb strings.Builder
	fmt.Fprintf(&sb, "LICENSE AUDIT FAILED: %d finding(s)\n", len(findings))
	for _, f := range findings {
		fmt.Fprintf(&sb, "  [%s] %s — %s\n", f.Kind, f.Path, f.Message)
	}
	return sb.String()
}

// formatLicenseAuditJSON formats the findings as canonical JSON. The envelope
// uses cliout.Envelope[T] so the schema/status/result key order is stable
// across runs.
func formatLicenseAuditJSON(findings []governance.LicenseFinding) (string, error) {
	if findings == nil {
		findings = []governance.LicenseFinding{}
	}
	env := cliout.Envelope[licenseAuditEnvelope]{
		Schema: "rhino-cli/license-audit/v1",
		Status: licenseAuditStatus(findings),
		Result: licenseAuditEnvelope{
			TotalFindings: len(findings),
			Findings:      findings,
		},
	}

	var buf bytes.Buffer
	enc := json.NewEncoder(&buf)
	enc.SetIndent("", "  ")
	if err := enc.Encode(env); err != nil {
		return "", err
	}
	return buf.String(), nil
}

// licenseAuditStatus returns "passed" when there are zero findings, "failed"
// otherwise.
func licenseAuditStatus(findings []governance.LicenseFinding) string {
	if len(findings) == 0 {
		return "passed"
	}
	return "failed"
}

// formatLicenseAuditMarkdown formats the findings as a markdown table for
// inclusion in PR comments and audit reports.
func formatLicenseAuditMarkdown(findings []governance.LicenseFinding) string {
	var sb strings.Builder
	sb.WriteString("## License Audit\n\n")
	if len(findings) == 0 {
		sb.WriteString("**PASSED**: no findings\n")
		return sb.String()
	}
	fmt.Fprintf(&sb, "**FAILED**: %d finding(s)\n\n", len(findings))
	sb.WriteString("| Kind | Path | Message |\n")
	sb.WriteString("| --- | --- | --- |\n")
	for _, f := range findings {
		fmt.Fprintf(&sb, "| %s | `%s` | %s |\n", f.Kind, f.Path, f.Message)
	}
	return sb.String()
}
