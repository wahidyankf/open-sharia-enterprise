package cmd

import (
	"fmt"
	"os"
	"strings"

	"github.com/spf13/cobra"
	"github.com/wahidyankf/ose-public/apps/rhino-cli/internal/bcregistry"
)

var bcSeverity string

var dddBcCmd = &cobra.Command{
	Use:   "bc <app>",
	Short: "Validate bounded-context structural parity against the registry",
	Long: `Verify that the filesystem matches the registry at
specs/apps/<app>/bounded-contexts.yaml.

Checks for each registered context:
  - Code directory exists with exactly the declared layer subfolders.
  - Glossary file exists.
  - Gherkin directory exists and contains at least one .feature file.

Detects orphans:
  - Code, glossary, or gherkin paths on disk not listed in the registry.

Verifies relationship symmetry for asymmetric relationship kinds
(customer-supplier, conformist).

Severity is resolved in priority order:
  1. --severity flag
  2. OSE_RHINO_DDD_SEVERITY environment variable
  3. Default: error`,
	Example: `  # Validate organiclever bounded-context structure
  rhino-cli ddd bc organiclever

  # Downgrade findings to warnings (escape hatch only)
  rhino-cli ddd bc organiclever --severity=warn`,
	Args:          cobra.ExactArgs(1),
	SilenceErrors: true,
	RunE:          runDddBc,
}

func init() {
	dddBcCmd.Flags().StringVar(&bcSeverity, "severity", "", "override finding severity: warn|error")
	dddCmd.AddCommand(dddBcCmd)
}

func runDddBc(cmd *cobra.Command, args []string) error {
	repoRoot, err := findGitRoot()
	if err != nil {
		return fmt.Errorf("failed to find git repository root: %w", err)
	}

	app := args[0]
	sev := resolveBcSeverity(bcSeverity)

	findings, err := bcValidateAllFn(bcregistry.ValidateOptions{
		RepoRoot: repoRoot,
		App:      app,
		Severity: sev,
	})
	if err != nil {
		return err
	}

	for _, f := range findings {
		_, _ = fmt.Fprintf(cmd.OutOrStdout(), "%s: %s: %s\n", f.File, f.Severity, f.Message)
	}

	errCount := 0
	for _, f := range findings {
		if f.Severity == "error" {
			errCount++
		}
	}
	if errCount > 0 {
		return fmt.Errorf("%d error finding(s) found by ddd bc", errCount)
	}
	return nil
}

func resolveBcSeverity(flagVal string) string {
	if flagVal != "" {
		return normaliseSeverity(flagVal)
	}
	if env := os.Getenv("OSE_RHINO_DDD_SEVERITY"); env != "" {
		sev := normaliseSeverity(env)
		if sev == "warn" {
			fmt.Fprintln(os.Stderr, `WARN: severity downgraded to "warn" via OSE_RHINO_DDD_SEVERITY env var`)
		}
		return sev
	}
	return "error"
}

func normaliseSeverity(s string) string {
	switch strings.ToLower(strings.TrimSpace(s)) {
	case "warn", "warning":
		return "warn"
	default:
		return "error"
	}
}
