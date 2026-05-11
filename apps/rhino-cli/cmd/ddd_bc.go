package cmd

import (
	"os"

	"github.com/spf13/cobra"
	"github.com/wahidyankf/ose-public/apps/rhino-cli/internal/bcregistry"
	"github.com/wahidyankf/ose-public/apps/rhino-cli/internal/severity"
)

// bcSeverity holds the --severity flag value for the ddd bc command.
var bcSeverity string

// dddBcCmd is the cobra.Command for "rhino-cli ddd bc".
var dddBcCmd *cobra.Command

func init() {
	spec := dddCommandSpec{
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
		ValidatorFn: func(repoRoot, app string) ([]dddFinding, error) {
			sev := severity.Resolve(bcSeverity, os.Getenv("OSE_RHINO_DDD_SEVERITY"), os.Stderr)
			raw, err := bcValidateAllFn(bcregistry.ValidateOptions{
				RepoRoot: repoRoot,
				App:      app,
				Severity: sev,
			})
			if err != nil {
				return nil, err
			}
			out := make([]dddFinding, len(raw))
			for i, f := range raw {
				out[i] = dddFinding{File: f.File, Message: f.Message, Severity: f.Severity}
			}
			return out, nil
		},
		FindingsLabel: "ddd bc",
	}
	dddBcCmd = newDddCommand(spec)
	dddBcCmd.Flags().StringVar(&bcSeverity, "severity", "", "override finding severity: warn|error")
	dddCmd.AddCommand(dddBcCmd)
}
