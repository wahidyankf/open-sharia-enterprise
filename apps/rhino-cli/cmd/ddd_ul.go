package cmd

import (
	"os"

	"github.com/spf13/cobra"
	"github.com/wahidyankf/ose-public/apps/rhino-cli/internal/glossary"
	"github.com/wahidyankf/ose-public/apps/rhino-cli/internal/severity"
)

// ulSeverity holds the --severity flag value for the ddd ul command.
var ulSeverity string

// dddUlCmd is the cobra.Command for "rhino-cli ddd ul".
var dddUlCmd *cobra.Command

func init() {
	spec := dddCommandSpec{
		Use:   "ul <app>",
		Short: "Validate ubiquitous-language glossary parity against the registry",
		Long: `Verify that every glossary file listed in specs/apps/<app>/bounded-contexts.yaml
is well-formed and internally consistent.

Checks for each registered glossary:
  - Frontmatter contains Bounded context, Maintainer, Last reviewed.
  - Terms table header matches the canonical column names.
  - Each code identifier in backticks exists in the BC's code path.
  - Each feature reference resolves to an existing .feature file.

Cross-context checks:
  - Same term defined in two glossaries without mutual Forbidden-synonyms entries.

Severity is resolved in priority order:
  1. --severity flag
  2. OSE_RHINO_DDD_SEVERITY environment variable
  3. Default: error`,
		Example: `  # Validate organiclever glossaries
  rhino-cli ddd ul organiclever

  # Downgrade findings to warnings (escape hatch only)
  rhino-cli ddd ul organiclever --severity=warn`,
		ValidatorFn: func(repoRoot, app string) ([]dddFinding, error) {
			sev := severity.Resolve(ulSeverity, os.Getenv("OSE_RHINO_DDD_SEVERITY"), os.Stderr)
			raw, err := ulValidateAllFn(glossary.ValidateOptions{
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
		FindingsLabel: "ddd ul",
	}
	dddUlCmd = newDddCommand(spec)
	dddUlCmd.Flags().StringVar(&ulSeverity, "severity", "", "override finding severity: warn|error")
	dddCmd.AddCommand(dddUlCmd)
}
