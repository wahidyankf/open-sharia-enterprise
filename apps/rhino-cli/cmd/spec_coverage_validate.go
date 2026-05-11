package cmd

import (
	"fmt"
	"path/filepath"

	"github.com/spf13/cobra"
	"github.com/wahidyankf/ose-public/apps/rhino-cli/internal/speccoverage"
)

var validateSpecCoverageCmd = &cobra.Command{
	Use:   "validate <specs-dir> [<specs-dir>...] <app-dir>",
	Short: "Validate that all BDD spec files have matching test implementations",
	Long: `Walk one or more <specs-dir> trees for .feature files and check each has at
least one matching test file anywhere under the final positional <app-dir>.

When multiple <specs-dir> arguments are provided, every gherkin tree is walked
in a single pass and ALL extracted impl matchers under <app-dir> are validated
against the union of gherkin steps. This is the correct shape when an app has
both web and api gherkin scopes that share step impls (e.g., oseplatform-web).

A matching test file is one whose base name starts with the feature file stem
followed by a dot (e.g. "user-login.feature" matches "user-login.integration.test.tsx").

All paths are resolved relative to the git repository root.

The reverse direction (test referencing a non-existent spec) is already enforced
at runtime by vitest-cucumber's loadFeature(), so only the spec-to-test direction
is checked here.`,
	Example: `  # Check organiclever-fe spec coverage
  rhino-cli spec-coverage validate specs/apps/organiclever-fe apps/organiclever-fe

  # Multiple gherkin scopes (web + api) against one app
  rhino-cli spec-coverage validate --shared-steps \
    specs/apps/oseplatform/behavior/web/gherkin \
    specs/apps/oseplatform/behavior/api/gherkin \
    apps/oseplatform-web

  # Output as JSON
  rhino-cli spec-coverage validate specs/apps/organiclever-fe apps/organiclever-fe -o json

  # Quiet mode
  rhino-cli spec-coverage validate specs/apps/organiclever-fe apps/organiclever-fe -q`,
	Args:          cobra.MinimumNArgs(2),
	SilenceErrors: true,
	RunE:          runValidateSpecCoverage,
}

var sharedSteps bool
var excludeDirs []string

func init() {
	validateSpecCoverageCmd.Flags().BoolVar(&sharedSteps, "shared-steps", false, "skip file matching, validate steps across ALL source files")
	validateSpecCoverageCmd.Flags().StringSliceVar(&excludeDirs, "exclude-dir", nil, "spec directory names to exclude (e.g., --exclude-dir test-support)")
	specCoverageCmd.AddCommand(validateSpecCoverageCmd)
}

func runValidateSpecCoverage(cmd *cobra.Command, args []string) error {
	repoRoot, err := findGitRoot()
	if err != nil {
		return fmt.Errorf("failed to find git repository root: %w", err)
	}

	// Last positional arg is the app-dir; preceding args are specs-dirs.
	appDir := filepath.Join(repoRoot, args[len(args)-1])
	specsDirs := make([]string, 0, len(args)-1)
	for _, sd := range args[:len(args)-1] {
		specsDirs = append(specsDirs, filepath.Join(repoRoot, sd))
	}

	opts := speccoverage.ScanOptions{
		RepoRoot:    repoRoot,
		SpecsDir:    specsDirs[0], // primary; preserved for backward-compat
		SpecsDirs:   specsDirs,
		AppDir:      appDir,
		Verbose:     verbose,
		Quiet:       quiet,
		SharedSteps: sharedSteps,
		ExcludeDirs: excludeDirs,
	}

	result, err := specCoverageCheckAllFn(opts)
	if err != nil {
		return fmt.Errorf("spec coverage check failed: %w", err)
	}

	if err := writeFormattedV2(cmd, verbose, quiet, outputFuncs{
		text:     func(v, q bool) string { return speccoverage.FormatText(result, v, q) },
		json:     func() (string, error) { return speccoverage.FormatJSON(result) },
		markdown: func() string { return speccoverage.FormatMarkdown(result) },
	}); err != nil {
		return err
	}

	hasGaps := len(result.Gaps) > 0 ||
		len(result.ScenarioGaps) > 0 ||
		len(result.StepGaps) > 0 ||
		len(result.OrphanStepImpls) > 0
	if hasGaps {
		if !quiet && output == "text" {
			if len(result.Gaps) > 0 {
				_, _ = fmt.Fprintf(cmd.OutOrStderr(), "\n❌ Found %d spec(s) without matching test files\n", len(result.Gaps))
			}
			if len(result.ScenarioGaps) > 0 {
				_, _ = fmt.Fprintf(cmd.OutOrStderr(), "❌ Found %d scenario(s) without matching test implementations\n", len(result.ScenarioGaps))
			}
			if len(result.StepGaps) > 0 {
				_, _ = fmt.Fprintf(cmd.OutOrStderr(), "❌ Found %d step(s) without matching step definitions\n", len(result.StepGaps))
			}
			if len(result.OrphanStepImpls) > 0 {
				_, _ = fmt.Fprintf(cmd.OutOrStderr(),
					"❌ Found %d orphan step implementation(s) (no Gherkin step matches them)\n",
					len(result.OrphanStepImpls))
			}
		}
		return fmt.Errorf("spec coverage gaps found: %d file gap(s), %d scenario gap(s), %d step gap(s), %d orphan step impl(s)",
			len(result.Gaps), len(result.ScenarioGaps), len(result.StepGaps), len(result.OrphanStepImpls))
	}

	return nil
}
