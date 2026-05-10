package cmd

import (
	"fmt"
	"path/filepath"
	"strings"

	"github.com/spf13/cobra"
	"github.com/wahidyankf/ose-public/apps/rhino-cli/internal/allowlist"
)

var specsValidateAdoptionAppsFlag []string

var specsValidateAdoptionCmd = &cobra.Command{
	Use:   "validate-adoption [app]",
	Short: "Verify that an app has adopted BDD and DDD practices per FR-10",
	Long: `Check that <app> has adopted BDD and DDD practices:

  (a) specs/apps/<app>/behavior/ exists and contains at least one .feature file
      (searched recursively).
  (b) specs/apps/<app>/ddd/bounded-contexts.yaml exists.

Reports HIGH findings for missing adoption. Exits non-zero if any findings are found.

App selection precedence (first non-empty wins):
  1. positional arg (single app, today's behavior)
  2. --apps flag (StringSlice, comma-separated or repeated)
  3. allowlist default (organiclever, wahidyankf, oseplatform, ayokoding)`,
	Example: `  # Validate organiclever has adopted BDD and DDD (positional)
  rhino-cli specs validate-adoption organiclever

  # Validate two apps via flag
  rhino-cli specs validate-adoption --apps organiclever,wahidyankf

  # Validate every allowlisted app (default when no positional and no flag)
  rhino-cli specs validate-adoption`,
	Args:          cobra.MaximumNArgs(1),
	SilenceErrors: true,
	RunE:          runSpecsValidateAdoption,
}

func init() {
	specsValidateAdoptionCmd.Flags().StringSliceVar(
		&specsValidateAdoptionAppsFlag,
		"apps",
		nil,
		"comma-separated list of apps to validate; defaults to the DDD allowlist when neither positional nor flag is given",
	)
	specsCmd.AddCommand(specsValidateAdoptionCmd)
}

// resolveAdoptionApps returns the list of apps to iterate over for
// `specs validate-adoption`. Precedence: positional arg > --apps flag >
// allowlist.AppsWithDDD default. The function is exported only via the
// package-internal test API; the production caller is runSpecsValidateAdoption.
func resolveAdoptionApps(positional []string, appsFlag []string) []string {
	if len(positional) > 0 {
		return []string{positional[0]}
	}
	if len(appsFlag) > 0 {
		return append([]string(nil), appsFlag...)
	}
	return append([]string(nil), allowlist.AppsWithDDD...)
}

func runSpecsValidateAdoption(cmd *cobra.Command, args []string) error {
	repoRoot, err := findGitRoot()
	if err != nil {
		return fmt.Errorf("failed to find git repository root: %w", err)
	}

	apps := resolveAdoptionApps(args, specsValidateAdoptionAppsFlag)

	totalFindings := 0
	for _, app := range apps {
		findings := specsValidateAdoptionFn(repoRoot, app)
		findingCount := len(findings)
		if findingCount == 0 {
			_, _ = fmt.Fprintf(cmd.OutOrStdout(), "specs validate-adoption: 0 finding(s) for %q\n", app)
			continue
		}
		for _, f := range findings {
			_, _ = fmt.Fprintf(cmd.OutOrStdout(), "%s: HIGH: %s\n", f.File, f.Evidence)
		}
		totalFindings += findingCount
	}

	if totalFindings > 0 {
		return fmt.Errorf("%d finding(s) found by specs validate-adoption", totalFindings)
	}
	return nil
}

// validateSpecAdoption checks BDD and DDD adoption for an app.
func validateSpecAdoption(repoRoot, app string) []SpecFinding {
	var findings []SpecFinding
	baseDir := filepath.Join(repoRoot, "specs", "apps", app)

	// Check (a): behavior/ exists and has at least one .feature file.
	behaviorDir := filepath.Join(baseDir, "behavior")
	if _, err := osStat(behaviorDir); err != nil {
		findings = append(findings, SpecFinding{
			Category:    "adoption",
			Criticality: "HIGH",
			File:        filepath.Join("specs", "apps", app, "behavior"),
			Evidence:    fmt.Sprintf("no feature files found under specs/apps/%s/behavior/ (directory does not exist)", app),
			Expected:    fmt.Sprintf("create specs/apps/%s/behavior/ with at least one .feature file", app),
		})
	} else {
		featureFiles := specsWalkFeatureFilesFn(behaviorDir)
		if len(featureFiles) == 0 {
			findings = append(findings, SpecFinding{
				Category:    "adoption",
				Criticality: "HIGH",
				File:        filepath.Join("specs", "apps", app, "behavior"),
				Evidence:    fmt.Sprintf("no feature files found under specs/apps/%s/behavior/", app),
				Expected:    fmt.Sprintf("add at least one .feature file under specs/apps/%s/behavior/", app),
			})
		}
	}

	// Check (b): bounded-contexts.yaml exists.
	bcYaml := filepath.Join(baseDir, "ddd", "bounded-contexts.yaml")
	if _, err := osStat(bcYaml); err != nil {
		findings = append(findings, SpecFinding{
			Category:    "adoption",
			Criticality: "HIGH",
			File:        filepath.Join("specs", "apps", app, "ddd"),
			Evidence:    fmt.Sprintf("missing bounded-contexts.yaml at specs/apps/%s/ddd/bounded-contexts.yaml", app),
			Expected:    fmt.Sprintf("create specs/apps/%s/ddd/bounded-contexts.yaml", app),
		})
	}

	return findings
}

// walkFeatureFiles returns all .feature files under dir recursively.
func walkFeatureFiles(dir string) []string {
	var result []string
	entries, err := readDirFn(dir)
	if err != nil {
		return result
	}
	for _, e := range entries {
		fullPath := filepath.Join(dir, e.Name())
		if e.IsDir() {
			result = append(result, walkFeatureFiles(fullPath)...)
			continue
		}
		if strings.HasSuffix(strings.ToLower(e.Name()), ".feature") {
			result = append(result, fullPath)
		}
	}
	return result
}
