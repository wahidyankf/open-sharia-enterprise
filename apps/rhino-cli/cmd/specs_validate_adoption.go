package cmd

import (
	"fmt"
	"path/filepath"
	"strings"

	"github.com/spf13/cobra"
)

var specsValidateAdoptionCmd = &cobra.Command{
	Use:   "validate-adoption <app>",
	Short: "Verify that an app has adopted BDD and DDD practices per FR-10",
	Long: `Check that <app> has adopted BDD and DDD practices:

  (a) specs/apps/<app>/behavior/ exists and contains at least one .feature file
      (searched recursively).
  (b) specs/apps/<app>/ddd/bounded-contexts.yaml exists.

Reports HIGH findings for missing adoption. Exits non-zero if any findings are found.`,
	Example: `  # Validate organiclever has adopted BDD and DDD
  rhino-cli specs validate-adoption organiclever`,
	Args:          cobra.ExactArgs(1),
	SilenceErrors: true,
	RunE:          runSpecsValidateAdoption,
}

func init() {
	specsCmd.AddCommand(specsValidateAdoptionCmd)
}

func runSpecsValidateAdoption(cmd *cobra.Command, args []string) error {
	repoRoot, err := findGitRoot()
	if err != nil {
		return fmt.Errorf("failed to find git repository root: %w", err)
	}

	app := args[0]
	findings := specsValidateAdoptionFn(repoRoot, app)

	findingCount := len(findings)
	if findingCount == 0 {
		_, _ = fmt.Fprintf(cmd.OutOrStdout(), "specs validate-adoption: 0 finding(s) for %q\n", app)
		return nil
	}

	for _, f := range findings {
		_, _ = fmt.Fprintf(cmd.OutOrStdout(), "%s: HIGH: %s\n", f.File, f.Evidence)
	}
	return fmt.Errorf("%d finding(s) found by specs validate-adoption", findingCount)
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
