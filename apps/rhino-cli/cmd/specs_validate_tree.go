package cmd

import (
	"fmt"
	"path/filepath"

	"github.com/spf13/cobra"
	"github.com/wahidyankf/ose-public/apps/rhino-cli/internal/allowlist"
)

// requiredSpecFolders is the canonical C4-aware five-folder spec tree.
var requiredSpecFolders = []string{
	"product",
	"system-context",
	"containers",
	"components",
	"behavior",
}

var specsValidateTreeAppsFlag []string

var specsValidateTreeCmd = &cobra.Command{
	Use:   "validate-tree [app]",
	Short: "Validate that a spec tree has the canonical C4-aware five-folder structure",
	Long: `Verify that specs/apps/<app>/ contains all five canonical top-level folders:
  - product/
  - system-context/
  - containers/
  - components/
  - behavior/

Each folder must exist and must contain a README.md file.
Reports one JSON finding per violation. Exits non-zero if any findings are found.

App selection precedence (first non-empty wins):
  1. positional arg (single app, today's behavior)
  2. --apps flag (StringSlice, comma-separated or repeated)
  3. allowlist default (organiclever, wahidyankf, ose-platform, ayokoding)`,
	Example: `  # Validate organiclever spec tree (positional)
  rhino-cli specs validate-tree organiclever

  # Validate two apps via flag
  rhino-cli specs validate-tree --apps organiclever,wahidyankf

  # Validate every allowlisted app (default when no positional and no flag)
  rhino-cli specs validate-tree`,
	Args:          cobra.MaximumNArgs(1),
	SilenceErrors: true,
	RunE:          runSpecsValidateTree,
}

func init() {
	specsValidateTreeCmd.Flags().StringSliceVar(
		&specsValidateTreeAppsFlag,
		"apps",
		nil,
		"comma-separated list of apps to validate; defaults to the DDD allowlist when neither positional nor flag is given",
	)
	specsCmd.AddCommand(specsValidateTreeCmd)
}

// SpecFinding is a single finding from a specs subcommand.
type SpecFinding struct {
	Category    string `json:"category"`
	Criticality string `json:"criticality"`
	File        string `json:"file"`
	Evidence    string `json:"evidence"`
	Expected    string `json:"expected"`
}

// resolveTreeApps returns the list of apps to iterate over for
// `specs validate-tree`. Precedence: positional arg > --apps flag >
// allowlist.AppsWithDDD default.
func resolveTreeApps(positional []string, appsFlag []string) []string {
	if len(positional) > 0 {
		return []string{positional[0]}
	}
	if len(appsFlag) > 0 {
		return append([]string(nil), appsFlag...)
	}
	return append([]string(nil), allowlist.AppsWithDDD...)
}

func runSpecsValidateTree(cmd *cobra.Command, args []string) error {
	repoRoot, err := findGitRoot()
	if err != nil {
		return fmt.Errorf("failed to find git repository root: %w", err)
	}

	apps := resolveTreeApps(args, specsValidateTreeAppsFlag)

	totalFindings := 0
	for _, app := range apps {
		findings := specsValidateTreeFn(repoRoot, app)
		findingCount := len(findings)
		if findingCount == 0 {
			_, _ = fmt.Fprintf(cmd.OutOrStdout(), "specs validate-tree: 0 finding(s) for %q\n", app)
			continue
		}
		for _, f := range findings {
			_, _ = fmt.Fprintf(cmd.OutOrStdout(), "%s: HIGH: %s\n", f.File, f.Evidence)
		}
		totalFindings += findingCount
	}

	if totalFindings > 0 {
		return fmt.Errorf("%d finding(s) found by specs validate-tree", totalFindings)
	}
	return nil
}

// validateSpecTree checks that the five required folders exist and each has a README.md.
func validateSpecTree(repoRoot, app string) []SpecFinding {
	var findings []SpecFinding
	baseDir := filepath.Join(repoRoot, "specs", "apps", app)

	for _, folder := range requiredSpecFolders {
		folderPath := filepath.Join(baseDir, folder)
		if _, err := osStat(folderPath); err != nil {
			findings = append(findings, SpecFinding{
				Category:    "tree-shape",
				Criticality: "HIGH",
				File:        filepath.Join("specs", "apps", app),
				Evidence:    fmt.Sprintf("missing required folder: %s", folder),
				Expected:    fmt.Sprintf("create specs/apps/%s/%s/ with README.md", app, folder),
			})
			continue
		}

		readmePath := filepath.Join(folderPath, "README.md")
		if _, err := osStat(readmePath); err != nil {
			findings = append(findings, SpecFinding{
				Category:    "tree-shape",
				Criticality: "HIGH",
				File:        filepath.Join("specs", "apps", app, folder),
				Evidence:    fmt.Sprintf("missing README.md in required folder: %s", folder),
				Expected:    fmt.Sprintf("create specs/apps/%s/%s/README.md", app, folder),
			})
		}
	}

	return findings
}
