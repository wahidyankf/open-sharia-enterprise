package cmd

import (
	"fmt"
	"path/filepath"

	"github.com/spf13/cobra"
)

// requiredSpecFolders is the canonical C4-aware five-folder spec tree.
var requiredSpecFolders = []string{
	"product",
	"system-context",
	"containers",
	"components",
	"behavior",
}

var specsValidateTreeCmd = &cobra.Command{
	Use:   "validate-tree <app>",
	Short: "Validate that a spec tree has the canonical C4-aware five-folder structure",
	Long: `Verify that specs/apps/<app>/ contains all five canonical top-level folders:
  - product/
  - system-context/
  - containers/
  - components/
  - behavior/

Each folder must exist and must contain a README.md file.
Reports one JSON finding per violation. Exits non-zero if any findings are found.`,
	Example: `  # Validate organiclever spec tree
  rhino-cli specs validate-tree organiclever`,
	Args:          cobra.ExactArgs(1),
	SilenceErrors: true,
	RunE:          runSpecsValidateTree,
}

func init() {
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

func runSpecsValidateTree(cmd *cobra.Command, args []string) error {
	repoRoot, err := findGitRoot()
	if err != nil {
		return fmt.Errorf("failed to find git repository root: %w", err)
	}

	app := args[0]
	findings := specsValidateTreeFn(repoRoot, app)

	findingCount := len(findings)
	if findingCount == 0 {
		_, _ = fmt.Fprintf(cmd.OutOrStdout(), "specs validate-tree: 0 finding(s) for %q\n", app)
		return nil
	}

	for _, f := range findings {
		_, _ = fmt.Fprintf(cmd.OutOrStdout(), "%s: HIGH: %s\n", f.File, f.Evidence)
	}
	return fmt.Errorf("%d finding(s) found by specs validate-tree", findingCount)
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
