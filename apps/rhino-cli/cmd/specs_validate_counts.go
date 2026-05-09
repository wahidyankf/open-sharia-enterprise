package cmd

import (
	"fmt"
	"os"
	"path/filepath"
	"strings"

	"github.com/spf13/cobra"
)

var specsValidateCountsCmd = &cobra.Command{
	Use:   "validate-counts <folder>",
	Short: "Validate that each required spec subfolder contains at least one spec file",
	Long: `Verify that each required subfolder of the given spec folder contains at least
one non-README .md file (i.e. a spec file beyond the index README.md).

Required subfolders checked: product, system-context, containers, components, behavior.

Reports one finding per empty or missing subfolder. Exits non-zero if any findings are found.`,
	Example: `  # Validate organiclever spec counts
  rhino-cli specs validate-counts specs/apps/organiclever`,
	Args:          cobra.ExactArgs(1),
	SilenceErrors: true,
	RunE:          runSpecsValidateCounts,
}

func init() {
	specsCmd.AddCommand(specsValidateCountsCmd)
}

func runSpecsValidateCounts(cmd *cobra.Command, args []string) error {
	repoRoot, err := findGitRoot()
	if err != nil {
		return fmt.Errorf("failed to find git repository root: %w", err)
	}

	folder := args[0]
	findings := specsValidateCountsFn(repoRoot, folder)

	findingCount := len(findings)
	if findingCount == 0 {
		_, _ = fmt.Fprintf(cmd.OutOrStdout(), "specs validate-counts: 0 finding(s) for %q\n", folder)
		return nil
	}

	for _, f := range findings {
		_, _ = fmt.Fprintf(cmd.OutOrStdout(), "%s: MEDIUM: %s\n", f.File, f.Evidence)
	}
	return fmt.Errorf("%d finding(s) found by specs validate-counts", findingCount)
}

// validateSpecCounts checks each required subfolder of the given spec folder contains
// at least one non-README .md file.
func validateSpecCounts(repoRoot, folder string) []SpecFinding {
	var findings []SpecFinding

	// Resolve the absolute path of the folder argument (may be relative to repo root).
	absFolder := folder
	if !filepath.IsAbs(folder) {
		absFolder = filepath.Join(repoRoot, folder)
	}

	if _, err := osStat(absFolder); err != nil {
		findings = append(findings, SpecFinding{
			Category:    "count",
			Criticality: "HIGH",
			File:        folder,
			Evidence:    fmt.Sprintf("spec folder does not exist: %s", folder),
			Expected:    "create the spec folder with required subfolders",
		})
		return findings
	}

	for _, sub := range requiredSpecFolders {
		subPath := filepath.Join(absFolder, sub)
		relPath := filepath.Join(folder, sub)

		if _, err := osStat(subPath); err != nil {
			findings = append(findings, SpecFinding{
				Category:    "count",
				Criticality: "MEDIUM",
				File:        relPath,
				Evidence:    fmt.Sprintf("missing required folder: %s", sub),
				Expected:    fmt.Sprintf("create %s/README.md plus at least one spec .md file", relPath),
			})
			continue
		}

		specFiles := specCountNonReadmeMdFilesFn(subPath)
		if specFiles == 0 {
			findings = append(findings, SpecFinding{
				Category:    "count",
				Criticality: "MEDIUM",
				File:        relPath,
				Evidence:    fmt.Sprintf("empty subfolder: %s contains no spec files (only README.md or nothing)", sub),
				Expected:    fmt.Sprintf("add at least one non-README .md spec file to %s/", relPath),
			})
		}
	}

	return findings
}

// countNonReadmeMdFiles counts spec files (.md non-README + .feature) recursively under dir.
func countNonReadmeMdFiles(dir string) int {
	count := 0
	_ = filepath.Walk(dir, func(path string, info os.FileInfo, err error) error {
		if err != nil {
			return nil //nolint:nilerr
		}
		if info.IsDir() {
			return nil
		}
		name := filepath.Base(path)
		lower := strings.ToLower(name)
		if strings.HasSuffix(lower, ".feature") {
			count++
		} else if strings.HasSuffix(lower, ".md") && !strings.EqualFold(name, "README.md") {
			count++
		}
		return nil
	})
	return count
}
