package cmd

import (
	"fmt"
	"os"
	"path/filepath"
	"strings"

	"github.com/spf13/cobra"
	"github.com/wahidyankf/ose-public/apps/rhino-cli/internal/allowlist"
)

var specsValidateCountsApps []string

var specsValidateCountsCmd = &cobra.Command{
	Use:   "validate-counts [<folder>]",
	Short: "Validate that each required spec subfolder contains at least one spec file",
	Long: `Verify that each required subfolder of the given spec folder contains at least
one non-README .md file (i.e. a spec file beyond the index README.md).

Required subfolders checked: product, system-context, containers, components, behavior.

Reports one finding per empty or missing subfolder. Exits non-zero if any findings are found.

Behavior:
  - With a positional <folder>, validate that single folder.
  - With --apps a,b,c, validate specs/apps/<a>, specs/apps/<b>, specs/apps/<c>.
  - With neither, default --apps to the AppsWithDDD allowlist.`,
	Example: `  # Validate organiclever spec counts (single folder)
  rhino-cli specs validate-counts specs/apps/organiclever

  # Multi-app via flag
  rhino-cli specs validate-counts --apps organiclever,wahidyankf

  # Default — runs across the AppsWithDDD allowlist
  rhino-cli specs validate-counts`,
	Args:          cobra.MaximumNArgs(1),
	SilenceErrors: true,
	RunE:          runSpecsValidateCounts,
}

func init() {
	specsValidateCountsCmd.Flags().StringSliceVar(&specsValidateCountsApps, "apps", nil, "comma-separated app names to validate (defaults to allowlist when no positional arg)")
	specsCmd.AddCommand(specsValidateCountsCmd)
}

func resolveCountsApps(args []string) []string {
	if len(args) > 0 {
		return []string{args[0]}
	}
	apps := specsValidateCountsApps
	if len(apps) == 0 {
		apps = allowlist.AppsWithDDD
	}
	out := make([]string, 0, len(apps))
	for _, a := range apps {
		out = append(out, filepath.Join("specs", "apps", a))
	}
	return out
}

func runSpecsValidateCounts(cmd *cobra.Command, args []string) error {
	repoRoot, err := findGitRoot()
	if err != nil {
		return fmt.Errorf("failed to find git repository root: %w", err)
	}

	folders := resolveCountsApps(args)
	totalFindings := 0
	for _, folder := range folders {
		findings := specsValidateCountsFn(repoRoot, folder)
		findingCount := len(findings)
		totalFindings += findingCount
		if findingCount == 0 {
			_, _ = fmt.Fprintf(cmd.OutOrStdout(), "specs validate-counts: 0 finding(s) for %q\n", folder)
			continue
		}
		for _, f := range findings {
			_, _ = fmt.Fprintf(cmd.OutOrStdout(), "%s: %s: %s\n", f.File, f.Criticality, f.Evidence)
		}
	}
	if totalFindings > 0 {
		return fmt.Errorf("%d finding(s) found by specs validate-counts", totalFindings)
	}
	return nil
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
				Criticality: "HIGH",
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
