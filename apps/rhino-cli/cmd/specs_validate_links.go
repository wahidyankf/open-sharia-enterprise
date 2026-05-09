package cmd

import (
	"fmt"
	"path/filepath"
	"regexp"
	"strings"

	"github.com/spf13/cobra"
)

var specsValidateLinksCmd = &cobra.Command{
	Use:   "validate-links <folder>",
	Short: "Check that markdown links in spec files resolve to existing files",
	Long: `Scan all .md files under <folder> and verify that every internal markdown
link points to an existing file. External links (starting with http:// or
https://) are ignored.

A link is considered internal if its target does not start with "http://" or
"https://". Relative paths are resolved from the directory containing the
linking file.

Reports one finding per broken link. Exits non-zero if any findings are found.`,
	Example: `  # Validate links in organiclever spec folder
  rhino-cli specs validate-links specs/apps/organiclever`,
	Args:          cobra.ExactArgs(1),
	SilenceErrors: true,
	RunE:          runSpecsValidateLinks,
}

func init() {
	specsCmd.AddCommand(specsValidateLinksCmd)
}

// markdownLinkRe matches [text](target) patterns.
var markdownLinkRe = regexp.MustCompile(`\[([^\]]*)\]\(([^)]+)\)`)

func runSpecsValidateLinks(cmd *cobra.Command, args []string) error {
	repoRoot, err := findGitRoot()
	if err != nil {
		return fmt.Errorf("failed to find git repository root: %w", err)
	}

	folder := args[0]
	findings := specsValidateLinksFn(repoRoot, folder)

	findingCount := len(findings)
	if findingCount == 0 {
		_, _ = fmt.Fprintf(cmd.OutOrStdout(), "specs validate-links: 0 finding(s) for %q\n", folder)
		return nil
	}

	for _, f := range findings {
		_, _ = fmt.Fprintf(cmd.OutOrStdout(), "%s: HIGH: %s\n", f.File, f.Evidence)
	}
	return fmt.Errorf("%d finding(s) found by specs validate-links", findingCount)
}

// validateSpecLinks scans all .md files under folder and checks internal link targets exist.
func validateSpecLinks(repoRoot, folder string) []SpecFinding {
	var findings []SpecFinding

	absFolder := folder
	if !filepath.IsAbs(folder) {
		absFolder = filepath.Join(repoRoot, folder)
	}

	if _, err := osStat(absFolder); err != nil {
		findings = append(findings, SpecFinding{
			Category:    "links",
			Criticality: "HIGH",
			File:        folder,
			Evidence:    fmt.Sprintf("spec folder does not exist: %s", folder),
			Expected:    "create the spec folder",
		})
		return findings
	}

	mdFiles := specsWalkMdFilesFn(absFolder)
	for _, mdFile := range mdFiles {
		content, err := readFileFn(mdFile)
		if err != nil {
			continue
		}

		matches := markdownLinkRe.FindAllSubmatch(content, -1)
		for _, match := range matches {
			target := string(match[2])

			// Strip fragment identifiers.
			if idx := strings.Index(target, "#"); idx >= 0 {
				target = target[:idx]
			}
			if target == "" {
				continue
			}

			// Skip external links.
			if strings.HasPrefix(target, "http://") || strings.HasPrefix(target, "https://") {
				continue
			}

			// Resolve relative to the file's directory.
			resolved := filepath.Join(filepath.Dir(mdFile), target)
			if _, err := osStat(resolved); err != nil {
				relFile, _ := filepath.Rel(repoRoot, mdFile)
				findings = append(findings, SpecFinding{
					Category:    "links",
					Criticality: "HIGH",
					File:        relFile,
					Evidence:    fmt.Sprintf("broken link: %s -> %s (file not found)", filepath.Base(mdFile), target),
					Expected:    fmt.Sprintf("fix or remove the link to %s", target),
				})
			}
		}
	}

	return findings
}

// walkMdFiles returns all .md files under dir recursively.
func walkMdFiles(dir string) []string {
	var result []string
	entries, err := readDirFn(dir)
	if err != nil {
		return result
	}
	for _, e := range entries {
		fullPath := filepath.Join(dir, e.Name())
		if e.IsDir() {
			result = append(result, walkMdFiles(fullPath)...)
			continue
		}
		if strings.HasSuffix(strings.ToLower(e.Name()), ".md") {
			result = append(result, fullPath)
		}
	}
	return result
}
