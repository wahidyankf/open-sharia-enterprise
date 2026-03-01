package cmd

import (
	"fmt"

	"github.com/spf13/cobra"
	"github.com/wahidyankf/open-sharia-enterprise/apps/rhino-cli/internal/links"
)

var (
	validateLinksStagedOnly bool
)

var validateLinksCmd = &cobra.Command{
	Use:   "validate-links",
	Short: "Validate markdown links in the repository",
	Long: `Scan markdown files for broken internal links.

This command scans markdown files in the repository and validates that all
internal links point to existing files. External URLs, Hugo paths, and
placeholder links are automatically skipped.

By default, scans all markdown files in core directories (docs/, governance/,
.claude/, and root). Use --staged-only to validate only staged files.`,
	Example: `  # Validate all markdown files
  rhino-cli validate-links

  # Validate only staged files (useful in pre-commit hooks)
  rhino-cli validate-links --staged-only

  # Output as JSON
  rhino-cli validate-links -o json

  # Output as markdown report
  rhino-cli validate-links -o markdown

  # Verbose mode with quiet output
  rhino-cli validate-links -v -q`,
	SilenceErrors: true, // We handle error messages ourselves
	RunE:          runValidateLinks,
}

func init() {
	rootCmd.AddCommand(validateLinksCmd)
	validateLinksCmd.Flags().BoolVar(&validateLinksStagedOnly, "staged-only", false, "only validate staged files")
}

func runValidateLinks(cmd *cobra.Command, args []string) error {
	// Find git repository root
	repoRoot, err := findGitRoot()
	if err != nil {
		return fmt.Errorf("failed to find git repository root: %w", err)
	}

	// Build scan options from flags
	opts := links.ScanOptions{
		RepoRoot:   repoRoot,
		StagedOnly: validateLinksStagedOnly,
		SkipPaths:  []string{".opencode/skill/"}, // Exclude auto-generated skill files
		Verbose:    verbose,
		Quiet:      quiet,
	}

	// Validate all links
	result, err := links.ValidateAll(opts)
	if err != nil {
		return fmt.Errorf("validation failed: %w", err)
	}

	if err := writeFormatted(cmd, output, verbose, quiet, outputFuncs{
		text:     func(v, q bool) string { return links.FormatText(result, v, q) },
		json:     func() (string, error) { return links.FormatJSON(result) },
		markdown: func() string { return links.FormatMarkdown(result) },
	}); err != nil {
		return err
	}

	// Return error if broken links found (Cobra will set exit code 1)
	if len(result.BrokenLinks) > 0 {
		if !quiet && output == "text" {
			_, _ = fmt.Fprintf(cmd.OutOrStderr(), "\n❌ Found %d broken links\n", len(result.BrokenLinks))
		}
		// Return error to signal failure (Cobra will set exit code 1)
		// Using os.Exit() directly would bypass deferred functions and tests
		return fmt.Errorf("found %d broken links", len(result.BrokenLinks))
	}

	// Success case - no error to return
	return nil
}
