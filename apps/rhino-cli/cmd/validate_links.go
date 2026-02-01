package cmd

import (
	"fmt"
	"os"
	"path/filepath"

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

// findGitRoot finds the root directory of the git repository by walking up from the current directory.
func findGitRoot() (string, error) {
	dir, err := os.Getwd()
	if err != nil {
		return "", err
	}

	// Walk up the directory tree looking for .git
	for {
		gitDir := filepath.Join(dir, ".git")
		if _, err := os.Stat(gitDir); err == nil {
			return dir, nil
		}

		// Move up one directory
		parent := filepath.Dir(dir)
		if parent == dir {
			// Reached root without finding .git
			return "", fmt.Errorf(".git directory not found")
		}
		dir = parent
	}
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

	// Format output based on --output flag
	var formattedOutput string
	switch output {
	case "json":
		formattedOutput, err = links.FormatJSON(result)
		if err != nil {
			return fmt.Errorf("failed to format JSON: %w", err)
		}
	case "markdown":
		formattedOutput = links.FormatMarkdown(result)
	default: // "text"
		formattedOutput = links.FormatText(result, verbose, quiet)
	}

	// Write output
	fmt.Fprint(cmd.OutOrStdout(), formattedOutput)

	// Return error if broken links found (Cobra will set exit code 1)
	if len(result.BrokenLinks) > 0 {
		if !quiet && output == "text" {
			fmt.Fprintf(cmd.OutOrStderr(), "\n❌ Found %d broken links\n", len(result.BrokenLinks))
		}
		// Return error to signal failure (Cobra will set exit code 1)
		// Using os.Exit() directly would bypass deferred functions and tests
		return fmt.Errorf("found %d broken links", len(result.BrokenLinks))
	}

	// Success case - no error to return
	return nil
}
