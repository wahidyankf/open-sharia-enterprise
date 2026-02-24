package cmd

import (
	"fmt"

	"github.com/spf13/cobra"
	"github.com/wahidyankf/open-sharia-enterprise/apps/rhino-cli/internal/docs"
)

var (
	validateDocsNamingStagedOnly bool
	validateDocsNamingFix        bool
	validateDocsNamingApply      bool
	validateDocsNamingNoLinks    bool
)

var validateDocsNamingCmd = &cobra.Command{
	Use:   "validate-docs-naming",
	Short: "Validate documentation file naming conventions",
	Long: `Scan documentation files in docs/ directory for naming convention violations.

This command validates that all files in docs/ follow the hierarchical prefix
naming convention defined in governance/conventions/structure/file-naming.md.

Pattern: [hierarchical-prefix]__[content-identifier].md

Examples of valid names:
  - tu__getting-started.md (tutorials)
  - hoto__deploy-docker.md (how-to)
  - re__api-reference.md (reference)
  - ex-so-prla-py__basics.md (explanation subdirectories)

Exceptions (no prefix required):
  - README.md files (index files)
  - Files in docs/metadata/ directory

Fix Mode:
  Use --fix to see what files would be renamed (dry-run).
  Use --fix --apply to actually rename the files using git mv.`,
	Example: `  # Validate all docs files
  rhino-cli validate-docs-naming

  # Validate only staged files (useful in pre-commit hooks)
  rhino-cli validate-docs-naming --staged-only

  # Output as JSON
  rhino-cli validate-docs-naming -o json

  # Output as markdown report
  rhino-cli validate-docs-naming -o markdown

  # Show what files would be renamed (dry-run)
  rhino-cli validate-docs-naming --fix

  # Actually rename files to fix violations
  rhino-cli validate-docs-naming --fix --apply

  # Fix without updating links in other files
  rhino-cli validate-docs-naming --fix --apply --no-update-links`,
	SilenceErrors: true, // We handle error messages ourselves
	RunE:          runValidateDocsNaming,
}

func init() {
	rootCmd.AddCommand(validateDocsNamingCmd)
	validateDocsNamingCmd.Flags().BoolVar(&validateDocsNamingStagedOnly, "staged-only", false, "only validate staged files in docs/")
	validateDocsNamingCmd.Flags().BoolVar(&validateDocsNamingFix, "fix", false, "show files that would be renamed (dry-run)")
	validateDocsNamingCmd.Flags().BoolVar(&validateDocsNamingApply, "apply", false, "actually apply the renames (requires --fix)")
	validateDocsNamingCmd.Flags().BoolVar(&validateDocsNamingNoLinks, "no-update-links", false, "skip updating links in other files")
}

func runValidateDocsNaming(cmd *cobra.Command, args []string) error {
	// Validate flag combinations
	if validateDocsNamingApply && !validateDocsNamingFix {
		return fmt.Errorf("--apply requires --fix flag")
	}

	// Find git repository root
	repoRoot, err := findGitRoot()
	if err != nil {
		return fmt.Errorf("failed to find git repository root: %w", err)
	}

	// Build validation options from flags
	opts := docs.ValidationOptions{
		RepoRoot:   repoRoot,
		StagedOnly: validateDocsNamingStagedOnly,
		Verbose:    verbose,
		Quiet:      quiet,
	}

	// Validate all docs files
	result, err := docs.ValidateAll(opts)
	if err != nil {
		return fmt.Errorf("validation failed: %w", err)
	}

	// If --fix flag is set, run fix mode
	if validateDocsNamingFix {
		return runFixMode(cmd, result, repoRoot)
	}

	// Normal validation mode
	return runValidationMode(cmd, result)
}

func runValidationMode(cmd *cobra.Command, result *docs.ValidationResult) error {
	// Format output based on --output flag
	var formattedOutput string
	var err error

	switch output {
	case "json":
		formattedOutput, err = docs.FormatJSON(result)
		if err != nil {
			return fmt.Errorf("failed to format JSON: %w", err)
		}
	case "markdown":
		formattedOutput = docs.FormatMarkdown(result)
	default: // "text"
		formattedOutput = docs.FormatText(result, verbose, quiet)
	}

	// Write output
	_, _ = fmt.Fprint(cmd.OutOrStdout(), formattedOutput)

	// Return error if violations found (Cobra will set exit code 1)
	if len(result.Violations) > 0 {
		if !quiet && output == "text" {
			_, _ = fmt.Fprintf(cmd.OutOrStderr(), "\n❌ Found %d naming violations\n", len(result.Violations))
		}
		return fmt.Errorf("found %d naming violations", len(result.Violations))
	}

	return nil
}

func runFixMode(cmd *cobra.Command, validationResult *docs.ValidationResult, repoRoot string) error {
	// Build fix options
	fixOpts := docs.FixOptions{
		RepoRoot:    repoRoot,
		DryRun:      !validateDocsNamingApply,
		UpdateLinks: !validateDocsNamingNoLinks,
		Verbose:     verbose,
	}

	// Run fix operation
	fixResult, err := docs.Fix(validationResult, fixOpts)
	if err != nil {
		return fmt.Errorf("fix operation failed: %w", err)
	}

	// Format output
	var formattedOutput string

	switch output {
	case "json":
		formattedOutput, err = docs.FormatFixJSON(fixResult)
		if err != nil {
			return fmt.Errorf("failed to format JSON: %w", err)
		}
	default: // "text" or "markdown"
		if fixResult.DryRun {
			formattedOutput = docs.FormatFixPlan(fixResult)
		} else {
			formattedOutput = docs.FormatFixResult(fixResult)
		}
	}

	// Write output
	_, _ = fmt.Fprint(cmd.OutOrStdout(), formattedOutput)

	// Return error if there were errors during apply
	if !fixResult.DryRun && len(fixResult.Errors) > 0 {
		return fmt.Errorf("encountered %d errors during fix", len(fixResult.Errors))
	}

	return nil
}
