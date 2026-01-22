package cmd

import (
	"fmt"

	"github.com/spf13/cobra"
	"github.com/wahidyankf/open-sharia-enterprise/apps/rhino-cli/internal/sync"
)

var validateSyncCmd = &cobra.Command{
	Use:   "validate-sync",
	Short: "Validate that .claude/ and .opencode/ are in sync",
	Long: `Validate that .claude/ and .opencode/ configurations are semantically equivalent.

This command performs the following validations:

Agents:
- Count check: Ensures equal number of agents in both directories
- Equivalence check: Validates each agent is semantically equivalent:
  * Description matches exactly
  * Model is correctly converted (empty/sonnet/opus → inherit/zai/glm-4.7)
  * Tools are correctly mapped (array → boolean map, lowercase)
  * Skills array matches exactly
  * Body content is identical

Skills:
- Count check: Ensures equal number of skills in both directories
- Identity check: Validates skills are byte-for-byte identical`,
	Example: `  # Validate sync
  rhino-cli validate-sync

  # Output as JSON
  rhino-cli validate-sync -o json

  # Verbose mode (show all checks)
  rhino-cli validate-sync -v

  # Quiet mode (show only summary)
  rhino-cli validate-sync -q`,
	SilenceErrors: true,
	RunE:          runValidateSync,
}

func init() {
	rootCmd.AddCommand(validateSyncCmd)
}

func runValidateSync(cmd *cobra.Command, args []string) error {
	// Find git repository root
	repoRoot, err := findGitRoot()
	if err != nil {
		return fmt.Errorf("failed to find git repository root: %w", err)
	}

	// Perform validation
	result, err := sync.ValidateSync(repoRoot)
	if err != nil {
		return fmt.Errorf("validation failed: %w", err)
	}

	// Format and print output
	formattedOutput := sync.FormatValidationResult(result, output, verbose, quiet)
	fmt.Fprint(cmd.OutOrStdout(), formattedOutput)

	// Return error if validation failed
	if result.FailedChecks > 0 {
		return fmt.Errorf("validation failed: %d checks failed", result.FailedChecks)
	}

	return nil
}
