package cmd

import (
	"fmt"

	"github.com/spf13/cobra"
)

var specsDriftContractsCmd = &cobra.Command{
	Use:   "drift-contracts <app>",
	Short: "Detect drift between spec contracts and OpenAPI definitions (not yet implemented)",
	Long: `Placeholder: contract drift detection is out of scope for the current pilot.

This command is scaffolded so governance references are valid. It will print a
"Not yet implemented" message and exit 0.`,
	Example:       `  rhino-cli specs drift-contracts organiclever`,
	Args:          cobra.ExactArgs(1),
	SilenceErrors: true,
	RunE:          runSpecsDriftContracts,
}

func init() {
	specsCmd.AddCommand(specsDriftContractsCmd)
}

func runSpecsDriftContracts(cmd *cobra.Command, args []string) error {
	_, _ = fmt.Fprintf(cmd.OutOrStdout(), "Not yet implemented: drift-contracts (app: %s)\n", args[0])
	return nil
}
