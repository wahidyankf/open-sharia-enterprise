package cmd

import (
	"fmt"

	"github.com/spf13/cobra"
)

var specsDriftEndpointsCmd = &cobra.Command{
	Use:   "drift-endpoints <app>",
	Short: "Detect drift between spec endpoints and implementation (not yet implemented)",
	Long: `Placeholder: endpoint drift detection is out of scope for the current pilot.

This command is scaffolded so governance references are valid. It will print a
"Not yet implemented" message and exit 0.`,
	Example:       `  rhino-cli specs drift-endpoints organiclever`,
	Args:          cobra.ExactArgs(1),
	SilenceErrors: true,
	RunE:          runSpecsDriftEndpoints,
}

func init() {
	specsCmd.AddCommand(specsDriftEndpointsCmd)
}

func runSpecsDriftEndpoints(cmd *cobra.Command, args []string) error {
	_, _ = fmt.Fprintf(cmd.OutOrStdout(), "Not yet implemented: drift-endpoints (app: %s)\n", args[0])
	return nil
}
