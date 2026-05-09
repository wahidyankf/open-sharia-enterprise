package cmd

import (
	"fmt"

	"github.com/spf13/cobra"
)

var specsDriftRoutesCmd = &cobra.Command{
	Use:   "drift-routes <app>",
	Short: "Detect drift between spec routes and Next.js route definitions (not yet implemented)",
	Long: `Placeholder: parsing Next.js routes is out of scope for the current pilot.

This command is scaffolded so governance references are valid. It will print a
"Not yet implemented" message and exit 0.`,
	Example:       `  rhino-cli specs drift-routes organiclever`,
	Args:          cobra.ExactArgs(1),
	SilenceErrors: true,
	RunE:          runSpecsDriftRoutes,
}

func init() {
	specsCmd.AddCommand(specsDriftRoutesCmd)
}

func runSpecsDriftRoutes(cmd *cobra.Command, args []string) error {
	_, _ = fmt.Fprintf(cmd.OutOrStdout(), "Not yet implemented: drift-routes (app: %s)\n", args[0])
	return nil
}
