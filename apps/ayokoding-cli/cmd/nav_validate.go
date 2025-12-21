package cmd

import (
	"fmt"

	"github.com/spf13/cobra"
)

var navValidateCmd = &cobra.Command{
	Use:   "validate [path]",
	Short: "Validate navigation structure and ordering",
	Long:  `Validates that navigation structure follows ayokoding-web conventions.`,
	Example: `  ayokoding-cli nav validate
  ayokoding-cli nav validate --path apps/ayokoding-web/content/en/learn`,
	Args: cobra.MaximumNArgs(1),
	RunE: func(cmd *cobra.Command, args []string) error {
		fmt.Println("Command not yet implemented. Coming soon!")
		fmt.Println()
		fmt.Println("This command will validate:")
		fmt.Println("  - Navigation depth (3 layers)")
		fmt.Println("  - Item ordering by weight")
		fmt.Println("  - Link validity")
		return nil
	},
}

func init() {
	navCmd.AddCommand(navValidateCmd)
}
