package cmd

import (
	"fmt"

	"github.com/spf13/cobra"
)

var weightCmd = &cobra.Command{
	Use:   "weight",
	Short: "Weight validation commands (planned)",
	Long:  `Commands for checking and fixing weight values.`,
}

func init() {
	rootCmd.AddCommand(weightCmd)

	weightCmd.AddCommand(&cobra.Command{
		Use:   "validate",
		Short: "Check weight values (planned)",
		RunE: func(cmd *cobra.Command, args []string) error {
			fmt.Println("Command not yet implemented. Coming soon!")
			return nil
		},
	})
}
