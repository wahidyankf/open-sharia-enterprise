package cmd

import (
	"fmt"

	"github.com/spf13/cobra"
)

var linkCmd = &cobra.Command{
	Use:   "link",
	Short: "Link validation commands (planned)",
	Long:  `Commands for checking internal and external links.`,
}

func init() {
	rootCmd.AddCommand(linkCmd)

	linkCmd.AddCommand(&cobra.Command{
		Use:   "validate",
		Short: "Verify links (planned)",
		RunE: func(cmd *cobra.Command, args []string) error {
			fmt.Println("Command not yet implemented. Coming soon!")
			return nil
		},
	})
}
