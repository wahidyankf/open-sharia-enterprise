package cmd

import (
	"fmt"

	"github.com/spf13/cobra"
)

var contentCmd = &cobra.Command{
	Use:   "content",
	Short: "Content management commands (planned)",
	Long:  `Commands for scaffolding and validating content files.`,
}

func init() {
	rootCmd.AddCommand(contentCmd)

	// Placeholder subcommands
	contentCmd.AddCommand(&cobra.Command{
		Use:   "scaffold",
		Short: "Create new content files (planned)",
		RunE: func(cmd *cobra.Command, args []string) error {
			fmt.Println("Command not yet implemented. Coming soon!")
			return nil
		},
	})

	contentCmd.AddCommand(&cobra.Command{
		Use:   "validate",
		Short: "Validate frontmatter (planned)",
		RunE: func(cmd *cobra.Command, args []string) error {
			fmt.Println("Command not yet implemented. Coming soon!")
			return nil
		},
	})
}
