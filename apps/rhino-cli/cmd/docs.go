package cmd

import "github.com/spf13/cobra"

var docsCmd = &cobra.Command{
	Use:   "docs",
	Short: "Documentation validation commands",
	Long:  `Commands for validating documentation links and naming conventions.`,
}

func init() {
	rootCmd.AddCommand(docsCmd)
}
