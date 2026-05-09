package cmd

import "github.com/spf13/cobra"

var specsCmd = &cobra.Command{
	Use:   "specs",
	Short: "Spec validation and drift detection commands",
	Long:  `Commands for validating spec tree structure and detecting drift between specs and code.`,
}

func init() {
	rootCmd.AddCommand(specsCmd)
}
