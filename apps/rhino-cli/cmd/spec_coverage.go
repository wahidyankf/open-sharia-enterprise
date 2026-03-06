package cmd

import "github.com/spf13/cobra"

var specCoverageCmd = &cobra.Command{
	Use:   "spec-coverage",
	Short: "BDD spec coverage commands",
	Long:  `Commands for validating BDD spec-to-test coverage.`,
}

func init() {
	rootCmd.AddCommand(specCoverageCmd)
}
