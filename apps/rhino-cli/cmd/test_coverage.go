package cmd

import "github.com/spf13/cobra"

var testCoverageCmd = &cobra.Command{
	Use:   "test-coverage",
	Short: "Test coverage commands",
	Long:  `Commands for validating test coverage against thresholds.`,
}

func init() {
	rootCmd.AddCommand(testCoverageCmd)
}
