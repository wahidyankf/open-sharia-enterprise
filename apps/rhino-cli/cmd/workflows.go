package cmd

import "github.com/spf13/cobra"

var workflowsCmd = &cobra.Command{
	Use:   "workflows",
	Short: "Workflow definition commands",
	Long:  `Commands for validating governance workflow definitions.`,
}

func init() {
	rootCmd.AddCommand(workflowsCmd)
}
