package cmd

import "github.com/spf13/cobra"

var agentsCmd = &cobra.Command{
	Use:   "agents",
	Short: "Agent configuration commands",
	Long:  `Commands for syncing and validating Claude Code / OpenCode agent configurations.`,
}

func init() {
	rootCmd.AddCommand(agentsCmd)
}
