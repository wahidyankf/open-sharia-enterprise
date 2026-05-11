package cmd

import "github.com/spf13/cobra"

var repoGovernanceCmd = &cobra.Command{
	Use:   "repo-governance",
	Short: "Repo-governance validation commands",
	Long:  `Commands for validating repo-governance layer conventions.`,
}

func init() {
	rootCmd.AddCommand(repoGovernanceCmd)
}
