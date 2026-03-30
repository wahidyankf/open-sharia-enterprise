package cmd

import "github.com/spf13/cobra"

var envCmd = &cobra.Command{
	Use:   "env",
	Short: "Environment file backup and restore commands",
	Long:  `Commands for backing up and restoring .env files across the repository.`,
}

func init() {
	rootCmd.AddCommand(envCmd)
}
