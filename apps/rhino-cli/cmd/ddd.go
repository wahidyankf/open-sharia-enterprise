package cmd

import "github.com/spf13/cobra"

var dddCmd = &cobra.Command{
	Use:   "ddd",
	Short: "Domain-driven design commands",
	Long:  `Commands for validating DDD bounded-context structure and ubiquitous-language glossaries.`,
}

func init() {
	rootCmd.AddCommand(dddCmd)
}
