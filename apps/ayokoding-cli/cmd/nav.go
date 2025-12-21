package cmd

import "github.com/spf13/cobra"

var navCmd = &cobra.Command{
	Use:   "nav",
	Short: "Navigation management commands",
	Long:  `Commands for regenerating and validating navigation structure in ayokoding-web.`,
}

func init() {
	rootCmd.AddCommand(navCmd)
}
