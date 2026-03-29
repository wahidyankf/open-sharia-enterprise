package cmd

import "github.com/spf13/cobra"

var linksCmd = &cobra.Command{
	Use:   "links",
	Short: "Link management commands for oseplatform-fs content",
	Long:  `Commands for validating links in oseplatform-fs markdown files.`,
}

func init() {
	rootCmd.AddCommand(linksCmd)
}
