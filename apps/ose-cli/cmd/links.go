package cmd

import "github.com/spf13/cobra"

var linksCmd = &cobra.Command{
	Use:   "links",
	Short: "Link management commands for ose-web content",
	Long:  `Commands for validating links in ose-web markdown files.`,
}

func init() {
	rootCmd.AddCommand(linksCmd)
}
