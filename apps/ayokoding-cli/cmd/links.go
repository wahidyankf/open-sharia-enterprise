package cmd

import "github.com/spf13/cobra"

var linksCmd = &cobra.Command{
	Use:   "links",
	Short: "Link management commands for ayokoding-web content",
	Long:  `Commands for validating links in ayokoding-web markdown files.`,
}

func init() {
	rootCmd.AddCommand(linksCmd)
}
