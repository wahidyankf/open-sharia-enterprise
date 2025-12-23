package cmd

import "github.com/spf13/cobra"

var titlesCmd = &cobra.Command{
	Use:   "titles",
	Short: "Title management commands",
	Long:  `Commands for managing title fields in ayokoding-web markdown files.`,
}

func init() {
	rootCmd.AddCommand(titlesCmd)
}
