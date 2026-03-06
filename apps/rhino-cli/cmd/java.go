package cmd

import "github.com/spf13/cobra"

var javaCmd = &cobra.Command{
	Use:   "java",
	Short: "Java validation commands",
	Long:  `Commands for validating Java source code conventions.`,
}

func init() {
	rootCmd.AddCommand(javaCmd)
}
