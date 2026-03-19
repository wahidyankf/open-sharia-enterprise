package cmd

import "github.com/spf13/cobra"

var contractsCmd = &cobra.Command{
	Use:   "contracts",
	Short: "Contract codegen post-processing commands",
	Long:  `Commands for post-processing generated API contract code.`,
}

func init() {
	rootCmd.AddCommand(contractsCmd)
}
