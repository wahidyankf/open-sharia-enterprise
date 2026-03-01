// Package cmd implements the CLI commands for oseplatform-cli.
package cmd

import (
	"fmt"
	"os"

	"github.com/spf13/cobra"
)

var (
	// Global flags
	verbose bool
	quiet   bool
	output  string
	noColor bool
)

var rootCmd = &cobra.Command{
	Use:   "oseplatform-cli",
	Short: "CLI tools for oseplatform-web Hugo site",
	Long: `Command-line tools for oseplatform-web Hugo site maintenance and automation.

Provides link validation with support for multiple output formats
and verbose logging.`,
	Version: "0.1.0",
}

// Execute adds all child commands to the root command and sets flags appropriately.
func Execute() {
	if err := rootCmd.Execute(); err != nil {
		fmt.Fprintf(os.Stderr, "Error: %v\n", err)
		os.Exit(1)
	}
}

func init() {
	// Global flags available to all commands
	rootCmd.PersistentFlags().BoolVarP(&verbose, "verbose", "v", false, "verbose output with timestamps")
	rootCmd.PersistentFlags().BoolVarP(&quiet, "quiet", "q", false, "quiet mode (errors only)")
	rootCmd.PersistentFlags().StringVarP(&output, "output", "o", "text", "output format: text, json, markdown")
	rootCmd.PersistentFlags().BoolVar(&noColor, "no-color", false, "disable colored output")
}
