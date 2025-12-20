package main

import (
	"fmt"
	"os"

	"github.com/wahidyankf/open-sharia-enterprise/apps/ayokoding-cli/cmd"
)

func main() {
	if len(os.Args) < 2 {
		printUsage()
		os.Exit(1)
	}

	command := os.Args[1]
	args := os.Args[2:]

	switch command {
	case "regen-nav":
		if err := cmd.RegenNavCommand(args); err != nil {
			fmt.Fprintf(os.Stderr, "Error: %v\n", err)
			os.Exit(1)
		}
	case "help", "-h", "--help":
		printUsage()
	case "version", "-v", "--version":
		printVersion()
	default:
		fmt.Fprintf(os.Stderr, "Unknown command: %s\n\n", command)
		printUsage()
		os.Exit(1)
	}
}

func printUsage() {
	fmt.Println("ayokoding-cli - CLI tools for ayokoding-web")
	fmt.Println()
	fmt.Println("Usage:")
	fmt.Println("  ayokoding-cli <command> [arguments]")
	fmt.Println()
	fmt.Println("Commands:")
	fmt.Println("  regen-nav [path]    Regenerate 3-layer navigation in _index.md files")
	fmt.Println("                      Default path: apps/ayokoding-web/content")
	fmt.Println("  help                Show this help message")
	fmt.Println("  version             Show version information")
	fmt.Println()
	fmt.Println("Examples:")
	fmt.Println("  ayokoding-cli regen-nav")
	fmt.Println("  ayokoding-cli regen-nav apps/ayokoding-web/content/en/learn")
}

func printVersion() {
	fmt.Println("ayokoding-cli v0.1.0")
	fmt.Println("Navigation regeneration tool for ayokoding-web Hugo site")
}
