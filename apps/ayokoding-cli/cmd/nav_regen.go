package cmd

import (
	"encoding/json"
	"fmt"
	"path/filepath"
	"time"

	"github.com/spf13/cobra"
	"github.com/wahidyankf/open-sharia-enterprise/apps/ayokoding-cli/internal/navigation"
)

var (
	regenPath    string
	regenExclude []string
)

var navRegenCmd = &cobra.Command{
	Use:   "regen [path]",
	Short: "Regenerate 3-layer navigation in _index.md files",
	Long: `Regenerate 3-layer navigation listings in all _index.md files.

Scans all _index.md files (excluding root language files) and regenerates
navigation trees based on file structure 3 layers deep. Items are sorted
by weight within each level.`,
	Example: `  ayokoding-cli nav regen
  ayokoding-cli nav regen --path apps/ayokoding-web/content/en/learn
  ayokoding-cli nav regen --verbose
  ayokoding-cli nav regen -o json`,
	Args: cobra.MaximumNArgs(1),
	RunE: runNavRegen,
}

func init() {
	navCmd.AddCommand(navRegenCmd)

	navRegenCmd.Flags().StringVarP(&regenPath, "path", "p", "", "content directory (default: apps/ayokoding-web/content)")
	navRegenCmd.Flags().StringSliceVar(&regenExclude, "exclude", []string{"en/_index.md", "id/_index.md"}, "files to exclude")
}

func runNavRegen(cmd *cobra.Command, args []string) error {
	// Priority: flag > positional arg > default
	contentDir := "apps/ayokoding-web/content"
	if regenPath != "" {
		contentDir = regenPath
	} else if len(args) > 0 {
		contentDir = args[0]
	}

	// Convert to absolute path
	absPath, err := filepath.Abs(contentDir)
	if err != nil {
		return fmt.Errorf("failed to resolve path: %w", err)
	}

	// Text output header (unless quiet or JSON)
	if !quiet && output == "text" {
		fmt.Printf("Regenerating navigation for: %s\n", absPath)
		fmt.Println("---")
	}

	startTime := time.Now()

	result, err := navigation.RegenerateNavigation(absPath)
	if err != nil {
		return fmt.Errorf("navigation regeneration failed: %w", err)
	}

	elapsed := time.Since(startTime)

	// Output based on format
	switch output {
	case "json":
		return outputJSON(result, elapsed)
	case "markdown":
		return outputMarkdown(result, elapsed, absPath)
	default:
		return outputText(result, elapsed)
	}
}

func outputText(result *navigation.RegenerateResult, elapsed time.Duration) error {
	if quiet {
		return nil // Silent success
	}

	fmt.Println()
	fmt.Println("Navigation Regeneration Complete")
	fmt.Println("=================================")
	fmt.Printf("Processed: %d files\n", result.ProcessedCount)
	fmt.Printf("Skipped:   %d files\n", result.SkippedCount)
	fmt.Printf("Errors:    %d files\n", result.ErrorCount)
	fmt.Printf("Duration:  %v\n", elapsed)

	if len(result.Errors) > 0 {
		fmt.Println()
		fmt.Println("Errors:")
		for _, err := range result.Errors {
			fmt.Printf("  - %s\n", err)
		}
	}

	if verbose {
		loc, _ := time.LoadLocation("Asia/Jakarta")
		timestamp := time.Now().In(loc).Format("2006-01-02T15:04:05-07:00")
		fmt.Printf("\nCompleted at: %s\n", timestamp)
	}

	return nil
}

func outputJSON(result *navigation.RegenerateResult, elapsed time.Duration) error {
	loc, _ := time.LoadLocation("Asia/Jakarta")
	timestamp := time.Now().In(loc).Format("2006-01-02T15:04:05-07:00")

	jsonOutput := map[string]interface{}{
		"status":      "success",
		"timestamp":   timestamp,
		"duration_ms": elapsed.Milliseconds(),
		"stats": map[string]int{
			"processed": result.ProcessedCount,
			"skipped":   result.SkippedCount,
			"errors":    result.ErrorCount,
		},
		"errors": result.Errors,
	}

	data, err := json.MarshalIndent(jsonOutput, "", "  ")
	if err != nil {
		return fmt.Errorf("failed to marshal JSON: %w", err)
	}

	fmt.Println(string(data))
	return nil
}

func outputMarkdown(result *navigation.RegenerateResult, elapsed time.Duration, path string) error {
	loc, _ := time.LoadLocation("Asia/Jakarta")
	timestamp := time.Now().In(loc).Format("2006-01-02T15:04:05-07:00")

	fmt.Println("# Navigation Regeneration Report")
	fmt.Println()
	fmt.Printf("**Path**: %s\n", path)
	fmt.Printf("**Timestamp**: %s\n", timestamp)
	fmt.Printf("**Duration**: %v\n", elapsed)
	fmt.Println()
	fmt.Println("## Summary")
	fmt.Println()
	fmt.Printf("- Processed: %d files\n", result.ProcessedCount)
	fmt.Printf("- Skipped: %d files\n", result.SkippedCount)
	fmt.Printf("- Errors: %d files\n", result.ErrorCount)

	if len(result.Errors) > 0 {
		fmt.Println()
		fmt.Println("## Errors")
		fmt.Println()
		for _, err := range result.Errors {
			fmt.Printf("- %s\n", err)
		}
	}

	return nil
}
