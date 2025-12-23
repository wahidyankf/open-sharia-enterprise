package cmd

import (
	"encoding/json"
	"fmt"
	"time"

	"github.com/spf13/cobra"
	"github.com/wahidyankf/open-sharia-enterprise/apps/ayokoding-cli/internal/titles"
)

var (
	titlesLang     string
	titlesDryRun   bool
	titlesConfigEn string
	titlesConfigId string
)

var titlesUpdateCmd = &cobra.Command{
	Use:   "update",
	Short: "Update title fields in markdown files",
	Long: `Update title frontmatter fields in all markdown files based on filenames.

Generates titles from filenames using Title Case with support for:
- Custom overrides for special cases (e.g., "cliftonstrengths" → "CliftonStrengths")
- Lowercase articles/prepositions (e.g., "terms-and-conditions" → "Terms and Conditions")
- Separate configuration for English and Indonesian`,
	Example: `  ayokoding-cli titles update
  ayokoding-cli titles update --lang en
  ayokoding-cli titles update --dry-run
  ayokoding-cli titles update --verbose
  ayokoding-cli titles update -o json`,
	RunE: runTitlesUpdate,
}

func init() {
	titlesCmd.AddCommand(titlesUpdateCmd)

	titlesUpdateCmd.Flags().StringVar(&titlesLang, "lang", "both", "language to process (en/id/both)")
	titlesUpdateCmd.Flags().BoolVar(&titlesDryRun, "dry-run", false, "preview changes without writing")
	titlesUpdateCmd.Flags().StringVar(&titlesConfigEn, "config-en", "apps/ayokoding-cli/config/title-overrides-en.yaml", "path to English config")
	titlesUpdateCmd.Flags().StringVar(&titlesConfigId, "config-id", "apps/ayokoding-cli/config/title-overrides-id.yaml", "path to Indonesian config")
}

func runTitlesUpdate(cmd *cobra.Command, args []string) error {
	// Validate lang flag
	if titlesLang != "en" && titlesLang != "id" && titlesLang != "both" {
		return fmt.Errorf("invalid --lang value: %s (must be en, id, or both)", titlesLang)
	}

	// Text output header (unless quiet or JSON)
	if !quiet && output == "text" {
		fmt.Println("Updating titles in ayokoding-web content...")
		if titlesDryRun {
			fmt.Println("(DRY RUN - no files will be modified)")
		}
		fmt.Println("---")
	}

	startTime := time.Now()

	// Process titles update
	result, err := titles.UpdateTitles(titlesLang, titlesDryRun, titlesConfigEn, titlesConfigId)
	if err != nil {
		return fmt.Errorf("title update failed: %w", err)
	}

	elapsed := time.Since(startTime)

	// Output based on format
	switch output {
	case "json":
		return outputTitlesJSON(result, elapsed)
	case "markdown":
		return outputTitlesMarkdown(result, elapsed)
	default:
		return outputTitlesText(result, elapsed)
	}
}

func outputTitlesText(result *titles.UpdateResult, elapsed time.Duration) error {
	if quiet {
		return nil // Silent success
	}

	fmt.Println()
	fmt.Println("Title Update Complete")
	fmt.Println("=====================")

	if result.EnResult != nil {
		fmt.Println("English (en/):")
		fmt.Printf("  Updated:   %d files\n", result.EnResult.UpdatedCount)
		fmt.Printf("  Skipped:   %d files\n", result.EnResult.SkippedCount)
		fmt.Printf("  Errors:    %d files\n", result.EnResult.ErrorCount)
		fmt.Println()
	}

	if result.IdResult != nil {
		fmt.Println("Indonesian (id/):")
		fmt.Printf("  Updated:   %d files\n", result.IdResult.UpdatedCount)
		fmt.Printf("  Skipped:   %d files\n", result.IdResult.SkippedCount)
		fmt.Printf("  Errors:    %d files\n", result.IdResult.ErrorCount)
		fmt.Println()
	}

	fmt.Printf("Total Duration: %v\n", elapsed)

	// Show errors if any
	allErrors := []string{}
	if result.EnResult != nil {
		allErrors = append(allErrors, result.EnResult.Errors...)
	}
	if result.IdResult != nil {
		allErrors = append(allErrors, result.IdResult.Errors...)
	}

	if len(allErrors) > 0 {
		fmt.Println()
		fmt.Println("Errors:")
		for _, err := range allErrors {
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

func outputTitlesJSON(result *titles.UpdateResult, elapsed time.Duration) error {
	loc, _ := time.LoadLocation("Asia/Jakarta")
	timestamp := time.Now().In(loc).Format("2006-01-02T15:04:05-07:00")

	jsonOutput := map[string]interface{}{
		"status":      "success",
		"timestamp":   timestamp,
		"duration_ms": elapsed.Milliseconds(),
	}

	if result.EnResult != nil {
		jsonOutput["en"] = map[string]interface{}{
			"updated": result.EnResult.UpdatedCount,
			"skipped": result.EnResult.SkippedCount,
			"errors":  result.EnResult.ErrorCount,
		}
	}

	if result.IdResult != nil {
		jsonOutput["id"] = map[string]interface{}{
			"updated": result.IdResult.UpdatedCount,
			"skipped": result.IdResult.SkippedCount,
			"errors":  result.IdResult.ErrorCount,
		}
	}

	// Collect all errors
	allErrors := []string{}
	if result.EnResult != nil {
		allErrors = append(allErrors, result.EnResult.Errors...)
	}
	if result.IdResult != nil {
		allErrors = append(allErrors, result.IdResult.Errors...)
	}
	jsonOutput["errors"] = allErrors

	data, err := json.MarshalIndent(jsonOutput, "", "  ")
	if err != nil {
		return fmt.Errorf("failed to marshal JSON: %w", err)
	}

	fmt.Println(string(data))
	return nil
}

func outputTitlesMarkdown(result *titles.UpdateResult, elapsed time.Duration) error {
	loc, _ := time.LoadLocation("Asia/Jakarta")
	timestamp := time.Now().In(loc).Format("2006-01-02T15:04:05-07:00")

	fmt.Println("# Title Update Report")
	fmt.Println()
	fmt.Printf("**Timestamp**: %s\n", timestamp)
	fmt.Printf("**Duration**: %v\n", elapsed)
	fmt.Println()
	fmt.Println("## Summary")
	fmt.Println()

	if result.EnResult != nil {
		fmt.Println("### English (en/)")
		fmt.Println()
		fmt.Printf("- Updated: %d files\n", result.EnResult.UpdatedCount)
		fmt.Printf("- Skipped: %d files\n", result.EnResult.SkippedCount)
		fmt.Printf("- Errors: %d files\n", result.EnResult.ErrorCount)
		fmt.Println()
	}

	if result.IdResult != nil {
		fmt.Println("### Indonesian (id/)")
		fmt.Println()
		fmt.Printf("- Updated: %d files\n", result.IdResult.UpdatedCount)
		fmt.Printf("- Skipped: %d files\n", result.IdResult.SkippedCount)
		fmt.Printf("- Errors: %d files\n", result.IdResult.ErrorCount)
		fmt.Println()
	}

	// Show errors if any
	allErrors := []string{}
	if result.EnResult != nil {
		allErrors = append(allErrors, result.EnResult.Errors...)
	}
	if result.IdResult != nil {
		allErrors = append(allErrors, result.IdResult.Errors...)
	}

	if len(allErrors) > 0 {
		fmt.Println("## Errors")
		fmt.Println()
		for _, err := range allErrors {
			fmt.Printf("- %s\n", err)
		}
	}

	return nil
}
