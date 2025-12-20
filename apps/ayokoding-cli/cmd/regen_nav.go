package cmd

import (
	"fmt"
	"path/filepath"
	"time"

	"github.com/wahidyankf/open-sharia-enterprise/apps/ayokoding-cli/internal/navigation"
)

// RegenNavCommand handles the regen-nav command
func RegenNavCommand(args []string) error {
	// Default content directory
	contentDir := "apps/ayokoding-web/content"

	// Allow custom directory path as argument
	if len(args) > 0 {
		contentDir = args[0]
	}

	// Convert to absolute path
	absPath, err := filepath.Abs(contentDir)
	if err != nil {
		return fmt.Errorf("failed to resolve path: %w", err)
	}

	fmt.Printf("Regenerating navigation for: %s\n", absPath)
	fmt.Println("---")

	startTime := time.Now()

	// Run navigation regeneration
	result, err := navigation.RegenerateNavigation(absPath)
	if err != nil {
		return fmt.Errorf("navigation regeneration failed: %w", err)
	}

	elapsed := time.Since(startTime)

	// Print results
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

	// Get current timestamp in UTC+7
	loc, _ := time.LoadLocation("Asia/Jakarta")
	timestamp := time.Now().In(loc).Format("2006-01-02T15:04:05-07:00")
	fmt.Printf("\nCompleted at: %s\n", timestamp)

	return nil
}
