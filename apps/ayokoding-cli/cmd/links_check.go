package cmd

import (
	"encoding/json"
	"fmt"
	"time"

	"github.com/spf13/cobra"
	"github.com/wahidyankf/open-sharia-enterprise/apps/ayokoding-cli/internal/links"
)

var linksContentDir string

var linksCheckCmd = &cobra.Command{
	Use:   "check",
	Short: "Validate internal links in ayokoding-web content",
	Long: `Validate all internal links in ayokoding-web markdown content.

Walks all .md files in the content directory, extracts internal links, and
checks that each link resolves to a real file on disk. Internal links use
Hugo absolute paths (/en/... or /id/...) without .md extension.

External links (http://, https://, mailto://) are skipped — use the
apps-ayokoding-web-link-checker AI agent for external link validation.`,
	Example: `  ayokoding-cli links check
  ayokoding-cli links check --content apps/ayokoding-web/content
  ayokoding-cli links check -o json`,
	RunE: runLinksCheck,
}

func init() {
	linksCmd.AddCommand(linksCheckCmd)
	linksCheckCmd.Flags().StringVar(&linksContentDir, "content", "apps/ayokoding-web/content", "content directory path")
}

func jakartaTimestamp() string {
	loc, _ := time.LoadLocation("Asia/Jakarta")
	return time.Now().In(loc).Format("2006-01-02T15:04:05-07:00")
}

func runLinksCheck(_ *cobra.Command, _ []string) error {
	if !quiet && output == "text" {
		fmt.Printf("Checking internal links in: %s\n", linksContentDir)
		fmt.Println("---")
	}

	startTime := time.Now()

	result, err := links.CheckLinks(linksContentDir)
	if err != nil {
		return fmt.Errorf("link check failed: %w", err)
	}

	elapsed := time.Since(startTime)

	var outputErr error
	switch output {
	case "json":
		outputErr = outputLinksJSON(result, elapsed)
	case "markdown":
		outputLinksMarkdown(result, elapsed)
	default:
		outputLinksText(result, elapsed)
	}
	if outputErr != nil {
		return outputErr
	}

	if len(result.BrokenLinks) > 0 {
		return fmt.Errorf("%d broken link(s) found", len(result.BrokenLinks))
	}

	return nil
}

func outputLinksText(result *links.CheckResult, elapsed time.Duration) {
	if quiet {
		return
	}

	fmt.Println()
	fmt.Println("Link Check Complete")
	fmt.Println("===================")
	fmt.Printf("Checked:  %d link(s)\n", result.CheckedCount)
	fmt.Printf("Broken:   %d link(s)\n", len(result.BrokenLinks))
	fmt.Printf("Errors:   %d\n", result.ErrorCount)
	fmt.Printf("Duration: %v\n", elapsed)

	if len(result.Errors) > 0 {
		fmt.Println()
		fmt.Println("Errors:")
		for _, e := range result.Errors {
			fmt.Printf("  - %s\n", e)
		}
	}

	if len(result.BrokenLinks) > 0 {
		fmt.Println()
		fmt.Println("Broken Links:")
		fmt.Printf("  %-60s %5s  %-30s  %s\n", "Source File", "Line", "Text", "Target")
		fmt.Printf("  %-60s %5s  %-30s  %s\n", "---", "---", "---", "---")
		for _, bl := range result.BrokenLinks {
			fmt.Printf("  %-60s %5d  %-30s  %s\n", bl.SourceFile, bl.Line, bl.Text, bl.Target)
		}
	}

	if verbose {
		fmt.Printf("\nCompleted at: %s\n", jakartaTimestamp())
	}
}

func outputLinksJSON(result *links.CheckResult, elapsed time.Duration) error {
	status := "success"
	if len(result.BrokenLinks) > 0 {
		status = "failure"
	}

	jsonOutput := map[string]any{
		"status":       status,
		"timestamp":    jakartaTimestamp(),
		"duration_ms":  elapsed.Milliseconds(),
		"checked":      result.CheckedCount,
		"broken":       len(result.BrokenLinks),
		"errors":       result.Errors,
		"broken_links": result.BrokenLinks,
	}

	data, err := json.MarshalIndent(jsonOutput, "", "  ")
	if err != nil {
		return fmt.Errorf("failed to marshal JSON: %w", err)
	}

	fmt.Println(string(data))
	return nil
}

func outputLinksMarkdown(result *links.CheckResult, elapsed time.Duration) {
	status := "PASS"
	if len(result.BrokenLinks) > 0 {
		status = "FAIL"
	}

	fmt.Println("# Link Check Report")
	fmt.Println()
	fmt.Printf("**Timestamp**: %s\n", jakartaTimestamp())
	fmt.Printf("**Duration**: %v\n", elapsed)
	fmt.Printf("**Status**: %s\n", status)
	fmt.Println()
	fmt.Println("## Summary")
	fmt.Println()
	fmt.Printf("| Metric | Count |\n")
	fmt.Printf("| --- | --- |\n")
	fmt.Printf("| Checked | %d |\n", result.CheckedCount)
	fmt.Printf("| Broken | %d |\n", len(result.BrokenLinks))
	fmt.Printf("| Errors | %d |\n", result.ErrorCount)

	if len(result.Errors) > 0 {
		fmt.Println()
		fmt.Println("## Errors")
		fmt.Println()
		for _, e := range result.Errors {
			fmt.Printf("- %s\n", e)
		}
	}

	if len(result.BrokenLinks) > 0 {
		fmt.Println()
		fmt.Println("## Broken Links")
		fmt.Println()
		fmt.Printf("| Source File | Line | Text | Target |\n")
		fmt.Printf("| --- | --- | --- | --- |\n")
		for _, bl := range result.BrokenLinks {
			fmt.Printf("| %s | %d | %s | %s |\n", bl.SourceFile, bl.Line, bl.Text, bl.Target)
		}
	}
}
