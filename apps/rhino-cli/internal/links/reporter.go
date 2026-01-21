package links

import (
	"encoding/json"
	"fmt"
	"sort"
	"strings"
	"time"
)

// FormatText formats the validation result as human-readable text.
func FormatText(result *ValidationResult, verbose, quiet bool) string {
	var output strings.Builder

	// If no broken links and not quiet, show success message
	if len(result.BrokenLinks) == 0 {
		if !quiet {
			output.WriteString("✓ All links valid! No broken links found.\n")
		}
		return output.String()
	}

	// Generate broken links report
	output.WriteString("# Broken Links Report\n\n")
	output.WriteString(fmt.Sprintf("**Total broken links**: %d\n", len(result.BrokenLinks)))

	// Category order for report (matches Python version)
	categoryOrder := []string{
		"Old ex-ru-* prefixes",
		"Missing files",
		"General/other paths",
		"workflows/ paths",
		"vision/ paths",
		"conventions README",
	}

	for _, category := range categoryOrder {
		links, exists := result.BrokenByCategory[category]
		if !exists || len(links) == 0 {
			continue
		}

		output.WriteString(fmt.Sprintf("\n## %s (%d links)\n", category, len(links)))

		// Group by file
		byFile := make(map[string][]BrokenLink)
		for _, link := range links {
			byFile[link.SourceFile] = append(byFile[link.SourceFile], link)
		}

		// Sort files alphabetically
		files := make([]string, 0, len(byFile))
		for file := range byFile {
			files = append(files, file)
		}
		sort.Strings(files)

		for _, file := range files {
			output.WriteString(fmt.Sprintf("\n### %s\n\n", file))
			
			// Sort links by line number
			fileLinks := byFile[file]
			sort.Slice(fileLinks, func(i, j int) bool {
				return fileLinks[i].LineNumber < fileLinks[j].LineNumber
			})

			for _, link := range fileLinks {
				output.WriteString(fmt.Sprintf("- Line %d: `%s`\n", link.LineNumber, link.LinkText))
			}
		}
	}

	return output.String()
}

// JSONOutput represents the JSON output format.
type JSONOutput struct {
	Status      string                      `json:"status"`
	Timestamp   string                      `json:"timestamp"`
	TotalFiles  int                         `json:"total_files"`
	TotalLinks  int                         `json:"total_links"`
	BrokenCount int                         `json:"broken_count"`
	DurationMS  int64                       `json:"duration_ms"`
	Categories  map[string][]JSONBrokenLink `json:"categories"`
}

// JSONBrokenLink represents a broken link in JSON format.
type JSONBrokenLink struct {
	SourceFile string `json:"source_file"`
	LineNumber int    `json:"line_number"`
	LinkText   string `json:"link_text"`
	TargetPath string `json:"target_path"`
}

// FormatJSON formats the validation result as JSON.
func FormatJSON(result *ValidationResult) (string, error) {
	status := "success"
	if len(result.BrokenLinks) > 0 {
		status = "failure"
	}

	// Build categories map
	categories := make(map[string][]JSONBrokenLink)
	for category, links := range result.BrokenByCategory {
		jsonLinks := make([]JSONBrokenLink, 0, len(links))
		for _, link := range links {
			jsonLinks = append(jsonLinks, JSONBrokenLink{
				SourceFile: link.SourceFile,
				LineNumber: link.LineNumber,
				LinkText:   link.LinkText,
				TargetPath: link.TargetPath,
			})
		}
		categories[category] = jsonLinks
	}

	output := JSONOutput{
		Status:      status,
		Timestamp:   time.Now().Format(time.RFC3339),
		TotalFiles:  result.TotalFiles,
		TotalLinks:  result.TotalLinks,
		BrokenCount: len(result.BrokenLinks),
		DurationMS:  result.ScanDuration.Milliseconds(),
		Categories:  categories,
	}

	bytes, err := json.MarshalIndent(output, "", "  ")
	if err != nil {
		return "", err
	}

	return string(bytes), nil
}

// FormatMarkdown formats the validation result as markdown (same as text).
func FormatMarkdown(result *ValidationResult) string {
	// Markdown format is identical to text format
	return FormatText(result, false, false)
}
