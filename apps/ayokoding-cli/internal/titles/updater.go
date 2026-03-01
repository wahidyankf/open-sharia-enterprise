// Package titles provides utilities for updating title frontmatter in ayokoding-web markdown files.
package titles

import (
	"fmt"
	"os"
	"path/filepath"
	"regexp"
	"strings"

	"github.com/wahidyankf/open-sharia-enterprise/apps/ayokoding-cli/internal/markdown"
)

// LangResult holds the result of processing one language
type LangResult struct {
	UpdatedCount int
	SkippedCount int
	ErrorCount   int
	Errors       []string
}

// UpdateResult holds the combined result of title updates
type UpdateResult struct {
	EnResult *LangResult
	IDResult *LangResult
}

// UpdateTitles updates titles in markdown files based on language selection
func UpdateTitles(lang string, dryRun bool, configEnPath string, configIDPath string) (*UpdateResult, error) {
	result := &UpdateResult{}

	// Process English if requested
	if lang == "en" || lang == "both" {
		config, err := LoadConfig(configEnPath)
		if err != nil {
			return nil, fmt.Errorf("failed to load English config: %w", err)
		}

		enResult, err := processLanguageDirectory("apps/ayokoding-web/content/en", config, dryRun)
		if err != nil {
			return nil, fmt.Errorf("failed to process English directory: %w", err)
		}
		result.EnResult = enResult
	}

	// Process Indonesian if requested
	if lang == "id" || lang == "both" {
		config, err := LoadConfig(configIDPath)
		if err != nil {
			return nil, fmt.Errorf("failed to load Indonesian config: %w", err)
		}

		idResult, err := processLanguageDirectory("apps/ayokoding-web/content/id", config, dryRun)
		if err != nil {
			return nil, fmt.Errorf("failed to process Indonesian directory: %w", err)
		}
		result.IDResult = idResult
	}

	return result, nil
}

// processLanguageDirectory processes all markdown files in a directory
func processLanguageDirectory(dirPath string, config *Config, dryRun bool) (*LangResult, error) {
	result := &LangResult{
		Errors: []string{},
	}

	// Convert to absolute path
	absPath, err := filepath.Abs(dirPath)
	if err != nil {
		return nil, fmt.Errorf("failed to resolve path: %w", err)
	}

	// Check if directory exists
	if _, err := os.Stat(absPath); os.IsNotExist(err) {
		return nil, fmt.Errorf("directory does not exist: %s", absPath)
	}

	// Walk the directory tree
	err = filepath.Walk(absPath, func(path string, info os.FileInfo, err error) error {
		if err != nil {
			result.Errors = append(result.Errors, fmt.Sprintf("%s: walk error: %v", path, err))
			result.ErrorCount++
			return nil // Continue walking
		}

		// Skip directories
		if info.IsDir() {
			return nil
		}

		// Only process .md files
		if filepath.Ext(path) != ".md" {
			return nil
		}

		// Process the file
		if err := processFile(path, config, dryRun, result); err != nil {
			result.Errors = append(result.Errors, fmt.Sprintf("%s: %v", path, err))
			result.ErrorCount++
		}

		return nil
	})

	if err != nil {
		return nil, fmt.Errorf("failed to walk directory: %w", err)
	}

	return result, nil
}

// processFile processes a single markdown file
func processFile(filePath string, config *Config, dryRun bool, result *LangResult) error {
	// Extract current frontmatter
	fm, err := markdown.ExtractFrontmatter(filePath)
	if err != nil {
		return fmt.Errorf("failed to extract frontmatter: %w", err)
	}

	// Generate expected title from filename
	expectedTitle := GenerateTitle(filePath, config)

	// Compare current vs expected title
	if fm.Title == expectedTitle {
		// Title is already correct, skip
		result.SkippedCount++
		return nil
	}

	// Title needs updating
	if dryRun {
		// Dry run: just count, don't actually update
		result.UpdatedCount++
		return nil
	}

	// Update the title in the file
	if err := updateTitleInFile(filePath, expectedTitle); err != nil {
		return fmt.Errorf("failed to update title: %w", err)
	}

	result.UpdatedCount++
	return nil
}

// updateTitleInFile updates the title field in a markdown file's frontmatter
func updateTitleInFile(filePath string, newTitle string) error {
	// Read entire file content
	content, err := os.ReadFile(filePath)
	if err != nil {
		return fmt.Errorf("failed to read file: %w", err)
	}

	// Convert to string for manipulation
	fileContent := string(content)

	// Find frontmatter boundaries
	lines := strings.Split(fileContent, "\n")
	if len(lines) < 3 || lines[0] != "---" {
		return fmt.Errorf("invalid frontmatter format")
	}

	// Find closing delimiter
	endIdx := -1
	for i := 1; i < len(lines); i++ {
		if lines[i] == "---" {
			endIdx = i
			break
		}
	}

	if endIdx == -1 {
		return fmt.Errorf("frontmatter closing delimiter not found")
	}

	// Update title line using regex
	// Match: title: "value" or title: 'value' or title: value
	titleRegex := regexp.MustCompile(`(?m)^title:\s*["']?.*["']?$`)

	// Build new title line
	// Always quote the title to handle special characters
	newTitleLine := fmt.Sprintf(`title: "%s"`, escapeQuotes(newTitle))

	// Replace in content
	updatedContent := titleRegex.ReplaceAllString(fileContent, newTitleLine)

	// Write back to file
	if err := os.WriteFile(filePath, []byte(updatedContent), 0644); err != nil {
		return fmt.Errorf("failed to write file: %w", err)
	}

	return nil
}

// escapeQuotes escapes double quotes in a string for YAML
func escapeQuotes(s string) string {
	// Replace " with \"
	return strings.ReplaceAll(s, `"`, `\"`)
}
