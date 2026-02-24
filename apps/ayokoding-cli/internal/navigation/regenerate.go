package navigation

import (
	"fmt"
	"os"
	"path/filepath"
	"strings"

	"github.com/wahidyankf/open-sharia-enterprise/apps/ayokoding-cli/internal/markdown"
)

// RegenerateResult holds the results of navigation regeneration
type RegenerateResult struct {
	ProcessedCount int
	SkippedCount   int
	ErrorCount     int
	Errors         []string
}

// RegenerateNavigation regenerates navigation for all _index.md files in the content directory
func RegenerateNavigation(contentDir string) (*RegenerateResult, error) {
	result := &RegenerateResult{
		Errors: []string{},
	}

	// Verify content directory exists
	if _, err := os.Stat(contentDir); os.IsNotExist(err) {
		return nil, fmt.Errorf("content directory not found: %s", contentDir)
	}

	// Find the actual content root directory
	// If contentDir is a subdirectory like content/en/learn/swe, we need to find the actual root
	contentRoot := findContentRoot(contentDir)

	// Find all _index.md files within contentDir (not the root)
	indexFiles, err := findIndexFiles(contentDir)
	if err != nil {
		return nil, fmt.Errorf("failed to find index files: %w", err)
	}

	if len(indexFiles) == 0 {
		return nil, fmt.Errorf("no _index.md files found to process")
	}

	// Process each _index.md file
	for _, indexFile := range indexFiles {
		if err := processIndexFile(indexFile, contentRoot); err != nil {
			result.ErrorCount++
			result.Errors = append(result.Errors, fmt.Sprintf("%s: %v", indexFile, err))
		} else {
			result.ProcessedCount++
		}
	}

	return result, nil
}

// findContentRoot finds the actual ayokoding-web content directory
// Returns the path that contains "en" and "id" subdirectories
func findContentRoot(path string) string {
	// Check if this path or any parent contains "content" directory
	current := path
	for {
		base := filepath.Base(current)
		if base == "content" {
			return current
		}

		parent := filepath.Dir(current)
		if parent == current {
			// Reached root, return original path
			return path
		}
		current = parent
	}
}

// findIndexFiles finds all _index.md files within a specific directory
// Excludes root language index files only if we're processing the full content directory
func findIndexFiles(contentDir string) ([]string, error) {
	var indexFiles []string

	err := filepath.Walk(contentDir, func(path string, info os.FileInfo, err error) error {
		if err != nil {
			return err
		}

		if !info.IsDir() && info.Name() == "_index.md" {
			// Exclude root language index files
			// e.g., content/en/_index.md, content/id/_index.md
			relPath, _ := filepath.Rel(contentDir, path)
			parts := strings.Split(relPath, string(os.PathSeparator))

			// If it's directly under a language directory (2 parts: "en", "_index.md")
			if len(parts) == 2 && parts[1] == "_index.md" {
				return nil // Skip root language index
			}

			indexFiles = append(indexFiles, path)
		}

		return nil
	})

	return indexFiles, err
}

// processIndexFile processes a single _index.md file
func processIndexFile(indexPath string, contentRoot string) error {
	// Extract frontmatter
	fm, err := markdown.ExtractFrontmatter(indexPath)
	if err != nil {
		return fmt.Errorf("failed to extract frontmatter: %w", err)
	}

	// Get parent directory
	parentDir := filepath.Dir(indexPath)

	// Calculate base URL path from content root
	// Example: contentRoot = /content, parentDir = /content/en/learn/swe
	// Result should be: /en/learn/swe
	relPath, err := filepath.Rel(contentRoot, parentDir)
	if err != nil {
		return fmt.Errorf("failed to calculate relative path: %w", err)
	}

	// Convert filesystem path to URL path (replace backslashes with forward slashes on Windows)
	// Handle edge case where relPath is "." (same directory as contentRoot)
	var basePath string
	if relPath == "." {
		basePath = ""
	} else {
		basePath = "/" + filepath.ToSlash(relPath)
	}

	// Scan directory structure (2 layers deep, starting from layer 1)
	items, err := ScanDirectory(parentDir, basePath, 1, 2)
	if err != nil {
		return fmt.Errorf("failed to scan directory: %w", err)
	}

	// Generate new content
	newContent := GenerateNavigationContent(fm.Raw, items)

	// Write back to file
	if err := os.WriteFile(indexPath, []byte(newContent), 0644); err != nil {
		return fmt.Errorf("failed to write file: %w", err)
	}

	return nil
}
