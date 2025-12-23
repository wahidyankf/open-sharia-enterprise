package navigation

import (
	"os"
	"path/filepath"
	"sort"
	"strings"

	"github.com/wahidyankf/open-sharia-enterprise/apps/ayokoding-cli/internal/markdown"
)

// Item represents a navigation item (file or directory)
type Item struct {
	Title      string
	Path       string
	Weight     int
	IsDir      bool
	Children   []Item
	RawContent string // For non-directory files
}

// ScanDirectory scans a directory and returns navigation items up to 2 layers deep
// basePath is the absolute URL path to this directory (e.g., "/en/learn/swe")
func ScanDirectory(dirPath string, basePath string, currentLayer int, maxLayers int) ([]Item, error) {
	if currentLayer > maxLayers {
		return nil, nil
	}

	entries, err := os.ReadDir(dirPath)
	if err != nil {
		return nil, err
	}

	var items []Item

	for _, entry := range entries {
		// Skip hidden files
		if strings.HasPrefix(entry.Name(), ".") {
			continue
		}

		fullPath := filepath.Join(dirPath, entry.Name())

		if entry.IsDir() {
			// Check if directory has _index.md
			indexPath := filepath.Join(fullPath, "_index.md")
			if _, err := os.Stat(indexPath); err != nil {
				// No _index.md, skip this directory
				continue
			}

			// Extract frontmatter from _index.md
			fm, err := markdown.ExtractFrontmatter(indexPath)
			if err != nil {
				// Skip if frontmatter extraction fails
				continue
			}

			// Build absolute URL path for this directory
			itemPath := basePath + "/" + entry.Name()

			item := Item{
				Title:  fm.Title,
				Path:   itemPath,
				Weight: fm.Weight,
				IsDir:  true,
			}

			// Recursively scan children if we haven't reached max depth
			if currentLayer < maxLayers {
				children, err := ScanDirectory(fullPath, itemPath, currentLayer+1, maxLayers)
				if err == nil {
					item.Children = children
				}
			}

			items = append(items, item)

		} else if strings.HasSuffix(entry.Name(), ".md") && entry.Name() != "_index.md" {
			// Standalone markdown file (not _index.md)
			fm, err := markdown.ExtractFrontmatter(fullPath)
			if err != nil {
				// Skip if frontmatter extraction fails
				continue
			}

			// Remove .md extension for path
			pathWithoutExt := strings.TrimSuffix(entry.Name(), ".md")

			// Build absolute URL path for this file
			itemPath := basePath + "/" + pathWithoutExt

			item := Item{
				Title:  fm.Title,
				Path:   itemPath,
				Weight: fm.Weight,
				IsDir:  false,
			}

			items = append(items, item)
		}
	}

	// Sort items by weight
	sort.Slice(items, func(i, j int) bool {
		return items[i].Weight < items[j].Weight
	})

	return items, nil
}
