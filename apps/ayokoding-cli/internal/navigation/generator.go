package navigation

import (
	"fmt"
	"strings"
)

// GenerateMarkdown creates a DFS navigation tree in markdown format
// Layer 1: no indentation
// Layer 2: 2 spaces
// Layer 3: 4 spaces
func GenerateMarkdown(items []Item, currentLayer int) string {
	var sb strings.Builder

	indentation := strings.Repeat("  ", currentLayer)

	for _, item := range items {
		// Generate the navigation link
		sb.WriteString(fmt.Sprintf("%s- [%s](%s)\n", indentation, item.Title, item.Path))

		// If this item has children, recursively generate their navigation
		if item.IsDir && len(item.Children) > 0 {
			// Update children paths to be relative to current item
			for i := range item.Children {
				item.Children[i].Path = item.Path + "/" + item.Children[i].Path
			}
			sb.WriteString(GenerateMarkdown(item.Children, currentLayer+1))
		}
	}

	return sb.String()
}

// GenerateNavigationContent creates the complete content for an _index.md file
// Combines frontmatter with generated navigation
func GenerateNavigationContent(frontmatter string, items []Item) string {
	var sb strings.Builder

	// Write frontmatter
	sb.WriteString(frontmatter)
	sb.WriteString("\n")

	// Write navigation
	navigationMd := GenerateMarkdown(items, 0)
	sb.WriteString(navigationMd)

	return sb.String()
}
