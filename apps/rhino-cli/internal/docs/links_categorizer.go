package docs

import "strings"

// CategorizeBrokenLink categorizes a broken link by pattern.
func CategorizeBrokenLink(link string) string {
	// Check patterns in order (most specific first)

	// workflows/ paths (but not repo-governance/workflows/)
	if strings.Contains(link, "workflows/") && !strings.Contains(link, "repo-governance/workflows/") {
		return "workflows/ paths"
	}

	// vision/ paths (but not repo-governance/vision/)
	if strings.Contains(link, "vision/") && !strings.Contains(link, "repo-governance/vision/") {
		return "vision/ paths"
	}

	// conventions README
	if strings.Contains(link, "conventions/README.md") {
		return "conventions README"
	}

	// Missing files
	if link == "CODE_OF_CONDUCT.md" || link == "CHANGELOG.md" {
		return "Missing files"
	}

	// Default category
	return "General/other paths"
}
