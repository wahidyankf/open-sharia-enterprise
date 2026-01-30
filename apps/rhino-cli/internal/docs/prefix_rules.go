package docs

import (
	"path/filepath"
	"strings"
)

// rootPrefixes maps root Diátaxis directory names to their prefixes.
var rootPrefixes = map[string]string{
	"tutorials":   "tu",
	"how-to":      "hoto",
	"reference":   "re",
	"explanation": "ex",
}

// GenerateExpectedPrefix returns the expected prefix for a file at the given relative path.
// It also returns whether the file is exempt from prefix requirements.
func GenerateExpectedPrefix(relativePath string) (prefix string, isExempt bool) {
	// Check if the file is exempt first
	if isException(relativePath) {
		return "", true
	}

	// Parse the path to extract directory hierarchy
	// Expected format: docs/[root-category]/[optional-subdirs]/filename.md
	parts := strings.Split(relativePath, string(filepath.Separator))
	if len(parts) < 3 {
		// Not deep enough to have a prefix requirement
		return "", true
	}

	// Verify it's under docs/
	if parts[0] != "docs" {
		return "", true
	}

	// Get the root category (tutorials, how-to, reference, explanation)
	rootCategory := parts[1]
	rootPrefix, ok := rootPrefixes[rootCategory]
	if !ok {
		// Unknown root category - no prefix requirement
		return "", true
	}

	// For files directly under the root category (e.g., docs/tutorials/file.md)
	if len(parts) == 3 {
		return rootPrefix + "__", false
	}

	// For files in subdirectories, build the hierarchical prefix
	// docs/explanation/software/prog-lang/python/file.md
	// -> ex-so-prla-py__
	subdirs := parts[2 : len(parts)-1] // All directories between root and filename
	prefixParts := []string{rootPrefix}
	for _, subdir := range subdirs {
		prefixParts = append(prefixParts, encodeSubdirectory(subdir))
	}

	return strings.Join(prefixParts, "-") + "__", false
}

// encodeSubdirectory encodes a subdirectory name into a prefix component.
// Rules:
// - Single words: first 2 characters (e.g., "software" -> "so")
// - Hyphenated compounds: first 2 characters of each word concatenated (e.g., "prog-lang" -> "prla")
func encodeSubdirectory(dirName string) string {
	// Check if it's a hyphenated compound
	if strings.Contains(dirName, "-") {
		words := strings.Split(dirName, "-")

		// Standard encoding: first 2 chars of each word
		var encoded strings.Builder
		for _, word := range words {
			if len(word) >= 2 {
				encoded.WriteString(word[:2])
			} else if len(word) == 1 {
				encoded.WriteString(word)
			}
		}
		return encoded.String()
	}

	// Single word: take first 2 characters
	if len(dirName) >= 2 {
		return dirName[:2]
	}
	return dirName
}

// isException checks if a file path is exempt from the prefix requirement.
// Exceptions:
// - README.md files (index files)
// - Files in docs/metadata/ directory
func isException(relativePath string) bool {
	// Get the filename
	filename := filepath.Base(relativePath)

	// README.md files are exempt
	if filename == "README.md" {
		return true
	}

	// Files in docs/metadata/ are exempt
	parts := strings.Split(relativePath, string(filepath.Separator))
	if len(parts) >= 2 && parts[0] == "docs" && parts[1] == "metadata" {
		return true
	}

	return false
}

// ExtractPrefix extracts the prefix from a filename if present.
// Returns the prefix (including __) and the content identifier.
// If no prefix is found, returns empty strings.
func ExtractPrefix(filename string) (prefix string, contentID string) {
	idx := strings.Index(filename, "__")
	if idx == -1 {
		return "", filename
	}

	// Remove extension for content ID
	contentWithExt := filename[idx+2:]
	ext := filepath.Ext(contentWithExt)
	contentID = strings.TrimSuffix(contentWithExt, ext)

	return filename[:idx+2], contentID
}
