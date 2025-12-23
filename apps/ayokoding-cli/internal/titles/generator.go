package titles

import (
	"path/filepath"
	"strings"
)

// GenerateTitle generates a title from a filename using the provided config
// Algorithm:
// 1. Extract filename/directory name (without extension)
//    - Special case: _index.md files use parent directory name instead
//    - Other files: strip leading underscores
// 2. Normalize: lowercase the extracted name
// 3. Exact filename override check: if entire normalized name is in overrides, use it and STOP
// 4. Split on hyphens and underscores into words
// 5. For each word:
//    - Per-word override check: if word is in overrides, use override value
//    - If not found: capitalize first letter
//    - Lowercase word check: if word is in lowercase_words list AND not first word, lowercase it
// 6. Join words with spaces
func GenerateTitle(filePath string, config *Config) string {
	// Extract filename without extension
	filename := filepath.Base(filePath)
	nameWithoutExt := strings.TrimSuffix(filename, filepath.Ext(filename))

	// Special case: _index.md files should use parent directory name
	// Example: apps/ayokoding-web/content/en/learn/_index.md → uses "learn" from parent dir
	if nameWithoutExt == "_index" {
		parentDir := filepath.Dir(filePath)
		parentName := filepath.Base(parentDir)
		nameWithoutExt = parentName
	} else {
		// Strip leading underscores for other files (e.g., _foo → foo)
		nameWithoutExt = strings.TrimPrefix(nameWithoutExt, "_")
	}

	// Trim leading/trailing hyphens
	nameWithoutExt = strings.Trim(nameWithoutExt, "-_")

	// If empty after trimming, use filename as-is
	if nameWithoutExt == "" {
		nameWithoutExt = filename
	}

	// Normalize to lowercase for comparison
	normalized := strings.ToLower(nameWithoutExt)

	// Step 3: Check for exact filename override
	if config.HasOverride(normalized) {
		return config.GetOverride(normalized)
	}

	// Step 4: Split on hyphens and underscores
	// Replace multiple consecutive separators with single space
	normalized = strings.ReplaceAll(normalized, "_", "-")
	for strings.Contains(normalized, "--") {
		normalized = strings.ReplaceAll(normalized, "--", "-")
	}
	words := strings.Split(normalized, "-")

	// Step 5: Process each word
	processedWords := make([]string, 0, len(words))
	for i, word := range words {
		// Skip empty words
		if word == "" {
			continue
		}

		// Check for per-word override
		if config.HasOverride(word) {
			processedWords = append(processedWords, config.GetOverride(word))
			continue
		}

		// Capitalize first letter
		capitalized := capitalizeFirst(word)

		// Check if should be lowercase (but never lowercase first word)
		if i > 0 && config.IsLowercaseWord(word) {
			processedWords = append(processedWords, strings.ToLower(word))
		} else {
			processedWords = append(processedWords, capitalized)
		}
	}

	// Step 6: Join with spaces
	return strings.Join(processedWords, " ")
}

// capitalizeFirst capitalizes the first letter of a string
func capitalizeFirst(s string) string {
	if s == "" {
		return ""
	}
	// Handle strings that start with non-letter characters
	runes := []rune(s)
	for i, r := range runes {
		if (r >= 'a' && r <= 'z') || (r >= 'A' && r <= 'Z') {
			// Convert first letter to uppercase, rest to lowercase
			result := string(runes[:i])
			result += strings.ToUpper(string(r))
			if i+1 < len(runes) {
				result += strings.ToLower(string(runes[i+1:]))
			}
			return result
		}
	}
	// No letters found, return as-is
	return s
}
