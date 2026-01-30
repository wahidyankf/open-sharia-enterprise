package docs

import (
	"fmt"
	"path/filepath"
	"regexp"
	"strings"
	"time"
)

var (
	// kebabCaseRegex matches valid kebab-case strings
	// Must be lowercase letters, numbers, hyphens, and dots only
	// Cannot start or end with hyphen, no consecutive hyphens
	// Dots allowed for version numbers (e.g., release-1.12)
	kebabCaseRegex = regexp.MustCompile(`^[a-z0-9]+(-[a-z0-9.]+)*$`)

	// prefixSeparator is the separator between prefix and content identifier
	prefixSeparator = "__"
)

// ValidateAll validates all documentation files based on options.
func ValidateAll(opts ValidationOptions) (*ValidationResult, error) {
	startTime := time.Now()

	files, err := GetDocsFiles(opts)
	if err != nil {
		return nil, err
	}

	result := &ValidationResult{
		TotalFiles:       len(files),
		ValidFiles:       0,
		ViolationCount:   0,
		Violations:       []NamingViolation{},
		ViolationsByType: make(map[ViolationType][]NamingViolation),
	}

	for _, filePath := range files {
		relPath, err := GetRelativePath(filePath, opts.RepoRoot)
		if err != nil {
			relPath = filePath
		}

		violations := validateFileName(relPath)
		if len(violations) == 0 {
			result.ValidFiles++
		} else {
			result.ViolationCount += len(violations)
			for _, v := range violations {
				result.Violations = append(result.Violations, v)
				result.ViolationsByType[v.ViolationType] = append(
					result.ViolationsByType[v.ViolationType],
					v,
				)
			}
		}
	}

	result.ScanDuration = time.Since(startTime)
	return result, nil
}

// validateFileName validates a single file's naming convention.
func validateFileName(relativePath string) []NamingViolation {
	var violations []NamingViolation
	filename := filepath.Base(relativePath)

	// Get expected prefix for this location
	expectedPrefix, isExempt := GenerateExpectedPrefix(relativePath)
	if isExempt {
		// File is exempt from naming requirements
		return violations
	}

	// Check for __ separator
	separatorIdx := strings.Index(filename, prefixSeparator)
	if separatorIdx == -1 {
		violations = append(violations, NamingViolation{
			FilePath:       relativePath,
			FileName:       filename,
			ViolationType:  ViolationMissingSeparator,
			ExpectedPrefix: expectedPrefix,
			ActualPrefix:   "",
			Message:        fmt.Sprintf("Missing '__' separator. Expected prefix: %s", expectedPrefix),
		})
		return violations
	}

	// Extract actual prefix
	actualPrefix := filename[:separatorIdx+2]

	// Check if prefix matches expected
	if actualPrefix != expectedPrefix {
		violations = append(violations, NamingViolation{
			FilePath:       relativePath,
			FileName:       filename,
			ViolationType:  ViolationWrongPrefix,
			ExpectedPrefix: expectedPrefix,
			ActualPrefix:   actualPrefix,
			Message:        fmt.Sprintf("Wrong prefix '%s', expected '%s'", actualPrefix, expectedPrefix),
		})
	}

	// Check kebab-case for content identifier (part after __)
	contentWithExt := filename[separatorIdx+2:]
	ext := filepath.Ext(contentWithExt)
	contentID := strings.TrimSuffix(contentWithExt, ext)

	if !isKebabCase(contentID) {
		violations = append(violations, NamingViolation{
			FilePath:       relativePath,
			FileName:       filename,
			ViolationType:  ViolationBadCase,
			ExpectedPrefix: expectedPrefix,
			ActualPrefix:   actualPrefix,
			Message:        fmt.Sprintf("Content identifier '%s' is not in kebab-case", contentID),
		})
	}

	return violations
}

// isKebabCase checks if a string is valid kebab-case.
// Rules:
// - All lowercase
// - Only letters, numbers, hyphens, and dots (for version numbers)
// - Cannot start or end with hyphen
// - No consecutive hyphens
func isKebabCase(s string) bool {
	if s == "" {
		return false
	}
	return kebabCaseRegex.MatchString(s)
}
