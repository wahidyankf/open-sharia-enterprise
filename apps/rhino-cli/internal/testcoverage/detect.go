package testcoverage

import (
	"bufio"
	"os"
	"strings"
)

// DetectFormat determines the coverage file format from the filename and content.
// Returns FormatLCOV if filename ends in ".info" or contains "lcov" (case-insensitive).
// Otherwise reads the first line: "mode:" → FormatGo, "SF:" or "TN:" → FormatLCOV.
// Falls back to FormatGo.
func DetectFormat(filename string) Format {
	lower := strings.ToLower(filename)
	if strings.HasSuffix(lower, ".info") || strings.Contains(lower, "lcov") {
		return FormatLCOV
	}
	if strings.HasSuffix(lower, ".xml") && strings.Contains(lower, "jacoco") {
		return FormatJaCoCo
	}

	f, err := os.Open(filename)
	if err != nil {
		return FormatGo
	}
	defer func() { _ = f.Close() }()

	scanner := bufio.NewScanner(f)
	if scanner.Scan() {
		first := strings.TrimSpace(scanner.Text())
		if strings.HasPrefix(first, "mode:") {
			return FormatGo
		}
		if strings.HasPrefix(first, "SF:") || strings.HasPrefix(first, "TN:") {
			return FormatLCOV
		}
		if strings.HasPrefix(first, "<?xml") || strings.HasPrefix(first, "<report") {
			return FormatJaCoCo
		}
	}

	return FormatGo
}
