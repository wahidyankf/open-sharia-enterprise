package testcoverage

import (
	"bufio"
	"os"
	"strings"
)

// DetectFormat determines the coverage file format from the filename and content.
//
// Detection priority:
//  1. Filename-based: .info/lcov → LCOV, .xml+jacoco → JaCoCo, .xml+cobertura → Cobertura
//  2. Content-based: mode: → Go, SF:/TN: → LCOV, <report> → JaCoCo, <coverage> → Cobertura
//  3. Fallback: Go
func DetectFormat(filename string) Format {
	lower := strings.ToLower(filename)
	if strings.HasSuffix(lower, ".info") || strings.Contains(lower, "lcov") {
		return FormatLCOV
	}
	if strings.HasSuffix(lower, ".xml") && strings.Contains(lower, "jacoco") {
		return FormatJaCoCo
	}
	if strings.HasSuffix(lower, ".xml") && strings.Contains(lower, "cobertura") {
		return FormatCobertura
	}

	f, err := os.Open(filename)
	if err != nil {
		return FormatGo
	}
	defer func() { _ = f.Close() }()

	scanner := bufio.NewScanner(f)
	for scanner.Scan() {
		line := strings.TrimSpace(scanner.Text())
		if line == "" {
			continue
		}
		if strings.HasPrefix(line, "mode:") {
			return FormatGo
		}
		if strings.HasPrefix(line, "SF:") || strings.HasPrefix(line, "TN:") {
			return FormatLCOV
		}
		if strings.HasPrefix(line, "<!DOCTYPE") {
			continue
		}
		// For XML: strip <?xml ...?> declaration if present, then check root element
		if strings.HasPrefix(line, "<?xml") {
			// Handle case where root element is on the same line after ?>
			if idx := strings.Index(line, "?>"); idx >= 0 {
				rest := strings.TrimSpace(line[idx+2:])
				if rest != "" {
					line = rest
				} else {
					continue
				}
			} else {
				continue
			}
		}
		if strings.HasPrefix(line, "<report") {
			return FormatJaCoCo
		}
		if strings.HasPrefix(line, "<coverage") {
			return FormatCobertura
		}
		// Unknown content — stop scanning
		break
	}

	return FormatGo
}
