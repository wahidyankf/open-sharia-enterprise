package claude

import (
	"bytes"
	"fmt"
	"strings"

	"github.com/wahidyankf/open-sharia-enterprise/apps/rhino-cli/internal/sync"
)

// validateYAMLFormattingRaw checks YAML frontmatter formatting (space after colons).
// checkName is the full ValidationCheck.Name, e.g. "Agent: foo.md - YAML Formatting".
func validateYAMLFormattingRaw(checkName string, content []byte) sync.ValidationCheck {
	lines := bytes.Split(content, []byte("\n"))

	if len(lines) < 3 {
		return sync.ValidationCheck{
			Name:    checkName,
			Status:  "passed",
			Message: "File too short to check formatting",
		}
	}

	// Find frontmatter boundaries
	if !bytes.Equal(bytes.TrimSpace(lines[0]), []byte("---")) {
		return sync.ValidationCheck{
			Name:    checkName,
			Status:  "failed",
			Message: "Frontmatter does not start with ---",
		}
	}

	endIndex := -1
	for i := 1; i < len(lines); i++ {
		if bytes.Equal(bytes.TrimSpace(lines[i]), []byte("---")) {
			endIndex = i
			break
		}
	}

	if endIndex == -1 {
		return sync.ValidationCheck{
			Name:    checkName,
			Status:  "failed",
			Message: "Frontmatter closing --- not found",
		}
	}

	// Check each line in frontmatter for missing spaces after colons
	var issues []string
	for i := 1; i < endIndex; i++ {
		line := lines[i]
		trimmed := bytes.TrimSpace(line)

		// Skip empty lines, list items, and comments
		if len(trimmed) == 0 || bytes.HasPrefix(trimmed, []byte("-")) || bytes.HasPrefix(trimmed, []byte("#")) {
			continue
		}

		// Check if line is a key-value pair without space after colon
		if bytes.Contains(trimmed, []byte(":")) {
			parts := bytes.SplitN(trimmed, []byte(":"), 2)
			if len(parts) == 2 {
				if len(parts[1]) > 0 && parts[1][0] != ' ' {
					issues = append(issues, fmt.Sprintf("Line %d: '%s' (missing space after colon)", i+1, string(trimmed)))
				}
			}
		}
	}

	if len(issues) > 0 {
		return sync.ValidationCheck{
			Name:     checkName,
			Status:   "failed",
			Expected: "Space after colon in YAML key-value pairs (e.g., 'name: value')",
			Actual:   fmt.Sprintf("Found %d formatting issues", len(issues)),
			Message:  fmt.Sprintf("YAML formatting errors:\n  %s", strings.Join(issues, "\n  ")),
		}
	}

	return sync.ValidationCheck{
		Name:    checkName,
		Status:  "passed",
		Message: "YAML formatting correct (spaces after colons)",
	}
}
