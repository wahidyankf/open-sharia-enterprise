package markdown

import (
	"bufio"
	"fmt"
	"os"
	"regexp"
	"strconv"
	"strings"
)

// Frontmatter represents the YAML frontmatter of a markdown file
type Frontmatter struct {
	Raw    string
	Title  string
	Weight int
}

// ExtractFrontmatter extracts and parses frontmatter from a markdown file
func ExtractFrontmatter(filePath string) (*Frontmatter, error) {
	file, err := os.Open(filePath)
	if err != nil {
		return nil, fmt.Errorf("failed to open file: %w", err)
	}
	defer func() { _ = file.Close() }()

	scanner := bufio.NewScanner(file)
	var lines []string
	inFrontmatter := false
	frontmatterStarted := false

	for scanner.Scan() {
		line := scanner.Text()

		if line == "---" {
			if !frontmatterStarted {
				frontmatterStarted = true
				inFrontmatter = true
				lines = append(lines, line)
				continue
			}
			if inFrontmatter {
				lines = append(lines, line)
				break
			}
		}

		if inFrontmatter {
			lines = append(lines, line)
		}
	}

	if err := scanner.Err(); err != nil {
		return nil, fmt.Errorf("error reading file: %w", err)
	}

	if len(lines) < 3 {
		return nil, fmt.Errorf("no valid frontmatter found")
	}

	fm := &Frontmatter{
		Raw:    strings.Join(lines, "\n") + "\n",
		Weight: 999999, // Default weight
	}

	// Parse title and weight
	for _, line := range lines {
		if strings.HasPrefix(line, "title:") {
			title := strings.TrimSpace(strings.TrimPrefix(line, "title:"))
			// Remove quotes if present
			title = strings.Trim(title, `"'`)
			fm.Title = title
		}
		if strings.HasPrefix(line, "weight:") {
			weightStr := strings.TrimSpace(strings.TrimPrefix(line, "weight:"))
			if weight, err := strconv.Atoi(weightStr); err == nil {
				fm.Weight = weight
			}
		}
	}

	// Fallback title from filename if not found
	if fm.Title == "" {
		// Extract filename without extension and path
		re := regexp.MustCompile(`[^/]+\.md$`)
		if match := re.FindString(filePath); match != "" {
			fm.Title = strings.TrimSuffix(match, ".md")
		}
	}

	return fm, nil
}
