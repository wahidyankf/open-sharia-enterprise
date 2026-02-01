package links

import (
	"bufio"
	"os"
	"os/exec"
	"path/filepath"
	"regexp"
	"strings"
)

var (
	// linkRegex matches markdown links: [text](url)
	linkRegex = regexp.MustCompile(`\[([^\]]+)\]\(([^)]+)\)`)
)

// GetMarkdownFiles returns a list of markdown files to scan based on options.
func GetMarkdownFiles(opts ScanOptions) ([]string, error) {
	if opts.StagedOnly {
		return getStagedMarkdownFiles(opts.RepoRoot)
	}
	return getAllMarkdownFiles(opts.RepoRoot)
}

// getStagedMarkdownFiles returns staged markdown files from git.
func getStagedMarkdownFiles(repoRoot string) ([]string, error) {
	cmd := exec.Command("git", "diff", "--cached", "--name-only", "--diff-filter=ACM")
	cmd.Dir = repoRoot
	output, err := cmd.Output()
	if err != nil {
		return nil, err
	}

	var files []string
	lines := strings.Split(strings.TrimSpace(string(output)), "\n")
	for _, line := range lines {
		if line != "" && strings.HasSuffix(line, ".md") {
			files = append(files, filepath.Join(repoRoot, line))
		}
	}
	return files, nil
}

// getAllMarkdownFiles returns all markdown files in core directories.
func getAllMarkdownFiles(repoRoot string) ([]string, error) {
	directories := []string{
		"governance",
		"docs",
		".claude",
	}

	var files []string

	// Walk each directory recursively
	for _, dir := range directories {
		dirPath := filepath.Join(repoRoot, dir)

		// Check if directory exists
		if _, err := os.Stat(dirPath); os.IsNotExist(err) {
			continue // Skip non-existent directories
		}

		err := filepath.Walk(dirPath, func(path string, info os.FileInfo, err error) error {
			if err != nil {
				return nil // Skip errors
			}
			if !info.IsDir() && strings.HasSuffix(path, ".md") {
				files = append(files, path)
			}
			return nil
		})
		if err != nil {
			return nil, err
		}
	}

	// Add root-level .md files
	rootMatches, err := filepath.Glob(filepath.Join(repoRoot, "*.md"))
	if err != nil {
		return nil, err
	}
	files = append(files, rootMatches...)

	return files, nil
}

// ExtractLinks extracts markdown links from a file with line numbers.
func ExtractLinks(filePath string) ([]LinkInfo, error) {
	file, err := os.Open(filePath)
	if err != nil {
		return nil, err
	}
	defer file.Close()

	var links []LinkInfo
	scanner := bufio.NewScanner(file)
	lineNumber := 0
	inCodeBlock := false

	for scanner.Scan() {
		lineNumber++
		line := scanner.Text()

		// Track code block boundaries
		if strings.HasPrefix(strings.TrimSpace(line), "```") {
			inCodeBlock = !inCodeBlock
			continue
		}

		// Skip lines inside code blocks
		if inCodeBlock {
			continue
		}

		// Find all markdown links in the line
		matches := linkRegex.FindAllStringSubmatch(line, -1)
		for _, match := range matches {
			if len(match) < 3 {
				continue
			}
			url := match[2]

			// Strip angle brackets if present (markdown autolink syntax)
			url = strings.Trim(url, "<>")

			// Skip external URLs, anchors, and mailto
			if strings.HasPrefix(url, "http://") ||
				strings.HasPrefix(url, "https://") ||
				strings.HasPrefix(url, "#") ||
				strings.HasPrefix(url, "mailto:") {
				continue
			}

			// Skip placeholder/example/Hugo paths
			if ShouldSkipLink(url) {
				continue
			}

			links = append(links, LinkInfo{
				LineNumber: lineNumber,
				URL:        url,
				IsRelative: !strings.HasPrefix(url, "/"),
			})
		}
	}

	if err := scanner.Err(); err != nil {
		return nil, err
	}

	return links, nil
}

// ShouldSkipLink determines if a link should be skipped during validation.
func ShouldSkipLink(link string) bool {
	// Skip Hugo absolute paths (these are valid in Hugo sites)
	if strings.HasPrefix(link, "/") {
		return true
	}

	// Skip Hugo shortcodes
	if strings.Contains(link, "{{<") || strings.Contains(link, "{{%") {
		return true
	}

	// Skip obvious placeholder patterns
	placeholders := []string{
		"path.md", "target", "link",
		"./path/to/", "../path/to/",
		"path/to/convention.md", "path/to/practice.md",
		"path/to/rule.md", "./relative/path/to/",
	}
	for _, placeholder := range placeholders {
		if strings.Contains(link, placeholder) {
			return true
		}
	}

	// Skip links with template placeholders in square brackets
	if regexp.MustCompile(`\[[\w-]+\]`).MatchString(link) {
		return true
	}

	// Skip links that are just "path", "target", or "link"
	if link == "path" || link == "target" || link == "link" {
		return true
	}

	// Skip example image paths
	if strings.Contains(link, "/images/") && !strings.HasPrefix(link, "../") {
		return true
	}

	// Skip example tutorial file names (clearly examples)
	examplePatterns := []string{
		"tu__rag-", "ex-co__", "ex__", "tu-bufi__",
		"./tu__", "./re__", "./hoto__", "./ex__",
		"./overview", "./guide.md", "./examples.md", "./reference.md",
		"./diagram.png", "./image.png", "./screenshots/",
		"./auth-guide.md", "by-concept/beginner", "./by-example/beginner",
		"swe/prog-lang/", "../parent", "./ai/", "../swe/", "../../advanced/",
		"url", "./LICENSE", "../../features.md",
		"./tutorials/tu__", // Example tutorial paths
		"../../.opencode/", // OpenCode references (not part of this repo)
	}
	for _, pattern := range examplePatterns {
		if strings.Contains(link, pattern) {
			return true
		}
	}

	return false
}
