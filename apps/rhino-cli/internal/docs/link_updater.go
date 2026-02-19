package docs

import (
	"bufio"
	"os"
	"path/filepath"
	"regexp"
	"strings"

	"github.com/wahidyankf/open-sharia-enterprise/apps/rhino-cli/internal/fileutil"
)

var (
	// markdownLinkRegex matches markdown links: [text](url)
	markdownLinkRegex = regexp.MustCompile(`\[([^\]]+)\]\(([^)]+)\)`)
)

// FindLinksToUpdate scans markdown files for links that reference files being renamed.
func FindLinksToUpdate(renames []RenameOperation, repoRoot string) ([]LinkUpdate, error) {
	// Build a map of old filenames to new filenames for quick lookup
	filenameMap := make(map[string]string)
	for _, op := range renames {
		filenameMap[op.OldName] = op.NewName
	}

	// Get all markdown files to scan
	files, err := getMarkdownFilesToScan(repoRoot)
	if err != nil {
		return nil, err
	}

	var updates []LinkUpdate

	for _, filePath := range files {
		fileUpdates, err := scanFileForLinks(filePath, filenameMap, repoRoot)
		if err != nil {
			// Log error but continue with other files
			continue
		}
		updates = append(updates, fileUpdates...)
	}

	return updates, nil
}

// getMarkdownFilesToScan returns all markdown files that might contain links.
func getMarkdownFilesToScan(repoRoot string) ([]string, error) {
	return fileutil.WalkMarkdownDirs(repoRoot, []string{"docs", "governance", ".claude"})
}

// scanFileForLinks scans a single file for links that need updating.
func scanFileForLinks(filePath string, filenameMap map[string]string, repoRoot string) ([]LinkUpdate, error) {
	file, err := os.Open(filePath)
	if err != nil {
		return nil, err
	}
	defer file.Close()

	relPath, err := filepath.Rel(repoRoot, filePath)
	if err != nil {
		relPath = filePath
	}

	var updates []LinkUpdate
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
		matches := markdownLinkRegex.FindAllStringSubmatchIndex(line, -1)
		for _, match := range matches {
			if len(match) < 6 {
				continue
			}

			// Extract the URL part (group 2)
			linkStart := match[4]
			linkEnd := match[5]
			link := line[linkStart:linkEnd]

			// Check if this link references a file being renamed
			newLink := updateLinkIfNeeded(link, filenameMap)
			if newLink != "" && newLink != link {
				updates = append(updates, LinkUpdate{
					FilePath:   relPath,
					LineNumber: lineNumber,
					OldLink:    link,
					NewLink:    newLink,
				})
			}
		}
	}

	return updates, scanner.Err()
}

// updateLinkIfNeeded checks if a link references a renamed file and returns the updated link.
// Returns empty string if no update needed.
func updateLinkIfNeeded(link string, filenameMap map[string]string) string {
	// Skip external URLs
	if strings.HasPrefix(link, "http://") || strings.HasPrefix(link, "https://") ||
		strings.HasPrefix(link, "mailto:") {
		return ""
	}

	// Skip anchor-only links
	if strings.HasPrefix(link, "#") {
		return ""
	}

	// Handle anchors: split link from anchor
	anchor := ""
	linkWithoutAnchor := link
	if idx := strings.Index(link, "#"); idx != -1 {
		anchor = link[idx:]
		linkWithoutAnchor = link[:idx]
	}

	// Get the filename from the link
	filename := filepath.Base(linkWithoutAnchor)

	// Check if this filename is in our rename map
	newFilename, ok := filenameMap[filename]
	if !ok {
		return ""
	}

	// Replace the filename in the link
	dir := filepath.Dir(linkWithoutAnchor)
	var newLink string
	if dir == "." {
		newLink = newFilename
	} else {
		newLink = filepath.Join(dir, newFilename)
	}

	// Preserve forward slashes (filepath.Join uses OS separator)
	newLink = strings.ReplaceAll(newLink, "\\", "/")

	// Re-add anchor if present
	return newLink + anchor
}

// ApplyLinkUpdates applies the link updates to files.
func ApplyLinkUpdates(updates []LinkUpdate, repoRoot string) (int, []string) {
	// Group updates by file
	updatesByFile := make(map[string][]LinkUpdate)
	for _, u := range updates {
		updatesByFile[u.FilePath] = append(updatesByFile[u.FilePath], u)
	}

	var applied int
	var errors []string

	for filePath, fileUpdates := range updatesByFile {
		err := applyUpdatesToFile(filePath, fileUpdates, repoRoot)
		if err != nil {
			errors = append(errors, err.Error())
		} else {
			applied += len(fileUpdates)
		}
	}

	return applied, errors
}

// applyUpdatesToFile applies link updates to a single file.
func applyUpdatesToFile(relPath string, updates []LinkUpdate, repoRoot string) error {
	filePath := filepath.Join(repoRoot, relPath)

	// Read the entire file
	content, err := os.ReadFile(filePath)
	if err != nil {
		return err
	}

	// Apply each update
	newContent := string(content)
	for _, u := range updates {
		// Replace old link with new link
		// Use a simple string replacement - this works because we're replacing
		// the exact link text that was found
		oldPattern := "](" + u.OldLink + ")"
		newPattern := "](" + u.NewLink + ")"
		newContent = strings.Replace(newContent, oldPattern, newPattern, -1)
	}

	// Write the file back
	return os.WriteFile(filePath, []byte(newContent), 0644)
}
