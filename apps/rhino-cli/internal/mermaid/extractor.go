package mermaid

import (
	"strings"
)

// ExtractBlocks scans markdown content line-by-line and returns all mermaid
// fenced code blocks. StartLine is the 1-based line number of the opening fence.
func ExtractBlocks(filePath string, content string) []MermaidBlock {
	var blocks []MermaidBlock
	lines := strings.Split(content, "\n")

	inBlock := false
	var sourceLines []string
	blockIndex := 0
	startLine := 0

	for i, line := range lines {
		trimmed := strings.TrimSpace(line)
		if !inBlock {
			// Accept ```mermaid or ~~~mermaid as opening fences.
			if strings.HasPrefix(line, "```mermaid") || strings.HasPrefix(line, "~~~mermaid") {
				inBlock = true
				sourceLines = nil
				startLine = i + 1 // convert to 1-based
			}
		} else {
			// Accept ``` or ~~~ as closing fences.
			if trimmed == "```" || trimmed == "~~~" {
				blocks = append(blocks, MermaidBlock{
					FilePath:   filePath,
					BlockIndex: blockIndex,
					Source:     strings.Join(sourceLines, "\n"),
					StartLine:  startLine,
				})
				blockIndex++
				inBlock = false
			} else {
				sourceLines = append(sourceLines, line)
			}
		}
	}
	return blocks
}
