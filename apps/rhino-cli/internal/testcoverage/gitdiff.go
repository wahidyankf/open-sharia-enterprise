package testcoverage

import (
	"regexp"
	"strconv"
	"strings"
)

// DiffHunk represents a set of changed lines in a file from a git diff.
type DiffHunk struct {
	FilePath    string
	ChangedLines []int // line numbers in the new file
}

var (
	diffFileRe = regexp.MustCompile(`^diff --git a/.+ b/(.+)$`)
	hunkRe     = regexp.MustCompile(`^@@ -\d+(?:,\d+)? \+(\d+)(?:,(\d+))? @@`)
	renameRe   = regexp.MustCompile(`^rename to (.+)$`)
)

// ParseGitDiff parses unified diff output and returns changed lines per file.
// Only counts added/modified lines (+ lines), not deleted lines.
func ParseGitDiff(diffOutput string) []DiffHunk {
	lines := strings.Split(diffOutput, "\n")
	var hunks []DiffHunk
	fileLines := make(map[string][]int)
	currentFile := ""
	inHunk := false
	currentLineNo := 0

	for _, line := range lines {
		// New file header
		if m := diffFileRe.FindStringSubmatch(line); m != nil {
			currentFile = m[1]
			inHunk = false
			continue
		}

		// Handle renames
		if m := renameRe.FindStringSubmatch(line); m != nil {
			currentFile = m[1]
			continue
		}

		// Skip binary files
		if strings.HasPrefix(line, "Binary files") {
			currentFile = ""
			continue
		}

		// Hunk header
		if m := hunkRe.FindStringSubmatch(line); m != nil {
			startLine, _ := strconv.Atoi(m[1])
			currentLineNo = startLine
			inHunk = true
			continue
		}

		if !inHunk || currentFile == "" {
			continue
		}

		// Count lines in the diff
		if strings.HasPrefix(line, "+") && !strings.HasPrefix(line, "+++") {
			fileLines[currentFile] = append(fileLines[currentFile], currentLineNo)
			currentLineNo++
		} else if strings.HasPrefix(line, "-") && !strings.HasPrefix(line, "---") {
			// Deleted line — don't increment new-file line counter
		} else {
			// Context line
			currentLineNo++
		}
	}

	for filePath, changedLines := range fileLines {
		hunks = append(hunks, DiffHunk{FilePath: filePath, ChangedLines: changedLines})
	}
	return hunks
}
