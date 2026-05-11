package agents

import (
	"crypto/sha256"
	"encoding/hex"
	"fmt"
	"os"
	"path/filepath"
	"sort"
	"strings"
)

// DuplicationWindowSize is the fixed sliding-window length (in normalized
// lines) used to detect verbatim duplication across agent and skill files.
// 10 lines matches the existing AI checker's threshold for LOW severity
// duplication; smaller windows produce too many fragmentary matches and
// larger windows miss real duplication.
const DuplicationWindowSize = 10

// DuplicationFinding describes a single verbatim duplication cluster — a
// 10-line window present in two or more agent or skill files.
type DuplicationFinding struct {
	// Files is the list of file paths participating in the cluster, sorted
	// ascending so that output is deterministic.
	Files []string
	// StartLines is the 1-based starting line number of the matching window
	// in each corresponding entry of Files (same index).
	StartLines []int
	// WindowSize is the number of contiguous lines in the matching window.
	// Currently always equal to DuplicationWindowSize.
	WindowSize int
	// Severity classifies the finding. Currently always "high" — verbatim
	// duplication across two or more agent or skill files is a high-impact
	// signal that prose should move into a shared skill.
	Severity string
	// Message is a human-readable summary of the finding.
	Message string
}

// DetectDuplication enumerates `.claude/agents/*.md` and
// `.claude/skills/*/SKILL.md` under `repoRoot`, strips each file's YAML
// frontmatter and normalizes whitespace, then scans for 10-line verbatim
// windows that appear in two or more files. Heading-only and
// whitespace-only windows are excluded — they would otherwise generate
// noise without indicating real prose duplication.
//
// Findings are sorted by (first File, first StartLine) for deterministic
// output and stable test assertions.
func DetectDuplication(repoRoot string) ([]DuplicationFinding, error) {
	files, err := enumerateAgentAndSkillFiles(repoRoot)
	if err != nil {
		return nil, fmt.Errorf("enumerate agent/skill files: %w", err)
	}

	// Per-file: read, strip frontmatter, normalize whitespace, split into
	// lines, then produce overlapping 10-line windows. For each window
	// record its SHA-256 hash, the absolute file path, and the 1-based
	// starting line number of the window. Filter out heading-only and
	// whitespace-only windows up front so they never become cluster
	// candidates.
	type windowRef struct {
		file      string
		startLine int
	}
	hashIndex := map[string][]windowRef{}

	for _, path := range files {
		raw, err := os.ReadFile(path) //nolint:gosec // path comes from controlled enumeration under repoRoot
		if err != nil {
			return nil, fmt.Errorf("read %s: %w", path, err)
		}
		lines := normalizeLines(stripFrontmatter(string(raw)))
		if len(lines) < DuplicationWindowSize {
			continue
		}
		for i := 0; i+DuplicationWindowSize <= len(lines); i++ {
			window := lines[i : i+DuplicationWindowSize]
			if isExcludedWindow(window) {
				continue
			}
			h := hashWindow(window)
			hashIndex[h] = append(hashIndex[h], windowRef{file: path, startLine: i + 1})
		}
	}

	// Group: any hash appearing in 2+ distinct files becomes a finding.
	// Multiple matches inside the same file do not constitute cross-file
	// duplication and are not reported.
	var findings []DuplicationFinding
	for _, refs := range hashIndex {
		distinctFiles := map[string]int{}
		for _, r := range refs {
			if _, ok := distinctFiles[r.file]; !ok {
				distinctFiles[r.file] = r.startLine
			}
		}
		if len(distinctFiles) < 2 {
			continue
		}
		// Build deterministic file order and matched start lines.
		paths := make([]string, 0, len(distinctFiles))
		for p := range distinctFiles {
			paths = append(paths, p)
		}
		sort.Strings(paths)
		starts := make([]int, len(paths))
		for i, p := range paths {
			starts[i] = distinctFiles[p]
		}
		findings = append(findings, DuplicationFinding{
			Files:      paths,
			StartLines: starts,
			WindowSize: DuplicationWindowSize,
			Severity:   "high",
			Message: fmt.Sprintf(
				"%d-line verbatim duplication across %d files",
				DuplicationWindowSize, len(paths),
			),
		})
	}

	sort.SliceStable(findings, func(i, j int) bool {
		if findings[i].Files[0] != findings[j].Files[0] {
			return findings[i].Files[0] < findings[j].Files[0]
		}
		return findings[i].StartLines[0] < findings[j].StartLines[0]
	})
	return findings, nil
}

// enumerateAgentAndSkillFiles returns absolute paths for every
// `.claude/agents/*.md` and `.claude/skills/*/SKILL.md` under repoRoot.
// Missing parent directories yield an empty slice (not an error) so the
// detector can run in trees where neither directory has been created.
func enumerateAgentAndSkillFiles(repoRoot string) ([]string, error) {
	var files []string

	agentsDir := filepath.Join(repoRoot, ".claude", "agents")
	if entries, err := os.ReadDir(agentsDir); err == nil {
		for _, e := range entries {
			if e.IsDir() {
				continue
			}
			name := e.Name()
			if !strings.HasSuffix(name, ".md") {
				continue
			}
			if name == "README.md" {
				continue
			}
			files = append(files, filepath.Join(agentsDir, name))
		}
	} else if !os.IsNotExist(err) {
		return nil, fmt.Errorf("read %s: %w", agentsDir, err)
	}

	skillsDir := filepath.Join(repoRoot, ".claude", "skills")
	if entries, err := os.ReadDir(skillsDir); err == nil {
		for _, e := range entries {
			if !e.IsDir() {
				continue
			}
			skillFile := filepath.Join(skillsDir, e.Name(), "SKILL.md")
			if _, err := os.Stat(skillFile); err == nil {
				files = append(files, skillFile)
			}
		}
	} else if !os.IsNotExist(err) {
		return nil, fmt.Errorf("read %s: %w", skillsDir, err)
	}

	sort.Strings(files)
	return files, nil
}

// stripFrontmatter removes a leading YAML frontmatter block, if present.
// A frontmatter block starts with a line whose only content is `---` and
// ends at the next line whose only content is `---`. When no frontmatter
// is present the input is returned unchanged.
func stripFrontmatter(s string) string {
	if !strings.HasPrefix(s, "---\n") && !strings.HasPrefix(s, "---\r\n") {
		return s
	}
	// Skip past the opening fence and find the closing fence.
	_, body, ok := strings.Cut(s, "\n")
	if !ok {
		return s
	}
	idx := indexOfFenceLine(body)
	if idx < 0 {
		// No closing fence — treat as no frontmatter rather than dropping
		// the whole document.
		return s
	}
	// idx points to the start of the closing `---` line; skip past its
	// terminating newline.
	closeLine := body[idx:]
	closeNL := strings.Index(closeLine, "\n")
	if closeNL < 0 {
		// File ends exactly at the closing fence with no trailing newline.
		return ""
	}
	return body[idx+closeNL+1:]
}

// indexOfFenceLine returns the byte offset of the first line in `body`
// whose entire content (ignoring trailing CR) is exactly `---`, or -1 if
// no such line exists.
func indexOfFenceLine(body string) int {
	offset := 0
	for offset <= len(body) {
		end := strings.Index(body[offset:], "\n")
		var line string
		if end < 0 {
			line = body[offset:]
		} else {
			line = body[offset : offset+end]
		}
		line = strings.TrimRight(line, "\r")
		if line == "---" {
			return offset
		}
		if end < 0 {
			break
		}
		offset += end + 1
	}
	return -1
}

// normalizeLines normalizes whitespace and splits `s` into lines. Each
// returned line has trailing spaces and tabs trimmed; consecutive blank
// lines are collapsed into a single blank line so cosmetic whitespace
// differences cannot mask real duplication.
func normalizeLines(s string) []string {
	// Replace CRLF with LF for cross-platform stability.
	s = strings.ReplaceAll(s, "\r\n", "\n")
	raw := strings.Split(s, "\n")
	out := make([]string, 0, len(raw))
	prevBlank := false
	for _, line := range raw {
		trimmed := strings.TrimRight(line, " \t")
		blank := trimmed == ""
		if blank && prevBlank {
			continue
		}
		out = append(out, trimmed)
		prevBlank = blank
	}
	return out
}

// isExcludedWindow reports whether `lines` is a window that should NOT
// participate in duplication detection — windows composed entirely of
// blank lines or of heading lines (every non-blank line starts with `#`).
// These windows generate noise without indicating real prose duplication.
func isExcludedWindow(lines []string) bool {
	allBlank := true
	allHeadingOrBlank := true
	for _, l := range lines {
		t := strings.TrimSpace(l)
		if t != "" {
			allBlank = false
			if !strings.HasPrefix(t, "#") {
				allHeadingOrBlank = false
			}
		}
	}
	return allBlank || allHeadingOrBlank
}

// hashWindow returns the lowercase hex SHA-256 of the concatenation of
// `lines` joined by `\n`. SHA-256 is collision-resistant for the small
// content space hashed here and is available from the standard library.
func hashWindow(lines []string) string {
	joined := strings.Join(lines, "\n")
	sum := sha256.Sum256([]byte(joined))
	return hex.EncodeToString(sum[:])
}
