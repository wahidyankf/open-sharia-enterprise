package mermaid

import (
	"regexp"
	"strings"
)

// flowchartHeaderRe matches a flowchart or graph header line (with optional direction).
var flowchartHeaderRe = regexp.MustCompile(`(?m)^\s*(flowchart|graph)(\s+(TB|TD|BT|LR|RL))?\s*$`)

// arrowTokens is the set of substrings that identify edge lines.
var arrowTokenRe = regexp.MustCompile(`-->|---|-\.->|==>|--o|--x|<-->`)

// Node shape regexes: order matters — longest/most-specific match first.
// Each captures (nodeID, label).
var nodeShapePatterns = []*regexp.Regexp{
	// Double-circle: A(((label)))
	regexp.MustCompile(`^(\w+)\(\(\(([^)]*)\)\)\)`),
	// Stadium: A([label])
	regexp.MustCompile(`^(\w+)\(\[([^\]]*)\]\)`),
	// Circle: A((label))
	regexp.MustCompile(`^(\w+)\(\(([^)]*)\)\)`),
	// Subroutine: A[[label]]
	regexp.MustCompile(`^(\w+)\[\[([^\]]*)\]\]`),
	// Cylinder: A[(label)]
	regexp.MustCompile(`^(\w+)\[\(([^)]*)\)\]`),
	// Round: A(label)
	regexp.MustCompile(`^(\w+)\(([^)]*)\)`),
	// Hexagon: A{{label}}
	regexp.MustCompile(`^(\w+)\{\{([^}]*)\}\}`),
	// Diamond: A{label}
	regexp.MustCompile(`^(\w+)\{([^}]*)\}`),
	// Asymmetric: A>label]
	regexp.MustCompile(`^(\w+)>([^\]]*)\]`),
	// Parallelogram forward: A[/label/]
	regexp.MustCompile(`^(\w+)\[/([^/]*)/\]`),
	// Parallelogram back: A[\label\]
	regexp.MustCompile(`^(\w+)\[\\([^\\]*)\\]`),
	// Rectangle: A[label]
	regexp.MustCompile(`^(\w+)\[([^\]]*)\]`),
	// Modern API: A@{ label: "text" }
	regexp.MustCompile(`^(\w+)@\{\s*[^}]*label:\s*"([^"]*)"\s*[^}]*\}`),
}

// nodeIDRe matches a bare word node identifier.
var nodeIDRe = regexp.MustCompile(`^(\w+)$`)

// ParseDiagram parses a MermaidBlock into a ParsedDiagram.
// The second return value is the number of flowchart/graph headers found.
// count == 0 means the block is not a flowchart (caller should skip Rule 1/2).
// count > 1 means the caller should emit ViolationMultipleDiagrams.
func ParseDiagram(block MermaidBlock) (ParsedDiagram, int, error) {
	matches := flowchartHeaderRe.FindAllStringSubmatch(block.Source, -1)
	count := len(matches)
	if count == 0 {
		return ParsedDiagram{Block: block}, 0, nil
	}

	// Extract direction from the first header match.
	firstMatch := matches[0]
	dir := DirectionTB
	if len(firstMatch) >= 4 && strings.TrimSpace(firstMatch[3]) != "" {
		dir = Direction(strings.TrimSpace(firstMatch[3]))
	}

	// Parse nodes and edges.
	nodeMap := make(map[string]string) // id → label (last-declaration-wins)
	var edges []Edge

	lines := strings.Split(block.Source, "\n")
	for _, raw := range lines {
		line := strings.TrimSpace(raw)
		if line == "" {
			continue
		}
		// Skip subgraph delimiters.
		if strings.HasPrefix(line, "subgraph") || line == "end" {
			continue
		}
		// Skip the flowchart/graph header lines.
		if flowchartHeaderRe.MatchString(line) {
			continue
		}

		if arrowTokenRe.MatchString(line) {
			// Edge line: extract nodes and edges.
			extractEdgeLine(line, nodeMap, &edges)
		} else {
			// Standalone node declaration (Pass A).
			extractStandaloneNode(line, nodeMap)
		}
	}

	// Build ordered node list preserving insertion order via a separate slice.
	// We need all node IDs that were referenced.
	seenOrder := collectNodeOrder(block.Source, nodeMap)
	var nodes []Node
	for _, id := range seenOrder {
		nodes = append(nodes, Node{ID: id, Label: nodeMap[id]})
	}

	return ParsedDiagram{
		Block:     block,
		Direction: dir,
		Nodes:     nodes,
		Edges:     edges,
	}, count, nil
}

// collectNodeOrder returns node IDs in first-seen order from the source lines.
func collectNodeOrder(source string, nodeMap map[string]string) []string {
	seen := make(map[string]bool)
	var order []string

	lines := strings.Split(source, "\n")
	for _, raw := range lines {
		line := strings.TrimSpace(raw)
		if line == "" || strings.HasPrefix(line, "subgraph") || line == "end" {
			continue
		}
		if flowchartHeaderRe.MatchString(line) {
			continue
		}
		// Collect IDs referenced on this line.
		ids := extractAllNodeIDs(line)
		for _, id := range ids {
			if _, exists := nodeMap[id]; exists && !seen[id] {
				seen[id] = true
				order = append(order, id)
			}
		}
	}
	// Include any node IDs in nodeMap not yet seen (shouldn't happen but be safe).
	for id := range nodeMap {
		if !seen[id] {
			seen[id] = true
			order = append(order, id)
		}
	}
	return order
}

// extractAllNodeIDs pulls every node ID referenced on a single line.
func extractAllNodeIDs(line string) []string {
	var ids []string
	if arrowTokenRe.MatchString(line) {
		// Split on arrow tokens to get segments.
		segments := arrowTokenRe.Split(line, -1)
		for _, seg := range segments {
			id := extractNodeIDFromSegment(seg)
			if id != "" {
				ids = append(ids, id)
			}
		}
	} else {
		id := extractNodeIDFromSegment(line)
		if id != "" {
			ids = append(ids, id)
		}
	}
	return ids
}

// extractNodeIDFromSegment extracts a node ID from a single segment.
func extractNodeIDFromSegment(seg string) string {
	seg = strings.TrimSpace(seg)
	if seg == "" {
		return ""
	}
	// Try shape patterns first.
	for _, re := range nodeShapePatterns {
		if m := re.FindStringSubmatch(seg); m != nil {
			return m[1]
		}
	}
	// Bare word.
	if m := nodeIDRe.FindStringSubmatch(seg); m != nil {
		return m[1]
	}
	return ""
}

// extractStandaloneNode parses a standalone node declaration line and updates nodeMap.
func extractStandaloneNode(line string, nodeMap map[string]string) {
	line = strings.TrimSpace(line)
	for _, re := range nodeShapePatterns {
		if m := re.FindStringSubmatch(line); m != nil {
			nodeMap[m[1]] = normalizeLabel(m[2])
			return
		}
	}
	// Bare word (no label).
	if m := nodeIDRe.FindStringSubmatch(line); m != nil {
		if _, exists := nodeMap[m[1]]; !exists {
			nodeMap[m[1]] = ""
		}
	}
}

// extractEdgeLine parses an edge line, updating nodeMap and appending to edges.
func extractEdgeLine(line string, nodeMap map[string]string, edges *[]Edge) {
	// Handle edge labels like "A -- text --> B": replace " -- text -->" with "-->".
	// Normalise link-text arrows: "-- text -->" → "-->"
	// Pattern: -- <anything> --> or -- <anything> ---
	linkTextRe := regexp.MustCompile(`--[^-\n]*?-->`)
	line = linkTextRe.ReplaceAllString(line, "-->")

	// Split on arrow tokens.
	parts := arrowTokenRe.Split(line, -1)
	if len(parts) < 2 {
		return
	}

	// Extract node IDs and labels from each part.
	var nodeIDs []string
	for _, part := range parts {
		part = strings.TrimSpace(part)
		if part == "" {
			continue
		}
		// Try shape patterns to get ID + label.
		matched := false
		for _, re := range nodeShapePatterns {
			if m := re.FindStringSubmatch(part); m != nil {
				nodeMap[m[1]] = normalizeLabel(m[2])
				nodeIDs = append(nodeIDs, m[1])
				matched = true
				break
			}
		}
		if !matched {
			// Bare word.
			if m := nodeIDRe.FindStringSubmatch(part); m != nil {
				if _, exists := nodeMap[m[1]]; !exists {
					nodeMap[m[1]] = ""
				}
				nodeIDs = append(nodeIDs, m[1])
			}
		}
	}

	// Create edges between consecutive pairs.
	for i := 0; i+1 < len(nodeIDs); i++ {
		*edges = append(*edges, Edge{From: nodeIDs[i], To: nodeIDs[i+1]})
	}
}

// normalizeLabel strips surrounding quotes (single or double) and backtick wrappers.
func normalizeLabel(s string) string {
	s = strings.TrimSpace(s)
	if len(s) >= 2 {
		if (s[0] == '"' && s[len(s)-1] == '"') ||
			(s[0] == '\'' && s[len(s)-1] == '\'') ||
			(s[0] == '`' && s[len(s)-1] == '`') {
			s = s[1 : len(s)-1]
		}
	}
	return s
}

// EffectiveLabelLen returns the display length of a Mermaid node label.
// Labels may contain <br/>, <br> HTML tags or \n escape sequences for multi-line
// rendering; each visual line is checked independently and the longest line length
// is returned. This matches Mermaid's rendering behaviour where wrappingWidth
// applies per visual line.
func EffectiveLabelLen(label string) int {
	if label == "" {
		return 0
	}
	// Normalise all multi-line break variants to a real newline.
	normalized := strings.ReplaceAll(label, "<br/>", "\n")
	normalized = strings.ReplaceAll(normalized, "<BR/>", "\n")
	normalized = strings.ReplaceAll(normalized, "<br>", "\n")
	normalized = strings.ReplaceAll(normalized, "<BR>", "\n")
	normalized = strings.ReplaceAll(normalized, `\n`, "\n") // Mermaid literal \n in labels
	maxLen := 0
	for _, line := range strings.Split(normalized, "\n") {
		if l := len([]rune(line)); l > maxLen {
			maxLen = l
		}
	}
	return maxLen
}
