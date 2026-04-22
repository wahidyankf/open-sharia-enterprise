package mermaid

import (
	"testing"
)

func makeBlock(source string) MermaidBlock {
	return MermaidBlock{
		FilePath:   "test.md",
		BlockIndex: 0,
		Source:     source,
		StartLine:  1,
	}
}

func TestParseDiagram(t *testing.T) {
	tests := []struct {
		name          string
		source        string
		wantCount     int
		wantDirection Direction
		wantNodeIDs   []string
		wantEdges     []Edge
		nodeLabels    map[string]string
	}{
		{
			name:      "empty source",
			source:    "",
			wantCount: 0,
		},
		{
			name:      "non-flowchart block",
			source:    "sequenceDiagram\nA->>B: hello",
			wantCount: 0,
		},
		{
			name:          "simple flowchart TD",
			source:        "flowchart TD\nA --> B",
			wantCount:     1,
			wantDirection: DirectionTD,
			wantNodeIDs:   []string{"A", "B"},
			wantEdges:     []Edge{{From: "A", To: "B"}},
		},
		{
			name:          "node with rectangle label",
			source:        "flowchart TD\nA[Hello World]",
			wantCount:     1,
			wantDirection: DirectionTD,
			wantNodeIDs:   []string{"A"},
			nodeLabels:    map[string]string{"A": "Hello World"},
		},
		{
			name:          "node with quoted label",
			source:        `flowchart TD` + "\n" + `A["Long label text"]`,
			wantCount:     1,
			wantDirection: DirectionTD,
			wantNodeIDs:   []string{"A"},
			nodeLabels:    map[string]string{"A": "Long label text"},
		},
		{
			name:          "duplicate node id last-declaration-wins",
			source:        "flowchart TD\nA[First]\nA[Second]\nA --> B",
			wantCount:     1,
			wantDirection: DirectionTD,
			wantNodeIDs:   []string{"A", "B"},
			nodeLabels:    map[string]string{"A": "Second"},
		},
		{
			name:          "edge with link text",
			source:        "flowchart TD\nA -- text --> B",
			wantCount:     1,
			wantDirection: DirectionTD,
			wantNodeIDs:   []string{"A", "B"},
			wantEdges:     []Edge{{From: "A", To: "B"}},
		},
		{
			name:      "two flowchart headers",
			source:    "flowchart TD\nA --> B\nflowchart LR\nC --> D",
			wantCount: 2,
		},
		{
			name:          "graph LR direction",
			source:        "graph LR\nA --> B",
			wantCount:     1,
			wantDirection: DirectionLR,
			wantNodeIDs:   []string{"A", "B"},
			wantEdges:     []Edge{{From: "A", To: "B"}},
		},
		{
			name:          "flowchart with no direction defaults to TB",
			source:        "flowchart\nA --> B",
			wantCount:     1,
			wantDirection: DirectionTB,
			wantNodeIDs:   []string{"A", "B"},
		},
		{
			name: "subgraph block skipped",
			source: `flowchart TD
subgraph sg
  A
end
B --> A`,
			wantCount:     1,
			wantDirection: DirectionTD,
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			block := makeBlock(tt.source)
			diagram, count, err := ParseDiagram(block)
			if err != nil {
				t.Fatalf("unexpected error: %v", err)
			}
			if count != tt.wantCount {
				t.Errorf("count = %d, want %d", count, tt.wantCount)
			}
			if count == 0 {
				return // non-flowchart: no further checks
			}
			if tt.wantDirection != "" && diagram.Direction != tt.wantDirection {
				t.Errorf("Direction = %q, want %q", diagram.Direction, tt.wantDirection)
			}
			// Check node IDs present.
			for _, wantID := range tt.wantNodeIDs {
				found := false
				for _, n := range diagram.Nodes {
					if n.ID == wantID {
						found = true
						break
					}
				}
				if !found {
					t.Errorf("node %q not found; got nodes %v", wantID, diagram.Nodes)
				}
			}
			// Check labels.
			for wantID, wantLabel := range tt.nodeLabels {
				for _, n := range diagram.Nodes {
					if n.ID == wantID {
						if n.Label != wantLabel {
							t.Errorf("node %q label = %q, want %q", wantID, n.Label, wantLabel)
						}
						break
					}
				}
			}
			// Check edges.
			for _, wantEdge := range tt.wantEdges {
				found := false
				for _, e := range diagram.Edges {
					if e.From == wantEdge.From && e.To == wantEdge.To {
						found = true
						break
					}
				}
				if !found {
					t.Errorf("edge %v not found; got edges %v", wantEdge, diagram.Edges)
				}
			}
		})
	}
}

func TestParseDiagram_SubgraphHeaderNotANode(t *testing.T) {
	source := `flowchart TD
subgraph sg
  A
end
B --> A`
	block := makeBlock(source)
	diagram, count, err := ParseDiagram(block)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	if count != 1 {
		t.Fatalf("count = %d, want 1", count)
	}
	for _, n := range diagram.Nodes {
		if n.ID == "sg" {
			t.Errorf("node 'sg' (subgraph header) must not appear in parsed nodes")
		}
	}
}
