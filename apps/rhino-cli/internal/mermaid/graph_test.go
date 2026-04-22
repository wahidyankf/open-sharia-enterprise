package mermaid

import (
	"testing"
)

func makeNodes(ids ...string) []Node {
	nodes := make([]Node, len(ids))
	for i, id := range ids {
		nodes[i] = Node{ID: id}
	}
	return nodes
}

func makeEdges(pairs ...string) []Edge {
	if len(pairs)%2 != 0 {
		panic("makeEdges: odd number of args")
	}
	edges := make([]Edge, len(pairs)/2)
	for i := 0; i < len(pairs); i += 2 {
		edges[i/2] = Edge{From: pairs[i], To: pairs[i+1]}
	}
	return edges
}

func TestMaxWidth(t *testing.T) {
	tests := []struct {
		name  string
		nodes []Node
		edges []Edge
		want  int
	}{
		{
			name:  "empty graph",
			nodes: nil,
			edges: nil,
			want:  0,
		},
		{
			name:  "single node no edges",
			nodes: makeNodes("A"),
			edges: nil,
			want:  1,
		},
		{
			name:  "linear chain A->B->C",
			nodes: makeNodes("A", "B", "C"),
			edges: makeEdges("A", "B", "B", "C"),
			want:  1,
		},
		{
			name:  "long sequential chain 10 nodes",
			nodes: makeNodes("A", "B", "C", "D", "E", "F", "G", "H", "I", "J"),
			edges: makeEdges(
				"A", "B", "B", "C", "C", "D", "D", "E",
				"E", "F", "F", "G", "G", "H", "H", "I", "I", "J",
			),
			want: 1,
		},
		{
			name:  "fan-out A->B A->C A->D",
			nodes: makeNodes("A", "B", "C", "D"),
			edges: makeEdges("A", "B", "A", "C", "A", "D"),
			want:  3,
		},
		{
			name:  "fan-out A->B A->C A->D A->E",
			nodes: makeNodes("A", "B", "C", "D", "E"),
			edges: makeEdges("A", "B", "A", "C", "A", "D", "A", "E"),
			want:  4,
		},
		{
			name:  "diamond A->B A->C B->D C->D",
			nodes: makeNodes("A", "B", "C", "D"),
			edges: makeEdges("A", "B", "A", "C", "B", "D", "C", "D"),
			want:  2,
		},
		{
			name:  "two disconnected chains A->B and C->D",
			nodes: makeNodes("A", "B", "C", "D"),
			edges: makeEdges("A", "B", "C", "D"),
			want:  2, // rank 0: A, C (count=2); rank 1: B, D (count=2)
		},
		{
			name:  "cycle A->B B->A no panic",
			nodes: makeNodes("A", "B"),
			edges: makeEdges("A", "B", "B", "A"),
			// Both get rank 0 after cycle fallback → width = 2
			want: 2,
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			got := MaxWidth(tt.nodes, tt.edges)
			if got != tt.want {
				t.Errorf("MaxWidth = %d, want %d", got, tt.want)
			}
		})
	}
}

func TestDepth(t *testing.T) {
	tests := []struct {
		name  string
		nodes []Node
		edges []Edge
		want  int
	}{
		{
			name:  "empty graph",
			nodes: nil,
			edges: nil,
			want:  0,
		},
		{
			name:  "single node no edges",
			nodes: makeNodes("A"),
			edges: nil,
			want:  1,
		},
		{
			name:  "linear chain A->B->C",
			nodes: makeNodes("A", "B", "C"),
			edges: makeEdges("A", "B", "B", "C"),
			want:  3,
		},
		{
			name:  "fan-out A->B A->C A->D",
			nodes: makeNodes("A", "B", "C", "D"),
			edges: makeEdges("A", "B", "A", "C", "A", "D"),
			want:  2,
		},
		{
			name:  "diamond A->B A->C B->D C->D",
			nodes: makeNodes("A", "B", "C", "D"),
			edges: makeEdges("A", "B", "A", "C", "B", "D", "C", "D"),
			want:  3,
		},
		{
			name:  "cycle A->B B->A depth 1 after fallback",
			nodes: makeNodes("A", "B"),
			edges: makeEdges("A", "B", "B", "A"),
			want:  1,
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			got := Depth(tt.nodes, tt.edges)
			if got != tt.want {
				t.Errorf("Depth = %d, want %d", got, tt.want)
			}
		})
	}
}
