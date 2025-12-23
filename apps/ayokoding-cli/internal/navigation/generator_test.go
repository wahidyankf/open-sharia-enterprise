package navigation

import (
	"strings"
	"testing"
)

func TestGenerateMarkdown(t *testing.T) {
	tests := []struct {
		name     string
		items    []Item
		layer    int
		expected string
	}{
		{
			name: "single item at layer 0",
			items: []Item{
				{Title: "Overview", Path: "/test/overview", Weight: 1, IsDir: false},
			},
			layer:    0,
			expected: "- [Overview](/test/overview)\n",
		},
		{
			name: "multiple items at layer 0",
			items: []Item{
				{Title: "Overview", Path: "/test/overview", Weight: 1, IsDir: false},
				{Title: "Installation", Path: "/test/installation", Weight: 2, IsDir: false},
				{Title: "Getting Started", Path: "/test/getting-started", Weight: 3, IsDir: false},
			},
			layer: 0,
			expected: `- [Overview](/test/overview)
- [Installation](/test/installation)
- [Getting Started](/test/getting-started)
`,
		},
		{
			name: "items with children (DFS structure)",
			items: []Item{
				{Title: "Overview", Path: "/test/overview", Weight: 1, IsDir: false},
				{
					Title:  "Tutorials",
					Path:   "/test/tutorials",
					Weight: 2,
					IsDir:  true,
					Children: []Item{
						{Title: "Beginner", Path: "/test/tutorials/beginner", Weight: 1, IsDir: false},
						{Title: "Advanced", Path: "/test/tutorials/advanced", Weight: 2, IsDir: false},
					},
				},
			},
			layer: 0,
			expected: `- [Overview](/test/overview)
- [Tutorials](/test/tutorials)
  - [Beginner](/test/tutorials/beginner)
  - [Advanced](/test/tutorials/advanced)
`,
		},
		{
			name: "nested children (2 layers)",
			items: []Item{
				{
					Title:  "Learn",
					Path:   "/test/learn",
					Weight: 1,
					IsDir:  true,
					Children: []Item{
						{
							Title:    "Programming",
							Path:     "/test/learn/programming",
							Weight:   1,
							IsDir:    true,
							Children: []Item{}, // No Layer 3
						},
					},
				},
			},
			layer: 0,
			expected: `- [Learn](/test/learn)
  - [Programming](/test/learn/programming)
`,
		},
		{
			name: "items at layer 1 (2-space indentation)",
			items: []Item{
				{Title: "Overview", Path: "/test/tutorials/overview", Weight: 1, IsDir: false},
				{Title: "Beginner", Path: "/test/tutorials/beginner", Weight: 2, IsDir: false},
			},
			layer: 1,
			expected: `  - [Overview](/test/tutorials/overview)
  - [Beginner](/test/tutorials/beginner)
`,
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			result := GenerateMarkdown(tt.items, tt.layer)

			if result != tt.expected {
				t.Errorf("GenerateMarkdown() mismatch:\nGot:\n%s\nWant:\n%s", result, tt.expected)
			}
		})
	}
}

func TestGenerateNavigationContent(t *testing.T) {
	frontmatter := `---
title: Test Page
weight: 100
---
`

	items := []Item{
		{Title: "Overview", Path: "/test/overview", Weight: 1, IsDir: false},
		{
			Title:  "Tutorials",
			Path:   "/test/tutorials",
			Weight: 2,
			IsDir:  true,
			Children: []Item{
				{Title: "Beginner", Path: "/test/tutorials/beginner", Weight: 1, IsDir: false},
			},
		},
	}

	expected := `---
title: Test Page
weight: 100
---

- [Overview](/test/overview)
- [Tutorials](/test/tutorials)
  - [Beginner](/test/tutorials/beginner)
`

	result := GenerateNavigationContent(frontmatter, items)

	if result != expected {
		t.Errorf("GenerateNavigationContent() mismatch:\nGot:\n%s\nWant:\n%s", result, expected)
	}
}

func TestGenerateMarkdown_EmptyItems(t *testing.T) {
	result := GenerateMarkdown([]Item{}, 0)
	if result != "" {
		t.Errorf("Expected empty string for empty items, got: %q", result)
	}
}

func TestGenerateMarkdown_PathPrefixing(t *testing.T) {
	// Test that paths are used as-is (absolute paths from scanner)
	items := []Item{
		{
			Title:  "Parent",
			Path:   "/test/parent",
			Weight: 1,
			IsDir:  true,
			Children: []Item{
				{Title: "Child1", Path: "/test/parent/child1", Weight: 1, IsDir: false},
				{Title: "Child2", Path: "/test/parent/child2", Weight: 2, IsDir: false},
			},
		},
	}

	result := GenerateMarkdown(items, 0)

	// Verify that paths are used as-is (absolute paths)
	if !strings.Contains(result, "[Child1](/test/parent/child1)") {
		t.Error("Child1 path should be absolute path from scanner")
	}
	if !strings.Contains(result, "[Child2](/test/parent/child2)") {
		t.Error("Child2 path should be absolute path from scanner")
	}
}

func TestGenerateMarkdown_Indentation(t *testing.T) {
	items := []Item{
		{
			Title:  "L1",
			Path:   "/test/l1",
			Weight: 1,
			IsDir:  true,
			Children: []Item{
				{
					Title:    "L2",
					Path:     "/test/l1/l2",
					Weight:   1,
					IsDir:    true,
					Children: []Item{}, // No Layer 3
				},
			},
		},
	}

	result := GenerateMarkdown(items, 0)

	lines := strings.Split(result, "\n")

	// Layer 1: no indentation
	if !strings.HasPrefix(lines[0], "- [L1]") {
		t.Error("Layer 1 should have no indentation")
	}

	// Layer 2: 2 spaces
	if !strings.HasPrefix(lines[1], "  - [L2]") {
		t.Error("Layer 2 should have 2-space indentation")
	}

	// No Layer 3 - only 2 layers are generated
}
