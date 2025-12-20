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
				{Title: "Overview", Path: "overview", Weight: 1, IsDir: false},
			},
			layer:    0,
			expected: "- [Overview](overview)\n",
		},
		{
			name: "multiple items at layer 0",
			items: []Item{
				{Title: "Overview", Path: "overview", Weight: 1, IsDir: false},
				{Title: "Installation", Path: "installation", Weight: 2, IsDir: false},
				{Title: "Getting Started", Path: "getting-started", Weight: 3, IsDir: false},
			},
			layer: 0,
			expected: `- [Overview](overview)
- [Installation](installation)
- [Getting Started](getting-started)
`,
		},
		{
			name: "items with children (DFS structure)",
			items: []Item{
				{Title: "Overview", Path: "overview", Weight: 1, IsDir: false},
				{
					Title:  "Tutorials",
					Path:   "tutorials",
					Weight: 2,
					IsDir:  true,
					Children: []Item{
						{Title: "Beginner", Path: "beginner", Weight: 1, IsDir: false},
						{Title: "Advanced", Path: "advanced", Weight: 2, IsDir: false},
					},
				},
			},
			layer: 0,
			expected: `- [Overview](overview)
- [Tutorials](tutorials)
  - [Beginner](tutorials/beginner)
  - [Advanced](tutorials/advanced)
`,
		},
		{
			name: "nested children (3 layers)",
			items: []Item{
				{
					Title:  "Learn",
					Path:   "learn",
					Weight: 1,
					IsDir:  true,
					Children: []Item{
						{
							Title:  "Programming",
							Path:   "programming",
							Weight: 1,
							IsDir:  true,
							Children: []Item{
								{Title: "Python", Path: "python", Weight: 1, IsDir: false},
								{Title: "Go", Path: "go", Weight: 2, IsDir: false},
							},
						},
					},
				},
			},
			layer: 0,
			expected: `- [Learn](learn)
  - [Programming](learn/programming)
    - [Python](learn/programming/python)
    - [Go](learn/programming/go)
`,
		},
		{
			name: "items at layer 1 (2-space indentation)",
			items: []Item{
				{Title: "Overview", Path: "tutorials/overview", Weight: 1, IsDir: false},
				{Title: "Beginner", Path: "tutorials/beginner", Weight: 2, IsDir: false},
			},
			layer: 1,
			expected: `  - [Overview](tutorials/overview)
  - [Beginner](tutorials/beginner)
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
		{Title: "Overview", Path: "overview", Weight: 1, IsDir: false},
		{
			Title:  "Tutorials",
			Path:   "tutorials",
			Weight: 2,
			IsDir:  true,
			Children: []Item{
				{Title: "Beginner", Path: "beginner", Weight: 1, IsDir: false},
			},
		},
	}

	expected := `---
title: Test Page
weight: 100
---

- [Overview](overview)
- [Tutorials](tutorials)
  - [Beginner](tutorials/beginner)
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
	// Test that child paths are correctly prefixed with parent path
	items := []Item{
		{
			Title:  "Parent",
			Path:   "parent",
			Weight: 1,
			IsDir:  true,
			Children: []Item{
				{Title: "Child1", Path: "child1", Weight: 1, IsDir: false},
				{Title: "Child2", Path: "child2", Weight: 2, IsDir: false},
			},
		},
	}

	result := GenerateMarkdown(items, 0)

	// Verify that child paths are prefixed with parent path
	if !strings.Contains(result, "[Child1](parent/child1)") {
		t.Error("Child1 path not correctly prefixed with parent path")
	}
	if !strings.Contains(result, "[Child2](parent/child2)") {
		t.Error("Child2 path not correctly prefixed with parent path")
	}
}

func TestGenerateMarkdown_Indentation(t *testing.T) {
	items := []Item{
		{
			Title:  "L1",
			Path:   "l1",
			Weight: 1,
			IsDir:  true,
			Children: []Item{
				{
					Title:  "L2",
					Path:   "l2",
					Weight: 1,
					IsDir:  true,
					Children: []Item{
						{Title: "L3", Path: "l3", Weight: 1, IsDir: false},
					},
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

	// Layer 3: 4 spaces
	if !strings.HasPrefix(lines[2], "    - [L3]") {
		t.Error("Layer 3 should have 4-space indentation")
	}
}
