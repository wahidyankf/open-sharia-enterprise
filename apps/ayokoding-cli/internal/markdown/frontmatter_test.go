package markdown

import (
	"os"
	"path/filepath"
	"testing"
)

func TestExtractFrontmatter(t *testing.T) {
	tests := []struct {
		name           string
		content        string
		expectedTitle  string
		expectedWeight int
		expectError    bool
	}{
		{
			name: "valid frontmatter with title and weight",
			content: `---
title: Test Page
weight: 100
---

Content here`,
			expectedTitle:  "Test Page",
			expectedWeight: 100,
			expectError:    false,
		},
		{
			name: "frontmatter with quoted title",
			content: `---
title: "Quoted Title"
weight: 50
---

Content`,
			expectedTitle:  "Quoted Title",
			expectedWeight: 50,
			expectError:    false,
		},
		{
			name: "frontmatter with single-quoted title",
			content: `---
title: 'Single Quoted'
weight: 25
---`,
			expectedTitle:  "Single Quoted",
			expectedWeight: 25,
			expectError:    false,
		},
		{
			name: "frontmatter without weight (uses default)",
			content: `---
title: No Weight
---`,
			expectedTitle:  "No Weight",
			expectedWeight: 999999,
			expectError:    false,
		},
		{
			name: "frontmatter without title",
			content: `---
weight: 10
---`,
			expectedTitle:  "test", // Falls back to filename
			expectedWeight: 10,
			expectError:    false,
		},
		{
			name: "no frontmatter",
			content: `This is just content
without frontmatter`,
			expectError: true,
		},
		{
			name: "malformed frontmatter (no closing)",
			content: `---
title: Test
weight: 10

Content without closing ---`,
			expectedTitle:  "Test",
			expectedWeight: 10,
			expectError:    false, // Current implementation accepts this
		},
		{
			name: "frontmatter with extra fields",
			content: `---
title: Full Frontmatter
date: 2025-12-20
draft: false
weight: 200
description: A test page
---

Content`,
			expectedTitle:  "Full Frontmatter",
			expectedWeight: 200,
			expectError:    false,
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Create temporary file
			tmpDir := t.TempDir()
			tmpFile := filepath.Join(tmpDir, "test.md")

			if err := os.WriteFile(tmpFile, []byte(tt.content), 0644); err != nil {
				t.Fatalf("Failed to create temp file: %v", err)
			}

			// Test extraction
			fm, err := ExtractFrontmatter(tmpFile)

			if tt.expectError {
				if err == nil {
					t.Errorf("Expected error but got none")
				}
				return
			}

			if err != nil {
				t.Fatalf("Unexpected error: %v", err)
			}

			if fm.Title != tt.expectedTitle {
				t.Errorf("Title mismatch: got %q, want %q", fm.Title, tt.expectedTitle)
			}

			if fm.Weight != tt.expectedWeight {
				t.Errorf("Weight mismatch: got %d, want %d", fm.Weight, tt.expectedWeight)
			}

			// Verify raw frontmatter is preserved
			if !tt.expectError && fm.Raw == "" {
				t.Error("Raw frontmatter should not be empty")
			}
		})
	}
}

func TestExtractFrontmatter_NonExistentFile(t *testing.T) {
	_, err := ExtractFrontmatter("/nonexistent/file.md")
	if err == nil {
		t.Error("Expected error for non-existent file")
	}
}

func TestExtractFrontmatter_RawPreservation(t *testing.T) {
	content := `---
title: Test
weight: 100
date: 2025-12-20
---

Content`

	tmpDir := t.TempDir()
	tmpFile := filepath.Join(tmpDir, "test.md")

	if err := os.WriteFile(tmpFile, []byte(content), 0644); err != nil {
		t.Fatalf("Failed to create temp file: %v", err)
	}

	fm, err := ExtractFrontmatter(tmpFile)
	if err != nil {
		t.Fatalf("Unexpected error: %v", err)
	}

	expectedRaw := `---
title: Test
weight: 100
date: 2025-12-20
---
`

	if fm.Raw != expectedRaw {
		t.Errorf("Raw frontmatter not preserved correctly.\nGot:\n%s\nWant:\n%s", fm.Raw, expectedRaw)
	}
}
