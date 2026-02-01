package titles

import (
	"os"
	"path/filepath"
	"testing"
)

func TestUpdateTitleInFile(t *testing.T) {
	tests := []struct {
		name            string
		originalContent string
		newTitle        string
		expectedContent string
		expectError     bool
	}{
		{
			name: "update simple title",
			originalContent: `---
title: Old Title
weight: 100
---

Content here`,
			newTitle: "New Title",
			expectedContent: `---
title: "New Title"
weight: 100
---

Content here`,
			expectError: false,
		},
		{
			name: "update quoted title",
			originalContent: `---
title: "Old Quoted Title"
weight: 50
---

Content`,
			newTitle: "New Quoted Title",
			expectedContent: `---
title: "New Quoted Title"
weight: 50
---

Content`,
			expectError: false,
		},
		{
			name: "update single-quoted title",
			originalContent: `---
title: 'Old Single Quoted'
weight: 25
---`,
			newTitle: "New Single Quoted",
			expectedContent: `---
title: "New Single Quoted"
weight: 25
---`,
			expectError: false,
		},
		{
			name: "update title with special characters",
			originalContent: `---
title: Old Title
date: 2025-12-20
draft: false
---

Content`,
			newTitle: "Node.js Tutorial",
			expectedContent: `---
title: "Node.js Tutorial"
date: 2025-12-20
draft: false
---

Content`,
			expectError: false,
		},
		{
			name: "title with quotes in value",
			originalContent: `---
title: Old Title
---`,
			newTitle: `Title with "Quotes"`,
			expectedContent: `---
title: "Title with \"Quotes\""
---`,
			expectError: false,
		},
		{
			name: "no frontmatter",
			originalContent: `This is just content
without frontmatter`,
			newTitle:    "New Title",
			expectError: true,
		},
		{
			name: "frontmatter without title field (regex adds it)",
			originalContent: `---
weight: 100
date: 2025-12-20
---

Content`,
			newTitle: "New Title",
			expectedContent: `---
weight: 100
date: 2025-12-20
---

Content`,
			expectError: false, // Regex won't find title to replace, file unchanged
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Create temporary file
			tmpDir := t.TempDir()
			tmpFile := filepath.Join(tmpDir, "test.md")

			if err := os.WriteFile(tmpFile, []byte(tt.originalContent), 0644); err != nil {
				t.Fatalf("Failed to create temp file: %v", err)
			}

			// Update title
			err := updateTitleInFile(tmpFile, tt.newTitle)

			if tt.expectError {
				if err == nil {
					t.Error("Expected error but got none")
				}
				return
			}

			if err != nil {
				t.Fatalf("Unexpected error: %v", err)
			}

			// Read updated content
			updatedContent, err := os.ReadFile(tmpFile)
			if err != nil {
				t.Fatalf("Failed to read updated file: %v", err)
			}

			if string(updatedContent) != tt.expectedContent {
				t.Errorf("Content mismatch.\nGot:\n%s\nWant:\n%s", string(updatedContent), tt.expectedContent)
			}
		})
	}
}

func TestProcessFile(t *testing.T) {
	config := &Config{
		Overrides: map[string]string{
			"javascript": "JavaScript",
		},
		LowercaseWords: []string{"and"},
	}

	tests := []struct {
		name          string
		filename      string
		originalTitle string
		expectedTitle string
		dryRun        bool
		expectSkip    bool
		expectUpdate  bool
	}{
		{
			name:          "title needs update",
			filename:      "javascript-basics.md",
			originalTitle: "Javascript Basics",
			expectedTitle: "JavaScript Basics",
			dryRun:        false,
			expectUpdate:  true,
		},
		{
			name:          "title already correct - skip",
			filename:      "javascript-basics.md",
			originalTitle: "JavaScript Basics",
			expectedTitle: "JavaScript Basics",
			dryRun:        false,
			expectSkip:    true,
		},
		{
			name:          "dry run - count but don't update",
			filename:      "terms-and-conditions.md",
			originalTitle: "Terms And Conditions",
			expectedTitle: "Terms and Conditions",
			dryRun:        true,
			expectUpdate:  true, // Counted as update but file not modified
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Create temporary file with frontmatter
			tmpDir := t.TempDir()
			tmpFile := filepath.Join(tmpDir, tt.filename)

			content := "---\ntitle: " + tt.originalTitle + "\nweight: 100\n---\n\nContent"
			if err := os.WriteFile(tmpFile, []byte(content), 0644); err != nil {
				t.Fatalf("Failed to create temp file: %v", err)
			}

			// Process file
			result := &LangResult{
				Errors: []string{},
			}
			err := processFile(tmpFile, config, tt.dryRun, result)

			if err != nil {
				t.Fatalf("Unexpected error: %v", err)
			}

			// Verify counts
			if tt.expectSkip && result.SkippedCount != 1 {
				t.Errorf("Expected skip count 1, got %d", result.SkippedCount)
			}

			if tt.expectUpdate && result.UpdatedCount != 1 {
				t.Errorf("Expected update count 1, got %d", result.UpdatedCount)
			}

			// For non-dry-run, verify file was actually updated
			if !tt.dryRun && tt.expectUpdate {
				updatedContent, err := os.ReadFile(tmpFile)
				if err != nil {
					t.Fatalf("Failed to read updated file: %v", err)
				}

				expectedContent := "---\ntitle: \"" + tt.expectedTitle + "\"\nweight: 100\n---\n\nContent"
				if string(updatedContent) != expectedContent {
					t.Errorf("File not updated correctly.\nGot:\n%s\nWant:\n%s", string(updatedContent), expectedContent)
				}
			}

			// For dry-run, verify file was NOT modified
			if tt.dryRun && tt.expectUpdate {
				unchangedContent, err := os.ReadFile(tmpFile)
				if err != nil {
					t.Fatalf("Failed to read file: %v", err)
				}

				if string(unchangedContent) != content {
					t.Error("File was modified during dry-run (should not be)")
				}
			}
		})
	}
}

func TestProcessLanguageDirectory(t *testing.T) {
	config := &Config{
		Overrides:      map[string]string{"javascript": "JavaScript"},
		LowercaseWords: []string{"and"},
	}

	// Create temporary directory structure
	tmpDir := t.TempDir()
	enDir := filepath.Join(tmpDir, "en")
	learnDir := filepath.Join(enDir, "learn")

	if err := os.MkdirAll(learnDir, 0755); err != nil {
		t.Fatalf("Failed to create directories: %v", err)
	}

	// Create test files
	testFiles := map[string]string{
		filepath.Join(enDir, "about.md"):                   "title: About\n",
		filepath.Join(learnDir, "programming-language.md"): "title: Programming Language\n", // Already correct
		filepath.Join(learnDir, "javascript-basics.md"):    "title: Javascript Basics\n",    // Needs update
	}

	for path, title := range testFiles {
		content := "---\n" + title + "weight: 100\n---\n\nContent"
		if err := os.WriteFile(path, []byte(content), 0644); err != nil {
			t.Fatalf("Failed to create test file %s: %v", path, err)
		}
	}

	// Process directory
	result, err := processLanguageDirectory(enDir, config, false)
	if err != nil {
		t.Fatalf("Unexpected error: %v", err)
	}

	// Verify results
	expectedUpdated := 1 // javascript-basics.md
	expectedSkipped := 2 // about.md, programming-language.md

	if result.UpdatedCount != expectedUpdated {
		t.Errorf("Updated count: got %d, want %d", result.UpdatedCount, expectedUpdated)
	}

	if result.SkippedCount != expectedSkipped {
		t.Errorf("Skipped count: got %d, want %d", result.SkippedCount, expectedSkipped)
	}

	if result.ErrorCount != 0 {
		t.Errorf("Error count: got %d, want 0. Errors: %v", result.ErrorCount, result.Errors)
	}
}

func TestProcessLanguageDirectory_NonExistent(t *testing.T) {
	config := &Config{
		Overrides:      map[string]string{},
		LowercaseWords: []string{},
	}

	_, err := processLanguageDirectory("/nonexistent/directory", config, false)
	if err == nil {
		t.Error("Expected error for non-existent directory")
	}
}

func TestEscapeQuotes(t *testing.T) {
	tests := []struct {
		input    string
		expected string
	}{
		{`Simple Title`, `Simple Title`},
		{`Title with "quotes"`, `Title with \"quotes\"`},
		{`Multiple "quotes" and "more"`, `Multiple \"quotes\" and \"more\"`},
		{`No quotes here`, `No quotes here`},
		{``, ``},
	}

	for _, tt := range tests {
		t.Run(tt.input, func(t *testing.T) {
			result := escapeQuotes(tt.input)
			if result != tt.expected {
				t.Errorf("escapeQuotes(%q) = %q, want %q", tt.input, result, tt.expected)
			}
		})
	}
}
