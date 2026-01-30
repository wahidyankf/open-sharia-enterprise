package docs

import (
	"os"
	"path/filepath"
	"strings"
	"testing"
)

func TestUpdateLinkIfNeeded(t *testing.T) {
	filenameMap := map[string]string{
		"wrong__getting-started.md": "tu__getting-started.md",
		"old__file.md":              "hoto__file.md",
		"ex-so__basics.md":          "ex-so-prla-py__basics.md",
	}

	tests := []struct {
		name     string
		link     string
		expected string
	}{
		{
			name:     "simple relative link - needs update",
			link:     "wrong__getting-started.md",
			expected: "tu__getting-started.md",
		},
		{
			name:     "relative link with directory - needs update",
			link:     "../tutorials/wrong__getting-started.md",
			expected: "../tutorials/tu__getting-started.md",
		},
		{
			name:     "absolute link - needs update",
			link:     "/docs/how-to/old__file.md",
			expected: "/docs/how-to/hoto__file.md",
		},
		{
			name:     "link with anchor - needs update",
			link:     "wrong__getting-started.md#section-1",
			expected: "tu__getting-started.md#section-1",
		},
		{
			name:     "deep path link with anchor - needs update",
			link:     "../explanation/software/ex-so__basics.md#intro",
			expected: "../explanation/software/ex-so-prla-py__basics.md#intro",
		},
		{
			name:     "link not in rename map",
			link:     "tu__other-file.md",
			expected: "", // empty means no update needed
		},
		{
			name:     "external http link",
			link:     "https://example.com/wrong__getting-started.md",
			expected: "", // skip external URLs
		},
		{
			name:     "external https link",
			link:     "https://github.com/repo/file.md",
			expected: "",
		},
		{
			name:     "mailto link",
			link:     "mailto:test@example.com",
			expected: "",
		},
		{
			name:     "anchor-only link",
			link:     "#section-heading",
			expected: "",
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			got := updateLinkIfNeeded(tt.link, filenameMap)
			if got != tt.expected {
				t.Errorf("updateLinkIfNeeded(%q) = %q, want %q", tt.link, got, tt.expected)
			}
		})
	}
}

func TestScanFileForLinks(t *testing.T) {
	// Create temp directory and test file
	tempDir := t.TempDir()

	filenameMap := map[string]string{
		"wrong__file.md": "tu__file.md",
		"old__ref.md":    "re__ref.md",
	}

	tests := []struct {
		name        string
		content     string
		wantCount   int
		wantUpdates []LinkUpdate
	}{
		{
			name:      "file with no links",
			content:   "# Heading\n\nSome text without links.\n",
			wantCount: 0,
		},
		{
			name:      "file with link to rename",
			content:   "# Heading\n\nSee [this file](wrong__file.md) for more.\n",
			wantCount: 1,
			wantUpdates: []LinkUpdate{
				{LineNumber: 3, OldLink: "wrong__file.md", NewLink: "tu__file.md"},
			},
		},
		{
			name:      "file with multiple links",
			content:   "# Heading\n\n[Link 1](wrong__file.md) and [Link 2](old__ref.md).\n",
			wantCount: 2,
		},
		{
			name:      "file with links not in map",
			content:   "# Heading\n\n[External](https://example.com) and [other](other-file.md).\n",
			wantCount: 0,
		},
		{
			name: "file with code block containing links",
			content: `# Heading

` + "```markdown" + `
[Link in code](wrong__file.md)
` + "```" + `

[Real link](wrong__file.md)
`,
			wantCount: 1, // only the link outside code block
		},
		{
			name:      "link with path components",
			content:   "See [tutorial](../tutorials/wrong__file.md) for info.\n",
			wantCount: 1,
			wantUpdates: []LinkUpdate{
				{LineNumber: 1, OldLink: "../tutorials/wrong__file.md", NewLink: "../tutorials/tu__file.md"},
			},
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Create test file
			testFile := filepath.Join(tempDir, "test.md")
			if err := os.WriteFile(testFile, []byte(tt.content), 0644); err != nil {
				t.Fatalf("failed to create test file: %v", err)
			}

			updates, err := scanFileForLinks(testFile, filenameMap, tempDir)
			if err != nil {
				t.Fatalf("scanFileForLinks() error: %v", err)
			}

			if len(updates) != tt.wantCount {
				t.Errorf("got %d updates, want %d", len(updates), tt.wantCount)
			}

			// Verify specific updates if provided
			for i, want := range tt.wantUpdates {
				if i >= len(updates) {
					break
				}
				got := updates[i]
				if got.LineNumber != want.LineNumber {
					t.Errorf("update[%d].LineNumber = %d, want %d", i, got.LineNumber, want.LineNumber)
				}
				if got.OldLink != want.OldLink {
					t.Errorf("update[%d].OldLink = %q, want %q", i, got.OldLink, want.OldLink)
				}
				if got.NewLink != want.NewLink {
					t.Errorf("update[%d].NewLink = %q, want %q", i, got.NewLink, want.NewLink)
				}
			}
		})
	}
}

func TestFindLinksToUpdate(t *testing.T) {
	// Create temp directory with test files
	tempDir := t.TempDir()

	// Create docs directory structure
	docsDir := filepath.Join(tempDir, "docs", "tutorials")
	if err := os.MkdirAll(docsDir, 0755); err != nil {
		t.Fatalf("failed to create docs directory: %v", err)
	}

	// Create a markdown file with links
	fileContent := `# Tutorial Index

See [getting started](wrong__getting-started.md) for basics.
Also check [reference](../reference/old__api.md) for API docs.
`
	testFile := filepath.Join(docsDir, "README.md")
	if err := os.WriteFile(testFile, []byte(fileContent), 0644); err != nil {
		t.Fatalf("failed to create test file: %v", err)
	}

	renames := []RenameOperation{
		{
			OldPath: "docs/tutorials/wrong__getting-started.md",
			NewPath: "docs/tutorials/tu__getting-started.md",
			OldName: "wrong__getting-started.md",
			NewName: "tu__getting-started.md",
		},
		{
			OldPath: "docs/reference/old__api.md",
			NewPath: "docs/reference/re__api.md",
			OldName: "old__api.md",
			NewName: "re__api.md",
		},
	}

	updates, err := FindLinksToUpdate(renames, tempDir)
	if err != nil {
		t.Fatalf("FindLinksToUpdate() error: %v", err)
	}

	// Should find at least the links in our test file
	if len(updates) < 2 {
		t.Errorf("expected at least 2 updates, got %d", len(updates))
	}

	// Verify that updates contain expected data
	foundGettingStarted := false
	foundApi := false
	for _, u := range updates {
		if strings.Contains(u.OldLink, "wrong__getting-started.md") {
			foundGettingStarted = true
			if !strings.Contains(u.NewLink, "tu__getting-started.md") {
				t.Errorf("expected new link to contain 'tu__getting-started.md', got %q", u.NewLink)
			}
		}
		if strings.Contains(u.OldLink, "old__api.md") {
			foundApi = true
			if !strings.Contains(u.NewLink, "re__api.md") {
				t.Errorf("expected new link to contain 're__api.md', got %q", u.NewLink)
			}
		}
	}

	if !foundGettingStarted {
		t.Error("expected to find update for wrong__getting-started.md link")
	}
	if !foundApi {
		t.Error("expected to find update for old__api.md link")
	}
}

func TestApplyLinkUpdates(t *testing.T) {
	tempDir := t.TempDir()

	// Create a test file with links to update
	originalContent := `# Test Document

See [getting started](wrong__getting-started.md) for basics.
Check [API reference](old__api.md) for details.
`
	testFile := filepath.Join(tempDir, "test.md")
	if err := os.WriteFile(testFile, []byte(originalContent), 0644); err != nil {
		t.Fatalf("failed to create test file: %v", err)
	}

	updates := []LinkUpdate{
		{
			FilePath:   "test.md",
			LineNumber: 3,
			OldLink:    "wrong__getting-started.md",
			NewLink:    "tu__getting-started.md",
		},
		{
			FilePath:   "test.md",
			LineNumber: 4,
			OldLink:    "old__api.md",
			NewLink:    "re__api.md",
		},
	}

	applied, errors := ApplyLinkUpdates(updates, tempDir)

	if len(errors) > 0 {
		t.Errorf("unexpected errors: %v", errors)
	}

	if applied != 2 {
		t.Errorf("expected 2 links applied, got %d", applied)
	}

	// Verify file contents were updated
	content, err := os.ReadFile(testFile)
	if err != nil {
		t.Fatalf("failed to read updated file: %v", err)
	}

	contentStr := string(content)
	if !strings.Contains(contentStr, "tu__getting-started.md") {
		t.Error("expected file to contain 'tu__getting-started.md'")
	}
	if !strings.Contains(contentStr, "re__api.md") {
		t.Error("expected file to contain 're__api.md'")
	}
	if strings.Contains(contentStr, "wrong__getting-started.md") {
		t.Error("file should not contain old link 'wrong__getting-started.md'")
	}
	if strings.Contains(contentStr, "old__api.md") {
		t.Error("file should not contain old link 'old__api.md'")
	}
}

func TestApplyLinkUpdates_FileNotFound(t *testing.T) {
	tempDir := t.TempDir()

	updates := []LinkUpdate{
		{
			FilePath:   "nonexistent.md",
			LineNumber: 1,
			OldLink:    "old.md",
			NewLink:    "new.md",
		},
	}

	applied, errors := ApplyLinkUpdates(updates, tempDir)

	if applied != 0 {
		t.Errorf("expected 0 applied for nonexistent file, got %d", applied)
	}

	if len(errors) != 1 {
		t.Errorf("expected 1 error for nonexistent file, got %d", len(errors))
	}
}

func TestApplyLinkUpdates_MultipleFiles(t *testing.T) {
	tempDir := t.TempDir()

	// Create two test files
	file1Content := "Link: [file](old__file.md)\n"
	file1 := filepath.Join(tempDir, "file1.md")
	if err := os.WriteFile(file1, []byte(file1Content), 0644); err != nil {
		t.Fatalf("failed to create file1: %v", err)
	}

	file2Content := "Another: [link](old__file.md)\n"
	file2 := filepath.Join(tempDir, "file2.md")
	if err := os.WriteFile(file2, []byte(file2Content), 0644); err != nil {
		t.Fatalf("failed to create file2: %v", err)
	}

	updates := []LinkUpdate{
		{FilePath: "file1.md", LineNumber: 1, OldLink: "old__file.md", NewLink: "new__file.md"},
		{FilePath: "file2.md", LineNumber: 1, OldLink: "old__file.md", NewLink: "new__file.md"},
	}

	applied, errors := ApplyLinkUpdates(updates, tempDir)

	if len(errors) > 0 {
		t.Errorf("unexpected errors: %v", errors)
	}

	if applied != 2 {
		t.Errorf("expected 2 applied, got %d", applied)
	}

	// Verify both files updated
	for _, f := range []string{file1, file2} {
		content, err := os.ReadFile(f)
		if err != nil {
			t.Fatalf("failed to read %s: %v", f, err)
		}
		if !strings.Contains(string(content), "new__file.md") {
			t.Errorf("%s should contain 'new__file.md'", f)
		}
	}
}

func TestGetMarkdownFilesToScan(t *testing.T) {
	tempDir := t.TempDir()

	// Create directory structure
	dirs := []string{
		"docs/tutorials",
		"docs/reference",
		"governance/conventions",
	}
	for _, dir := range dirs {
		if err := os.MkdirAll(filepath.Join(tempDir, dir), 0755); err != nil {
			t.Fatalf("failed to create directory %s: %v", dir, err)
		}
	}

	// Create some markdown files
	files := []string{
		"docs/tutorials/tu__test.md",
		"docs/reference/re__api.md",
		"governance/conventions/naming.md",
		"README.md",
	}
	for _, f := range files {
		path := filepath.Join(tempDir, f)
		if err := os.WriteFile(path, []byte("# Test\n"), 0644); err != nil {
			t.Fatalf("failed to create file %s: %v", f, err)
		}
	}

	// Also create a non-markdown file that should be skipped
	if err := os.WriteFile(filepath.Join(tempDir, "docs/tutorials/config.json"), []byte("{}"), 0644); err != nil {
		t.Fatalf("failed to create json file: %v", err)
	}

	foundFiles, err := getMarkdownFilesToScan(tempDir)
	if err != nil {
		t.Fatalf("getMarkdownFilesToScan() error: %v", err)
	}

	// Should find all markdown files
	if len(foundFiles) < 4 {
		t.Errorf("expected at least 4 markdown files, got %d", len(foundFiles))
	}

	// Check that non-markdown files are not included
	for _, f := range foundFiles {
		if !strings.HasSuffix(f, ".md") {
			t.Errorf("found non-markdown file: %s", f)
		}
	}
}
