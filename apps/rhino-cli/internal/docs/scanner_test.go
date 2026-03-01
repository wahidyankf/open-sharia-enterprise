package docs

import (
	"os"
	"path/filepath"
	"strings"
	"testing"
)

func TestGetAllDocsFiles(t *testing.T) {
	// Create temporary test directory structure
	tmpDir := t.TempDir()
	docsDir := filepath.Join(tmpDir, "docs")

	// Create test directories
	dirs := []string{
		"docs/tutorials",
		"docs/how-to",
		"docs/reference",
		"docs/explanation/software",
		"docs/metadata",
	}
	for _, dir := range dirs {
		if err := os.MkdirAll(filepath.Join(tmpDir, dir), 0755); err != nil {
			t.Fatalf("Failed to create dir %s: %v", dir, err)
		}
	}

	// Create test files
	files := []string{
		"docs/tutorials/tu__getting-started.md",
		"docs/tutorials/README.md",
		"docs/how-to/hoto__deploy.md",
		"docs/reference/re__api.md",
		"docs/explanation/software/ex-so__overview.md",
		"docs/metadata/cache.yaml",
	}
	for _, file := range files {
		path := filepath.Join(tmpDir, file)
		if err := os.WriteFile(path, []byte("# Test"), 0644); err != nil {
			t.Fatalf("Failed to create file %s: %v", file, err)
		}
	}

	// Test getAllDocsFiles
	gotFiles, err := getAllDocsFiles(tmpDir)
	if err != nil {
		t.Fatalf("getAllDocsFiles() error = %v", err)
	}

	// Should find all 6 files
	if len(gotFiles) != len(files) {
		t.Errorf("getAllDocsFiles() found %d files, want %d", len(gotFiles), len(files))
		for _, f := range gotFiles {
			t.Logf("  Found: %s", f)
		}
	}

	// Verify all files are under docs/
	for _, f := range gotFiles {
		rel, err := filepath.Rel(tmpDir, f)
		if err != nil {
			t.Errorf("Failed to get relative path for %s: %v", f, err)
			continue
		}
		if filepath.Dir(rel) == "." || !strings.HasPrefix(rel, "docs/") {
			t.Errorf("File %s is not under docs/", f)
		}
	}

	// Test with non-existent docs directory
	emptyDir := t.TempDir()
	emptyFiles, err := getAllDocsFiles(emptyDir)
	if err != nil {
		t.Errorf("getAllDocsFiles() on empty dir error = %v", err)
	}
	if len(emptyFiles) != 0 {
		t.Errorf("getAllDocsFiles() on empty dir found %d files, want 0", len(emptyFiles))
	}

	_ = docsDir // avoid unused variable warning
}

func TestGetRelativePath(t *testing.T) {
	tests := []struct {
		name     string
		filePath string
		repoRoot string
		expected string
	}{
		{
			name:     "simple path",
			filePath: "/home/user/project/docs/tutorials/tu__file.md",
			repoRoot: "/home/user/project",
			expected: "docs/tutorials/tu__file.md",
		},
		{
			name:     "nested path",
			filePath: "/project/docs/explanation/software/prog-lang/ex-so-prla__file.md",
			repoRoot: "/project",
			expected: "docs/explanation/software/prog-lang/ex-so-prla__file.md",
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			got, err := GetRelativePath(tt.filePath, tt.repoRoot)
			if err != nil {
				t.Errorf("GetRelativePath() error = %v", err)
				return
			}
			if got != tt.expected {
				t.Errorf("GetRelativePath() = %q, want %q", got, tt.expected)
			}
		})
	}
}

func TestGetDocsFiles_NonStaged(t *testing.T) {
	tmpDir := t.TempDir()
	docsDir := filepath.Join(tmpDir, "docs")
	if err := os.MkdirAll(docsDir, 0755); err != nil {
		t.Fatalf("failed to create docs dir: %v", err)
	}
	if err := os.WriteFile(filepath.Join(docsDir, "file.md"), []byte("# Content"), 0644); err != nil {
		t.Fatalf("failed to create file: %v", err)
	}

	opts := ValidationOptions{
		RepoRoot:   tmpDir,
		StagedOnly: false,
	}

	files, err := GetDocsFiles(opts)
	if err != nil {
		t.Fatalf("GetDocsFiles() error: %v", err)
	}
	if len(files) != 1 {
		t.Errorf("expected 1 file, got %d: %v", len(files), files)
	}
}

func TestGetDocsFiles_NonExistentDocsDir(t *testing.T) {
	tmpDir := t.TempDir()
	// No docs dir
	opts := ValidationOptions{
		RepoRoot:   tmpDir,
		StagedOnly: false,
	}

	files, err := GetDocsFiles(opts)
	if err != nil {
		t.Fatalf("GetDocsFiles() should not error for non-existent docs dir: %v", err)
	}
	if len(files) != 0 {
		t.Errorf("expected 0 files, got %v", files)
	}
}
