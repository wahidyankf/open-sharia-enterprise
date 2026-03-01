package fileutil

import (
	"os"
	"os/exec"
	"path/filepath"
	"strings"
	"testing"
)

func TestWalkMarkdownDirs_SingleDir(t *testing.T) {
	tmpDir := t.TempDir()

	docsDir := filepath.Join(tmpDir, "docs")
	if err := os.MkdirAll(docsDir, 0755); err != nil {
		t.Fatalf("failed to create docs dir: %v", err)
	}

	for _, name := range []string{"file1.md", "file2.md"} {
		if err := os.WriteFile(filepath.Join(docsDir, name), []byte("# Content"), 0644); err != nil {
			t.Fatalf("failed to create %s: %v", name, err)
		}
	}
	// Non-.md file should be excluded
	if err := os.WriteFile(filepath.Join(docsDir, "other.txt"), []byte("text"), 0644); err != nil {
		t.Fatalf("failed to create other.txt: %v", err)
	}

	files, err := WalkMarkdownDirs(tmpDir, []string{"docs"})
	if err != nil {
		t.Fatalf("WalkMarkdownDirs() error: %v", err)
	}

	var mdFiles []string
	for _, f := range files {
		if strings.HasSuffix(f, ".md") {
			mdFiles = append(mdFiles, f)
		}
	}
	if len(mdFiles) != 2 {
		t.Errorf("expected 2 .md files, got %d: %v", len(mdFiles), files)
	}
}

func TestWalkMarkdownDirs_NonExistentDir(t *testing.T) {
	tmpDir := t.TempDir()

	// Non-existent dir should be silently skipped
	files, err := WalkMarkdownDirs(tmpDir, []string{"nonexistent"})
	if err != nil {
		t.Fatalf("WalkMarkdownDirs() should not error for nonexistent dir, got: %v", err)
	}
	// At most root-level .md files
	for _, f := range files {
		if strings.Contains(f, "nonexistent") {
			t.Errorf("expected nonexistent dir to be skipped, got file: %v", f)
		}
	}
}

func TestWalkMarkdownDirs_RootLevelMdFiles(t *testing.T) {
	tmpDir := t.TempDir()

	if err := os.WriteFile(filepath.Join(tmpDir, "README.md"), []byte("# Readme"), 0644); err != nil {
		t.Fatalf("failed to create README: %v", err)
	}

	files, err := WalkMarkdownDirs(tmpDir, []string{})
	if err != nil {
		t.Fatalf("WalkMarkdownDirs() error: %v", err)
	}

	found := false
	for _, f := range files {
		if filepath.Base(f) == "README.md" {
			found = true
			break
		}
	}
	if !found {
		t.Errorf("expected README.md in results, got %v", files)
	}
}

func TestWalkMarkdownDirs_MultipleDirs(t *testing.T) {
	tmpDir := t.TempDir()

	for _, dir := range []string{"docs", "governance"} {
		d := filepath.Join(tmpDir, dir)
		if err := os.MkdirAll(d, 0755); err != nil {
			t.Fatalf("failed to create dir: %v", err)
		}
		if err := os.WriteFile(filepath.Join(d, "file.md"), []byte("# Content"), 0644); err != nil {
			t.Fatalf("failed to create file: %v", err)
		}
	}

	files, err := WalkMarkdownDirs(tmpDir, []string{"docs", "governance"})
	if err != nil {
		t.Fatalf("WalkMarkdownDirs() error: %v", err)
	}

	mdCount := 0
	for _, f := range files {
		if strings.HasSuffix(f, ".md") {
			mdCount++
		}
	}
	if mdCount < 2 {
		t.Errorf("expected at least 2 .md files, got %d: %v", mdCount, files)
	}
}

func TestWalkMarkdownDirs_NestedSubdirs(t *testing.T) {
	tmpDir := t.TempDir()

	nestedDir := filepath.Join(tmpDir, "docs", "tutorials", "sub")
	if err := os.MkdirAll(nestedDir, 0755); err != nil {
		t.Fatalf("failed to create nested dir: %v", err)
	}
	if err := os.WriteFile(filepath.Join(nestedDir, "deep.md"), []byte("# Deep"), 0644); err != nil {
		t.Fatalf("failed to create deep.md: %v", err)
	}

	files, err := WalkMarkdownDirs(tmpDir, []string{"docs"})
	if err != nil {
		t.Fatalf("WalkMarkdownDirs() error: %v", err)
	}

	found := false
	for _, f := range files {
		if filepath.Base(f) == "deep.md" {
			found = true
			break
		}
	}
	if !found {
		t.Errorf("expected deep.md in nested subdir, got %v", files)
	}
}

func TestGetStagedFilesFiltered_InGitRepo(t *testing.T) {
	tmpDir := t.TempDir()

	// Initialize a git repo so git commands succeed
	if err := exec.Command("git", "-C", tmpDir, "init").Run(); err != nil {
		t.Skipf("git not available: %v", err)
	}
	// Configure git identity for this repo
	_ = exec.Command("git", "-C", tmpDir, "config", "user.email", "test@test.com").Run()
	_ = exec.Command("git", "-C", tmpDir, "config", "user.name", "Test").Run()

	// With no staged files, should return empty list
	files, err := GetStagedFilesFiltered(tmpDir, func(f string) bool {
		return strings.HasSuffix(f, ".md")
	})
	if err != nil {
		t.Fatalf("GetStagedFilesFiltered() error: %v", err)
	}
	if len(files) != 0 {
		t.Errorf("expected 0 staged files, got %d: %v", len(files), files)
	}
}

func TestGetStagedFilesFiltered_WithStagedFiles(t *testing.T) {
	tmpDir := t.TempDir()

	// Initialize git repo
	if err := exec.Command("git", "-C", tmpDir, "init").Run(); err != nil {
		t.Skipf("git not available: %v", err)
	}
	_ = exec.Command("git", "-C", tmpDir, "config", "user.email", "test@test.com").Run()
	_ = exec.Command("git", "-C", tmpDir, "config", "user.name", "Test").Run()

	// Create and stage a .md file
	mdFile := filepath.Join(tmpDir, "test.md")
	if err := os.WriteFile(mdFile, []byte("# Test"), 0644); err != nil {
		t.Fatalf("failed to create file: %v", err)
	}
	if err := exec.Command("git", "-C", tmpDir, "add", "test.md").Run(); err != nil {
		t.Fatalf("failed to stage file: %v", err)
	}

	// Also create a non-.md file (should be filtered out)
	txtFile := filepath.Join(tmpDir, "other.txt")
	if err := os.WriteFile(txtFile, []byte("text"), 0644); err != nil {
		t.Fatalf("failed to create txt file: %v", err)
	}
	if err := exec.Command("git", "-C", tmpDir, "add", "other.txt").Run(); err != nil {
		t.Fatalf("failed to stage txt file: %v", err)
	}

	// Filter for .md files only
	files, err := GetStagedFilesFiltered(tmpDir, func(f string) bool {
		return strings.HasSuffix(f, ".md")
	})
	if err != nil {
		t.Fatalf("GetStagedFilesFiltered() error: %v", err)
	}
	if len(files) != 1 {
		t.Errorf("expected 1 .md file, got %d: %v", len(files), files)
	}
	if len(files) > 0 && filepath.Base(files[0]) != "test.md" {
		t.Errorf("expected test.md, got %v", files[0])
	}
}
