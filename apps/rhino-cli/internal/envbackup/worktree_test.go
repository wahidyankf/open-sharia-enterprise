package envbackup

import (
	"os"
	"path/filepath"
	"testing"
)

// makeDir creates a directory and returns its path.
func makeDir(t *testing.T, base, name string) string {
	t.Helper()
	dir := filepath.Join(base, name)
	if err := os.MkdirAll(dir, 0o755); err != nil {
		t.Fatalf("makeDir %s: %v", dir, err)
	}
	return dir
}

// writeFile writes content to a file, creating parent directories as needed.
func writeFile(t *testing.T, path, content string) {
	t.Helper()
	if err := os.MkdirAll(filepath.Dir(path), 0o755); err != nil {
		t.Fatalf("writeFile mkdir %s: %v", path, err)
	}
	if err := os.WriteFile(path, []byte(content), 0o644); err != nil {
		t.Fatalf("writeFile %s: %v", path, err)
	}
}

func TestDetectWorktree_NormalRepo(t *testing.T) {
	tmp := t.TempDir()
	repoRoot := makeDir(t, tmp, "myrepo")
	// Normal repo: .git is a directory.
	makeDir(t, repoRoot, ".git")

	info, err := DetectWorktree(repoRoot)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	if info.IsWorktree {
		t.Error("expected IsWorktree=false for normal repo")
	}
	if info.WorktreeName != "myrepo" {
		t.Errorf("WorktreeName: got %q, want %q", info.WorktreeName, "myrepo")
	}
}

func TestDetectWorktree_LinkedWorktree(t *testing.T) {
	tmp := t.TempDir()
	repoRoot := makeDir(t, tmp, "my-feature-branch")
	// Linked worktree: .git is a file with "gitdir: ..." content.
	writeFile(t, filepath.Join(repoRoot, ".git"), "gitdir: /some/real/repo/.git/worktrees/my-feature-branch\n")

	info, err := DetectWorktree(repoRoot)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	if !info.IsWorktree {
		t.Error("expected IsWorktree=true for linked worktree")
	}
	if info.WorktreeName != "my-feature-branch" {
		t.Errorf("WorktreeName: got %q, want %q", info.WorktreeName, "my-feature-branch")
	}
}

func TestDetectWorktree_InvalidGitFile(t *testing.T) {
	tmp := t.TempDir()
	repoRoot := makeDir(t, tmp, "badrepo")
	// .git file present but content is not a valid gitdir pointer.
	writeFile(t, filepath.Join(repoRoot, ".git"), "not-a-gitdir-pointer\n")

	_, err := DetectWorktree(repoRoot)
	if err == nil {
		t.Error("expected error for invalid .git file content")
	}
}

func TestDetectWorktree_MissingGit(t *testing.T) {
	tmp := t.TempDir()
	repoRoot := makeDir(t, tmp, "norepo")
	// No .git at all.

	_, err := DetectWorktree(repoRoot)
	if err == nil {
		t.Error("expected error when .git is missing")
	}
}
