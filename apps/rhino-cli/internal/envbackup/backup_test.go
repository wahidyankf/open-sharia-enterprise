package envbackup

import (
	"os"
	"path/filepath"
	"testing"
)

func TestBackup_BasicBackup(t *testing.T) {
	tmp := t.TempDir()
	repo := makeDir(t, tmp, "repo")
	bkup := makeDir(t, tmp, "backup")

	writeFile(t, filepath.Join(repo, ".env"), "KEY=value")

	result, err := Backup(Options{
		RepoRoot:  repo,
		BackupDir: bkup,
		SkipDirs:  DefaultSkipDirs,
	})
	if err != nil {
		t.Fatalf("Backup error: %v", err)
	}
	if result.Direction != "backup" {
		t.Errorf("Direction: got %q, want %q", result.Direction, "backup")
	}
	if result.Copied != 1 {
		t.Errorf("Copied: got %d, want %d", result.Copied, 1)
	}
	if result.Skipped != 0 {
		t.Errorf("Skipped: got %d, want %d", result.Skipped, 0)
	}

	dst := filepath.Join(bkup, ".env")
	if _, err := os.Stat(dst); err != nil {
		t.Errorf("backup file not found at %s: %v", dst, err)
	}
}

func TestBackup_PathPreservation(t *testing.T) {
	tmp := t.TempDir()
	repo := makeDir(t, tmp, "repo")
	bkup := makeDir(t, tmp, "backup")

	writeFile(t, filepath.Join(repo, "apps", "web", ".env.local"), "WEB=1")
	writeFile(t, filepath.Join(repo, "apps", "api", ".env"), "API=1")

	result, err := Backup(Options{RepoRoot: repo, BackupDir: bkup, SkipDirs: DefaultSkipDirs})
	if err != nil {
		t.Fatalf("Backup error: %v", err)
	}
	if result.Copied != 2 {
		t.Errorf("Copied: got %d, want %d", result.Copied, 2)
	}

	// Verify relative paths are preserved.
	for _, relPath := range []string{filepath.Join("apps", "web", ".env.local"), filepath.Join("apps", "api", ".env")} {
		dst := filepath.Join(bkup, relPath)
		if _, err := os.Stat(dst); err != nil {
			t.Errorf("expected backup at %s: %v", dst, err)
		}
	}
}

func TestBackup_PermissionPreservation(t *testing.T) {
	tmp := t.TempDir()
	repo := makeDir(t, tmp, "repo")
	bkup := makeDir(t, tmp, "backup")

	src := filepath.Join(repo, ".env")
	writeFile(t, src, "SECRET=abc")
	if err := os.Chmod(src, 0o600); err != nil {
		t.Fatalf("chmod: %v", err)
	}

	if _, err := Backup(Options{RepoRoot: repo, BackupDir: bkup, SkipDirs: DefaultSkipDirs}); err != nil {
		t.Fatalf("Backup error: %v", err)
	}

	fi, err := os.Stat(filepath.Join(bkup, ".env"))
	if err != nil {
		t.Fatalf("stat dst: %v", err)
	}
	if fi.Mode().Perm() != 0o600 {
		t.Errorf("permission: got %o, want %o", fi.Mode().Perm(), 0o600)
	}
}

func TestBackup_ContentIntegrity(t *testing.T) {
	tmp := t.TempDir()
	repo := makeDir(t, tmp, "repo")
	bkup := makeDir(t, tmp, "backup")

	content := "DATABASE_URL=postgres://user:pass@localhost/db\nSECRET_KEY=mysecret\n"
	writeFile(t, filepath.Join(repo, ".env"), content)

	if _, err := Backup(Options{RepoRoot: repo, BackupDir: bkup, SkipDirs: DefaultSkipDirs}); err != nil {
		t.Fatalf("Backup error: %v", err)
	}

	got, err := os.ReadFile(filepath.Join(bkup, ".env"))
	if err != nil {
		t.Fatalf("read dst: %v", err)
	}
	if string(got) != content {
		t.Errorf("content mismatch:\ngot:  %q\nwant: %q", string(got), content)
	}
}

func TestBackup_OverwritesExistingFile(t *testing.T) {
	tmp := t.TempDir()
	repo := makeDir(t, tmp, "repo")
	bkup := makeDir(t, tmp, "backup")

	// Write initial backup.
	writeFile(t, filepath.Join(bkup, ".env"), "OLD=1")
	writeFile(t, filepath.Join(repo, ".env"), "NEW=2")

	if _, err := Backup(Options{RepoRoot: repo, BackupDir: bkup, SkipDirs: DefaultSkipDirs}); err != nil {
		t.Fatalf("Backup error: %v", err)
	}

	got, err := os.ReadFile(filepath.Join(bkup, ".env"))
	if err != nil {
		t.Fatalf("read dst: %v", err)
	}
	if string(got) != "NEW=2" {
		t.Errorf("expected overwrite; got %q", string(got))
	}
}

func TestBackup_InsideRepoRejected(t *testing.T) {
	tmp := t.TempDir()
	repo := makeDir(t, tmp, "repo")
	// Backup dir is inside repo root.
	insideDir := makeDir(t, repo, "my-backup")

	_, err := Backup(Options{RepoRoot: repo, BackupDir: insideDir, SkipDirs: DefaultSkipDirs})
	if err == nil {
		t.Error("expected error when backup dir is inside repo root")
	}
}

func TestBackup_WorktreeAware(t *testing.T) {
	tmp := t.TempDir()
	repo := makeDir(t, tmp, "myrepo")
	bkup := makeDir(t, tmp, "backup")

	writeFile(t, filepath.Join(repo, ".env"), "KEY=1")

	result, err := Backup(Options{
		RepoRoot:      repo,
		BackupDir:     bkup,
		SkipDirs:      DefaultSkipDirs,
		WorktreeAware: true,
		WorktreeName:  "myrepo",
	})
	if err != nil {
		t.Fatalf("Backup error: %v", err)
	}
	if result.WorktreeName != "myrepo" {
		t.Errorf("WorktreeName: got %q, want %q", result.WorktreeName, "myrepo")
	}

	// File should be under backup/myrepo/.env.
	dst := filepath.Join(bkup, "myrepo", ".env")
	if _, err := os.Stat(dst); err != nil {
		t.Errorf("expected file at %s: %v", dst, err)
	}
}

func TestBackup_AutoGenDirExcluded(t *testing.T) {
	tmp := t.TempDir()
	repo := makeDir(t, tmp, "repo")
	bkup := makeDir(t, tmp, "backup")

	// These should be skipped by DefaultSkipDirs.
	writeFile(t, filepath.Join(repo, "generated-contracts", ".env"), "SKIP=1")
	writeFile(t, filepath.Join(repo, ".env"), "KEEP=1")

	result, err := Backup(Options{RepoRoot: repo, BackupDir: bkup, SkipDirs: DefaultSkipDirs})
	if err != nil {
		t.Fatalf("Backup error: %v", err)
	}
	if result.Copied != 1 {
		t.Errorf("Copied: got %d, want 1 (generated-contracts should be skipped)", result.Copied)
	}
}

func TestBackup_ZeroFiles(t *testing.T) {
	tmp := t.TempDir()
	repo := makeDir(t, tmp, "repo")
	bkup := makeDir(t, tmp, "backup")

	result, err := Backup(Options{RepoRoot: repo, BackupDir: bkup, SkipDirs: DefaultSkipDirs})
	if err != nil {
		t.Fatalf("Backup error: %v", err)
	}
	if result.Copied != 0 {
		t.Errorf("Copied: got %d, want 0", result.Copied)
	}
	if len(result.Files) != 0 {
		t.Errorf("Files: got %d, want 0", len(result.Files))
	}
}

func TestBackup_TildeExpansion(t *testing.T) {
	tmp := t.TempDir()
	repo := makeDir(t, tmp, "repo")
	writeFile(t, filepath.Join(repo, ".env"), "KEY=val")

	// Use an absolute path (not tilde) but exercise the no-tilde branch.
	bkup := makeDir(t, tmp, "backup")
	result, err := Backup(Options{RepoRoot: repo, BackupDir: bkup, SkipDirs: DefaultSkipDirs})
	if err != nil {
		t.Fatalf("Backup error: %v", err)
	}
	if result.Copied != 1 {
		t.Errorf("Copied: got %d, want 1", result.Copied)
	}
}

func TestBackup_DefaultSkipDirsUsedWhenEmpty(t *testing.T) {
	tmp := t.TempDir()
	repo := makeDir(t, tmp, "repo")
	bkup := makeDir(t, tmp, "backup")

	writeFile(t, filepath.Join(repo, ".env"), "KEY=1")
	writeFile(t, filepath.Join(repo, "node_modules", ".env"), "SKIP=1")

	// Pass empty SkipDirs — Backup should default to DefaultSkipDirs.
	result, err := Backup(Options{RepoRoot: repo, BackupDir: bkup, SkipDirs: nil})
	if err != nil {
		t.Fatalf("Backup error: %v", err)
	}
	if result.Copied != 1 {
		t.Errorf("Copied: got %d, want 1 (node_modules should be skipped via default)", result.Copied)
	}
}

func TestBackup_RepoRootSameAsBackupDir(t *testing.T) {
	tmp := t.TempDir()
	repo := makeDir(t, tmp, "repo")
	// Backup dir is exactly the repo root — should be rejected.
	_, err := Backup(Options{RepoRoot: repo, BackupDir: repo, SkipDirs: DefaultSkipDirs})
	if err == nil {
		t.Error("expected error when backup dir equals repo root")
	}
}

func TestBackup_WorktreeAwareEmptyName(t *testing.T) {
	// WorktreeAware=true but WorktreeName="" — should behave like non-aware.
	tmp := t.TempDir()
	repo := makeDir(t, tmp, "repo")
	bkup := makeDir(t, tmp, "backup")

	writeFile(t, filepath.Join(repo, ".env"), "KEY=1")

	result, err := Backup(Options{
		RepoRoot:      repo,
		BackupDir:     bkup,
		SkipDirs:      DefaultSkipDirs,
		WorktreeAware: true,
		WorktreeName:  "", // No name — falls back to plain backup dir.
	})
	if err != nil {
		t.Fatalf("Backup error: %v", err)
	}
	if result.Copied != 1 {
		t.Errorf("Copied: got %d, want 1", result.Copied)
	}
	// File goes directly to bkup/.env (not namespaced).
	if _, err := os.Stat(filepath.Join(bkup, ".env")); err != nil {
		t.Errorf("expected file at %s: %v", filepath.Join(bkup, ".env"), err)
	}
}

func TestIsInsideRepo_Equal(t *testing.T) {
	dir := "/some/path"
	if !isInsideRepo(dir, dir) {
		t.Error("expected isInsideRepo=true when paths are equal")
	}
}

func TestIsInsideRepo_Outside(t *testing.T) {
	if isInsideRepo("/other/path", "/some/path") {
		t.Error("expected isInsideRepo=false when path is outside")
	}
}

func TestExpandTilde_NoTilde(t *testing.T) {
	result, err := ExpandTilde("/absolute/path")
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	if result != "/absolute/path" {
		t.Errorf("got %q, want %q", result, "/absolute/path")
	}
}

func TestExpandTilde_WithTilde(t *testing.T) {
	result, err := ExpandTilde("~/mydir")
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	if result == "~/mydir" {
		t.Error("tilde should have been expanded")
	}
	if len(result) == 0 {
		t.Error("expanded path should not be empty")
	}
}

func TestBackup_CopyErrorRecorded(t *testing.T) {
	tmp := t.TempDir()
	repo := makeDir(t, tmp, "repo")
	bkup := makeDir(t, tmp, "backup")

	writeFile(t, filepath.Join(repo, ".env"), "KEY=1")

	// Make the destination .env a directory so copyFile will fail.
	dstDir := filepath.Join(bkup, ".env")
	if err := os.MkdirAll(dstDir, 0o755); err != nil {
		t.Fatalf("mkdir dst as dir: %v", err)
	}

	result, err := Backup(Options{RepoRoot: repo, BackupDir: bkup, SkipDirs: DefaultSkipDirs})
	if err != nil {
		t.Fatalf("Backup should not return top-level error for copy failure: %v", err)
	}
	if result.Skipped != 1 {
		t.Errorf("Skipped: got %d, want 1", result.Skipped)
	}
	if len(result.Errors) == 0 {
		t.Error("expected non-fatal error in result.Errors")
	}
}

func TestCopyFile_MissingSource(t *testing.T) {
	tmp := t.TempDir()
	err := copyFile(filepath.Join(tmp, "nonexistent.txt"), filepath.Join(tmp, "dst.txt"))
	if err == nil {
		t.Error("expected error when source file does not exist")
	}
}

func TestCopyFile_UnwritableDestDir(t *testing.T) {
	tmp := t.TempDir()
	src := filepath.Join(tmp, ".env")
	writeFile(t, src, "KEY=1")

	// Try to write to a path whose parent is a file (not a directory).
	dst := filepath.Join(src, "subpath") // src is a file, not a dir
	err := copyFile(src, dst)
	if err == nil {
		t.Error("expected error when destination parent is a file")
	}
}

func TestBackup_SkippedFileCountedCorrectly(t *testing.T) {
	tmp := t.TempDir()
	repo := makeDir(t, tmp, "repo")
	bkup := makeDir(t, tmp, "backup")

	// Write an oversized file (exceeds MaxSize).
	writeFile(t, filepath.Join(repo, ".env"), "1234567890")
	writeFile(t, filepath.Join(repo, ".env.local"), "OK=1")

	result, err := Backup(Options{
		RepoRoot:  repo,
		BackupDir: bkup,
		SkipDirs:  DefaultSkipDirs,
		MaxSize:   5, // Only .env (10 bytes) will be oversized.
	})
	if err != nil {
		t.Fatalf("Backup error: %v", err)
	}
	if result.Copied != 1 {
		t.Errorf("Copied: got %d, want 1", result.Copied)
	}
	if result.Skipped != 1 {
		t.Errorf("Skipped: got %d, want 1", result.Skipped)
	}
}
