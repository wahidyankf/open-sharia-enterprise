package envbackup

import (
	"os"
	"path/filepath"
	"testing"
)

// discoverRelPaths runs Discover and returns only the RelPath of each entry.
func discoverRelPaths(t *testing.T, opts Options) []string {
	t.Helper()
	entries, err := Discover(opts)
	if err != nil {
		t.Fatalf("Discover error: %v", err)
	}
	paths := make([]string, len(entries))
	for i, e := range entries {
		paths[i] = e.RelPath
	}
	return paths
}

func TestDiscover_BasicEnvFile(t *testing.T) {
	root := t.TempDir()
	writeFile(t, filepath.Join(root, ".env"), "KEY=value")

	entries, err := Discover(Options{RepoRoot: root, MaxSize: DefaultMaxSize})
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	if len(entries) != 1 {
		t.Fatalf("expected 1 entry, got %d", len(entries))
	}
	if entries[0].RelPath != ".env" {
		t.Errorf("RelPath: got %q, want %q", entries[0].RelPath, ".env")
	}
	if entries[0].Skipped {
		t.Error("expected file not to be skipped")
	}
}

func TestDiscover_SkipsNodeModules(t *testing.T) {
	root := t.TempDir()
	writeFile(t, filepath.Join(root, "node_modules", "pkg", ".env"), "SHOULD_SKIP=1")
	writeFile(t, filepath.Join(root, ".env"), "KEY=value")

	paths := discoverRelPaths(t, Options{RepoRoot: root, SkipDirs: DefaultSkipDirs, MaxSize: DefaultMaxSize})
	for _, p := range paths {
		if p == filepath.Join("node_modules", "pkg", ".env") {
			t.Errorf("node_modules .env file should be skipped, found: %s", p)
		}
	}
	found := false
	for _, p := range paths {
		if p == ".env" {
			found = true
		}
	}
	if !found {
		t.Error("root .env should be discovered")
	}
}

func TestDiscover_SkipsCommonBuildDirs(t *testing.T) {
	root := t.TempDir()
	skipDirs := []string{"dist", "build", ".next", "__pycache__", "target", "vendor", "coverage", "generated-contracts"}
	for _, d := range skipDirs {
		writeFile(t, filepath.Join(root, d, ".env"), "SKIP=1")
	}
	writeFile(t, filepath.Join(root, ".env"), "ROOT=1")

	entries, err := Discover(Options{RepoRoot: root, SkipDirs: DefaultSkipDirs, MaxSize: DefaultMaxSize})
	if err != nil {
		t.Fatalf("Discover error: %v", err)
	}

	// Only the root .env should be discovered.
	if len(entries) != 1 {
		t.Errorf("expected 1 entry (root .env), got %d", len(entries))
		for _, e := range entries {
			t.Logf("  found: %s (skipped=%v)", e.RelPath, e.Skipped)
		}
	}
}

func TestDiscover_SkipsNestedBuildDirs(t *testing.T) {
	root := t.TempDir()
	// Nested: apps/myapp/node_modules/.env should be skipped.
	writeFile(t, filepath.Join(root, "apps", "myapp", "node_modules", ".env"), "SKIP=1")
	writeFile(t, filepath.Join(root, "apps", "myapp", ".env"), "KEEP=1")

	entries, err := Discover(Options{RepoRoot: root, SkipDirs: DefaultSkipDirs, MaxSize: DefaultMaxSize})
	if err != nil {
		t.Fatalf("Discover error: %v", err)
	}
	for _, e := range entries {
		if e.RelPath == filepath.Join("apps", "myapp", "node_modules", ".env") {
			t.Errorf("nested node_modules .env should be skipped: %s", e.RelPath)
		}
	}
}

func TestDiscover_SkipsSymlinks(t *testing.T) {
	root := t.TempDir()
	realFile := filepath.Join(root, ".env.real")
	writeFile(t, realFile, "REAL=1")

	linkPath := filepath.Join(root, ".env.link")
	if err := os.Symlink(realFile, linkPath); err != nil {
		t.Skip("symlinks not supported on this platform")
	}

	entries, err := Discover(Options{RepoRoot: root, MaxSize: DefaultMaxSize})
	if err != nil {
		t.Fatalf("Discover error: %v", err)
	}

	// .env.real is a regular file; .env.link is a symlink that should be skipped.
	for _, e := range entries {
		if e.RelPath == ".env.link" {
			if !e.Skipped {
				t.Error(".env.link should be marked as skipped")
			}
			if e.Reason != "symlink" {
				t.Errorf("skip reason: got %q, want %q", e.Reason, "symlink")
			}
		}
		if e.RelPath == ".env.real" && e.Skipped {
			t.Error(".env.real should not be skipped")
		}
	}
}

func TestDiscover_SkipsOversizedFiles(t *testing.T) {
	root := t.TempDir()
	// Write a 10-byte file and cap MaxSize at 5 bytes.
	writeFile(t, filepath.Join(root, ".env"), "1234567890")

	entries, err := Discover(Options{RepoRoot: root, MaxSize: 5})
	if err != nil {
		t.Fatalf("Discover error: %v", err)
	}
	if len(entries) != 1 {
		t.Fatalf("expected 1 entry, got %d", len(entries))
	}
	if !entries[0].Skipped {
		t.Error("oversized file should be skipped")
	}
	if entries[0].Reason != "exceeds 1 MB" {
		t.Errorf("skip reason: got %q, want %q", entries[0].Reason, "exceeds 1 MB")
	}
}

func TestDiscover_NonEnvDotfilesIgnored(t *testing.T) {
	root := t.TempDir()
	writeFile(t, filepath.Join(root, ".gitignore"), "*.log")
	writeFile(t, filepath.Join(root, ".dockerignore"), "node_modules")
	writeFile(t, filepath.Join(root, ".env"), "KEY=val")

	entries, err := Discover(Options{RepoRoot: root, MaxSize: DefaultMaxSize})
	if err != nil {
		t.Fatalf("Discover error: %v", err)
	}
	if len(entries) != 1 || entries[0].RelPath != ".env" {
		t.Errorf("expected only .env, got %v", entries)
	}
}

func TestDiscover_SortedByRelPath(t *testing.T) {
	root := t.TempDir()
	writeFile(t, filepath.Join(root, "z", ".env"), "Z=1")
	writeFile(t, filepath.Join(root, "a", ".env"), "A=1")
	writeFile(t, filepath.Join(root, ".env"), "ROOT=1")

	entries, err := Discover(Options{RepoRoot: root, SkipDirs: []string{".git"}, MaxSize: DefaultMaxSize})
	if err != nil {
		t.Fatalf("Discover error: %v", err)
	}
	for i := 1; i < len(entries); i++ {
		if entries[i].RelPath < entries[i-1].RelPath {
			t.Errorf("entries not sorted: %q > %q", entries[i-1].RelPath, entries[i].RelPath)
		}
	}
}

func TestDiscover_EmptyRepo(t *testing.T) {
	root := t.TempDir()

	entries, err := Discover(Options{RepoRoot: root, MaxSize: DefaultMaxSize})
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	if len(entries) != 0 {
		t.Errorf("expected 0 entries, got %d", len(entries))
	}
}

func TestDiscover_DefaultMaxSizeApplied(t *testing.T) {
	root := t.TempDir()
	writeFile(t, filepath.Join(root, ".env"), "KEY=val")

	// Pass MaxSize=0 — Discover should default to DefaultMaxSize.
	entries, err := Discover(Options{RepoRoot: root, MaxSize: 0})
	if err != nil {
		t.Fatalf("Discover error: %v", err)
	}
	if len(entries) != 1 {
		t.Fatalf("expected 1 entry, got %d", len(entries))
	}
	if entries[0].Skipped {
		t.Error("file within DefaultMaxSize should not be skipped")
	}
}

func TestDiscover_MultipleEnvVariants(t *testing.T) {
	root := t.TempDir()
	writeFile(t, filepath.Join(root, ".env"), "A=1")
	writeFile(t, filepath.Join(root, ".env.local"), "B=2")
	writeFile(t, filepath.Join(root, ".env.production"), "C=3")
	writeFile(t, filepath.Join(root, ".env.test"), "D=4")

	entries, err := Discover(Options{RepoRoot: root, MaxSize: DefaultMaxSize})
	if err != nil {
		t.Fatalf("Discover error: %v", err)
	}
	if len(entries) != 4 {
		t.Errorf("expected 4 entries, got %d", len(entries))
	}
}
