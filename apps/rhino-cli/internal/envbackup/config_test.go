package envbackup

import (
	"os"
	"path/filepath"
	"testing"
)

func TestDiscoverConfig_FindsExistingFiles(t *testing.T) {
	tmp := t.TempDir()

	// Create a subset of config files.
	writeFile(t, filepath.Join(tmp, ".claude", "settings.local.json"), `{"key":"val"}`)
	writeFile(t, filepath.Join(tmp, ".windsurfrules"), "rules")

	patterns := []ConfigPattern{
		{RelPath: ".claude/settings.local.json", Description: "Claude settings", Category: "ai-tools"},
		{RelPath: ".windsurfrules", Description: "Windsurf rules", Category: "ai-tools"},
		{RelPath: ".cursor/mcp.json", Description: "Cursor MCP", Category: "ai-tools"}, // missing
	}

	entries, err := DiscoverConfig(tmp, patterns, DefaultMaxSize)
	if err != nil {
		t.Fatalf("DiscoverConfig error: %v", err)
	}
	if len(entries) != 2 {
		t.Errorf("expected 2 entries, got %d", len(entries))
	}
}

func TestDiscoverConfig_SilentlySkipsMissing(t *testing.T) {
	tmp := t.TempDir()

	patterns := []ConfigPattern{
		{RelPath: ".cursor/mcp.json", Description: "Cursor MCP", Category: "ai-tools"},
		{RelPath: ".aider.conf.yml", Description: "Aider config", Category: "ai-tools"},
	}

	entries, err := DiscoverConfig(tmp, patterns, DefaultMaxSize)
	if err != nil {
		t.Fatalf("DiscoverConfig error: %v", err)
	}
	if len(entries) != 0 {
		t.Errorf("expected 0 entries for missing patterns, got %d", len(entries))
	}
}

func TestDiscoverConfig_SkipsSymlinks(t *testing.T) {
	tmp := t.TempDir()

	// Create a real file and a symlink config.
	realFile := filepath.Join(tmp, "real-config.json")
	writeFile(t, realFile, `{}`)

	linkPath := filepath.Join(tmp, ".windsurfrules")
	if err := os.Symlink(realFile, linkPath); err != nil {
		t.Skip("symlinks not supported on this platform")
	}

	patterns := []ConfigPattern{
		{RelPath: ".windsurfrules", Description: "Windsurf rules", Category: "ai-tools"},
	}

	entries, err := DiscoverConfig(tmp, patterns, DefaultMaxSize)
	if err != nil {
		t.Fatalf("DiscoverConfig error: %v", err)
	}
	if len(entries) != 1 {
		t.Fatalf("expected 1 entry, got %d", len(entries))
	}
	if !entries[0].Skipped {
		t.Error("expected symlink config to be skipped")
	}
	if entries[0].Reason != "symlink" {
		t.Errorf("expected reason 'symlink', got %q", entries[0].Reason)
	}
}

func TestDiscoverConfig_SkipsOversized(t *testing.T) {
	tmp := t.TempDir()

	// Create a large config file.
	writeFile(t, filepath.Join(tmp, ".windsurfrules"), "x")

	patterns := []ConfigPattern{
		{RelPath: ".windsurfrules", Description: "Windsurf rules", Category: "ai-tools"},
	}

	// Set maxSize to 0 (will default to 1MB) — instead use a tiny maxSize.
	entries, err := DiscoverConfig(tmp, patterns, 0) // 0 triggers default — won't skip 1 byte
	if err != nil {
		t.Fatalf("DiscoverConfig error: %v", err)
	}
	if len(entries) != 1 || entries[0].Skipped {
		t.Error("with default maxSize, 1 byte file should not be skipped")
	}

	// Now with explicit tiny maxSize.
	entries2, err := DiscoverConfig(tmp, patterns, 1) // maxSize=1 byte, file is 1 byte — not oversized
	if err != nil {
		t.Fatalf("DiscoverConfig error: %v", err)
	}
	if len(entries2) != 1 || entries2[0].Skipped {
		t.Error("1 byte file with maxSize=1 should not be skipped (equal, not exceeding)")
	}

	// File > maxSize
	writeFile(t, filepath.Join(tmp, ".windsurfrules"), "xx") // 2 bytes
	entries3, err := DiscoverConfig(tmp, patterns, 1)
	if err != nil {
		t.Fatalf("DiscoverConfig error: %v", err)
	}
	if len(entries3) != 1 {
		t.Fatalf("expected 1 entry, got %d", len(entries3))
	}
	if !entries3[0].Skipped {
		t.Error("2 byte file with maxSize=1 should be skipped")
	}
}

func TestDiscoverConfig_SetsSourceConfig(t *testing.T) {
	tmp := t.TempDir()
	writeFile(t, filepath.Join(tmp, ".envrc"), "export FOO=bar")

	patterns := []ConfigPattern{
		{RelPath: ".envrc", Description: "direnv setup", Category: "environment"},
	}

	entries, err := DiscoverConfig(tmp, patterns, DefaultMaxSize)
	if err != nil {
		t.Fatalf("DiscoverConfig error: %v", err)
	}
	if len(entries) != 1 {
		t.Fatalf("expected 1 entry, got %d", len(entries))
	}
	if entries[0].Source != "config" {
		t.Errorf("expected Source='config', got %q", entries[0].Source)
	}
}

func TestDiscoverConfig_ReturnsSorted(t *testing.T) {
	tmp := t.TempDir()
	writeFile(t, filepath.Join(tmp, ".windsurfrules"), "w")
	writeFile(t, filepath.Join(tmp, ".aider.conf.yml"), "a")
	writeFile(t, filepath.Join(tmp, ".envrc"), "e")

	patterns := []ConfigPattern{
		{RelPath: ".windsurfrules", Description: "Windsurf rules", Category: "ai-tools"},
		{RelPath: ".aider.conf.yml", Description: "Aider config", Category: "ai-tools"},
		{RelPath: ".envrc", Description: "direnv setup", Category: "environment"},
	}

	entries, err := DiscoverConfig(tmp, patterns, DefaultMaxSize)
	if err != nil {
		t.Fatalf("DiscoverConfig error: %v", err)
	}
	if len(entries) != 3 {
		t.Fatalf("expected 3 entries, got %d", len(entries))
	}

	// Verify sorted order.
	for i := 1; i < len(entries); i++ {
		if entries[i].RelPath < entries[i-1].RelPath {
			t.Errorf("entries not sorted: %q comes after %q", entries[i].RelPath, entries[i-1].RelPath)
		}
	}
}

func TestDefaultConfigPatterns_Length(t *testing.T) {
	if len(DefaultConfigPatterns) != 14 {
		t.Errorf("DefaultConfigPatterns has %d entries, want 14", len(DefaultConfigPatterns))
	}
}

func TestDiscoverConfig_EmptyPatterns(t *testing.T) {
	tmp := t.TempDir()
	entries, err := DiscoverConfig(tmp, nil, DefaultMaxSize)
	if err != nil {
		t.Fatalf("DiscoverConfig error: %v", err)
	}
	if len(entries) != 0 {
		t.Errorf("expected 0 entries for nil patterns, got %d", len(entries))
	}
}

func TestDiscoverConfig_SkipsDirectories(t *testing.T) {
	tmp := t.TempDir()
	// Create a directory at a pattern path (edge case).
	if err := os.MkdirAll(filepath.Join(tmp, ".windsurfrules"), 0o755); err != nil {
		t.Fatalf("mkdir: %v", err)
	}

	patterns := []ConfigPattern{
		{RelPath: ".windsurfrules", Description: "Windsurf rules", Category: "ai-tools"},
	}

	entries, err := DiscoverConfig(tmp, patterns, DefaultMaxSize)
	if err != nil {
		t.Fatalf("DiscoverConfig error: %v", err)
	}
	if len(entries) != 0 {
		t.Errorf("expected 0 entries for directory at pattern path, got %d", len(entries))
	}
}
