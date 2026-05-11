package repogovernance

import (
	"bytes"
	"os"
	"path/filepath"
	"strings"
	"testing"
)

// writeSized writes a file of exactly `size` bytes at path. It pads with the
// letter 'a' to reach the requested length.
func writeSized(t *testing.T, path string, size int) {
	t.Helper()
	if err := os.WriteFile(path, bytes.Repeat([]byte("a"), size), 0o600); err != nil {
		t.Fatalf("write fixture %s: %v", path, err)
	}
}

// TestCheckAgentsMdSize_Thresholds verifies the four threshold ranges produce
// the expected severity labels and that the Size and File fields are populated.
func TestCheckAgentsMdSize_Thresholds(t *testing.T) {
	t.Parallel()

	cases := []struct {
		name         string
		size         int
		wantSeverity string
		wantMsgPart  string
	}{
		{"at-target", AgentsMdTargetSize, "ok", "within"},
		{"just-over-target", AgentsMdTargetSize + 1, "warn", "over 30000-byte target"},
		{"at-warning", AgentsMdWarningSize, "warn", "over 30000-byte target"},
		{"just-over-warning", AgentsMdWarningSize + 1, "warn", "over 35000-byte warning threshold"},
		{"at-hard-limit", AgentsMdHardLimitSize, "warn", "over 35000-byte warning threshold"},
		{"just-over-hard-limit", AgentsMdHardLimitSize + 1, "fail", "over 40000-byte hard limit"},
	}

	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			t.Parallel()
			dir := t.TempDir()
			path := filepath.Join(dir, "AGENTS.md")
			writeSized(t, path, tc.size)

			got, err := CheckAgentsMdSize(path)
			if err != nil {
				t.Fatalf("CheckAgentsMdSize: %v", err)
			}
			if got.Severity != tc.wantSeverity {
				t.Errorf("Severity = %q, want %q", got.Severity, tc.wantSeverity)
			}
			if got.Size != int64(tc.size) {
				t.Errorf("Size = %d, want %d", got.Size, tc.size)
			}
			if got.File != path {
				t.Errorf("File = %q, want %q", got.File, path)
			}
			if !strings.Contains(got.Message, tc.wantMsgPart) {
				t.Errorf("Message = %q, expected to contain %q", got.Message, tc.wantMsgPart)
			}
		})
	}
}

// TestCheckAgentsMdSize_MissingFile verifies a missing file returns an error
// wrapped with the underlying os error.
func TestCheckAgentsMdSize_MissingFile(t *testing.T) {
	t.Parallel()
	dir := t.TempDir()
	missing := filepath.Join(dir, "no-such-file.md")

	_, err := CheckAgentsMdSize(missing)
	if err == nil {
		t.Fatal("expected error for missing file, got nil")
	}
	if !os.IsNotExist(err) && !strings.Contains(err.Error(), "no such file") {
		t.Errorf("expected not-exist error, got: %v", err)
	}
}

// TestCheckAgentsMdSize_ZeroBytes verifies a zero-byte file is classified as ok.
func TestCheckAgentsMdSize_ZeroBytes(t *testing.T) {
	t.Parallel()
	dir := t.TempDir()
	path := filepath.Join(dir, "AGENTS.md")
	writeSized(t, path, 0)

	got, err := CheckAgentsMdSize(path)
	if err != nil {
		t.Fatalf("CheckAgentsMdSize: %v", err)
	}
	if got.Severity != "ok" {
		t.Errorf("Severity = %q, want %q", got.Severity, "ok")
	}
	if got.Size != 0 {
		t.Errorf("Size = %d, want 0", got.Size)
	}
}
