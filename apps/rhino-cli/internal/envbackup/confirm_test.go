package envbackup

import (
	"bytes"
	"os"
	"path/filepath"
	"strings"
	"testing"
)

func TestFindExisting_MixedExistingAndMissing(t *testing.T) {
	tmp := t.TempDir()

	// Create some files in the destination.
	writeFile(t, filepath.Join(tmp, ".env"), "KEY=1")
	writeFile(t, filepath.Join(tmp, "apps", "web", ".env"), "WEB=1")

	entries := []FileEntry{
		{RelPath: ".env"},
		{RelPath: "apps/web/.env"},
		{RelPath: "apps/api/.env"}, // does not exist in tmp
	}

	existing := FindExisting(entries, tmp)
	if len(existing) != 2 {
		t.Errorf("FindExisting: got %d, want 2", len(existing))
	}
}

func TestFindExisting_SkipsSkippedEntries(t *testing.T) {
	tmp := t.TempDir()
	writeFile(t, filepath.Join(tmp, ".env"), "KEY=1")

	entries := []FileEntry{
		{RelPath: ".env", Skipped: true}, // should be ignored even though file exists
	}

	existing := FindExisting(entries, tmp)
	if len(existing) != 0 {
		t.Errorf("FindExisting should skip entries with Skipped=true, got %d", len(existing))
	}
}

func TestFindExisting_AllMissing(t *testing.T) {
	tmp := t.TempDir()
	entries := []FileEntry{
		{RelPath: ".env"},
		{RelPath: ".env.local"},
	}

	existing := FindExisting(entries, tmp)
	if len(existing) != 0 {
		t.Errorf("FindExisting: got %d, want 0", len(existing))
	}
}

func TestFindExisting_EmptyEntries(t *testing.T) {
	tmp := t.TempDir()
	existing := FindExisting(nil, tmp)
	if len(existing) != 0 {
		t.Errorf("FindExisting: got %d, want 0", len(existing))
	}
}

func TestDefaultConfirmFn_AcceptsY(t *testing.T) {
	for _, input := range []string{"y\n", "Y\n", "yes\n", "YES\n", "Yes\n"} {
		r := strings.NewReader(input)
		w := &bytes.Buffer{}
		fn := DefaultConfirmFn(r, w)
		if !fn([]string{".env"}) {
			t.Errorf("DefaultConfirmFn(%q) = false, want true", strings.TrimSpace(input))
		}
	}
}

func TestDefaultConfirmFn_DeclinesOther(t *testing.T) {
	for _, input := range []string{"n\n", "N\n", "\n", "anything\n", "no\n"} {
		r := strings.NewReader(input)
		w := &bytes.Buffer{}
		fn := DefaultConfirmFn(r, w)
		if fn([]string{".env"}) {
			t.Errorf("DefaultConfirmFn(%q) = true, want false", strings.TrimSpace(input))
		}
	}
}

func TestDefaultConfirmFn_DeclinesEOF(t *testing.T) {
	// Empty reader (EOF immediately).
	r := strings.NewReader("")
	w := &bytes.Buffer{}
	fn := DefaultConfirmFn(r, w)
	if fn([]string{".env"}) {
		t.Error("DefaultConfirmFn(EOF) = true, want false")
	}
}

func TestDefaultConfirmFn_WritesConflictList(t *testing.T) {
	r := strings.NewReader("n\n")
	w := &bytes.Buffer{}
	fn := DefaultConfirmFn(r, w)
	fn([]string{".env", "apps/web/.env"})

	output := w.String()
	if !strings.Contains(output, "2 file(s) already exist") {
		t.Errorf("expected conflict count in output, got: %q", output)
	}
	if !strings.Contains(output, ".env") {
		t.Errorf("expected .env in conflict list, got: %q", output)
	}
	if !strings.Contains(output, "apps/web/.env") {
		t.Errorf("expected apps/web/.env in conflict list, got: %q", output)
	}
}

func TestFindExisting_ConfigSource(t *testing.T) {
	tmp := t.TempDir()

	// Create a config file in destination.
	writeFile(t, filepath.Join(tmp, ".claude", "settings.local.json"), "{}")

	entries := []FileEntry{
		{RelPath: ".claude/settings.local.json", Source: "config"},
	}

	existing := FindExisting(entries, tmp)
	if len(existing) != 1 {
		t.Errorf("FindExisting should find config files too, got %d", len(existing))
	}
}

// Verify FindExisting works with real filesystem for cross-platform correctness.
func TestFindExisting_RealFilesystem(t *testing.T) {
	tmp := t.TempDir()
	writeFile(t, filepath.Join(tmp, "sub", ".env.local"), "X=1")

	entries := []FileEntry{
		{RelPath: filepath.Join("sub", ".env.local")},
		{RelPath: filepath.Join("sub", ".env.missing")},
	}

	existing := FindExisting(entries, tmp)
	if len(existing) != 1 {
		t.Errorf("FindExisting: got %d, want 1", len(existing))
	}

	// Cleanup is handled by t.TempDir().
	_ = os.RemoveAll(tmp)
}
