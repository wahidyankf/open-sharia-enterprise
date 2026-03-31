package envbackup

import (
	"encoding/json"
	"strings"
	"testing"
)

// sampleResult builds a Result for use in reporter tests.
func sampleBackupResult() *Result {
	return &Result{
		Direction: "backup",
		Dir:       "/tmp/backup",
		Files: []FileEntry{
			{RelPath: ".env", Size: 42},
			{RelPath: "apps/web/.env.local", Size: 100},
			{RelPath: "apps/api/.env", Size: 80, Skipped: true, Reason: "symlink"},
		},
		Copied:  2,
		Skipped: 1,
	}
}

// --------------------------------------------------------------------------
// FormatText
// --------------------------------------------------------------------------

func TestFormatText_DefaultMode(t *testing.T) {
	r := sampleBackupResult()
	out := FormatText(r, false, false)

	// Default mode: non-skipped files appear, skipped files do not.
	if !strings.Contains(out, ".env") {
		t.Error("expected .env in output")
	}
	if !strings.Contains(out, "apps/web/.env.local") {
		t.Error("expected apps/web/.env.local in output")
	}
	if strings.Contains(out, "symlink") {
		t.Error("default mode should not show skip reason")
	}

	// Summary line.
	if !strings.Contains(out, "2 file(s)") {
		t.Errorf("expected copied count in summary; got: %q", out)
	}
	if !strings.Contains(out, "1 skipped") {
		t.Errorf("expected skipped count in summary; got: %q", out)
	}
}

func TestFormatText_VerboseMode(t *testing.T) {
	r := sampleBackupResult()
	out := FormatText(r, true, false)

	if !strings.Contains(out, "SKIPPED") {
		t.Error("verbose mode should show SKIPPED files")
	}
	if !strings.Contains(out, "symlink") {
		t.Error("verbose mode should show skip reason")
	}
}

func TestFormatText_QuietMode(t *testing.T) {
	r := sampleBackupResult()
	out := FormatText(r, false, true)

	// Quiet mode: no per-file lines, only summary.
	if strings.Contains(out, "BACKUP") {
		t.Error("quiet mode should not show per-file lines")
	}
	if !strings.Contains(out, "2 file(s)") {
		t.Errorf("quiet mode should still show summary; got: %q", out)
	}
}

func TestFormatText_WithWorktreeName(t *testing.T) {
	r := sampleBackupResult()
	r.WorktreeName = "my-branch"

	out := FormatText(r, false, true)
	if !strings.Contains(out, "my-branch") {
		t.Errorf("expected worktree name in summary; got: %q", out)
	}
}

func TestFormatText_WithErrors(t *testing.T) {
	r := sampleBackupResult()
	r.Errors = []string{"copy apps/broken/.env: permission denied"}

	out := FormatText(r, false, false)
	if !strings.Contains(out, "WARNING") {
		t.Errorf("expected WARNING for non-fatal errors; got: %q", out)
	}
	if !strings.Contains(out, "permission denied") {
		t.Errorf("expected error text in output; got: %q", out)
	}
}

func TestFormatText_RestoreDirection(t *testing.T) {
	r := &Result{
		Direction: "restore",
		Dir:       "/tmp/backup",
		Files:     []FileEntry{{RelPath: ".env", Size: 10}},
		Copied:    1,
	}
	out := FormatText(r, false, false)
	if !strings.Contains(out, "restore") {
		t.Errorf("expected 'restore' in output; got: %q", out)
	}
}

// --------------------------------------------------------------------------
// FormatJSON
// --------------------------------------------------------------------------

func TestFormatJSON_Structure(t *testing.T) {
	r := sampleBackupResult()
	out, err := FormatJSON(r)
	if err != nil {
		t.Fatalf("FormatJSON error: %v", err)
	}

	var parsed map[string]any
	if err := json.Unmarshal([]byte(out), &parsed); err != nil {
		t.Fatalf("invalid JSON: %v\noutput: %s", err, out)
	}

	for _, key := range []string{"direction", "dir", "files", "copied", "skipped"} {
		if _, ok := parsed[key]; !ok {
			t.Errorf("expected key %q in JSON output", key)
		}
	}

	if parsed["direction"] != "backup" {
		t.Errorf("direction: got %v, want %q", parsed["direction"], "backup")
	}
	if parsed["copied"] != float64(2) {
		t.Errorf("copied: got %v, want 2", parsed["copied"])
	}
	if parsed["skipped"] != float64(1) {
		t.Errorf("skipped: got %v, want 1", parsed["skipped"])
	}
}

func TestFormatJSON_WorktreeNameOmittedWhenEmpty(t *testing.T) {
	r := sampleBackupResult()
	// WorktreeName is empty.

	out, err := FormatJSON(r)
	if err != nil {
		t.Fatalf("FormatJSON error: %v", err)
	}

	if strings.Contains(out, "worktreeName") {
		t.Error("worktreeName should be omitted when empty")
	}
}

func TestFormatJSON_WorktreeNamePresent(t *testing.T) {
	r := sampleBackupResult()
	r.WorktreeName = "feature-x"

	out, err := FormatJSON(r)
	if err != nil {
		t.Fatalf("FormatJSON error: %v", err)
	}
	if !strings.Contains(out, "feature-x") {
		t.Errorf("expected worktreeName in JSON; got: %s", out)
	}
}

func TestFormatJSON_FilesArray(t *testing.T) {
	r := sampleBackupResult()
	out, err := FormatJSON(r)
	if err != nil {
		t.Fatalf("FormatJSON error: %v", err)
	}

	var parsed struct {
		Files []struct {
			RelPath string `json:"relPath"`
			Skipped bool   `json:"skipped"`
			Reason  string `json:"reason"`
		} `json:"files"`
	}
	if err := json.Unmarshal([]byte(out), &parsed); err != nil {
		t.Fatalf("unmarshal: %v", err)
	}
	if len(parsed.Files) != 3 {
		t.Errorf("expected 3 files in JSON, got %d", len(parsed.Files))
	}
}

// --------------------------------------------------------------------------
// FormatMarkdown
// --------------------------------------------------------------------------

func TestFormatMarkdown_ContainsTable(t *testing.T) {
	r := sampleBackupResult()
	out := FormatMarkdown(r)

	if !strings.Contains(out, "| File |") {
		t.Error("expected markdown table header")
	}
	if !strings.Contains(out, "|---") {
		t.Error("expected markdown table separator")
	}
	if !strings.Contains(out, ".env") {
		t.Error("expected .env in markdown table")
	}
}

func TestFormatMarkdown_ShowsSkippedFiles(t *testing.T) {
	r := sampleBackupResult()
	out := FormatMarkdown(r)

	if !strings.Contains(out, "skipped") {
		t.Error("expected 'skipped' status in markdown table")
	}
	if !strings.Contains(out, "symlink") {
		t.Error("expected symlink reason in markdown table")
	}
}

func TestFormatMarkdown_SummaryHeader(t *testing.T) {
	r := sampleBackupResult()
	out := FormatMarkdown(r)

	if !strings.Contains(out, "## Backup Report") {
		t.Errorf("expected H2 header; got: %q", out[:min(len(out), 200)])
	}
	if !strings.Contains(out, "**Copied**: 2") {
		t.Errorf("expected copied count in header; got: %q", out[:min(len(out), 200)])
	}
}

func TestFormatMarkdown_EmptyResult(t *testing.T) {
	r := &Result{
		Direction: "backup",
		Dir:       "/tmp/backup",
	}
	out := FormatMarkdown(r)

	if !strings.Contains(out, "No .env files found") {
		t.Errorf("expected empty state message; got: %q", out)
	}
}

func TestFormatMarkdown_WorktreeNameShown(t *testing.T) {
	r := sampleBackupResult()
	r.WorktreeName = "main-branch"

	out := FormatMarkdown(r)
	if !strings.Contains(out, "main-branch") {
		t.Errorf("expected worktree name in markdown; got: %q", out[:min(len(out), 200)])
	}
}

func TestFormatMarkdown_ErrorsSection(t *testing.T) {
	r := sampleBackupResult()
	r.Errors = []string{"copy failed: permission denied"}

	out := FormatMarkdown(r)
	if !strings.Contains(out, "### Warnings") {
		t.Errorf("expected Warnings section for errors; got: %q", out)
	}
	if !strings.Contains(out, "permission denied") {
		t.Errorf("expected error text in warnings; got: %q", out)
	}
}

func TestCapitalize_Empty(t *testing.T) {
	if got := capitalize(""); got != "" {
		t.Errorf("capitalize empty: got %q, want %q", got, "")
	}
}

func TestCapitalize_NonEmpty(t *testing.T) {
	if got := capitalize("backup"); got != "Backup" {
		t.Errorf("capitalize: got %q, want %q", got, "Backup")
	}
}

func TestFormatText_ConfigTag(t *testing.T) {
	r := &Result{
		Direction: "backup",
		Dir:       "/tmp/backup",
		Files: []FileEntry{
			{RelPath: ".env", Size: 42, Source: "env"},
			{RelPath: ".claude/settings.local.json", Size: 100, Source: "config"},
		},
		Copied: 2,
	}
	out := FormatText(r, false, false)

	if !strings.Contains(out, "[config]") {
		t.Errorf("expected [config] tag in output; got: %q", out)
	}
	if !strings.Contains(out, ".claude/settings.local.json") {
		t.Error("expected config file path in output")
	}
	if !strings.Contains(out, "(1 config)") {
		t.Errorf("expected config count in summary; got: %q", out)
	}
}

func TestFormatText_CancelledResult(t *testing.T) {
	r := &Result{
		Direction: "backup",
		Dir:       "/tmp/backup",
		Cancelled: true,
	}
	out := FormatText(r, false, false)

	if !strings.Contains(out, "Backup cancelled.") {
		t.Errorf("expected 'Backup cancelled.' in output; got: %q", out)
	}
}

func TestFormatText_CancelledRestore(t *testing.T) {
	r := &Result{
		Direction: "restore",
		Dir:       "/tmp/backup",
		Cancelled: true,
	}
	out := FormatText(r, false, false)

	if !strings.Contains(out, "Restore cancelled.") {
		t.Errorf("expected 'Restore cancelled.' in output; got: %q", out)
	}
}

func TestFormatJSON_SourceField(t *testing.T) {
	r := &Result{
		Direction: "backup",
		Dir:       "/tmp/backup",
		Files: []FileEntry{
			{RelPath: ".env", Size: 42, Source: "env"},
			{RelPath: ".claude/settings.local.json", Size: 100, Source: "config"},
		},
		Copied: 2,
	}

	out, err := FormatJSON(r)
	if err != nil {
		t.Fatalf("FormatJSON error: %v", err)
	}

	if !strings.Contains(out, `"source": "config"`) {
		t.Errorf("expected source field in JSON; got: %s", out)
	}
	if !strings.Contains(out, `"source": "env"`) {
		t.Errorf("expected source 'env' in JSON; got: %s", out)
	}
}

func TestFormatJSON_CancelledField(t *testing.T) {
	r := &Result{
		Direction: "backup",
		Dir:       "/tmp/backup",
		Cancelled: true,
	}

	out, err := FormatJSON(r)
	if err != nil {
		t.Fatalf("FormatJSON error: %v", err)
	}

	if !strings.Contains(out, `"cancelled": true`) {
		t.Errorf("expected 'cancelled: true' in JSON; got: %s", out)
	}
}

func TestFormatJSON_CancelledOmittedWhenFalse(t *testing.T) {
	r := sampleBackupResult()

	out, err := FormatJSON(r)
	if err != nil {
		t.Fatalf("FormatJSON error: %v", err)
	}

	if strings.Contains(out, "cancelled") {
		t.Errorf("cancelled should be omitted when false; got: %s", out)
	}
}

func TestFormatMarkdown_SourceColumn(t *testing.T) {
	r := &Result{
		Direction: "backup",
		Dir:       "/tmp/backup",
		Files: []FileEntry{
			{RelPath: ".env", Size: 42, Source: "env"},
			{RelPath: ".claude/settings.local.json", Size: 100, Source: "config"},
		},
		Copied: 2,
	}
	out := FormatMarkdown(r)

	if !strings.Contains(out, "| Source |") {
		t.Errorf("expected Source column in markdown table; got: %q", out[:min(len(out), 300)])
	}
	if !strings.Contains(out, "config") {
		t.Error("expected 'config' source in markdown table")
	}
}

func TestFormatMarkdown_CancelledResult(t *testing.T) {
	r := &Result{
		Direction: "backup",
		Dir:       "/tmp/backup",
		Cancelled: true,
	}
	out := FormatMarkdown(r)

	if !strings.Contains(out, "cancelled") {
		t.Errorf("expected 'cancelled' in markdown output; got: %q", out)
	}
}

func TestFormatText_EmptyDirection(t *testing.T) {
	r := &Result{
		Direction: "",
		Dir:       "/tmp/backup",
		Copied:    0,
		Skipped:   0,
	}
	out := FormatText(r, false, true)
	// Should not panic and should contain a summary.
	if len(out) == 0 {
		t.Error("expected non-empty output even with empty direction")
	}
}
