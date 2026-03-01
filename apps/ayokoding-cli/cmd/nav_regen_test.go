package cmd

import (
	"encoding/json"
	"os"
	"path/filepath"
	"strings"
	"testing"
	"time"

	"github.com/wahidyankf/open-sharia-enterprise/apps/ayokoding-cli/internal/navigation"
	"github.com/wahidyankf/open-sharia-enterprise/libs/golang-commons/testutil"
)

func makeIndexDir(t *testing.T) string {
	t.Helper()
	tmpDir := t.TempDir()
	content := "---\ntitle: \"Test\"\n---\n"
	if err := os.WriteFile(filepath.Join(tmpDir, "_index.md"), []byte(content), 0644); err != nil {
		t.Fatalf("failed to create _index.md: %v", err)
	}
	return tmpDir
}

func TestRunNavRegen_WithIndexFile(t *testing.T) {
	tmpDir := makeIndexDir(t)
	resetFlags()
	regenPath = tmpDir

	read := testutil.CaptureStdout(t)
	err := runNavRegen(nil, nil)
	read()
	if err != nil {
		t.Errorf("expected nil error for dir with _index.md, got %v", err)
	}
}

func TestRunNavRegen_NonExistentDir(t *testing.T) {
	resetFlags()
	regenPath = "/tmp/nonexistent-nav-dir-xyz-99999"

	read := testutil.CaptureStdout(t)
	err := runNavRegen(nil, nil)
	read()
	if err == nil {
		t.Error("expected error for nonexistent directory, got nil")
	}
}

func TestRunNavRegen_QuietMode(t *testing.T) {
	tmpDir := makeIndexDir(t)
	resetFlags()
	regenPath = tmpDir
	quiet = true

	read := testutil.CaptureStdout(t)
	err := runNavRegen(nil, nil)
	out := read()
	if err != nil {
		t.Errorf("expected nil error in quiet mode, got %v", err)
	}
	if out != "" {
		t.Errorf("expected no stdout in quiet mode, got %q", out)
	}
}

func TestRunNavRegen_JSONOutput(t *testing.T) {
	tmpDir := makeIndexDir(t)
	resetFlags()
	regenPath = tmpDir
	output = "json"

	read := testutil.CaptureStdout(t)
	err := runNavRegen(nil, nil)
	out := read()
	if err != nil {
		t.Errorf("expected nil error, got %v", err)
	}
	var parsed map[string]any
	if err := json.Unmarshal([]byte(out), &parsed); err != nil {
		t.Errorf("expected valid JSON output, got %q: %v", out, err)
	}
	if parsed["status"] != "success" {
		t.Errorf("expected status 'success', got %v", parsed["status"])
	}
}

func TestRunNavRegen_MarkdownOutput(t *testing.T) {
	tmpDir := makeIndexDir(t)
	resetFlags()
	regenPath = tmpDir
	output = "markdown"

	read := testutil.CaptureStdout(t)
	err := runNavRegen(nil, nil)
	out := read()
	if err != nil {
		t.Errorf("expected nil error, got %v", err)
	}
	if !strings.Contains(out, "# Navigation Regeneration Report") {
		t.Errorf("expected markdown header, got %q", out)
	}
}

func TestRunNavRegen_PositionalArg(t *testing.T) {
	tmpDir := makeIndexDir(t)
	resetFlags()
	regenPath = ""

	read := testutil.CaptureStdout(t)
	err := runNavRegen(nil, []string{tmpDir})
	read()
	if err != nil {
		t.Errorf("expected nil error for positional arg, got %v", err)
	}
}

func TestOutputText_WithErrors(t *testing.T) {
	result := &navigation.RegenerateResult{
		ProcessedCount: 2,
		SkippedCount:   1,
		ErrorCount:     1,
		Errors:         []string{"some error occurred"},
	}
	read := testutil.CaptureStdout(t)
	err := outputText(result, time.Second)
	out := read()
	if err != nil {
		t.Errorf("expected nil error, got %v", err)
	}
	if !strings.Contains(out, "Navigation Regeneration Complete") {
		t.Errorf("expected header in output, got %q", out)
	}
	if !strings.Contains(out, "some error occurred") {
		t.Errorf("expected error message in output, got %q", out)
	}
}

func TestOutputText_Verbose(t *testing.T) {
	result := &navigation.RegenerateResult{
		ProcessedCount: 0,
		SkippedCount:   0,
		ErrorCount:     0,
		Errors:         []string{},
	}
	verbose = true
	read := testutil.CaptureStdout(t)
	err := outputText(result, time.Second)
	out := read()
	verbose = false
	if err != nil {
		t.Errorf("expected nil error, got %v", err)
	}
	if !strings.Contains(out, "Completed at:") {
		t.Errorf("expected 'Completed at:' in verbose output, got %q", out)
	}
}

func TestOutputJSON_Success(t *testing.T) {
	result := &navigation.RegenerateResult{
		ProcessedCount: 3,
		SkippedCount:   1,
		ErrorCount:     0,
		Errors:         []string{},
	}
	read := testutil.CaptureStdout(t)
	err := outputJSON(result, 500*time.Millisecond)
	out := read()
	if err != nil {
		t.Fatalf("expected nil error, got %v", err)
	}
	var parsed map[string]any
	if err := json.Unmarshal([]byte(out), &parsed); err != nil {
		t.Fatalf("expected valid JSON, got %q: %v", out, err)
	}
	if parsed["status"] != "success" {
		t.Errorf("expected status 'success', got %v", parsed["status"])
	}
}

func TestOutputMarkdown_Success(t *testing.T) {
	result := &navigation.RegenerateResult{
		ProcessedCount: 5,
		SkippedCount:   0,
		ErrorCount:     0,
		Errors:         []string{},
	}
	read := testutil.CaptureStdout(t)
	err := outputMarkdown(result, time.Second, "/some/path")
	out := read()
	if err != nil {
		t.Errorf("expected nil error, got %v", err)
	}
	if !strings.Contains(out, "# Navigation Regeneration Report") {
		t.Errorf("expected markdown header, got %q", out)
	}
}

func TestOutputMarkdown_WithErrors(t *testing.T) {
	result := &navigation.RegenerateResult{
		ProcessedCount: 0,
		SkippedCount:   0,
		ErrorCount:     1,
		Errors:         []string{"nav error"},
	}
	read := testutil.CaptureStdout(t)
	err := outputMarkdown(result, time.Second, "/some/path")
	out := read()
	if err != nil {
		t.Errorf("expected nil error, got %v", err)
	}
	if !strings.Contains(out, "## Errors") {
		t.Errorf("expected errors section in markdown, got %q", out)
	}
}
