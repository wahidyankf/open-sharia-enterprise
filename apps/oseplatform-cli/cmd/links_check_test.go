package cmd

import (
	"encoding/json"
	"fmt"
	"os"
	"path/filepath"
	"strings"
	"testing"
	"time"

	"github.com/wahidyankf/open-sharia-enterprise/libs/golang-commons/testutil"
	"github.com/wahidyankf/open-sharia-enterprise/libs/hugo-commons/links"
)

func resetFlags() {
	quiet = false
	verbose = false
	output = "text"
}

func TestRunLinksCheck_NoMarkdownFiles(t *testing.T) {
	tmpDir := t.TempDir()
	linksContentDir = tmpDir
	resetFlags()

	read := testutil.CaptureStdout(t)
	err := runLinksCheck(nil, nil)
	read()
	if err != nil {
		t.Errorf("expected nil error for empty dir, got %v", err)
	}
}

func TestRunLinksCheck_ValidLinks(t *testing.T) {
	tmpDir := t.TempDir()

	// Create a file that the link points to
	target := filepath.Join(tmpDir, "about.md")
	if err := os.WriteFile(target, []byte("# About"), 0644); err != nil {
		t.Fatal(err)
	}
	// Create a file that links to it
	index := filepath.Join(tmpDir, "index.md")
	if err := os.WriteFile(index, []byte("[About](/about)"), 0644); err != nil {
		t.Fatal(err)
	}

	linksContentDir = tmpDir
	resetFlags()

	read := testutil.CaptureStdout(t)
	err := runLinksCheck(nil, nil)
	read()
	if err != nil {
		t.Errorf("expected nil error for valid links, got %v", err)
	}
}

func TestRunLinksCheck_BrokenLinks(t *testing.T) {
	tmpDir := t.TempDir()

	index := filepath.Join(tmpDir, "index.md")
	if err := os.WriteFile(index, []byte("[Missing](/does-not-exist)"), 0644); err != nil {
		t.Fatal(err)
	}

	linksContentDir = tmpDir
	resetFlags()

	read := testutil.CaptureStdout(t)
	err := runLinksCheck(nil, nil)
	read()
	if err == nil {
		t.Error("expected error for broken links, got nil")
	}
	if !strings.Contains(err.Error(), "broken link") {
		t.Errorf("expected 'broken link' in error, got %q", err.Error())
	}
}

func TestRunLinksCheck_DirectoryNotExist(t *testing.T) {
	linksContentDir = "/tmp/nonexistent-dir-xyz-12345"
	resetFlags()

	read := testutil.CaptureStdout(t)
	err := runLinksCheck(nil, nil)
	read()
	if err == nil {
		t.Error("expected error for nonexistent directory, got nil")
	}
}

func TestRunLinksCheck_JSONOutput(t *testing.T) {
	tmpDir := t.TempDir()
	linksContentDir = tmpDir
	resetFlags()
	output = "json"

	read := testutil.CaptureStdout(t)
	err := runLinksCheck(nil, nil)
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

func TestRunLinksCheck_MarkdownOutput(t *testing.T) {
	tmpDir := t.TempDir()
	linksContentDir = tmpDir
	resetFlags()
	output = "markdown"

	read := testutil.CaptureStdout(t)
	err := runLinksCheck(nil, nil)
	out := read()
	if err != nil {
		t.Errorf("expected nil error, got %v", err)
	}
	if !strings.Contains(out, "# Link Check Report") {
		t.Errorf("expected '# Link Check Report' header in markdown output, got %q", out)
	}
}

func TestRunLinksCheck_QuietMode(t *testing.T) {
	tmpDir := t.TempDir()
	linksContentDir = tmpDir
	resetFlags()
	quiet = true

	read := testutil.CaptureStdout(t)
	err := runLinksCheck(nil, nil)
	out := read()
	if err != nil {
		t.Errorf("expected nil error in quiet mode, got %v", err)
	}
	if out != "" {
		t.Errorf("expected no stdout in quiet mode, got %q", out)
	}
}

func TestRunLinksCheck_JSONOutputError(t *testing.T) {
	tmpDir := t.TempDir()
	linksContentDir = tmpDir
	resetFlags()
	output = "json"

	// Inject a failing JSON output function to cover the if outputErr != nil path
	outputLinksJSONFn = func(_ *links.CheckResult, _ time.Duration) error {
		return fmt.Errorf("simulated json output error")
	}
	defer func() { outputLinksJSONFn = links.OutputLinksJSON }()

	read := testutil.CaptureStdout(t)
	err := runLinksCheck(nil, nil)
	read()
	if err == nil {
		t.Error("expected error from JSON output failure")
	}
	if !strings.Contains(err.Error(), "simulated json output error") {
		t.Errorf("expected simulated error in message, got %q", err.Error())
	}
}
