package main

import (
	"encoding/json"
	"io"
	"os"
	"strings"
	"testing"
)

func captureStdout(t *testing.T) func() string {
	t.Helper()
	r, w, err := os.Pipe()
	if err != nil {
		t.Fatalf("os.Pipe: %v", err)
	}
	orig := os.Stdout
	os.Stdout = w
	t.Cleanup(func() { os.Stdout = orig })
	return func() string {
		w.Close()
		out, _ := io.ReadAll(r)
		return string(out)
	}
}

func TestRun_NoArgs(t *testing.T) {
	os.Args = []string{"javaproject-cli"}
	read := captureStdout(t)
	code := run()
	read()
	if code != 2 {
		t.Errorf("expected exit code 2 for no args, got %d", code)
	}
}

func TestRun_HelpFlag(t *testing.T) {
	os.Args = []string{"javaproject-cli", "-h"}
	read := captureStdout(t)
	code := run()
	read()
	if code != 0 {
		t.Errorf("expected exit code 0 for -h, got %d", code)
	}
}

func TestRun_VerboseAndQuietMutuallyExclusive(t *testing.T) {
	tmpDir := t.TempDir()
	os.Args = []string{"javaproject-cli", "-v", "-q", tmpDir}
	read := captureStdout(t)
	code := run()
	read()
	if code != 2 {
		t.Errorf("expected exit code 2 for -v -q, got %d", code)
	}
}

func TestRun_InvalidOutputFormat(t *testing.T) {
	tmpDir := t.TempDir()
	os.Args = []string{"javaproject-cli", "-o", "xml", tmpDir}
	read := captureStdout(t)
	code := run()
	read()
	if code != 2 {
		t.Errorf("expected exit code 2 for invalid -o, got %d", code)
	}
}

func TestRun_EmptySourceRoot_TextOutput(t *testing.T) {
	tmpDir := t.TempDir()
	os.Args = []string{"javaproject-cli", tmpDir}
	read := captureStdout(t)
	code := run()
	read()
	if code != 0 {
		t.Errorf("expected exit code 0 for empty dir, got %d", code)
	}
}

func TestRun_EmptySourceRoot_JSONOutput(t *testing.T) {
	tmpDir := t.TempDir()
	os.Args = []string{"javaproject-cli", "-o", "json", tmpDir}
	read := captureStdout(t)
	code := run()
	out := read()
	if code != 0 {
		t.Errorf("expected exit code 0 for JSON output, got %d", code)
	}
	var parsed map[string]any
	if err := json.Unmarshal([]byte(out), &parsed); err != nil {
		t.Errorf("expected valid JSON output, got %q: %v", out, err)
	}
}

func TestRun_EmptySourceRoot_MarkdownOutput(t *testing.T) {
	tmpDir := t.TempDir()
	os.Args = []string{"javaproject-cli", "-o", "markdown", tmpDir}
	read := captureStdout(t)
	code := run()
	out := read()
	if code != 0 {
		t.Errorf("expected exit code 0 for markdown output, got %d", code)
	}
	if !strings.Contains(out, "#") {
		t.Errorf("expected markdown header in output, got %q", out)
	}
}

func TestRun_VerboseMode(t *testing.T) {
	tmpDir := t.TempDir()
	os.Args = []string{"javaproject-cli", "-v", tmpDir}
	read := captureStdout(t)
	code := run()
	read()
	if code != 0 {
		t.Errorf("expected exit code 0 for verbose mode, got %d", code)
	}
}

func TestRun_QuietMode(t *testing.T) {
	tmpDir := t.TempDir()
	os.Args = []string{"javaproject-cli", "-q", tmpDir}
	read := captureStdout(t)
	code := run()
	read()
	if code != 0 {
		t.Errorf("expected exit code 0 for quiet mode, got %d", code)
	}
}
