package cmd

import (
	"bytes"
	"os"
	"path/filepath"
	"strings"
	"testing"
)

// makeJavaContractsDir creates a temp directory with a .java file that has unused imports.
// Returns the directory path.
func makeJavaContractsDir(t *testing.T) string {
	t.Helper()
	dir := t.TempDir()

	content := `package com.example;

import com.other.UsedClass;
import com.other.UnusedClass;

public class Foo {
    UsedClass x;
}
`
	if err := os.WriteFile(filepath.Join(dir, "Foo.java"), []byte(content), 0644); err != nil {
		t.Fatal(err)
	}
	return dir
}

func TestContractsJavaCleanImportsCmd_NoArgs(t *testing.T) {
	err := contractsJavaCleanImportsCmd.Args(contractsJavaCleanImportsCmd, []string{})
	if err == nil {
		t.Error("expected error when no args provided")
	}
}

func TestContractsJavaCleanImportsCmd_ValidDir(t *testing.T) {
	src := makeJavaContractsDir(t)

	cmd := contractsJavaCleanImportsCmd
	buf := new(bytes.Buffer)
	cmd.SetOut(buf)
	cmd.SetErr(buf)

	output = "text"
	verbose = false
	quiet = false

	err := cmd.RunE(cmd, []string{src})
	if err != nil {
		t.Errorf("expected no error for valid dir, got: %v", err)
	}
}

func TestContractsJavaCleanImportsCmd_EmptyDir(t *testing.T) {
	src := t.TempDir()

	cmd := contractsJavaCleanImportsCmd
	buf := new(bytes.Buffer)
	cmd.SetOut(buf)
	cmd.SetErr(buf)

	output = "text"
	verbose = false
	quiet = false

	err := cmd.RunE(cmd, []string{src})
	if err != nil {
		t.Errorf("expected no error for empty dir, got: %v", err)
	}

	got := buf.String()
	if !strings.Contains(got, "No imports needed cleaning") {
		t.Errorf("expected 'No imports needed cleaning' in output for empty dir, got: %s", got)
	}
}

func TestContractsJavaCleanImportsCmd_JSONOutput(t *testing.T) {
	src := makeJavaContractsDir(t)

	cmd := contractsJavaCleanImportsCmd
	buf := new(bytes.Buffer)
	cmd.SetOut(buf)
	cmd.SetErr(buf)

	output = "json"
	verbose = false
	quiet = false

	_ = cmd.RunE(cmd, []string{src})

	got := buf.String()
	if !strings.Contains(got, `"status"`) {
		t.Errorf("expected 'status' field in JSON output, got: %s", got)
	}
	if !strings.Contains(got, `"total_files"`) {
		t.Errorf("expected 'total_files' field in JSON output, got: %s", got)
	}
	if !strings.Contains(got, `"modified_files"`) {
		t.Errorf("expected 'modified_files' field in JSON output, got: %s", got)
	}
}

func TestContractsJavaCleanImportsCmd_MarkdownOutput(t *testing.T) {
	src := makeJavaContractsDir(t)

	cmd := contractsJavaCleanImportsCmd
	buf := new(bytes.Buffer)
	cmd.SetOut(buf)
	cmd.SetErr(buf)

	output = "markdown"
	verbose = false
	quiet = false

	_ = cmd.RunE(cmd, []string{src})

	got := buf.String()
	if !strings.Contains(got, "# Java Import Cleaning Report") {
		t.Errorf("expected markdown heading in output, got: %s", got)
	}
}

func TestContractsJavaCleanImportsCmd_QuietMode(t *testing.T) {
	src := t.TempDir()

	// File with only required imports (no unused)
	content := `package com.example;

import com.other.UsedClass;

public class Foo {
    UsedClass x;
}
`
	if err := os.WriteFile(filepath.Join(src, "Foo.java"), []byte(content), 0644); err != nil {
		t.Fatal(err)
	}

	cmd := contractsJavaCleanImportsCmd
	buf := new(bytes.Buffer)
	cmd.SetOut(buf)
	cmd.SetErr(buf)

	output = "text"
	verbose = false
	quiet = true

	err := cmd.RunE(cmd, []string{src})
	if err != nil {
		t.Errorf("expected no error, got: %v", err)
	}

	got := buf.String()
	if strings.Contains(got, "Cleaned imports") {
		t.Error("quiet mode should suppress summary message when no files modified")
	}
}

func TestContractsJavaCleanImportsCmd_VerboseMode(t *testing.T) {
	src := makeJavaContractsDir(t) // Has unused import, will be modified

	cmd := contractsJavaCleanImportsCmd
	buf := new(bytes.Buffer)
	cmd.SetOut(buf)
	cmd.SetErr(buf)

	output = "text"
	verbose = true
	quiet = false

	err := cmd.RunE(cmd, []string{src})
	if err != nil {
		t.Errorf("expected no error, got: %v", err)
	}

	got := buf.String()
	if !strings.Contains(got, "✓") {
		t.Errorf("expected '✓' listing in verbose output for modified files, got: %s", got)
	}
}
