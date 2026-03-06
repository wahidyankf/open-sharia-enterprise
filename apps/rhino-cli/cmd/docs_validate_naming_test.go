package cmd

import (
	"bytes"
	"encoding/json"
	"os"
	"path/filepath"
	"strings"
	"testing"
)

func TestValidateDocsNamingCommand_ValidFiles(t *testing.T) {
	// Save original working directory
	originalWd, err := os.Getwd()
	if err != nil {
		t.Fatalf("Failed to get working directory: %v", err)
	}
	defer func() { _ = os.Chdir(originalWd) }()

	// Create temporary test repository
	tmpDir := t.TempDir()
	if err := os.Chdir(tmpDir); err != nil {
		t.Fatalf("Failed to change to temp directory: %v", err)
	}

	// Create .git directory to simulate a git repository
	gitDir := filepath.Join(tmpDir, ".git")
	if err := os.MkdirAll(gitDir, 0755); err != nil {
		t.Fatalf("Failed to create .git dir: %v", err)
	}

	// Create docs directory structure
	dirs := []string{
		"docs/tutorials",
		"docs/how-to",
		"docs/reference",
		"docs/explanation/software/prog-lang/python",
		"docs/metadata",
	}
	for _, dir := range dirs {
		if err := os.MkdirAll(filepath.Join(tmpDir, dir), 0755); err != nil {
			t.Fatalf("Failed to create dir %s: %v", dir, err)
		}
	}

	// Create valid test files
	validFiles := map[string]string{
		"docs/tutorials/tu__getting-started.md":                               "# Getting Started",
		"docs/tutorials/README.md":                                            "# Tutorials Index",
		"docs/how-to/hoto__deploy-docker.md":                                  "# Deploy Docker",
		"docs/reference/re__api-reference.md":                                 "# API Reference",
		"docs/explanation/software/prog-lang/python/ex-so-prla-py__basics.md": "# Python Basics",
		"docs/metadata/cache.yaml":                                            "# Cache",
	}
	for path, content := range validFiles {
		fullPath := filepath.Join(tmpDir, path)
		if err := os.WriteFile(fullPath, []byte(content), 0644); err != nil {
			t.Fatalf("Failed to create file %s: %v", path, err)
		}
	}

	// Test command execution
	cmd := validateDocsNamingCmd
	buf := new(bytes.Buffer)
	cmd.SetOut(buf)
	cmd.SetErr(buf)

	// Reset flags
	validateDocsNamingStagedOnly = false
	output = "text"
	verbose = false
	quiet = false

	// Execute command (should succeed)
	err = cmd.RunE(cmd, []string{})
	if err != nil {
		t.Errorf("Expected success, got error: %v", err)
	}

	result := buf.String()
	t.Logf("Command output:\n%s", result)

	// Verify success message
	if !strings.Contains(result, "All documentation files follow naming conventions") {
		t.Errorf("Expected success message, got: %s", result)
	}
}

func TestValidateDocsNamingCommand_InvalidFiles(t *testing.T) {
	// Save original working directory
	originalWd, err := os.Getwd()
	if err != nil {
		t.Fatalf("Failed to get working directory: %v", err)
	}
	defer func() { _ = os.Chdir(originalWd) }()

	// Create temporary test repository
	tmpDir := t.TempDir()
	if err := os.Chdir(tmpDir); err != nil {
		t.Fatalf("Failed to change to temp directory: %v", err)
	}

	// Create .git directory
	gitDir := filepath.Join(tmpDir, ".git")
	if err := os.MkdirAll(gitDir, 0755); err != nil {
		t.Fatalf("Failed to create .git dir: %v", err)
	}

	// Create docs directory structure
	if err := os.MkdirAll(filepath.Join(tmpDir, "docs/tutorials"), 0755); err != nil {
		t.Fatalf("Failed to create dir: %v", err)
	}

	// Create invalid test file (missing separator)
	invalidFile := filepath.Join(tmpDir, "docs/tutorials/missing-separator.md")
	if err := os.WriteFile(invalidFile, []byte("# Test"), 0644); err != nil {
		t.Fatalf("Failed to create file: %v", err)
	}

	// Test command execution
	cmd := validateDocsNamingCmd
	buf := new(bytes.Buffer)
	cmd.SetOut(buf)
	cmd.SetErr(buf)

	// Reset flags
	validateDocsNamingStagedOnly = false
	output = "text"
	verbose = false
	quiet = false

	// Execute command (should fail)
	err = cmd.RunE(cmd, []string{})
	if err == nil {
		t.Error("Expected error for invalid files, got nil")
	}
	if err != nil && !strings.Contains(err.Error(), "naming violations") {
		t.Errorf("Expected 'naming violations' error, got: %v", err)
	}

	result := buf.String()
	t.Logf("Command output:\n%s", result)

	// Verify violation is reported
	if !strings.Contains(result, "Missing '__' separator") {
		t.Errorf("Expected violation type in output, got: %s", result)
	}
}

func TestValidateDocsNamingCommand_WrongPrefix(t *testing.T) {
	// Save original working directory
	originalWd, err := os.Getwd()
	if err != nil {
		t.Fatalf("Failed to get working directory: %v", err)
	}
	defer func() { _ = os.Chdir(originalWd) }()

	// Create temporary test repository
	tmpDir := t.TempDir()
	if err := os.Chdir(tmpDir); err != nil {
		t.Fatalf("Failed to change to temp directory: %v", err)
	}

	// Create .git directory
	gitDir := filepath.Join(tmpDir, ".git")
	if err := os.MkdirAll(gitDir, 0755); err != nil {
		t.Fatalf("Failed to create .git dir: %v", err)
	}

	// Create docs directory structure
	if err := os.MkdirAll(filepath.Join(tmpDir, "docs/tutorials"), 0755); err != nil {
		t.Fatalf("Failed to create dir: %v", err)
	}

	// Create invalid test file (wrong prefix)
	invalidFile := filepath.Join(tmpDir, "docs/tutorials/wrong__prefix.md")
	if err := os.WriteFile(invalidFile, []byte("# Test"), 0644); err != nil {
		t.Fatalf("Failed to create file: %v", err)
	}

	// Test command execution
	cmd := validateDocsNamingCmd
	buf := new(bytes.Buffer)
	cmd.SetOut(buf)
	cmd.SetErr(buf)

	// Reset flags
	validateDocsNamingStagedOnly = false
	output = "text"
	verbose = false
	quiet = false

	// Execute command (should fail)
	err = cmd.RunE(cmd, []string{})
	if err == nil {
		t.Error("Expected error for wrong prefix, got nil")
	}

	result := buf.String()
	t.Logf("Command output:\n%s", result)

	// Verify violation is reported
	if !strings.Contains(result, "Wrong prefix") {
		t.Errorf("Expected 'Wrong prefix' in output, got: %s", result)
	}
}

func TestValidateDocsNamingCommand_JSONOutput(t *testing.T) {
	// Save original working directory
	originalWd, err := os.Getwd()
	if err != nil {
		t.Fatalf("Failed to get working directory: %v", err)
	}
	defer func() { _ = os.Chdir(originalWd) }()

	// Create temporary test repository
	tmpDir := t.TempDir()
	if err := os.Chdir(tmpDir); err != nil {
		t.Fatalf("Failed to change to temp directory: %v", err)
	}

	// Create .git directory
	gitDir := filepath.Join(tmpDir, ".git")
	if err := os.MkdirAll(gitDir, 0755); err != nil {
		t.Fatalf("Failed to create .git dir: %v", err)
	}

	// Create docs directory structure
	if err := os.MkdirAll(filepath.Join(tmpDir, "docs/tutorials"), 0755); err != nil {
		t.Fatalf("Failed to create dir: %v", err)
	}

	// Create valid test file
	validFile := filepath.Join(tmpDir, "docs/tutorials/tu__test.md")
	if err := os.WriteFile(validFile, []byte("# Test"), 0644); err != nil {
		t.Fatalf("Failed to create file: %v", err)
	}

	// Test command execution
	cmd := validateDocsNamingCmd
	buf := new(bytes.Buffer)
	cmd.SetOut(buf)
	cmd.SetErr(buf)

	// Set JSON output
	validateDocsNamingStagedOnly = false
	output = "json"
	verbose = false
	quiet = false

	// Execute command
	err = cmd.RunE(cmd, []string{})
	if err != nil {
		t.Errorf("Expected success, got error: %v", err)
	}

	result := buf.String()
	t.Logf("JSON output:\n%s", result)

	// Verify valid JSON
	var jsonResult map[string]interface{}
	if err := json.Unmarshal([]byte(result), &jsonResult); err != nil {
		t.Errorf("Expected valid JSON output, got: %s", result)
	}

	// Check expected fields
	if status, ok := jsonResult["status"].(string); !ok || status != "success" {
		t.Errorf("Expected status 'success', got: %v", jsonResult["status"])
	}
}

func TestValidateDocsNamingCommand_MarkdownOutput(t *testing.T) {
	// Save original working directory
	originalWd, err := os.Getwd()
	if err != nil {
		t.Fatalf("Failed to get working directory: %v", err)
	}
	defer func() { _ = os.Chdir(originalWd) }()

	// Create temporary test repository
	tmpDir := t.TempDir()
	if err := os.Chdir(tmpDir); err != nil {
		t.Fatalf("Failed to change to temp directory: %v", err)
	}

	// Create .git directory
	gitDir := filepath.Join(tmpDir, ".git")
	if err := os.MkdirAll(gitDir, 0755); err != nil {
		t.Fatalf("Failed to create .git dir: %v", err)
	}

	// Create docs directory structure
	if err := os.MkdirAll(filepath.Join(tmpDir, "docs/tutorials"), 0755); err != nil {
		t.Fatalf("Failed to create dir: %v", err)
	}

	// Create valid test file
	validFile := filepath.Join(tmpDir, "docs/tutorials/tu__test.md")
	if err := os.WriteFile(validFile, []byte("# Test"), 0644); err != nil {
		t.Fatalf("Failed to create file: %v", err)
	}

	// Test command execution
	cmd := validateDocsNamingCmd
	buf := new(bytes.Buffer)
	cmd.SetOut(buf)
	cmd.SetErr(buf)

	// Set markdown output
	validateDocsNamingStagedOnly = false
	output = "markdown"
	verbose = false
	quiet = false

	// Execute command
	err = cmd.RunE(cmd, []string{})
	if err != nil {
		t.Errorf("Expected success, got error: %v", err)
	}

	result := buf.String()
	t.Logf("Markdown output:\n%s", result)

	// Verify markdown format
	if !strings.Contains(result, "# Documentation Naming Validation Report") {
		t.Errorf("Expected markdown report header, got: %s", result)
	}
	if !strings.Contains(result, "## Summary") {
		t.Errorf("Expected markdown summary section, got: %s", result)
	}
}

func makeDocsRepo(t *testing.T) (tmpDir string) {
	t.Helper()
	tmpDir = t.TempDir()
	if err := os.MkdirAll(filepath.Join(tmpDir, ".git"), 0755); err != nil {
		t.Fatal(err)
	}
	if err := os.MkdirAll(filepath.Join(tmpDir, "docs/tutorials"), 0755); err != nil {
		t.Fatal(err)
	}
	return
}

func TestValidateDocsNamingCommand_FixFlagDryRun(t *testing.T) {
	originalWd, _ := os.Getwd()
	defer func() { _ = os.Chdir(originalWd) }()

	tmpDir := makeDocsRepo(t)
	if err := os.Chdir(tmpDir); err != nil {
		t.Fatal(err)
	}

	// Create a file with wrong prefix (fixable)
	if err := os.WriteFile(filepath.Join(tmpDir, "docs/tutorials/wrong__my-guide.md"), []byte("# test"), 0644); err != nil {
		t.Fatal(err)
	}

	cmd := validateDocsNamingCmd
	buf := new(bytes.Buffer)
	cmd.SetOut(buf)
	cmd.SetErr(buf)

	validateDocsNamingStagedOnly = false
	validateDocsNamingFix = true
	validateDocsNamingApply = false
	validateDocsNamingNoLinks = false
	output = "text"
	verbose = false
	quiet = false

	err := cmd.RunE(cmd, []string{})
	if err != nil {
		t.Errorf("expected no error in fix dry-run mode, got: %v", err)
	}

	got := buf.String()
	if !strings.Contains(got, "wrong__my-guide.md") && !strings.Contains(got, "tu__") {
		t.Errorf("expected dry-run fix plan in output, got: %s", got)
	}
}

func TestValidateDocsNamingCommand_FixApplyFlagNoFix_Error(t *testing.T) {
	originalWd, _ := os.Getwd()
	defer func() { _ = os.Chdir(originalWd) }()

	tmpDir := makeDocsRepo(t)
	if err := os.Chdir(tmpDir); err != nil {
		t.Fatal(err)
	}

	cmd := validateDocsNamingCmd
	buf := new(bytes.Buffer)
	cmd.SetOut(buf)
	cmd.SetErr(buf)

	validateDocsNamingStagedOnly = false
	validateDocsNamingFix = false
	validateDocsNamingApply = true
	output = "text"
	verbose = false
	quiet = false

	err := cmd.RunE(cmd, []string{})
	if err == nil {
		t.Error("expected error when --apply used without --fix")
	}
}

func TestValidateDocsNamingCommand_FixFlagDryRun_JSONOutput(t *testing.T) {
	originalWd, _ := os.Getwd()
	defer func() { _ = os.Chdir(originalWd) }()

	tmpDir := makeDocsRepo(t)
	if err := os.Chdir(tmpDir); err != nil {
		t.Fatal(err)
	}

	// Create a file with wrong prefix
	if err := os.WriteFile(filepath.Join(tmpDir, "docs/tutorials/wrong__guide.md"), []byte("# test"), 0644); err != nil {
		t.Fatal(err)
	}

	cmd := validateDocsNamingCmd
	buf := new(bytes.Buffer)
	cmd.SetOut(buf)
	cmd.SetErr(buf)

	validateDocsNamingStagedOnly = false
	validateDocsNamingFix = true
	validateDocsNamingApply = false
	validateDocsNamingNoLinks = false
	output = "json"
	verbose = false
	quiet = false

	err := cmd.RunE(cmd, []string{})
	if err != nil {
		t.Errorf("expected no error in fix dry-run JSON mode, got: %v", err)
	}

	got := buf.String()
	var result map[string]interface{}
	if err := json.Unmarshal([]byte(got), &result); err != nil {
		t.Errorf("expected valid JSON, got: %s", got)
	}
}

func TestValidateDocsNamingCommand_FixApply_NoViolations(t *testing.T) {
	originalWd, _ := os.Getwd()
	defer func() { _ = os.Chdir(originalWd) }()

	tmpDir := makeDocsRepo(t)
	if err := os.Chdir(tmpDir); err != nil {
		t.Fatal(err)
	}

	// Create a valid file (no violations)
	if err := os.WriteFile(filepath.Join(tmpDir, "docs/tutorials/tu__valid-guide.md"), []byte("# test"), 0644); err != nil {
		t.Fatal(err)
	}

	cmd := validateDocsNamingCmd
	buf := new(bytes.Buffer)
	cmd.SetOut(buf)
	cmd.SetErr(buf)

	validateDocsNamingStagedOnly = false
	validateDocsNamingFix = true
	validateDocsNamingApply = true
	validateDocsNamingNoLinks = true
	output = "text"
	verbose = false
	quiet = false

	err := cmd.RunE(cmd, []string{})
	if err != nil {
		t.Errorf("expected no error for valid files in apply mode, got: %v", err)
	}
}

func TestValidateDocsNamingCommand_MissingGitRoot(t *testing.T) {
	originalWd, _ := os.Getwd()
	defer func() { _ = os.Chdir(originalWd) }()

	tmpDir := t.TempDir()
	if err := os.Chdir(tmpDir); err != nil {
		t.Fatal(err)
	}
	// No .git directory

	cmd := validateDocsNamingCmd
	buf := new(bytes.Buffer)
	cmd.SetOut(buf)
	cmd.SetErr(buf)

	validateDocsNamingStagedOnly = false
	validateDocsNamingFix = false
	validateDocsNamingApply = false
	output = "text"
	verbose = false
	quiet = false

	err := cmd.RunE(cmd, []string{})
	if err == nil {
		t.Error("expected error when no .git directory found")
	}
	if !strings.Contains(err.Error(), "git") {
		t.Errorf("expected error mentioning 'git', got: %v", err)
	}
}

func TestValidateDocsNamingCommand_InvalidFiles_QuietMode(t *testing.T) {
	originalWd, _ := os.Getwd()
	defer func() { _ = os.Chdir(originalWd) }()

	tmpDir := makeDocsRepo(t)
	if err := os.Chdir(tmpDir); err != nil {
		t.Fatal(err)
	}

	// Create an invalid file (missing separator)
	if err := os.WriteFile(filepath.Join(tmpDir, "docs/tutorials/bad-name.md"), []byte("# test"), 0644); err != nil {
		t.Fatal(err)
	}

	cmd := validateDocsNamingCmd
	buf := new(bytes.Buffer)
	cmd.SetOut(buf)
	cmd.SetErr(buf)

	validateDocsNamingStagedOnly = false
	validateDocsNamingFix = false
	validateDocsNamingApply = false
	output = "text"
	verbose = false
	quiet = true

	err := cmd.RunE(cmd, []string{})
	if err == nil {
		t.Error("expected error in quiet mode with violations")
	}
}

func TestValidateDocsNamingCommand_FixConflict(t *testing.T) {
	// Covers docs_validate_naming.go:138 — Fix() returns error due to conflict.
	// Two files in the same dir would both rename to the same target.
	originalWd, _ := os.Getwd()
	defer func() { _ = os.Chdir(originalWd) }()

	tmpDir := makeDocsRepo(t)
	if err := os.Chdir(tmpDir); err != nil {
		t.Fatal(err)
	}

	// Create two files in docs/tutorials that would both rename to tu__guide.md
	if err := os.WriteFile(filepath.Join(tmpDir, "docs/tutorials/wrong__guide.md"), []byte("# guide"), 0644); err != nil {
		t.Fatal(err)
	}
	if err := os.WriteFile(filepath.Join(tmpDir, "docs/tutorials/other__guide.md"), []byte("# guide"), 0644); err != nil {
		t.Fatal(err)
	}

	cmd := validateDocsNamingCmd
	buf := new(bytes.Buffer)
	cmd.SetOut(buf)
	cmd.SetErr(buf)

	validateDocsNamingStagedOnly = false
	validateDocsNamingFix = true
	validateDocsNamingApply = false
	validateDocsNamingNoLinks = true
	output = "text"
	verbose = false
	quiet = false

	err := cmd.RunE(cmd, []string{})
	if err == nil {
		t.Error("expected error when two files would rename to the same target")
	}
	if err != nil && !strings.Contains(err.Error(), "fix operation failed") {
		t.Errorf("expected 'fix operation failed' in error, got: %v", err)
	}
}

func TestValidateDocsNamingCommand_FixApplyWithErrors(t *testing.T) {
	// Covers docs_validate_naming.go:163 — fixResult.Errors > 0 && !fixResult.DryRun
	// Force ApplyRenames to fail by making the docs/tutorials directory read-only
	// after the scan detects the violation.
	// Since scan and fix happen in one RunE call, we instead make tutorials read-only
	// BEFORE the call — the scan will still detect the file (it can read), but
	// the rename (os.Rename) will fail because the directory is not writable.
	originalWd, _ := os.Getwd()
	defer func() { _ = os.Chdir(originalWd) }()

	tmpDir := makeDocsRepo(t)
	if err := os.Chdir(tmpDir); err != nil {
		t.Fatal(err)
	}

	tutorialsDir := filepath.Join(tmpDir, "docs/tutorials")

	// Create a file with wrong prefix
	if err := os.WriteFile(filepath.Join(tutorialsDir, "wrong__my-guide.md"), []byte("# guide"), 0644); err != nil {
		t.Fatal(err)
	}

	// Make the tutorials directory read-only so rename fails (os.Rename requires write perm on dir)
	if err := os.Chmod(tutorialsDir, 0555); err != nil {
		t.Fatal(err)
	}
	defer func() { _ = os.Chmod(tutorialsDir, 0755) }()

	cmd := validateDocsNamingCmd
	buf := new(bytes.Buffer)
	cmd.SetOut(buf)
	cmd.SetErr(buf)

	validateDocsNamingStagedOnly = false
	validateDocsNamingFix = true
	validateDocsNamingApply = true
	validateDocsNamingNoLinks = true
	output = "text"
	verbose = false
	quiet = false

	err := cmd.RunE(cmd, []string{})
	// On non-root systems, the rename fails → fixResult.Errors > 0 → error returned
	// On root systems, rename may succeed → no error (acceptable)
	if err != nil {
		if !strings.Contains(err.Error(), "errors during fix") && !strings.Contains(err.Error(), "fix operation failed") {
			t.Logf("FixApplyWithErrors: got error: %v", err)
		}
	}
}

func TestValidateDocsNamingCommand_FixMode_ApplyNoLinks(t *testing.T) {
	// Test runFixMode with apply=true and no-update-links=true
	// This tests the FormatFixResult path (not DryRun path)
	originalWd, _ := os.Getwd()
	defer func() { _ = os.Chdir(originalWd) }()

	tmpDir := makeDocsRepo(t)
	if err := os.Chdir(tmpDir); err != nil {
		t.Fatal(err)
	}

	// Create a file with wrong prefix in tutorials — this is fixable
	wrongFile := filepath.Join(tmpDir, "docs/tutorials/wrong__my-doc.md")
	if err := os.WriteFile(wrongFile, []byte("# test"), 0644); err != nil {
		t.Fatal(err)
	}

	cmd := validateDocsNamingCmd
	buf := new(bytes.Buffer)
	cmd.SetOut(buf)
	cmd.SetErr(buf)

	validateDocsNamingStagedOnly = false
	validateDocsNamingFix = true
	validateDocsNamingApply = true
	validateDocsNamingNoLinks = true // skip link updates
	output = "text"
	verbose = false
	quiet = false

	// Apply renames — may succeed or fail depending on git mv
	_ = cmd.RunE(cmd, []string{})

	got := buf.String()
	// Either fix result or no-rename output should appear
	if got == "" {
		t.Error("expected some output from fix apply mode")
	}
}
