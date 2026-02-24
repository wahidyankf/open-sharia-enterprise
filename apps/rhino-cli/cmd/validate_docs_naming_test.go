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
