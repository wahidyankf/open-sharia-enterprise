package cmd

import (
	"bytes"
	"os"
	"path/filepath"
	"strings"
	"testing"
)

// setupJavaSourceRoot creates a temp dir with a fake .git and a Java source tree.
// It returns the temp dir root and the Java source root path.
func setupJavaSourceRoot(t *testing.T) (string, string) {
	t.Helper()

	tmpDir := t.TempDir()

	// Create .git to simulate a git repository
	if err := os.MkdirAll(filepath.Join(tmpDir, ".git"), 0755); err != nil {
		t.Fatalf("failed to create .git: %v", err)
	}

	srcRoot := filepath.Join(tmpDir, "src", "main", "java")
	return tmpDir, srcRoot
}

func writeJavaFile(t *testing.T, path, content string) {
	t.Helper()
	if err := os.MkdirAll(filepath.Dir(path), 0755); err != nil {
		t.Fatalf("MkdirAll: %v", err)
	}
	if err := os.WriteFile(path, []byte(content), 0644); err != nil {
		t.Fatalf("WriteFile(%s): %v", path, err)
	}
}

func TestValidateJavaNullSafetyCommand_AllValid(t *testing.T) {
	originalWd, err := os.Getwd()
	if err != nil {
		t.Fatalf("Getwd: %v", err)
	}
	defer func() { _ = os.Chdir(originalWd) }()

	tmpDir, srcRoot := setupJavaSourceRoot(t)
	if err := os.Chdir(tmpDir); err != nil {
		t.Fatalf("Chdir: %v", err)
	}

	pkgDir := filepath.Join(srcRoot, "com", "example")
	writeJavaFile(t, filepath.Join(pkgDir, "Foo.java"), "class Foo {}")
	writeJavaFile(t, filepath.Join(pkgDir, "package-info.java"),
		"@NullMarked\npackage com.example;\n")

	cmd := validateJavaNullSafetyCmd
	buf := new(bytes.Buffer)
	cmd.SetOut(buf)
	cmd.SetErr(buf)

	// Reset flags
	validateJavaNullSafetyAnnotation = "NullMarked"
	output = "text"
	verbose = false
	quiet = false

	// Use relative path (resolved from tmpDir)
	relSrcRoot, _ := filepath.Rel(tmpDir, srcRoot)
	err = cmd.RunE(cmd, []string{relSrcRoot})
	if err != nil {
		t.Errorf("Command should succeed with all-valid packages, got: %v", err)
	}

	out := buf.String()
	t.Logf("Output:\n%s", out)

	if !strings.Contains(out, "✓") {
		t.Error("expected ✓ in output for valid packages")
	}
	if !strings.Contains(out, "0 violations found") {
		t.Error("expected '0 violations found'")
	}
}

func TestValidateJavaNullSafetyCommand_MissingPackageInfo(t *testing.T) {
	originalWd, err := os.Getwd()
	if err != nil {
		t.Fatalf("Getwd: %v", err)
	}
	defer func() { _ = os.Chdir(originalWd) }()

	tmpDir, srcRoot := setupJavaSourceRoot(t)
	if err := os.Chdir(tmpDir); err != nil {
		t.Fatalf("Chdir: %v", err)
	}

	pkgDir := filepath.Join(srcRoot, "com", "example")
	writeJavaFile(t, filepath.Join(pkgDir, "Foo.java"), "class Foo {}")
	// No package-info.java

	cmd := validateJavaNullSafetyCmd
	buf := new(bytes.Buffer)
	cmd.SetOut(buf)
	cmd.SetErr(buf)

	validateJavaNullSafetyAnnotation = "NullMarked"
	output = "text"
	verbose = false
	quiet = false

	relSrcRoot, _ := filepath.Rel(tmpDir, srcRoot)
	err = cmd.RunE(cmd, []string{relSrcRoot})
	if err == nil {
		t.Error("Command should fail when package-info.java is missing")
	}

	out := buf.String()
	t.Logf("Output:\n%s", out)

	if !strings.Contains(out, "✗") {
		t.Error("expected ✗ for missing package-info.java")
	}
	if !strings.Contains(out, "package-info.java missing") {
		t.Error("expected 'package-info.java missing' in output")
	}
}

func TestValidateJavaNullSafetyCommand_MissingAnnotation(t *testing.T) {
	originalWd, err := os.Getwd()
	if err != nil {
		t.Fatalf("Getwd: %v", err)
	}
	defer func() { _ = os.Chdir(originalWd) }()

	tmpDir, srcRoot := setupJavaSourceRoot(t)
	if err := os.Chdir(tmpDir); err != nil {
		t.Fatalf("Chdir: %v", err)
	}

	pkgDir := filepath.Join(srcRoot, "com", "example")
	writeJavaFile(t, filepath.Join(pkgDir, "Foo.java"), "class Foo {}")
	writeJavaFile(t, filepath.Join(pkgDir, "package-info.java"), "package com.example;") // no annotation

	cmd := validateJavaNullSafetyCmd
	buf := new(bytes.Buffer)
	cmd.SetOut(buf)
	cmd.SetErr(buf)

	validateJavaNullSafetyAnnotation = "NullMarked"
	output = "text"
	verbose = false
	quiet = false

	relSrcRoot, _ := filepath.Rel(tmpDir, srcRoot)
	err = cmd.RunE(cmd, []string{relSrcRoot})
	if err == nil {
		t.Error("Command should fail when annotation is missing")
	}

	out := buf.String()
	t.Logf("Output:\n%s", out)

	if !strings.Contains(out, "@NullMarked missing") {
		t.Error("expected '@NullMarked missing' in output")
	}
}

func TestValidateJavaNullSafetyCommand_JSONOutput(t *testing.T) {
	originalWd, err := os.Getwd()
	if err != nil {
		t.Fatalf("Getwd: %v", err)
	}
	defer func() { _ = os.Chdir(originalWd) }()

	tmpDir, srcRoot := setupJavaSourceRoot(t)
	if err := os.Chdir(tmpDir); err != nil {
		t.Fatalf("Chdir: %v", err)
	}

	pkgDir := filepath.Join(srcRoot, "com", "example")
	writeJavaFile(t, filepath.Join(pkgDir, "Foo.java"), "class Foo {}")
	writeJavaFile(t, filepath.Join(pkgDir, "package-info.java"),
		"@NullMarked\npackage com.example;\n")

	cmd := validateJavaNullSafetyCmd
	buf := new(bytes.Buffer)
	cmd.SetOut(buf)
	cmd.SetErr(buf)

	validateJavaNullSafetyAnnotation = "NullMarked"
	output = "json"
	verbose = false
	quiet = false

	relSrcRoot, _ := filepath.Rel(tmpDir, srcRoot)
	_ = cmd.RunE(cmd, []string{relSrcRoot})

	out := buf.String()
	t.Logf("JSON output:\n%s", out)

	if !strings.Contains(out, `"status"`) {
		t.Error("expected 'status' field in JSON")
	}
	if !strings.Contains(out, `"total_packages"`) {
		t.Error("expected 'total_packages' field in JSON")
	}
	if !strings.Contains(out, `"violations"`) {
		t.Error("expected 'violations' field in JSON")
	}
}

func TestValidateJavaNullSafetyCommand_CustomAnnotation(t *testing.T) {
	originalWd, err := os.Getwd()
	if err != nil {
		t.Fatalf("Getwd: %v", err)
	}
	defer func() { _ = os.Chdir(originalWd) }()

	tmpDir, srcRoot := setupJavaSourceRoot(t)
	if err := os.Chdir(tmpDir); err != nil {
		t.Fatalf("Chdir: %v", err)
	}

	pkgDir := filepath.Join(srcRoot, "com", "example")
	writeJavaFile(t, filepath.Join(pkgDir, "Foo.java"), "class Foo {}")
	writeJavaFile(t, filepath.Join(pkgDir, "package-info.java"),
		"@NonNullByDefault\npackage com.example;\n")

	cmd := validateJavaNullSafetyCmd
	buf := new(bytes.Buffer)
	cmd.SetOut(buf)
	cmd.SetErr(buf)

	validateJavaNullSafetyAnnotation = "NonNullByDefault"
	output = "text"
	verbose = false
	quiet = false

	relSrcRoot, _ := filepath.Rel(tmpDir, srcRoot)
	err = cmd.RunE(cmd, []string{relSrcRoot})
	if err != nil {
		t.Errorf("Command should succeed with matching custom annotation, got: %v", err)
	}

	out := buf.String()
	if !strings.Contains(out, "@NonNullByDefault found") {
		t.Error("expected custom annotation in output")
	}
}

func TestValidateJavaNullSafetyCommand_EmptySourceRoot(t *testing.T) {
	originalWd, err := os.Getwd()
	if err != nil {
		t.Fatalf("Getwd: %v", err)
	}
	defer func() { _ = os.Chdir(originalWd) }()

	tmpDir, srcRoot := setupJavaSourceRoot(t)
	if err := os.Chdir(tmpDir); err != nil {
		t.Fatalf("Chdir: %v", err)
	}

	// Create empty source root directory
	if err := os.MkdirAll(srcRoot, 0755); err != nil {
		t.Fatalf("MkdirAll: %v", err)
	}

	cmd := validateJavaNullSafetyCmd
	buf := new(bytes.Buffer)
	cmd.SetOut(buf)
	cmd.SetErr(buf)

	validateJavaNullSafetyAnnotation = "NullMarked"
	output = "text"
	verbose = false
	quiet = false

	relSrcRoot, _ := filepath.Rel(tmpDir, srcRoot)
	err = cmd.RunE(cmd, []string{relSrcRoot})
	if err != nil {
		t.Errorf("Command should succeed with empty source root, got: %v", err)
	}

	out := buf.String()
	if !strings.Contains(out, "0 violations found") {
		t.Error("expected '0 violations found' for empty source root")
	}
}
