package cmd

import (
	"bytes"
	"os"
	"path/filepath"
	"strings"
	"testing"
)

// makeDartContractsDirWithModels creates a temp dir with model Dart files.
func makeDartContractsDirWithModels(t *testing.T) string {
	t.Helper()
	dir := t.TempDir()

	modelDir := filepath.Join(dir, "lib", "model")
	if err := os.MkdirAll(modelDir, 0755); err != nil {
		t.Fatal(err)
	}
	if err := os.WriteFile(filepath.Join(modelDir, "foo_model.dart"), []byte("// model"), 0644); err != nil {
		t.Fatal(err)
	}
	if err := os.WriteFile(filepath.Join(modelDir, "bar_model.dart"), []byte("// model"), 0644); err != nil {
		t.Fatal(err)
	}
	return dir
}

func TestContractsDartScaffoldCmd_NoArgs(t *testing.T) {
	err := contractsDartScaffoldCmd.Args(contractsDartScaffoldCmd, []string{})
	if err == nil {
		t.Error("expected error when no args provided")
	}
}

func TestContractsDartScaffoldCmd_WithModels(t *testing.T) {
	src := makeDartContractsDirWithModels(t)

	cmd := contractsDartScaffoldCmd
	buf := new(bytes.Buffer)
	cmd.SetOut(buf)
	cmd.SetErr(buf)

	output = "text"
	verbose = false
	quiet = false

	err := cmd.RunE(cmd, []string{src})
	if err != nil {
		t.Errorf("expected no error, got: %v", err)
	}

	// Verify pubspec.yaml was created with correct content
	pubspecPath := filepath.Join(src, "pubspec.yaml")
	pubspecData, readErr := os.ReadFile(pubspecPath)
	if readErr != nil {
		t.Fatalf("expected pubspec.yaml to be created, got error: %v", readErr)
	}
	if !strings.Contains(string(pubspecData), "name: demo_contracts") {
		t.Errorf("expected pubspec.yaml to contain 'name: demo_contracts', got: %s", string(pubspecData))
	}

	// Verify barrel file was created with sorted part directives
	barrelPath := filepath.Join(src, "lib", "demo_contracts.dart")
	barrelData, readErr := os.ReadFile(barrelPath)
	if readErr != nil {
		t.Fatalf("expected barrel library to be created, got error: %v", readErr)
	}
	barrelContent := string(barrelData)
	if !strings.Contains(barrelContent, "part 'model/bar_model.dart';") {
		t.Errorf("expected barrel to contain bar_model.dart part directive, got: %s", barrelContent)
	}
	if !strings.Contains(barrelContent, "part 'model/foo_model.dart';") {
		t.Errorf("expected barrel to contain foo_model.dart part directive, got: %s", barrelContent)
	}
	// Verify sorted order: bar comes before foo
	barIdx := strings.Index(barrelContent, "bar_model.dart")
	fooIdx := strings.Index(barrelContent, "foo_model.dart")
	if barIdx > fooIdx {
		t.Errorf("expected bar_model.dart to appear before foo_model.dart (sorted), got wrong order")
	}
}

func TestContractsDartScaffoldCmd_NoModels(t *testing.T) {
	src := t.TempDir()

	// Create lib dir but no model files
	if err := os.MkdirAll(filepath.Join(src, "lib"), 0755); err != nil {
		t.Fatal(err)
	}

	cmd := contractsDartScaffoldCmd
	buf := new(bytes.Buffer)
	cmd.SetOut(buf)
	cmd.SetErr(buf)

	output = "text"
	verbose = false
	quiet = false

	err := cmd.RunE(cmd, []string{src})
	if err != nil {
		t.Errorf("expected no error, got: %v", err)
	}

	// Verify pubspec.yaml and barrel are still created
	if _, statErr := os.Stat(filepath.Join(src, "pubspec.yaml")); statErr != nil {
		t.Errorf("expected pubspec.yaml to be created even with no models, got error: %v", statErr)
	}
	if _, statErr := os.Stat(filepath.Join(src, "lib", "demo_contracts.dart")); statErr != nil {
		t.Errorf("expected barrel library to be created even with no models, got error: %v", statErr)
	}
}

func TestContractsDartScaffoldCmd_JSONOutput(t *testing.T) {
	src := makeDartContractsDirWithModels(t)

	cmd := contractsDartScaffoldCmd
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
	if !strings.Contains(got, `"pubspec_created"`) {
		t.Errorf("expected 'pubspec_created' field in JSON output, got: %s", got)
	}
	if !strings.Contains(got, `"barrel_created"`) {
		t.Errorf("expected 'barrel_created' field in JSON output, got: %s", got)
	}
	if !strings.Contains(got, `"model_files"`) {
		t.Errorf("expected 'model_files' field in JSON output, got: %s", got)
	}
}

func TestContractsDartScaffoldCmd_MarkdownOutput(t *testing.T) {
	src := makeDartContractsDirWithModels(t)

	cmd := contractsDartScaffoldCmd
	buf := new(bytes.Buffer)
	cmd.SetOut(buf)
	cmd.SetErr(buf)

	output = "markdown"
	verbose = false
	quiet = false

	_ = cmd.RunE(cmd, []string{src})

	got := buf.String()
	if !strings.Contains(got, "# Dart Contract Scaffold Report") {
		t.Errorf("expected markdown heading in output, got: %s", got)
	}
}

func TestContractsDartScaffoldCmd_QuietMode(t *testing.T) {
	src := makeDartContractsDirWithModels(t)

	cmd := contractsDartScaffoldCmd
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
	if strings.Contains(got, "Dart scaffold created") {
		t.Error("quiet mode should suppress the full summary message")
	}
}
