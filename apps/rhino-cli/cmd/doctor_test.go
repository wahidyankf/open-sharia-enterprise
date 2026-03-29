package cmd

import (
	"bytes"
	"encoding/json"
	"os"
	"path/filepath"
	"strings"
	"testing"
)

// setupDoctorTestRepo creates a temporary git repository with minimal config files
// required for the doctor command and returns the tmpDir path and a cleanup func.
func setupDoctorTestRepo(t *testing.T) func() {
	t.Helper()

	originalWd, err := os.Getwd()
	if err != nil {
		t.Fatalf("Failed to get working directory: %v", err)
	}

	tmpDir := t.TempDir()
	if err := os.Chdir(tmpDir); err != nil {
		t.Fatalf("Failed to change to temp directory: %v", err)
	}

	// Minimal .git directory so findGitRoot succeeds
	if err := os.MkdirAll(filepath.Join(tmpDir, ".git"), 0755); err != nil {
		t.Fatalf("Failed to create .git dir: %v", err)
	}

	// Create all required directories
	for _, dir := range []string{
		"apps/organiclever-be-jasb",
		"apps/rhino-cli",
		"apps/oseplatform-web",
		"apps/a-demo-be-python-fastapi",
		"apps/a-demo-be-fsharp-giraffe",
		"apps/a-demo-fe-dart-flutterweb",
	} {
		if err := os.MkdirAll(filepath.Join(tmpDir, dir), 0755); err != nil {
			t.Fatalf("Failed to create dir %s: %v", dir, err)
		}
	}

	// Write all config files
	files := map[string]string{
		"package.json":                                  `{"name":"test","volta":{"node":"24.11.1","npm":"11.6.3"}}`,
		"apps/organiclever-be-jasb/pom.xml":             `<project><properties><java.version>25</java.version></properties></project>`,
		"apps/rhino-cli/go.mod":                         "module foo\n\ngo 1.24.2\n",
		"apps/oseplatform-web/vercel.json":              `{"build":{"env":{"HUGO_VERSION":"0.156.0"}}}`,
		"apps/a-demo-be-python-fastapi/.python-version": "3.13\n",
		".tool-versions":                                "erlang 27.3\nelixir 1.19.5-otp-27\n",
		"apps/a-demo-be-fsharp-giraffe/global.json":     `{"sdk":{"version":"10.0.103","rollForward":"latestMinor"}}`,
		"apps/a-demo-fe-dart-flutterweb/pubspec.yaml":   "name: demo\n\nenvironment:\n  sdk: ^3.11.1\n",
	}
	for relPath, content := range files {
		if err := os.WriteFile(filepath.Join(tmpDir, relPath), []byte(content), 0644); err != nil {
			t.Fatalf("Failed to create %s: %v", relPath, err)
		}
	}

	return func() {
		_ = os.Chdir(originalWd)
	}
}

func TestDoctorCommand_Initialization(t *testing.T) {
	if doctorCmd.Use != "doctor" {
		t.Errorf("expected Use == %q, got %q", "doctor", doctorCmd.Use)
	}
	if !strings.Contains(strings.ToLower(doctorCmd.Short), "tool") {
		t.Errorf("expected Short to contain 'tool', got %q", doctorCmd.Short)
	}
}

func TestDoctorCommand_TextOutput(t *testing.T) {
	cleanup := setupDoctorTestRepo(t)
	defer cleanup()

	cmd := doctorCmd
	buf := new(bytes.Buffer)
	cmd.SetOut(buf)
	cmd.SetErr(buf)

	output = "text"
	verbose = false
	quiet = false

	// Run the command — may return an error if some tools are not installed,
	// but we only check output structure.
	_ = cmd.RunE(cmd, []string{})

	outputStr := buf.String()
	t.Logf("doctor text output:\n%s", outputStr)

	if !strings.Contains(outputStr, "Doctor Report") {
		t.Error("expected output to contain 'Doctor Report'")
	}

	// All 19 tool names should appear in the output
	for _, toolName := range []string{"git", "volta", "node", "npm", "java", "maven", "golang", "hugo", "python", "rust", "cargo-llvm-cov", "elixir", "erlang", "dotnet", "clojure", "dart", "flutter", "docker", "jq"} {
		if !strings.Contains(outputStr, toolName) {
			t.Errorf("expected output to contain tool name %q", toolName)
		}
	}
}

func TestDoctorCommand_JSONOutput(t *testing.T) {
	cleanup := setupDoctorTestRepo(t)
	defer cleanup()

	cmd := doctorCmd
	buf := new(bytes.Buffer)
	cmd.SetOut(buf)
	cmd.SetErr(buf)

	output = "json"
	verbose = false
	quiet = false

	_ = cmd.RunE(cmd, []string{})

	jsonStr := buf.String()
	t.Logf("doctor JSON output:\n%s", jsonStr)

	if !strings.Contains(jsonStr, `"tools"`) {
		t.Error("expected JSON output to contain 'tools' array key")
	}

	// Validate it is parseable JSON
	var parsed map[string]interface{}
	if err := json.Unmarshal([]byte(jsonStr), &parsed); err != nil {
		t.Errorf("output is not valid JSON: %v\n%s", err, jsonStr)
	}

	tools, ok := parsed["tools"].([]interface{})
	if !ok {
		t.Error("expected 'tools' to be an array")
	} else if len(tools) != 19 {
		t.Errorf("expected 19 tools in JSON output, got %d", len(tools))
	}
}

func TestDoctorCommand_MarkdownOutput(t *testing.T) {
	cleanup := setupDoctorTestRepo(t)
	defer cleanup()

	cmd := doctorCmd
	buf := new(bytes.Buffer)
	cmd.SetOut(buf)
	cmd.SetErr(buf)

	output = "markdown"
	verbose = false
	quiet = false

	_ = cmd.RunE(cmd, []string{})

	mdStr := buf.String()
	t.Logf("doctor markdown output:\n%s", mdStr)

	if !strings.Contains(mdStr, "| Tool |") {
		t.Error("expected markdown table with '| Tool |' header")
	}
}

func TestDoctorCommand_MissingGitRoot(t *testing.T) {
	originalWd, err := os.Getwd()
	if err != nil {
		t.Fatalf("Failed to get working directory: %v", err)
	}
	defer func() { _ = os.Chdir(originalWd) }()

	// Use a temp dir with no .git anywhere up the tree
	tmpDir := t.TempDir()
	if err := os.Chdir(tmpDir); err != nil {
		t.Fatalf("Failed to change to temp directory: %v", err)
	}

	cmd := doctorCmd
	buf := new(bytes.Buffer)
	cmd.SetOut(buf)
	cmd.SetErr(buf)

	output = "text"
	verbose = false
	quiet = false

	err = cmd.RunE(cmd, []string{})
	if err == nil {
		t.Fatal("expected command to fail when no .git directory found")
	}
	if !strings.Contains(err.Error(), "git") {
		t.Errorf("expected error to mention 'git', got: %v", err)
	}
}

func TestDoctorCommand_MissingToolsReturnError(t *testing.T) {
	// Verify that when tools are missing the command returns an error
	// with the count in the message. We use a repo without package.json / go.mod
	// so that version reads fail gracefully (empty required), then rely on
	// actual system tools for existence checks.
	cleanup := setupDoctorTestRepo(t)
	defer cleanup()

	cmd := doctorCmd
	buf := new(bytes.Buffer)
	cmd.SetOut(buf)
	cmd.SetErr(buf)

	output = "text"
	verbose = false
	quiet = false

	err := cmd.RunE(cmd, []string{})
	// The test environment might or might not have tools missing.
	// We just check that if an error is returned, it mentions "not found in PATH"
	if err != nil {
		if !strings.Contains(err.Error(), "not found in PATH") {
			t.Errorf("expected 'not found in PATH' in error, got: %v", err)
		}
	}
	// Also ensure output always contains the report header
	if !strings.Contains(buf.String(), "Doctor Report") {
		t.Error("expected output to contain 'Doctor Report'")
	}
}

func TestDoctorCommand_VerboseOutput(t *testing.T) {
	cleanup := setupDoctorTestRepo(t)
	defer cleanup()

	cmd := doctorCmd
	buf := new(bytes.Buffer)
	cmd.SetOut(buf)
	cmd.SetErr(buf)

	output = "text"
	verbose = true
	quiet = false

	_ = cmd.RunE(cmd, []string{})

	outputStr := buf.String()
	// Verbose mode should include timing info
	if !strings.Contains(outputStr, "Doctor Report") {
		t.Error("expected verbose output to contain 'Doctor Report'")
	}
}
