package doctor

import (
	"fmt"
	"os"
	"path/filepath"
	"strings"
	"testing"
)

// fakeRunnerConfig holds the response for a fake command runner entry.
type fakeRunnerConfig struct {
	stdout   string
	stderr   string
	exitCode int
	missing  bool // if true, return an error simulating binary not found
}

// makeFakeRunner returns a CommandRunner that looks up responses by binary name.
// If the binary is not in the map or config.missing is true, it returns an error.
func makeFakeRunner(responses map[string]fakeRunnerConfig) CommandRunner {
	return func(name string, args ...string) (stdout, stderr string, exitCode int, err error) {
		cfg, ok := responses[name]
		if !ok || cfg.missing {
			return "", "", -1, fmt.Errorf("binary not found in PATH: %s", name)
		}
		return cfg.stdout, cfg.stderr, cfg.exitCode, nil
	}
}

func TestNormalizeSimpleVersion(t *testing.T) {
	tests := []struct {
		input string
		want  string
	}{
		{"v24.11.1", "24.11.1"},
		{"24.11.1", "24.11.1"},
		{"v1.0.0", "1.0.0"},
		{"", ""},
		{"v", ""},
		{"v2.0.2", "2.0.2"},
	}
	for _, tt := range tests {
		got := normalizeSimpleVersion(tt.input)
		if got != tt.want {
			t.Errorf("normalizeSimpleVersion(%q) = %q, want %q", tt.input, got, tt.want)
		}
	}
}

func TestParseJavaVersion(t *testing.T) {
	tests := []struct {
		name   string
		stderr string
		want   string
	}{
		{
			name:   "openjdk new style major only",
			stderr: `openjdk version "25" 2025-09-16`,
			want:   "25",
		},
		{
			name:   "openjdk with patch version",
			stderr: `openjdk version "21.0.1" 2023-10-17`,
			want:   "21",
		},
		{
			name:   "old java 1.8 style",
			stderr: `java version "1.8.0_292"`,
			want:   "8",
		},
		{
			name:   "multiline openjdk output",
			stderr: "openjdk version \"21.0.1\" 2023-10-17\nOpenJDK Runtime Environment\n",
			want:   "21",
		},
		{
			name:   "empty stderr",
			stderr: "",
			want:   "",
		},
		{
			name:   "no version line",
			stderr: "some other output",
			want:   "",
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			got := parseJavaVersion(tt.stderr)
			if got != tt.want {
				t.Errorf("parseJavaVersion(%q) = %q, want %q", tt.stderr, got, tt.want)
			}
		})
	}
}

func TestParseLineWord(t *testing.T) {
	tests := []struct {
		name        string
		output      string
		linePrefix  string
		wordIdx     int
		tokenPrefix string
		want        string
	}{
		// git --version cases
		{name: "git standard", output: "git version 2.47.2", linePrefix: "git version ", wordIdx: 2, tokenPrefix: "", want: "2.47.2"},
		{name: "git windows suffix", output: "git version 2.47.2.windows.1", linePrefix: "git version ", wordIdx: 2, tokenPrefix: "", want: "2.47.2.windows.1"},
		{name: "git trailing newline", output: "git version 2.47.2\n", linePrefix: "git version ", wordIdx: 2, tokenPrefix: "", want: "2.47.2"},
		// mvn --version cases
		{name: "maven standard", output: "Apache Maven 3.9.9 (8e8579a9e76f7d015ee5ec7bfcdc97d260186937)\nMaven home: /usr/share/maven", linePrefix: "Apache Maven ", wordIdx: 2, tokenPrefix: "", want: "3.9.9"},
		{name: "maven leading whitespace", output: "  Apache Maven 3.8.6\n", linePrefix: "Apache Maven ", wordIdx: 2, tokenPrefix: "", want: "3.8.6"},
		// go version cases
		{name: "go linux", output: "go version go1.24.2 linux/amd64", linePrefix: "go version ", wordIdx: 2, tokenPrefix: "go", want: "1.24.2"},
		{name: "go darwin", output: "go version go1.23.0 darwin/arm64", linePrefix: "go version ", wordIdx: 2, tokenPrefix: "go", want: "1.23.0"},
		{name: "go windows", output: "go version go1.22.1 windows/amd64", linePrefix: "go version ", wordIdx: 2, tokenPrefix: "go", want: "1.22.1"},
		// edge cases
		{name: "empty output", output: "", linePrefix: "git version ", wordIdx: 2, tokenPrefix: "", want: ""},
		{name: "no matching line", output: "some other output", linePrefix: "git version ", wordIdx: 2, tokenPrefix: "", want: ""},
		{name: "word index out of bounds", output: "git version", linePrefix: "git version ", wordIdx: 5, tokenPrefix: "", want: ""},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			got := parseLineWord(tt.output, tt.linePrefix, tt.wordIdx, tt.tokenPrefix)
			if got != tt.want {
				t.Errorf("parseLineWord(%q, %q, %d, %q) = %q, want %q",
					tt.output, tt.linePrefix, tt.wordIdx, tt.tokenPrefix, got, tt.want)
			}
		})
	}
}

func TestParseVersionParts(t *testing.T) {
	tests := []struct {
		input     string
		wantMajor int
		wantMinor int
		wantPatch int
		wantOK    bool
	}{
		{"1.24.2", 1, 24, 2, true},
		{"v1.24.2", 1, 24, 2, true},
		{"1.24", 1, 24, 0, true},
		{"25", 25, 0, 0, true},
		{"", 0, 0, 0, false},
		{"not.a.version", 0, 0, 0, false},
		{"1.x.0", 0, 0, 0, false},
	}
	for _, tt := range tests {
		t.Run(tt.input, func(t *testing.T) {
			maj, min, pat, ok := parseVersionParts(tt.input)
			if ok != tt.wantOK {
				t.Errorf("parseVersionParts(%q) ok = %v, want %v", tt.input, ok, tt.wantOK)
			}
			if ok && (maj != tt.wantMajor || min != tt.wantMinor || pat != tt.wantPatch) {
				t.Errorf("parseVersionParts(%q) = (%d,%d,%d), want (%d,%d,%d)",
					tt.input, maj, min, pat, tt.wantMajor, tt.wantMinor, tt.wantPatch)
			}
		})
	}
}

func TestCompareGTE(t *testing.T) {
	tests := []struct {
		name       string
		installed  string
		required   string
		wantStatus ToolStatus
	}{
		{"exact match", "1.24.2", "1.24.2", StatusOK},
		{"newer minor", "1.26.0", "1.24.2", StatusOK},
		{"newer major", "2.0.0", "1.24.2", StatusOK},
		{"newer patch", "1.24.3", "1.24.2", StatusOK},
		{"older minor", "1.23.0", "1.24.2", StatusWarning},
		{"older patch", "1.24.1", "1.24.2", StatusWarning},
		{"older major", "0.99.0", "1.24.2", StatusWarning},
		{"empty required", "1.26.0", "", StatusOK},
		{"v prefix installed", "v1.26.0", "1.24.2", StatusOK},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			gotStatus, gotNote := compareGTE(tt.installed, tt.required)
			if gotStatus != tt.wantStatus {
				t.Errorf("compareGTE(%q, %q) status = %q, want %q (note: %q)",
					tt.installed, tt.required, gotStatus, tt.wantStatus, gotNote)
			}
			if tt.required == "" && gotNote != "no version requirement" {
				t.Errorf("compareGTE(%q, %q) note = %q, want %q",
					tt.installed, tt.required, gotNote, "no version requirement")
			}
			if tt.required != "" && tt.wantStatus == StatusOK && !strings.Contains(gotNote, "≥") {
				t.Errorf("compareGTE OK note should contain '≥', got: %q", gotNote)
			}
			if tt.wantStatus == StatusWarning && !strings.Contains(gotNote, "too old") {
				t.Errorf("compareGTE warning note should contain 'too old', got: %q", gotNote)
			}
		})
	}
}

func TestCompareExact(t *testing.T) {
	tests := []struct {
		name       string
		installed  string
		required   string
		wantStatus ToolStatus
	}{
		{"exact match", "24.11.1", "24.11.1", StatusOK},
		{"match with v prefix installed", "v24.11.1", "24.11.1", StatusOK},
		{"match with v prefix required", "24.11.1", "v24.11.1", StatusOK},
		{"mismatch", "23.0.0", "24.11.1", StatusWarning},
		{"empty required", "24.11.1", "", StatusOK},
		{"both empty", "", "", StatusOK},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			gotStatus, gotNote := compareExact(tt.installed, tt.required)
			if gotStatus != tt.wantStatus {
				t.Errorf("compareExact(%q, %q) status = %q, want %q (note: %q)",
					tt.installed, tt.required, gotStatus, tt.wantStatus, gotNote)
			}
			if tt.required == "" && gotNote != "no version requirement" {
				t.Errorf("compareExact(%q, %q) note = %q, want %q",
					tt.installed, tt.required, gotNote, "no version requirement")
			}
		})
	}
}

func TestCompareMajor(t *testing.T) {
	tests := []struct {
		name       string
		installed  string
		required   string
		wantStatus ToolStatus
	}{
		{"major match exact", "25", "25", StatusOK},
		{"major match with patch", "25.0.1", "25", StatusOK},
		{"major mismatch", "21", "25", StatusWarning},
		{"major mismatch with patch", "21.0.1", "25", StatusWarning},
		{"empty required", "25", "", StatusOK},
		{"installed with v prefix", "v25", "25", StatusOK},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			gotStatus, gotNote := compareMajor(tt.installed, tt.required)
			if gotStatus != tt.wantStatus {
				t.Errorf("compareMajor(%q, %q) status = %q, want %q (note: %q)",
					tt.installed, tt.required, gotStatus, tt.wantStatus, gotNote)
			}
			if tt.required == "" && gotNote != "no version requirement" {
				t.Errorf("compareMajor(%q, %q) note = %q, want %q",
					tt.installed, tt.required, gotNote, "no version requirement")
			}
		})
	}
}

func TestReadNodeVersion(t *testing.T) {
	t.Run("valid package.json", func(t *testing.T) {
		tmpDir := t.TempDir()
		path := filepath.Join(tmpDir, "package.json")
		if err := os.WriteFile(path, []byte(`{"volta":{"node":"24.11.1","npm":"11.6.3"}}`), 0644); err != nil {
			t.Fatalf("failed to write test file: %v", err)
		}

		got, err := readNodeVersion(path)
		if err != nil {
			t.Fatalf("unexpected error: %v", err)
		}
		if got != "24.11.1" {
			t.Errorf("got %q, want %q", got, "24.11.1")
		}
	})

	t.Run("missing volta key returns empty", func(t *testing.T) {
		tmpDir := t.TempDir()
		path := filepath.Join(tmpDir, "package.json")
		if err := os.WriteFile(path, []byte(`{"name":"foo"}`), 0644); err != nil {
			t.Fatalf("failed to write test file: %v", err)
		}

		got, err := readNodeVersion(path)
		if err != nil {
			t.Fatalf("unexpected error: %v", err)
		}
		if got != "" {
			t.Errorf("got %q, want empty string", got)
		}
	})

	t.Run("malformed JSON returns error", func(t *testing.T) {
		path := filepath.Join(t.TempDir(), "package.json")
		if err := os.WriteFile(path, []byte(`{not valid json`), 0644); err != nil {
			t.Fatalf("failed to write test file: %v", err)
		}

		_, err := readNodeVersion(path)
		if err == nil {
			t.Fatal("expected error for malformed JSON")
		}
	})

	t.Run("missing file returns error", func(t *testing.T) {
		_, err := readNodeVersion("/nonexistent/path/package.json")
		if err == nil {
			t.Fatal("expected error for missing file")
		}
	})
}

func TestReadNpmVersion(t *testing.T) {
	t.Run("valid package.json", func(t *testing.T) {
		tmpDir := t.TempDir()
		path := filepath.Join(tmpDir, "package.json")
		if err := os.WriteFile(path, []byte(`{"volta":{"node":"24.11.1","npm":"11.6.3"}}`), 0644); err != nil {
			t.Fatalf("failed to write test file: %v", err)
		}

		got, err := readNpmVersion(path)
		if err != nil {
			t.Fatalf("unexpected error: %v", err)
		}
		if got != "11.6.3" {
			t.Errorf("got %q, want %q", got, "11.6.3")
		}
	})

	t.Run("malformed JSON returns error", func(t *testing.T) {
		path := filepath.Join(t.TempDir(), "package.json")
		if err := os.WriteFile(path, []byte(`{not valid`), 0644); err != nil {
			t.Fatalf("failed to write test file: %v", err)
		}

		_, err := readNpmVersion(path)
		if err == nil {
			t.Fatal("expected error for malformed JSON")
		}
	})
}

func TestReadJavaVersion(t *testing.T) {
	t.Run("valid pom.xml", func(t *testing.T) {
		tmpDir := t.TempDir()
		path := filepath.Join(tmpDir, "pom.xml")
		if err := os.WriteFile(path, []byte(`<project><properties><java.version>25</java.version></properties></project>`), 0644); err != nil {
			t.Fatalf("failed to write test file: %v", err)
		}

		got, err := readJavaVersion(path)
		if err != nil {
			t.Fatalf("unexpected error: %v", err)
		}
		if got != "25" {
			t.Errorf("got %q, want %q", got, "25")
		}
	})

	t.Run("malformed XML returns error", func(t *testing.T) {
		path := filepath.Join(t.TempDir(), "pom.xml")
		if err := os.WriteFile(path, []byte(`<project><not_closed>`), 0644); err != nil {
			t.Fatalf("failed to write test file: %v", err)
		}

		_, err := readJavaVersion(path)
		if err == nil {
			t.Fatal("expected error for malformed XML")
		}
	})

	t.Run("missing file returns error", func(t *testing.T) {
		_, err := readJavaVersion("/nonexistent/path/pom.xml")
		if err == nil {
			t.Fatal("expected error for missing file")
		}
	})
}

func TestReadGoVersion(t *testing.T) {
	t.Run("valid go.mod", func(t *testing.T) {
		tmpDir := t.TempDir()
		path := filepath.Join(tmpDir, "go.mod")
		if err := os.WriteFile(path, []byte("module foo\n\ngo 1.24.2\n"), 0644); err != nil {
			t.Fatalf("failed to write test file: %v", err)
		}

		got, err := readGoVersion(path)
		if err != nil {
			t.Fatalf("unexpected error: %v", err)
		}
		if got != "1.24.2" {
			t.Errorf("got %q, want %q", got, "1.24.2")
		}
	})

	t.Run("missing go directive returns error", func(t *testing.T) {
		tmpDir := t.TempDir()
		path := filepath.Join(tmpDir, "go.mod")
		if err := os.WriteFile(path, []byte("module foo\n\nrequire github.com/some/dep v1.0.0\n"), 0644); err != nil {
			t.Fatalf("failed to write test file: %v", err)
		}

		_, err := readGoVersion(path)
		if err == nil {
			t.Fatal("expected error when go directive missing")
		}
	})

	t.Run("missing file returns error", func(t *testing.T) {
		_, err := readGoVersion("/nonexistent/path/go.mod")
		if err == nil {
			t.Fatal("expected error for missing file")
		}
	})
}

func TestRunOneDef_Git_Found(t *testing.T) {
	runner := makeFakeRunner(map[string]fakeRunnerConfig{
		"git": {stdout: "git version 2.47.2\n", exitCode: 0},
	})
	def := findDef(t, buildToolDefs(t.TempDir()), "git")
	check := runOneDef(runner, def)
	if check.Status != StatusOK {
		t.Errorf("expected StatusOK, got %q", check.Status)
	}
	if check.InstalledVersion != "2.47.2" {
		t.Errorf("expected installed version %q, got %q", "2.47.2", check.InstalledVersion)
	}
	if check.Name != "git" {
		t.Errorf("expected name %q, got %q", "git", check.Name)
	}
}

func TestRunOneDef_Git_Missing(t *testing.T) {
	runner := makeFakeRunner(map[string]fakeRunnerConfig{})
	def := findDef(t, buildToolDefs(t.TempDir()), "git")
	check := runOneDef(runner, def)
	if check.Status != StatusMissing {
		t.Errorf("expected StatusMissing, got %q", check.Status)
	}
}

// findDef locates a toolDef by name; fails the test if not found.
func findDef(t *testing.T, defs []toolDef, name string) toolDef {
	t.Helper()
	for _, d := range defs {
		if d.name == name {
			return d
		}
	}
	t.Fatalf("no toolDef with name %q", name)
	return toolDef{}
}

func TestRunOneDef_Volta_Found(t *testing.T) {
	runner := makeFakeRunner(map[string]fakeRunnerConfig{
		"volta": {stdout: "2.0.2\n", exitCode: 0},
	})
	def := findDef(t, buildToolDefs(t.TempDir()), "volta")
	check := runOneDef(runner, def)
	if check.Status != StatusOK {
		t.Errorf("expected StatusOK, got %q", check.Status)
	}
	if check.InstalledVersion != "2.0.2" {
		t.Errorf("expected installed version %q, got %q", "2.0.2", check.InstalledVersion)
	}
	if check.Name != "volta" {
		t.Errorf("expected name %q, got %q", "volta", check.Name)
	}
}

func TestRunOneDef_Volta_Missing(t *testing.T) {
	runner := makeFakeRunner(map[string]fakeRunnerConfig{})
	def := findDef(t, buildToolDefs(t.TempDir()), "volta")
	check := runOneDef(runner, def)
	if check.Status != StatusMissing {
		t.Errorf("expected StatusMissing, got %q", check.Status)
	}
}

func TestRunOneDef_Node_Match(t *testing.T) {
	runner := makeFakeRunner(map[string]fakeRunnerConfig{
		"node": {stdout: "v24.11.1\n", exitCode: 0},
	})
	def := findDef(t, buildToolDefs(setupCheckAllRepo(t)), "node")
	check := runOneDef(runner, def)
	if check.Status != StatusOK {
		t.Errorf("expected StatusOK, got %q (note: %q)", check.Status, check.Note)
	}
	if check.InstalledVersion != "24.11.1" {
		t.Errorf("expected installed version %q, got %q", "24.11.1", check.InstalledVersion)
	}
}

func TestRunOneDef_Node_Mismatch(t *testing.T) {
	runner := makeFakeRunner(map[string]fakeRunnerConfig{
		"node": {stdout: "v20.0.0\n", exitCode: 0},
	})
	def := findDef(t, buildToolDefs(setupCheckAllRepo(t)), "node")
	check := runOneDef(runner, def)
	if check.Status != StatusWarning {
		t.Errorf("expected StatusWarning, got %q", check.Status)
	}
}

func TestRunOneDef_Node_Missing(t *testing.T) {
	runner := makeFakeRunner(map[string]fakeRunnerConfig{})
	def := findDef(t, buildToolDefs(setupCheckAllRepo(t)), "node")
	check := runOneDef(runner, def)
	if check.Status != StatusMissing {
		t.Errorf("expected StatusMissing, got %q", check.Status)
	}
}

func TestRunOneDef_Java_Match(t *testing.T) {
	runner := makeFakeRunner(map[string]fakeRunnerConfig{
		// java -version writes to stderr; useStderr=true in the def routes it correctly
		"java": {stderr: `openjdk version "25" 2025-09-16`, exitCode: 0},
	})
	def := findDef(t, buildToolDefs(setupCheckAllRepo(t)), "java")
	check := runOneDef(runner, def)
	if check.Status != StatusOK {
		t.Errorf("expected StatusOK, got %q (note: %q)", check.Status, check.Note)
	}
	if check.InstalledVersion != "25" {
		t.Errorf("expected installed version %q, got %q", "25", check.InstalledVersion)
	}
}

func TestRunOneDef_Java_Mismatch(t *testing.T) {
	runner := makeFakeRunner(map[string]fakeRunnerConfig{
		"java": {stderr: `openjdk version "21.0.1" 2023-10-17`, exitCode: 0},
	})
	def := findDef(t, buildToolDefs(setupCheckAllRepo(t)), "java")
	check := runOneDef(runner, def)
	if check.Status != StatusWarning {
		t.Errorf("expected StatusWarning, got %q", check.Status)
	}
}

func TestRunOneDef_Java_Missing(t *testing.T) {
	runner := makeFakeRunner(map[string]fakeRunnerConfig{})
	def := findDef(t, buildToolDefs(setupCheckAllRepo(t)), "java")
	check := runOneDef(runner, def)
	if check.Status != StatusMissing {
		t.Errorf("expected StatusMissing, got %q", check.Status)
	}
}

func TestRunOneDef_Maven_Found(t *testing.T) {
	runner := makeFakeRunner(map[string]fakeRunnerConfig{
		"mvn": {stdout: "Apache Maven 3.9.9 (8e8579a9e76f7d015ee5ec7bfcdc97d260186937)\nMaven home: /usr/share/maven\n", exitCode: 0},
	})
	def := findDef(t, buildToolDefs(t.TempDir()), "maven")
	check := runOneDef(runner, def)
	if check.Status != StatusOK {
		t.Errorf("expected StatusOK, got %q (note: %q)", check.Status, check.Note)
	}
	if check.InstalledVersion != "3.9.9" {
		t.Errorf("expected installed version %q, got %q", "3.9.9", check.InstalledVersion)
	}
}

func TestRunOneDef_Maven_Missing(t *testing.T) {
	runner := makeFakeRunner(map[string]fakeRunnerConfig{})
	def := findDef(t, buildToolDefs(t.TempDir()), "maven")
	check := runOneDef(runner, def)
	if check.Status != StatusMissing {
		t.Errorf("expected StatusMissing, got %q", check.Status)
	}
}

func TestRunOneDef_Go_Match(t *testing.T) {
	runner := makeFakeRunner(map[string]fakeRunnerConfig{
		"go": {stdout: "go version go1.24.2 linux/amd64\n", exitCode: 0},
	})
	def := findDef(t, buildToolDefs(setupCheckAllRepo(t)), "golang")
	check := runOneDef(runner, def)
	if check.Status != StatusOK {
		t.Errorf("expected StatusOK, got %q (note: %q)", check.Status, check.Note)
	}
	if check.InstalledVersion != "1.24.2" {
		t.Errorf("expected installed version %q, got %q", "1.24.2", check.InstalledVersion)
	}
	if !strings.Contains(check.Note, "≥") {
		t.Errorf("expected note to contain '≥', got %q", check.Note)
	}
}

func TestRunOneDef_Go_NewerVersion(t *testing.T) {
	// Go is backward compatible: newer installed version satisfies an older requirement
	runner := makeFakeRunner(map[string]fakeRunnerConfig{
		"go": {stdout: "go version go1.26.0 linux/amd64\n", exitCode: 0},
	})
	def := findDef(t, buildToolDefs(setupCheckAllRepo(t)), "golang")
	check := runOneDef(runner, def)
	if check.Status != StatusOK {
		t.Errorf("expected StatusOK for 1.26.0 >= 1.24.2, got %q (note: %q)", check.Status, check.Note)
	}
}

func TestRunOneDef_Go_Mismatch(t *testing.T) {
	runner := makeFakeRunner(map[string]fakeRunnerConfig{
		"go": {stdout: "go version go1.23.0 linux/amd64\n", exitCode: 0},
	})
	def := findDef(t, buildToolDefs(setupCheckAllRepo(t)), "golang")
	check := runOneDef(runner, def)
	if check.Status != StatusWarning {
		t.Errorf("expected StatusWarning for 1.23.0 < 1.24.2, got %q", check.Status)
	}
	if !strings.Contains(check.Note, "too old") {
		t.Errorf("expected note to contain 'too old', got %q", check.Note)
	}
}

func TestRunOneDef_Go_Missing(t *testing.T) {
	runner := makeFakeRunner(map[string]fakeRunnerConfig{})
	def := findDef(t, buildToolDefs(setupCheckAllRepo(t)), "golang")
	check := runOneDef(runner, def)
	if check.Status != StatusMissing {
		t.Errorf("expected StatusMissing, got %q", check.Status)
	}
}

// setupCheckAllRepo creates a minimal temp repo with config files for CheckAll tests.
func setupCheckAllRepo(t *testing.T) string {
	t.Helper()
	tmpDir := t.TempDir()

	for _, dir := range []string{
		"apps/organiclever-be-jasb",
		"apps/rhino-cli",
		"apps/a-demo-be-python-fastapi",
		"apps/a-demo-be-fsharp-giraffe",
		"apps/a-demo-fe-dart-flutterweb",
		"apps/a-demo-be-rust-axum",
	} {
		if err := os.MkdirAll(filepath.Join(tmpDir, dir), 0755); err != nil {
			t.Fatalf("failed to create dirs: %v", err)
		}
	}

	files := map[string]string{
		"package.json":                                  `{"volta":{"node":"24.11.1","npm":"11.6.3"}}`,
		"apps/organiclever-be-jasb/pom.xml":             `<project><properties><java.version>25</java.version></properties></project>`,
		"apps/rhino-cli/go.mod":                         "module foo\n\ngo 1.24.2\n",
		"apps/a-demo-be-python-fastapi/.python-version": "3.13\n",
		".tool-versions":                                "erlang 27.3\nelixir 1.19.5-otp-27\n",
		"apps/a-demo-be-fsharp-giraffe/global.json":     `{"sdk":{"version":"10.0.103","rollForward":"latestMinor"}}`,
		"apps/a-demo-be-rust-axum/Cargo.toml":           "[package]\nname = \"test\"\nrust-version = \"1.80\"\n",
		"apps/a-demo-fe-dart-flutterweb/pubspec.yaml":   "name: demo\n\nenvironment:\n  sdk: ^3.11.1\n  flutter: \">=3.41.0\"\n",
	}
	for relPath, content := range files {
		if err := os.WriteFile(filepath.Join(tmpDir, relPath), []byte(content), 0644); err != nil {
			t.Fatalf("failed to write %s: %v", relPath, err)
		}
	}

	return tmpDir
}

func TestCheckAll_WithFakeRunner(t *testing.T) {
	checkPlaywrightBrowsersFn = func() bool { return true }
	defer func() { checkPlaywrightBrowsersFn = checkPlaywrightBrowsers }()

	tmpDir := setupCheckAllRepo(t)

	runner := makeFakeRunner(map[string]fakeRunnerConfig{
		"git":     {stdout: "git version 2.47.2\n", exitCode: 0},
		"volta":   {stdout: "2.0.2\n", exitCode: 0},
		"node":    {stdout: "v24.11.1\n", exitCode: 0},
		"npm":     {stdout: "11.6.3\n", exitCode: 0},
		"java":    {stderr: `openjdk version "25" 2025-09-16`, exitCode: 0},
		"mvn":     {stdout: "Apache Maven 3.9.9 (abc)\nMaven home: /usr\n", exitCode: 0},
		"go":      {stdout: "go version go1.24.2 linux/amd64\n", exitCode: 0},
		"python3": {stdout: "Python 3.13.1\n", exitCode: 0},
		"rustc":   {stdout: "rustc 1.94.0 (4a4ef493e 2026-03-02)\n", exitCode: 0},
		"cargo":   {stdout: "cargo-llvm-cov 0.8.5\n", exitCode: 0},
		"elixir":  {stdout: "Erlang/OTP 27 [erts-15.2.3]\n\nElixir 1.19.5 (compiled with Erlang/OTP 27)\n", exitCode: 0},
		"erl":     {stdout: "27", exitCode: 0},
		"dotnet":  {stdout: "10.0.103\n", exitCode: 0},
		"clj":     {stdout: "Clojure CLI version 1.12.4.1582\n", exitCode: 0},
		"dart":    {stdout: "Dart SDK version: 3.11.3 (stable)\n", exitCode: 0},
		"flutter": {stdout: "Flutter 3.41.5 • channel stable\n", exitCode: 0},
		"docker":  {stdout: "Docker version 29.2.1, build a5c7197\n", exitCode: 0},
		"jq":      {stdout: "jq-1.8.1\n", exitCode: 0},
		"npx":     {stdout: "Version 1.58.2\n", exitCode: 0},
	})

	result, err := CheckAll(CheckOptions{RepoRoot: tmpDir, Runner: runner})
	if err != nil {
		t.Fatalf("CheckAll returned error: %v", err)
	}

	if result.OKCount != 19 {
		t.Errorf("expected OKCount == 19, got %d", result.OKCount)
	}
	if result.WarnCount != 0 {
		t.Errorf("expected WarnCount == 0, got %d", result.WarnCount)
	}
	if result.MissingCount != 0 {
		t.Errorf("expected MissingCount == 0, got %d", result.MissingCount)
	}
	if len(result.Checks) != 19 {
		t.Errorf("expected 19 checks, got %d", len(result.Checks))
	}
}

func TestCheckAll_WithMissingTools(t *testing.T) {
	tmpDir := setupCheckAllRepo(t)

	// Empty map — all tools will be "missing"
	runner := makeFakeRunner(map[string]fakeRunnerConfig{})

	result, err := CheckAll(CheckOptions{RepoRoot: tmpDir, Runner: runner})
	if err != nil {
		t.Fatalf("CheckAll returned error: %v", err)
	}

	if result.MissingCount != 19 {
		t.Errorf("expected MissingCount == 19, got %d", result.MissingCount)
	}
	if result.OKCount != 0 {
		t.Errorf("expected OKCount == 0, got %d", result.OKCount)
	}
}

func TestRealRunner_Success(t *testing.T) {
	stdout, stderr, exitCode, err := realRunner("echo", "hello")
	if err != nil {
		t.Fatalf("realRunner(echo) unexpected error: %v", err)
	}
	if exitCode != 0 {
		t.Errorf("expected exit code 0, got %d", exitCode)
	}
	if !strings.Contains(stdout, "hello") {
		t.Errorf("expected 'hello' in stdout, got: %q", stdout)
	}
	_ = stderr
}

func TestRealRunner_NonZeroExit(t *testing.T) {
	_, _, exitCode, err := realRunner("sh", "-c", "exit 1")
	if err != nil {
		t.Fatalf("realRunner should not return error for non-zero exit, got: %v", err)
	}
	if exitCode != 1 {
		t.Errorf("expected exit code 1, got %d", exitCode)
	}
}

func TestRealRunner_BinaryNotFound(t *testing.T) {
	_, _, _, err := realRunner("__binary_that_does_not_exist_xyz__")
	if err == nil {
		t.Error("expected error for missing binary, got nil")
	}
	if !strings.Contains(err.Error(), "binary not found in PATH") {
		t.Errorf("expected 'binary not found in PATH' in error, got: %v", err)
	}
}

func TestReadNpmVersion_MissingFile(t *testing.T) {
	_, err := readNpmVersion("/nonexistent/path/package.json")
	if err == nil {
		t.Fatal("expected error for missing package.json")
	}
}

func TestCompareGTE_Fallback(t *testing.T) {
	// When version parsing fails, compareGTE falls back to compareExact (line 187-189)
	// Use the same unparseable string on both sides so compareExact returns OK
	status, note := compareGTE("not-a-version", "not-a-version")
	// Both identical, exact comparison returns OK
	if status != StatusOK {
		t.Errorf("compareGTE fallback: expected OK for identical unparseable strings, got %q (note: %q)", status, note)
	}
}

func TestCompareGTE_FallbackMismatch(t *testing.T) {
	// Fallback to exact when one side is unparseable
	status, note := compareGTE("not-a-version", "1.0.0")
	// They don't match exactly, so Warning
	if status != StatusWarning {
		t.Errorf("compareGTE fallback: expected Warning for mismatch, got %q (note: %q)", status, note)
	}
}

func TestCheckAll_NilRunner_UsesRealRunner(t *testing.T) {
	// CheckAll with nil runner should use realRunner (line 251) — covers the nil check branch
	tmpDir := setupCheckAllRepo(t)
	result, err := CheckAll(CheckOptions{RepoRoot: tmpDir, Runner: nil})
	if err != nil {
		t.Fatalf("CheckAll() with nil runner error: %v", err)
	}
	// We can't assert exact status because it depends on the system tools,
	// but we can assert the result is non-nil and has checks.
	if result == nil {
		t.Fatal("expected non-nil result")
	}
	if len(result.Checks) != 19 {
		t.Errorf("expected 19 checks, got %d", len(result.Checks))
	}
}

func TestCheckAll_WithWarningStatus(t *testing.T) {
	checkPlaywrightBrowsersFn = func() bool { return true }
	defer func() { checkPlaywrightBrowsersFn = checkPlaywrightBrowsers }()

	tmpDir := setupCheckAllRepo(t)

	runner := makeFakeRunner(map[string]fakeRunnerConfig{
		"git":   {stdout: "git version 2.47.2\n", exitCode: 0},
		"volta": {stdout: "2.0.2\n", exitCode: 0},
		// node returns old version → Warning
		"node":    {stdout: "v20.0.0\n", exitCode: 0},
		"npm":     {stdout: "11.6.3\n", exitCode: 0},
		"java":    {stderr: `openjdk version "25" 2025-09-16`, exitCode: 0},
		"mvn":     {stdout: "Apache Maven 3.9.9 (abc)\nMaven home: /usr\n", exitCode: 0},
		"go":      {stdout: "go version go1.24.2 linux/amd64\n", exitCode: 0},
		"python3": {stdout: "Python 3.13.1\n", exitCode: 0},
		"rustc":   {stdout: "rustc 1.94.0 (abc)\n", exitCode: 0},
		"cargo":   {stdout: "cargo-llvm-cov 0.8.5\n", exitCode: 0},
		"elixir":  {stdout: "Elixir 1.19.5 (compiled with Erlang/OTP 27)\n", exitCode: 0},
		"erl":     {stdout: "27", exitCode: 0},
		"dotnet":  {stdout: "10.0.103\n", exitCode: 0},
		"clj":     {stdout: "Clojure CLI version 1.12.4.1582\n", exitCode: 0},
		"dart":    {stdout: "Dart SDK version: 3.11.3 (stable)\n", exitCode: 0},
		"flutter": {stdout: "Flutter 3.41.5\n", exitCode: 0},
		"docker":  {stdout: "Docker version 29.2.1, build abc\n", exitCode: 0},
		"jq":      {stdout: "jq-1.8.1\n", exitCode: 0},
		"npx":     {stdout: "Version 1.58.2\n", exitCode: 0},
	})

	result, err := CheckAll(CheckOptions{RepoRoot: tmpDir, Runner: runner})
	if err != nil {
		t.Fatalf("CheckAll() error: %v", err)
	}

	if result.WarnCount != 1 {
		t.Errorf("expected WarnCount == 1 (old node version), got %d", result.WarnCount)
	}
}

// --- Tests for new parser functions ---

func TestParsePythonVersion(t *testing.T) {
	tests := []struct{ input, want string }{
		{"Python 3.13.1\n", "3.13.1"},
		{"Python 3.10.5\n", "3.10.5"},
		{"", ""},
	}
	for _, tt := range tests {
		got := parsePythonVersion(tt.input)
		if got != tt.want {
			t.Errorf("parsePythonVersion(%q) = %q, want %q", tt.input, got, tt.want)
		}
	}
}

func TestParseRustVersion(t *testing.T) {
	tests := []struct{ input, want string }{
		{"rustc 1.94.0 (4a4ef493e 2026-03-02)\n", "1.94.0"},
		{"rustc 1.80.1 (3f5fd8dd4 2024-08-06)\n", "1.80.1"},
		{"", ""},
	}
	for _, tt := range tests {
		got := parseRustVersion(tt.input)
		if got != tt.want {
			t.Errorf("parseRustVersion(%q) = %q, want %q", tt.input, got, tt.want)
		}
	}
}

func TestParseCargoLlvmCov(t *testing.T) {
	tests := []struct{ input, want string }{
		{"cargo-llvm-cov 0.8.5\n", "0.8.5"},
		{"", ""},
	}
	for _, tt := range tests {
		got := parseCargoLlvmCov(tt.input)
		if got != tt.want {
			t.Errorf("parseCargoLlvmCov(%q) = %q, want %q", tt.input, got, tt.want)
		}
	}
}

func TestParseElixirVersion(t *testing.T) {
	tests := []struct{ input, want string }{
		{"Erlang/OTP 27 [erts-15.2.3]\n\nElixir 1.19.5 (compiled with Erlang/OTP 27)\n", "1.19.5"},
		{"Elixir 1.18.1 (compiled with Erlang/OTP 27)\n", "1.18.1"},
		{"", ""},
	}
	for _, tt := range tests {
		got := parseElixirVersion(tt.input)
		if got != tt.want {
			t.Errorf("parseElixirVersion(%q) = %q, want %q", tt.input, got, tt.want)
		}
	}
}

func TestParseErlangVersion(t *testing.T) {
	tests := []struct{ input, want string }{
		{"27", "27"},
		{"27\n", "27"},
		{"  27  ", "27"},
		{"", ""},
	}
	for _, tt := range tests {
		got := parseErlangVersion(tt.input)
		if got != tt.want {
			t.Errorf("parseErlangVersion(%q) = %q, want %q", tt.input, got, tt.want)
		}
	}
}

func TestParseDotnetVersion(t *testing.T) {
	tests := []struct{ input, want string }{
		{"10.0.103\n", "10.0.103"},
		{"8.0.401\n", "8.0.401"},
		{"", ""},
	}
	for _, tt := range tests {
		got := parseDotnetVersion(tt.input)
		if got != tt.want {
			t.Errorf("parseDotnetVersion(%q) = %q, want %q", tt.input, got, tt.want)
		}
	}
}

func TestParseClojureVersion(t *testing.T) {
	tests := []struct{ input, want string }{
		{"Clojure CLI version 1.12.4.1582\n", "1.12.4.1582"},
		{"", ""},
	}
	for _, tt := range tests {
		got := parseClojureVersion(tt.input)
		if got != tt.want {
			t.Errorf("parseClojureVersion(%q) = %q, want %q", tt.input, got, tt.want)
		}
	}
}

func TestParseDartVersion(t *testing.T) {
	tests := []struct{ input, want string }{
		{"Dart SDK version: 3.11.3 (stable) (Tue Mar 17 01:06:16 2026 -0700) on \"macos_arm64\"\n", "3.11.3"},
		{"Dart SDK version: 3.7.0 (stable)\n", "3.7.0"},
		{"", ""},
	}
	for _, tt := range tests {
		got := parseDartVersion(tt.input)
		if got != tt.want {
			t.Errorf("parseDartVersion(%q) = %q, want %q", tt.input, got, tt.want)
		}
	}
}

func TestParseFlutterVersion(t *testing.T) {
	tests := []struct{ input, want string }{
		{"Flutter 3.41.5 • channel stable\n", "3.41.5"},
		{"Flutter 3.41.5 â\u0080¢ channel stable\n", "3.41.5"},
		{"", ""},
	}
	for _, tt := range tests {
		got := parseFlutterVersion(tt.input)
		if got != tt.want {
			t.Errorf("parseFlutterVersion(%q) = %q, want %q", tt.input, got, tt.want)
		}
	}
}

func TestParseDockerVersion(t *testing.T) {
	tests := []struct{ input, want string }{
		{"Docker version 29.2.1, build a5c7197\n", "29.2.1"},
		{"Docker version 24.0.7, build afdd53b\n", "24.0.7"},
		{"", ""},
	}
	for _, tt := range tests {
		got := parseDockerVersion(tt.input)
		if got != tt.want {
			t.Errorf("parseDockerVersion(%q) = %q, want %q", tt.input, got, tt.want)
		}
	}
}

func TestParseJqVersion(t *testing.T) {
	tests := []struct{ input, want string }{
		{"jq-1.8.1\n", "1.8.1"},
		{"jq-1.7\n", "1.7"},
		{"", ""},
	}
	for _, tt := range tests {
		got := parseJqVersion(tt.input)
		if got != tt.want {
			t.Errorf("parseJqVersion(%q) = %q, want %q", tt.input, got, tt.want)
		}
	}
}

// --- Tests for new reader functions ---

func TestReadPythonVersion(t *testing.T) {
	t.Run("valid", func(t *testing.T) {
		path := filepath.Join(t.TempDir(), ".python-version")
		if err := os.WriteFile(path, []byte("3.13\n"), 0644); err != nil {
			t.Fatalf("write: %v", err)
		}
		got, err := readPythonVersion(path)
		if err != nil {
			t.Fatalf("unexpected error: %v", err)
		}
		if got != "3.13" {
			t.Errorf("got %q, want %q", got, "3.13")
		}
	})
	t.Run("missing file", func(t *testing.T) {
		_, err := readPythonVersion("/nonexistent")
		if err == nil {
			t.Fatal("expected error")
		}
	})
}

func TestReadToolVersionsEntry(t *testing.T) {
	t.Run("valid erlang", func(t *testing.T) {
		path := filepath.Join(t.TempDir(), ".tool-versions")
		if err := os.WriteFile(path, []byte("erlang 27.3\nelixir 1.19.5-otp-27\n"), 0644); err != nil {
			t.Fatalf("write: %v", err)
		}
		got, err := readToolVersionsEntry(path, "erlang")
		if err != nil {
			t.Fatalf("unexpected error: %v", err)
		}
		if got != "27.3" {
			t.Errorf("got %q, want %q", got, "27.3")
		}
	})
	t.Run("valid elixir", func(t *testing.T) {
		path := filepath.Join(t.TempDir(), ".tool-versions")
		if err := os.WriteFile(path, []byte("erlang 27.3\nelixir 1.19.5-otp-27\n"), 0644); err != nil {
			t.Fatalf("write: %v", err)
		}
		got, err := readToolVersionsEntry(path, "elixir")
		if err != nil {
			t.Fatalf("unexpected error: %v", err)
		}
		if got != "1.19.5-otp-27" {
			t.Errorf("got %q, want %q", got, "1.19.5-otp-27")
		}
	})
	t.Run("tool not found", func(t *testing.T) {
		path := filepath.Join(t.TempDir(), ".tool-versions")
		if err := os.WriteFile(path, []byte("erlang 27.3\n"), 0644); err != nil {
			t.Fatalf("write: %v", err)
		}
		_, err := readToolVersionsEntry(path, "ruby")
		if err == nil {
			t.Fatal("expected error for missing tool")
		}
	})
	t.Run("missing file", func(t *testing.T) {
		_, err := readToolVersionsEntry("/nonexistent", "erlang")
		if err == nil {
			t.Fatal("expected error")
		}
	})
}

func TestReadDotnetVersion(t *testing.T) {
	t.Run("valid", func(t *testing.T) {
		path := filepath.Join(t.TempDir(), "global.json")
		if err := os.WriteFile(path, []byte(`{"sdk":{"version":"10.0.103"}}`), 0644); err != nil {
			t.Fatalf("write: %v", err)
		}
		got, err := readDotnetVersion(path)
		if err != nil {
			t.Fatalf("unexpected error: %v", err)
		}
		if got != "10.0.103" {
			t.Errorf("got %q, want %q", got, "10.0.103")
		}
	})
	t.Run("missing file", func(t *testing.T) {
		_, err := readDotnetVersion("/nonexistent")
		if err == nil {
			t.Fatal("expected error")
		}
	})
}

func TestReadDartSDKVersion(t *testing.T) {
	t.Run("with caret", func(t *testing.T) {
		path := filepath.Join(t.TempDir(), "pubspec.yaml")
		if err := os.WriteFile(path, []byte("name: demo\n\nenvironment:\n  sdk: ^3.11.1\n"), 0644); err != nil {
			t.Fatalf("write: %v", err)
		}
		got, err := readDartSDKVersion(path)
		if err != nil {
			t.Fatalf("unexpected error: %v", err)
		}
		if got != "3.11.1" {
			t.Errorf("got %q, want %q", got, "3.11.1")
		}
	})
	t.Run("with >=", func(t *testing.T) {
		path := filepath.Join(t.TempDir(), "pubspec.yaml")
		if err := os.WriteFile(path, []byte("name: demo\n\nenvironment:\n  sdk: >=3.0.0\n"), 0644); err != nil {
			t.Fatalf("write: %v", err)
		}
		got, err := readDartSDKVersion(path)
		if err != nil {
			t.Fatalf("unexpected error: %v", err)
		}
		if got != "3.0.0" {
			t.Errorf("got %q, want %q", got, "3.0.0")
		}
	})
	t.Run("no environment block", func(t *testing.T) {
		path := filepath.Join(t.TempDir(), "pubspec.yaml")
		if err := os.WriteFile(path, []byte("name: demo\n"), 0644); err != nil {
			t.Fatalf("write: %v", err)
		}
		_, err := readDartSDKVersion(path)
		if err == nil {
			t.Fatal("expected error for missing environment.sdk")
		}
	})
	t.Run("missing file", func(t *testing.T) {
		_, err := readDartSDKVersion("/nonexistent")
		if err == nil {
			t.Fatal("expected error")
		}
	})
}

func TestCompareMajorGTE(t *testing.T) {
	tests := []struct {
		name       string
		installed  string
		required   string
		wantStatus ToolStatus
	}{
		{"major match exact", "27", "27.3", StatusOK},
		{"major newer", "28", "27.3", StatusOK},
		{"major older", "26", "27.3", StatusWarning},
		{"same with dots", "10.0.103", "10.0.103", StatusOK},
		{"major same dotted", "10.0.401", "10.0.103", StatusOK},
		{"major older dotted", "8.0.401", "10.0.103", StatusWarning},
		{"empty required", "27", "", StatusOK},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			gotStatus, gotNote := compareMajorGTE(tt.installed, tt.required)
			if gotStatus != tt.wantStatus {
				t.Errorf("compareMajorGTE(%q, %q) status = %q, want %q (note: %q)",
					tt.installed, tt.required, gotStatus, tt.wantStatus, gotNote)
			}
		})
	}
}

// --- RunOneDef tests for new tools ---

func TestRunOneDef_Python_Found(t *testing.T) {
	runner := makeFakeRunner(map[string]fakeRunnerConfig{
		"python3": {stdout: "Python 3.13.1\n", exitCode: 0},
	})
	def := findDef(t, buildToolDefs(setupCheckAllRepo(t)), "python")
	check := runOneDef(runner, def)
	if check.Status != StatusOK {
		t.Errorf("expected StatusOK, got %q (note: %q)", check.Status, check.Note)
	}
	if check.InstalledVersion != "3.13.1" {
		t.Errorf("expected version %q, got %q", "3.13.1", check.InstalledVersion)
	}
}

func TestRunOneDef_Python_Missing(t *testing.T) {
	runner := makeFakeRunner(map[string]fakeRunnerConfig{})
	def := findDef(t, buildToolDefs(setupCheckAllRepo(t)), "python")
	check := runOneDef(runner, def)
	if check.Status != StatusMissing {
		t.Errorf("expected StatusMissing, got %q", check.Status)
	}
}

func TestRunOneDef_Rust_Found(t *testing.T) {
	runner := makeFakeRunner(map[string]fakeRunnerConfig{
		"rustc": {stdout: "rustc 1.94.0 (4a4ef493e 2026-03-02)\n", exitCode: 0},
	})
	def := findDef(t, buildToolDefs(t.TempDir()), "rust")
	check := runOneDef(runner, def)
	if check.Status != StatusOK {
		t.Errorf("expected StatusOK, got %q", check.Status)
	}
	if check.InstalledVersion != "1.94.0" {
		t.Errorf("expected version %q, got %q", "1.94.0", check.InstalledVersion)
	}
}

func TestRunOneDef_Elixir_Found(t *testing.T) {
	runner := makeFakeRunner(map[string]fakeRunnerConfig{
		"elixir": {stdout: "Erlang/OTP 27 [erts-15.2.3]\n\nElixir 1.19.5 (compiled with Erlang/OTP 27)\n", exitCode: 0},
	})
	def := findDef(t, buildToolDefs(setupCheckAllRepo(t)), "elixir")
	check := runOneDef(runner, def)
	if check.Status != StatusOK {
		t.Errorf("expected StatusOK, got %q (note: %q)", check.Status, check.Note)
	}
	if check.InstalledVersion != "1.19.5" {
		t.Errorf("expected version %q, got %q", "1.19.5", check.InstalledVersion)
	}
}

func TestRunOneDef_Erlang_Found(t *testing.T) {
	runner := makeFakeRunner(map[string]fakeRunnerConfig{
		"erl": {stdout: "27", exitCode: 0},
	})
	def := findDef(t, buildToolDefs(setupCheckAllRepo(t)), "erlang")
	check := runOneDef(runner, def)
	if check.Status != StatusOK {
		t.Errorf("expected StatusOK, got %q (note: %q)", check.Status, check.Note)
	}
}

func TestRunOneDef_Dotnet_Found(t *testing.T) {
	runner := makeFakeRunner(map[string]fakeRunnerConfig{
		"dotnet": {stdout: "10.0.103\n", exitCode: 0},
	})
	def := findDef(t, buildToolDefs(setupCheckAllRepo(t)), "dotnet")
	check := runOneDef(runner, def)
	if check.Status != StatusOK {
		t.Errorf("expected StatusOK, got %q (note: %q)", check.Status, check.Note)
	}
}

func TestRunOneDef_Dart_Found(t *testing.T) {
	runner := makeFakeRunner(map[string]fakeRunnerConfig{
		"dart": {stdout: "Dart SDK version: 3.11.3 (stable)\n", exitCode: 0},
	})
	def := findDef(t, buildToolDefs(setupCheckAllRepo(t)), "dart")
	check := runOneDef(runner, def)
	if check.Status != StatusOK {
		t.Errorf("expected StatusOK, got %q (note: %q)", check.Status, check.Note)
	}
	if check.InstalledVersion != "3.11.3" {
		t.Errorf("expected version %q, got %q", "3.11.3", check.InstalledVersion)
	}
}

func TestRunOneDef_Docker_Found(t *testing.T) {
	runner := makeFakeRunner(map[string]fakeRunnerConfig{
		"docker": {stdout: "Docker version 29.2.1, build a5c7197\n", exitCode: 0},
	})
	def := findDef(t, buildToolDefs(t.TempDir()), "docker")
	check := runOneDef(runner, def)
	if check.Status != StatusOK {
		t.Errorf("expected StatusOK, got %q", check.Status)
	}
	if check.InstalledVersion != "29.2.1" {
		t.Errorf("expected version %q, got %q", "29.2.1", check.InstalledVersion)
	}
}

func TestParsePlaywrightVersion(t *testing.T) {
	tests := []struct{ input, want string }{
		{"Version 1.58.2\n", "1.58.2"},
		{"Version 1.50.0\n", "1.50.0"},
		{"", ""},
	}
	for _, tt := range tests {
		got := parsePlaywrightVersion(tt.input)
		if got != tt.want {
			t.Errorf("parsePlaywrightVersion(%q) = %q, want %q", tt.input, got, tt.want)
		}
	}
}

func TestRunOneDef_Playwright_Found(t *testing.T) {
	checkPlaywrightBrowsersFn = func() bool { return true }
	defer func() { checkPlaywrightBrowsersFn = checkPlaywrightBrowsers }()

	runner := makeFakeRunner(map[string]fakeRunnerConfig{
		"npx": {stdout: "Version 1.58.2\n", exitCode: 0},
	})
	def := findDef(t, buildToolDefs(t.TempDir()), "playwright")
	check := runOneDef(runner, def)
	if check.Status != StatusOK {
		t.Errorf("expected StatusOK, got %q (note: %q)", check.Status, check.Note)
	}
	if check.InstalledVersion != "1.58.2" {
		t.Errorf("expected version %q, got %q", "1.58.2", check.InstalledVersion)
	}
}

func TestRunOneDef_Playwright_Missing(t *testing.T) {
	runner := makeFakeRunner(map[string]fakeRunnerConfig{})
	def := findDef(t, buildToolDefs(t.TempDir()), "playwright")
	check := runOneDef(runner, def)
	if check.Status != StatusMissing {
		t.Errorf("expected StatusMissing, got %q", check.Status)
	}
}

func TestRunOneDef_Jq_Found(t *testing.T) {
	runner := makeFakeRunner(map[string]fakeRunnerConfig{
		"jq": {stdout: "jq-1.8.1\n", exitCode: 0},
	})
	def := findDef(t, buildToolDefs(t.TempDir()), "jq")
	check := runOneDef(runner, def)
	if check.Status != StatusOK {
		t.Errorf("expected StatusOK, got %q", check.Status)
	}
	if check.InstalledVersion != "1.8.1" {
		t.Errorf("expected version %q, got %q", "1.8.1", check.InstalledVersion)
	}
}

func TestReadRustVersion(t *testing.T) {
	t.Run("valid Cargo.toml with rust-version", func(t *testing.T) {
		path := filepath.Join(t.TempDir(), "Cargo.toml")
		content := "[package]\nname = \"test\"\nversion = \"0.1.0\"\nedition = \"2021\"\nrust-version = \"1.80\"\n"
		if err := os.WriteFile(path, []byte(content), 0644); err != nil {
			t.Fatalf("write: %v", err)
		}
		got, err := readRustVersion(path)
		if err != nil {
			t.Fatalf("unexpected error: %v", err)
		}
		if got != "1.80" {
			t.Errorf("got %q, want %q", got, "1.80")
		}
	})
	t.Run("missing rust-version", func(t *testing.T) {
		path := filepath.Join(t.TempDir(), "Cargo.toml")
		content := "[package]\nname = \"test\"\nversion = \"0.1.0\"\nedition = \"2021\"\n"
		if err := os.WriteFile(path, []byte(content), 0644); err != nil {
			t.Fatalf("write: %v", err)
		}
		got, err := readRustVersion(path)
		if err != nil {
			t.Fatalf("unexpected error: %v", err)
		}
		if got != "" {
			t.Errorf("got %q, want empty string", got)
		}
	})
	t.Run("missing file", func(t *testing.T) {
		_, err := readRustVersion("/nonexistent")
		if err == nil {
			t.Fatal("expected error")
		}
	})
}

func TestReadFlutterVersion(t *testing.T) {
	t.Run("valid pubspec with flutter constraint", func(t *testing.T) {
		path := filepath.Join(t.TempDir(), "pubspec.yaml")
		content := "name: test\n\nenvironment:\n  sdk: ^3.11.1\n  flutter: \">=3.41.0\"\n"
		if err := os.WriteFile(path, []byte(content), 0644); err != nil {
			t.Fatalf("write: %v", err)
		}
		got, err := readFlutterVersion(path)
		if err != nil {
			t.Fatalf("unexpected error: %v", err)
		}
		if got != "3.41.0" {
			t.Errorf("got %q, want %q", got, "3.41.0")
		}
	})
	t.Run("no flutter constraint", func(t *testing.T) {
		path := filepath.Join(t.TempDir(), "pubspec.yaml")
		content := "name: test\n\nenvironment:\n  sdk: ^3.11.1\n"
		if err := os.WriteFile(path, []byte(content), 0644); err != nil {
			t.Fatalf("write: %v", err)
		}
		got, err := readFlutterVersion(path)
		if err != nil {
			t.Fatalf("unexpected error: %v", err)
		}
		if got != "" {
			t.Errorf("got %q, want empty string", got)
		}
	})
	t.Run("missing file", func(t *testing.T) {
		_, err := readFlutterVersion("/nonexistent")
		if err == nil {
			t.Fatal("expected error")
		}
	})
}
