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

func TestParseMavenVersion(t *testing.T) {
	tests := []struct {
		name   string
		stdout string
		want   string
	}{
		{
			name:   "standard maven output",
			stdout: "Apache Maven 3.9.9 (8e8579a9e76f7d015ee5ec7bfcdc97d260186937)\nMaven home: /usr/share/maven",
			want:   "3.9.9",
		},
		{
			name:   "leading whitespace",
			stdout: "  Apache Maven 3.8.6\n",
			want:   "3.8.6",
		},
		{
			name:   "empty stdout",
			stdout: "",
			want:   "",
		},
		{
			name:   "no maven line",
			stdout: "some other output\nno version here",
			want:   "",
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			got := parseMavenVersion(tt.stdout)
			if got != tt.want {
				t.Errorf("parseMavenVersion(%q) = %q, want %q", tt.stdout, got, tt.want)
			}
		})
	}
}

func TestParseGoVersion(t *testing.T) {
	tests := []struct {
		name   string
		stdout string
		want   string
	}{
		{
			name:   "standard go version output linux",
			stdout: "go version go1.24.2 linux/amd64",
			want:   "1.24.2",
		},
		{
			name:   "darwin output",
			stdout: "go version go1.23.0 darwin/arm64",
			want:   "1.23.0",
		},
		{
			name:   "windows output",
			stdout: "go version go1.22.1 windows/amd64",
			want:   "1.22.1",
		},
		{
			name:   "empty stdout",
			stdout: "",
			want:   "",
		},
		{
			name:   "no go version line",
			stdout: "some other output",
			want:   "",
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			got := parseGoVersion(tt.stdout)
			if got != tt.want {
				t.Errorf("parseGoVersion(%q) = %q, want %q", tt.stdout, got, tt.want)
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
		os.WriteFile(path, []byte(`{"volta":{"node":"24.11.1","npm":"11.6.3"}}`), 0644)

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
		os.WriteFile(path, []byte(`{"name":"foo"}`), 0644)

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
		os.WriteFile(path, []byte(`{not valid json`), 0644)

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
		os.WriteFile(path, []byte(`{"volta":{"node":"24.11.1","npm":"11.6.3"}}`), 0644)

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
		os.WriteFile(path, []byte(`{not valid`), 0644)

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
		os.WriteFile(path, []byte(`<project><properties><java.version>25</java.version></properties></project>`), 0644)

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
		os.WriteFile(path, []byte(`<project><not_closed>`), 0644)

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
		os.WriteFile(path, []byte("module foo\n\ngo 1.24.2\n"), 0644)

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
		os.WriteFile(path, []byte("module foo\n\nrequire github.com/some/dep v1.0.0\n"), 0644)

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

func TestCheckVolta_Found(t *testing.T) {
	runner := makeFakeRunner(map[string]fakeRunnerConfig{
		"volta": {stdout: "2.0.2\n", exitCode: 0},
	})
	check := checkVolta(runner)
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

func TestCheckVolta_Missing(t *testing.T) {
	runner := makeFakeRunner(map[string]fakeRunnerConfig{})
	check := checkVolta(runner)
	if check.Status != StatusMissing {
		t.Errorf("expected StatusMissing, got %q", check.Status)
	}
}

func TestCheckNode_Match(t *testing.T) {
	runner := makeFakeRunner(map[string]fakeRunnerConfig{
		"node": {stdout: "v24.11.1\n", exitCode: 0},
	})
	check := checkNode(runner, "24.11.1")
	if check.Status != StatusOK {
		t.Errorf("expected StatusOK, got %q (note: %q)", check.Status, check.Note)
	}
	if check.InstalledVersion != "24.11.1" {
		t.Errorf("expected installed version %q, got %q", "24.11.1", check.InstalledVersion)
	}
}

func TestCheckNode_Mismatch(t *testing.T) {
	runner := makeFakeRunner(map[string]fakeRunnerConfig{
		"node": {stdout: "v20.0.0\n", exitCode: 0},
	})
	check := checkNode(runner, "24.11.1")
	if check.Status != StatusWarning {
		t.Errorf("expected StatusWarning, got %q", check.Status)
	}
}

func TestCheckNode_Missing(t *testing.T) {
	runner := makeFakeRunner(map[string]fakeRunnerConfig{})
	check := checkNode(runner, "24.11.1")
	if check.Status != StatusMissing {
		t.Errorf("expected StatusMissing, got %q", check.Status)
	}
}

func TestCheckJava_Match(t *testing.T) {
	runner := makeFakeRunner(map[string]fakeRunnerConfig{
		// java -version writes to stderr
		"java": {stderr: `openjdk version "25" 2025-09-16`, exitCode: 0},
	})
	check := checkJava(runner, "25")
	if check.Status != StatusOK {
		t.Errorf("expected StatusOK, got %q (note: %q)", check.Status, check.Note)
	}
	if check.InstalledVersion != "25" {
		t.Errorf("expected installed version %q, got %q", "25", check.InstalledVersion)
	}
}

func TestCheckJava_Mismatch(t *testing.T) {
	runner := makeFakeRunner(map[string]fakeRunnerConfig{
		"java": {stderr: `openjdk version "21.0.1" 2023-10-17`, exitCode: 0},
	})
	check := checkJava(runner, "25")
	if check.Status != StatusWarning {
		t.Errorf("expected StatusWarning, got %q", check.Status)
	}
}

func TestCheckJava_Missing(t *testing.T) {
	runner := makeFakeRunner(map[string]fakeRunnerConfig{})
	check := checkJava(runner, "25")
	if check.Status != StatusMissing {
		t.Errorf("expected StatusMissing, got %q", check.Status)
	}
}

func TestCheckMaven_Found(t *testing.T) {
	runner := makeFakeRunner(map[string]fakeRunnerConfig{
		"mvn": {stdout: "Apache Maven 3.9.9 (8e8579a9e76f7d015ee5ec7bfcdc97d260186937)\nMaven home: /usr/share/maven\n", exitCode: 0},
	})
	check := checkMaven(runner)
	if check.Status != StatusOK {
		t.Errorf("expected StatusOK, got %q (note: %q)", check.Status, check.Note)
	}
	if check.InstalledVersion != "3.9.9" {
		t.Errorf("expected installed version %q, got %q", "3.9.9", check.InstalledVersion)
	}
}

func TestCheckMaven_Missing(t *testing.T) {
	runner := makeFakeRunner(map[string]fakeRunnerConfig{})
	check := checkMaven(runner)
	if check.Status != StatusMissing {
		t.Errorf("expected StatusMissing, got %q", check.Status)
	}
}

func TestCheckGo_Match(t *testing.T) {
	runner := makeFakeRunner(map[string]fakeRunnerConfig{
		"go": {stdout: "go version go1.24.2 linux/amd64\n", exitCode: 0},
	})
	check := checkGo(runner, "1.24.2")
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

func TestCheckGo_NewerVersion(t *testing.T) {
	// Go is backward compatible: newer installed version satisfies an older requirement
	runner := makeFakeRunner(map[string]fakeRunnerConfig{
		"go": {stdout: "go version go1.26.0 linux/amd64\n", exitCode: 0},
	})
	check := checkGo(runner, "1.24.2")
	if check.Status != StatusOK {
		t.Errorf("expected StatusOK for 1.26.0 >= 1.24.2, got %q (note: %q)", check.Status, check.Note)
	}
}

func TestCheckGo_Mismatch(t *testing.T) {
	// Older version → warning (version too old)
	runner := makeFakeRunner(map[string]fakeRunnerConfig{
		"go": {stdout: "go version go1.23.0 linux/amd64\n", exitCode: 0},
	})
	check := checkGo(runner, "1.24.2")
	if check.Status != StatusWarning {
		t.Errorf("expected StatusWarning for 1.23.0 < 1.24.2, got %q", check.Status)
	}
	if !strings.Contains(check.Note, "too old") {
		t.Errorf("expected note to contain 'too old', got %q", check.Note)
	}
}

func TestCheckGo_Missing(t *testing.T) {
	runner := makeFakeRunner(map[string]fakeRunnerConfig{})
	check := checkGo(runner, "1.24.2")
	if check.Status != StatusMissing {
		t.Errorf("expected StatusMissing, got %q", check.Status)
	}
}

// setupCheckAllRepo creates a minimal temp repo with config files for CheckAll tests.
func setupCheckAllRepo(t *testing.T) string {
	t.Helper()
	tmpDir := t.TempDir()

	if err := os.MkdirAll(filepath.Join(tmpDir, "apps", "organiclever-be"), 0755); err != nil {
		t.Fatalf("failed to create dirs: %v", err)
	}
	if err := os.MkdirAll(filepath.Join(tmpDir, "apps", "rhino-cli"), 0755); err != nil {
		t.Fatalf("failed to create dirs: %v", err)
	}

	os.WriteFile(filepath.Join(tmpDir, "package.json"),
		[]byte(`{"volta":{"node":"24.11.1","npm":"11.6.3"}}`), 0644)
	os.WriteFile(filepath.Join(tmpDir, "apps", "organiclever-be", "pom.xml"),
		[]byte(`<project><properties><java.version>25</java.version></properties></project>`), 0644)
	os.WriteFile(filepath.Join(tmpDir, "apps", "rhino-cli", "go.mod"),
		[]byte("module foo\n\ngo 1.24.2\n"), 0644)

	return tmpDir
}

func TestCheckAll_WithFakeRunner(t *testing.T) {
	tmpDir := setupCheckAllRepo(t)

	runner := makeFakeRunner(map[string]fakeRunnerConfig{
		"volta": {stdout: "2.0.2\n", exitCode: 0},
		"node":  {stdout: "v24.11.1\n", exitCode: 0},
		"npm":   {stdout: "11.6.3\n", exitCode: 0},
		"java":  {stderr: `openjdk version "25" 2025-09-16`, exitCode: 0},
		"mvn":   {stdout: "Apache Maven 3.9.9 (abc)\nMaven home: /usr\n", exitCode: 0},
		"go":    {stdout: "go version go1.24.2 linux/amd64\n", exitCode: 0},
	})

	result, err := CheckAll(CheckOptions{RepoRoot: tmpDir, Runner: runner})
	if err != nil {
		t.Fatalf("CheckAll returned error: %v", err)
	}

	if result.OKCount != 6 {
		t.Errorf("expected OKCount == 6, got %d", result.OKCount)
	}
	if result.WarnCount != 0 {
		t.Errorf("expected WarnCount == 0, got %d", result.WarnCount)
	}
	if result.MissingCount != 0 {
		t.Errorf("expected MissingCount == 0, got %d", result.MissingCount)
	}
	if len(result.Checks) != 6 {
		t.Errorf("expected 6 checks, got %d", len(result.Checks))
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

	if result.MissingCount != 6 {
		t.Errorf("expected MissingCount == 6, got %d", result.MissingCount)
	}
	if result.OKCount != 0 {
		t.Errorf("expected OKCount == 0, got %d", result.OKCount)
	}
}
