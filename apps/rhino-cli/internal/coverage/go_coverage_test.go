package coverage

import (
	"os"
	"path/filepath"
	"testing"
)

func writeTempCoverOut(t *testing.T, dir, content string) string {
	t.Helper()
	path := filepath.Join(dir, "cover.out")
	if err := os.WriteFile(path, []byte(content), 0644); err != nil {
		t.Fatal(err)
	}
	return path
}

func TestIsGoCodeLine(t *testing.T) {
	cases := []struct {
		input string
		want  bool
	}{
		{"", false},
		{"   ", false},
		{"// comment", false},
		{"	// indented comment", false},
		{"{", false},
		{"}", false},
		{"  {  ", false},
		{"  }  ", false},
		{"x := 1", true},
		{"return x", true},
		{"func foo() {", true}, // has { but is not brace-only
		{"(", true},            // ( is NOT excluded
		{")", true},            // ) is NOT excluded
	}
	for _, tc := range cases {
		got := isGoCodeLine(tc.input)
		if got != tc.want {
			t.Errorf("isGoCodeLine(%q) = %v, want %v", tc.input, got, tc.want)
		}
	}
}

func TestGetModuleNameFrom_NoGoMod(t *testing.T) {
	tmpDir := t.TempDir()
	name := getModuleNameFrom(tmpDir)
	if name != "" {
		t.Errorf("expected empty module name when no go.mod, got %q", name)
	}
}

func TestGetModuleNameFrom_WithGoMod(t *testing.T) {
	tmpDir := t.TempDir()
	content := "module github.com/example/myapp\n\ngo 1.21\n"
	if err := os.WriteFile(filepath.Join(tmpDir, "go.mod"), []byte(content), 0644); err != nil {
		t.Fatal(err)
	}

	name := getModuleNameFrom(tmpDir)
	if name != "github.com/example/myapp" {
		t.Errorf("expected github.com/example/myapp, got %q", name)
	}
}

func TestGetSourceLinesFrom_MissingFile(t *testing.T) {
	tmpDir := t.TempDir()
	lines := getSourceLinesFrom(tmpDir, "nonexistent.go")
	if lines != nil {
		t.Errorf("expected nil for missing file, got %v", lines)
	}
}

func TestGetSourceLinesFrom_ValidFile(t *testing.T) {
	tmpDir := t.TempDir()
	content := "line1\nline2\nline3\n"
	if err := os.WriteFile(filepath.Join(tmpDir, "source.go"), []byte(content), 0644); err != nil {
		t.Fatal(err)
	}

	lines := getSourceLinesFrom(tmpDir, "source.go")
	if lines == nil {
		t.Fatal("expected non-nil map")
	}
	if lines[1] != "line1" || lines[2] != "line2" || lines[3] != "line3" {
		t.Errorf("unexpected lines: %v", lines)
	}
}

func TestParseCoverOut_FileNotFound(t *testing.T) {
	_, err := parseCoverOut("/nonexistent/cover.out")
	if err == nil {
		t.Error("expected error for missing file")
	}
}

func TestParseCoverOut_Valid(t *testing.T) {
	tmpDir := t.TempDir()
	content := `mode: set
github.com/example/myapp/pkg/foo:10.1,20.5 1 1
github.com/example/myapp/pkg/foo:22.1,30.5 1 0
github.com/example/myapp/pkg/foo:32.1,40.5 1 1
github.com/example/myapp/pkg/bar:5.1,15.5 1 1
`
	path := writeTempCoverOut(t, tmpDir, content)

	blocks, err := parseCoverOut(path)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	if len(blocks) != 4 {
		t.Errorf("expected 4 blocks, got %d", len(blocks))
	}
	if blocks[0].filepath != "github.com/example/myapp/pkg/foo" {
		t.Errorf("unexpected filepath: %s", blocks[0].filepath)
	}
	if blocks[0].startLine != 10 || blocks[0].endLine != 20 || blocks[0].count != 1 {
		t.Errorf("unexpected block values: %+v", blocks[0])
	}
	if blocks[1].count != 0 {
		t.Errorf("expected count 0 for second block, got %d", blocks[1].count)
	}
}

func TestParseCoverOut_IgnoresModeAndEmpty(t *testing.T) {
	tmpDir := t.TempDir()
	content := "mode: set\n\ngithub.com/example/myapp/pkg/foo:5.1,10.5 1 1\n"
	path := writeTempCoverOut(t, tmpDir, content)

	blocks, err := parseCoverOut(path)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	if len(blocks) != 1 {
		t.Errorf("expected 1 block (mode and empty lines ignored), got %d", len(blocks))
	}
}

func TestComputeGoResult_FileNotFound(t *testing.T) {
	_, err := ComputeGoResult("/nonexistent/cover.out", 85)
	if err == nil {
		t.Error("expected error for missing file")
	}
}

func TestComputeGoResult_EmptyBlocks(t *testing.T) {
	tmpDir := t.TempDir()
	path := writeTempCoverOut(t, tmpDir, "mode: set\n")

	result, err := ComputeGoResult(path, 85)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	// No blocks → total=0, pct=100, passed
	if result.Total != 0 {
		t.Errorf("expected total=0, got %d", result.Total)
	}
	if result.Pct != 100.0 {
		t.Errorf("expected pct=100, got %f", result.Pct)
	}
	if !result.Passed {
		t.Error("expected passed=true for empty coverage")
	}
}

func TestComputeGoResult_NoSourceFiles(t *testing.T) {
	// Without source files (no go.mod, no .go files), all lines in blocks are counted
	tmpDir := t.TempDir()

	content := "mode: set\n" +
		"example.com/pkg/foo:1.1,5.5 1 3\n" + // covered
		"example.com/pkg/foo:7.1,9.5 1 0\n" + // missed
		"example.com/pkg/bar:1.1,3.5 1 2\n" // covered
	path := writeTempCoverOut(t, tmpDir, content)

	result, err := ComputeGoResult(path, 85)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	if result.Total == 0 {
		t.Error("expected non-zero total")
	}
	if result.Format != FormatGo {
		t.Errorf("expected FormatGo, got %s", result.Format)
	}
	if result.Threshold != 85 {
		t.Errorf("expected threshold=85, got %f", result.Threshold)
	}
}

func TestComputeGoResult_WithSourceFiles(t *testing.T) {
	tmpDir := t.TempDir()

	// go.mod with module name — placed in the same dir as cover.out
	gomod := "module github.com/example/myapp\n\ngo 1.21\n"
	if err := os.WriteFile(filepath.Join(tmpDir, "go.mod"), []byte(gomod), 0644); err != nil {
		t.Fatal(err)
	}

	// Source file: line 1=code, 2=blank, 3=func (code), 4=comment, 5=code, 6=brace-only
	if err := os.MkdirAll(filepath.Join(tmpDir, "pkg"), 0755); err != nil {
		t.Fatal(err)
	}
	src := "package pkg\n\nfunc Add(a, b int) int {\n// add two numbers\nreturn a + b\n}\n"
	if err := os.WriteFile(filepath.Join(tmpDir, "pkg", "add.go"), []byte(src), 0644); err != nil {
		t.Fatal(err)
	}

	// Cover block covering lines 1-6, count=5 (covered)
	content := "mode: set\ngithub.com/example/myapp/pkg/add.go:1.1,6.2 1 5\n"
	path := writeTempCoverOut(t, tmpDir, content)

	result, err := ComputeGoResult(path, 85)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	// Lines 1, 3, 5 are code; 2 (blank), 4 (comment), 6 ({/}) excluded
	if result.Covered != 3 {
		t.Errorf("expected covered=3 (lines 1, 3, 5), got %d", result.Covered)
	}
	if result.Partial != 0 {
		t.Errorf("expected partial=0, got %d", result.Partial)
	}
	if result.Missed != 0 {
		t.Errorf("expected missed=0, got %d", result.Missed)
	}
	if result.Total != 3 {
		t.Errorf("expected total=3, got %d", result.Total)
	}
	if result.Pct != 100.0 {
		t.Errorf("expected pct=100.0, got %f", result.Pct)
	}
}

func TestComputeGoResult_PartialLines(t *testing.T) {
	tmpDir := t.TempDir()
	// No go.mod → no module stripping; overlapping blocks on same line → partial
	content := "mode: set\n" +
		"example.com/pkg:5.1,5.9 1 1\n" + // line 5 count=1
		"example.com/pkg:5.1,5.9 1 0\n" // line 5 count=0 → partial
	path := writeTempCoverOut(t, tmpDir, content)

	result, err := ComputeGoResult(path, 85)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	if result.Partial != 1 {
		t.Errorf("expected partial=1, got %d", result.Partial)
	}
}

func TestComputeGoResult_PassFail(t *testing.T) {
	tmpDir := t.TempDir()
	// 1 covered, 0 partial, 1 missed → 50% coverage
	content := "mode: set\n" +
		"example.com/pkg:1.1,1.9 1 1\n" + // covered
		"example.com/pkg:2.1,2.9 1 0\n" // missed
	path := writeTempCoverOut(t, tmpDir, content)

	result, err := ComputeGoResult(path, 85)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	if result.Passed {
		t.Error("expected Passed=false for 50% coverage with 85% threshold")
	}
	if result.Pct >= 85 {
		t.Errorf("expected pct < 85, got %f", result.Pct)
	}

	// Same file but with threshold=40 → pass
	result2, err := ComputeGoResult(path, 40)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	if !result2.Passed {
		t.Error("expected Passed=true for 50% coverage with 40% threshold")
	}
}

func TestComputeGoResult_ProjectDirFromFilePath(t *testing.T) {
	// Verify that source files are resolved relative to the cover.out's directory,
	// not the process cwd. This mirrors the Python script's behaviour.
	projectDir := t.TempDir()

	gomod := "module github.com/example/proj\n\ngo 1.21\n"
	if err := os.WriteFile(filepath.Join(projectDir, "go.mod"), []byte(gomod), 0644); err != nil {
		t.Fatal(err)
	}
	if err := os.MkdirAll(filepath.Join(projectDir, "pkg"), 0755); err != nil {
		t.Fatal(err)
	}
	// 3 code lines, all covered
	src := "package pkg\n\nfunc F() int {\nreturn 1\n}\n"
	if err := os.WriteFile(filepath.Join(projectDir, "pkg", "f.go"), []byte(src), 0644); err != nil {
		t.Fatal(err)
	}
	content := "mode: set\ngithub.com/example/proj/pkg/f.go:1.1,5.2 1 1\n"
	path := writeTempCoverOut(t, projectDir, content)

	// Run from a DIFFERENT working directory
	originalWd, _ := os.Getwd()
	otherDir := t.TempDir()
	_ = os.Chdir(otherDir)
	defer func() { _ = os.Chdir(originalWd) }()

	result, err := ComputeGoResult(path, 85)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	// Lines 1 (code), 2 (blank→excluded), 3 (func...{→code), 4 (code), 5 (}→excluded)
	// code lines: 1, 3, 4 → 3 covered
	if result.Covered != 3 {
		t.Errorf("expected covered=3 (source filtering from project dir), got %d", result.Covered)
	}
}
