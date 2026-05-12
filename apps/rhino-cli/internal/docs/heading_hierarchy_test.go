package docs

import (
	"errors"
	"io"
	"os"
	"path/filepath"
	"strings"
	"testing"
)

func TestValidateDocsHeadingHierarchy_RequiresAtLeastOnePath(t *testing.T) {
	if _, err := ValidateDocsHeadingHierarchy(nil); err == nil {
		t.Errorf("expected error for nil paths, got nil")
	}
	if _, err := ValidateDocsHeadingHierarchy([]string{}); err == nil {
		t.Errorf("expected error for empty paths, got nil")
	}
}

func TestValidateDocsHeadingHierarchy_CleanTree(t *testing.T) {
	tmp := t.TempDir()
	writeFile(t, tmp, "clean-one.md", "# Title\n\n## Section\n\n### Subsection\n")
	writeFile(t, tmp, "nested/clean-two.md", "# Title\n\n## A\n\n## B\n\n### B1\n")
	writeFile(t, tmp, "README.md", "# Index\n")

	findings, err := ValidateDocsHeadingHierarchy([]string{tmp})
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	if len(findings) != 0 {
		t.Fatalf("expected zero findings, got %d: %+v", len(findings), findings)
	}
}

func TestValidateDocsHeadingHierarchy_DetectsDuplicateH1(t *testing.T) {
	tmp := t.TempDir()
	writeFile(t, tmp, "bad.md", "# First\n\nbody\n\n# Second\n")

	findings, err := ValidateDocsHeadingHierarchy([]string{tmp})
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	if len(findings) != 1 {
		t.Fatalf("expected 1 finding, got %d: %+v", len(findings), findings)
	}
	if findings[0].Kind != "duplicate-h1" {
		t.Errorf("expected kind duplicate-h1, got %q", findings[0].Kind)
	}
	if findings[0].Line != 5 {
		t.Errorf("expected line 5 (second H1), got %d", findings[0].Line)
	}
	if findings[0].Severity != "high" {
		t.Errorf("expected severity high, got %q", findings[0].Severity)
	}
	if !strings.Contains(findings[0].Message, "H1") {
		t.Errorf("expected message to mention H1, got %q", findings[0].Message)
	}
}

func TestValidateDocsHeadingHierarchy_DetectsMissingH1(t *testing.T) {
	tmp := t.TempDir()
	writeFile(t, tmp, "no-h1.md", "## Section\n\n### Subsection\n")

	findings, err := ValidateDocsHeadingHierarchy([]string{tmp})
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	// Two findings expected: missing-h1 AND a skipped-level (H2 first with no
	// H1 baseline isn't itself a skip — first heading sets baseline — but
	// missing-h1 fires unconditionally).
	hasMissing := false
	for _, f := range findings {
		if f.Kind == "missing-h1" {
			hasMissing = true
		}
	}
	if !hasMissing {
		t.Fatalf("expected a missing-h1 finding, got: %+v", findings)
	}
}

func TestValidateDocsHeadingHierarchy_DetectsSkippedLevel(t *testing.T) {
	tmp := t.TempDir()
	writeFile(t, tmp, "skipped.md", "# Title\n\n## Section\n\n#### Skipped\n")

	findings, err := ValidateDocsHeadingHierarchy([]string{tmp})
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	if len(findings) != 1 {
		t.Fatalf("expected 1 finding (skipped-level), got %d: %+v", len(findings), findings)
	}
	if findings[0].Kind != "skipped-level" {
		t.Errorf("expected kind skipped-level, got %q", findings[0].Kind)
	}
	if findings[0].Line != 5 {
		t.Errorf("expected line 5 (#### Skipped), got %d", findings[0].Line)
	}
	if !strings.Contains(findings[0].Message, "H4") || !strings.Contains(findings[0].Message, "H2") {
		t.Errorf("expected message to mention H4 and H2, got %q", findings[0].Message)
	}
}

func TestValidateDocsHeadingHierarchy_NoHeadingsIsClean(t *testing.T) {
	tmp := t.TempDir()
	writeFile(t, tmp, "single-line.md", "just prose, no headings here")

	findings, err := ValidateDocsHeadingHierarchy([]string{tmp})
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	if len(findings) != 0 {
		t.Fatalf("expected zero findings for file with no headings, got %d: %+v", len(findings), findings)
	}
}

func TestValidateDocsHeadingHierarchy_EmptyFileIsClean(t *testing.T) {
	tmp := t.TempDir()
	writeFile(t, tmp, "empty.md", "")

	findings, err := ValidateDocsHeadingHierarchy([]string{tmp})
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	if len(findings) != 0 {
		t.Fatalf("expected zero findings for empty file, got %d: %+v", len(findings), findings)
	}
}

func TestValidateDocsHeadingHierarchy_HeadingsInsideFenceIgnored(t *testing.T) {
	tmp := t.TempDir()
	// The "# Not a heading" inside the code fence must NOT be counted; the
	// file has exactly one real H1 and zero skipped levels.
	content := "# Title\n\n## Real H2\n\n```markdown\n# Not a heading\n## Also not\n#### Definitely not\n```\n\n### Real H3\n"
	writeFile(t, tmp, "fenced.md", content)

	findings, err := ValidateDocsHeadingHierarchy([]string{tmp})
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	if len(findings) != 0 {
		t.Fatalf("expected zero findings (fenced headings ignored), got %d: %+v", len(findings), findings)
	}
}

func TestValidateDocsHeadingHierarchy_TildeFenceIgnored(t *testing.T) {
	tmp := t.TempDir()
	content := "# Title\n\n~~~\n# Inside tilde fence\n~~~\n\n## After fence\n"
	writeFile(t, tmp, "tilde.md", content)

	findings, err := ValidateDocsHeadingHierarchy([]string{tmp})
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	if len(findings) != 0 {
		t.Fatalf("expected zero findings, got %d: %+v", len(findings), findings)
	}
}

func TestValidateDocsHeadingHierarchy_MismatchedFenceClosersIgnored(t *testing.T) {
	// A backtick-opened fence cannot be closed by a tilde line — the second
	// "~~~" opens its own block, but since the first fence remains open until
	// EOF the entire remainder is treated as code. This file therefore has
	// only one real heading ("# Title"), zero findings.
	tmp := t.TempDir()
	content := "# Title\n\n```\n## Inside backtick\n~~~ this looks like a tilde fence but isn't\n# also inside\n"
	writeFile(t, tmp, "mismatch.md", content)

	findings, err := ValidateDocsHeadingHierarchy([]string{tmp})
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	if len(findings) != 0 {
		t.Fatalf("expected zero findings, got %d: %+v", len(findings), findings)
	}
}

func TestValidateDocsHeadingHierarchy_MultiplePaths(t *testing.T) {
	tmpA := t.TempDir()
	tmpB := t.TempDir()
	writeFile(t, tmpA, "a.md", "# A\n\n#### Skip\n") // skipped-level finding
	writeFile(t, tmpB, "b.md", "# B1\n\n# B2\n")     // duplicate-h1 finding

	findings, err := ValidateDocsHeadingHierarchy([]string{tmpA, tmpB})
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	if len(findings) != 2 {
		t.Fatalf("expected 2 findings, got %d: %+v", len(findings), findings)
	}
}

func TestValidateDocsHeadingHierarchy_NonExistentPathIsEmpty(t *testing.T) {
	tmp := t.TempDir()
	findings, err := ValidateDocsHeadingHierarchy([]string{filepath.Join(tmp, "no-such-dir")})
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	if len(findings) != 0 {
		t.Fatalf("expected zero findings for missing path, got %d", len(findings))
	}
}

func TestValidateDocsHeadingHierarchy_SkipsSkipDirs(t *testing.T) {
	tmp := t.TempDir()
	// Each of these violation files lives inside a skip-dir; none should be
	// reported.
	for _, dir := range []string{"node_modules", ".git", ".next", "dist", "build", "target"} {
		writeFile(t, tmp, filepath.Join(dir, "violation.md"), "## H2 first, no H1\n")
	}
	// One genuine violation outside skip-dirs.
	writeFile(t, tmp, "real/violation.md", "## H2 first, no H1\n")

	findings, err := ValidateDocsHeadingHierarchy([]string{tmp})
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	if len(findings) == 0 {
		t.Fatalf("expected at least one finding for real/violation.md, got none")
	}
	for _, f := range findings {
		if !strings.Contains(f.File, "real") {
			t.Errorf("finding from skip-dir leaked through: %+v", f)
		}
	}
}

func TestValidateDocsHeadingHierarchy_SkipsNonMarkdownFiles(t *testing.T) {
	tmp := t.TempDir()
	writeFile(t, tmp, "image.png", "binary-data")
	writeFile(t, tmp, "config.json", "{}")
	writeFile(t, tmp, "valid.md", "# Title\n\n## Section\n")

	findings, err := ValidateDocsHeadingHierarchy([]string{tmp})
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	if len(findings) != 0 {
		t.Fatalf("expected zero findings, got %d: %+v", len(findings), findings)
	}
}

func TestValidateDocsHeadingHierarchy_WalkErrorPropagates(t *testing.T) {
	tmp := t.TempDir()
	bad := filepath.Join(tmp, "no-perm")
	if err := os.MkdirAll(bad, 0o755); err != nil {
		t.Fatal(err)
	}
	writeFile(t, bad, "inside.md", "# OK\n")
	if err := os.Chmod(bad, 0o000); err != nil {
		t.Skipf("could not revoke read permission (running as root?): %v", err)
	}
	defer func() { _ = os.Chmod(bad, 0o755) }()

	if os.Geteuid() == 0 {
		t.Skip("running as root, cannot exercise walk-error path")
	}
	_, err := ValidateDocsHeadingHierarchy([]string{tmp})
	if err == nil {
		t.Errorf("expected walk error, got nil")
	}
}

func TestValidateDocsHeadingHierarchy_OpenErrorPropagates(t *testing.T) {
	// Calling scanFileHeadingHierarchy directly on a missing file should
	// return an error (the WalkDir path swallows non-existent roots, but
	// per-file open failures during a successful walk surface as errors).
	_, err := scanFileHeadingHierarchy("/nonexistent/path/does-not-exist.md")
	if err == nil {
		t.Errorf("expected open error for missing file, got nil")
	}
}

// errorReader is an io.Reader that always returns a permanent error;
// used to exercise the scanner.Err() path in collectHeadings.
type errorReader struct{}

func (errorReader) Read(_ []byte) (int, error) {
	return 0, errors.New("simulated read failure")
}

// Compile-time assertion that errorReader satisfies io.Reader.
var _ io.Reader = errorReader{}

func TestCollectHeadings_ScannerErrorPropagates(t *testing.T) {
	_, err := collectHeadings(errorReader{})
	if err == nil {
		t.Errorf("expected scanner error, got nil")
	}
}

func TestParseHeadingLevel(t *testing.T) {
	tests := []struct {
		name      string
		input     string
		wantLevel int
		wantOK    bool
	}{
		{name: "h1", input: "# Title", wantLevel: 1, wantOK: true},
		{name: "h2", input: "## Section", wantLevel: 2, wantOK: true},
		{name: "h6", input: "###### Deep", wantLevel: 6, wantOK: true},
		{name: "seven hashes is not a heading", input: "####### Too deep"},
		{name: "no space", input: "#hashtag"},
		{name: "bare hash", input: "#"},
		{name: "trailing-space-only after hash", input: "#   "},
		{name: "tab-after-hash", input: "#\tTitle", wantLevel: 1, wantOK: true},
		{name: "empty", input: ""},
		{name: "plain text", input: "not a heading"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			lvl, ok := parseHeadingLevel(tt.input)
			if ok != tt.wantOK {
				t.Errorf("ok = %v, want %v", ok, tt.wantOK)
			}
			if lvl != tt.wantLevel {
				t.Errorf("level = %d, want %d", lvl, tt.wantLevel)
			}
		})
	}
}

func TestAnalyzeHeadings_FirstHeadingNotH1StillFlagsMissing(t *testing.T) {
	// First heading is H3; no H1 in file. missing-h1 should fire.
	findings := analyzeHeadings("x.md", []heading{
		{line: 1, level: 3},
		{line: 2, level: 4},
	})
	hasMissing := false
	for _, f := range findings {
		if f.Kind == "missing-h1" {
			hasMissing = true
		}
	}
	if !hasMissing {
		t.Errorf("expected missing-h1, got: %+v", findings)
	}
}

// Test_ValidateDocsHeadingHierarchy_NFence verifies that 4-backtick and
// 5-backtick outer fences containing 3-backtick inner code blocks are handled
// correctly — the inner ``` must NOT close the outer fence.
func Test_ValidateDocsHeadingHierarchy_NFence(t *testing.T) {
	tests := []struct {
		name    string
		content string
		wantOK  bool // true = zero findings expected
	}{
		{
			name: "4-backtick outer contains 3-backtick inner",
			// The ```` ``` ```` opens a 4-tick fence; the inner ``` should NOT close it.
			content: "# Title\n\n## Section\n\n```` markdown\n```go\n# not a heading\n```\n````\n\n### Subsection\n",
			wantOK:  true,
		},
		{
			name:    "5-backtick outer fence",
			content: "# Title\n\n````` markdown\n```go\n# not a heading\n```\n`````\n\n## Section\n",
			wantOK:  true,
		},
		{
			name:    "4-tilde outer with 3-backtick inner",
			content: "# Title\n\n~~~~ text\n```\n# not a heading\n```\n~~~~\n\n## Section\n",
			wantOK:  true,
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			tmp := t.TempDir()
			writeFile(t, tmp, "test.md", tt.content)
			findings, err := ValidateDocsHeadingHierarchy([]string{tmp})
			if err != nil {
				t.Fatalf("ValidateDocsHeadingHierarchy: %v", err)
			}
			if tt.wantOK && len(findings) != 0 {
				t.Errorf("expected 0 findings, got %d: %+v", len(findings), findings)
			}
			if !tt.wantOK && len(findings) == 0 {
				t.Errorf("expected findings, got none")
			}
		})
	}
}

func TestAnalyzeHeadings_ThreeH1sReportedOnce(t *testing.T) {
	// Three H1s should produce one duplicate-h1 finding (we only report the
	// fact of >1, not one finding per extra H1). The reported line is the
	// SECOND H1 — the canonical "first violation" location.
	findings := analyzeHeadings("x.md", []heading{
		{line: 1, level: 1},
		{line: 5, level: 1},
		{line: 9, level: 1},
	})
	dupCount := 0
	var dupLine int
	for _, f := range findings {
		if f.Kind == "duplicate-h1" {
			dupCount++
			dupLine = f.Line
		}
	}
	if dupCount != 1 {
		t.Errorf("expected exactly 1 duplicate-h1 finding, got %d: %+v", dupCount, findings)
	}
	if dupLine != 5 {
		t.Errorf("expected duplicate finding to point at line 5, got %d", dupLine)
	}
}
