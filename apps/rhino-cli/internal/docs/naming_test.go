package docs

import (
	"os"
	"path/filepath"
	"strings"
	"testing"
)

// writeFile is a test helper that writes `content` to `dir/rel`, creating
// intermediate directories as needed.
func writeFile(t *testing.T, dir, rel, content string) {
	t.Helper()
	full := filepath.Join(dir, rel)
	if err := os.MkdirAll(filepath.Dir(full), 0o755); err != nil {
		t.Fatal(err)
	}
	if err := os.WriteFile(full, []byte(content), 0o644); err != nil {
		t.Fatal(err)
	}
}

func TestValidateDocsNaming_RequiresAtLeastOnePath(t *testing.T) {
	if _, err := ValidateDocsNaming(nil, nil); err == nil {
		t.Errorf("expected error for empty paths, got nil")
	}
	if _, err := ValidateDocsNaming([]string{}, nil); err == nil {
		t.Errorf("expected error for empty paths, got nil")
	}
}

func TestValidateDocsNaming_CleanTree(t *testing.T) {
	tmp := t.TempDir()
	writeFile(t, tmp, "file-naming.md", "ok")
	writeFile(t, tmp, "nested/three-level-testing-standard.md", "ok")
	writeFile(t, tmp, "README.md", "index")

	findings, err := ValidateDocsNaming([]string{tmp}, nil)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	if len(findings) != 0 {
		t.Fatalf("expected zero findings, got %d: %+v", len(findings), findings)
	}
}

func TestValidateDocsNaming_DetectsUppercase(t *testing.T) {
	tmp := t.TempDir()
	writeFile(t, tmp, "MyDoc.md", "bad")
	writeFile(t, tmp, "ok-doc.md", "ok")

	findings, err := ValidateDocsNaming([]string{tmp}, nil)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	if len(findings) != 1 {
		t.Fatalf("expected exactly 1 finding, got %d: %+v", len(findings), findings)
	}
	if !strings.HasSuffix(findings[0].File, "MyDoc.md") {
		t.Errorf("expected MyDoc.md finding, got %q", findings[0].File)
	}
	if findings[0].Severity != "high" {
		t.Errorf("expected severity high, got %q", findings[0].Severity)
	}
	if !strings.Contains(findings[0].Message, "lowercase-kebab-case") {
		t.Errorf("message missing rule explanation: %q", findings[0].Message)
	}
}

func TestValidateDocsNaming_DetectsUnderscoreAndCamelCase(t *testing.T) {
	tmp := t.TempDir()
	writeFile(t, tmp, "snake_case.md", "bad")
	writeFile(t, tmp, "camelCase.md", "bad")
	writeFile(t, tmp, "UPPER.md", "bad")
	writeFile(t, tmp, "fine-name.md", "ok")

	findings, err := ValidateDocsNaming([]string{tmp}, nil)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	if len(findings) != 3 {
		t.Fatalf("expected 3 findings, got %d: %+v", len(findings), findings)
	}
	// Findings are sorted by File: case-sensitive ASCII order puts capitals
	// before lowercase, so "UPPER.md" < "camelCase.md" < "snake_case.md".
	wantOrder := []string{"UPPER.md", "camelCase.md", "snake_case.md"}
	for i, want := range wantOrder {
		if !strings.HasSuffix(findings[i].File, want) {
			t.Errorf("findings[%d].File = %q, want suffix %q", i, findings[i].File, want)
		}
	}
}

func TestValidateDocsNaming_README_ExemptAtAnyDepth(t *testing.T) {
	tmp := t.TempDir()
	writeFile(t, tmp, "README.md", "top")
	writeFile(t, tmp, "subdir/README.md", "nested")
	writeFile(t, tmp, "a/b/c/README.md", "deeply-nested")

	findings, err := ValidateDocsNaming([]string{tmp}, nil)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	if len(findings) != 0 {
		t.Fatalf("expected zero findings (README exempt), got %d: %+v", len(findings), findings)
	}
}

func TestValidateDocsNaming_ExemptionGlobs(t *testing.T) {
	tmp := t.TempDir()
	writeFile(t, tmp, "MyDoc.md", "bad-but-exempt")
	writeFile(t, tmp, "AnotherBadOne.md", "violation")

	findings, err := ValidateDocsNaming([]string{tmp}, []string{"MyDoc.md"})
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	if len(findings) != 1 {
		t.Fatalf("expected 1 finding (AnotherBadOne.md), got %d: %+v", len(findings), findings)
	}
	if !strings.HasSuffix(findings[0].File, "AnotherBadOne.md") {
		t.Errorf("expected AnotherBadOne.md, got %q", findings[0].File)
	}
}

func TestValidateDocsNaming_ExemptionGlobsWildcard(t *testing.T) {
	tmp := t.TempDir()
	writeFile(t, tmp, "AGENTS.md", "exempt")
	writeFile(t, tmp, "CLAUDE.md", "exempt")
	writeFile(t, tmp, "OtherBadName.md", "violation")

	findings, err := ValidateDocsNaming([]string{tmp}, []string{"[A-Z]*.md"})
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	// All three uppercase basenames match [A-Z]*.md so should all be exempt.
	if len(findings) != 0 {
		t.Fatalf("expected zero findings (all exempt by wildcard), got %d: %+v", len(findings), findings)
	}
}

func TestValidateDocsNaming_InvalidGlobReturnsError(t *testing.T) {
	tmp := t.TempDir()
	writeFile(t, tmp, "anything.md", "x")
	_, err := ValidateDocsNaming([]string{tmp}, []string{"["})
	if err == nil {
		t.Errorf("expected error for malformed glob, got nil")
	}
}

func TestValidateDocsNaming_SkipsNonMarkdownFiles(t *testing.T) {
	tmp := t.TempDir()
	writeFile(t, tmp, "ImageFile.png", "binary-data")
	writeFile(t, tmp, "BadConfig.JSON", "{}")
	writeFile(t, tmp, "fine-name.md", "ok")

	findings, err := ValidateDocsNaming([]string{tmp}, nil)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	if len(findings) != 0 {
		t.Fatalf("expected zero findings (non-.md skipped), got %d: %+v", len(findings), findings)
	}
}

func TestValidateDocsNaming_SkipsSkipDirs(t *testing.T) {
	tmp := t.TempDir()
	// Add a violating file inside each skip-dir; all should be ignored.
	for _, dir := range []string{"node_modules", ".git", ".next", "dist", "build", "target"} {
		writeFile(t, tmp, filepath.Join(dir, "BadName.md"), "violation")
	}
	// And one violation outside skip-dirs to confirm walking still works.
	writeFile(t, tmp, "real/BadName.md", "violation")

	findings, err := ValidateDocsNaming([]string{tmp}, nil)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	if len(findings) != 1 {
		t.Fatalf("expected 1 finding (only real/BadName.md), got %d: %+v", len(findings), findings)
	}
	if !strings.Contains(findings[0].File, "real") {
		t.Errorf("expected real/BadName.md, got %q", findings[0].File)
	}
}

func TestValidateDocsNaming_NonExistentPathIsEmpty(t *testing.T) {
	tmp := t.TempDir()
	findings, err := ValidateDocsNaming([]string{filepath.Join(tmp, "no-such-dir")}, nil)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	if len(findings) != 0 {
		t.Fatalf("expected zero findings for missing path, got %d", len(findings))
	}
}

func TestValidateDocsNaming_MultiplePaths(t *testing.T) {
	tmpA := t.TempDir()
	tmpB := t.TempDir()
	writeFile(t, tmpA, "BadOne.md", "bad-a")
	writeFile(t, tmpB, "BadTwo.md", "bad-b")

	findings, err := ValidateDocsNaming([]string{tmpA, tmpB}, nil)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	if len(findings) != 2 {
		t.Fatalf("expected 2 findings, got %d: %+v", len(findings), findings)
	}
}

func TestValidateDocsNaming_DigitsAndHyphens(t *testing.T) {
	tmp := t.TempDir()
	writeFile(t, tmp, "2025-12-14-phase-0-week-4-initial-commit.md", "ok-date-prefixed")
	writeFile(t, tmp, "a1-b2-c3.md", "ok-digits")
	writeFile(t, tmp, "release-1.md", "ok-with-digit")

	findings, err := ValidateDocsNaming([]string{tmp}, nil)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	if len(findings) != 0 {
		t.Fatalf("expected zero findings (digits and hyphens permitted), got %d: %+v", len(findings), findings)
	}
}

func TestValidateDocsNaming_WalkErrorPropagates(t *testing.T) {
	// Create an unreadable directory to force a walk error. Skip on platforms
	// where chmod cannot revoke read permission (e.g. running as root).
	tmp := t.TempDir()
	bad := filepath.Join(tmp, "no-perm")
	if err := os.MkdirAll(bad, 0o755); err != nil {
		t.Fatal(err)
	}
	writeFile(t, bad, "inside.md", "ok")
	if err := os.Chmod(bad, 0o000); err != nil {
		t.Skipf("could not revoke read permission (running as root?): %v", err)
	}
	defer func() { _ = os.Chmod(bad, 0o755) }()

	_, err := ValidateDocsNaming([]string{tmp}, nil)
	if os.Geteuid() == 0 {
		// Root bypasses chmod restrictions; the call will succeed.
		t.Skip("running as root, cannot exercise walk-error path")
	}
	if err == nil {
		t.Errorf("expected walk error, got nil")
	}
}
