package testcoverage

import (
	"os"
	"path/filepath"
	"testing"
)

func writeTempDiffLCOV(t *testing.T, content string) string {
	t.Helper()
	tmpDir := t.TempDir()
	lcovPath := filepath.Join(tmpDir, "lcov.info")
	if err := os.WriteFile(lcovPath, []byte(content), 0644); err != nil {
		t.Fatal(err)
	}
	return lcovPath
}

func TestComputeDiffCoverage_NoChanges(t *testing.T) {
	origGetGitDiff := getGitDiff
	defer func() { getGitDiff = origGetGitDiff }()
	getGitDiff = func(base string, staged bool) (string, error) {
		return "", nil
	}

	result, err := ComputeDiffCoverage(DiffCoverageOptions{
		CoverageFile: "dummy",
		Base:         "main",
	})
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	if result.Pct != 100.0 {
		t.Errorf("expected 100%% for no changes, got %.2f", result.Pct)
	}
	if !result.Passed {
		t.Error("expected Passed=true for no changes")
	}
}

func TestComputeDiffCoverage_WithChanges(t *testing.T) {
	lcovPath := writeTempDiffLCOV(t, "TN:\nSF:src/foo.ts\nDA:2,1\nDA:3,0\nend_of_record\n")

	origGetGitDiff := getGitDiff
	defer func() { getGitDiff = origGetGitDiff }()
	getGitDiff = func(base string, staged bool) (string, error) {
		return `diff --git a/src/foo.ts b/src/foo.ts
--- a/src/foo.ts
+++ b/src/foo.ts
@@ -1,2 +1,4 @@
 line1
+added1
+added2
 line2
`, nil
	}

	result, err := ComputeDiffCoverage(DiffCoverageOptions{
		CoverageFile: lcovPath,
		Base:         "main",
		Threshold:    50,
	})
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	if result.Covered != 1 {
		t.Errorf("expected 1 covered, got %d", result.Covered)
	}
	if result.Missed != 1 {
		t.Errorf("expected 1 missed, got %d", result.Missed)
	}
	if result.Pct != 50.0 {
		t.Errorf("expected 50%%, got %.2f", result.Pct)
	}
}

func TestComputeDiffCoverage_FileNotInCoverage(t *testing.T) {
	lcovPath := writeTempDiffLCOV(t, "TN:\nSF:src/other.ts\nDA:1,1\nend_of_record\n")

	origGetGitDiff := getGitDiff
	defer func() { getGitDiff = origGetGitDiff }()
	getGitDiff = func(base string, staged bool) (string, error) {
		return `diff --git a/src/new.ts b/src/new.ts
--- /dev/null
+++ b/src/new.ts
@@ -0,0 +1,3 @@
+line1
+line2
+line3
`, nil
	}

	result, err := ComputeDiffCoverage(DiffCoverageOptions{
		CoverageFile: lcovPath,
		Base:         "main",
	})
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	if result.Missed != 3 {
		t.Errorf("expected 3 missed, got %d", result.Missed)
	}
}

func TestComputeDiffCoverage_WithExclusion(t *testing.T) {
	lcovPath := writeTempDiffLCOV(t, "TN:\nSF:src/foo.ts\nDA:2,0\nend_of_record\n")

	origGetGitDiff := getGitDiff
	defer func() { getGitDiff = origGetGitDiff }()
	getGitDiff = func(base string, staged bool) (string, error) {
		return `diff --git a/src/foo.ts b/src/foo.ts
--- a/src/foo.ts
+++ b/src/foo.ts
@@ -1,1 +1,2 @@
 line1
+added
`, nil
	}

	result, err := ComputeDiffCoverage(DiffCoverageOptions{
		CoverageFile:    lcovPath,
		Base:            "main",
		ExcludePatterns: []string{"*.ts"},
	})
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	if result.Total != 0 {
		t.Errorf("expected 0 total after exclusion, got %d", result.Total)
	}
}
