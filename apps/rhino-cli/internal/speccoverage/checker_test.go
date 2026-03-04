package speccoverage

import (
	"os"
	"path/filepath"
	"testing"
)

// makeFile creates a file at the given path, creating parent dirs as needed.
func makeFile(t *testing.T, path string) {
	t.Helper()
	if err := os.MkdirAll(filepath.Dir(path), 0o755); err != nil {
		t.Fatalf("MkdirAll: %v", err)
	}
	f, err := os.Create(path)
	if err != nil {
		t.Fatalf("Create %s: %v", path, err)
	}
	_ = f.Close()
}

func TestCheckAll_AllCovered(t *testing.T) {
	root := t.TempDir()

	// Create specs
	makeFile(t, filepath.Join(root, "specs", "user-login.feature"))
	makeFile(t, filepath.Join(root, "specs", "auth", "route-protection.feature"))

	// Create matching test files
	makeFile(t, filepath.Join(root, "app", "src", "user-login.integration.test.tsx"))
	makeFile(t, filepath.Join(root, "app", "src", "route-protection.integration.test.tsx"))

	opts := ScanOptions{
		RepoRoot: root,
		SpecsDir: filepath.Join(root, "specs"),
		AppDir:   filepath.Join(root, "app"),
	}

	result, err := CheckAll(opts)
	if err != nil {
		t.Fatalf("CheckAll() error = %v", err)
	}

	if result.TotalSpecs != 2 {
		t.Errorf("TotalSpecs = %d, want 2", result.TotalSpecs)
	}
	if len(result.Gaps) != 0 {
		t.Errorf("Gaps = %v, want none", result.Gaps)
	}
	if result.Duration <= 0 {
		t.Error("Duration should be positive")
	}
}

func TestCheckAll_MissingTest(t *testing.T) {
	root := t.TempDir()

	// Create specs
	makeFile(t, filepath.Join(root, "specs", "user-login.feature"))
	makeFile(t, filepath.Join(root, "specs", "dashboard.feature"))

	// Only one matching test file
	makeFile(t, filepath.Join(root, "app", "src", "user-login.integration.test.tsx"))

	opts := ScanOptions{
		RepoRoot: root,
		SpecsDir: filepath.Join(root, "specs"),
		AppDir:   filepath.Join(root, "app"),
	}

	result, err := CheckAll(opts)
	if err != nil {
		t.Fatalf("CheckAll() error = %v", err)
	}

	if result.TotalSpecs != 2 {
		t.Errorf("TotalSpecs = %d, want 2", result.TotalSpecs)
	}
	if len(result.Gaps) != 1 {
		t.Fatalf("Gaps count = %d, want 1", len(result.Gaps))
	}
	if result.Gaps[0].Stem != "dashboard" {
		t.Errorf("Gap stem = %q, want %q", result.Gaps[0].Stem, "dashboard")
	}
	if result.Gaps[0].SpecFile == "" {
		t.Error("Gap SpecFile should not be empty")
	}
}

func TestCheckAll_AllMissing(t *testing.T) {
	root := t.TempDir()

	makeFile(t, filepath.Join(root, "specs", "feature-a.feature"))
	makeFile(t, filepath.Join(root, "specs", "sub", "feature-b.feature"))

	// App dir exists but no matching test files
	makeFile(t, filepath.Join(root, "app", "unrelated.tsx"))

	opts := ScanOptions{
		RepoRoot: root,
		SpecsDir: filepath.Join(root, "specs"),
		AppDir:   filepath.Join(root, "app"),
	}

	result, err := CheckAll(opts)
	if err != nil {
		t.Fatalf("CheckAll() error = %v", err)
	}

	if result.TotalSpecs != 2 {
		t.Errorf("TotalSpecs = %d, want 2", result.TotalSpecs)
	}
	if len(result.Gaps) != 2 {
		t.Errorf("Gaps count = %d, want 2", len(result.Gaps))
	}
}

func TestCheckAll_EmptySpecsDir(t *testing.T) {
	root := t.TempDir()

	// Specs dir exists but is empty
	if err := os.MkdirAll(filepath.Join(root, "specs"), 0o755); err != nil {
		t.Fatal(err)
	}
	makeFile(t, filepath.Join(root, "app", "something.tsx"))

	opts := ScanOptions{
		RepoRoot: root,
		SpecsDir: filepath.Join(root, "specs"),
		AppDir:   filepath.Join(root, "app"),
	}

	result, err := CheckAll(opts)
	if err != nil {
		t.Fatalf("CheckAll() error = %v", err)
	}

	if result.TotalSpecs != 0 {
		t.Errorf("TotalSpecs = %d, want 0", result.TotalSpecs)
	}
	if len(result.Gaps) != 0 {
		t.Errorf("Gaps = %v, want none", result.Gaps)
	}
}

func TestCheckAll_NonExistentSpecsDir(t *testing.T) {
	root := t.TempDir()

	opts := ScanOptions{
		RepoRoot: root,
		SpecsDir: filepath.Join(root, "nonexistent-specs"),
		AppDir:   filepath.Join(root, "app"),
	}

	result, err := CheckAll(opts)
	if err != nil {
		t.Fatalf("CheckAll() error = %v", err)
	}

	if result.TotalSpecs != 0 {
		t.Errorf("TotalSpecs = %d, want 0", result.TotalSpecs)
	}
}

func TestCheckAll_NonExistentAppDir(t *testing.T) {
	root := t.TempDir()

	makeFile(t, filepath.Join(root, "specs", "user-login.feature"))

	opts := ScanOptions{
		RepoRoot: root,
		SpecsDir: filepath.Join(root, "specs"),
		AppDir:   filepath.Join(root, "nonexistent-app"),
	}

	result, err := CheckAll(opts)
	if err != nil {
		t.Fatalf("CheckAll() error = %v", err)
	}

	if result.TotalSpecs != 1 {
		t.Errorf("TotalSpecs = %d, want 1", result.TotalSpecs)
	}
	if len(result.Gaps) != 1 {
		t.Errorf("Gaps count = %d, want 1", len(result.Gaps))
	}
}

func TestCheckAll_MatchExactStem(t *testing.T) {
	root := t.TempDir()

	makeFile(t, filepath.Join(root, "specs", "feature-x.feature"))

	// Match file with no extension
	makeFile(t, filepath.Join(root, "app", "feature-x"))

	opts := ScanOptions{
		RepoRoot: root,
		SpecsDir: filepath.Join(root, "specs"),
		AppDir:   filepath.Join(root, "app"),
	}

	result, err := CheckAll(opts)
	if err != nil {
		t.Fatalf("CheckAll() error = %v", err)
	}

	if len(result.Gaps) != 0 {
		t.Errorf("Expected no gaps, got %v", result.Gaps)
	}
}

func TestCheckAll_PartialStemNotMatched(t *testing.T) {
	root := t.TempDir()

	makeFile(t, filepath.Join(root, "specs", "login.feature"))

	// "user-login" does NOT match stem "login" (stem is substring, not match)
	makeFile(t, filepath.Join(root, "app", "user-login.integration.test.tsx"))

	opts := ScanOptions{
		RepoRoot: root,
		SpecsDir: filepath.Join(root, "specs"),
		AppDir:   filepath.Join(root, "app"),
	}

	result, err := CheckAll(opts)
	if err != nil {
		t.Fatalf("CheckAll() error = %v", err)
	}

	// "login" stem should not match "user-login.integration.test.tsx"
	if len(result.Gaps) != 1 {
		t.Errorf("Expected 1 gap, got %d: %v", len(result.Gaps), result.Gaps)
	}
}

func TestCheckAll_RelativePath(t *testing.T) {
	root := t.TempDir()

	makeFile(t, filepath.Join(root, "specs", "sub", "deep-feature.feature"))
	makeFile(t, filepath.Join(root, "app", "deep-feature.test.ts"))

	opts := ScanOptions{
		RepoRoot: root,
		SpecsDir: filepath.Join(root, "specs"),
		AppDir:   filepath.Join(root, "app"),
	}

	result, err := CheckAll(opts)
	if err != nil {
		t.Fatalf("CheckAll() error = %v", err)
	}

	if result.TotalSpecs != 1 {
		t.Errorf("TotalSpecs = %d, want 1", result.TotalSpecs)
	}
	if len(result.Gaps) != 0 {
		t.Errorf("Expected no gaps, got: %v", result.Gaps)
	}
}

func TestWalkFeatureFiles_OnlyFeatureFiles(t *testing.T) {
	root := t.TempDir()

	makeFile(t, filepath.Join(root, "a.feature"))
	makeFile(t, filepath.Join(root, "b.feature"))
	makeFile(t, filepath.Join(root, "not-a-feature.ts"))
	makeFile(t, filepath.Join(root, "sub", "c.feature"))

	files, err := walkFeatureFiles(root)
	if err != nil {
		t.Fatalf("walkFeatureFiles() error = %v", err)
	}

	if len(files) != 3 {
		t.Errorf("got %d files, want 3: %v", len(files), files)
	}
}

func TestHasMatchingTestFile_Found(t *testing.T) {
	root := t.TempDir()

	makeFile(t, filepath.Join(root, "src", "user-login.integration.test.tsx"))

	found, err := hasMatchingTestFile(root, "user-login")
	if err != nil {
		t.Fatalf("hasMatchingTestFile() error = %v", err)
	}
	if !found {
		t.Error("Expected found=true")
	}
}

func TestHasMatchingTestFile_NotFound(t *testing.T) {
	root := t.TempDir()

	makeFile(t, filepath.Join(root, "src", "other-file.tsx"))

	found, err := hasMatchingTestFile(root, "user-login")
	if err != nil {
		t.Fatalf("hasMatchingTestFile() error = %v", err)
	}
	if found {
		t.Error("Expected found=false")
	}
}

func TestHasMatchingTestFile_DirNotExist(t *testing.T) {
	root := t.TempDir()

	found, err := hasMatchingTestFile(filepath.Join(root, "nonexistent"), "user-login")
	if err != nil {
		t.Fatalf("hasMatchingTestFile() error = %v", err)
	}
	if found {
		t.Error("Expected found=false for nonexistent dir")
	}
}
