package java

import (
	"os"
	"path/filepath"
	"testing"
)

func TestScanPackages_Empty(t *testing.T) {
	tmpDir := t.TempDir()

	packages, err := ScanPackages(tmpDir)
	if err != nil {
		t.Fatalf("ScanPackages returned error: %v", err)
	}
	if len(packages) != 0 {
		t.Errorf("expected 0 packages, got %d", len(packages))
	}
}

func TestScanPackages_SinglePackage(t *testing.T) {
	tmpDir := t.TempDir()

	pkgDir := filepath.Join(tmpDir, "com", "example")
	if err := os.MkdirAll(pkgDir, 0755); err != nil {
		t.Fatal(err)
	}
	if err := os.WriteFile(filepath.Join(pkgDir, "Foo.java"), []byte("class Foo {}"), 0644); err != nil {
		t.Fatal(err)
	}

	packages, err := ScanPackages(tmpDir)
	if err != nil {
		t.Fatalf("ScanPackages returned error: %v", err)
	}
	if len(packages) != 1 {
		t.Errorf("expected 1 package, got %d", len(packages))
	}
	if packages[0] != pkgDir {
		t.Errorf("expected %s, got %s", pkgDir, packages[0])
	}
}

func TestScanPackages_MultiplePackages(t *testing.T) {
	tmpDir := t.TempDir()

	dirs := []string{
		filepath.Join(tmpDir, "com", "example"),
		filepath.Join(tmpDir, "com", "example", "service"),
		filepath.Join(tmpDir, "com", "example", "config"),
	}
	for _, dir := range dirs {
		if err := os.MkdirAll(dir, 0755); err != nil {
			t.Fatal(err)
		}
		if err := os.WriteFile(filepath.Join(dir, "A.java"), []byte("class A {}"), 0644); err != nil {
			t.Fatal(err)
		}
	}

	packages, err := ScanPackages(tmpDir)
	if err != nil {
		t.Fatalf("ScanPackages returned error: %v", err)
	}
	if len(packages) != 3 {
		t.Errorf("expected 3 packages, got %d: %v", len(packages), packages)
	}

	// Verify packages are sorted
	for i := 1; i < len(packages); i++ {
		if packages[i] < packages[i-1] {
			t.Errorf("packages not sorted: %v", packages)
		}
	}
}

func TestScanPackages_NonJavaFilesIgnored(t *testing.T) {
	tmpDir := t.TempDir()

	// Directory with only non-.java files should not be included
	dir := filepath.Join(tmpDir, "com", "example")
	if err := os.MkdirAll(dir, 0755); err != nil {
		t.Fatal(err)
	}
	if err := os.WriteFile(filepath.Join(dir, "README.md"), []byte("# Readme"), 0644); err != nil {
		t.Fatal(err)
	}
	if err := os.WriteFile(filepath.Join(dir, "config.xml"), []byte("<config/>"), 0644); err != nil {
		t.Fatal(err)
	}

	packages, err := ScanPackages(tmpDir)
	if err != nil {
		t.Fatalf("ScanPackages returned error: %v", err)
	}
	if len(packages) != 0 {
		t.Errorf("expected 0 packages (non-java files), got %d: %v", len(packages), packages)
	}
}

func TestScanPackages_PackageInfoCounts(t *testing.T) {
	tmpDir := t.TempDir()

	// A directory with only package-info.java should be included
	pkgDir := filepath.Join(tmpDir, "com", "example")
	if err := os.MkdirAll(pkgDir, 0755); err != nil {
		t.Fatal(err)
	}
	if err := os.WriteFile(filepath.Join(pkgDir, "package-info.java"), []byte("@NullMarked\npackage com.example;"), 0644); err != nil {
		t.Fatal(err)
	}

	packages, err := ScanPackages(tmpDir)
	if err != nil {
		t.Fatalf("ScanPackages returned error: %v", err)
	}
	if len(packages) != 1 {
		t.Errorf("expected 1 package (package-info.java counts), got %d", len(packages))
	}
}

func TestScanPackages_IntermediateDirsNotIncluded(t *testing.T) {
	tmpDir := t.TempDir()

	// Only the leaf directory has .java files; intermediate dirs should not be included
	leafDir := filepath.Join(tmpDir, "com", "example", "service")
	if err := os.MkdirAll(leafDir, 0755); err != nil {
		t.Fatal(err)
	}
	if err := os.WriteFile(filepath.Join(leafDir, "Service.java"), []byte("class Service {}"), 0644); err != nil {
		t.Fatal(err)
	}

	packages, err := ScanPackages(tmpDir)
	if err != nil {
		t.Fatalf("ScanPackages returned error: %v", err)
	}
	if len(packages) != 1 {
		t.Errorf("expected 1 package (only leaf dir), got %d: %v", len(packages), packages)
	}
	if packages[0] != leafDir {
		t.Errorf("expected %s, got %s", leafDir, packages[0])
	}
}
