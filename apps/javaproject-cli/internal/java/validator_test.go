package java

import (
	"os"
	"path/filepath"
	"testing"
)

func makeSourceRoot(t *testing.T) string {
	t.Helper()
	return t.TempDir()
}

func writeFile(t *testing.T, path, content string) {
	t.Helper()
	if err := os.MkdirAll(filepath.Dir(path), 0755); err != nil {
		t.Fatalf("MkdirAll(%s): %v", filepath.Dir(path), err)
	}
	if err := os.WriteFile(path, []byte(content), 0644); err != nil {
		t.Fatalf("WriteFile(%s): %v", path, err)
	}
}

// countViolations returns the number of invalid packages in the result.
func countViolations(result *ValidationResult) int {
	return result.TotalPackages - result.ValidPackages
}

// firstViolation returns the first invalid PackageEntry, or zero value if none.
func firstViolation(result *ValidationResult) PackageEntry {
	for _, pkg := range result.AllPackages {
		if !pkg.Valid {
			return pkg
		}
	}
	return PackageEntry{}
}

func TestValidateAll_AllValid(t *testing.T) {
	src := makeSourceRoot(t)

	pkgDir := filepath.Join(src, "com", "example")
	writeFile(t, filepath.Join(pkgDir, "Foo.java"), "class Foo {}")
	writeFile(t, filepath.Join(pkgDir, "package-info.java"), "@NullMarked\npackage com.example;")

	result, err := ValidateAll(ValidationOptions{SourceRoot: src, Annotation: "NullMarked"})
	if err != nil {
		t.Fatalf("ValidateAll error: %v", err)
	}

	if result.TotalPackages != 1 {
		t.Errorf("TotalPackages: want 1, got %d", result.TotalPackages)
	}
	if result.ValidPackages != 1 {
		t.Errorf("ValidPackages: want 1, got %d", result.ValidPackages)
	}
	if countViolations(result) != 0 {
		t.Errorf("violations: want 0, got %d", countViolations(result))
	}
	if result.AllPackages[0].Valid != true {
		t.Error("AllPackages[0].Valid should be true")
	}
}

func TestValidateAll_MissingPackageInfo(t *testing.T) {
	src := makeSourceRoot(t)

	pkgDir := filepath.Join(src, "com", "example")
	writeFile(t, filepath.Join(pkgDir, "Foo.java"), "class Foo {}")
	// No package-info.java

	result, err := ValidateAll(ValidationOptions{SourceRoot: src, Annotation: "NullMarked"})
	if err != nil {
		t.Fatalf("ValidateAll error: %v", err)
	}

	if result.TotalPackages != 1 {
		t.Errorf("TotalPackages: want 1, got %d", result.TotalPackages)
	}
	if result.ValidPackages != 0 {
		t.Errorf("ValidPackages: want 0, got %d", result.ValidPackages)
	}
	if countViolations(result) != 1 {
		t.Fatalf("violations: want 1, got %d", countViolations(result))
	}
	v := firstViolation(result)
	if v.ViolationType != ViolationMissingPackageInfo {
		t.Errorf("ViolationType: want %s, got %s", ViolationMissingPackageInfo, v.ViolationType)
	}
	if result.AllPackages[0].Valid != false {
		t.Error("AllPackages[0].Valid should be false")
	}
}

func TestValidateAll_MissingAnnotation(t *testing.T) {
	src := makeSourceRoot(t)

	pkgDir := filepath.Join(src, "com", "example")
	writeFile(t, filepath.Join(pkgDir, "Foo.java"), "class Foo {}")
	writeFile(t, filepath.Join(pkgDir, "package-info.java"), "package com.example;") // no annotation

	result, err := ValidateAll(ValidationOptions{SourceRoot: src, Annotation: "NullMarked"})
	if err != nil {
		t.Fatalf("ValidateAll error: %v", err)
	}

	if countViolations(result) != 1 {
		t.Fatalf("violations: want 1, got %d", countViolations(result))
	}
	v := firstViolation(result)
	if v.ViolationType != ViolationMissingAnnotation {
		t.Errorf("ViolationType: want %s, got %s", ViolationMissingAnnotation, v.ViolationType)
	}
}

func TestValidateAll_MultiplePackages_MixedResults(t *testing.T) {
	src := makeSourceRoot(t)

	// Package 1: valid
	pkg1 := filepath.Join(src, "com", "example")
	writeFile(t, filepath.Join(pkg1, "A.java"), "class A {}")
	writeFile(t, filepath.Join(pkg1, "package-info.java"), "@NullMarked\npackage com.example;")

	// Package 2: missing package-info.java
	pkg2 := filepath.Join(src, "com", "example", "service")
	writeFile(t, filepath.Join(pkg2, "B.java"), "class B {}")

	// Package 3: package-info.java present but missing annotation
	pkg3 := filepath.Join(src, "com", "example", "config")
	writeFile(t, filepath.Join(pkg3, "C.java"), "class C {}")
	writeFile(t, filepath.Join(pkg3, "package-info.java"), "package com.example.config;")

	result, err := ValidateAll(ValidationOptions{SourceRoot: src, Annotation: "NullMarked"})
	if err != nil {
		t.Fatalf("ValidateAll error: %v", err)
	}

	if result.TotalPackages != 3 {
		t.Errorf("TotalPackages: want 3, got %d", result.TotalPackages)
	}
	if result.ValidPackages != 1 {
		t.Errorf("ValidPackages: want 1, got %d", result.ValidPackages)
	}
	if countViolations(result) != 2 {
		t.Errorf("violations: want 2, got %d", countViolations(result))
	}
}

func TestValidateAll_EmptySourceRoot(t *testing.T) {
	src := makeSourceRoot(t)

	result, err := ValidateAll(ValidationOptions{SourceRoot: src, Annotation: "NullMarked"})
	if err != nil {
		t.Fatalf("ValidateAll error: %v", err)
	}
	if result.TotalPackages != 0 {
		t.Errorf("TotalPackages: want 0, got %d", result.TotalPackages)
	}
	if countViolations(result) != 0 {
		t.Errorf("violations: want 0, got %d", countViolations(result))
	}
}

func TestValidateAll_AnnotationStored(t *testing.T) {
	src := makeSourceRoot(t)

	result, err := ValidateAll(ValidationOptions{SourceRoot: src, Annotation: "MyAnnotation"})
	if err != nil {
		t.Fatalf("ValidateAll error: %v", err)
	}
	if result.Annotation != "MyAnnotation" {
		t.Errorf("Annotation: want MyAnnotation, got %s", result.Annotation)
	}
}

func TestValidateAll_RelativePackagePaths(t *testing.T) {
	src := makeSourceRoot(t)

	pkgDir := filepath.Join(src, "com", "example")
	writeFile(t, filepath.Join(pkgDir, "A.java"), "class A {}")
	writeFile(t, filepath.Join(pkgDir, "package-info.java"), "@NullMarked\npackage com.example;")

	result, err := ValidateAll(ValidationOptions{SourceRoot: src, Annotation: "NullMarked"})
	if err != nil {
		t.Fatalf("ValidateAll error: %v", err)
	}

	if len(result.AllPackages) != 1 {
		t.Fatalf("expected 1 package, got %d", len(result.AllPackages))
	}
	got := result.AllPackages[0].PackageDir
	want := filepath.Join("com", "example")
	if got != want {
		t.Errorf("PackageDir: want %q, got %q", want, got)
	}
}
