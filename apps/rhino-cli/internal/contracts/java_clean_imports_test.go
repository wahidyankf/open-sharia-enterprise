package contracts

import (
	"errors"
	"os"
	"path/filepath"
	"strings"
	"testing"
)

func writeJavaFile(t *testing.T, dir, name, content string) string {
	t.Helper()
	path := filepath.Join(dir, name)
	if err := os.WriteFile(path, []byte(content), 0644); err != nil {
		t.Fatalf("writing java file: %v", err)
	}
	return path
}

func readJavaFile(t *testing.T, path string) string {
	t.Helper()
	data, err := os.ReadFile(path)
	if err != nil {
		t.Fatalf("reading java file: %v", err)
	}
	return string(data)
}

func TestCleanJavaImports_RemovesUnusedImports(t *testing.T) {
	dir := t.TempDir()

	content := `package com.example;

import com.other.UsedClass;
import com.other.UnusedClass;
import java.util.List;

public class Foo {
    UsedClass x;
    List<String> items;
}
`
	path := writeJavaFile(t, dir, "Foo.java", content)

	result, err := CleanJavaImports(JavaCleanImportsOptions{Dir: dir})
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}

	if result.TotalFiles != 1 {
		t.Errorf("TotalFiles: got %d, want 1", result.TotalFiles)
	}
	if result.ModifiedFiles != 1 {
		t.Errorf("ModifiedFiles: got %d, want 1", result.ModifiedFiles)
	}
	if len(result.Modified) != 1 {
		t.Errorf("len(Modified): got %d, want 1", len(result.Modified))
	}

	got := readJavaFile(t, path)
	if strings.Contains(got, "UnusedClass") {
		t.Error("expected UnusedClass import to be removed")
	}
	if !strings.Contains(got, "import com.other.UsedClass;") {
		t.Error("expected UsedClass import to be kept")
	}
	if !strings.Contains(got, "import java.util.List;") {
		t.Error("expected List import to be kept")
	}
}

func TestCleanJavaImports_RemovesSamePackageImports(t *testing.T) {
	dir := t.TempDir()

	content := `package com.example;

import com.example.Helper;
import com.other.UsedClass;

public class Bar {
    Helper h;
    UsedClass x;
}
`
	path := writeJavaFile(t, dir, "Bar.java", content)

	result, err := CleanJavaImports(JavaCleanImportsOptions{Dir: dir})
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}

	if result.ModifiedFiles != 1 {
		t.Errorf("ModifiedFiles: got %d, want 1", result.ModifiedFiles)
	}

	got := readJavaFile(t, path)
	if strings.Contains(got, "import com.example.Helper;") {
		t.Error("expected same-package import to be removed")
	}
	if !strings.Contains(got, "import com.other.UsedClass;") {
		t.Error("expected cross-package used import to be kept")
	}
}

func TestCleanJavaImports_DeduplicatesImports(t *testing.T) {
	dir := t.TempDir()

	content := `package com.example;

import com.other.UsedClass;
import com.other.UsedClass;

public class Baz {
    UsedClass x;
}
`
	path := writeJavaFile(t, dir, "Baz.java", content)

	_, err := CleanJavaImports(JavaCleanImportsOptions{Dir: dir})
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}

	got := readJavaFile(t, path)
	count := strings.Count(got, "import com.other.UsedClass;")
	if count != 1 {
		t.Errorf("expected exactly 1 occurrence of the import, got %d", count)
	}
}

func TestCleanJavaImports_KeepsStaticImports(t *testing.T) {
	dir := t.TempDir()

	content := `package com.example;

import static com.other.Constants.MAX_SIZE;

public class Qux {
    int x = MAX_SIZE;
}
`
	path := writeJavaFile(t, dir, "Qux.java", content)

	_, err := CleanJavaImports(JavaCleanImportsOptions{Dir: dir})
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}

	got := readJavaFile(t, path)
	if !strings.Contains(got, "import static com.other.Constants.MAX_SIZE;") {
		t.Error("expected used static import to be kept")
	}
}

func TestCleanJavaImports_EmptyDir(t *testing.T) {
	dir := t.TempDir()

	result, err := CleanJavaImports(JavaCleanImportsOptions{Dir: dir})
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}

	if result.TotalFiles != 0 {
		t.Errorf("TotalFiles: got %d, want 0", result.TotalFiles)
	}
	if result.ModifiedFiles != 0 {
		t.Errorf("ModifiedFiles: got %d, want 0", result.ModifiedFiles)
	}
	if len(result.Modified) != 0 {
		t.Errorf("len(Modified): got %d, want 0", len(result.Modified))
	}
}

func TestCleanJavaImports_NoChanges(t *testing.T) {
	dir := t.TempDir()

	content := `package com.example;

import com.other.UsedClass;

public class Clean {
    UsedClass x;
}
`
	originalPath := writeJavaFile(t, dir, "Clean.java", content)

	result, err := CleanJavaImports(JavaCleanImportsOptions{Dir: dir})
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}

	if result.ModifiedFiles != 0 {
		t.Errorf("ModifiedFiles: got %d, want 0", result.ModifiedFiles)
	}

	got := readJavaFile(t, originalPath)
	gotNorm := strings.TrimRight(got, "\n") + "\n"
	wantNorm := strings.TrimRight(content, "\n") + "\n"
	if gotNorm != wantNorm {
		t.Errorf("file content changed unexpectedly:\ngot:  %q\nwant: %q", gotNorm, wantNorm)
	}
}

func TestCleanJavaImports_NonexistentDir(t *testing.T) {
	_, err := CleanJavaImports(JavaCleanImportsOptions{Dir: "/nonexistent/path/that/does/not/exist"})
	if err == nil {
		t.Error("expected error for nonexistent dir, got nil")
	}
}

func TestCleanJavaImports_WriteError(t *testing.T) {
	dir := t.TempDir()

	content := `package com.example;

import com.other.UsedClass;
import com.other.UnusedClass;

public class Foo {
    UsedClass x;
}
`
	writeJavaFile(t, dir, "Foo.java", content)

	injectedErr := errors.New("injected write error")
	original := writeFile
	writeFile = func(name string, data []byte, perm os.FileMode) error {
		return injectedErr
	}
	defer func() { writeFile = original }()

	_, err := CleanJavaImports(JavaCleanImportsOptions{Dir: dir})
	if err == nil {
		t.Error("expected error from injected writeFile failure, got nil")
	}
	if !errors.Is(err, injectedErr) {
		t.Errorf("expected injected error in chain, got: %v", err)
	}
}

func TestCleanJavaImports_RenameError(t *testing.T) {
	dir := t.TempDir()

	content := `package com.example;

import com.other.UsedClass;
import com.other.UnusedClass;

public class Foo {
    UsedClass x;
}
`
	writeJavaFile(t, dir, "Foo.java", content)

	injectedErr := errors.New("injected rename error")
	original := osRename
	osRename = func(oldpath, newpath string) error {
		return injectedErr
	}
	defer func() { osRename = original }()

	_, err := CleanJavaImports(JavaCleanImportsOptions{Dir: dir})
	if err == nil {
		t.Error("expected error from injected osRename failure, got nil")
	}
	if !errors.Is(err, injectedErr) {
		t.Errorf("expected injected error in chain, got: %v", err)
	}
}

func TestCleanJavaImports_ReadError(t *testing.T) {
	dir := t.TempDir()

	writeJavaFile(t, dir, "Foo.java", "package com.example;\n")

	// Make file unreadable.
	if err := os.Chmod(filepath.Join(dir, "Foo.java"), 0000); err != nil {
		t.Fatalf("chmod: %v", err)
	}
	defer func() {
		_ = os.Chmod(filepath.Join(dir, "Foo.java"), 0644)
	}()

	_, err := CleanJavaImports(JavaCleanImportsOptions{Dir: dir})
	if err == nil {
		t.Error("expected error for unreadable file, got nil")
	}
}
