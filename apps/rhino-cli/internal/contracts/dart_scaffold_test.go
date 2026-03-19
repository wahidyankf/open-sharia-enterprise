package contracts

import (
	"errors"
	"os"
	"path/filepath"
	"strings"
	"testing"
)

func TestScaffoldDart_WithModels(t *testing.T) {
	dir := t.TempDir()
	modelDir := filepath.Join(dir, "lib", "model")
	if err := os.MkdirAll(modelDir, 0755); err != nil {
		t.Fatalf("creating model dir: %v", err)
	}

	// Create two model files — intentionally out of sorted order.
	for _, name := range []string{"user.dart", "account.dart"} {
		path := filepath.Join(modelDir, name)
		if err := os.WriteFile(path, []byte("// model"), 0644); err != nil {
			t.Fatalf("creating model file: %v", err)
		}
	}

	result, err := ScaffoldDart(DartScaffoldOptions{Dir: dir})
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}

	if !result.PubspecCreated {
		t.Error("expected PubspecCreated to be true")
	}
	if !result.BarrelCreated {
		t.Error("expected BarrelCreated to be true")
	}
	if len(result.ModelFiles) != 2 {
		t.Errorf("ModelFiles count: got %d, want 2", len(result.ModelFiles))
	}
	if result.ModelFiles[0] != "account.dart" {
		t.Errorf("ModelFiles[0]: got %q, want %q", result.ModelFiles[0], "account.dart")
	}
	if result.ModelFiles[1] != "user.dart" {
		t.Errorf("ModelFiles[1]: got %q, want %q", result.ModelFiles[1], "user.dart")
	}

	// Verify pubspec.yaml content.
	pubspecData, err := os.ReadFile(filepath.Join(dir, "pubspec.yaml"))
	if err != nil {
		t.Fatalf("reading pubspec.yaml: %v", err)
	}
	pubspec := string(pubspecData)
	if !strings.Contains(pubspec, "name: demo_contracts") {
		t.Error("pubspec.yaml missing 'name: demo_contracts'")
	}
	if !strings.Contains(pubspec, "sdk: ^3.11.1") {
		t.Error("pubspec.yaml missing sdk constraint")
	}
	if !strings.Contains(pubspec, "collection: ^1.18.0") {
		t.Error("pubspec.yaml missing collection dependency")
	}

	// Verify barrel file content.
	barrelData, err := os.ReadFile(filepath.Join(dir, "lib", "demo_contracts.dart"))
	if err != nil {
		t.Fatalf("reading barrel file: %v", err)
	}
	barrel := string(barrelData)

	if !strings.Contains(barrel, "library openapi.api;") {
		t.Error("barrel missing 'library openapi.api;'")
	}
	if !strings.Contains(barrel, "part 'model/account.dart';") {
		t.Error("barrel missing part directive for account.dart")
	}
	if !strings.Contains(barrel, "part 'model/user.dart';") {
		t.Error("barrel missing part directive for user.dart")
	}

	// Verify sorted order: account.dart should appear before user.dart.
	accountPos := strings.Index(barrel, "account.dart")
	userPos := strings.Index(barrel, "user.dart")
	if accountPos >= userPos {
		t.Error("expected account.dart to appear before user.dart in barrel file")
	}

	// Verify utility functions are present.
	if !strings.Contains(barrel, "mapValueOfType") {
		t.Error("barrel missing mapValueOfType utility function")
	}
	if !strings.Contains(barrel, "mapDateTime") {
		t.Error("barrel missing mapDateTime utility function")
	}
	if !strings.Contains(barrel, "mapCastOfType") {
		t.Error("barrel missing mapCastOfType utility function")
	}
	if !strings.Contains(barrel, "_deepEquality") {
		t.Error("barrel missing _deepEquality constant")
	}
}

func TestScaffoldDart_NoModels(t *testing.T) {
	dir := t.TempDir()

	result, err := ScaffoldDart(DartScaffoldOptions{Dir: dir})
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}

	if !result.PubspecCreated {
		t.Error("expected PubspecCreated to be true")
	}
	if !result.BarrelCreated {
		t.Error("expected BarrelCreated to be true")
	}
	if len(result.ModelFiles) != 0 {
		t.Errorf("ModelFiles: got %v, want empty", result.ModelFiles)
	}

	barrelData, err := os.ReadFile(filepath.Join(dir, "lib", "demo_contracts.dart"))
	if err != nil {
		t.Fatalf("reading barrel file: %v", err)
	}
	barrel := string(barrelData)

	if strings.Contains(barrel, "part 'model/") {
		t.Error("barrel should have no part directives when no model files exist")
	}
	if !strings.Contains(barrel, "mapValueOfType") {
		t.Error("barrel should still have utility functions when no models")
	}
}

func TestScaffoldDart_NonexistentDir(t *testing.T) {
	_, err := ScaffoldDart(DartScaffoldOptions{Dir: "/nonexistent/path/that/does/not/exist"})
	if err == nil {
		t.Error("expected error for nonexistent dir, got nil")
	}
}

func TestScaffoldDart_WriteError(t *testing.T) {
	dir := t.TempDir()

	injectedErr := errors.New("injected dartWriteFile error")
	original := dartWriteFile
	dartWriteFile = func(name string, data []byte, perm os.FileMode) error {
		return injectedErr
	}
	defer func() { dartWriteFile = original }()

	_, err := ScaffoldDart(DartScaffoldOptions{Dir: dir})
	if err == nil {
		t.Error("expected error from injected dartWriteFile failure, got nil")
	}
	if !errors.Is(err, injectedErr) {
		t.Errorf("expected injected error in chain, got: %v", err)
	}
}

func TestScaffoldDart_MkdirError(t *testing.T) {
	dir := t.TempDir()

	injectedErr := errors.New("injected dartMkdirAll error")
	original := dartMkdirAll
	dartMkdirAll = func(path string, perm os.FileMode) error {
		return injectedErr
	}
	defer func() { dartMkdirAll = original }()

	_, err := ScaffoldDart(DartScaffoldOptions{Dir: dir})
	if err == nil {
		t.Error("expected error from injected dartMkdirAll failure, got nil")
	}
	if !errors.Is(err, injectedErr) {
		t.Errorf("expected injected error in chain, got: %v", err)
	}
}

func TestScaffoldDart_GlobError(t *testing.T) {
	dir := t.TempDir()

	injectedErr := errors.New("injected dartGlob error")
	original := dartGlob
	dartGlob = func(pattern string) ([]string, error) {
		return nil, injectedErr
	}
	defer func() { dartGlob = original }()

	_, err := ScaffoldDart(DartScaffoldOptions{Dir: dir})
	if err == nil {
		t.Error("expected error from injected dartGlob failure, got nil")
	}
	if !errors.Is(err, injectedErr) {
		t.Errorf("expected injected error in chain, got: %v", err)
	}
}
