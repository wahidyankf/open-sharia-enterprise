package testcoverage

import (
	"os"
	"path/filepath"
	"testing"
)

func TestDetectFormat_InfoExtension(t *testing.T) {
	got := DetectFormat("/some/path/coverage.info")
	if got != FormatLCOV {
		t.Errorf("expected FormatLCOV for .info extension, got %v", got)
	}
}

func TestDetectFormat_LcovInName(t *testing.T) {
	got := DetectFormat("/some/path/lcov.out")
	if got != FormatLCOV {
		t.Errorf("expected FormatLCOV for lcov in name, got %v", got)
	}
}

func TestDetectFormat_CaseInsensitiveLcov(t *testing.T) {
	got := DetectFormat("/some/path/LCOV_report.txt")
	if got != FormatLCOV {
		t.Errorf("expected FormatLCOV for uppercase LCOV in name, got %v", got)
	}
}

func TestDetectFormat_FileOpenError(t *testing.T) {
	got := DetectFormat("/nonexistent/path/cover.out")
	if got != FormatGo {
		t.Errorf("expected FormatGo for non-existent file, got %v", got)
	}
}

func TestDetectFormat_ModePrefix(t *testing.T) {
	tmpDir := t.TempDir()
	path := filepath.Join(tmpDir, "cover.out")
	if err := os.WriteFile(path, []byte("mode: atomic\n"), 0644); err != nil {
		t.Fatal(err)
	}
	got := DetectFormat(path)
	if got != FormatGo {
		t.Errorf("expected FormatGo for mode: prefix, got %v", got)
	}
}

func TestDetectFormat_SFPrefix(t *testing.T) {
	tmpDir := t.TempDir()
	path := filepath.Join(tmpDir, "cover.out")
	if err := os.WriteFile(path, []byte("SF:src/foo.ts\n"), 0644); err != nil {
		t.Fatal(err)
	}
	got := DetectFormat(path)
	if got != FormatLCOV {
		t.Errorf("expected FormatLCOV for SF: prefix, got %v", got)
	}
}

func TestDetectFormat_TNPrefix(t *testing.T) {
	tmpDir := t.TempDir()
	path := filepath.Join(tmpDir, "cover.out")
	if err := os.WriteFile(path, []byte("TN:test name\n"), 0644); err != nil {
		t.Fatal(err)
	}
	got := DetectFormat(path)
	if got != FormatLCOV {
		t.Errorf("expected FormatLCOV for TN: prefix, got %v", got)
	}
}

func TestDetectFormat_UnknownFallback(t *testing.T) {
	tmpDir := t.TempDir()
	path := filepath.Join(tmpDir, "cover.out")
	if err := os.WriteFile(path, []byte("some unknown first line\n"), 0644); err != nil {
		t.Fatal(err)
	}
	got := DetectFormat(path)
	if got != FormatGo {
		t.Errorf("expected FormatGo for unknown first line, got %v", got)
	}
}

func TestDetectFormat_EmptyFile(t *testing.T) {
	tmpDir := t.TempDir()
	path := filepath.Join(tmpDir, "cover.out")
	if err := os.WriteFile(path, []byte(""), 0644); err != nil {
		t.Fatal(err)
	}
	got := DetectFormat(path)
	if got != FormatGo {
		t.Errorf("expected FormatGo for empty file, got %v", got)
	}
}

func TestDetectFormat_JaCoCoFilename(t *testing.T) {
	got := DetectFormat("/some/path/jacoco.xml")
	if got != FormatJaCoCo {
		t.Errorf("expected FormatJaCoCo for jacoco.xml, got %v", got)
	}
}

func TestDetectFormat_JaCoCoCaseInsensitive(t *testing.T) {
	got := DetectFormat("/some/path/JACOCO_report.xml")
	if got != FormatJaCoCo {
		t.Errorf("expected FormatJaCoCo for JACOCO_report.xml, got %v", got)
	}
}

func TestDetectFormat_JaCoCoRequiresXmlExtension(t *testing.T) {
	got := DetectFormat("/some/path/jacoco.exec")
	if got != FormatGo {
		t.Errorf("expected FormatGo for jacoco.exec (not .xml), got %v", got)
	}
}

func TestDetectFormat_XMLContentReport(t *testing.T) {
	tmpDir := t.TempDir()
	path := filepath.Join(tmpDir, "report.xml")
	if err := os.WriteFile(path, []byte(`<?xml version="1.0"?><report name="t"></report>`), 0644); err != nil {
		t.Fatal(err)
	}
	got := DetectFormat(path)
	if got != FormatJaCoCo {
		t.Errorf("expected FormatJaCoCo for <?xml content, got %v", got)
	}
}

func TestDetectFormat_XMLContentReportTag(t *testing.T) {
	tmpDir := t.TempDir()
	path := filepath.Join(tmpDir, "coverage.xml")
	if err := os.WriteFile(path, []byte(`<report name="t"></report>`), 0644); err != nil {
		t.Fatal(err)
	}
	got := DetectFormat(path)
	if got != FormatJaCoCo {
		t.Errorf("expected FormatJaCoCo for <report content, got %v", got)
	}
}
