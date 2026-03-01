package java

import (
	"strings"
	"testing"
)

func TestFormatText_NoViolations(t *testing.T) {
	result := &ValidationResult{
		TotalPackages: 1,
		ValidPackages: 1,
		AllPackages:   []PackageEntry{{PackageDir: "com/example", Valid: true}},
		Annotation:    "NullMarked",
	}

	out := FormatText(result, false, false)

	if !strings.Contains(out, "✓") {
		t.Error("expected ✓ for valid package")
	}
	if !strings.Contains(out, "com/example") {
		t.Error("expected package path in output")
	}
	if !strings.Contains(out, "@NullMarked found") {
		t.Error("expected annotation found message")
	}
	if !strings.Contains(out, "0 violations found") {
		t.Error("expected '0 violations found' message")
	}
}

func TestFormatText_WithMissingPackageInfo(t *testing.T) {
	result := &ValidationResult{
		TotalPackages: 1,
		ValidPackages: 0,
		AllPackages:   []PackageEntry{{PackageDir: "com/example", Valid: false, ViolationType: ViolationMissingPackageInfo}},
		Annotation:    "NullMarked",
	}

	out := FormatText(result, false, false)

	if !strings.Contains(out, "✗") {
		t.Error("expected ✗ for invalid package")
	}
	if !strings.Contains(out, "package-info.java missing") {
		t.Error("expected 'package-info.java missing' message")
	}
	if !strings.Contains(out, "1 violation(s) found") {
		t.Error("expected violation count")
	}
}

func TestFormatText_WithMissingAnnotation(t *testing.T) {
	result := &ValidationResult{
		TotalPackages: 1,
		ValidPackages: 0,
		AllPackages:   []PackageEntry{{PackageDir: "com/example", Valid: false, ViolationType: ViolationMissingAnnotation}},
		Annotation:    "NullMarked",
	}

	out := FormatText(result, false, false)

	if !strings.Contains(out, "package-info.java present, @NullMarked missing") {
		t.Error("expected missing annotation message")
	}
}

func TestFormatText_QuietMode_NoViolations(t *testing.T) {
	result := &ValidationResult{
		TotalPackages: 1,
		ValidPackages: 1,
		AllPackages:   []PackageEntry{{PackageDir: "com/example", Valid: true}},
		Annotation:    "NullMarked",
	}

	out := FormatText(result, false, true) // quiet=true

	if strings.Contains(out, "0 violations") {
		t.Error("quiet mode should suppress '0 violations found' message")
	}
}

func TestFormatJSON_Success(t *testing.T) {
	result := &ValidationResult{
		TotalPackages: 2,
		ValidPackages: 2,
		AllPackages: []PackageEntry{
			{PackageDir: "com/example", Valid: true},
			{PackageDir: "com/example/service", Valid: true},
		},
		Annotation: "NullMarked",
	}

	out, err := FormatJSON(result)
	if err != nil {
		t.Fatalf("FormatJSON error: %v", err)
	}

	if !strings.Contains(out, `"status"`) {
		t.Error("expected 'status' field")
	}
	if !strings.Contains(out, `"success"`) {
		t.Error("expected 'success' status")
	}
	if !strings.Contains(out, `"total_packages"`) {
		t.Error("expected 'total_packages' field")
	}
	if !strings.Contains(out, `"valid_packages"`) {
		t.Error("expected 'valid_packages' field")
	}
	if !strings.Contains(out, `"violations"`) {
		t.Error("expected 'violations' field")
	}
	if !strings.Contains(out, `"annotation"`) {
		t.Error("expected 'annotation' field")
	}
}

func TestFormatJSON_Failure(t *testing.T) {
	result := &ValidationResult{
		TotalPackages: 1,
		ValidPackages: 0,
		AllPackages:   []PackageEntry{{PackageDir: "com/example", Valid: false, ViolationType: ViolationMissingPackageInfo}},
		Annotation:    "NullMarked",
	}

	out, err := FormatJSON(result)
	if err != nil {
		t.Fatalf("FormatJSON error: %v", err)
	}

	if !strings.Contains(out, `"failure"`) {
		t.Error("expected 'failure' status")
	}
	if !strings.Contains(out, `"missing_package_info"`) {
		t.Error("expected violation type in JSON")
	}
}

func TestFormatMarkdown_NoViolations(t *testing.T) {
	result := &ValidationResult{
		TotalPackages: 1,
		ValidPackages: 1,
		AllPackages:   []PackageEntry{{PackageDir: "com/example", Valid: true}},
		Annotation:    "NullMarked",
	}

	out := FormatMarkdown(result)

	if !strings.Contains(out, "# Java Null Safety Validation Report") {
		t.Error("expected markdown heading")
	}
	if !strings.Contains(out, "All packages have the required annotation") {
		t.Error("expected success message")
	}
}

func TestFormatMarkdown_WithViolations(t *testing.T) {
	result := &ValidationResult{
		TotalPackages: 2,
		ValidPackages: 1,
		AllPackages: []PackageEntry{
			{PackageDir: "com/example", Valid: true},
			{PackageDir: "com/example/service", Valid: false, ViolationType: ViolationMissingAnnotation},
		},
		Annotation: "NullMarked",
	}

	out := FormatMarkdown(result)

	if !strings.Contains(out, "## Violations") {
		t.Error("expected violations section")
	}
	if !strings.Contains(out, "com/example/service") {
		t.Error("expected violating package path")
	}
}
