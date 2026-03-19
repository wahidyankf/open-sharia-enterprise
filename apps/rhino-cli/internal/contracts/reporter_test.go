package contracts

import (
	"encoding/json"
	"strings"
	"testing"
)

// --- Java clean imports text ---

func TestFormatJavaCleanImportsText_WithModifications_Normal(t *testing.T) {
	result := &JavaCleanImportsResult{
		TotalFiles:    5,
		ModifiedFiles: 2,
		Modified:      []string{"com/example/Foo.java", "com/example/Bar.java"},
	}

	out := FormatJavaCleanImportsText(result, false, false)

	if !strings.Contains(out, "Cleaned imports in 2 of 5 Java files.") {
		t.Errorf("expected summary line, got: %q", out)
	}
	if strings.Contains(out, "Foo.java") {
		t.Error("non-verbose mode should not list individual files")
	}
}

func TestFormatJavaCleanImportsText_WithModifications_Verbose(t *testing.T) {
	result := &JavaCleanImportsResult{
		TotalFiles:    5,
		ModifiedFiles: 2,
		Modified:      []string{"com/example/Foo.java", "com/example/Bar.java"},
	}

	out := FormatJavaCleanImportsText(result, true, false)

	if !strings.Contains(out, "Cleaned imports in 2 of 5 Java files.") {
		t.Errorf("expected summary line, got: %q", out)
	}
	if !strings.Contains(out, "✓ com/example/Foo.java") {
		t.Errorf("expected verbose listing of Foo.java, got: %q", out)
	}
	if !strings.Contains(out, "✓ com/example/Bar.java") {
		t.Errorf("expected verbose listing of Bar.java, got: %q", out)
	}
}

func TestFormatJavaCleanImportsText_WithModifications_Quiet(t *testing.T) {
	result := &JavaCleanImportsResult{
		TotalFiles:    5,
		ModifiedFiles: 2,
		Modified:      []string{"com/example/Foo.java"},
	}

	out := FormatJavaCleanImportsText(result, false, true)

	// Quiet with modifications should still show the summary.
	if !strings.Contains(out, "Cleaned imports") {
		t.Errorf("expected summary even in quiet mode when files modified, got: %q", out)
	}
}

func TestFormatJavaCleanImportsText_NoModifications_Normal(t *testing.T) {
	result := &JavaCleanImportsResult{
		TotalFiles:    3,
		ModifiedFiles: 0,
		Modified:      []string{},
	}

	out := FormatJavaCleanImportsText(result, false, false)

	if !strings.Contains(out, "No imports needed cleaning.") {
		t.Errorf("expected 'No imports needed cleaning.', got: %q", out)
	}
}

func TestFormatJavaCleanImportsText_NoModifications_Quiet(t *testing.T) {
	result := &JavaCleanImportsResult{
		TotalFiles:    3,
		ModifiedFiles: 0,
		Modified:      []string{},
	}

	out := FormatJavaCleanImportsText(result, false, true)

	if out != "" {
		t.Errorf("expected empty string in quiet mode with no modifications, got: %q", out)
	}
}

// --- Java clean imports JSON ---

func TestFormatJavaCleanImportsJSON_WithModifications(t *testing.T) {
	result := &JavaCleanImportsResult{
		TotalFiles:    5,
		ModifiedFiles: 2,
		Modified:      []string{"Foo.java", "Bar.java"},
	}

	out, err := FormatJavaCleanImportsJSON(result)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}

	if !strings.Contains(out, `"status"`) {
		t.Error("expected 'status' field")
	}
	if !strings.Contains(out, `"success"`) {
		t.Error("expected 'success' status")
	}
	if !strings.Contains(out, `"timestamp"`) {
		t.Error("expected 'timestamp' field")
	}
	if !strings.Contains(out, `"total_files"`) {
		t.Error("expected 'total_files' field")
	}
	if !strings.Contains(out, `"modified_files"`) {
		t.Error("expected 'modified_files' field")
	}
	if !strings.Contains(out, `"modified"`) {
		t.Error("expected 'modified' field")
	}
	if !strings.Contains(out, "Foo.java") {
		t.Error("expected Foo.java in modified array")
	}

	// Verify valid JSON that can be unmarshaled.
	var parsed map[string]any
	if err := json.Unmarshal([]byte(out), &parsed); err != nil {
		t.Errorf("output is not valid JSON: %v", err)
	}
}

func TestFormatJavaCleanImportsJSON_EmptyModified(t *testing.T) {
	result := &JavaCleanImportsResult{
		TotalFiles:    3,
		ModifiedFiles: 0,
		Modified:      []string{},
	}

	out, err := FormatJavaCleanImportsJSON(result)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}

	// Empty array, not null.
	if !strings.Contains(out, `"modified": []`) {
		t.Errorf("expected empty modified array, got: %q", out)
	}

	var parsed map[string]any
	if err := json.Unmarshal([]byte(out), &parsed); err != nil {
		t.Errorf("output is not valid JSON: %v", err)
	}
}

// --- Java clean imports markdown ---

func TestFormatJavaCleanImportsMarkdown_WithModifications(t *testing.T) {
	result := &JavaCleanImportsResult{
		TotalFiles:    5,
		ModifiedFiles: 2,
		Modified:      []string{"com/example/Foo.java", "com/example/Bar.java"},
	}

	out := FormatJavaCleanImportsMarkdown(result)

	if !strings.Contains(out, "# Java Import Cleaning Report") {
		t.Error("expected markdown heading")
	}
	if !strings.Contains(out, "**Total files**") {
		t.Error("expected total files field")
	}
	if !strings.Contains(out, "**Modified files**") {
		t.Error("expected modified files field")
	}
	if !strings.Contains(out, "## Modified Files") {
		t.Error("expected modified files section")
	}
	if !strings.Contains(out, "`com/example/Foo.java`") {
		t.Error("expected Foo.java in modified files list")
	}
	if !strings.Contains(out, "`com/example/Bar.java`") {
		t.Error("expected Bar.java in modified files list")
	}
}

func TestFormatJavaCleanImportsMarkdown_NoModifications(t *testing.T) {
	result := &JavaCleanImportsResult{
		TotalFiles:    3,
		ModifiedFiles: 0,
		Modified:      []string{},
	}

	out := FormatJavaCleanImportsMarkdown(result)

	if !strings.Contains(out, "# Java Import Cleaning Report") {
		t.Error("expected markdown heading")
	}
	if strings.Contains(out, "## Modified Files") {
		t.Error("should not have Modified Files section when nothing was modified")
	}
	if !strings.Contains(out, "No files needed cleaning.") {
		t.Errorf("expected 'No files needed cleaning.' message, got: %q", out)
	}
}

// --- Dart scaffold text ---

func TestFormatDartScaffoldText_Normal(t *testing.T) {
	result := &DartScaffoldResult{
		PubspecCreated: true,
		BarrelCreated:  true,
		ModelFiles:     []string{"account.dart", "user.dart"},
	}

	out := FormatDartScaffoldText(result, false, false)

	if !strings.Contains(out, "Dart scaffold created:") {
		t.Errorf("expected scaffold summary, got: %q", out)
	}
	if !strings.Contains(out, "2 model files") {
		t.Errorf("expected model file count, got: %q", out)
	}
	if strings.Contains(out, "account.dart") {
		t.Error("non-verbose mode should not list individual files")
	}
}

func TestFormatDartScaffoldText_Verbose(t *testing.T) {
	result := &DartScaffoldResult{
		PubspecCreated: true,
		BarrelCreated:  true,
		ModelFiles:     []string{"account.dart", "user.dart"},
	}

	out := FormatDartScaffoldText(result, true, false)

	if !strings.Contains(out, "Dart scaffold created:") {
		t.Errorf("expected scaffold summary, got: %q", out)
	}
	if !strings.Contains(out, "✓ account.dart") {
		t.Errorf("expected account.dart in verbose output, got: %q", out)
	}
	if !strings.Contains(out, "✓ user.dart") {
		t.Errorf("expected user.dart in verbose output, got: %q", out)
	}
}

func TestFormatDartScaffoldText_Quiet(t *testing.T) {
	result := &DartScaffoldResult{
		PubspecCreated: true,
		BarrelCreated:  true,
		ModelFiles:     []string{"account.dart"},
	}

	out := FormatDartScaffoldText(result, false, true)

	if out != "ok\n" {
		t.Errorf("expected 'ok\\n' in quiet mode, got: %q", out)
	}
}

func TestFormatDartScaffoldText_NoModels(t *testing.T) {
	result := &DartScaffoldResult{
		PubspecCreated: true,
		BarrelCreated:  true,
		ModelFiles:     []string{},
	}

	out := FormatDartScaffoldText(result, false, false)

	if !strings.Contains(out, "0 model files") {
		t.Errorf("expected '0 model files', got: %q", out)
	}
}

// --- Dart scaffold JSON ---

func TestFormatDartScaffoldJSON_WithModels(t *testing.T) {
	result := &DartScaffoldResult{
		PubspecCreated: true,
		BarrelCreated:  true,
		ModelFiles:     []string{"account.dart", "user.dart"},
	}

	out, err := FormatDartScaffoldJSON(result)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}

	if !strings.Contains(out, `"status"`) {
		t.Error("expected 'status' field")
	}
	if !strings.Contains(out, `"success"`) {
		t.Error("expected 'success' status")
	}
	if !strings.Contains(out, `"timestamp"`) {
		t.Error("expected 'timestamp' field")
	}
	if !strings.Contains(out, `"pubspec_created"`) {
		t.Error("expected 'pubspec_created' field")
	}
	if !strings.Contains(out, `"barrel_created"`) {
		t.Error("expected 'barrel_created' field")
	}
	if !strings.Contains(out, `"model_files"`) {
		t.Error("expected 'model_files' field")
	}
	if !strings.Contains(out, "account.dart") {
		t.Error("expected account.dart in model_files")
	}

	var parsed map[string]any
	if err := json.Unmarshal([]byte(out), &parsed); err != nil {
		t.Errorf("output is not valid JSON: %v", err)
	}
}

func TestFormatDartScaffoldJSON_EmptyModelFiles(t *testing.T) {
	result := &DartScaffoldResult{
		PubspecCreated: true,
		BarrelCreated:  true,
		ModelFiles:     []string{},
	}

	out, err := FormatDartScaffoldJSON(result)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}

	if !strings.Contains(out, `"model_files": []`) {
		t.Errorf("expected empty model_files array, got: %q", out)
	}

	var parsed map[string]any
	if err := json.Unmarshal([]byte(out), &parsed); err != nil {
		t.Errorf("output is not valid JSON: %v", err)
	}
}

// --- Dart scaffold markdown ---

func TestFormatDartScaffoldMarkdown_WithModels(t *testing.T) {
	result := &DartScaffoldResult{
		PubspecCreated: true,
		BarrelCreated:  true,
		ModelFiles:     []string{"account.dart", "user.dart"},
	}

	out := FormatDartScaffoldMarkdown(result)

	if !strings.Contains(out, "# Dart Contract Scaffold Report") {
		t.Error("expected markdown heading")
	}
	if !strings.Contains(out, "**pubspec.yaml**") {
		t.Error("expected pubspec.yaml field")
	}
	if !strings.Contains(out, "**Barrel library**") {
		t.Error("expected Barrel library field")
	}
	if !strings.Contains(out, "**Model files**") {
		t.Error("expected Model files field")
	}
	if !strings.Contains(out, "## Model Files") {
		t.Error("expected Model Files section")
	}
	if !strings.Contains(out, "`account.dart`") {
		t.Error("expected account.dart in model files list")
	}
	if !strings.Contains(out, "`user.dart`") {
		t.Error("expected user.dart in model files list")
	}
}

func TestFormatDartScaffoldMarkdown_NoModels(t *testing.T) {
	result := &DartScaffoldResult{
		PubspecCreated: true,
		BarrelCreated:  true,
		ModelFiles:     []string{},
	}

	out := FormatDartScaffoldMarkdown(result)

	if !strings.Contains(out, "# Dart Contract Scaffold Report") {
		t.Error("expected markdown heading")
	}
	if strings.Contains(out, "## Model Files") {
		t.Error("should not have Model Files section when no models")
	}
	if !strings.Contains(out, "**Model files**: 0") {
		t.Errorf("expected model files count of 0, got: %q", out)
	}
}
