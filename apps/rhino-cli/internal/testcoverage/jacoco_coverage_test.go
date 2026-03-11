package testcoverage

import (
	"math"
	"os"
	"path/filepath"
	"testing"
)

func TestComputeJaCoCoResult_ValidReport(t *testing.T) {
	xml := `<?xml version="1.0" encoding="UTF-8"?>
<report name="test">
  <package name="com/example">
    <sourcefile name="Foo.java">
      <line nr="10" mi="0" ci="3" mb="0" cb="2"/>
      <line nr="11" mi="0" ci="3" mb="0" cb="0"/>
      <line nr="12" mi="3" ci="0" mb="0" cb="0"/>
    </sourcefile>
  </package>
</report>`

	path := writeTemp(t, "jacoco.xml", xml)
	result, err := ComputeJaCoCoResult(path, 50)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}

	if result.Covered != 2 {
		t.Errorf("expected 2 covered, got %d", result.Covered)
	}
	if result.Missed != 1 {
		t.Errorf("expected 1 missed, got %d", result.Missed)
	}
	if result.Partial != 0 {
		t.Errorf("expected 0 partial, got %d", result.Partial)
	}
	if result.Total != 3 {
		t.Errorf("expected 3 total, got %d", result.Total)
	}
	expectedPct := 100.0 * 2.0 / 3.0
	if math.Abs(result.Pct-expectedPct) > 0.01 {
		t.Errorf("expected pct ~%.2f, got %.2f", expectedPct, result.Pct)
	}
	if !result.Passed {
		t.Error("expected Passed=true")
	}
	if result.Format != FormatJaCoCo {
		t.Errorf("expected FormatJaCoCo, got %v", result.Format)
	}
}

func TestComputeJaCoCoResult_PartialBranches(t *testing.T) {
	xml := `<?xml version="1.0" encoding="UTF-8"?>
<report name="test">
  <package name="com/example">
    <sourcefile name="Bar.java">
      <line nr="5" mi="0" ci="2" mb="1" cb="1"/>
      <line nr="6" mi="0" ci="2" mb="0" cb="2"/>
      <line nr="7" mi="2" ci="0" mb="2" cb="0"/>
    </sourcefile>
  </package>
</report>`

	path := writeTemp(t, "jacoco.xml", xml)
	result, err := ComputeJaCoCoResult(path, 90)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}

	if result.Covered != 1 {
		t.Errorf("expected 1 covered, got %d", result.Covered)
	}
	if result.Partial != 1 {
		t.Errorf("expected 1 partial, got %d", result.Partial)
	}
	if result.Missed != 1 {
		t.Errorf("expected 1 missed, got %d", result.Missed)
	}
	expectedPct := 100.0 / 3.0
	if math.Abs(result.Pct-expectedPct) > 0.01 {
		t.Errorf("expected pct ~%.2f, got %.2f", expectedPct, result.Pct)
	}
	if result.Passed {
		t.Error("expected Passed=false")
	}
}

func TestComputeJaCoCoResult_EmptyReport(t *testing.T) {
	xml := `<?xml version="1.0" encoding="UTF-8"?>
<report name="empty">
</report>`

	path := writeTemp(t, "jacoco.xml", xml)
	result, err := ComputeJaCoCoResult(path, 90)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}

	if result.Total != 0 {
		t.Errorf("expected 0 total, got %d", result.Total)
	}
	if result.Pct != 100.0 {
		t.Errorf("expected 100%% for empty report, got %.2f", result.Pct)
	}
	if !result.Passed {
		t.Error("expected Passed=true for empty report")
	}
}

func TestComputeJaCoCoResult_FileNotFound(t *testing.T) {
	_, err := ComputeJaCoCoResult("/nonexistent/jacoco.xml", 90)
	if err == nil {
		t.Fatal("expected error for non-existent file")
	}
}

func TestComputeJaCoCoResult_MalformedXML(t *testing.T) {
	path := writeTemp(t, "jacoco.xml", "this is not xml at all <><><<")
	_, err := ComputeJaCoCoResult(path, 90)
	if err == nil {
		t.Fatal("expected error for malformed XML")
	}
}

func TestComputeJaCoCoResult_MultiplePackages(t *testing.T) {
	xml := `<?xml version="1.0" encoding="UTF-8"?>
<report name="multi">
  <package name="com/example/a">
    <sourcefile name="A.java">
      <line nr="1" mi="0" ci="1" mb="0" cb="0"/>
      <line nr="2" mi="0" ci="1" mb="0" cb="0"/>
    </sourcefile>
  </package>
  <package name="com/example/b">
    <sourcefile name="B.java">
      <line nr="1" mi="1" ci="0" mb="0" cb="0"/>
      <line nr="2" mi="1" ci="0" mb="0" cb="0"/>
    </sourcefile>
  </package>
</report>`

	path := writeTemp(t, "jacoco.xml", xml)
	result, err := ComputeJaCoCoResult(path, 50)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}

	if result.Covered != 2 {
		t.Errorf("expected 2 covered, got %d", result.Covered)
	}
	if result.Missed != 2 {
		t.Errorf("expected 2 missed, got %d", result.Missed)
	}
	if result.Total != 4 {
		t.Errorf("expected 4 total, got %d", result.Total)
	}
	if result.Pct != 50.0 {
		t.Errorf("expected 50%%, got %.2f", result.Pct)
	}
	if !result.Passed {
		t.Error("expected Passed=true when coverage exactly meets threshold")
	}
}

func TestComputeJaCoCoResult_ThresholdExactlyMet(t *testing.T) {
	xml := `<?xml version="1.0" encoding="UTF-8"?>
<report name="exact">
  <package name="com/example">
    <sourcefile name="X.java">
      <line nr="1" mi="0" ci="1" mb="0" cb="0"/>
    </sourcefile>
  </package>
</report>`

	path := writeTemp(t, "jacoco.xml", xml)
	result, err := ComputeJaCoCoResult(path, 100)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}

	if !result.Passed {
		t.Error("expected Passed=true when coverage exactly meets threshold")
	}
}

func writeTemp(t *testing.T, name, content string) string {
	t.Helper()
	path := filepath.Join(t.TempDir(), name)
	if err := os.WriteFile(path, []byte(content), 0644); err != nil {
		t.Fatal(err)
	}
	return path
}
