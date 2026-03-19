package testcoverage

import (
	"math"
	"os"
	"path/filepath"
	"testing"
)

func writeTempCobertura(t *testing.T, content string) string {
	t.Helper()
	path := filepath.Join(t.TempDir(), "cobertura.xml")
	if err := os.WriteFile(path, []byte(content), 0644); err != nil {
		t.Fatal(err)
	}
	return path
}

func TestComputeCoberturaResult_ValidReport(t *testing.T) {
	xml := `<?xml version="1.0" ?>
<coverage version="5.5" timestamp="1234567890">
  <packages>
    <package name="mypackage">
      <classes>
        <class name="mypackage.MyClass" filename="mypackage/myclass.py">
          <lines>
            <line number="1" hits="3"/>
            <line number="2" hits="1"/>
            <line number="3" hits="0"/>
          </lines>
        </class>
      </classes>
    </package>
  </packages>
</coverage>`

	path := writeTempCobertura(t, xml)
	result, err := ComputeCoberturaResult(path, 50)
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
	if result.Format != FormatCobertura {
		t.Errorf("expected FormatCobertura, got %v", result.Format)
	}
}

func TestComputeCoberturaResult_PartialBranches(t *testing.T) {
	xml := `<?xml version="1.0" ?>
<coverage version="5.5">
  <packages>
    <package name="pkg">
      <classes>
        <class name="pkg.Cls" filename="pkg/cls.py">
          <lines>
            <line number="5" hits="2" branch="true" condition-coverage="50% (1/2)"/>
            <line number="6" hits="3" branch="true" condition-coverage="100% (2/2)"/>
            <line number="7" hits="0"/>
          </lines>
        </class>
      </classes>
    </package>
  </packages>
</coverage>`

	path := writeTempCobertura(t, xml)
	result, err := ComputeCoberturaResult(path, 90)
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

func TestComputeCoberturaResult_EmptyReport(t *testing.T) {
	xml := `<?xml version="1.0" ?>
<coverage version="5.5">
  <packages/>
</coverage>`

	path := writeTempCobertura(t, xml)
	result, err := ComputeCoberturaResult(path, 90)
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

func TestComputeCoberturaResult_FileNotFound(t *testing.T) {
	_, err := ComputeCoberturaResult("/nonexistent/cobertura.xml", 90)
	if err == nil {
		t.Fatal("expected error for non-existent file")
	}
}

func TestComputeCoberturaResult_MalformedXML(t *testing.T) {
	path := writeTempCobertura(t, "this is not xml at all <><><<")
	_, err := ComputeCoberturaResult(path, 90)
	if err == nil {
		t.Fatal("expected error for malformed XML")
	}
}

func TestComputeCoberturaResult_MultiplePackages(t *testing.T) {
	xml := `<?xml version="1.0" ?>
<coverage version="5.5">
  <packages>
    <package name="a">
      <classes>
        <class name="a.A" filename="a/a.py">
          <lines>
            <line number="1" hits="1"/>
            <line number="2" hits="1"/>
          </lines>
        </class>
      </classes>
    </package>
    <package name="b">
      <classes>
        <class name="b.B" filename="b/b.py">
          <lines>
            <line number="1" hits="0"/>
            <line number="2" hits="0"/>
          </lines>
        </class>
      </classes>
    </package>
  </packages>
</coverage>`

	path := writeTempCobertura(t, xml)
	result, err := ComputeCoberturaResult(path, 50)
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

func TestComputeCoberturaResult_BranchWithNoCoverageAttr(t *testing.T) {
	xml := `<?xml version="1.0" ?>
<coverage version="5.5">
  <packages>
    <package name="pkg">
      <classes>
        <class name="pkg.C" filename="pkg/c.py">
          <lines>
            <line number="1" hits="1" branch="true"/>
          </lines>
        </class>
      </classes>
    </package>
  </packages>
</coverage>`

	path := writeTempCobertura(t, xml)
	result, err := ComputeCoberturaResult(path, 90)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}

	// branch=true but no condition-coverage → treat as covered (brTotal=0)
	if result.Covered != 1 {
		t.Errorf("expected 1 covered, got %d", result.Covered)
	}
}

func TestParseBranchCoverage(t *testing.T) {
	tests := []struct {
		input           string
		wantCov, wantTot int
	}{
		{"50% (1/2)", 1, 2},
		{"100% (4/4)", 4, 4},
		{"0% (0/2)", 0, 2},
		{"75% (3/4)", 3, 4},
		{"", 0, 0},
		{"invalid", 0, 0},
		{"50% (bad)", 0, 0},
	}

	for _, tt := range tests {
		c, tot := parseBranchCoverage(tt.input)
		if c != tt.wantCov || tot != tt.wantTot {
			t.Errorf("parseBranchCoverage(%q) = (%d, %d), want (%d, %d)",
				tt.input, c, tot, tt.wantCov, tt.wantTot)
		}
	}
}
