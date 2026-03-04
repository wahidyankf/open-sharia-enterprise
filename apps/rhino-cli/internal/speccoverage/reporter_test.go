package speccoverage

import (
	"encoding/json"
	"strings"
	"testing"
	"time"
)

func TestFormatText_NoCoverage(t *testing.T) {
	result := &CheckResult{
		TotalSpecs: 5,
		Gaps:       []CoverageGap{},
		Duration:   50 * time.Millisecond,
	}

	out := FormatText(result, false, false)

	if !strings.Contains(out, "✓ Spec coverage valid!") {
		t.Errorf("Expected success message, got: %q", out)
	}
	if !strings.Contains(out, "5 specs checked") {
		t.Errorf("Expected spec count, got: %q", out)
	}
}

func TestFormatText_NoCoverage_Quiet(t *testing.T) {
	result := &CheckResult{
		TotalSpecs: 3,
		Gaps:       []CoverageGap{},
		Duration:   10 * time.Millisecond,
	}

	out := FormatText(result, false, true)

	if out != "" {
		t.Errorf("Expected empty output in quiet mode, got: %q", out)
	}
}

func TestFormatText_WithGaps(t *testing.T) {
	result := &CheckResult{
		TotalSpecs: 3,
		Gaps: []CoverageGap{
			{SpecFile: "specs/auth/user-login.feature", Stem: "user-login"},
			{SpecFile: "specs/dashboard.feature", Stem: "dashboard"},
		},
		Duration: 30 * time.Millisecond,
	}

	out := FormatText(result, false, false)

	if !strings.Contains(out, "✗ Spec coverage gaps found!") {
		t.Errorf("Expected failure message, got: %q", out)
	}
	if !strings.Contains(out, "2 of 3") {
		t.Errorf("Expected gap count, got: %q", out)
	}
	if !strings.Contains(out, "specs/auth/user-login.feature") {
		t.Errorf("Expected spec file path, got: %q", out)
	}
	if !strings.Contains(out, "user-login") {
		t.Errorf("Expected stem hint, got: %q", out)
	}
	if !strings.Contains(out, "specs/dashboard.feature") {
		t.Errorf("Expected second gap, got: %q", out)
	}
}

func TestFormatJSON_Success(t *testing.T) {
	result := &CheckResult{
		TotalSpecs: 9,
		Gaps:       []CoverageGap{},
		Duration:   100 * time.Millisecond,
	}

	jsonStr, err := FormatJSON(result)
	if err != nil {
		t.Fatalf("FormatJSON() error = %v", err)
	}

	var out JSONOutput
	if err := json.Unmarshal([]byte(jsonStr), &out); err != nil {
		t.Fatalf("Failed to parse JSON: %v", err)
	}

	if out.Status != "success" {
		t.Errorf("Status = %q, want %q", out.Status, "success")
	}
	if out.TotalSpecs != 9 {
		t.Errorf("TotalSpecs = %d, want 9", out.TotalSpecs)
	}
	if out.GapCount != 0 {
		t.Errorf("GapCount = %d, want 0", out.GapCount)
	}
	if out.Timestamp == "" {
		t.Error("Timestamp should not be empty")
	}
	if len(out.Gaps) != 0 {
		t.Errorf("Gaps = %v, want empty", out.Gaps)
	}
}

func TestFormatJSON_WithGaps(t *testing.T) {
	result := &CheckResult{
		TotalSpecs: 3,
		Gaps: []CoverageGap{
			{SpecFile: "specs/missing.feature", Stem: "missing"},
		},
		Duration: 42 * time.Millisecond,
	}

	jsonStr, err := FormatJSON(result)
	if err != nil {
		t.Fatalf("FormatJSON() error = %v", err)
	}

	var out JSONOutput
	if err := json.Unmarshal([]byte(jsonStr), &out); err != nil {
		t.Fatalf("Failed to parse JSON: %v", err)
	}

	if out.Status != "failure" {
		t.Errorf("Status = %q, want %q", out.Status, "failure")
	}
	if out.TotalSpecs != 3 {
		t.Errorf("TotalSpecs = %d, want 3", out.TotalSpecs)
	}
	if out.GapCount != 1 {
		t.Errorf("GapCount = %d, want 1", out.GapCount)
	}
	if out.DurationMS != 42 {
		t.Errorf("DurationMS = %d, want 42", out.DurationMS)
	}
	if len(out.Gaps) != 1 {
		t.Fatalf("Gaps count = %d, want 1", len(out.Gaps))
	}
	if out.Gaps[0].SpecFile != "specs/missing.feature" {
		t.Errorf("Gap SpecFile = %q, want %q", out.Gaps[0].SpecFile, "specs/missing.feature")
	}
	if out.Gaps[0].Stem != "missing" {
		t.Errorf("Gap Stem = %q, want %q", out.Gaps[0].Stem, "missing")
	}
}

func TestFormatMarkdown_SameAsText(t *testing.T) {
	result := &CheckResult{
		TotalSpecs: 2,
		Gaps: []CoverageGap{
			{SpecFile: "specs/example.feature", Stem: "example"},
		},
		Duration: 20 * time.Millisecond,
	}

	textOut := FormatText(result, false, false)
	mdOut := FormatMarkdown(result)

	if textOut != mdOut {
		t.Errorf("Markdown output differs from text:\ntext: %q\nmarkdown: %q", textOut, mdOut)
	}
}

func TestFormatMarkdown_Success(t *testing.T) {
	result := &CheckResult{
		TotalSpecs: 4,
		Gaps:       []CoverageGap{},
		Duration:   15 * time.Millisecond,
	}

	out := FormatMarkdown(result)

	if !strings.Contains(out, "✓ Spec coverage valid!") {
		t.Errorf("Expected success message, got: %q", out)
	}
}
