package testcoverage

import (
	"testing"
)

func TestExcludeFiles_NoPatterns(t *testing.T) {
	r := &Result{
		Covered: 10, Partial: 1, Missed: 2, Total: 13, Pct: 76.92, Threshold: 70, Passed: true,
		Files: []FileResult{
			{Path: "src/a.ts", Covered: 5, Total: 6, Pct: 83.33},
			{Path: "src/b.ts", Covered: 5, Partial: 1, Missed: 2, Total: 7, Pct: 62.50},
		},
	}
	ExcludeFiles(r, nil)
	if len(r.Files) != 2 {
		t.Errorf("expected 2 files, got %d", len(r.Files))
	}
}

func TestExcludeFiles_MatchByBaseName(t *testing.T) {
	r := &Result{
		Covered: 10, Partial: 0, Missed: 5, Total: 15, Threshold: 50, Passed: true,
		Files: []FileResult{
			{Path: "src/a.ts", Covered: 5, Total: 5, Pct: 100.0},
			{Path: "src/b.ts", Covered: 3, Missed: 2, Total: 5, Pct: 60.0},
			{Path: "src/c.ts", Covered: 2, Missed: 3, Total: 5, Pct: 40.0},
		},
	}
	ExcludeFiles(r, []string{"b.ts"})
	if len(r.Files) != 2 {
		t.Errorf("expected 2 files after excluding b.ts, got %d", len(r.Files))
	}
	if r.Covered != 7 || r.Missed != 3 {
		t.Errorf("expected recalculated aggregates: covered=7 missed=3, got covered=%d missed=%d", r.Covered, r.Missed)
	}
}

func TestExcludeFiles_MatchByGlob(t *testing.T) {
	r := &Result{
		Covered: 10, Total: 15, Threshold: 50, Passed: true,
		Files: []FileResult{
			{Path: "src/generated/a.ts", Covered: 3, Total: 5, Pct: 60.0},
			{Path: "src/main.ts", Covered: 7, Total: 10, Pct: 70.0},
		},
	}
	ExcludeFiles(r, []string{"src/generated/*"})
	if len(r.Files) != 1 {
		t.Errorf("expected 1 file, got %d", len(r.Files))
	}
	if r.Files[0].Path != "src/main.ts" {
		t.Errorf("expected src/main.ts, got %s", r.Files[0].Path)
	}
	if r.Covered != 7 {
		t.Errorf("expected covered=7, got %d", r.Covered)
	}
}

func TestExcludeFiles_AllExcluded(t *testing.T) {
	r := &Result{
		Covered: 5, Total: 5, Threshold: 90, Passed: false,
		Files: []FileResult{
			{Path: "a.ts", Covered: 5, Total: 5, Pct: 100.0},
		},
	}
	ExcludeFiles(r, []string{"*.ts"})
	if len(r.Files) != 0 {
		t.Errorf("expected 0 files, got %d", len(r.Files))
	}
	if r.Total != 0 {
		t.Errorf("expected total=0, got %d", r.Total)
	}
	if r.Pct != 100.0 {
		t.Errorf("expected 100%% for empty, got %.2f", r.Pct)
	}
	if !r.Passed {
		t.Error("expected Passed=true for empty result")
	}
}

func TestExcludeFiles_RecalculatesPassFail(t *testing.T) {
	// 70% overall but after excluding the good file, drops to 50% < 60% threshold
	r := &Result{
		Covered: 7, Missed: 3, Total: 10, Pct: 70, Threshold: 60, Passed: true,
		Files: []FileResult{
			{Path: "good.ts", Covered: 5, Total: 5, Pct: 100.0},
			{Path: "bad.ts", Covered: 2, Missed: 3, Total: 5, Pct: 40.0},
		},
	}
	ExcludeFiles(r, []string{"good.ts"})
	if r.Passed {
		t.Error("expected Passed=false after excluding good file")
	}
}

func TestMatchesAnyPattern(t *testing.T) {
	tests := []struct {
		path     string
		patterns []string
		want     bool
	}{
		{"src/a.ts", []string{"*.ts"}, true},
		{"src/a.ts", []string{"*.go"}, false},
		{"src/gen/a.ts", []string{"src/gen/*"}, true},
		{"src/a.ts", []string{"a.ts"}, true},
		{"src/a.ts", []string{"b.ts"}, false},
	}
	for _, tt := range tests {
		got := MatchesAnyExcludePattern(tt.path, tt.patterns)
		if got != tt.want {
			t.Errorf("MatchesAnyExcludePattern(%q, %v) = %v, want %v", tt.path, tt.patterns, got, tt.want)
		}
	}
}
