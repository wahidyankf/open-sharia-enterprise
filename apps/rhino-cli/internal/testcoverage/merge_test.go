package testcoverage

import (
	"os"
	"path/filepath"
	"strings"
	"testing"
)

func TestMergeCoverageMaps_SingleMap(t *testing.T) {
	cm := CoverageMap{
		"a.go": {1: {HitCount: 1}, 2: {HitCount: 0}},
	}
	merged := MergeCoverageMaps(cm)
	if merged["a.go"][1].HitCount != 1 {
		t.Errorf("expected hit count 1, got %d", merged["a.go"][1].HitCount)
	}
}

func TestMergeCoverageMaps_OverlappingLines(t *testing.T) {
	cm1 := CoverageMap{"a.go": {1: {HitCount: 3}, 2: {HitCount: 0}}}
	cm2 := CoverageMap{"a.go": {1: {HitCount: 1}, 2: {HitCount: 5}}}
	merged := MergeCoverageMaps(cm1, cm2)
	if merged["a.go"][1].HitCount != 3 {
		t.Errorf("expected max hit count 3, got %d", merged["a.go"][1].HitCount)
	}
	if merged["a.go"][2].HitCount != 5 {
		t.Errorf("expected max hit count 5, got %d", merged["a.go"][2].HitCount)
	}
}

func TestMergeCoverageMaps_DifferentFiles(t *testing.T) {
	cm1 := CoverageMap{"a.go": {1: {HitCount: 1}}}
	cm2 := CoverageMap{"b.go": {1: {HitCount: 2}}}
	merged := MergeCoverageMaps(cm1, cm2)
	if len(merged) != 2 {
		t.Errorf("expected 2 files, got %d", len(merged))
	}
}

func TestMergeCoverageMaps_BranchMerge(t *testing.T) {
	cm1 := CoverageMap{"a.go": {1: {HitCount: 1, Branches: []BranchCoverage{
		{BlockID: 0, BranchID: 0, HitCount: 1},
		{BlockID: 0, BranchID: 1, HitCount: 0},
	}}}}
	cm2 := CoverageMap{"a.go": {1: {HitCount: 1, Branches: []BranchCoverage{
		{BlockID: 0, BranchID: 0, HitCount: 0},
		{BlockID: 0, BranchID: 1, HitCount: 1},
	}}}}
	merged := MergeCoverageMaps(cm1, cm2)
	branches := merged["a.go"][1].Branches
	if len(branches) != 2 {
		t.Fatalf("expected 2 branches, got %d", len(branches))
	}
	// Both should have max hit count of 1
	for _, br := range branches {
		if br.HitCount != 1 {
			t.Errorf("expected branch hit count 1, got %d (block=%d, branch=%d)", br.HitCount, br.BlockID, br.BranchID)
		}
	}
}

func TestFormatLCOVString(t *testing.T) {
	cm := CoverageMap{
		"src/a.ts": {
			1: {HitCount: 3},
			2: {HitCount: 0},
		},
	}
	out := FormatLCOVString(cm)
	if !strings.Contains(out, "SF:src/a.ts") {
		t.Errorf("expected SF record, got: %s", out)
	}
	if !strings.Contains(out, "DA:1,3") {
		t.Errorf("expected DA:1,3, got: %s", out)
	}
	if !strings.Contains(out, "DA:2,0") {
		t.Errorf("expected DA:2,0, got: %s", out)
	}
	if !strings.Contains(out, "end_of_record") {
		t.Error("expected end_of_record")
	}
}

func TestWriteLCOV(t *testing.T) {
	cm := CoverageMap{"src/a.ts": {1: {HitCount: 1}}}
	outPath := filepath.Join(t.TempDir(), "merged.info")
	err := WriteLCOV(cm, outPath)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	data, err := os.ReadFile(outPath)
	if err != nil {
		t.Fatalf("failed to read output: %v", err)
	}
	if !strings.Contains(string(data), "SF:src/a.ts") {
		t.Error("expected SF record in output file")
	}
}

func TestResultFromCoverageMap(t *testing.T) {
	cm := CoverageMap{
		"a.ts": {
			1: {HitCount: 1},
			2: {HitCount: 1, Branches: []BranchCoverage{{HitCount: 0}}},
			3: {HitCount: 0},
		},
	}
	r := ResultFromCoverageMap(cm, 50)
	if r.Covered != 1 {
		t.Errorf("expected 1 covered, got %d", r.Covered)
	}
	if r.Partial != 1 {
		t.Errorf("expected 1 partial, got %d", r.Partial)
	}
	if r.Missed != 1 {
		t.Errorf("expected 1 missed, got %d", r.Missed)
	}
}

func TestToCoverageMapLCOV(t *testing.T) {
	content := "TN:\nSF:src/a.ts\nDA:1,1\nDA:2,0\nend_of_record\n"
	path := filepath.Join(t.TempDir(), "lcov.info")
	if err := os.WriteFile(path, []byte(content), 0644); err != nil {
		t.Fatal(err)
	}

	cm, err := ToCoverageMapLCOV(path)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	if len(cm) != 1 {
		t.Errorf("expected 1 file, got %d", len(cm))
	}
	if cm["src/a.ts"][1].HitCount != 1 {
		t.Errorf("expected hit count 1 for line 1, got %d", cm["src/a.ts"][1].HitCount)
	}
}

func TestToCoverageMap_AutoDetect(t *testing.T) {
	content := "TN:\nSF:src/a.ts\nDA:1,1\nend_of_record\n"
	path := filepath.Join(t.TempDir(), "lcov.info")
	if err := os.WriteFile(path, []byte(content), 0644); err != nil {
		t.Fatal(err)
	}

	cm, err := ToCoverageMap(path)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	if len(cm) != 1 {
		t.Errorf("expected 1 file, got %d", len(cm))
	}
}
