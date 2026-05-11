package repogovernance

import (
	"os"
	"path/filepath"
	"strings"
	"testing"
)

// writeLayerFixture is a small test helper that creates path with the given
// content under tmp, creating parent directories as needed.
func writeLayerFixture(t *testing.T, tmp, rel, content string) {
	t.Helper()
	full := filepath.Join(tmp, rel)
	if err := os.MkdirAll(filepath.Dir(full), 0o755); err != nil {
		t.Fatalf("mkdir parent for %s: %v", rel, err)
	}
	if err := os.WriteFile(full, []byte(content), 0o644); err != nil {
		t.Fatalf("write %s: %v", rel, err)
	}
}

// cleanArchDoc is a minimal architecture-doc fixture that declares layers 0
// through 5 in the H2-heading form used by the real architecture document.
const cleanArchDoc = `# Repository Governance Architecture

## Layer 0: Vision (WHY WE EXIST)

text

## Layer 1: Principles (WHY - Values)

text

## Layer 2: Conventions (WHAT - Rules)

text

## Layer 3: Development (HOW - Practices)

text

## Layer 4: AI Agents (WHO - Executors)

text

## Layer 5: Workflows (WHEN - Multi-Step Processes)

text
`

// cleanReadme is a minimal README fixture that declares the same six layers
// in the bold-span form used by the real governance README.
const cleanReadme = `# repo-governance

- **Layer 0: Vision** - WHY we exist
- **Layer 1: Principles** - WHY we value approaches
- **Layer 2: Conventions** - WHAT rules
- **Layer 3: Development** - HOW we develop
- **Layer 4: AI Agents** - WHO enforces rules
- **Layer 5: Workflows** - WHEN orchestrate
`

func TestAuditLayerCoherence_CleanRepo(t *testing.T) {
	tmp := t.TempDir()
	writeLayerFixture(t, tmp, layerCoherenceArchPath, cleanArchDoc)
	writeLayerFixture(t, tmp, layerCoherenceReadmePath, cleanReadme)

	findings, err := AuditLayerCoherence(tmp)
	if err != nil {
		t.Fatalf("AuditLayerCoherence: %v", err)
	}
	if len(findings) != 0 {
		t.Errorf("expected zero findings on clean repo, got %d: %+v", len(findings), findings)
	}
}

func TestAuditLayerCoherence_NumberingGap(t *testing.T) {
	tmp := t.TempDir()
	// Both docs agree on 0, 1, 3 (no 2). Same names so the only failure
	// signal is the numbering gap.
	arch := `# Arch

## Layer 0: Vision (purpose)
## Layer 1: Principles (values)
## Layer 3: Development (practices)
`
	readme := `# README

- **Layer 0: Vision** - x
- **Layer 1: Principles** - x
- **Layer 3: Development** - x
`
	writeLayerFixture(t, tmp, layerCoherenceArchPath, arch)
	writeLayerFixture(t, tmp, layerCoherenceReadmePath, readme)

	findings, err := AuditLayerCoherence(tmp)
	if err != nil {
		t.Fatalf("AuditLayerCoherence: %v", err)
	}
	var gapFound bool
	for _, f := range findings {
		if f.Kind == LayerCoherenceNumberingGap && strings.Contains(f.Message, "Layer 2") {
			gapFound = true
		}
	}
	if !gapFound {
		t.Errorf("expected numbering-gap finding for Layer 2, got: %+v", findings)
	}
}

func TestAuditLayerCoherence_CrossFileNameMismatch(t *testing.T) {
	tmp := t.TempDir()
	arch := `# Arch

## Layer 0: Vision (a)
## Layer 1: Principles (b)
`
	readme := `# README

- **Layer 0: Vision** - x
- **Layer 1: Practices** - x
`
	writeLayerFixture(t, tmp, layerCoherenceArchPath, arch)
	writeLayerFixture(t, tmp, layerCoherenceReadmePath, readme)

	findings, err := AuditLayerCoherence(tmp)
	if err != nil {
		t.Fatalf("AuditLayerCoherence: %v", err)
	}
	var mismatchFound bool
	for _, f := range findings {
		if f.Kind == LayerCoherenceCrossFileNameMismatch && strings.Contains(f.Message, "Layer 1") {
			mismatchFound = true
		}
	}
	if !mismatchFound {
		t.Errorf("expected cross-file-name-mismatch for Layer 1, got: %+v", findings)
	}
}

func TestAuditLayerCoherence_CrossFileNumberMismatch(t *testing.T) {
	tmp := t.TempDir()
	arch := `# Arch

## Layer 0: Vision (a)
## Layer 1: Principles (b)
## Layer 2: Conventions (c)
`
	readme := `# README

- **Layer 0: Vision** - x
- **Layer 1: Principles** - x
`
	writeLayerFixture(t, tmp, layerCoherenceArchPath, arch)
	writeLayerFixture(t, tmp, layerCoherenceReadmePath, readme)

	findings, err := AuditLayerCoherence(tmp)
	if err != nil {
		t.Fatalf("AuditLayerCoherence: %v", err)
	}
	var numMismatchFound bool
	for _, f := range findings {
		if f.Kind == LayerCoherenceCrossFileNumberMismatch && strings.Contains(f.Message, "Layer 2") {
			numMismatchFound = true
		}
	}
	if !numMismatchFound {
		t.Errorf("expected cross-file-number-mismatch for Layer 2, got: %+v", findings)
	}
}

func TestAuditLayerCoherence_IntraFileNameConflict(t *testing.T) {
	tmp := t.TempDir()
	// Same number declared twice in one file with different names.
	arch := `# Arch

## Layer 0: Vision (a)
## Layer 0: Mission (a)
`
	writeLayerFixture(t, tmp, layerCoherenceArchPath, arch)
	writeLayerFixture(t, tmp, layerCoherenceReadmePath, `# r

- **Layer 0: Vision** - x
`)

	findings, err := AuditLayerCoherence(tmp)
	if err != nil {
		t.Fatalf("AuditLayerCoherence: %v", err)
	}
	var conflictFound bool
	for _, f := range findings {
		if f.Kind == LayerCoherenceIntraFileNameConflict {
			conflictFound = true
		}
	}
	if !conflictFound {
		t.Errorf("expected intra-file-name-conflict finding, got: %+v", findings)
	}
}

func TestAuditLayerCoherence_MissingArchitectureDoc(t *testing.T) {
	tmp := t.TempDir()
	// Only README; architecture doc missing.
	writeLayerFixture(t, tmp, layerCoherenceReadmePath, cleanReadme)

	findings, err := AuditLayerCoherence(tmp)
	if err != nil {
		t.Fatalf("AuditLayerCoherence: %v", err)
	}
	var missingFound bool
	for _, f := range findings {
		if f.Kind == LayerCoherenceMissingDoc && strings.Contains(f.File, "repository-governance-architecture.md") {
			missingFound = true
		}
	}
	if !missingFound {
		t.Errorf("expected missing-doc finding for architecture doc, got: %+v", findings)
	}
}

func TestAuditLayerCoherence_MissingReadme(t *testing.T) {
	tmp := t.TempDir()
	writeLayerFixture(t, tmp, layerCoherenceArchPath, cleanArchDoc)

	findings, err := AuditLayerCoherence(tmp)
	if err != nil {
		t.Fatalf("AuditLayerCoherence: %v", err)
	}
	var missingFound bool
	for _, f := range findings {
		if f.Kind == LayerCoherenceMissingDoc && strings.HasSuffix(f.File, "repo-governance/README.md") {
			missingFound = true
		}
	}
	if !missingFound {
		t.Errorf("expected missing-doc finding for README, got: %+v", findings)
	}
}

func TestAuditLayerCoherence_BothDocsMissing(t *testing.T) {
	tmp := t.TempDir()

	findings, err := AuditLayerCoherence(tmp)
	if err != nil {
		t.Fatalf("AuditLayerCoherence: %v", err)
	}
	if len(findings) != 2 {
		t.Errorf("expected exactly two missing-doc findings, got %d: %+v", len(findings), findings)
	}
	for _, f := range findings {
		if f.Kind != LayerCoherenceMissingDoc {
			t.Errorf("expected only missing-doc findings when both docs absent, got: %+v", f)
		}
	}
}

func TestAuditLayerCoherence_DeterministicOrdering(t *testing.T) {
	tmp := t.TempDir()
	// Construct a fixture that yields multiple findings of different kinds
	// so we can assert that AuditLayerCoherence returns them sorted by
	// (File, Kind).
	arch := `# Arch

## Layer 0: Vision (a)
## Layer 0: Mission (a)
## Layer 1: Principles (b)
`
	readme := `# README

- **Layer 0: Vision** - x
- **Layer 2: Conventions** - y
`
	writeLayerFixture(t, tmp, layerCoherenceArchPath, arch)
	writeLayerFixture(t, tmp, layerCoherenceReadmePath, readme)

	findings, err := AuditLayerCoherence(tmp)
	if err != nil {
		t.Fatalf("AuditLayerCoherence: %v", err)
	}
	// Verify sort by (File, Kind).
	for i := 1; i < len(findings); i++ {
		prev, cur := findings[i-1], findings[i]
		if prev.File > cur.File {
			t.Errorf("findings not sorted by File: %q > %q", prev.File, cur.File)
		} else if prev.File == cur.File && prev.Kind > cur.Kind {
			t.Errorf("findings not sorted by Kind within File %q: %q > %q", prev.File, prev.Kind, cur.Kind)
		}
	}
}

func TestAuditLayerCoherence_BoldFormHonored(t *testing.T) {
	// Sanity check that the bold-span regex captures cleanly regardless of
	// surrounding line decoration (list bullets, ordered list digits, leading
	// whitespace). The bold regex intentionally is not line-anchored — the
	// canonical README form uses `- **Layer N: Name**` as a list item.
	tmp := t.TempDir()
	arch := "# Arch\n\n## Layer 0: Vision (a)\n## Layer 1: Principles (b)\n"
	readme := "# README\n\n- **Layer 0: Vision** with description\n  - nested **Layer 1: Principles** inline\n"
	writeLayerFixture(t, tmp, layerCoherenceArchPath, arch)
	writeLayerFixture(t, tmp, layerCoherenceReadmePath, readme)

	findings, err := AuditLayerCoherence(tmp)
	if err != nil {
		t.Fatalf("AuditLayerCoherence: %v", err)
	}
	if len(findings) != 0 {
		t.Errorf("expected zero findings when bold-span regex captures non-anchored layer entries, got: %+v", findings)
	}
}

func TestAuditLayerCoherence_UnreadableArchitectureDoc(t *testing.T) {
	tmp := t.TempDir()
	// Create the architecture path as a directory so os.ReadFile returns a
	// non-ErrNotExist error and the audit propagates it via the error return.
	archDir := filepath.Join(tmp, layerCoherenceArchPath)
	if err := os.MkdirAll(archDir, 0o755); err != nil {
		t.Fatal(err)
	}
	writeLayerFixture(t, tmp, layerCoherenceReadmePath, cleanReadme)

	_, err := AuditLayerCoherence(tmp)
	if err == nil {
		t.Fatal("expected error when architecture path is a directory, got nil")
	}
	if !strings.Contains(err.Error(), "read") {
		t.Errorf("expected wrapped read error, got: %v", err)
	}
}

func TestAuditLayerCoherence_UnreadableReadme(t *testing.T) {
	tmp := t.TempDir()
	writeLayerFixture(t, tmp, layerCoherenceArchPath, cleanArchDoc)
	// README created as a directory to force a non-ErrNotExist read error.
	readmeDir := filepath.Join(tmp, layerCoherenceReadmePath)
	if err := os.MkdirAll(readmeDir, 0o755); err != nil {
		t.Fatal(err)
	}

	_, err := AuditLayerCoherence(tmp)
	if err == nil {
		t.Fatal("expected error when README path is a directory, got nil")
	}
	if !strings.Contains(err.Error(), "read") {
		t.Errorf("expected wrapped read error, got: %v", err)
	}
}

func TestAuditLayerCoherence_LayersOnlyInReadme(t *testing.T) {
	// Architecture doc declares no layers; README declares 0..2. Exercises
	// the cross-file-number-mismatch branch where the layer is missing from
	// the architecture map.
	tmp := t.TempDir()
	arch := "# Arch\n\nNo layer declarations here.\n"
	readme := "# README\n\n- **Layer 0: Vision** - x\n- **Layer 1: Principles** - x\n- **Layer 2: Conventions** - x\n"
	writeLayerFixture(t, tmp, layerCoherenceArchPath, arch)
	writeLayerFixture(t, tmp, layerCoherenceReadmePath, readme)

	findings, err := AuditLayerCoherence(tmp)
	if err != nil {
		t.Fatalf("AuditLayerCoherence: %v", err)
	}
	var mismatchCount int
	for _, f := range findings {
		if f.Kind == LayerCoherenceCrossFileNumberMismatch {
			mismatchCount++
		}
	}
	if mismatchCount < 3 {
		t.Errorf("expected ≥3 cross-file-number-mismatch findings (one per README-only layer), got %d: %+v",
			mismatchCount, findings)
	}
}

func TestAuditLayerCoherence_NoGapWhenContiguous(t *testing.T) {
	// Tight contiguous range 0..2 — should not yield any numbering-gap
	// findings (covers the loop where no missing integer is found).
	tmp := t.TempDir()
	arch := `# Arch

## Layer 0: Vision (a)
## Layer 1: Principles (b)
## Layer 2: Conventions (c)
`
	readme := `# README

- **Layer 0: Vision** - x
- **Layer 1: Principles** - x
- **Layer 2: Conventions** - x
`
	writeLayerFixture(t, tmp, layerCoherenceArchPath, arch)
	writeLayerFixture(t, tmp, layerCoherenceReadmePath, readme)

	findings, err := AuditLayerCoherence(tmp)
	if err != nil {
		t.Fatalf("AuditLayerCoherence: %v", err)
	}
	for _, f := range findings {
		if f.Kind == LayerCoherenceNumberingGap {
			t.Errorf("expected no numbering-gap findings on contiguous 0..2, got: %+v", f)
		}
	}
}

func TestAuditLayerCoherence_DuplicateSameName(t *testing.T) {
	// Same number declared twice in one file with the SAME name should
	// collapse silently and produce no intra-file conflict.
	tmp := t.TempDir()
	arch := `# Arch

## Layer 0: Vision (one)
## Layer 0: Vision (two)
## Layer 1: Principles (b)
`
	readme := `# README

- **Layer 0: Vision** - x
- **Layer 1: Principles** - x
`
	writeLayerFixture(t, tmp, layerCoherenceArchPath, arch)
	writeLayerFixture(t, tmp, layerCoherenceReadmePath, readme)

	findings, err := AuditLayerCoherence(tmp)
	if err != nil {
		t.Fatalf("AuditLayerCoherence: %v", err)
	}
	for _, f := range findings {
		if f.Kind == LayerCoherenceIntraFileNameConflict {
			t.Errorf("expected no intra-file-name-conflict on duplicate-same-name, got: %+v", f)
		}
	}
}
