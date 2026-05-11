package cmd

import (
	"bytes"
	"encoding/json"
	"os"
	"path/filepath"
	"testing"

	governance "github.com/wahidyankf/ose-public/apps/rhino-cli/internal/repo-governance"
)

// TestGovernanceAuditGolden seeds a deterministic AuditEnvelope, invokes the
// JSON branch of governance_audit, normalizes timestamps with the shared
// golden helper, and compares against a persisted golden file. The fixture
// pins both git_sha and ran_at so the comparison is byte-stable.
//
// Seed via:
//
//	UPDATE_GOLDEN=1 go test ./cmd -run TestGovernanceAuditGolden
func TestGovernanceAuditGolden(t *testing.T) {
	origGetwd := osGetwd
	origStat := osStat
	origFn := runAuditFn
	defer func() {
		osGetwd = origGetwd
		osStat = origStat
		runAuditFn = origFn
		output = "text"
		_ = parseOutputFormat(nil, nil)
		governanceAuditSkip = nil
		governanceAuditIncludeOnly = nil
	}()

	osGetwd = func() (string, error) { return "/mock-repo", nil }
	osStat = func(name string) (os.FileInfo, error) {
		if name == "/mock-repo/.git" {
			return &mockFileInfo{name: ".git", isDir: true}, nil
		}
		return nil, os.ErrNotExist
	}

	runAuditFn = func(_ governance.AuditOptions) (governance.AuditEnvelope, error) {
		return goldenFixtureEnvelope(), nil
	}

	output = "json"
	_ = parseOutputFormat(nil, nil)

	buf := new(bytes.Buffer)
	governanceAuditCmd.SetOut(buf)
	governanceAuditCmd.SetErr(buf)
	// We expect non-nil error because total_findings > 0; the JSON output is
	// still written before the error is returned.
	_ = governanceAuditCmd.RunE(governanceAuditCmd, []string{})

	got := normaliseGoldenOutput(buf.String())

	goldenFile := filepath.Join(goldenDir, "governance_audit_fixed.stdout")
	update := os.Getenv("UPDATE_GOLDEN") == "1"
	if update {
		if err := os.MkdirAll(goldenDir, 0o755); err != nil {
			t.Fatalf("golden: cannot create dir: %v", err)
		}
		if err := os.WriteFile(goldenFile, []byte(got), 0o644); err != nil {
			t.Fatalf("golden: cannot write %s: %v", goldenFile, err)
		}
		t.Logf("golden: updated %s", goldenFile)
		return
	}
	wantBytes, err := os.ReadFile(goldenFile) //nolint:gosec
	if err != nil {
		if os.IsNotExist(err) {
			t.Errorf("golden file %s does not exist — run UPDATE_GOLDEN=1 go test ./cmd -run TestGovernanceAuditGolden to seed it", goldenFile)
			return
		}
		t.Fatalf("golden: cannot read %s: %v", goldenFile, err)
	}
	want := string(wantBytes)
	if got != want {
		t.Errorf("golden output mismatch:\nGOT:\n%s\nWANT:\n%s", got, want)
	}

	// Independent sanity check: the output must be valid JSON and the schema
	// field must match the canonical identifier.
	var env map[string]any
	if err := json.Unmarshal(buf.Bytes(), &env); err != nil {
		t.Fatalf("golden output is not valid JSON: %v", err)
	}
	if env["schema"] != governance.AuditEnvelopeSchema {
		t.Errorf("schema field: got %v want %q", env["schema"], governance.AuditEnvelopeSchema)
	}
}

// goldenFixtureEnvelope returns the canonical envelope captured by the
// governance_audit golden test. The fixture covers passed and failed
// categories, a non-empty skipped-false-positives list, and stable map keys.
func goldenFixtureEnvelope() governance.AuditEnvelope {
	cats := []governance.AuditCategoryResult{
		{
			Name: "agents-md-size", Command: "repo-governance agents-md-size", Passed: false,
			Findings: []governance.AuditFinding{{
				Key:      "agents-md-size|AGENTS.md|deadbeef",
				Severity: "high", Criticality: "HIGH",
				File: "AGENTS.md", Line: 1,
				Message: "AGENTS.md exceeds hard limit",
			}},
		},
		{
			Name: "frontmatter-audit", Command: "repo-governance frontmatter-audit", Passed: true,
		},
		{
			Name: "traceability-audit", Command: "repo-governance traceability-audit", Passed: true,
		},
		{
			Name: "license-audit", Command: "repo-governance license-audit", Passed: true,
		},
		{
			Name: "readme-index-audit", Command: "repo-governance readme-index-audit", Passed: true,
		},
		{
			Name: "emoji-audit", Command: "repo-governance emoji-audit", Passed: true,
		},
		{
			Name: "layer-coherence", Command: "repo-governance layer-coherence", Passed: true,
		},
		{
			Name: "docs-validate-naming", Command: "docs validate-naming", Passed: true,
		},
		{
			Name: "docs-validate-frontmatter", Command: "docs validate-frontmatter", Passed: true,
		},
		{
			Name: "docs-validate-heading-hierarchy", Command: "docs validate-heading-hierarchy", Passed: true,
		},
		{
			Name: "agents-detect-duplication", Command: "agents detect-duplication", Passed: true,
		},
	}
	return governance.AuditEnvelope{
		Schema: governance.AuditEnvelopeSchema,
		Status: "failed",
		Result: governance.AuditResult{
			GitSHA:        "abc1234",
			RanAt:         "2026-05-12T00:00:00Z",
			TotalFindings: 1,
			BySeverity:    map[string]int{"high": 1},
			ByCategory: map[string]int{
				"agents-md-size":                  1,
				"frontmatter-audit":               0,
				"traceability-audit":              0,
				"license-audit":                   0,
				"readme-index-audit":              0,
				"emoji-audit":                     0,
				"layer-coherence":                 0,
				"docs-validate-naming":            0,
				"docs-validate-frontmatter":       0,
				"docs-validate-heading-hierarchy": 0,
				"agents-detect-duplication":       0,
			},
			Categories: cats,
			SkippedFalsePositives: []governance.AuditFinding{{
				Key:      "emoji-audit|apps/example.go|cafef00d",
				Severity: "high", Criticality: "HIGH",
				File: "apps/example.go", Line: 12,
				Message: "forbidden emoji codepoint U+1F600 at column 5",
			}},
		},
	}
}
