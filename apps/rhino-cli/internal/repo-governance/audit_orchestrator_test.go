package repogovernance

import (
	"encoding/json"
	"os"
	"path/filepath"
	"strings"
	"testing"
	"time"
)

// fixedNow returns a deterministic time function pinned to 2026-05-12T00:00:00Z.
func fixedNow() func() time.Time {
	t := time.Date(2026, time.May, 12, 0, 0, 0, 0, time.UTC)
	return func() time.Time { return t }
}

// newSyntheticRepo constructs a minimal repo skeleton that exercises every
// category without producing real findings on a clean run.
func newSyntheticRepo(t *testing.T) string {
	t.Helper()
	root := t.TempDir()

	// AGENTS.md within the 30 KB target.
	mustWrite(t, filepath.Join(root, "AGENTS.md"), strings.Repeat("a", 1000))

	// Minimal governance subtree — every dir empty so audits cleanly return
	// nothing. Each subtree must exist or the walk treats it as empty (which
	// is also fine here).
	for _, d := range []string{
		"repo-governance/principles",
		"repo-governance/conventions",
		"repo-governance/development",
		"repo-governance/workflows",
		"repo-governance/conventions/structure",
		"repo-governance/principles/foundation",
		"repo-governance/development/workflow",
		"repo-governance/workflows/meta",
		"docs/explanation/software-engineering",
		"docs",
		".claude/agents",
		".claude/skills",
		"plans",
		"generated-reports",
		"apps",
		"libs",
		"specs",
	} {
		mustMkdir(t, filepath.Join(root, d))
	}

	// LICENSE files for the LICENSE audit. Each required dir gets MIT.
	// Empty apps/ and libs/ mean no required LICENSE except specs/.
	mustWrite(t, filepath.Join(root, "specs", "LICENSE"), "MIT License\n\nCopyright (c) 2026\n")
	mustWrite(t, filepath.Join(root, "LICENSING-NOTICE.md"), `# Licensing

| Path | License |
| ---- | ------- |
| `+"`"+`specs`+"`"+` | MIT |
`)

	// repo-governance/README.md and architecture doc — layer-coherence needs
	// matching declarations 0..5.
	layerBlocks := strings.Join([]string{
		"**Layer 0: Vision**",
		"**Layer 1: Principles**",
		"**Layer 2: Conventions**",
		"**Layer 3: Development**",
		"**Layer 4: Agents**",
		"**Layer 5: Workflows**",
	}, "\n")
	mustWrite(t, filepath.Join(root, "repo-governance", "README.md"),
		"# Index\n\n"+layerBlocks+"\n\n- [Architecture](./repository-governance-architecture.md)\n")
	mustWrite(t, filepath.Join(root, "repo-governance", "repository-governance-architecture.md"),
		"# Architecture\n\n"+layerBlocks+"\n")

	return root
}

// TestRunAuditClean verifies a clean repo produces zero findings, status ok.
func TestRunAuditClean(t *testing.T) {
	root := newSyntheticRepo(t)
	env, err := RunAudit(AuditOptions{RepoRoot: root, Now: fixedNow()})
	if err != nil {
		t.Fatalf("RunAudit: %v", err)
	}
	if env.Schema != AuditEnvelopeSchema {
		t.Errorf("schema: got %q want %q", env.Schema, AuditEnvelopeSchema)
	}
	if env.Status != "ok" {
		t.Errorf("status: got %q want %q", env.Status, "ok")
	}
	if env.Result.TotalFindings != 0 {
		t.Errorf("TotalFindings: got %d want 0; findings: %#v", env.Result.TotalFindings, env.Result.Categories)
	}
	if len(env.Result.Categories) != len(AuditCategoryOrder) {
		t.Errorf("Categories: got %d want %d", len(env.Result.Categories), len(AuditCategoryOrder))
	}
	for i, want := range AuditCategoryOrder {
		if env.Result.Categories[i].Name != want {
			t.Errorf("Categories[%d]: got %q want %q", i, env.Result.Categories[i].Name, want)
		}
	}
}

// TestRunAuditCleanRanAtFormat verifies the RanAt field is RFC 3339 with UTC
// suffix when the injected clock returns UTC.
func TestRunAuditCleanRanAtFormat(t *testing.T) {
	root := newSyntheticRepo(t)
	env, err := RunAudit(AuditOptions{RepoRoot: root, Now: fixedNow()})
	if err != nil {
		t.Fatalf("RunAudit: %v", err)
	}
	if env.Result.RanAt != "2026-05-12T00:00:00Z" {
		t.Errorf("RanAt: got %q", env.Result.RanAt)
	}
}

// TestRunAuditAgentsMdSizeFailing forces an over-limit AGENTS.md and confirms
// the agents-md-size category produces a finding which feeds total_findings.
func TestRunAuditAgentsMdSizeFailing(t *testing.T) {
	root := newSyntheticRepo(t)
	// 50 KB > 40 KB hard limit.
	mustWrite(t, filepath.Join(root, "AGENTS.md"), strings.Repeat("x", 50000))

	env, err := RunAudit(AuditOptions{RepoRoot: root, Now: fixedNow()})
	if err != nil {
		t.Fatalf("RunAudit: %v", err)
	}
	if env.Status != "failed" {
		t.Errorf("status: got %q want failed", env.Status)
	}
	if env.Result.TotalFindings < 1 {
		t.Errorf("expected ≥1 finding, got %d", env.Result.TotalFindings)
	}
	if env.Result.ByCategory["agents-md-size"] < 1 {
		t.Errorf("ByCategory[agents-md-size]: got %d want ≥1", env.Result.ByCategory["agents-md-size"])
	}
	if env.Result.BySeverity["high"] < 1 {
		t.Errorf("BySeverity[high]: got %d want ≥1", env.Result.BySeverity["high"])
	}
}

// TestRunAuditSkipCategory verifies --skip removes a category from the run.
func TestRunAuditSkipCategory(t *testing.T) {
	root := newSyntheticRepo(t)
	env, err := RunAudit(AuditOptions{
		RepoRoot: root,
		Now:      fixedNow(),
		Skip:     []string{"agents-md-size"},
	})
	if err != nil {
		t.Fatalf("RunAudit: %v", err)
	}
	for _, c := range env.Result.Categories {
		if c.Name == "agents-md-size" {
			t.Errorf("agents-md-size should be skipped but appears in categories")
		}
	}
}

// TestRunAuditIncludeOnly verifies --include-category restricts the run.
func TestRunAuditIncludeOnly(t *testing.T) {
	root := newSyntheticRepo(t)
	env, err := RunAudit(AuditOptions{
		RepoRoot:    root,
		Now:         fixedNow(),
		IncludeOnly: []string{"agents-md-size"},
	})
	if err != nil {
		t.Fatalf("RunAudit: %v", err)
	}
	if len(env.Result.Categories) != 1 {
		t.Fatalf("Categories: got %d want 1", len(env.Result.Categories))
	}
	if env.Result.Categories[0].Name != "agents-md-size" {
		t.Errorf("expected only agents-md-size, got %q", env.Result.Categories[0].Name)
	}
}

// TestRunAuditSkipListPartitions verifies findings whose Key matches an entry
// in the skip-list move to SkippedFalsePositives and do NOT count toward
// TotalFindings.
func TestRunAuditSkipListPartitions(t *testing.T) {
	root := newSyntheticRepo(t)
	// Force an agents-md-size failure.
	mustWrite(t, filepath.Join(root, "AGENTS.md"), strings.Repeat("x", 50000))

	// First run: capture the produced Key so we can write a matching skip-list entry.
	first, err := RunAudit(AuditOptions{RepoRoot: root, Now: fixedNow()})
	if err != nil {
		t.Fatalf("RunAudit baseline: %v", err)
	}
	var key string
	for _, c := range first.Result.Categories {
		if c.Name != "agents-md-size" {
			continue
		}
		if len(c.Findings) == 0 {
			t.Fatalf("expected baseline finding to capture key")
		}
		key = c.Findings[0].Key
	}
	if key == "" {
		t.Fatalf("no key captured")
	}

	// Write the skip-list and rerun.
	skipPath := filepath.Join(root, "generated-reports", ".known-false-positives.md")
	mustWrite(t, skipPath, "# False positives\n\n- `"+key+"`\n")

	env, err := RunAudit(AuditOptions{RepoRoot: root, Now: fixedNow()})
	if err != nil {
		t.Fatalf("RunAudit with skip-list: %v", err)
	}
	if env.Result.TotalFindings != 0 {
		t.Errorf("expected TotalFindings=0 after skip, got %d", env.Result.TotalFindings)
	}
	if env.Status != "ok" {
		t.Errorf("expected status ok, got %q", env.Status)
	}
	if len(env.Result.SkippedFalsePositives) != 1 {
		t.Fatalf("expected 1 skipped finding, got %d", len(env.Result.SkippedFalsePositives))
	}
	if env.Result.SkippedFalsePositives[0].Key != key {
		t.Errorf("skipped key: got %q want %q", env.Result.SkippedFalsePositives[0].Key, key)
	}
}

// TestRunAuditByteDeterministic verifies that JSON output is byte-identical
// across 10 consecutive runs with a fixed clock against an unchanged repo.
func TestRunAuditByteDeterministic(t *testing.T) {
	root := newSyntheticRepo(t)
	var first []byte
	for i := range 10 {
		env, err := RunAudit(AuditOptions{RepoRoot: root, Now: fixedNow()})
		if err != nil {
			t.Fatalf("RunAudit iter %d: %v", i, err)
		}
		data, err := json.Marshal(env)
		if err != nil {
			t.Fatalf("Marshal iter %d: %v", i, err)
		}
		if i == 0 {
			first = data
			continue
		}
		if string(data) != string(first) {
			t.Fatalf("iter %d byte mismatch", i)
		}
	}
}

// TestRunAuditFindingsSortedByFileLineKey verifies the sortAuditFindings
// helper orders findings deterministically (File then Line then Key).
func TestRunAuditFindingsSortedByFileLineKey(t *testing.T) {
	in := []AuditFinding{
		{Key: "z", File: "b.md", Line: 1},
		{Key: "k2", File: "a.md", Line: 5},
		{Key: "k4", File: "a.md", Line: 1},
		{Key: "k3", File: "a.md", Line: 1},
	}
	sortAuditFindings(in)
	if in[0].File != "a.md" || in[0].Line != 1 || in[0].Key != "k3" {
		t.Errorf("expected a.md:1 k3 first, got %v", in[0])
	}
	if in[1].File != "a.md" || in[1].Line != 1 || in[1].Key != "k4" {
		t.Errorf("expected a.md:1 k4 second, got %v", in[1])
	}
	if in[2].File != "a.md" || in[2].Line != 5 {
		t.Errorf("expected a.md:5 third, got %v", in[2])
	}
	if in[3].File != "b.md" {
		t.Errorf("expected b.md last, got %v", in[3])
	}
}

// TestRunAuditUnknownCategorySilentlyIgnored verifies that a Skip name not in
// AuditCategoryOrder does not raise an error.
func TestRunAuditUnknownCategorySilentlyIgnored(t *testing.T) {
	root := newSyntheticRepo(t)
	_, err := RunAudit(AuditOptions{RepoRoot: root, Now: fixedNow(), Skip: []string{"not-a-category"}})
	if err != nil {
		t.Errorf("unknown skip name should not error: %v", err)
	}
}

// TestRunAuditEnvelopeStableJSONOrder verifies the JSON output is sorted by
// category in the canonical AuditCategoryOrder, regardless of insertion order.
func TestRunAuditEnvelopeStableJSONOrder(t *testing.T) {
	root := newSyntheticRepo(t)
	env, err := RunAudit(AuditOptions{RepoRoot: root, Now: fixedNow()})
	if err != nil {
		t.Fatalf("RunAudit: %v", err)
	}
	data, err := json.Marshal(env)
	if err != nil {
		t.Fatalf("Marshal: %v", err)
	}
	s := string(data)
	last := -1
	for _, name := range AuditCategoryOrder {
		i := strings.Index(s, `"name":"`+name+`"`)
		if i < 0 {
			t.Errorf("category %q not in output", name)
			continue
		}
		if i <= last {
			t.Errorf("category %q out of canonical order", name)
		}
		last = i
	}
}

// TestBuildAuditKeyDeterministic exercises the key derivation independent of
// the orchestrator pipeline.
func TestBuildAuditKeyDeterministic(t *testing.T) {
	a := buildAuditKey("cat", "f.md", "hello")
	b := buildAuditKey("cat", "f.md", "hello")
	if a != b {
		t.Errorf("expected stable key, got %q vs %q", a, b)
	}
	c := buildAuditKey("cat", "f.md", "different")
	if a == c {
		t.Errorf("expected different keys for different messages")
	}
	if !strings.HasPrefix(a, "cat|f.md|") {
		t.Errorf("key prefix wrong: %q", a)
	}
}

// TestLoadKnownFalsePositivesMissingOk verifies a missing skip-list file is
// not an error.
func TestLoadKnownFalsePositivesMissingOk(t *testing.T) {
	root := t.TempDir()
	set, err := loadKnownFalsePositives(AuditOptions{RepoRoot: root})
	if err != nil {
		t.Errorf("missing file should not error: %v", err)
	}
	if len(set) != 0 {
		t.Errorf("expected empty set, got %d entries", len(set))
	}
}

// TestLoadKnownFalsePositivesParsesBacktickedKeys verifies the markdown parser
// recovers backtick-spanned keys from bullet lines.
func TestLoadKnownFalsePositivesParsesBacktickedKeys(t *testing.T) {
	root := t.TempDir()
	mustMkdir(t, filepath.Join(root, "generated-reports"))
	mustWrite(t, filepath.Join(root, "generated-reports", ".known-false-positives.md"), `# Skip list

Random prose.

- `+"`first-key`"+`
  - `+"`indented-key`"+`
- text without a backtick span
- `+"`second-key`"+` with trailing prose
`)
	set, err := loadKnownFalsePositives(AuditOptions{RepoRoot: root})
	if err != nil {
		t.Fatalf("load: %v", err)
	}
	for _, want := range []string{"first-key", "indented-key", "second-key"} {
		if _, ok := set[want]; !ok {
			t.Errorf("expected %q in skip set, got: %v", want, set)
		}
	}
}

// TestRunAuditDuplicationProjection verifies the agents-detect-duplication
// category produces an AuditFinding whose File and Line come from the first
// cluster member and whose Message embeds the full file list.
func TestRunAuditDuplicationProjection(t *testing.T) {
	root := newSyntheticRepo(t)
	// Seed two agent files sharing 12 distinct prose lines so the detector
	// fires.
	shared := strings.Join([]string{
		"shared alpha line one.",
		"shared alpha line two.",
		"shared alpha line three.",
		"shared alpha line four.",
		"shared alpha line five.",
		"shared alpha line six.",
		"shared alpha line seven.",
		"shared alpha line eight.",
		"shared alpha line nine.",
		"shared alpha line ten.",
		"shared alpha line eleven.",
		"shared alpha line twelve.",
	}, "\n") + "\n"
	mustWrite(t, filepath.Join(root, ".claude", "agents", "alpha-maker.md"),
		"---\nname: alpha-maker\n---\nalpha pre.\n"+shared+"alpha post.\n")
	mustWrite(t, filepath.Join(root, ".claude", "agents", "beta-checker.md"),
		"---\nname: beta-checker\n---\nbeta pre.\n"+shared+"beta post.\n")

	env, err := RunAudit(AuditOptions{
		RepoRoot:    root,
		Now:         fixedNow(),
		IncludeOnly: []string{"agents-detect-duplication"},
	})
	if err != nil {
		t.Fatalf("RunAudit: %v", err)
	}
	if len(env.Result.Categories) != 1 {
		t.Fatalf("expected 1 category, got %d", len(env.Result.Categories))
	}
	cat := env.Result.Categories[0]
	if cat.Name != "agents-detect-duplication" {
		t.Fatalf("expected category agents-detect-duplication, got %q", cat.Name)
	}
	if len(cat.Findings) < 1 {
		t.Fatalf("expected ≥1 finding, got %d", len(cat.Findings))
	}
	f := cat.Findings[0]
	if !strings.HasSuffix(f.File, "alpha-maker.md") {
		t.Errorf("expected File to point at alpha-maker.md, got %q", f.File)
	}
	if f.Line == 0 {
		t.Errorf("expected non-zero Line, got %d", f.Line)
	}
	if !strings.Contains(f.Message, "alpha-maker.md") || !strings.Contains(f.Message, "beta-checker.md") {
		t.Errorf("expected Message to embed both file paths, got %q", f.Message)
	}
}

// mustWrite writes data to path or fails the test.
func mustWrite(t *testing.T, path, content string) {
	t.Helper()
	if err := os.MkdirAll(filepath.Dir(path), 0o755); err != nil {
		t.Fatalf("mkdir for %s: %v", path, err)
	}
	if err := os.WriteFile(path, []byte(content), 0o644); err != nil {
		t.Fatalf("write %s: %v", path, err)
	}
}

// mustMkdir creates dir or fails the test.
func mustMkdir(t *testing.T, dir string) {
	t.Helper()
	if err := os.MkdirAll(dir, 0o755); err != nil {
		t.Fatalf("mkdir %s: %v", dir, err)
	}
}
