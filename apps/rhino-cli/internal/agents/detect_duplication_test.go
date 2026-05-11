package agents

import (
	"os"
	"path/filepath"
	"strings"
	"testing"
)

// writeDupFile writes `content` to `rel` under `dir`, creating parent
// directories as needed. Test helper.
func writeDupFile(t *testing.T, dir, rel, content string) {
	t.Helper()
	full := filepath.Join(dir, rel)
	if err := os.MkdirAll(filepath.Dir(full), 0o755); err != nil {
		t.Fatalf("mkdir: %v", err)
	}
	if err := os.WriteFile(full, []byte(content), 0o644); err != nil {
		t.Fatalf("write: %v", err)
	}
}

// uniqueProse returns a deterministic block of N distinct non-heading
// prose lines suitable for filling around a shared duplicated window.
func uniqueProse(prefix string, n int) string {
	var sb strings.Builder
	for i := range n {
		sb.WriteString(prefix)
		sb.WriteString(" line ")
		sb.WriteString(itoa(i))
		sb.WriteByte('\n')
	}
	return sb.String()
}

// itoa returns the decimal string for non-negative i. Avoids strconv to
// keep this test helper inline with the existing pattern.
func itoa(i int) string {
	if i == 0 {
		return "0"
	}
	var digits []byte
	for i > 0 {
		digits = append([]byte{byte('0' + i%10)}, digits...)
		i /= 10
	}
	return string(digits)
}

// sharedBlock returns a 12-line block of distinct non-heading prose lines
// that two files can share to exercise the duplication detector.
func sharedBlock() string {
	lines := []string{
		"Shared paragraph alpha describing a concrete behaviour.",
		"Shared paragraph beta explaining an invariant.",
		"Shared paragraph gamma giving a worked example.",
		"Shared paragraph delta calling out an edge case.",
		"Shared paragraph epsilon noting a non-goal.",
		"Shared paragraph zeta restating the contract.",
		"Shared paragraph eta with a step-by-step list.",
		"Shared paragraph theta with a caveat to remember.",
		"Shared paragraph iota linking to a sibling document.",
		"Shared paragraph kappa with a footnote-style aside.",
		"Shared paragraph lambda summarising the outcome.",
		"Shared paragraph mu wrapping up the section.",
	}
	return strings.Join(lines, "\n") + "\n"
}

func TestDetectDuplication_CleanTree_NoFindings(t *testing.T) {
	tmp := t.TempDir()
	writeDupFile(t, tmp, ".claude/agents/alpha-maker.md", uniqueProse("alpha", 30))
	writeDupFile(t, tmp, ".claude/agents/beta-checker.md", uniqueProse("beta", 30))
	writeDupFile(t, tmp, ".claude/skills/gamma/SKILL.md", uniqueProse("gamma", 30))

	findings, err := DetectDuplication(tmp)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	if len(findings) != 0 {
		t.Fatalf("expected zero findings, got %d: %+v", len(findings), findings)
	}
}

func TestDetectDuplication_TwoAgentsShare12Lines_Reports(t *testing.T) {
	tmp := t.TempDir()
	shared := sharedBlock()
	writeDupFile(t, tmp, ".claude/agents/alpha-maker.md",
		uniqueProse("alpha-pre", 5)+shared+uniqueProse("alpha-post", 5))
	writeDupFile(t, tmp, ".claude/agents/beta-checker.md",
		uniqueProse("beta-pre", 8)+shared+uniqueProse("beta-post", 3))

	findings, err := DetectDuplication(tmp)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	if len(findings) == 0 {
		t.Fatalf("expected at least one finding, got zero")
	}
	// At least one cluster must include both agent paths.
	var matched bool
	for _, f := range findings {
		hasAlpha, hasBeta := false, false
		for _, p := range f.Files {
			if strings.HasSuffix(p, "alpha-maker.md") {
				hasAlpha = true
			}
			if strings.HasSuffix(p, "beta-checker.md") {
				hasBeta = true
			}
		}
		if hasAlpha && hasBeta {
			matched = true
			if f.WindowSize != DuplicationWindowSize {
				t.Errorf("expected WindowSize=%d, got %d", DuplicationWindowSize, f.WindowSize)
			}
			if f.Severity != "high" {
				t.Errorf("expected severity=high, got %q", f.Severity)
			}
			if len(f.StartLines) != len(f.Files) {
				t.Errorf("StartLines length %d != Files length %d", len(f.StartLines), len(f.Files))
			}
		}
	}
	if !matched {
		t.Fatalf("no finding linked alpha-maker.md and beta-checker.md; findings=%+v", findings)
	}
}

func TestDetectDuplication_AgentSkillDuplication_Reports(t *testing.T) {
	tmp := t.TempDir()
	shared := sharedBlock()
	writeDupFile(t, tmp, ".claude/agents/alpha-maker.md",
		uniqueProse("alpha-pre", 4)+shared+uniqueProse("alpha-post", 4))
	writeDupFile(t, tmp, ".claude/skills/shared-skill/SKILL.md",
		uniqueProse("skill-pre", 6)+shared+uniqueProse("skill-post", 6))

	findings, err := DetectDuplication(tmp)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	if len(findings) == 0 {
		t.Fatalf("expected at least one agent-skill cluster, got zero")
	}
	var matched bool
	for _, f := range findings {
		hasAgent, hasSkill := false, false
		for _, p := range f.Files {
			if strings.HasSuffix(p, "alpha-maker.md") {
				hasAgent = true
			}
			if strings.Contains(p, "shared-skill") && strings.HasSuffix(p, "SKILL.md") {
				hasSkill = true
			}
		}
		if hasAgent && hasSkill {
			matched = true
		}
	}
	if !matched {
		t.Fatalf("no finding linked agent and SKILL.md; findings=%+v", findings)
	}
}

func TestDetectDuplication_HeadingOnlyWindowExcluded(t *testing.T) {
	tmp := t.TempDir()
	headings := strings.Join([]string{
		"# Title",
		"## Section A",
		"### Subsection A.1",
		"#### Detail",
		"## Section B",
		"### Subsection B.1",
		"#### Detail",
		"## Section C",
		"### Subsection C.1",
		"#### Detail",
	}, "\n") + "\n"
	writeDupFile(t, tmp, ".claude/agents/alpha-maker.md",
		uniqueProse("alpha", 5)+headings+uniqueProse("alpha-post", 5))
	writeDupFile(t, tmp, ".claude/agents/beta-checker.md",
		uniqueProse("beta", 5)+headings+uniqueProse("beta-post", 5))

	findings, err := DetectDuplication(tmp)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	if len(findings) != 0 {
		t.Fatalf("expected zero findings (heading-only window must be excluded), got %d: %+v",
			len(findings), findings)
	}
}

func TestDetectDuplication_WhitespaceOnlyWindowExcluded(t *testing.T) {
	tmp := t.TempDir()
	blanks := strings.Repeat("\n", 20) // collapses to one blank but still excluded
	writeDupFile(t, tmp, ".claude/agents/alpha-maker.md",
		uniqueProse("alpha", 5)+blanks+uniqueProse("alpha-post", 5))
	writeDupFile(t, tmp, ".claude/agents/beta-checker.md",
		uniqueProse("beta", 5)+blanks+uniqueProse("beta-post", 5))

	findings, err := DetectDuplication(tmp)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	if len(findings) != 0 {
		t.Fatalf("expected zero findings (whitespace-only collapsed), got %d", len(findings))
	}
}

func TestDetectDuplication_FrontmatterStripped(t *testing.T) {
	tmp := t.TempDir()
	// Identical frontmatter, but bodies share no duplication. Without
	// stripping, the frontmatter alone would generate a duplication
	// finding; with stripping, none.
	fm := "---\nname: shared\ndescription: shared description for the file\nmodel: sonnet\ntools: [Read]\ncolor: blue\nfield_a: alpha\nfield_b: beta\nfield_c: gamma\nfield_d: delta\nfield_e: epsilon\n---\n"
	writeDupFile(t, tmp, ".claude/agents/alpha-maker.md", fm+uniqueProse("alpha", 30))
	writeDupFile(t, tmp, ".claude/agents/beta-checker.md", fm+uniqueProse("beta", 30))

	findings, err := DetectDuplication(tmp)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	if len(findings) != 0 {
		t.Fatalf("expected zero findings (frontmatter stripped), got %d: %+v", len(findings), findings)
	}
}

func TestDetectDuplication_ShortFileSkipped(t *testing.T) {
	tmp := t.TempDir()
	short := strings.Repeat("only one line\n", 5) // 5 lines < 10
	writeDupFile(t, tmp, ".claude/agents/alpha-maker.md", short)
	writeDupFile(t, tmp, ".claude/agents/beta-checker.md", short)

	findings, err := DetectDuplication(tmp)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	if len(findings) != 0 {
		t.Fatalf("expected zero findings (files too short for window), got %d", len(findings))
	}
}

func TestDetectDuplication_MissingDirsNoError(t *testing.T) {
	tmp := t.TempDir()
	// No .claude/agents or .claude/skills present.
	findings, err := DetectDuplication(tmp)
	if err != nil {
		t.Fatalf("missing dirs must not error: %v", err)
	}
	if len(findings) != 0 {
		t.Fatalf("expected zero findings, got %d", len(findings))
	}
}

func TestDetectDuplication_ReadmeAgentExcluded(t *testing.T) {
	tmp := t.TempDir()
	shared := sharedBlock()
	// README.md inside .claude/agents/ must be skipped entirely; even if
	// its body matches an agent, no cross-file cluster should form.
	writeDupFile(t, tmp, ".claude/agents/README.md", shared+uniqueProse("readme", 5))
	writeDupFile(t, tmp, ".claude/agents/alpha-maker.md", shared+uniqueProse("alpha", 5))

	findings, err := DetectDuplication(tmp)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	for _, f := range findings {
		for _, p := range f.Files {
			if strings.HasSuffix(p, "agents/README.md") {
				t.Fatalf("README.md must be excluded; finding included %q", p)
			}
		}
	}
}

func TestDetectDuplication_DeterministicSort(t *testing.T) {
	tmp := t.TempDir()
	shared := sharedBlock()
	// Three files all sharing the same block — ensures cluster sort is
	// stable and the first reported finding uses the lexicographically
	// smallest file path.
	writeDupFile(t, tmp, ".claude/agents/zeta-maker.md",
		uniqueProse("zeta", 3)+shared+uniqueProse("zeta-post", 3))
	writeDupFile(t, tmp, ".claude/agents/alpha-maker.md",
		uniqueProse("alpha", 5)+shared+uniqueProse("alpha-post", 5))
	writeDupFile(t, tmp, ".claude/agents/middle-fixer.md",
		uniqueProse("middle", 7)+shared+uniqueProse("middle-post", 7))

	findings, err := DetectDuplication(tmp)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	if len(findings) == 0 {
		t.Fatalf("expected at least one finding")
	}
	for _, f := range findings {
		// Each cluster's Files slice must be sorted ascending.
		for i := 1; i < len(f.Files); i++ {
			if f.Files[i-1] > f.Files[i] {
				t.Errorf("cluster files not sorted: %v", f.Files)
			}
		}
		if len(f.StartLines) != len(f.Files) {
			t.Errorf("StartLines length %d != Files length %d", len(f.StartLines), len(f.Files))
		}
	}
}

func TestStripFrontmatter_NoFrontmatter(t *testing.T) {
	s := "hello\nworld\n"
	if got := stripFrontmatter(s); got != s {
		t.Errorf("expected unchanged, got %q", got)
	}
}

func TestStripFrontmatter_Present(t *testing.T) {
	s := "---\nname: a\nmodel: b\n---\nbody line one\nbody line two\n"
	got := stripFrontmatter(s)
	want := "body line one\nbody line two\n"
	if got != want {
		t.Errorf("got %q, want %q", got, want)
	}
}

func TestStripFrontmatter_CRLF(t *testing.T) {
	s := "---\r\nname: a\r\n---\r\nbody\r\n"
	got := stripFrontmatter(s)
	if !strings.Contains(got, "body") {
		t.Errorf("expected body retained, got %q", got)
	}
	if strings.Contains(got, "name: a") {
		t.Errorf("frontmatter not stripped: %q", got)
	}
}

func TestStripFrontmatter_NoClosingFence(t *testing.T) {
	// Missing closing fence — treat as no frontmatter.
	s := "---\nname: a\nmodel: b\nbody line\n"
	if got := stripFrontmatter(s); got != s {
		t.Errorf("expected unchanged when closing fence missing, got %q", got)
	}
}

func TestNormalizeLines_CollapsesBlanks(t *testing.T) {
	s := "alpha\n\n\n\nbeta\n"
	lines := normalizeLines(s)
	// Expect: alpha, blank, beta, blank (trailing newline produces a blank).
	if len(lines) != 4 {
		t.Fatalf("expected 4 normalized lines, got %d: %q", len(lines), lines)
	}
	if lines[0] != "alpha" || lines[2] != "beta" {
		t.Errorf("unexpected content: %q", lines)
	}
}

func TestNormalizeLines_TrimsTrailingWhitespace(t *testing.T) {
	s := "alpha   \t\nbeta\t \n"
	lines := normalizeLines(s)
	if lines[0] != "alpha" || lines[1] != "beta" {
		t.Errorf("expected trailing spaces trimmed, got %q", lines)
	}
}

func TestIsExcludedWindow_AllBlank(t *testing.T) {
	w := make([]string, DuplicationWindowSize)
	if !isExcludedWindow(w) {
		t.Error("all-blank window should be excluded")
	}
}

func TestIsExcludedWindow_AllHeadings(t *testing.T) {
	w := []string{"#", "## a", "### b", "#### c", "# d", "## e", "### f", "# g", "## h", "### i"}
	if !isExcludedWindow(w) {
		t.Error("all-heading window should be excluded")
	}
}

func TestIsExcludedWindow_MixedExcluded(t *testing.T) {
	// Blank + heading lines only — still excluded.
	w := []string{"", "# a", "", "## b", "", "### c", "", "# d", "", "## e"}
	if !isExcludedWindow(w) {
		t.Error("blank-plus-heading window should be excluded")
	}
}

func TestIsExcludedWindow_ProseRetained(t *testing.T) {
	w := []string{"# a", "## b", "prose line", "", "### c", "", "# d", "", "## e", "### f"}
	if isExcludedWindow(w) {
		t.Error("window containing prose line should NOT be excluded")
	}
}

func TestHashWindow_Deterministic(t *testing.T) {
	w := []string{"alpha", "beta", "gamma", "delta", "epsilon", "zeta", "eta", "theta", "iota", "kappa"}
	a := hashWindow(w)
	b := hashWindow(w)
	if a != b {
		t.Errorf("hashWindow not deterministic: %q vs %q", a, b)
	}
	if len(a) != 64 {
		t.Errorf("expected 64-char hex SHA-256, got %d chars", len(a))
	}
}

func TestEnumerateAgentAndSkillFiles_Sorted(t *testing.T) {
	tmp := t.TempDir()
	writeDupFile(t, tmp, ".claude/agents/zeta-maker.md", "x")
	writeDupFile(t, tmp, ".claude/agents/alpha-maker.md", "x")
	writeDupFile(t, tmp, ".claude/skills/beta/SKILL.md", "x")
	files, err := enumerateAgentAndSkillFiles(tmp)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	for i := 1; i < len(files); i++ {
		if files[i-1] > files[i] {
			t.Errorf("files not sorted: %v", files)
		}
	}
	if len(files) != 3 {
		t.Errorf("expected 3 files, got %d: %v", len(files), files)
	}
}
