package repogovernance

import (
	"os"
	"path/filepath"
	"strings"
	"testing"
)

// writeFile is a small test helper that creates parent directories as needed
// and writes content to path with 0o644 permissions.
func writeTraceabilityFile(t *testing.T, path, content string) {
	t.Helper()
	if err := os.MkdirAll(filepath.Dir(path), 0o755); err != nil {
		t.Fatalf("mkdir %s: %v", filepath.Dir(path), err)
	}
	if err := os.WriteFile(path, []byte(content), 0o644); err != nil {
		t.Fatalf("write %s: %v", path, err)
	}
}

// principleBody is a valid principle document carrying the "## Vision Supported" heading.
const principleBody = `# Sample Principle

Body.

## Vision Supported

Links here.
`

// conventionBody is a valid convention document carrying the
// "## Principles Implemented/Respected" heading.
const conventionBody = `# Sample Convention

Body.

## Principles Implemented/Respected

- Some principle.
`

// developmentBody is a valid development doc carrying BOTH required headings.
const developmentBody = `# Sample Development Doc

Body.

## Principles Implemented/Respected

- Some principle.

## Conventions Implemented/Respected

- Some convention.
`

// workflowBody references one agent file under .claude/agents/.
const workflowBody = `# Sample Workflow

Invokes [agent](../../.claude/agents/sample-maker.md).
`

func TestAuditTraceability_CleanRepo(t *testing.T) {
	tmp := t.TempDir()

	writeTraceabilityFile(t, filepath.Join(tmp, "repo-governance", "principles", "general", "sample.md"), principleBody)
	writeTraceabilityFile(t, filepath.Join(tmp, "repo-governance", "principles", "general", "README.md"), "# index\n")
	writeTraceabilityFile(t, filepath.Join(tmp, "repo-governance", "conventions", "structure", "sample.md"), conventionBody)
	writeTraceabilityFile(t, filepath.Join(tmp, "repo-governance", "development", "workflow", "sample.md"), developmentBody)
	writeTraceabilityFile(t, filepath.Join(tmp, "repo-governance", "workflows", "plan", "sample-execution.md"), workflowBody)

	findings, err := AuditTraceability(tmp)
	if err != nil {
		t.Fatalf("AuditTraceability: %v", err)
	}
	if len(findings) != 0 {
		t.Fatalf("expected zero findings on clean repo, got %d: %+v", len(findings), findings)
	}
}

func TestAuditTraceability_PrincipleMissingVisionSupported(t *testing.T) {
	tmp := t.TempDir()

	bad := filepath.Join(tmp, "repo-governance", "principles", "general", "bad.md")
	writeTraceabilityFile(t, bad, "# Bad Principle\n\nNo vision section.\n")

	findings, err := AuditTraceability(tmp)
	if err != nil {
		t.Fatalf("AuditTraceability: %v", err)
	}
	if len(findings) != 1 {
		t.Fatalf("expected exactly 1 finding, got %d: %+v", len(findings), findings)
	}
	if findings[0].Kind != TraceabilityMissingVisionSupported {
		t.Errorf("expected kind %q, got %q", TraceabilityMissingVisionSupported, findings[0].Kind)
	}
	if findings[0].Path != bad {
		t.Errorf("expected path %q, got %q", bad, findings[0].Path)
	}
	if findings[0].Line != 1 {
		t.Errorf("expected Line 1, got %d", findings[0].Line)
	}
	if !strings.Contains(findings[0].Message, "Vision Supported") {
		t.Errorf("expected message to mention Vision Supported, got: %q", findings[0].Message)
	}
}

func TestAuditTraceability_ConventionMissingPrinciplesImplemented(t *testing.T) {
	tmp := t.TempDir()

	bad := filepath.Join(tmp, "repo-governance", "conventions", "structure", "bad.md")
	writeTraceabilityFile(t, bad, "# Bad Convention\n\nNo principles section.\n")

	findings, err := AuditTraceability(tmp)
	if err != nil {
		t.Fatalf("AuditTraceability: %v", err)
	}
	if len(findings) != 1 {
		t.Fatalf("expected exactly 1 finding, got %d: %+v", len(findings), findings)
	}
	if findings[0].Kind != TraceabilityMissingPrinciplesImplemented {
		t.Errorf("expected kind %q, got %q", TraceabilityMissingPrinciplesImplemented, findings[0].Kind)
	}
	if !strings.Contains(findings[0].Message, "convention") {
		t.Errorf("expected convention in message, got: %q", findings[0].Message)
	}
}

func TestAuditTraceability_DevelopmentMissingBoth(t *testing.T) {
	tmp := t.TempDir()

	bad := filepath.Join(tmp, "repo-governance", "development", "workflow", "bad.md")
	writeTraceabilityFile(t, bad, "# Bad Dev Doc\n\nNeither section.\n")

	findings, err := AuditTraceability(tmp)
	if err != nil {
		t.Fatalf("AuditTraceability: %v", err)
	}
	if len(findings) != 2 {
		t.Fatalf("expected 2 findings (principles + conventions missing), got %d: %+v", len(findings), findings)
	}
	kinds := map[string]bool{}
	for _, f := range findings {
		kinds[f.Kind] = true
	}
	if !kinds[TraceabilityMissingPrinciplesImplemented] {
		t.Errorf("expected %q in finding kinds: %+v", TraceabilityMissingPrinciplesImplemented, kinds)
	}
	if !kinds[TraceabilityMissingConventionsImplemented] {
		t.Errorf("expected %q in finding kinds: %+v", TraceabilityMissingConventionsImplemented, kinds)
	}
}

func TestAuditTraceability_DevelopmentMissingOnlyConventions(t *testing.T) {
	tmp := t.TempDir()

	bad := filepath.Join(tmp, "repo-governance", "development", "workflow", "bad.md")
	writeTraceabilityFile(t, bad, "# Bad Dev Doc\n\n## Principles Implemented/Respected\n\nOK.\n")

	findings, err := AuditTraceability(tmp)
	if err != nil {
		t.Fatalf("AuditTraceability: %v", err)
	}
	if len(findings) != 1 {
		t.Fatalf("expected 1 finding (only conventions missing), got %d: %+v", len(findings), findings)
	}
	if findings[0].Kind != TraceabilityMissingConventionsImplemented {
		t.Errorf("expected %q, got %q", TraceabilityMissingConventionsImplemented, findings[0].Kind)
	}
}

func TestAuditTraceability_WorkflowMissingAgentReference(t *testing.T) {
	tmp := t.TempDir()

	bad := filepath.Join(tmp, "repo-governance", "workflows", "plan", "no-agent-execution.md")
	writeTraceabilityFile(t, bad, "# Bad Workflow\n\nNo agent reference here.\n")

	findings, err := AuditTraceability(tmp)
	if err != nil {
		t.Fatalf("AuditTraceability: %v", err)
	}
	if len(findings) != 1 {
		t.Fatalf("expected 1 finding, got %d: %+v", len(findings), findings)
	}
	if findings[0].Kind != TraceabilityMissingAgentReference {
		t.Errorf("expected %q, got %q", TraceabilityMissingAgentReference, findings[0].Kind)
	}
}

func TestAuditTraceability_WorkflowWithAgentReferencePasses(t *testing.T) {
	tmp := t.TempDir()

	good := filepath.Join(tmp, "repo-governance", "workflows", "plan", "plan-execution.md")
	writeTraceabilityFile(t, good, "# Workflow\n\nSee `.claude/agents/plan-maker.md`.\n")

	findings, err := AuditTraceability(tmp)
	if err != nil {
		t.Fatalf("AuditTraceability: %v", err)
	}
	if len(findings) != 0 {
		t.Fatalf("expected zero findings, got %d: %+v", len(findings), findings)
	}
}

func TestAuditTraceability_ReadmesAreExempt(t *testing.T) {
	tmp := t.TempDir()

	// README files with NONE of the required sections should not produce findings.
	writeTraceabilityFile(t, filepath.Join(tmp, "repo-governance", "principles", "README.md"), "# index\n")
	writeTraceabilityFile(t, filepath.Join(tmp, "repo-governance", "principles", "general", "README.md"), "# index\n")
	writeTraceabilityFile(t, filepath.Join(tmp, "repo-governance", "conventions", "README.md"), "# index\n")
	writeTraceabilityFile(t, filepath.Join(tmp, "repo-governance", "conventions", "structure", "README.md"), "# index\n")
	writeTraceabilityFile(t, filepath.Join(tmp, "repo-governance", "development", "README.md"), "# index\n")
	writeTraceabilityFile(t, filepath.Join(tmp, "repo-governance", "development", "workflow", "README.md"), "# index\n")
	writeTraceabilityFile(t, filepath.Join(tmp, "repo-governance", "workflows", "README.md"), "# index\n")
	writeTraceabilityFile(t, filepath.Join(tmp, "repo-governance", "workflows", "plan", "README.md"), "# index\n")

	findings, err := AuditTraceability(tmp)
	if err != nil {
		t.Fatalf("AuditTraceability: %v", err)
	}
	if len(findings) != 0 {
		t.Fatalf("expected zero findings — README files are exempt — got %d: %+v", len(findings), findings)
	}
}

func TestAuditTraceability_MissingSubtreesAreOK(t *testing.T) {
	tmp := t.TempDir()
	// No repo-governance directories at all.
	findings, err := AuditTraceability(tmp)
	if err != nil {
		t.Fatalf("AuditTraceability: %v", err)
	}
	if len(findings) != 0 {
		t.Fatalf("expected zero findings on empty repo, got %d: %+v", len(findings), findings)
	}
}

func TestAuditTraceability_FindingsSortedByPath(t *testing.T) {
	tmp := t.TempDir()

	// Create two principle files in non-alphabetical write order so we can
	// verify sort.SliceStable orders them deterministically.
	writeTraceabilityFile(t, filepath.Join(tmp, "repo-governance", "principles", "general", "z-bad.md"), "# z\n")
	writeTraceabilityFile(t, filepath.Join(tmp, "repo-governance", "principles", "general", "a-bad.md"), "# a\n")

	findings, err := AuditTraceability(tmp)
	if err != nil {
		t.Fatalf("AuditTraceability: %v", err)
	}
	if len(findings) != 2 {
		t.Fatalf("expected 2 findings, got %d", len(findings))
	}
	if !strings.HasSuffix(findings[0].Path, "a-bad.md") {
		t.Errorf("expected a-bad.md first, got: %s", findings[0].Path)
	}
	if !strings.HasSuffix(findings[1].Path, "z-bad.md") {
		t.Errorf("expected z-bad.md second, got: %s", findings[1].Path)
	}
}

func TestAuditTraceability_AgentRegexNotMatchedByPlainText(t *testing.T) {
	tmp := t.TempDir()
	// "Claude agents" in plain prose is NOT a path reference.
	bad := filepath.Join(tmp, "repo-governance", "workflows", "plan", "bad-execution.md")
	writeTraceabilityFile(t, bad, "# Workflow\n\nClaude agents help here. Maybe .claude/agents/.\n")

	findings, err := AuditTraceability(tmp)
	if err != nil {
		t.Fatalf("AuditTraceability: %v", err)
	}
	if len(findings) != 1 {
		t.Fatalf("expected 1 finding (regex requires <name>.md suffix), got %d: %+v", len(findings), findings)
	}
}

func TestAuditTraceability_HeadingMustBeAtLineStart(t *testing.T) {
	tmp := t.TempDir()
	// "## Vision Supported" indented under another heading does not satisfy the audit.
	bad := filepath.Join(tmp, "repo-governance", "principles", "general", "indented.md")
	writeTraceabilityFile(t, bad, "# Principle\n\n  ## Vision Supported\n\nbody\n")

	findings, err := AuditTraceability(tmp)
	if err != nil {
		t.Fatalf("AuditTraceability: %v", err)
	}
	if len(findings) != 1 {
		t.Fatalf("expected 1 finding when heading is indented, got %d: %+v", len(findings), findings)
	}
}

// TestAuditTraceability_ReadFileErrorsPropagate ensures os.ReadFile errors
// from the four auditor functions bubble up. A file with mode 0 cannot be
// read by the current user.
func TestAuditTraceability_ReadFileErrorsPropagate(t *testing.T) {
	if os.Geteuid() == 0 {
		t.Skip("running as root makes unreadable files readable; skip")
	}

	cases := []struct{ subpath, body string }{
		{filepath.Join("repo-governance", "principles", "general", "bad.md"), "# x\n"},
		{filepath.Join("repo-governance", "conventions", "structure", "bad.md"), "# x\n"},
		{filepath.Join("repo-governance", "development", "workflow", "bad.md"), "# x\n"},
		{filepath.Join("repo-governance", "workflows", "plan", "bad-execution.md"), "# x\n"},
	}
	for _, c := range cases {
		t.Run(c.subpath, func(t *testing.T) {
			tmp := t.TempDir()
			path := filepath.Join(tmp, c.subpath)
			writeTraceabilityFile(t, path, c.body)
			// Make file unreadable so os.ReadFile returns a permission error.
			if err := os.Chmod(path, 0o000); err != nil {
				t.Fatalf("chmod: %v", err)
			}
			t.Cleanup(func() { _ = os.Chmod(path, 0o644) })

			if _, err := AuditTraceability(tmp); err == nil {
				t.Fatalf("expected read-error to surface, got nil")
			}
		})
	}
}

// TestListGovernanceMarkdown_WalkError ensures WalkDir errors propagate.
// Only os.ErrNotExist is silently swallowed; other walk errors must surface.
func TestListGovernanceMarkdown_WalkError(t *testing.T) {
	if os.Geteuid() == 0 {
		t.Skip("running as root makes unreadable dirs readable; skip")
	}
	tmp := t.TempDir()
	dir := filepath.Join(tmp, "repo-governance", "principles", "blocked")
	if err := os.MkdirAll(dir, 0o755); err != nil {
		t.Fatalf("mkdir: %v", err)
	}
	// Drop all permissions on the inner dir so WalkDir cannot read it.
	if err := os.Chmod(dir, 0o000); err != nil {
		t.Fatalf("chmod: %v", err)
	}
	t.Cleanup(func() { _ = os.Chmod(dir, 0o755) })

	_, err := listGovernanceMarkdown(filepath.Join(tmp, "repo-governance", "principles"))
	if err == nil {
		t.Skip("filesystem allowed reading the blocked dir on this host; skip")
	}
}

func TestFirstNonEmptyLine(t *testing.T) {
	tests := []struct {
		name string
		in   string
		want int
	}{
		{"empty", "", 1},
		{"all blank", "\n\n\n", 1},
		{"first line content", "hello\n", 1},
		{"second line content", "\nhello\n", 2},
		{"third line content", "\n\nhello\n", 3},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			if got := firstNonEmptyLine([]byte(tt.in)); got != tt.want {
				t.Errorf("firstNonEmptyLine(%q) = %d, want %d", tt.in, got, tt.want)
			}
		})
	}
}
