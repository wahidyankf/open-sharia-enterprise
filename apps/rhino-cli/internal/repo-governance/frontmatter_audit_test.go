package repogovernance

import (
	"os"
	"path/filepath"
	"strings"
	"testing"
)

func writeMarkdown(t *testing.T, path, content string) {
	t.Helper()
	if err := os.MkdirAll(filepath.Dir(path), 0o755); err != nil {
		t.Fatalf("mkdir %s: %v", filepath.Dir(path), err)
	}
	if err := os.WriteFile(path, []byte(content), 0o644); err != nil {
		t.Fatalf("write %s: %v", path, err)
	}
}

func TestAuditFrontmatter_CleanDirectory(t *testing.T) {
	tmp := t.TempDir()
	writeMarkdown(t, filepath.Join(tmp, "clean.md"), `---
title: Clean
created: 2026-01-01
---

# Clean

Body content without any date metadata.
`)

	findings, err := AuditFrontmatter([]string{tmp})
	if err != nil {
		t.Fatalf("AuditFrontmatter: %v", err)
	}
	if len(findings) != 0 {
		t.Fatalf("expected zero findings, got %d: %+v", len(findings), findings)
	}
}

func TestAuditFrontmatter_ForbiddenUpdatedField(t *testing.T) {
	tmp := t.TempDir()
	path := filepath.Join(tmp, "bad-frontmatter.md")
	writeMarkdown(t, path, `---
title: Bad
created: 2026-01-01
updated: 2026-04-01
---

# Bad

Body.
`)

	findings, err := AuditFrontmatter([]string{tmp})
	if err != nil {
		t.Fatalf("AuditFrontmatter: %v", err)
	}
	if len(findings) != 1 {
		t.Fatalf("expected exactly 1 finding, got %d: %+v", len(findings), findings)
	}
	f := findings[0]
	if f.File != path {
		t.Errorf("unexpected file: got %q want %q", f.File, path)
	}
	if f.Line != 4 {
		t.Errorf("unexpected line: got %d want 4", f.Line)
	}
	if !strings.Contains(f.Message, "updated") {
		t.Errorf("message does not mention 'updated': %q", f.Message)
	}
}

func TestAuditFrontmatter_LastUpdatedFooterBlock(t *testing.T) {
	tmp := t.TempDir()
	path := filepath.Join(tmp, "footer.md")
	writeMarkdown(t, path, `---
title: Footer
---

# Footer

Body content.

---

**Last Updated**: 2026-04-01
`)

	findings, err := AuditFrontmatter([]string{tmp})
	if err != nil {
		t.Fatalf("AuditFrontmatter: %v", err)
	}
	if len(findings) != 1 {
		t.Fatalf("expected exactly 1 finding, got %d: %+v", len(findings), findings)
	}
	if !strings.Contains(findings[0].Message, "Last Updated") {
		t.Errorf("message does not mention 'Last Updated': %q", findings[0].Message)
	}
}

func TestAuditFrontmatter_InlineCreatedAnnotation(t *testing.T) {
	tmp := t.TempDir()
	path := filepath.Join(tmp, "inline.md")
	writeMarkdown(t, path, `---
title: Inline
---

# Inline

## Document History

- **Created**: 2026-04-01
- **Last Updated**: 2026-04-02
`)

	findings, err := AuditFrontmatter([]string{tmp})
	if err != nil {
		t.Fatalf("AuditFrontmatter: %v", err)
	}
	if len(findings) != 2 {
		t.Fatalf("expected 2 findings (Created + Last Updated bullets), got %d: %+v", len(findings), findings)
	}
	for _, f := range findings {
		if f.File != path {
			t.Errorf("unexpected file path: %q", f.File)
		}
		if !strings.Contains(f.Message, "inline date annotation") {
			t.Errorf("expected inline annotation message, got %q", f.Message)
		}
	}
}

func TestAuditFrontmatter_WebsiteExemption(t *testing.T) {
	tmp := t.TempDir()
	// File is exempt because it lives under apps/ayokoding-web/.
	path := filepath.Join(tmp, "apps", "ayokoding-web", "content", "post.md")
	writeMarkdown(t, path, `---
title: Post
updated: 2026-04-01
---

# Post

**Last Updated**: 2026-04-01
`)

	findings, err := AuditFrontmatter([]string{tmp})
	if err != nil {
		t.Fatalf("AuditFrontmatter: %v", err)
	}
	if len(findings) != 0 {
		t.Fatalf("expected zero findings under website app, got %d: %+v", len(findings), findings)
	}
}

func TestAuditFrontmatter_MultipleFilesSorted(t *testing.T) {
	tmp := t.TempDir()
	writeMarkdown(t, filepath.Join(tmp, "z.md"), `---
title: Z
updated: 2026-04-01
---
body
`)
	writeMarkdown(t, filepath.Join(tmp, "a.md"), `---
title: A
updated: 2026-04-01
---
body
`)

	findings, err := AuditFrontmatter([]string{tmp})
	if err != nil {
		t.Fatalf("AuditFrontmatter: %v", err)
	}
	if len(findings) != 2 {
		t.Fatalf("expected 2 findings, got %d", len(findings))
	}
	if !strings.HasSuffix(findings[0].File, "a.md") {
		t.Errorf("findings not sorted by file: first is %q", findings[0].File)
	}
}

func TestAuditFrontmatter_MissingPathError(t *testing.T) {
	_, err := AuditFrontmatter(nil)
	if err == nil {
		t.Fatal("expected error for empty paths, got nil")
	}
}

func TestAuditFrontmatter_NonexistentPathIsEmpty(t *testing.T) {
	// A non-existent root directory should yield zero findings, not an error.
	findings, err := AuditFrontmatter([]string{"/tmp/this-path-does-not-exist-12345"})
	if err != nil {
		t.Fatalf("expected no error for missing path, got: %v", err)
	}
	if len(findings) != 0 {
		t.Fatalf("expected zero findings for missing path, got %d", len(findings))
	}
}

func TestAuditFrontmatter_NoFrontmatterFileWithFooter(t *testing.T) {
	// File without YAML frontmatter still gets body-rule scanning.
	tmp := t.TempDir()
	path := filepath.Join(tmp, "no-fm.md")
	writeMarkdown(t, path, `# No frontmatter

Body.

**Last Updated**: 2026-04-01
`)

	findings, err := AuditFrontmatter([]string{tmp})
	if err != nil {
		t.Fatalf("AuditFrontmatter: %v", err)
	}
	if len(findings) != 1 {
		t.Fatalf("expected 1 finding for footer-only file, got %d", len(findings))
	}
}

func TestAuditFrontmatter_InvalidYAMLFrontmatterIgnored(t *testing.T) {
	// Invalid YAML in the frontmatter is silently skipped (out of scope) — no
	// findings are emitted for the frontmatter rule, but body rules still run.
	tmp := t.TempDir()
	path := filepath.Join(tmp, "bad-yaml.md")
	writeMarkdown(t, path, `---
title: "Bad
unterminated string
---

# Body
`)
	findings, err := AuditFrontmatter([]string{tmp})
	if err != nil {
		t.Fatalf("AuditFrontmatter: %v", err)
	}
	if len(findings) != 0 {
		t.Fatalf("expected zero findings for invalid YAML, got %+v", findings)
	}
}

func TestAuditFrontmatter_UnclosedFrontmatterTreatedAsBody(t *testing.T) {
	// An unclosed frontmatter fence makes the whole file body — confirm body
	// rules still fire.
	tmp := t.TempDir()
	path := filepath.Join(tmp, "unclosed.md")
	writeMarkdown(t, path, `---
title: Unclosed
no closing fence

**Last Updated**: 2026-04-01
`)
	findings, err := AuditFrontmatter([]string{tmp})
	if err != nil {
		t.Fatalf("AuditFrontmatter: %v", err)
	}
	if len(findings) != 1 {
		t.Fatalf("expected 1 finding for unclosed frontmatter, got %+v", findings)
	}
}

func TestFindFieldLine_FallbackWhenNotFound(t *testing.T) {
	// When findFieldLine cannot locate the named key by line search, it falls
	// back to line 2.
	got := findFieldLine("title: Foo\ndescription: bar\n", "missing")
	if got != 2 {
		t.Errorf("expected fallback line 2, got %d", got)
	}
}

func TestSplitFrontmatter_TrailingCloseFence(t *testing.T) {
	// A file ending exactly with the closing fence (no body lines after) must
	// return an empty body string, not a panic.
	content := "---\ntitle: Foo\n---"
	fm, end, body := splitFrontmatter(content)
	if fm != "title: Foo" {
		t.Errorf("unexpected frontmatter: %q", fm)
	}
	if body != "" {
		t.Errorf("expected empty body, got %q", body)
	}
	if end != 3 {
		t.Errorf("expected close line 3, got %d", end)
	}
}

func TestAuditFrontmatter_NonMarkdownFilesIgnored(t *testing.T) {
	tmp := t.TempDir()
	writeMarkdown(t, filepath.Join(tmp, "ignored.txt"), `updated: 2026-04-01
**Last Updated**: 2026-04-01
`)
	findings, err := AuditFrontmatter([]string{tmp})
	if err != nil {
		t.Fatalf("AuditFrontmatter: %v", err)
	}
	if len(findings) != 0 {
		t.Fatalf("expected zero findings for .txt file, got %+v", findings)
	}
}
