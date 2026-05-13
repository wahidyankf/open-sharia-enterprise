package docs

import (
	"os"
	"path/filepath"
	"strings"
	"testing"
)

// writeTestFile writes content to a path relative to root, creating parent
// directories as needed. It fails the test on any I/O error.
func writeTestFile(t *testing.T, root, rel, content string) {
	t.Helper()
	full := filepath.Join(root, rel)
	if err := os.MkdirAll(filepath.Dir(full), 0o755); err != nil {
		t.Fatalf("mkdir %s: %v", filepath.Dir(full), err)
	}
	if err := os.WriteFile(full, []byte(content), 0o644); err != nil {
		t.Fatalf("write %s: %v", full, err)
	}
}

// softwareDocClean is a fully populated software-engineering doc using the
// canonical Diátaxis category value "tutorial".
const softwareDocClean = `---
title: Test Doc
description: A test doc with all fields.
category: tutorial
subcategory: testing
tags:
  - go
  - testing
---

# Test Doc

Body.
`

func TestValidateDocsFrontmatter_SoftwareDocClean(t *testing.T) {
	tmp := t.TempDir()
	writeTestFile(t, tmp, "docs/explanation/software-engineering/testing/intro.md", softwareDocClean)

	findings, err := ValidateDocsFrontmatter([]string{tmp})
	if err != nil {
		t.Fatalf("ValidateDocsFrontmatter: %v", err)
	}
	if len(findings) != 0 {
		t.Fatalf("expected zero findings, got %d: %+v", len(findings), findings)
	}
}

func TestValidateDocsFrontmatter_SoftwareDocMissingTitle(t *testing.T) {
	const content = `---
description: A test doc.
category: tutorial
subcategory: testing
tags: [go]
---

Body.
`
	tmp := t.TempDir()
	writeTestFile(t, tmp, "docs/explanation/software-engineering/testing/no-title.md", content)

	findings, err := ValidateDocsFrontmatter([]string{tmp})
	if err != nil {
		t.Fatalf("ValidateDocsFrontmatter: %v", err)
	}
	if len(findings) != 1 {
		t.Fatalf("expected 1 finding, got %d: %+v", len(findings), findings)
	}
	if findings[0].Kind != kindMissingTitle {
		t.Errorf("expected kind %q, got %q", kindMissingTitle, findings[0].Kind)
	}
	if findings[0].Severity != severityFail {
		t.Errorf("expected severity %q, got %q", severityFail, findings[0].Severity)
	}
}

func TestValidateDocsFrontmatter_SoftwareDocMissingCategory(t *testing.T) {
	const content = `---
title: Test
description: A test doc.
subcategory: testing
tags: [go]
---

Body.
`
	tmp := t.TempDir()
	writeTestFile(t, tmp, "docs/explanation/software-engineering/testing/no-cat.md", content)

	findings, err := ValidateDocsFrontmatter([]string{tmp})
	if err != nil {
		t.Fatalf("ValidateDocsFrontmatter: %v", err)
	}
	if len(findings) != 1 {
		t.Fatalf("expected 1 finding, got %d: %+v", len(findings), findings)
	}
	if findings[0].Kind != kindMissingCategory {
		t.Errorf("expected kind %q, got %q", kindMissingCategory, findings[0].Kind)
	}
}

func TestValidateDocsFrontmatter_SoftwareDocWrongCategoryValue(t *testing.T) {
	const content = `---
title: Test
description: A test doc.
category: hardware
subcategory: testing
tags: [go]
---

Body.
`
	tmp := t.TempDir()
	writeTestFile(t, tmp, "docs/explanation/software-engineering/testing/wrong-cat.md", content)

	findings, err := ValidateDocsFrontmatter([]string{tmp})
	if err != nil {
		t.Fatalf("ValidateDocsFrontmatter: %v", err)
	}
	if len(findings) != 1 {
		t.Fatalf("expected 1 finding, got %d: %+v", len(findings), findings)
	}
	if findings[0].Kind != kindWrongCategoryValue {
		t.Errorf("expected kind %q, got %q", kindWrongCategoryValue, findings[0].Kind)
	}
	if !strings.Contains(findings[0].Message, "hardware") {
		t.Errorf("expected message to include offending value, got %q", findings[0].Message)
	}
}

func TestValidateDocsFrontmatter_SoftwareDocMissingMultiple(t *testing.T) {
	// title and category missing → both reported, sorted by Kind.
	const content = `---
description: just a description
subcategory: testing
tags: [go]
---

Body.
`
	tmp := t.TempDir()
	writeTestFile(t, tmp, "docs/explanation/software-engineering/testing/multi.md", content)

	findings, err := ValidateDocsFrontmatter([]string{tmp})
	if err != nil {
		t.Fatalf("ValidateDocsFrontmatter: %v", err)
	}
	if len(findings) != 2 {
		t.Fatalf("expected 2 findings, got %d: %+v", len(findings), findings)
	}
	// Sorted by Kind: missing-category < missing-title alphabetically.
	if findings[0].Kind != kindMissingCategory {
		t.Errorf("expected first finding kind %q, got %q", kindMissingCategory, findings[0].Kind)
	}
	if findings[1].Kind != kindMissingTitle {
		t.Errorf("expected second finding kind %q, got %q", kindMissingTitle, findings[1].Kind)
	}
}

func TestValidateDocsFrontmatter_SoftwareDocEmptyTags(t *testing.T) {
	const content = `---
title: Test
description: A test doc.
category: tutorial
subcategory: testing
tags: []
---

Body.
`
	tmp := t.TempDir()
	writeTestFile(t, tmp, "docs/explanation/software-engineering/testing/empty-tags.md", content)

	findings, err := ValidateDocsFrontmatter([]string{tmp})
	if err != nil {
		t.Fatalf("ValidateDocsFrontmatter: %v", err)
	}
	if len(findings) != 1 || findings[0].Kind != kindMissingTags {
		t.Fatalf("expected one missing-tags finding, got %+v", findings)
	}
}

func TestValidateDocsFrontmatter_SoftwareDocWrongTagsType(t *testing.T) {
	const content = `---
title: Test
description: A test doc.
category: tutorial
subcategory: testing
tags: go
---

Body.
`
	tmp := t.TempDir()
	writeTestFile(t, tmp, "docs/explanation/software-engineering/testing/scalar-tags.md", content)

	findings, err := ValidateDocsFrontmatter([]string{tmp})
	if err != nil {
		t.Fatalf("ValidateDocsFrontmatter: %v", err)
	}
	if len(findings) != 1 || findings[0].Kind != kindMissingTags {
		t.Fatalf("expected one missing-tags finding (non-list type), got %+v", findings)
	}
}

func TestValidateDocsFrontmatter_GovernanceDocTitleOnly(t *testing.T) {
	const content = `---
title: A Governance Convention
---

Body.
`
	tmp := t.TempDir()
	writeTestFile(t, tmp, "repo-governance/conventions/structure/test.md", content)

	findings, err := ValidateDocsFrontmatter([]string{tmp})
	if err != nil {
		t.Fatalf("ValidateDocsFrontmatter: %v", err)
	}
	if HasFailFindings(findings) {
		t.Fatalf("expected zero fail findings for title-only governance doc, got %+v", findings)
	}
	// One warn finding is expected for missing description.
	if len(findings) != 1 || findings[0].Severity != severityWarn {
		t.Fatalf("expected exactly one warn finding for missing description, got %+v", findings)
	}
	if findings[0].Kind != kindMissingDescription {
		t.Errorf("expected warn kind %q, got %q", kindMissingDescription, findings[0].Kind)
	}
}

func TestValidateDocsFrontmatter_GovernanceDocTitleAndDescription(t *testing.T) {
	const content = `---
title: A Convention
description: This convention does something.
---

Body.
`
	tmp := t.TempDir()
	writeTestFile(t, tmp, "repo-governance/principles/test.md", content)

	findings, err := ValidateDocsFrontmatter([]string{tmp})
	if err != nil {
		t.Fatalf("ValidateDocsFrontmatter: %v", err)
	}
	if len(findings) != 0 {
		t.Fatalf("expected zero findings, got %+v", findings)
	}
}

func TestValidateDocsFrontmatter_GovernanceDocMissingTitle(t *testing.T) {
	const content = `---
description: This convention does something.
---

Body.
`
	tmp := t.TempDir()
	writeTestFile(t, tmp, "repo-governance/development/test.md", content)

	findings, err := ValidateDocsFrontmatter([]string{tmp})
	if err != nil {
		t.Fatalf("ValidateDocsFrontmatter: %v", err)
	}
	if !HasFailFindings(findings) {
		t.Fatalf("expected at least one fail finding for missing title, got %+v", findings)
	}
}

func TestValidateDocsFrontmatter_GovernanceWorkflowsApplies(t *testing.T) {
	const content = `---
description: This convention does something.
---

Body.
`
	tmp := t.TempDir()
	writeTestFile(t, tmp, "repo-governance/workflows/test.md", content)

	findings, err := ValidateDocsFrontmatter([]string{tmp})
	if err != nil {
		t.Fatalf("ValidateDocsFrontmatter: %v", err)
	}
	if !HasFailFindings(findings) {
		t.Fatalf("expected fail finding for missing title under workflows, got %+v", findings)
	}
}

func TestValidateDocsFrontmatter_UnknownAreaPasses(t *testing.T) {
	// A file outside both areas has no required fields and must pass even with
	// no frontmatter at all.
	tmp := t.TempDir()
	writeTestFile(t, tmp, "apps/ose-web/content/foo.md", "# foo body only\n")
	writeTestFile(t, tmp, "docs/tutorials/getting-started.md", "# tutorial body only\n")

	findings, err := ValidateDocsFrontmatter([]string{tmp})
	if err != nil {
		t.Fatalf("ValidateDocsFrontmatter: %v", err)
	}
	if len(findings) != 0 {
		t.Fatalf("expected zero findings for unknown areas, got %+v", findings)
	}
}

func TestValidateDocsFrontmatter_MissingFrontmatterInScopedArea(t *testing.T) {
	tmp := t.TempDir()
	writeTestFile(t, tmp, "docs/explanation/software-engineering/foo.md", "# no frontmatter\n")

	findings, err := ValidateDocsFrontmatter([]string{tmp})
	if err != nil {
		t.Fatalf("ValidateDocsFrontmatter: %v", err)
	}
	if len(findings) != 1 || findings[0].Kind != kindMissingFrontmatter {
		t.Fatalf("expected missing-frontmatter finding, got %+v", findings)
	}
}

func TestValidateDocsFrontmatter_InvalidYAML(t *testing.T) {
	const content = `---
title: [unterminated
---

Body.
`
	tmp := t.TempDir()
	writeTestFile(t, tmp, "docs/explanation/software-engineering/bad.md", content)

	findings, err := ValidateDocsFrontmatter([]string{tmp})
	if err != nil {
		t.Fatalf("ValidateDocsFrontmatter: %v", err)
	}
	if len(findings) != 1 || findings[0].Kind != kindInvalidYAML {
		t.Fatalf("expected invalid-yaml finding, got %+v", findings)
	}
}

func TestValidateDocsFrontmatter_UnclosedFrontmatterTreatedAsMissing(t *testing.T) {
	// File with leading --- but no closing --- → no frontmatter detected.
	const content = `---
title: Test
description: Nope ever closed

Body.
`
	tmp := t.TempDir()
	writeTestFile(t, tmp, "docs/explanation/software-engineering/unclosed.md", content)

	findings, err := ValidateDocsFrontmatter([]string{tmp})
	if err != nil {
		t.Fatalf("ValidateDocsFrontmatter: %v", err)
	}
	if len(findings) != 1 || findings[0].Kind != kindMissingFrontmatter {
		t.Fatalf("expected missing-frontmatter for unclosed block, got %+v", findings)
	}
}

func TestValidateDocsFrontmatter_SkipsSpecialDirectories(t *testing.T) {
	tmp := t.TempDir()
	// node_modules should be skipped wholesale, even if it sits below an
	// in-scope area path.
	writeTestFile(t, tmp, "docs/explanation/software-engineering/node_modules/bad.md", "# nope\n")
	writeTestFile(t, tmp, "docs/explanation/software-engineering/.git/hooks.md", "# nope\n")

	findings, err := ValidateDocsFrontmatter([]string{tmp})
	if err != nil {
		t.Fatalf("ValidateDocsFrontmatter: %v", err)
	}
	if len(findings) != 0 {
		t.Fatalf("expected skipDirs entries to be ignored, got %+v", findings)
	}
}

func TestValidateDocsFrontmatter_EmptyPathsRejected(t *testing.T) {
	_, err := ValidateDocsFrontmatter(nil)
	if err == nil {
		t.Fatal("expected error for empty paths, got nil")
	}
}

func TestValidateDocsFrontmatter_NonExistentPathIgnored(t *testing.T) {
	findings, err := ValidateDocsFrontmatter([]string{"/does/not/exist/nowhere"})
	if err != nil {
		t.Fatalf("expected nil error for non-existent path, got %v", err)
	}
	if len(findings) != 0 {
		t.Fatalf("expected zero findings for non-existent path, got %+v", findings)
	}
}

func TestValidateDocsFrontmatter_SortsStably(t *testing.T) {
	tmp := t.TempDir()
	// Two software-engineering files with same File but different Kinds —
	// we verify cross-file sorting by file path and same-file sorting by Kind.
	writeTestFile(t, tmp, "docs/explanation/software-engineering/b-doc.md", `---
title: B
---

Body.
`)
	writeTestFile(t, tmp, "docs/explanation/software-engineering/a-doc.md", `---
title: A
description: A description.
category: tutorial
subcategory: testing
tags: [go]
---

Body.
`)
	// a-doc.md passes; b-doc.md will have multiple fail findings
	// (description, category, subcategory, tags missing).
	findings, err := ValidateDocsFrontmatter([]string{tmp})
	if err != nil {
		t.Fatalf("ValidateDocsFrontmatter: %v", err)
	}
	if len(findings) == 0 {
		t.Fatalf("expected findings, got 0")
	}
	// All findings should reference b-doc.md only.
	for _, f := range findings {
		if !strings.HasSuffix(f.File, "b-doc.md") {
			t.Errorf("unexpected finding for non-b-doc file: %+v", f)
		}
	}
	// Findings within the same file are sorted by Kind alphabetically.
	for i := 1; i < len(findings); i++ {
		if findings[i].File == findings[i-1].File && findings[i].Kind < findings[i-1].Kind {
			t.Errorf("findings not sorted by Kind: %q < %q", findings[i].Kind, findings[i-1].Kind)
		}
	}
}

// Test_ValidateDocsFrontmatter_Diataxis verifies the four Diátaxis category
// values pass without findings, the deprecated "software" value emits a warn
// finding, and an entirely unknown value emits a fail finding.
func Test_ValidateDocsFrontmatter_Diataxis(t *testing.T) {
	tests := []struct {
		name         string
		category     string
		wantFindings int
		wantSeverity string // "" means no finding
		wantKind     string
	}{
		{
			name:         "tutorial passes",
			category:     "tutorial",
			wantFindings: 0,
		},
		{
			name:         "how-to passes",
			category:     "how-to",
			wantFindings: 0,
		},
		{
			name:         "reference passes",
			category:     "reference",
			wantFindings: 0,
		},
		{
			name:         "explanation passes",
			category:     "explanation",
			wantFindings: 0,
		},
		{
			name:         "deprecated software emits warn not fail",
			category:     "software",
			wantFindings: 1,
			wantSeverity: severityWarn,
			wantKind:     kindCategoryDeprecated,
		},
		{
			name:         "unknown value emits fail",
			category:     "foobar",
			wantFindings: 1,
			wantSeverity: severityFail,
			wantKind:     kindWrongCategoryValue,
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			content := "---\ntitle: Test Doc\ndescription: A test.\ncategory: " + tt.category + "\nsubcategory: testing\ntags:\n  - go\n---\n\n# Test\n\nBody.\n"
			tmp := t.TempDir()
			writeTestFile(t, tmp, "docs/explanation/software-engineering/diataxis-test.md", content)

			findings, err := ValidateDocsFrontmatter([]string{tmp})
			if err != nil {
				t.Fatalf("ValidateDocsFrontmatter: %v", err)
			}
			if len(findings) != tt.wantFindings {
				t.Fatalf("expected %d findings, got %d: %+v", tt.wantFindings, len(findings), findings)
			}
			if tt.wantFindings > 0 {
				if findings[0].Severity != tt.wantSeverity {
					t.Errorf("expected severity %q, got %q", tt.wantSeverity, findings[0].Severity)
				}
				if findings[0].Kind != tt.wantKind {
					t.Errorf("expected kind %q, got %q", tt.wantKind, findings[0].Kind)
				}
			}
		})
	}
}

func TestHasFailFindings(t *testing.T) {
	tests := []struct {
		name string
		in   []DocsFrontmatterFinding
		want bool
	}{
		{name: "empty", in: nil, want: false},
		{name: "only-warn", in: []DocsFrontmatterFinding{{Severity: severityWarn}}, want: false},
		{name: "only-fail", in: []DocsFrontmatterFinding{{Severity: severityFail}}, want: true},
		{name: "mixed", in: []DocsFrontmatterFinding{{Severity: severityWarn}, {Severity: severityFail}}, want: true},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			if got := HasFailFindings(tt.in); got != tt.want {
				t.Errorf("HasFailFindings = %v, want %v", got, tt.want)
			}
		})
	}
}

// TestClassifyDocArea exercises the path → area mapping including all
// governance prefixes and the unknown fallback.
func TestClassifyDocArea(t *testing.T) {
	tests := []struct {
		path string
		want docArea
	}{
		{"docs/explanation/software-engineering/foo.md", areaSoftware},
		{"some/prefix/docs/explanation/software-engineering/foo.md", areaSoftware},
		{"repo-governance/conventions/foo.md", areaGovernance},
		{"repo-governance/principles/foo.md", areaGovernance},
		{"repo-governance/development/foo.md", areaGovernance},
		{"repo-governance/workflows/foo.md", areaGovernance},
		{"docs/tutorials/foo.md", areaUnknown},
		{"apps/ose-web/content/foo.md", areaUnknown},
	}
	for _, tt := range tests {
		t.Run(tt.path, func(t *testing.T) {
			if got := classifyDocArea(tt.path); got != tt.want {
				t.Errorf("classifyDocArea(%q) = %v, want %v", tt.path, got, tt.want)
			}
		})
	}
}

// TestStringValue covers the coercion helper used in error messages.
func TestStringValue(t *testing.T) {
	tests := []struct {
		name string
		v    any
		want string
	}{
		{name: "string", v: "hello", want: "hello"},
		{name: "int", v: 42, want: "42"},
		{name: "nil", v: nil, want: ""},
		{name: "bool", v: true, want: "true"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			if got := stringValue(tt.v); got != tt.want {
				t.Errorf("stringValue(%v) = %q, want %q", tt.v, got, tt.want)
			}
		})
	}
}

// TestExtractFrontmatter covers all branches of the fence-extraction helper.
func TestExtractFrontmatter(t *testing.T) {
	tests := []struct {
		name  string
		in    string
		want  string
		wantB bool
	}{
		{name: "empty", in: "", want: "", wantB: false},
		{name: "no leading fence", in: "# body\n", want: "", wantB: false},
		{name: "no closing fence", in: "---\ntitle: x\n", want: "", wantB: false},
		{name: "well-formed", in: "---\ntitle: x\n---\n# body\n", want: "title: x", wantB: true},
		{name: "empty frontmatter", in: "---\n---\n# body\n", want: "", wantB: true},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			got, gotB := extractFrontmatter(tt.in)
			if got != tt.want || gotB != tt.wantB {
				t.Errorf("extractFrontmatter(%q) = (%q,%v), want (%q,%v)", tt.in, got, gotB, tt.want, tt.wantB)
			}
		})
	}
}
