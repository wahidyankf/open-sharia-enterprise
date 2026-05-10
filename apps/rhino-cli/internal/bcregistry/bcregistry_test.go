package bcregistry

import (
	"errors"
	"os"
	"path/filepath"
	"sort"
	"strings"
	"testing"
	"time"
)

// fakeStatInfo implements os.FileInfo for testing stat calls.
type fakeStatInfo struct{ isDir bool }

func (f fakeStatInfo) Name() string       { return "" }
func (f fakeStatInfo) IsDir() bool        { return f.isDir }
func (f fakeStatInfo) Size() int64        { return 0 }
func (f fakeStatInfo) Mode() os.FileMode  { return os.ModeDir }
func (f fakeStatInfo) ModTime() time.Time { return time.Time{} }
func (f fakeStatInfo) Sys() any           { return nil }

// fakeDirEntry implements fs.DirEntry for testing.
type fakeDirEntry struct {
	name  string
	isDir bool
}

func (e fakeDirEntry) Name() string               { return e.name }
func (e fakeDirEntry) IsDir() bool                { return e.isDir }
func (e fakeDirEntry) Type() os.FileMode          { return 0 }
func (e fakeDirEntry) Info() (os.FileInfo, error) { return nil, errors.New("info not used in test") }

// dirEntries builds a []os.DirEntry from name+isDir pairs.
func dirEntries(pairs ...any) []os.DirEntry {
	var out []os.DirEntry
	for i := 0; i+1 < len(pairs); i += 2 {
		name, ok1 := pairs[i].(string)
		isDir, ok2 := pairs[i+1].(bool)
		if !ok1 || !ok2 {
			panic("dirEntries: expected string, bool pairs")
		}
		out = append(out, fakeDirEntry{name: name, isDir: isDir})
	}
	return out
}

// TestLoad_GherkinScalarAutoConvertsToList — Fix #11 schema bump:
// scalar `gherkin: path` decodes to a one-element GherkinPaths slice.
// Backward-compat: every existing v2 registry uses the scalar form.
func TestLoad_GherkinScalarAutoConvertsToList(t *testing.T) {
	orig := osReadFileFn
	defer func() { osReadFileFn = orig }()

	osReadFileFn = func(_ string) ([]byte, error) {
		return []byte(`version: 2
app: test
contexts:
  - name: ctx1
    summary: first
    layers: [domain]
    code: [apps/test/src/contexts/ctx1]
    glossary: specs/apps/test/ubiquitous-language/ctx1.md
    gherkin: specs/apps/test/fe/gherkin/ctx1
    relationships: []
`), nil
	}
	reg, err := Load("/repo", "test")
	if err != nil {
		t.Fatalf("expected no error, got %v", err)
	}
	got := reg.Contexts[0].Gherkin
	if len(got) != 1 || got[0] != "specs/apps/test/fe/gherkin/ctx1" {
		t.Errorf("expected one-element GherkinPaths slice, got %v", got)
	}
}

// TestLoad_GherkinListFormDecodesIntact — Fix #11:
// list `gherkin: [a, b]` decodes into a two-element slice.
func TestLoad_GherkinListFormDecodesIntact(t *testing.T) {
	orig := osReadFileFn
	defer func() { osReadFileFn = orig }()

	osReadFileFn = func(_ string) ([]byte, error) {
		return []byte(`version: 2
app: test
contexts:
  - name: ctx1
    summary: first
    layers: [domain]
    code: [apps/test/src/contexts/ctx1]
    glossary: specs/apps/test/ubiquitous-language/ctx1.md
    gherkin: [behavior/web/gherkin/ctx1, behavior/api/gherkin/ctx1]
    relationships: []
`), nil
	}
	reg, err := Load("/repo", "test")
	if err != nil {
		t.Fatalf("expected no error, got %v", err)
	}
	got := reg.Contexts[0].Gherkin
	if len(got) != 2 {
		t.Fatalf("expected 2 paths, got %d: %v", len(got), got)
	}
	if got[0] != "behavior/web/gherkin/ctx1" || got[1] != "behavior/api/gherkin/ctx1" {
		t.Errorf("paths in unexpected order: %v", got)
	}
}

// TestLoad_GherkinEmptyListErrors — Fix #11:
// empty list errors with a clear message.
func TestLoad_GherkinEmptyListErrors(t *testing.T) {
	orig := osReadFileFn
	defer func() { osReadFileFn = orig }()

	osReadFileFn = func(_ string) ([]byte, error) {
		return []byte(`version: 2
app: test
contexts:
  - name: ctx1
    summary: first
    layers: [domain]
    code: [apps/test/src/contexts/ctx1]
    glossary: specs/apps/test/ubiquitous-language/ctx1.md
    gherkin: []
    relationships: []
`), nil
	}
	_, err := Load("/repo", "test")
	if err == nil {
		t.Fatal("expected error for empty gherkin list, got nil")
	}
	if !strings.Contains(err.Error(), "empty gherkin list") {
		t.Errorf("error should mention 'empty gherkin list', got: %v", err)
	}
}

func TestLoad_Success(t *testing.T) {
	orig := osReadFileFn
	defer func() { osReadFileFn = orig }()

	yaml := []byte(`version: 2
app: test
contexts:
  - name: ctx1
    summary: first
    layers: [domain, application]
    code:
      - apps/test/src/contexts/ctx1
    glossary: specs/apps/test/ubiquitous-language/ctx1.md
    gherkin: specs/apps/test/fe/gherkin/ctx1
    relationships: []
`)
	osReadFileFn = func(_ string) ([]byte, error) { return yaml, nil }

	reg, err := Load("/repo", "test")
	if err != nil {
		t.Fatalf("expected no error, got %v", err)
	}
	if reg.App != "test" || len(reg.Contexts) != 1 || reg.Contexts[0].Name != "ctx1" {
		t.Errorf("unexpected registry: %+v", reg)
	}
}

// TestLoad_CodeLangDefaultsToTSAndTSX verifies that a registry without an
// explicit code_lang field decodes with CodeLang defaulted to ["ts", "tsx"].
// This preserves today's TS-only behaviour for all existing BCs.
func TestLoad_CodeLangDefaultsToTSAndTSX(t *testing.T) {
	orig := osReadFileFn
	defer func() { osReadFileFn = orig }()

	yaml := []byte(`version: 2
app: test
contexts:
  - name: ctx1
    summary: first
    layers: [domain]
    code:
      - apps/test/src/contexts/ctx1
    glossary: specs/apps/test/ubiquitous-language/ctx1.md
    gherkin: specs/apps/test/fe/gherkin/ctx1
    relationships: []
`)
	osReadFileFn = func(_ string) ([]byte, error) { return yaml, nil }

	reg, err := Load("/repo", "test")
	if err != nil {
		t.Fatalf("expected no error, got %v", err)
	}
	if len(reg.Contexts) != 1 {
		t.Fatalf("expected 1 context, got %d", len(reg.Contexts))
	}
	got := reg.Contexts[0].CodeLang
	if len(got) != 2 || got[0] != "ts" || got[1] != "tsx" {
		t.Errorf("expected default CodeLang [ts tsx], got %v", got)
	}
}

// TestLoad_CodeLangExplicitFSharp verifies that an explicit single-element
// code_lang list (e.g., F#-only BCs) decodes intact without being overwritten
// by the default.
func TestLoad_CodeLangExplicitFSharp(t *testing.T) {
	orig := osReadFileFn
	defer func() { osReadFileFn = orig }()

	yaml := []byte(`version: 2
app: test
contexts:
  - name: ctx1
    summary: first
    layers: [domain]
    code:
      - apps/test/src/contexts/ctx1
    code_lang: [fs]
    glossary: specs/apps/test/ubiquitous-language/ctx1.md
    gherkin: specs/apps/test/fe/gherkin/ctx1
    relationships: []
`)
	osReadFileFn = func(_ string) ([]byte, error) { return yaml, nil }

	reg, err := Load("/repo", "test")
	if err != nil {
		t.Fatalf("expected no error, got %v", err)
	}
	got := reg.Contexts[0].CodeLang
	if len(got) != 1 || got[0] != "fs" {
		t.Errorf("expected CodeLang [fs], got %v", got)
	}
}

// TestLoad_CodeLangUnsupportedRejected verifies that an unsupported language
// in code_lang produces a clear error mentioning both the offending value and
// the BC name.
func TestLoad_CodeLangUnsupportedRejected(t *testing.T) {
	orig := osReadFileFn
	defer func() { osReadFileFn = orig }()

	yaml := []byte(`version: 2
app: test
contexts:
  - name: legacy-ledger
    summary: first
    layers: [domain]
    code:
      - apps/test/src/contexts/legacy-ledger
    code_lang: [cobol]
    glossary: specs/apps/test/ubiquitous-language/legacy-ledger.md
    gherkin: specs/apps/test/fe/gherkin/legacy-ledger
    relationships: []
`)
	osReadFileFn = func(_ string) ([]byte, error) { return yaml, nil }

	_, err := Load("/repo", "test")
	if err == nil {
		t.Fatal("expected error for unsupported code_lang, got nil")
	}
	msg := err.Error()
	if !strings.Contains(msg, "cobol") {
		t.Errorf("expected error message to mention 'cobol', got: %s", msg)
	}
	if !strings.Contains(msg, "legacy-ledger") {
		t.Errorf("expected error message to mention BC name 'legacy-ledger', got: %s", msg)
	}
}

func TestLoad_ReadError(t *testing.T) {
	orig := osReadFileFn
	defer func() { osReadFileFn = orig }()

	osReadFileFn = func(_ string) ([]byte, error) { return nil, errors.New("not found") }

	_, err := Load("/repo", "test")
	if err == nil {
		t.Fatal("expected error, got nil")
	}
}

func TestLoad_ParseError(t *testing.T) {
	orig := osReadFileFn
	defer func() { osReadFileFn = orig }()

	osReadFileFn = func(_ string) ([]byte, error) { return []byte(":\tinvalid: yaml: {"), nil }

	_, err := Load("/repo", "test")
	if err == nil {
		t.Fatal("expected error, got nil")
	}
}

func TestValidateAll_LoadError(t *testing.T) {
	orig := osReadFileFn
	defer func() { osReadFileFn = orig }()

	osReadFileFn = func(_ string) ([]byte, error) { return nil, errors.New("not found") }

	_, err := ValidateAll(ValidateOptions{RepoRoot: "/repo", App: "test", Severity: "error"})
	if err == nil {
		t.Fatal("expected error from ValidateAll when Load fails")
	}
}

func TestValidateAll_DefaultSeverity(t *testing.T) {
	origRead := osReadFileFn
	origStat := osStatFn
	origReadDir := osReadDirFn
	defer func() {
		osReadFileFn = origRead
		osStatFn = origStat
		osReadDirFn = origReadDir
	}()

	osReadFileFn = func(_ string) ([]byte, error) {
		return []byte("version: 2\napp: test\ncontexts: []\n"), nil
	}
	osStatFn = func(_ string) (os.FileInfo, error) { return fakeStatInfo{isDir: true}, nil }
	osReadDirFn = func(_ string) ([]os.DirEntry, error) { return nil, nil }

	findings, err := ValidateAll(ValidateOptions{RepoRoot: "/repo", App: "test", Severity: ""})
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	// Empty contexts → no findings
	if len(findings) != 0 {
		t.Errorf("expected 0 findings, got %d", len(findings))
	}
}

func TestCheckContext_MissingCodeDir(t *testing.T) {
	orig := osStatFn
	defer func() { osStatFn = orig }()

	// All stat calls fail: code, glossary, and gherkin all reported missing.
	osStatFn = func(_ string) (os.FileInfo, error) { return nil, errors.New("not found") }

	ctx := Context{
		Name:     "journal",
		Layers:   []string{"domain"},
		Code:     []string{"apps/test/src/contexts/journal"},
		Glossary: "specs/apps/test/ubiquitous-language/journal.md",
		Gherkin:  GherkinPaths{"specs/apps/test/fe/gherkin/journal"},
	}
	findings := checkContext("/repo", ctx, "error")
	// Expect findings for code + glossary + gherkin (each independently checked).
	if len(findings) != 3 {
		t.Fatalf("expected 3 findings (code, glossary, gherkin all missing), got %d: %+v", len(findings), findings)
	}
	hasCodeFinding := false
	for _, f := range findings {
		if f.Severity != "error" {
			t.Errorf("expected severity 'error', got %q", f.Severity)
		}
		if f.File == "apps/test/src/contexts/journal" {
			hasCodeFinding = true
		}
	}
	if !hasCodeFinding {
		t.Error("expected a finding for the missing code directory")
	}
}

func TestCheckContext_AllPresent(t *testing.T) {
	origStat := osStatFn
	origReadDir := osReadDirFn
	defer func() {
		osStatFn = origStat
		osReadDirFn = origReadDir
	}()

	osStatFn = func(_ string) (os.FileInfo, error) { return fakeStatInfo{isDir: true}, nil }
	osReadDirFn = func(_ string) ([]os.DirEntry, error) {
		return dirEntries("domain", true, "application", true), nil
	}

	ctx := Context{
		Name:     "journal",
		Layers:   []string{"domain", "application"},
		Code:     []string{"apps/test/src/contexts/journal"},
		Glossary: "specs/apps/test/ubiquitous-language/journal.md",
		Gherkin:  GherkinPaths{"specs/apps/test/fe/gherkin/journal"},
	}

	// Provide feature file for gherkin check.
	origReadDir2 := osReadDirFn
	callCount := 0
	osReadDirFn = func(p string) ([]os.DirEntry, error) {
		callCount++
		if filepath.Base(p) == "journal" && callCount > 1 {
			return dirEntries("journal.feature", false), nil
		}
		return origReadDir2(p)
	}

	findings := checkContext("/repo", ctx, "error")
	if len(findings) != 0 {
		t.Errorf("expected 0 findings, got %d: %+v", len(findings), findings)
	}
}

func TestCheckLayers_MissingLayer(t *testing.T) {
	orig := osReadDirFn
	defer func() { osReadDirFn = orig }()

	osReadDirFn = func(_ string) ([]os.DirEntry, error) {
		return dirEntries("domain", true), nil // application missing
	}

	ctx := Context{
		Name:   "journal",
		Layers: []string{"domain", "application"},
		Code:   []string{"apps/test/src/contexts/journal"},
	}
	findings := checkLayersAtPath("/repo", ctx, ctx.Code[0], "error")
	if len(findings) != 1 {
		t.Fatalf("expected 1 finding for missing layer, got %d: %+v", len(findings), findings)
	}
	if findings[0].Message == "" {
		t.Error("expected non-empty message")
	}
}

func TestCheckLayers_ExtraLayer(t *testing.T) {
	orig := osReadDirFn
	defer func() { osReadDirFn = orig }()

	osReadDirFn = func(_ string) ([]os.DirEntry, error) {
		return dirEntries("domain", true, "application", true, "extra", true), nil
	}

	ctx := Context{
		Name:   "journal",
		Layers: []string{"domain", "application"},
		Code:   []string{"apps/test/src/contexts/journal"},
	}
	findings := checkLayersAtPath("/repo", ctx, ctx.Code[0], "error")
	if len(findings) != 1 {
		t.Fatalf("expected 1 finding for extra layer, got %d: %+v", len(findings), findings)
	}
}

func TestCheckLayers_ExactMatch(t *testing.T) {
	orig := osReadDirFn
	defer func() { osReadDirFn = orig }()

	osReadDirFn = func(_ string) ([]os.DirEntry, error) {
		return dirEntries("domain", true, "application", true), nil
	}

	ctx := Context{
		Name:   "journal",
		Layers: []string{"domain", "application"},
		Code:   []string{"apps/test/src/contexts/journal"},
	}
	findings := checkLayersAtPath("/repo", ctx, ctx.Code[0], "error")
	if len(findings) != 0 {
		t.Errorf("expected 0 findings, got %d", len(findings))
	}
}

func TestCheckLayers_ReadDirError(t *testing.T) {
	orig := osReadDirFn
	defer func() { osReadDirFn = orig }()

	osReadDirFn = func(_ string) ([]os.DirEntry, error) {
		return nil, errors.New("permission denied")
	}

	ctx := Context{
		Name:   "journal",
		Layers: []string{"domain"},
		Code:   []string{"apps/test/src/contexts/journal"},
	}
	findings := checkLayersAtPath("/repo", ctx, ctx.Code[0], "error")
	if len(findings) != 1 {
		t.Fatalf("expected 1 finding for readdir error, got %d", len(findings))
	}
}

func TestCheckGherkin_MissingDir(t *testing.T) {
	orig := osStatFn
	defer func() { osStatFn = orig }()

	osStatFn = func(_ string) (os.FileInfo, error) { return nil, errors.New("not found") }

	ctx := Context{
		Name:    "journal",
		Gherkin: GherkinPaths{"specs/apps/test/fe/gherkin/journal"},
	}
	findings := checkGherkin("/repo", ctx, "error")
	if len(findings) != 1 {
		t.Fatalf("expected 1 finding for missing gherkin dir, got %d", len(findings))
	}
}

func TestCheckGherkin_NoFeatureFiles(t *testing.T) {
	origStat := osStatFn
	origReadDir := osReadDirFn
	defer func() {
		osStatFn = origStat
		osReadDirFn = origReadDir
	}()

	osStatFn = func(_ string) (os.FileInfo, error) { return fakeStatInfo{isDir: true}, nil }
	osReadDirFn = func(_ string) ([]os.DirEntry, error) {
		return dirEntries("README.md", false), nil
	}

	ctx := Context{
		Name:    "journal",
		Gherkin: GherkinPaths{"specs/apps/test/fe/gherkin/journal"},
	}
	findings := checkGherkin("/repo", ctx, "error")
	if len(findings) != 1 {
		t.Fatalf("expected 1 finding for no feature files, got %d", len(findings))
	}
}

func TestCheckGherkin_HasFeatureFile(t *testing.T) {
	origStat := osStatFn
	origReadDir := osReadDirFn
	defer func() {
		osStatFn = origStat
		osReadDirFn = origReadDir
	}()

	osStatFn = func(_ string) (os.FileInfo, error) { return fakeStatInfo{isDir: true}, nil }
	osReadDirFn = func(_ string) ([]os.DirEntry, error) {
		return dirEntries("journal.feature", false), nil
	}

	ctx := Context{
		Name:    "journal",
		Gherkin: GherkinPaths{"specs/apps/test/fe/gherkin/journal"},
	}
	findings := checkGherkin("/repo", ctx, "error")
	if len(findings) != 0 {
		t.Errorf("expected 0 findings, got %d", len(findings))
	}
}

func TestCheckGherkin_ReadDirError(t *testing.T) {
	origStat := osStatFn
	origReadDir := osReadDirFn
	defer func() {
		osStatFn = origStat
		osReadDirFn = origReadDir
	}()

	osStatFn = func(_ string) (os.FileInfo, error) { return fakeStatInfo{isDir: true}, nil }
	osReadDirFn = func(_ string) ([]os.DirEntry, error) {
		return nil, errors.New("permission denied")
	}

	ctx := Context{
		Name:    "journal",
		Gherkin: GherkinPaths{"specs/apps/test/fe/gherkin/journal"},
	}
	findings := checkGherkin("/repo", ctx, "error")
	if len(findings) != 1 {
		t.Fatalf("expected 1 finding for readdir error, got %d", len(findings))
	}
}

func TestDetectOrphanDirs_FindsOrphan(t *testing.T) {
	orig := osReadDirFn
	defer func() { osReadDirFn = orig }()

	osReadDirFn = func(_ string) ([]os.DirEntry, error) {
		return dirEntries("phantom", true), nil
	}

	registered := map[string]bool{"/repo/apps/test/src/contexts/journal": true}
	findings := detectOrphanDirs("/repo/apps/test/src/contexts", registered, "orphan code directory", "registered in bounded-contexts.yaml", "error")

	if len(findings) != 1 {
		t.Fatalf("expected 1 orphan finding, got %d", len(findings))
	}
}

func TestDetectOrphanDirs_NoOrphan(t *testing.T) {
	orig := osReadDirFn
	defer func() { osReadDirFn = orig }()

	osReadDirFn = func(p string) ([]os.DirEntry, error) {
		return dirEntries("journal", true), nil
	}

	registered := map[string]bool{"/repo/apps/test/src/contexts/journal": true}
	findings := detectOrphanDirs("/repo/apps/test/src/contexts", registered, "orphan code directory", "registered in bounded-contexts.yaml", "error")

	if len(findings) != 0 {
		t.Errorf("expected 0 findings, got %d", len(findings))
	}
}

func TestDetectOrphanDirs_ReadDirError(t *testing.T) {
	orig := osReadDirFn
	defer func() { osReadDirFn = orig }()

	osReadDirFn = func(_ string) ([]os.DirEntry, error) {
		return nil, errors.New("no such directory")
	}

	findings := detectOrphanDirs("/nonexistent", map[string]bool{}, "orphan", "reason", "error")
	if len(findings) != 0 {
		t.Errorf("expected 0 findings on readdir error, got %d", len(findings))
	}
}

func TestDetectOrphanFiles_FindsOrphan(t *testing.T) {
	orig := osReadDirFn
	defer func() { osReadDirFn = orig }()

	osReadDirFn = func(_ string) ([]os.DirEntry, error) {
		return dirEntries("phantom.md", false), nil
	}

	registered := map[string]bool{"/repo/specs/apps/test/ubiquitous-language/journal.md": true}
	findings := detectOrphanFiles("/repo/specs/apps/test/ubiquitous-language", registered, "orphan glossary file", "registered in bounded-contexts.yaml", "error")

	if len(findings) != 1 {
		t.Fatalf("expected 1 orphan finding, got %d", len(findings))
	}
}

func TestDetectOrphanFiles_SkipsREADME(t *testing.T) {
	orig := osReadDirFn
	defer func() { osReadDirFn = orig }()

	osReadDirFn = func(_ string) ([]os.DirEntry, error) {
		return dirEntries("README.md", false), nil
	}

	findings := detectOrphanFiles("/repo/specs/apps/test/ubiquitous-language", map[string]bool{}, "orphan glossary file", "registered", "error")
	if len(findings) != 0 {
		t.Errorf("expected README.md to be skipped, got %d findings", len(findings))
	}
}

func TestDetectOrphanFiles_SkipsDirectories(t *testing.T) {
	orig := osReadDirFn
	defer func() { osReadDirFn = orig }()

	osReadDirFn = func(_ string) ([]os.DirEntry, error) {
		return dirEntries("subdir", true), nil
	}

	findings := detectOrphanFiles("/repo", map[string]bool{}, "orphan", "reason", "error")
	if len(findings) != 0 {
		t.Errorf("expected directories to be skipped, got %d findings", len(findings))
	}
}

func TestDetectOrphanFiles_ReadDirError(t *testing.T) {
	orig := osReadDirFn
	defer func() { osReadDirFn = orig }()

	osReadDirFn = func(_ string) ([]os.DirEntry, error) {
		return nil, errors.New("permission denied")
	}

	findings := detectOrphanFiles("/nonexistent", map[string]bool{}, "orphan", "reason", "error")
	if len(findings) != 0 {
		t.Errorf("expected 0 findings on readdir error, got %d", len(findings))
	}
}

func TestCheckRelationshipSymmetry_Symmetric(t *testing.T) {
	reg := &Registry{
		App: "test",
		Contexts: []Context{
			{Name: "a", Relationships: []Relationship{{To: "b", Kind: "customer-supplier"}}},
			{Name: "b", Relationships: []Relationship{{To: "a", Kind: "customer-supplier"}}},
		},
	}
	ctxByName := map[string]*Context{
		"a": &reg.Contexts[0],
		"b": &reg.Contexts[1],
	}
	findings := checkRelationshipSymmetry(reg, ctxByName, "error")
	if len(findings) != 0 {
		t.Errorf("expected 0 findings for symmetric relationships, got %d: %+v", len(findings), findings)
	}
}

func TestCheckRelationshipSymmetry_Asymmetric(t *testing.T) {
	reg := &Registry{
		App: "test",
		Contexts: []Context{
			{Name: "a", Relationships: []Relationship{{To: "b", Kind: "customer-supplier"}}},
			{Name: "b", Relationships: []Relationship{}},
		},
	}
	ctxByName := map[string]*Context{
		"a": &reg.Contexts[0],
		"b": &reg.Contexts[1],
	}
	findings := checkRelationshipSymmetry(reg, ctxByName, "error")
	if len(findings) != 1 {
		t.Fatalf("expected 1 finding for asymmetric relationship, got %d", len(findings))
	}
}

func TestCheckRelationshipSymmetry_MissingTarget(t *testing.T) {
	reg := &Registry{
		App: "test",
		Contexts: []Context{
			{Name: "a", Relationships: []Relationship{{To: "nonexistent", Kind: "customer-supplier"}}},
		},
	}
	ctxByName := map[string]*Context{"a": &reg.Contexts[0]}
	findings := checkRelationshipSymmetry(reg, ctxByName, "error")
	if len(findings) != 1 {
		t.Fatalf("expected 1 finding for missing target, got %d", len(findings))
	}
}

func TestCheckRelationshipSymmetry_ConformistKind(t *testing.T) {
	reg := &Registry{
		App: "test",
		Contexts: []Context{
			{Name: "a", Relationships: []Relationship{{To: "b", Kind: "conformist"}}},
			{Name: "b", Relationships: []Relationship{}},
		},
	}
	ctxByName := map[string]*Context{
		"a": &reg.Contexts[0],
		"b": &reg.Contexts[1],
	}
	findings := checkRelationshipSymmetry(reg, ctxByName, "error")
	if len(findings) != 1 {
		t.Fatalf("expected 1 finding for asymmetric conformist, got %d", len(findings))
	}
}

func TestCheckRelationshipSymmetry_IgnoresOneWayACL(t *testing.T) {
	reg := &Registry{
		App: "test",
		Contexts: []Context{
			{Name: "a", Relationships: []Relationship{{To: "b", Kind: "anticorruption-layer"}}},
			{Name: "b", Relationships: []Relationship{}},
		},
	}
	ctxByName := map[string]*Context{
		"a": &reg.Contexts[0],
		"b": &reg.Contexts[1],
	}
	findings := checkRelationshipSymmetry(reg, ctxByName, "error")
	if len(findings) != 0 {
		t.Errorf("anticorruption-layer is intentionally one-way, expected 0 findings, got %d", len(findings))
	}
}

func TestCheckRelationshipSymmetry_IgnoresOneWayOHS(t *testing.T) {
	reg := &Registry{
		App: "test",
		Contexts: []Context{
			{Name: "a", Relationships: []Relationship{{To: "b", Kind: "open-host-service"}}},
			{Name: "b", Relationships: []Relationship{}},
		},
	}
	ctxByName := map[string]*Context{
		"a": &reg.Contexts[0],
		"b": &reg.Contexts[1],
	}
	findings := checkRelationshipSymmetry(reg, ctxByName, "error")
	if len(findings) != 0 {
		t.Errorf("open-host-service is intentionally one-way, expected 0 findings, got %d", len(findings))
	}
}

// Fix #10 — partnership must be reciprocal.
func TestCheckRelationshipSymmetry_PartnershipAsymmetric(t *testing.T) {
	reg := &Registry{
		App: "test",
		Contexts: []Context{
			{Name: "a", Relationships: []Relationship{{To: "b", Kind: "partnership"}}},
			{Name: "b", Relationships: []Relationship{}},
		},
	}
	ctxByName := map[string]*Context{
		"a": &reg.Contexts[0],
		"b": &reg.Contexts[1],
	}
	findings := checkRelationshipSymmetry(reg, ctxByName, "error")
	if len(findings) != 1 {
		t.Fatalf("partnership must be reciprocal (Fix #10), expected 1 finding, got %d", len(findings))
	}
}

// Fix #10 — shared-kernel must be reciprocal.
func TestCheckRelationshipSymmetry_SharedKernelAsymmetric(t *testing.T) {
	reg := &Registry{
		App: "test",
		Contexts: []Context{
			{Name: "a", Relationships: []Relationship{{To: "b", Kind: "shared-kernel"}}},
			{Name: "b", Relationships: []Relationship{}},
		},
	}
	ctxByName := map[string]*Context{
		"a": &reg.Contexts[0],
		"b": &reg.Contexts[1],
	}
	findings := checkRelationshipSymmetry(reg, ctxByName, "error")
	if len(findings) != 1 {
		t.Fatalf("shared-kernel must be reciprocal (Fix #10), expected 1 finding, got %d", len(findings))
	}
}

// Fix #10 — unknown relationship kinds produce a finding.
func TestCheckRelationshipKinds_UnknownReported(t *testing.T) {
	reg := &Registry{
		App: "test",
		Contexts: []Context{
			{Name: "a", Relationships: []Relationship{{To: "b", Kind: "made-up-kind"}}},
			{Name: "b", Relationships: []Relationship{}},
		},
	}
	findings := checkRelationshipKinds(reg, "error")
	if len(findings) == 0 {
		t.Fatal("unknown relationship kind must produce a finding (Fix #10)")
	}
	if !strings.Contains(findings[0].Message, "unknown relationship kind") {
		t.Errorf("finding message should mention 'unknown relationship kind', got: %q", findings[0].Message)
	}
}

// Fix #10 — known kinds produce no kind-validation finding.
func TestCheckRelationshipKinds_KnownKindsAreSilent(t *testing.T) {
	for _, k := range []string{"customer-supplier", "conformist", "partnership", "shared-kernel", "anticorruption-layer", "open-host-service"} {
		reg := &Registry{
			App: "test",
			Contexts: []Context{
				{Name: "a", Relationships: []Relationship{{To: "b", Kind: k}}},
			},
		}
		findings := checkRelationshipKinds(reg, "error")
		if len(findings) != 0 {
			t.Errorf("known kind %q should not produce a kind finding, got %d: %+v", k, len(findings), findings)
		}
	}
}

func TestHasReciprocal_True(t *testing.T) {
	ctx := &Context{
		Relationships: []Relationship{{To: "source", Kind: "customer-supplier"}},
	}
	if !hasReciprocal(ctx, "source", "customer-supplier") {
		t.Error("expected hasReciprocal to return true")
	}
}

func TestHasReciprocal_False_WrongTo(t *testing.T) {
	ctx := &Context{
		Relationships: []Relationship{{To: "other", Kind: "customer-supplier"}},
	}
	if hasReciprocal(ctx, "source", "customer-supplier") {
		t.Error("expected hasReciprocal to return false for wrong To")
	}
}

func TestHasReciprocal_False_WrongKind(t *testing.T) {
	ctx := &Context{
		Relationships: []Relationship{{To: "source", Kind: "shared-kernel"}},
	}
	if hasReciprocal(ctx, "source", "customer-supplier") {
		t.Error("expected hasReciprocal to return false for wrong Kind")
	}
}

// TestDetectOrphans_MultiParentGlossaryAndGherkin verifies that when a
// registry has contexts whose glossary/gherkin paths live under different
// parent directories (e.g., behavior/web/... AND behavior/api/...), orphan
// detection walks every parent — not only the first context's parent.
//
// Fixture: registry with 2 contexts; ctx-web has glossary+gherkin under
// "behavior/web/...", ctx-api has glossary+gherkin under "behavior/api/...".
// Plant an orphan glossary file AND an orphan gherkin dir under EACH parent.
// Validator must report all 4 orphans (2 glossary files + 2 gherkin dirs).
//
// Today (pre-Fix #5) only the first context's parents are walked, so this
// test fails until validator.go:208,212 are updated to iterate parents.
func TestDetectOrphans_MultiParentGlossaryAndGherkin(t *testing.T) {
	origReadDir := osReadDirFn
	defer func() { osReadDirFn = origReadDir }()

	osReadDirFn = func(p string) ([]os.DirEntry, error) {
		switch p {
		// Glossary parent dirs (called by detectOrphanFiles).
		case "/repo/specs/apps/test/behavior/web/glossary":
			return dirEntries("ctx-web.md", false, "orphan-web.md", false), nil
		case "/repo/specs/apps/test/behavior/api/glossary":
			return dirEntries("ctx-api.md", false, "orphan-api.md", false), nil
		// Gherkin parent dirs (called by detectOrphanDirs).
		case "/repo/specs/apps/test/behavior/web/gherkin":
			return dirEntries("ctx-web", true, "orphan-web-dir", true), nil
		case "/repo/specs/apps/test/behavior/api/gherkin":
			return dirEntries("ctx-api", true, "orphan-api-dir", true), nil
		}
		// Code parent dir or anything else: empty.
		return nil, nil
	}

	reg := &Registry{
		App: "test",
		Contexts: []Context{
			{
				Name:     "ctx-web",
				Layers:   []string{"presentation"},
				Code:     []string{"apps/test/src/contexts/ctx-web"},
				Glossary: "specs/apps/test/behavior/web/glossary/ctx-web.md",
				Gherkin:  GherkinPaths{"specs/apps/test/behavior/web/gherkin/ctx-web"},
			},
			{
				Name:     "ctx-api",
				Layers:   []string{"application"},
				Code:     []string{"apps/test/src/contexts/ctx-api"},
				Glossary: "specs/apps/test/behavior/api/glossary/ctx-api.md",
				Gherkin:  GherkinPaths{"specs/apps/test/behavior/api/gherkin/ctx-api"},
			},
		},
	}

	registeredCode := map[string]bool{
		"/repo/apps/test/src/contexts/ctx-web": true,
		"/repo/apps/test/src/contexts/ctx-api": true,
	}
	registeredGlossary := map[string]bool{
		"/repo/specs/apps/test/behavior/web/glossary/ctx-web.md": true,
		"/repo/specs/apps/test/behavior/api/glossary/ctx-api.md": true,
	}
	registeredGherkin := map[string]bool{
		"/repo/specs/apps/test/behavior/web/gherkin/ctx-web": true,
		"/repo/specs/apps/test/behavior/api/gherkin/ctx-api": true,
	}

	findings := detectOrphans("/repo", reg, registeredCode, registeredGlossary, registeredGherkin, "error")

	// Collect orphan filenames by category.
	var glossaryOrphans, gherkinOrphans []string
	for _, f := range findings {
		switch {
		case strings.Contains(f.Message, "orphan glossary file"):
			glossaryOrphans = append(glossaryOrphans, filepath.Base(f.File))
		case strings.Contains(f.Message, "orphan gherkin directory"):
			gherkinOrphans = append(gherkinOrphans, filepath.Base(f.File))
		}
	}
	sort.Strings(glossaryOrphans)
	sort.Strings(gherkinOrphans)

	wantGlossary := []string{"orphan-api.md", "orphan-web.md"}
	if len(glossaryOrphans) != 2 || glossaryOrphans[0] != wantGlossary[0] || glossaryOrphans[1] != wantGlossary[1] {
		t.Errorf("glossary orphans: got %v, want %v (findings=%+v)", glossaryOrphans, wantGlossary, findings)
	}

	wantGherkin := []string{"orphan-api-dir", "orphan-web-dir"}
	if len(gherkinOrphans) != 2 || gherkinOrphans[0] != wantGherkin[0] || gherkinOrphans[1] != wantGherkin[1] {
		t.Errorf("gherkin orphans: got %v, want %v (findings=%+v)", gherkinOrphans, wantGherkin, findings)
	}
}

// TestDetectOrphans_MultiPerspectiveBC — Combined Fix #5 + Fix #11:
// when a single BC declares two gherkin perspectives (web + api), every
// orphan under either parent is reported.
func TestDetectOrphans_MultiPerspectiveBC(t *testing.T) {
	origReadDir := osReadDirFn
	defer func() { osReadDirFn = origReadDir }()

	osReadDirFn = func(p string) ([]os.DirEntry, error) {
		switch p {
		case "/repo/specs/apps/test/behavior/web/gherkin":
			return dirEntries("content", true, "stale-web-orphan", true), nil
		case "/repo/specs/apps/test/behavior/api/gherkin":
			return dirEntries("content", true, "stale-api-orphan", true), nil
		}
		return nil, nil
	}

	reg := &Registry{
		App: "test",
		Contexts: []Context{
			{
				Name:     "content",
				Layers:   []string{"presentation"},
				Code:     []string{"apps/test/src/contexts/content"},
				Glossary: "specs/apps/test/ubiquitous-language/content.md",
				Gherkin: GherkinPaths{
					"specs/apps/test/behavior/web/gherkin/content",
					"specs/apps/test/behavior/api/gherkin/content",
				},
			},
		},
	}
	registeredCode := map[string]bool{"/repo/apps/test/src/contexts/content": true}
	registeredGlossary := map[string]bool{"/repo/specs/apps/test/ubiquitous-language/content.md": true}
	registeredGherkin := map[string]bool{
		"/repo/specs/apps/test/behavior/web/gherkin/content": true,
		"/repo/specs/apps/test/behavior/api/gherkin/content": true,
	}
	findings := detectOrphans("/repo", reg, registeredCode, registeredGlossary, registeredGherkin, "error")

	var gherkinOrphans []string
	for _, f := range findings {
		if strings.Contains(f.Message, "orphan gherkin directory") {
			gherkinOrphans = append(gherkinOrphans, filepath.Base(f.File))
		}
	}
	sort.Strings(gherkinOrphans)
	want := []string{"stale-api-orphan", "stale-web-orphan"}
	if len(gherkinOrphans) != 2 || gherkinOrphans[0] != want[0] || gherkinOrphans[1] != want[1] {
		t.Errorf("multi-perspective gherkin orphans: got %v, want %v (findings=%+v)", gherkinOrphans, want, findings)
	}
}

func TestValidate_SortsFindings(t *testing.T) {
	origStat := osStatFn
	origReadDir := osReadDirFn
	defer func() {
		osStatFn = origStat
		osReadDirFn = origReadDir
	}()

	// Simulate missing code dirs for two contexts — findings should be sorted by File.
	osStatFn = func(_ string) (os.FileInfo, error) { return nil, errors.New("not found") }
	osReadDirFn = func(_ string) ([]os.DirEntry, error) { return nil, nil }

	reg := &Registry{
		App: "test",
		Contexts: []Context{
			{
				Name:     "zzz",
				Layers:   []string{"domain"},
				Code:     []string{"apps/test/src/contexts/zzz"},
				Glossary: "specs/apps/test/ubiquitous-language/zzz.md",
				Gherkin:  GherkinPaths{"specs/apps/test/fe/gherkin/zzz"},
			},
			{
				Name:     "aaa",
				Layers:   []string{"domain"},
				Code:     []string{"apps/test/src/contexts/aaa"},
				Glossary: "specs/apps/test/ubiquitous-language/aaa.md",
				Gherkin:  GherkinPaths{"specs/apps/test/fe/gherkin/aaa"},
			},
		},
	}

	findings := validate("/repo", reg, "error")
	if !sort.SliceIsSorted(findings, func(i, j int) bool {
		return findings[i].File < findings[j].File
	}) {
		t.Error("findings not sorted by File")
	}
}
