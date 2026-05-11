package repogovernance

import (
	"os"
	"path/filepath"
	"strings"
	"testing"
)

// helper: build a synthetic repo tree under tmp with the given app/lib/specs
// directories. Each entry is mapped to the license header it should write
// (empty string means do not write a LICENSE file).
type repoLayout struct {
	apps  map[string]string
	libs  map[string]string
	specs string // empty means no specs dir at all; "absent" means dir exists without LICENSE
	// notice holds the verbatim LICENSING-NOTICE.md content (empty means no file).
	notice string
}

func buildRepo(t *testing.T, layout repoLayout) string {
	t.Helper()
	root := t.TempDir()
	for name, header := range layout.apps {
		dir := filepath.Join(root, "apps", name)
		if err := os.MkdirAll(dir, 0o755); err != nil {
			t.Fatalf("mkdir %s: %v", dir, err)
		}
		if header != "" {
			if err := os.WriteFile(filepath.Join(dir, "LICENSE"), []byte(header), 0o644); err != nil {
				t.Fatal(err)
			}
		}
	}
	for name, header := range layout.libs {
		dir := filepath.Join(root, "libs", name)
		if err := os.MkdirAll(dir, 0o755); err != nil {
			t.Fatalf("mkdir %s: %v", dir, err)
		}
		if header != "" {
			if err := os.WriteFile(filepath.Join(dir, "LICENSE"), []byte(header), 0o644); err != nil {
				t.Fatal(err)
			}
		}
	}
	switch layout.specs {
	case "":
		// no specs directory at all
	case "absent":
		if err := os.MkdirAll(filepath.Join(root, "specs"), 0o755); err != nil {
			t.Fatal(err)
		}
	default:
		if err := os.MkdirAll(filepath.Join(root, "specs"), 0o755); err != nil {
			t.Fatal(err)
		}
		if err := os.WriteFile(filepath.Join(root, "specs", "LICENSE"), []byte(layout.specs), 0o644); err != nil {
			t.Fatal(err)
		}
	}
	if layout.notice != "" {
		if err := os.WriteFile(filepath.Join(root, "LICENSING-NOTICE.md"), []byte(layout.notice), 0o644); err != nil {
			t.Fatal(err)
		}
	}
	return root
}

const mitHeader = "MIT License\n\nCopyright (c) 2025-2026 wahidyankf\n"
const apacheHeader = "Apache License 2.0\n\nCopyright (c) 2025-2026 wahidyankf\n"

func TestAuditLicense_CleanRepo(t *testing.T) {
	root := buildRepo(t, repoLayout{
		apps:  map[string]string{"ayokoding-web": mitHeader, "organiclever-web": mitHeader},
		libs:  map[string]string{"golang-commons": mitHeader, "web-ui": mitHeader},
		specs: mitHeader,
		notice: "# Notice\n\n" +
			"| Path | License | Copyright |\n" +
			"| --- | --- | --- |\n" +
			"| `apps/ayokoding-web` | MIT | wahidyankf |\n",
	})

	findings, err := AuditLicense(root)
	if err != nil {
		t.Fatalf("AuditLicense: %v", err)
	}
	if len(findings) != 0 {
		t.Fatalf("expected zero findings, got %+v", findings)
	}
}

func TestAuditLicense_MissingLicenseInApp(t *testing.T) {
	root := buildRepo(t, repoLayout{
		apps:  map[string]string{"ayokoding-web": mitHeader, "organiclever-web": ""},
		libs:  map[string]string{"golang-commons": mitHeader},
		specs: mitHeader,
	})

	findings, err := AuditLicense(root)
	if err != nil {
		t.Fatalf("AuditLicense: %v", err)
	}
	if len(findings) != 1 {
		t.Fatalf("expected 1 finding, got %d: %+v", len(findings), findings)
	}
	got := findings[0]
	if got.Kind != "missing-license" || got.Path != "apps/organiclever-web" {
		t.Errorf("unexpected finding: %+v", got)
	}
}

func TestAuditLicense_MissingLicenseInLib(t *testing.T) {
	root := buildRepo(t, repoLayout{
		apps:  map[string]string{"ayokoding-web": mitHeader},
		libs:  map[string]string{"golang-commons": mitHeader, "web-ui": ""},
		specs: mitHeader,
	})

	findings, err := AuditLicense(root)
	if err != nil {
		t.Fatalf("AuditLicense: %v", err)
	}
	if len(findings) != 1 || findings[0].Kind != "missing-license" || findings[0].Path != "libs/web-ui" {
		t.Fatalf("expected single missing-license for libs/web-ui, got %+v", findings)
	}
}

func TestAuditLicense_MissingLicenseInSpecs(t *testing.T) {
	root := buildRepo(t, repoLayout{
		apps:  map[string]string{"ayokoding-web": mitHeader},
		libs:  map[string]string{"golang-commons": mitHeader},
		specs: "absent",
	})

	findings, err := AuditLicense(root)
	if err != nil {
		t.Fatalf("AuditLicense: %v", err)
	}
	if len(findings) != 1 || findings[0].Path != "specs" {
		t.Fatalf("expected single missing-license for specs, got %+v", findings)
	}
}

func TestAuditLicense_E2eDirIsExempt(t *testing.T) {
	root := buildRepo(t, repoLayout{
		apps: map[string]string{
			"ayokoding-web":        mitHeader,
			"ayokoding-web-fe-e2e": "", // no LICENSE — must be exempt
		},
		libs:  map[string]string{"golang-commons": mitHeader},
		specs: mitHeader,
	})

	findings, err := AuditLicense(root)
	if err != nil {
		t.Fatalf("AuditLicense: %v", err)
	}
	if len(findings) != 0 {
		t.Fatalf("expected zero findings for *-e2e exemption, got %+v", findings)
	}
}

func TestAuditLicense_RhinoCliExempt(t *testing.T) {
	root := buildRepo(t, repoLayout{
		apps: map[string]string{
			"ayokoding-web": mitHeader,
			"rhino-cli":     "", // exempt internal CLI
		},
		libs:  map[string]string{"golang-commons": mitHeader},
		specs: mitHeader,
	})

	findings, err := AuditLicense(root)
	if err != nil {
		t.Fatalf("AuditLicense: %v", err)
	}
	if len(findings) != 0 {
		t.Fatalf("expected zero findings, got %+v", findings)
	}
}

func TestAuditLicense_NoticeRowSpdxMismatch(t *testing.T) {
	root := buildRepo(t, repoLayout{
		apps:  map[string]string{"ayokoding-web": mitHeader},
		libs:  map[string]string{"golang-commons": apacheHeader},
		specs: mitHeader,
		notice: "# Notice\n\n" +
			"| Path | License | Copyright |\n" +
			"| --- | --- | --- |\n" +
			"| `libs/golang-commons` | MIT | wahidyankf |\n",
	})

	findings, err := AuditLicense(root)
	if err != nil {
		t.Fatalf("AuditLicense: %v", err)
	}
	if len(findings) != 1 {
		t.Fatalf("expected single SPDX mismatch finding, got %+v", findings)
	}
	got := findings[0]
	if got.Kind != "spdx-mismatch" || got.Path != "libs/golang-commons" {
		t.Errorf("unexpected finding kind/path: %+v", got)
	}
	if !strings.Contains(got.Message, "Apache-2.0") || !strings.Contains(got.Message, "MIT") {
		t.Errorf("expected message to name both identifiers, got: %s", got.Message)
	}
}

func TestAuditLicense_NoticeWithMatchingClaimAndMissingFile(t *testing.T) {
	// LICENSING-NOTICE.md mentions a directory that lacks its LICENSE — the
	// missing-license finding is canonical; do not also emit a spdx-mismatch.
	root := buildRepo(t, repoLayout{
		apps:  map[string]string{"ayokoding-web": ""},
		libs:  map[string]string{"golang-commons": mitHeader},
		specs: mitHeader,
		notice: "# Notice\n\n" +
			"| Path | License | Copyright |\n" +
			"| --- | --- | --- |\n" +
			"| `apps/ayokoding-web` | MIT | wahidyankf |\n",
	})

	findings, err := AuditLicense(root)
	if err != nil {
		t.Fatalf("AuditLicense: %v", err)
	}
	if len(findings) != 1 || findings[0].Kind != "missing-license" {
		t.Fatalf("expected single missing-license, got %+v", findings)
	}
}

func TestAuditLicense_NoticeNonOwnedPathIgnored(t *testing.T) {
	// Third-party rows like `archived/...` are out of scope and must not
	// affect the audit (the audit only owns apps/, libs/, specs/).
	root := buildRepo(t, repoLayout{
		apps:  map[string]string{"ayokoding-web": mitHeader},
		libs:  map[string]string{"golang-commons": mitHeader},
		specs: mitHeader,
		notice: "# Notice\n\n" +
			"| Path | License | Copyright |\n" +
			"| --- | --- | --- |\n" +
			"| `archived/ayokoding-web-hugo` | Apache-2.0 | someone |\n",
	})

	findings, err := AuditLicense(root)
	if err != nil {
		t.Fatalf("AuditLicense: %v", err)
	}
	if len(findings) != 0 {
		t.Fatalf("expected zero findings for non-owned notice row, got %+v", findings)
	}
}

func TestAuditLicense_DeterministicOrdering(t *testing.T) {
	// Multiple findings sorted by Path then Kind.
	root := buildRepo(t, repoLayout{
		apps:  map[string]string{"zeta-app": "", "alpha-app": ""},
		libs:  map[string]string{"middle-lib": ""},
		specs: mitHeader,
	})

	findings, err := AuditLicense(root)
	if err != nil {
		t.Fatalf("AuditLicense: %v", err)
	}
	if len(findings) != 3 {
		t.Fatalf("expected 3 findings, got %d: %+v", len(findings), findings)
	}
	expected := []string{"apps/alpha-app", "apps/zeta-app", "libs/middle-lib"}
	for i, want := range expected {
		if findings[i].Path != want {
			t.Errorf("ordering mismatch at %d: got %q want %q", i, findings[i].Path, want)
		}
	}
}

func TestAuditLicense_MissingRepoYieldsClean(t *testing.T) {
	// Empty repoRoot — every required dir is absent. specs is absent, apps and
	// libs are absent. Expect zero findings (nothing to audit).
	tmp := t.TempDir()
	findings, err := AuditLicense(tmp)
	if err != nil {
		t.Fatalf("AuditLicense: %v", err)
	}
	if len(findings) != 0 {
		t.Fatalf("expected zero findings on empty repo, got %+v", findings)
	}
}

func TestExtractSPDX_RecognisesMIT(t *testing.T) {
	tmp := t.TempDir()
	path := filepath.Join(tmp, "LICENSE")
	if err := os.WriteFile(path, []byte("MIT License\n\nfoo\n"), 0o644); err != nil {
		t.Fatal(err)
	}
	got, err := extractSPDX(path)
	if err != nil {
		t.Fatalf("extractSPDX: %v", err)
	}
	if got != "MIT" {
		t.Errorf("got %q want MIT", got)
	}
}

func TestExtractSPDX_RecognisesApache(t *testing.T) {
	tmp := t.TempDir()
	path := filepath.Join(tmp, "LICENSE")
	if err := os.WriteFile(path, []byte("\n\nApache License, Version 2.0\n"), 0o644); err != nil {
		t.Fatal(err)
	}
	got, err := extractSPDX(path)
	if err != nil {
		t.Fatalf("extractSPDX: %v", err)
	}
	if got != "Apache-2.0" {
		t.Errorf("got %q want Apache-2.0", got)
	}
}

func TestExtractSPDX_RecognisesSPDXHeader(t *testing.T) {
	tmp := t.TempDir()
	path := filepath.Join(tmp, "LICENSE")
	if err := os.WriteFile(path, []byte("SPDX-License-Identifier: BSD-3-Clause\n"), 0o644); err != nil {
		t.Fatal(err)
	}
	got, err := extractSPDX(path)
	if err != nil {
		t.Fatalf("extractSPDX: %v", err)
	}
	if got != "BSD-3-Clause" {
		t.Errorf("got %q want BSD-3-Clause", got)
	}
}

func TestExtractSPDX_RecognisesBSD3Clause(t *testing.T) {
	tmp := t.TempDir()
	path := filepath.Join(tmp, "LICENSE")
	if err := os.WriteFile(path, []byte("BSD 3-Clause License\n"), 0o644); err != nil {
		t.Fatal(err)
	}
	got, err := extractSPDX(path)
	if err != nil {
		t.Fatalf("extractSPDX: %v", err)
	}
	if got != "BSD-3-Clause" {
		t.Errorf("got %q want BSD-3-Clause", got)
	}
}

func TestExtractSPDX_EmptyFile(t *testing.T) {
	tmp := t.TempDir()
	path := filepath.Join(tmp, "LICENSE")
	if err := os.WriteFile(path, []byte(""), 0o644); err != nil {
		t.Fatal(err)
	}
	if _, err := extractSPDX(path); err == nil {
		t.Fatal("expected error for empty LICENSE file")
	}
}

func TestExtractSPDX_MissingFile(t *testing.T) {
	tmp := t.TempDir()
	_, err := extractSPDX(filepath.Join(tmp, "missing"))
	if err == nil || !os.IsNotExist(err) {
		t.Fatalf("expected ErrNotExist, got %v", err)
	}
}

func TestExtractSPDX_UnknownLineReturnedVerbatim(t *testing.T) {
	tmp := t.TempDir()
	path := filepath.Join(tmp, "LICENSE")
	if err := os.WriteFile(path, []byte("Custom Internal License v1\n"), 0o644); err != nil {
		t.Fatal(err)
	}
	got, err := extractSPDX(path)
	if err != nil {
		t.Fatalf("extractSPDX: %v", err)
	}
	if got != "Custom Internal License v1" {
		t.Errorf("got %q want verbatim", got)
	}
}

func TestParseLicensingNotice_ExtractsRows(t *testing.T) {
	tmp := t.TempDir()
	path := filepath.Join(tmp, "LICENSING-NOTICE.md")
	body := "# Notice\n\nPrefix prose.\n\n" +
		"| Path | License | Copyright |\n" +
		"| --- | --- | --- |\n" +
		"| `apps/foo` | MIT | wahidyankf |\n" +
		"| `libs/bar` | Apache-2.0 | wahidyankf |\n" +
		"\nMore prose.\n"
	if err := os.WriteFile(path, []byte(body), 0o644); err != nil {
		t.Fatal(err)
	}
	claims, err := parseLicensingNotice(path)
	if err != nil {
		t.Fatalf("parseLicensingNotice: %v", err)
	}
	if len(claims) != 2 {
		t.Fatalf("expected 2 claims, got %+v", claims)
	}
	if claims[0].Path != "`apps/foo`" || claims[0].License != "MIT" {
		t.Errorf("first claim unexpected: %+v", claims[0])
	}
	if claims[1].Path != "`libs/bar`" || claims[1].License != "Apache-2.0" {
		t.Errorf("second claim unexpected: %+v", claims[1])
	}
}

func TestParseLicensingNotice_TolerantOfDirectoryHeader(t *testing.T) {
	tmp := t.TempDir()
	path := filepath.Join(tmp, "LICENSING-NOTICE.md")
	body := "# Notice\n\n" +
		"| Directory | License |\n" +
		"| --- | --- |\n" +
		"| apps/foo | MIT |\n"
	if err := os.WriteFile(path, []byte(body), 0o644); err != nil {
		t.Fatal(err)
	}
	claims, err := parseLicensingNotice(path)
	if err != nil {
		t.Fatalf("parseLicensingNotice: %v", err)
	}
	if len(claims) != 1 || claims[0].Path != "apps/foo" {
		t.Errorf("unexpected claims: %+v", claims)
	}
}

func TestParseLicensingNotice_IgnoresNonMatchingTables(t *testing.T) {
	tmp := t.TempDir()
	path := filepath.Join(tmp, "LICENSING-NOTICE.md")
	body := "# Notice\n\n" +
		"| Foo | Bar |\n" +
		"| --- | --- |\n" +
		"| a | b |\n" +
		"\n" +
		"| Path | License |\n" +
		"| --- | --- |\n" +
		"| apps/real | MIT |\n"
	if err := os.WriteFile(path, []byte(body), 0o644); err != nil {
		t.Fatal(err)
	}
	claims, err := parseLicensingNotice(path)
	if err != nil {
		t.Fatalf("parseLicensingNotice: %v", err)
	}
	if len(claims) != 1 || claims[0].Path != "apps/real" {
		t.Errorf("unexpected claims: %+v", claims)
	}
}

func TestNormaliseClaimPath(t *testing.T) {
	cases := map[string]string{
		"`apps/foo`":   "apps/foo",
		" `libs/bar` ": "libs/bar",
		"./apps/foo/":  "apps/foo",
		"specs":        "specs",
	}
	for in, want := range cases {
		got := normaliseClaimPath(in)
		if got != want {
			t.Errorf("normaliseClaimPath(%q) = %q, want %q", in, got, want)
		}
	}
}

func TestOwnedByLicenseAudit(t *testing.T) {
	type tc struct {
		in   string
		want bool
	}
	cases := []tc{
		{"specs", true},
		{"apps/foo", true},
		{"libs/bar", true},
		{"apps/foo/src", false},
		{"libs/", false},
		{"archived/legacy", false},
		{"docs/foo", false},
	}
	for _, c := range cases {
		got := ownedByLicenseAudit(c.in)
		if got != c.want {
			t.Errorf("ownedByLicenseAudit(%q) = %v, want %v", c.in, got, c.want)
		}
	}
}

func TestLicensesEqual(t *testing.T) {
	if !licensesEqual("MIT", "MIT License") {
		t.Error("MIT should equal MIT License")
	}
	if !licensesEqual("MIT License", "mit") {
		t.Error("MIT License should equal mit (case-insensitive)")
	}
	if licensesEqual("MIT", "Apache-2.0") {
		t.Error("MIT should not equal Apache-2.0")
	}
}

func TestSplitMarkdownRow_HonoursEscapedPipes(t *testing.T) {
	got := splitMarkdownRow(`| a \| still-a | b |`)
	if len(got) != 2 {
		t.Fatalf("expected 2 cells, got %+v", got)
	}
	if strings.TrimSpace(got[0]) != "a | still-a" {
		t.Errorf("unexpected first cell: %q", got[0])
	}
}

func TestClassifyLicenseLine_AdditionalSPDXForms(t *testing.T) {
	cases := map[string]string{
		"BSD 2-Clause License":             "BSD-2-Clause",
		"Mozilla Public License, v. 2.0":   "MPL-2.0",
		"GNU General Public License v3.0":  "GPL",
		"SPDX-License-Identifier: GPL-3.0": "GPL-3.0",
		"MIT":                              "MIT",
	}
	for in, want := range cases {
		if got := classifyLicenseLine(in); got != want {
			t.Errorf("classifyLicenseLine(%q) = %q, want %q", in, got, want)
		}
	}
}

func TestStripPrefixFold_NonMatch(t *testing.T) {
	if _, ok := stripPrefixFold("short", "longerprefix"); ok {
		t.Error("expected ok=false for shorter input")
	}
	if _, ok := stripPrefixFold("Other: foo", "SPDX:"); ok {
		t.Error("expected ok=false for non-matching prefix")
	}
}

func TestAuditLicense_LicensingNoticeNonOwnedNestedPath(t *testing.T) {
	// ownedByLicenseAudit refuses nested paths like apps/foo/src — they must
	// not produce findings.
	root := buildRepo(t, repoLayout{
		apps:  map[string]string{"ayokoding-web": mitHeader},
		libs:  map[string]string{"golang-commons": mitHeader},
		specs: mitHeader,
		notice: "# Notice\n\n" +
			"| Path | License | Copyright |\n" +
			"| --- | --- | --- |\n" +
			"| `apps/ayokoding-web/src` | Apache-2.0 | someone |\n",
	})
	findings, err := AuditLicense(root)
	if err != nil {
		t.Fatalf("AuditLicense: %v", err)
	}
	if len(findings) != 0 {
		t.Fatalf("expected zero findings for nested non-owned path, got %+v", findings)
	}
}

func TestAuditLicense_AppsPathBlockedReturnsError(t *testing.T) {
	// If apps/ is a regular file (not a directory), os.ReadDir returns a
	// non-ErrNotExist error, which the audit propagates.
	tmp := t.TempDir()
	// Create a file named "apps" — readdir on a non-dir returns an error
	// that is NOT os.IsNotExist.
	if err := os.WriteFile(filepath.Join(tmp, "apps"), []byte("not a dir"), 0o644); err != nil {
		t.Fatal(err)
	}
	_, err := AuditLicense(tmp)
	if err == nil {
		t.Fatal("expected error when apps/ is not a directory")
	}
}

func TestAuditLicense_LibsPathBlockedReturnsError(t *testing.T) {
	tmp := t.TempDir()
	if err := os.MkdirAll(filepath.Join(tmp, "apps"), 0o755); err != nil {
		t.Fatal(err)
	}
	if err := os.WriteFile(filepath.Join(tmp, "libs"), []byte("not a dir"), 0o644); err != nil {
		t.Fatal(err)
	}
	_, err := AuditLicense(tmp)
	if err == nil {
		t.Fatal("expected error when libs/ is not a directory")
	}
}

func TestExtractSPDX_OnDirReturnsError(t *testing.T) {
	tmp := t.TempDir()
	// Pointing extractSPDX at a directory yields a non-ErrNotExist error
	// after some reads, which surfaces via scanner.Err or read failure.
	_, err := extractSPDX(tmp)
	if err == nil {
		t.Fatal("expected error when path is a directory")
	}
}

func TestAuditLicense_UnreadableLicenseEmitsFinding(t *testing.T) {
	// An empty LICENSE file triggers extractSPDX's "empty" error path, which
	// translates to an unreadable-license finding inside AuditLicense.
	root := buildRepo(t, repoLayout{
		apps:  map[string]string{"ayokoding-web": ""}, // no LICENSE — missing-license
		libs:  map[string]string{},
		specs: mitHeader,
	})
	// Now overwrite: create an empty LICENSE inside apps/ayokoding-web/.
	if err := os.WriteFile(filepath.Join(root, "apps", "ayokoding-web", "LICENSE"), []byte(""), 0o644); err != nil {
		t.Fatal(err)
	}
	findings, err := AuditLicense(root)
	if err != nil {
		t.Fatalf("AuditLicense: %v", err)
	}
	if len(findings) != 1 || findings[0].Kind != "unreadable-license" {
		t.Fatalf("expected single unreadable-license, got %+v", findings)
	}
}

func TestParseLicensingNotice_HeaderAtEndOfFileSkipped(t *testing.T) {
	tmp := t.TempDir()
	path := filepath.Join(tmp, "LICENSING-NOTICE.md")
	// A header row that lives on the final line of the file has no separator
	// row after it; parseLicensingNotice must not panic and must produce no
	// claims.
	body := "Prose\n\n| Path | License |"
	if err := os.WriteFile(path, []byte(body), 0o644); err != nil {
		t.Fatal(err)
	}
	claims, err := parseLicensingNotice(path)
	if err != nil {
		t.Fatalf("parseLicensingNotice: %v", err)
	}
	if len(claims) != 0 {
		t.Errorf("expected zero claims, got %+v", claims)
	}
}

func TestParseLicensingNotice_RowWithMissingCellsSkipped(t *testing.T) {
	tmp := t.TempDir()
	path := filepath.Join(tmp, "LICENSING-NOTICE.md")
	body := "| Path | License |\n" +
		"| --- | --- |\n" +
		"| `apps/foo` |\n" + // missing license cell
		"|  | MIT |\n" + // empty path
		"| `apps/real` | MIT |\n"
	if err := os.WriteFile(path, []byte(body), 0o644); err != nil {
		t.Fatal(err)
	}
	claims, err := parseLicensingNotice(path)
	if err != nil {
		t.Fatalf("parseLicensingNotice: %v", err)
	}
	// Only the real row produces a claim.
	if len(claims) != 1 || claims[0].Path != "`apps/real`" {
		t.Fatalf("unexpected claims: %+v", claims)
	}
}

func TestSplitMarkdownRow_EmptyLineYieldsSingleEmptyCell(t *testing.T) {
	got := splitMarkdownRow("||")
	if len(got) != 1 || got[0] != "" {
		t.Errorf("expected [\"\"] for ||, got %+v", got)
	}
}

func TestIsMarkdownTableSeparator_EmptyAndDegenerate(t *testing.T) {
	if isMarkdownTableSeparator("||") {
		t.Error("|| should not be a separator")
	}
	if isMarkdownTableSeparator("| a |") {
		t.Error("| a | should not be a separator")
	}
}

func TestParseLicensingNotice_MissingFile(t *testing.T) {
	tmp := t.TempDir()
	_, err := parseLicensingNotice(filepath.Join(tmp, "absent.md"))
	if err == nil || !os.IsNotExist(err) {
		t.Fatalf("expected ErrNotExist, got %v", err)
	}
}

func TestIsMarkdownTableSeparator(t *testing.T) {
	if !isMarkdownTableSeparator("| --- | --- |") {
		t.Error("simple separator should pass")
	}
	if !isMarkdownTableSeparator("|:---|---:|") {
		t.Error("alignment colons should pass")
	}
	if isMarkdownTableSeparator("| a | b |") {
		t.Error("data row should not pass")
	}
	if isMarkdownTableSeparator("not a table line") {
		t.Error("non-table should not pass")
	}
}
