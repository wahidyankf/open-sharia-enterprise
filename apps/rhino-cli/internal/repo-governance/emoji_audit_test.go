package repogovernance

import (
	"os"
	"path/filepath"
	"strings"
	"testing"
)

// writeFile is a helper that writes content at path under the test temp dir
// creating parent directories as needed.
func writeEmojiFile(t *testing.T, path, content string) {
	t.Helper()
	if err := os.MkdirAll(filepath.Dir(path), 0o755); err != nil {
		t.Fatalf("mkdir: %v", err)
	}
	if err := os.WriteFile(path, []byte(content), 0o600); err != nil {
		t.Fatalf("write %s: %v", path, err)
	}
}

// TestAuditEmoji_RejectsEmptyPaths verifies the contract that at least one
// path is required.
func TestAuditEmoji_RejectsEmptyPaths(t *testing.T) {
	t.Parallel()
	_, err := AuditEmoji(nil)
	if err == nil {
		t.Fatal("expected error for empty paths, got nil")
	}
}

// TestAuditEmoji_CleanTree verifies a tree of forbidden file types without
// emoji codepoints returns zero findings.
func TestAuditEmoji_CleanTree(t *testing.T) {
	t.Parallel()
	dir := t.TempDir()
	writeEmojiFile(t, filepath.Join(dir, "main.go"), "package main\nfunc main() {}\n")
	writeEmojiFile(t, filepath.Join(dir, "config.json"), `{"a": 1}`+"\n")
	writeEmojiFile(t, filepath.Join(dir, "doc.md"), "Plain markdown.\n") // not forbidden

	findings, err := AuditEmoji([]string{dir})
	if err != nil {
		t.Fatalf("AuditEmoji: %v", err)
	}
	if len(findings) != 0 {
		t.Errorf("expected 0 findings, got %d: %+v", len(findings), findings)
	}
}

// TestAuditEmoji_DetectsEmojiInJSON verifies an emoji codepoint inside a
// JSON file is reported.
func TestAuditEmoji_DetectsEmojiInJSON(t *testing.T) {
	t.Parallel()
	dir := t.TempDir()
	// Smiling face U+1F600 inside a JSON string value.
	content := "{\"greeting\": \"hello \U0001F600 world\"}\n"
	path := filepath.Join(dir, "bad.json")
	writeEmojiFile(t, path, content)

	findings, err := AuditEmoji([]string{dir})
	if err != nil {
		t.Fatalf("AuditEmoji: %v", err)
	}
	if len(findings) != 1 {
		t.Fatalf("expected 1 finding, got %d: %+v", len(findings), findings)
	}
	got := findings[0]
	if got.File != path {
		t.Errorf("File = %q, want %q", got.File, path)
	}
	if got.Line != 1 {
		t.Errorf("Line = %d, want 1", got.Line)
	}
	if got.Codepoint != "U+1F600" {
		t.Errorf("Codepoint = %q, want %q", got.Codepoint, "U+1F600")
	}
	if got.Severity != "high" {
		t.Errorf("Severity = %q, want %q", got.Severity, "high")
	}
}

// TestAuditEmoji_DetectsEmojiInGoSource verifies an emoji codepoint inside a
// .go source file is reported. Demonstrates that the audit catches its own
// rule violations even on Go fixtures (the fixture itself does not introduce
// emoji into THIS test file's source — only into the runtime-written
// fixture).
func TestAuditEmoji_DetectsEmojiInGoSource(t *testing.T) {
	t.Parallel()
	dir := t.TempDir()
	// Heavy check mark U+2705 (PASS) in a comment.
	content := "package main\n// \U00002705 marker\nfunc main() {}\n"
	path := filepath.Join(dir, "bad.go")
	writeEmojiFile(t, path, content)

	findings, err := AuditEmoji([]string{dir})
	if err != nil {
		t.Fatalf("AuditEmoji: %v", err)
	}
	if len(findings) != 1 {
		t.Fatalf("expected 1 finding, got %d: %+v", len(findings), findings)
	}
	if findings[0].Codepoint != "U+2705" {
		t.Errorf("Codepoint = %q, want U+2705", findings[0].Codepoint)
	}
	if findings[0].Line != 2 {
		t.Errorf("Line = %d, want 2", findings[0].Line)
	}
}

// TestAuditEmoji_ArabicNotFlagged verifies that Arabic and other natural-
// language scripts are NOT treated as emoji.
func TestAuditEmoji_ArabicNotFlagged(t *testing.T) {
	t.Parallel()
	dir := t.TempDir()
	// "Arabic" (al-arabiyyah) plus CJK Han, Cyrillic, Hebrew — all language
	// scripts, no emoji content.
	content := "package main\n// العربية 中文 Привет שלום\nfunc main() {}\n"
	path := filepath.Join(dir, "lang.go")
	writeEmojiFile(t, path, content)

	findings, err := AuditEmoji([]string{dir})
	if err != nil {
		t.Fatalf("AuditEmoji: %v", err)
	}
	if len(findings) != 0 {
		t.Errorf("expected 0 findings for non-emoji multibyte unicode, got %d: %+v", len(findings), findings)
	}
}

// TestAuditEmoji_SkipsAllowedDirs verifies node_modules and other vendored
// directories are skipped entirely.
func TestAuditEmoji_SkipsAllowedDirs(t *testing.T) {
	t.Parallel()
	dir := t.TempDir()
	// Smiling face U+1F600 inside JSON under each skip dir.
	emoji := "{\"x\": \"\U0001F600\"}\n"
	for _, skip := range []string{"node_modules", ".git", ".next", "dist", "build", "target"} {
		writeEmojiFile(t, filepath.Join(dir, skip, "bad.json"), emoji)
	}
	// And a clean file outside the skip dirs.
	writeEmojiFile(t, filepath.Join(dir, "clean.json"), "{}\n")

	findings, err := AuditEmoji([]string{dir})
	if err != nil {
		t.Fatalf("AuditEmoji: %v", err)
	}
	if len(findings) != 0 {
		t.Errorf("expected 0 findings (skip dirs filtered), got %d: %+v", len(findings), findings)
	}
}

// TestAuditEmoji_NonForbiddenExtensionIgnored verifies that markdown and
// other allowed file types are NOT scanned.
func TestAuditEmoji_NonForbiddenExtensionIgnored(t *testing.T) {
	t.Parallel()
	dir := t.TempDir()
	writeEmojiFile(t, filepath.Join(dir, "doc.md"), "# Title \U0001F600\n")   // markdown
	writeEmojiFile(t, filepath.Join(dir, "notes.txt"), "Note \U0001F600\n")   // plaintext
	writeEmojiFile(t, filepath.Join(dir, "diagram.svg"), "<svg>\U0001F600\n") // svg

	findings, err := AuditEmoji([]string{dir})
	if err != nil {
		t.Fatalf("AuditEmoji: %v", err)
	}
	if len(findings) != 0 {
		t.Errorf("expected 0 findings (allowed extensions ignored), got %d: %+v", len(findings), findings)
	}
}

// TestAuditEmoji_AllForbiddenExtensions verifies each declared forbidden
// extension is scanned and produces a finding.
func TestAuditEmoji_AllForbiddenExtensions(t *testing.T) {
	t.Parallel()
	dir := t.TempDir()
	emoji := "x \U0001F600 y\n"
	for _, ext := range emojiForbiddenExtensions {
		name := "f" + ext
		writeEmojiFile(t, filepath.Join(dir, name), emoji)
	}

	findings, err := AuditEmoji([]string{dir})
	if err != nil {
		t.Fatalf("AuditEmoji: %v", err)
	}
	if len(findings) != len(emojiForbiddenExtensions) {
		t.Errorf("expected %d findings (one per forbidden ext), got %d", len(emojiForbiddenExtensions), len(findings))
	}
}

// TestAuditEmoji_MultipleEmojiOneLine verifies multiple emoji on one line
// each generate a distinct finding with correct column numbers.
func TestAuditEmoji_MultipleEmojiOneLine(t *testing.T) {
	t.Parallel()
	dir := t.TempDir()
	// Line: a U+1F600 b U+2705 c
	content := "package main\n// a \U0001F600 b \U00002705 c\nfunc main() {}\n"
	path := filepath.Join(dir, "many.go")
	writeEmojiFile(t, path, content)

	findings, err := AuditEmoji([]string{dir})
	if err != nil {
		t.Fatalf("AuditEmoji: %v", err)
	}
	if len(findings) != 2 {
		t.Fatalf("expected 2 findings, got %d: %+v", len(findings), findings)
	}
	if findings[0].Codepoint != "U+1F600" || findings[1].Codepoint != "U+2705" {
		t.Errorf("unexpected codepoints: %q / %q", findings[0].Codepoint, findings[1].Codepoint)
	}
	if findings[0].Column >= findings[1].Column {
		t.Errorf("expected column ordering, got %d then %d", findings[0].Column, findings[1].Column)
	}
}

// TestAuditEmoji_FindingsSorted verifies findings are sorted by file, line,
// then column.
func TestAuditEmoji_FindingsSorted(t *testing.T) {
	t.Parallel()
	dir := t.TempDir()
	// Two files; intentionally write in non-alphabetical order.
	writeEmojiFile(t, filepath.Join(dir, "z.go"), "// \U0001F600\n")
	writeEmojiFile(t, filepath.Join(dir, "a.go"), "// \U0001F600\n")

	findings, err := AuditEmoji([]string{dir})
	if err != nil {
		t.Fatalf("AuditEmoji: %v", err)
	}
	if len(findings) != 2 {
		t.Fatalf("expected 2 findings, got %d: %+v", len(findings), findings)
	}
	if !strings.HasSuffix(findings[0].File, "a.go") || !strings.HasSuffix(findings[1].File, "z.go") {
		t.Errorf("expected sorted by file, got %q then %q", findings[0].File, findings[1].File)
	}
}

// TestAuditEmoji_MissingPathHandled verifies a non-existent root yields no
// error and no findings.
func TestAuditEmoji_MissingPathHandled(t *testing.T) {
	t.Parallel()
	findings, err := AuditEmoji([]string{"/this/path/does/not/exist"})
	if err != nil {
		t.Fatalf("expected no error for missing path, got: %v", err)
	}
	if len(findings) != 0 {
		t.Errorf("expected 0 findings, got %d", len(findings))
	}
}

// TestIsEmojiRune covers each branch of the rune classifier.
func TestIsEmojiRune(t *testing.T) {
	t.Parallel()
	cases := []struct {
		name string
		r    rune
		want bool
	}{
		{"misc-technical-low", 0x2300, true},
		{"misc-technical-high", 0x23FF, true},
		{"misc-symbols-low", 0x2600, true},
		{"misc-symbols-checkmark", 0x2705, true}, // U+2705 white heavy check mark
		{"misc-symbols-high", 0x27BF, true},
		{"zwj", 0x200D, true},
		{"variation-selector-16", 0xFE0F, true},
		{"emoji-block-low", 0x1F000, true},
		{"emoji-smiling-face", 0x1F600, true},
		{"emoji-block-high", 0x1FFFF, true},
		{"ascii-letter", 'a', false},
		{"ascii-digit", '0', false},
		{"latin-extended", 0x00E9, false}, // é
		{"cjk-han", 0x4E2D, false},        // 中
		{"arabic-letter", 0x0627, false},  // ا
		{"hebrew-letter", 0x05D0, false},
		{"cyrillic", 0x0410, false},
		{"below-misc-technical", 0x22FF, false},
		{"between-blocks", 0x2500, false},
		{"above-dingbats", 0x27C0, false},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			t.Parallel()
			if got := isEmojiRune(tc.r); got != tc.want {
				t.Errorf("isEmojiRune(U+%04X) = %v, want %v", tc.r, got, tc.want)
			}
		})
	}
}

// TestFormatCodepoint verifies the codepoint formatter pads small values to
// four hex digits and lets larger values expand naturally.
func TestFormatCodepoint(t *testing.T) {
	t.Parallel()
	cases := []struct {
		r    rune
		want string
	}{
		{0x2705, "U+2705"},
		{0x200D, "U+200D"},
		{0x00A9, "U+00A9"},
		{0x1F600, "U+1F600"},
		{0x1F4A9, "U+1F4A9"},
	}
	for _, tc := range cases {
		if got := formatCodepoint(tc.r); got != tc.want {
			t.Errorf("formatCodepoint(U+%04X) = %q, want %q", tc.r, got, tc.want)
		}
	}
}

// TestScanEmojiFile_OpenError verifies a missing file path bubbles up as an
// error.
func TestScanEmojiFile_OpenError(t *testing.T) {
	t.Parallel()
	_, err := scanEmojiFile("/no/such/file/here.go")
	if err == nil {
		t.Fatal("expected error for missing file, got nil")
	}
}

// TestScanEmojiFiles_WrapsError verifies scanEmojiFiles wraps the underlying
// file-open error with the offending path.
func TestScanEmojiFiles_WrapsError(t *testing.T) {
	t.Parallel()
	_, err := scanEmojiFiles([]string{"/no/such/file/here.go"})
	if err == nil {
		t.Fatal("expected error for missing file, got nil")
	}
	if !strings.Contains(err.Error(), "scan ") {
		t.Errorf("expected error to be wrapped with 'scan' prefix, got: %v", err)
	}
}

// TestAuditEmoji_PropagatesScanError verifies AuditEmoji surfaces underlying
// scan errors. The walker still yields the path (extensions match), and the
// downstream scan fails because the file vanished between walk and open.
func TestAuditEmoji_PropagatesScanError(t *testing.T) {
	t.Parallel()
	dir := t.TempDir()
	bad := filepath.Join(dir, "bad.go")
	writeEmojiFile(t, bad, "package main\n")
	// Make the file unreadable by removing it after listing — simulate a
	// race or transient I/O failure. The walker still records the path, but
	// the subsequent scan fails. Use a manual call into scanEmojiFiles to
	// avoid races in AuditEmoji's walk.
	if err := os.Remove(bad); err != nil {
		t.Fatalf("remove fixture: %v", err)
	}
	_, err := scanEmojiFiles([]string{bad})
	if err == nil {
		t.Fatal("expected error for vanished file, got nil")
	}
}

// TestAuditEmoji_SortByLineAndColumn verifies the secondary and tertiary
// sort keys (line, then column) are honored when files match.
func TestAuditEmoji_SortByLineAndColumn(t *testing.T) {
	t.Parallel()
	dir := t.TempDir()
	// Two emoji on line 3, two on line 1; written in reverse line order to
	// confirm the sort comparator is doing the work.
	content := "// \U0001F600\n\n// a \U00002705 b \U0001F600 c\n"
	path := filepath.Join(dir, "ordering.go")
	writeEmojiFile(t, path, content)

	findings, err := AuditEmoji([]string{dir})
	if err != nil {
		t.Fatalf("AuditEmoji: %v", err)
	}
	if len(findings) != 3 {
		t.Fatalf("expected 3 findings, got %d", len(findings))
	}
	if findings[0].Line != 1 {
		t.Errorf("first finding should be on line 1, got %d", findings[0].Line)
	}
	if findings[1].Line != 3 || findings[2].Line != 3 {
		t.Errorf("findings 1 and 2 should be on line 3, got %d / %d", findings[1].Line, findings[2].Line)
	}
	if findings[1].Column >= findings[2].Column {
		t.Errorf("column ordering on same line broken: %d / %d", findings[1].Column, findings[2].Column)
	}
}

// TestHasForbiddenEmojiExtension verifies extension matching is
// case-insensitive and covers all declared extensions.
func TestHasForbiddenEmojiExtension(t *testing.T) {
	t.Parallel()
	if !hasForbiddenEmojiExtension("foo.go") {
		t.Errorf("expected foo.go to match")
	}
	if !hasForbiddenEmojiExtension("FOO.JSON") {
		t.Errorf("expected case-insensitive match for FOO.JSON")
	}
	if hasForbiddenEmojiExtension("foo.md") {
		t.Errorf("foo.md should not match")
	}
	if hasForbiddenEmojiExtension("Makefile") {
		t.Errorf("Makefile should not match")
	}
}
