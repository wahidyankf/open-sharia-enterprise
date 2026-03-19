package testcoverage

import (
	"testing"
)

func TestParseGitDiff_BasicAdd(t *testing.T) {
	diff := `diff --git a/src/foo.ts b/src/foo.ts
--- a/src/foo.ts
+++ b/src/foo.ts
@@ -1,3 +1,5 @@
 line1
+added1
+added2
 line2
 line3
`
	hunks := ParseGitDiff(diff)
	if len(hunks) != 1 {
		t.Fatalf("expected 1 hunk, got %d", len(hunks))
	}
	if hunks[0].FilePath != "src/foo.ts" {
		t.Errorf("expected src/foo.ts, got %s", hunks[0].FilePath)
	}
	if len(hunks[0].ChangedLines) != 2 {
		t.Errorf("expected 2 changed lines, got %d", len(hunks[0].ChangedLines))
	}
	// Lines 2 and 3 in new file
	if hunks[0].ChangedLines[0] != 2 || hunks[0].ChangedLines[1] != 3 {
		t.Errorf("expected lines 2,3 got %v", hunks[0].ChangedLines)
	}
}

func TestParseGitDiff_MultipleFiles(t *testing.T) {
	diff := `diff --git a/a.ts b/a.ts
--- a/a.ts
+++ b/a.ts
@@ -1,2 +1,3 @@
 line1
+added
 line2
diff --git a/b.ts b/b.ts
--- a/b.ts
+++ b/b.ts
@@ -1,2 +1,3 @@
 line1
+added
 line2
`
	hunks := ParseGitDiff(diff)
	if len(hunks) != 2 {
		t.Fatalf("expected 2 hunks, got %d", len(hunks))
	}
}

func TestParseGitDiff_DeleteOnly(t *testing.T) {
	diff := `diff --git a/a.ts b/a.ts
--- a/a.ts
+++ b/a.ts
@@ -1,3 +1,2 @@
 line1
-deleted
 line2
`
	hunks := ParseGitDiff(diff)
	// No added lines
	found := false
	for _, h := range hunks {
		if len(h.ChangedLines) > 0 {
			found = true
		}
	}
	if found {
		t.Error("expected no changed lines for delete-only diff")
	}
}

func TestParseGitDiff_BinarySkipped(t *testing.T) {
	diff := `diff --git a/image.png b/image.png
Binary files a/image.png and b/image.png differ
`
	hunks := ParseGitDiff(diff)
	for _, h := range hunks {
		if len(h.ChangedLines) > 0 {
			t.Error("expected no changes for binary files")
		}
	}
}

func TestParseGitDiff_EmptyDiff(t *testing.T) {
	hunks := ParseGitDiff("")
	if len(hunks) != 0 {
		t.Errorf("expected 0 hunks for empty diff, got %d", len(hunks))
	}
}
