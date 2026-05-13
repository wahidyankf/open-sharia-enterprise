package allowlist

import (
	"sort"
	"testing"
)

// TestAppsWithDDD_ExactMembership asserts that AppsWithDDD contains exactly
// the four expected apps (no CLIs, no extras). Adding or removing an app
// here is a deliberate governance decision and must update this test.
func TestAppsWithDDD_ExactMembership(t *testing.T) {
	want := []string{"ayokoding", "organiclever", "ose-platform", "wahidyankf"}

	got := append([]string(nil), AppsWithDDD...)
	sort.Strings(got)

	if len(got) != len(want) {
		t.Fatalf("AppsWithDDD length = %d, want %d; got = %v", len(got), len(want), got)
	}
	for i := range want {
		if got[i] != want[i] {
			t.Errorf("AppsWithDDD[%d] = %q, want %q (sorted comparison)", i, got[i], want[i])
		}
	}
}

// TestAppsWithDDD_ExcludesCLIs asserts that no CLI app names leak into the
// allowlist. CLI apps adopt BDD only (spec-coverage) and are excluded by design.
func TestAppsWithDDD_ExcludesCLIs(t *testing.T) {
	cliApps := []string{
		"ayokoding-cli",
		"ose-cli",
		"rhino-cli",
	}
	for _, cli := range cliApps {
		for _, app := range AppsWithDDD {
			if app == cli {
				t.Errorf("AppsWithDDD must not contain CLI app %q (CLI apps adopt BDD only)", cli)
			}
		}
	}
}

// TestAppsWithDDD_NoExtras asserts the four apps and only those four. Any
// extra entry (typo, premature inclusion) must fail this test.
func TestAppsWithDDD_NoExtras(t *testing.T) {
	expected := map[string]bool{
		"organiclever": true,
		"wahidyankf":   true,
		"ose-platform": true,
		"ayokoding":    true,
	}
	for _, app := range AppsWithDDD {
		if !expected[app] {
			t.Errorf("unexpected app %q in AppsWithDDD; expected only %v", app, mapKeys(expected))
		}
	}
}

func mapKeys(m map[string]bool) []string {
	keys := make([]string, 0, len(m))
	for k := range m {
		keys = append(keys, k)
	}
	sort.Strings(keys)
	return keys
}
