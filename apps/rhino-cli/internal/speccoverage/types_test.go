package speccoverage

import "testing"

// Phase 5B.1 (Fix #15): OrphanStepImpl finding type.

// TestOrphanStepImpl_FieldsAreExported verifies that the OrphanStepImpl
// finding type exists and exposes File, MatcherKind, and MatcherText as
// exported fields. The reverse-direction check populates these fields when
// a step implementation in source code has no matching Gherkin step.
func TestOrphanStepImpl_FieldsAreExported(t *testing.T) {
	o := OrphanStepImpl{
		File:        "apps/example/src/steps.ts",
		MatcherKind: "exact",
		MatcherText: "the orphan step",
	}
	if o.File != "apps/example/src/steps.ts" {
		t.Errorf("File = %q, want %q", o.File, "apps/example/src/steps.ts")
	}
	if o.MatcherKind != "exact" {
		t.Errorf("MatcherKind = %q, want %q", o.MatcherKind, "exact")
	}
	if o.MatcherText != "the orphan step" {
		t.Errorf("MatcherText = %q, want %q", o.MatcherText, "the orphan step")
	}
}

// TestCheckResult_HasOrphanStepImpls verifies that CheckResult exposes the
// new OrphanStepImpls slice for reverse-direction findings.
func TestCheckResult_HasOrphanStepImpls(t *testing.T) {
	r := CheckResult{
		OrphanStepImpls: []OrphanStepImpl{
			{File: "a.ts", MatcherKind: "exact", MatcherText: "a"},
		},
	}
	if len(r.OrphanStepImpls) != 1 {
		t.Errorf("OrphanStepImpls len = %d, want 1", len(r.OrphanStepImpls))
	}
}
