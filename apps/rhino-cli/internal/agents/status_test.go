package agents

import (
	"testing"
)

// TestParseStatus_AcceptsKnownStatuses verifies every documented status
// parses without error and Code() round-trips back to the input string.
func TestParseStatus_AcceptsKnownStatuses(t *testing.T) {
	known := []string{"passed", "warning", "failed"}
	for _, s := range known {
		t.Run(s, func(t *testing.T) {
			got, err := ParseStatus(s)
			if err != nil {
				t.Fatalf("ParseStatus(%q) unexpected error: %v", s, err)
			}
			if got.Code() != s {
				t.Errorf("Code() = %q, want %q", got.Code(), s)
			}
		})
	}
}

// TestParseStatus_RejectsUnknown verifies that unknown strings are rejected.
func TestParseStatus_RejectsUnknown(t *testing.T) {
	unknown := []string{"ok", "error", "", "PASSED"}
	for _, s := range unknown {
		t.Run(s, func(t *testing.T) {
			_, err := ParseStatus(s)
			if err == nil {
				t.Fatalf("ParseStatus(%q) expected error, got nil", s)
			}
		})
	}
}

// TestStatusConstants_Codes verifies concrete variant Code() values.
func TestStatusConstants_Codes(t *testing.T) {
	var p StatusPassed
	if p.Code() != "passed" {
		t.Errorf("StatusPassed.Code() = %q, want 'passed'", p.Code())
	}
	var w StatusWarning
	if w.Code() != "warning" {
		t.Errorf("StatusWarning.Code() = %q, want 'warning'", w.Code())
	}
	var f StatusFailed
	if f.Code() != "failed" {
		t.Errorf("StatusFailed.Code() = %q, want 'failed'", f.Code())
	}
}
