package doctor

import "testing"

// TestScopeSealed_TypeSwitch verifies exhaustive type-switch over Scope variants.
func TestScopeSealed_TypeSwitch(t *testing.T) {
	scopes := []Scope{ScopeFull{}, ScopeMinimal{}}
	for _, s := range scopes {
		switch s.(type) {
		case ScopeFull:
			if s.Code() != "full" {
				t.Errorf("ScopeFull.Code() = %q, want %q", s.Code(), "full")
			}
		case ScopeMinimal:
			if s.Code() != "minimal" {
				t.Errorf("ScopeMinimal.Code() = %q, want %q", s.Code(), "minimal")
			}
		}
	}
}

// TestParseScope verifies CLI string → Scope parsing.
func TestParseScope(t *testing.T) {
	tests := []struct {
		input    string
		wantCode string
		wantOK   bool
	}{
		{"full", "full", true},
		{"minimal", "minimal", true},
		{"", "full", true},   // empty defaults to full
		{"bad", "", false},
	}
	for _, tt := range tests {
		got, ok := ParseScope(tt.input)
		if ok != tt.wantOK {
			t.Errorf("ParseScope(%q) ok=%v, want %v", tt.input, ok, tt.wantOK)
			continue
		}
		if ok && got.Code() != tt.wantCode {
			t.Errorf("ParseScope(%q).Code()=%q, want %q", tt.input, got.Code(), tt.wantCode)
		}
	}
}

// TestScopeSealed_IncompleteSwitch is a TEMPORARY test verifying gochecksumtype
// detects non-exhaustive switches — remove the default case and run lint to confirm.
func TestScopeSealed_IncompleteSwitch_Noop(_ *testing.T) {
	var s Scope = ScopeFull{}
	// This switch intentionally covers all cases — gochecksumtype monitors this.
	switch s.(type) {
	case ScopeFull:
	case ScopeMinimal:
	}
}
