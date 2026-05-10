package doctor

import "testing"

// TestToolStatusSealed verifies the sealed ToolStatus interface variants.
func TestToolStatusSealed(t *testing.T) {
	statuses := []ToolStatus{StatusOK{}, StatusWarning{}, StatusMissing{}}
	wantCodes := []string{"ok", "warning", "missing"}
	for i, s := range statuses {
		switch s.(type) {
		case StatusOK:
			if s.Code() != wantCodes[i] {
				t.Errorf("StatusOK.Code()=%q, want %q", s.Code(), wantCodes[i])
			}
		case StatusWarning:
			if s.Code() != wantCodes[i] {
				t.Errorf("StatusWarning.Code()=%q, want %q", s.Code(), wantCodes[i])
			}
		case StatusMissing:
			if s.Code() != wantCodes[i] {
				t.Errorf("StatusMissing.Code()=%q, want %q", s.Code(), wantCodes[i])
			}
		}
	}
}
