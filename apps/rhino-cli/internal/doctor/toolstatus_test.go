package doctor

import "testing"

// TestToolStatusSealed verifies the sealed ToolStatus interface variants
// including marker methods (coverage of unexported method bodies).
func TestToolStatusSealed(t *testing.T) {
	statuses := []ToolStatus{StatusOK{}, StatusWarning{}, StatusMissing{}}
	wantCodes := []string{"ok", "warning", "missing"}
	for i, s := range statuses {
		s.isToolStatus() // cover marker method body
		if s.Code() != wantCodes[i] {
			t.Errorf("ToolStatus[%d].Code()=%q, want %q", i, s.Code(), wantCodes[i])
		}
		switch s.(type) {
		case StatusOK:
		case StatusWarning:
		case StatusMissing:
		}
	}
}
