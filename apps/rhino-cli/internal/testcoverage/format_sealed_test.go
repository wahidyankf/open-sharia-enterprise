package testcoverage

import "testing"

// TestFormatSealedInterface covers all Format variants and their methods.
func TestFormatSealedInterface(t *testing.T) {
	formats := []Format{FormatGo{}, FormatLCOV{}, FormatJaCoCo{}, FormatCobertura{}, FormatDiff{}}
	wantCodes := []string{"go", "lcov", "jacoco", "cobertura", "diff"}
	for i, f := range formats {
		f.isFormat()
		if f.Code() != wantCodes[i] {
			t.Errorf("Format[%d].Code()=%q, want %q", i, f.Code(), wantCodes[i])
		}
		if f.String() != wantCodes[i] {
			t.Errorf("Format[%d].String()=%q, want %q", i, f.String(), wantCodes[i])
		}
	}
}
