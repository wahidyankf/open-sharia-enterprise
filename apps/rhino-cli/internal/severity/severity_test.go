// Package severity_test tests the sealed Severity enum and resolver.
package severity_test

import (
	"bytes"
	"strings"
	"testing"

	"github.com/wahidyankf/ose-public/apps/rhino-cli/internal/severity"
)

// TestParse_AcceptsWarnVariants verifies "warn" / "warning" / "WARN" all produce SeverityWarn{}.
func TestParse_AcceptsWarnVariants(t *testing.T) {
	t.Parallel()

	inputs := []string{"warn", "warning", "WARN"}
	for _, input := range inputs {
		t.Run(input, func(t *testing.T) {
			t.Parallel()
			got := severity.Parse(input)
			if _, ok := got.(severity.SeverityWarn); !ok {
				t.Errorf("Parse(%q) = %T, want SeverityWarn", input, got)
			}
			if got.Code() != "warn" {
				t.Errorf("Parse(%q).Code() = %q, want %q", input, got.Code(), "warn")
			}
		})
	}
}

// TestParse_DefaultsToError verifies non-warn inputs produce SeverityError{}.
func TestParse_DefaultsToError(t *testing.T) {
	t.Parallel()

	inputs := []string{"error", "fatal", "", "garbage"}
	for _, input := range inputs {
		t.Run(input, func(t *testing.T) {
			t.Parallel()
			got := severity.Parse(input)
			if _, ok := got.(severity.SeverityError); !ok {
				t.Errorf("Parse(%q) = %T, want SeverityError", input, got)
			}
			if got.Code() != "error" {
				t.Errorf("Parse(%q).Code() = %q, want %q", input, got.Code(), "error")
			}
		})
	}
}

// TestResolve_FlagBeatsEnv verifies that when a flag value is provided it wins
// and no stderr output is emitted.
func TestResolve_FlagBeatsEnv(t *testing.T) {
	t.Parallel()

	var stderr bytes.Buffer
	got := severity.Resolve("warn", "error", &stderr)
	if _, ok := got.(severity.SeverityWarn); !ok {
		t.Errorf("Resolve(flag=warn, env=error) = %T, want SeverityWarn", got)
	}
	if stderr.Len() != 0 {
		t.Errorf("expected no stderr output when flag beats env; got: %q", stderr.String())
	}
}

// TestResolve_EnvUsedWhenFlagEmpty verifies that when flag is empty the env
// value is used and the documented audit line is written to stderr.
func TestResolve_EnvUsedWhenFlagEmpty(t *testing.T) {
	t.Parallel()

	var stderr bytes.Buffer
	got := severity.Resolve("", "warn", &stderr)
	if _, ok := got.(severity.SeverityWarn); !ok {
		t.Errorf("Resolve(flag='', env=warn) = %T, want SeverityWarn", got)
	}
	line := stderr.String()
	if !strings.Contains(line, "OSE_RHINO_DDD_SEVERITY") {
		t.Errorf("expected stderr to mention OSE_RHINO_DDD_SEVERITY; got: %q", line)
	}
}

// TestResolve_DefaultsToError verifies that when both flag and env are empty
// SeverityError{} is returned and nothing is written to stderr.
func TestResolve_DefaultsToError(t *testing.T) {
	t.Parallel()

	var stderr bytes.Buffer
	got := severity.Resolve("", "", &stderr)
	if _, ok := got.(severity.SeverityError); !ok {
		t.Errorf("Resolve(flag='', env='') = %T, want SeverityError", got)
	}
	if stderr.Len() != 0 {
		t.Errorf("expected no stderr output for default path; got: %q", stderr.String())
	}
}

// TestSeverity_ExhaustiveSwitch demonstrates that gochecksumtype would catch a
// non-exhaustive switch over severity.Severity. This test itself is exhaustive;
// the nolint comment on the incomplete function below is the negative-test
// documentation pattern.
func TestSeverity_ExhaustiveSwitch(t *testing.T) {
	t.Parallel()

	// Exhaustive switch — should compile with no linter complaints.
	sev := severity.Parse("warn")
	switch sev.(type) { //nolint:gochecksumtype // intentional negative test
	case severity.SeverityError:
		t.Fatal("expected SeverityWarn, got SeverityError")
	case severity.SeverityWarn:
		// correct
	}
}
