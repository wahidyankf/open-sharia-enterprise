// Package severity provides a sealed Severity enum used by DDD validators.
// It follows the same sealed-interface pattern as internal/testcoverage/types.go.
package severity

import (
	"fmt"
	"io"
	"strings"
)

// Severity is the sealed interface for finding severity levels.
//
//sumtype:decl
type Severity interface {
	isSeverity()
	// Code returns the canonical lowercase string code for the severity level.
	Code() string
	// String returns the human-readable representation of the severity level.
	String() string
}

// SeverityError represents an error-level finding that blocks the command.
type SeverityError struct{}

func (SeverityError) isSeverity() {}

// Code implements Severity.
func (SeverityError) Code() string { return "error" }

// String implements Severity.
func (SeverityError) String() string { return "error" }

// SeverityWarn represents a warning-level finding that does not block the command.
type SeverityWarn struct{}

func (SeverityWarn) isSeverity() {}

// Code implements Severity.
func (SeverityWarn) Code() string { return "warn" }

// String implements Severity.
func (SeverityWarn) String() string { return "warn" }

// Parse converts a raw string to a Severity, matching the semantics of the
// original normaliseSeverity functions in cmd/ddd_bc.go and cmd/ddd_ul.go.
// "warn" and "warning" (case-insensitive, trimmed) map to SeverityWarn{};
// everything else (including "error", empty string, and unrecognised values)
// maps to SeverityError{}.
func Parse(s string) Severity {
	switch strings.ToLower(strings.TrimSpace(s)) {
	case "warn", "warning":
		return SeverityWarn{}
	default:
		return SeverityError{}
	}
}

// Resolve determines severity from a CLI flag value and an environment variable
// value, following the priority order documented in the ddd bc/ul commands:
//
//  1. flagVal (non-empty) — parsed via Parse; no stderr output.
//  2. envVal (non-empty) — parsed via Parse; when resolved to SeverityWarn, the
//     audit line `WARN: severity downgraded to "warn" via OSE_RHINO_DDD_SEVERITY env var`
//     is written to stderr.
//  3. Default: SeverityError{}; no stderr output.
//
// The envVar parameter is the environment variable name used in the audit line.
// Callers pass "OSE_RHINO_DDD_SEVERITY" for DDD commands. The envVal parameter
// is the already-looked-up value of that variable (allows callers to inject in tests).
func Resolve(flagVal, envVal string, stderr io.Writer) Severity {
	if flagVal != "" {
		return Parse(flagVal)
	}
	if envVal != "" {
		sev := Parse(envVal)
		if _, ok := sev.(SeverityWarn); ok {
			_, _ = fmt.Fprintln(stderr, `WARN: severity downgraded to "warn" via OSE_RHINO_DDD_SEVERITY env var`)
		}
		return sev
	}
	return SeverityError{}
}
