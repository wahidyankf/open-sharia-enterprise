package agents

import "fmt"

// Closed-set enumeration of validation check status values used in
// ValidationCheck.Status and reporter.go (audited 2026-05-11).
//
// Status values:
//   - passed  — check evaluated affirmatively
//   - warning — advisory finding; does not fail validation
//   - failed  — check evaluated negatively; drives non-zero exit code

//sumtype:decl

// Status is a sealed interface representing all valid validation check
// status values.
type Status interface {
	isStatus()
	// Code returns the string representation used in JSON/text output.
	Code() string
	// String returns the human-readable representation.
	String() string
}

// StatusPassed represents a passing validation check.
type StatusPassed struct{}

func (StatusPassed) isStatus() {}

// Code implements Status.
func (StatusPassed) Code() string { return "passed" }

// String implements Status.
func (StatusPassed) String() string { return "passed" }

// StatusWarning represents a validation check with advisory warnings.
type StatusWarning struct{}

func (StatusWarning) isStatus() {}

// Code implements Status.
func (StatusWarning) Code() string { return "warning" }

// String implements Status.
func (StatusWarning) String() string { return "warning" }

// StatusFailed represents a failed validation check.
type StatusFailed struct{}

func (StatusFailed) isStatus() {}

// Code implements Status.
func (StatusFailed) Code() string { return "failed" }

// String implements Status.
func (StatusFailed) String() string { return "failed" }

// ParseStatus parses a string into a Status enum value.
// Returns an error for unknown status strings.
func ParseStatus(s string) (Status, error) {
	switch s {
	case "passed":
		return StatusPassed{}, nil
	case "warning":
		return StatusWarning{}, nil
	case "failed":
		return StatusFailed{}, nil
	default:
		return nil, fmt.Errorf("unknown status %q: must be one of passed, warning, failed", s)
	}
}
