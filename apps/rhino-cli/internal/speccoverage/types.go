// Package speccoverage provides functionality for validating BDD spec file coverage.
package speccoverage

import "time"

// ScanOptions configures how the spec coverage check should be performed.
type ScanOptions struct {
	RepoRoot string // Absolute path to repository root
	SpecsDir string // Absolute path to specs directory
	AppDir   string // Absolute path to app directory
	Verbose  bool   // Enable verbose logging
	Quiet    bool   // Quiet mode (errors only)
}

// CoverageGap represents a spec file that has no matching test implementation.
type CoverageGap struct {
	SpecFile string // Path to spec file (relative to repo root)
	Stem     string // Feature file stem (e.g. "user-login" from "user-login.feature")
}

// CheckResult contains the results of a spec coverage check.
type CheckResult struct {
	TotalSpecs int
	Gaps       []CoverageGap
	Duration   time.Duration
}
