// Package docs provides functionality for validating documentation file naming conventions.
package docs

import "time"

// ViolationType represents the type of naming violation.
type ViolationType string

const (
	// ViolationMissingSeparator indicates the file is missing the __ separator.
	ViolationMissingSeparator ViolationType = "missing_separator"

	// ViolationWrongPrefix indicates the file has an incorrect prefix for its location.
	ViolationWrongPrefix ViolationType = "wrong_prefix"

	// ViolationBadCase indicates the filename is not in kebab-case.
	ViolationBadCase ViolationType = "bad_case"

	// ViolationMissingPrefix indicates the file is missing a required prefix.
	ViolationMissingPrefix ViolationType = "missing_prefix"
)

// NamingViolation represents a single naming violation found during validation.
type NamingViolation struct {
	FilePath       string        // File path relative to repo root
	FileName       string        // The actual filename
	ViolationType  ViolationType // Type of violation
	ExpectedPrefix string        // The expected prefix for this location
	ActualPrefix   string        // The actual prefix found (if any)
	Message        string        // Human-readable description of the violation
}

// ValidationResult contains the complete results of a naming validation scan.
type ValidationResult struct {
	TotalFiles       int                                 // Total number of files scanned
	ValidFiles       int                                 // Number of files that passed validation
	ViolationCount   int                                 // Total number of violations found
	Violations       []NamingViolation                   // All violations found
	ViolationsByType map[ViolationType][]NamingViolation // Violations grouped by type
	ScanDuration     time.Duration                       // Time taken for the scan
}

// ValidationOptions configures how the naming validation scan should be performed.
type ValidationOptions struct {
	RepoRoot   string // Absolute path to repository root
	StagedOnly bool   // Only scan staged files from git
	Verbose    bool   // Enable verbose logging
	Quiet      bool   // Quiet mode (errors only)
}

// RenameOperation represents a single file rename to fix a naming violation.
type RenameOperation struct {
	OldPath string // Current file path relative to repo root
	NewPath string // New file path relative to repo root
	OldName string // Current filename only
	NewName string // New filename only
}

// LinkUpdate represents a link that needs to be updated after a file rename.
type LinkUpdate struct {
	FilePath   string // File containing the link (relative to repo root)
	LineNumber int    // Line number where the link appears
	OldLink    string // The current link text
	NewLink    string // The updated link text
}

// FixResult contains the results of a fix operation.
type FixResult struct {
	RenameOperations []RenameOperation // Files to rename / renamed
	LinkUpdates      []LinkUpdate      // Links to update / updated
	RenamesApplied   int               // Number of renames successfully applied
	LinksUpdated     int               // Number of links successfully updated
	Errors           []string          // Any errors encountered
	DryRun           bool              // Whether this was a dry run
}

// FixOptions configures the fix operation.
type FixOptions struct {
	RepoRoot    string // Absolute path to repository root
	DryRun      bool   // If true, only show what would be done
	UpdateLinks bool   // If true, update links referencing renamed files
	Verbose     bool   // Enable verbose output
}
