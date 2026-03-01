// Package java provides functionality for validating Java package null safety annotations.
package java

// ViolationType represents the category of a package violation.
type ViolationType string

const (
	// ViolationMissingPackageInfo indicates a directory has .java files but no package-info.java.
	ViolationMissingPackageInfo ViolationType = "missing_package_info"
	// ViolationMissingAnnotation indicates package-info.java exists but lacks the required annotation.
	ViolationMissingAnnotation ViolationType = "missing_annotation"
)

// PackageEntry represents a single Java package with its validation status.
type PackageEntry struct {
	PackageDir    string        // Relative path from source root
	Valid         bool          // Whether the package passed validation
	ViolationType ViolationType // Only set when Valid is false
}

// ValidationResult contains the complete results of a null safety validation scan.
type ValidationResult struct {
	TotalPackages int            // Total number of Java packages scanned
	ValidPackages int            // Number of packages that passed validation
	AllPackages   []PackageEntry // All packages in sorted order
	Annotation    string         // Annotation that was required
}

// ValidationOptions configures how the validation should be performed.
type ValidationOptions struct {
	SourceRoot string // Absolute path to Java source root
	Annotation string // Annotation name to require (e.g., "NullMarked")
}
