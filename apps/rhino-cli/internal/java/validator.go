package java

import (
	"os"
	"path/filepath"
	"strings"
)

// ValidateAll validates all Java packages in the source root for the required annotation.
// For each package directory it checks:
//  1. package-info.java exists → ViolationMissingPackageInfo if absent
//  2. package-info.java contains @<annotation> → ViolationMissingAnnotation if missing
func ValidateAll(opts ValidationOptions) (*ValidationResult, error) {
	packages, err := ScanPackages(opts.SourceRoot)
	if err != nil {
		return nil, err
	}

	result := &ValidationResult{
		TotalPackages: len(packages),
		AllPackages:   []PackageEntry{},
		Annotation:    opts.Annotation,
	}

	for _, dir := range packages {
		relDir, err := filepath.Rel(opts.SourceRoot, dir)
		if err != nil {
			relDir = dir
		}

		pkgInfoPath := filepath.Join(dir, "package-info.java")

		content, readErr := os.ReadFile(pkgInfoPath)
		if os.IsNotExist(readErr) {
			result.AllPackages = append(result.AllPackages, PackageEntry{
				PackageDir:    relDir,
				Valid:         false,
				ViolationType: ViolationMissingPackageInfo,
			})
			continue
		}
		if readErr != nil {
			return nil, readErr
		}

		if !strings.Contains(string(content), "@"+opts.Annotation) {
			result.AllPackages = append(result.AllPackages, PackageEntry{
				PackageDir:    relDir,
				Valid:         false,
				ViolationType: ViolationMissingAnnotation,
			})
			continue
		}

		result.ValidPackages++
		result.AllPackages = append(result.AllPackages, PackageEntry{
			PackageDir: relDir,
			Valid:      true,
		})
	}

	return result, nil
}
