package java

import (
	"os"
	"path/filepath"
	"sort"
)

// ScanPackages walks sourceRoot and returns absolute paths of directories that
// contain at least one .java file (package-info.java counts).
// The returned slice is sorted alphabetically.
func ScanPackages(sourceRoot string) ([]string, error) {
	packageSet := make(map[string]bool)

	err := filepath.Walk(sourceRoot, func(path string, info os.FileInfo, err error) error {
		if err != nil {
			return err
		}
		if !info.IsDir() && filepath.Ext(path) == ".java" {
			packageSet[filepath.Dir(path)] = true
		}
		return nil
	})
	if err != nil {
		return nil, err
	}

	packages := make([]string, 0, len(packageSet))
	for dir := range packageSet {
		packages = append(packages, dir)
	}
	sort.Strings(packages)

	return packages, nil
}
