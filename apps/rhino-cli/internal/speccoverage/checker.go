package speccoverage

import (
	"os"
	"path/filepath"
	"strings"
	"time"
)

// CheckAll walks SpecsDir for .feature files and checks each has a matching
// test file anywhere under AppDir. Returns gaps for unmatched specs.
func CheckAll(opts ScanOptions) (*CheckResult, error) {
	start := time.Now()

	specFiles, err := walkFeatureFiles(opts.SpecsDir)
	if err != nil {
		return nil, err
	}

	var gaps []CoverageGap

	for _, specFile := range specFiles {
		stem := strings.TrimSuffix(filepath.Base(specFile), ".feature")

		found, err := hasMatchingTestFile(opts.AppDir, stem)
		if err != nil {
			return nil, err
		}

		if !found {
			relPath, err := filepath.Rel(opts.RepoRoot, specFile)
			if err != nil {
				relPath = specFile
			}
			gaps = append(gaps, CoverageGap{
				SpecFile: relPath,
				Stem:     stem,
			})
		}
	}

	return &CheckResult{
		TotalSpecs: len(specFiles),
		Gaps:       gaps,
		Duration:   time.Since(start),
	}, nil
}

// walkFeatureFiles returns all .feature files under dir recursively.
func walkFeatureFiles(dir string) ([]string, error) {
	var files []string

	if _, err := os.Stat(dir); os.IsNotExist(err) {
		return nil, nil
	}

	err := filepath.Walk(dir, func(path string, info os.FileInfo, err error) error {
		if err != nil {
			return err
		}
		if !info.IsDir() && strings.HasSuffix(path, ".feature") {
			files = append(files, path)
		}
		return nil
	})
	if err != nil {
		return nil, err
	}

	return files, nil
}

// hasMatchingTestFile returns true if any file under appDir has a base name
// that starts with stem+"." or equals stem exactly.
func hasMatchingTestFile(appDir, stem string) (bool, error) {
	if _, err := os.Stat(appDir); os.IsNotExist(err) {
		return false, nil
	}

	found := false

	err := filepath.Walk(appDir, func(path string, info os.FileInfo, err error) error {
		if err != nil {
			return err
		}
		if info.IsDir() {
			return nil
		}

		base := filepath.Base(path)
		if strings.HasPrefix(base, stem+".") || base == stem {
			found = true
			return filepath.SkipAll
		}

		return nil
	})
	if err != nil {
		return false, err
	}

	return found, nil
}
