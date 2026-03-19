package testcoverage

import (
	"path/filepath"
)

// ExcludeFiles removes files matching any of the given glob patterns from the result.
// Exclusion is applied post-parse, pre-aggregate: filtered files are removed from Files
// and the aggregate counts are recalculated.
func ExcludeFiles(r *Result, patterns []string) {
	if len(patterns) == 0 || len(r.Files) == 0 {
		return
	}

	var kept []FileResult
	for _, f := range r.Files {
		if MatchesAnyExcludePattern(f.Path, patterns) {
			continue
		}
		kept = append(kept, f)
	}

	// Recalculate aggregates from kept files
	covered, partial, missed := 0, 0, 0
	for _, f := range kept {
		covered += f.Covered
		partial += f.Partial
		missed += f.Missed
	}

	total := covered + partial + missed
	pct := 100.0
	if total > 0 {
		pct = 100.0 * float64(covered) / float64(total)
	}

	r.Files = kept
	r.Covered = covered
	r.Partial = partial
	r.Missed = missed
	r.Total = total
	r.Pct = pct
	r.Passed = pct >= r.Threshold
}

// MatchesAnyExcludePattern returns true if the path matches any of the given filepath.Match patterns.
func MatchesAnyExcludePattern(path string, patterns []string) bool {
	base := filepath.Base(path)
	for _, pattern := range patterns {
		if matched, _ := filepath.Match(pattern, path); matched {
			return true
		}
		// Also match against just the base name
		if matched, _ := filepath.Match(pattern, base); matched {
			return true
		}
	}
	return false
}
