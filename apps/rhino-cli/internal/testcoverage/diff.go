package testcoverage

import (
	"fmt"
	"os/exec"
	"strings"
)

// DiffCoverageOptions configures diff coverage calculation.
type DiffCoverageOptions struct {
	CoverageFile    string
	Base            string // git ref to diff against (default: "main")
	Staged          bool   // diff staged changes instead of branch diff
	Threshold       float64
	PerFile         bool
	ExcludePatterns []string
}

// ComputeDiffCoverage calculates coverage for only the changed lines.
func ComputeDiffCoverage(opts DiffCoverageOptions) (Result, error) {
	// Get the diff
	diffOutput, err := getGitDiff(opts.Base, opts.Staged)
	if err != nil {
		return Result{}, fmt.Errorf("failed to get git diff: %w", err)
	}

	// Parse changed lines
	hunks := ParseGitDiff(diffOutput)
	if len(hunks) == 0 {
		return Result{
			File:      opts.CoverageFile,
			Format:    FormatDiff,
			Pct:       100.0,
			Threshold: opts.Threshold,
			Passed:    true,
		}, nil
	}

	// Parse coverage data
	cm, err := ToCoverageMap(opts.CoverageFile)
	if err != nil {
		return Result{}, err
	}

	// Apply exclusions
	if len(opts.ExcludePatterns) > 0 {
		for filePath := range cm {
			if MatchesAnyExcludePattern(filePath, opts.ExcludePatterns) {
				delete(cm, filePath)
			}
		}
	}

	// Cross-reference changed lines with coverage
	covered, partial, missed := 0, 0, 0
	var perFile []FileResult

	for _, hunk := range hunks {
		if len(opts.ExcludePatterns) > 0 && MatchesAnyExcludePattern(hunk.FilePath, opts.ExcludePatterns) {
			continue
		}

		fc, fp, fm := 0, 0, 0
		fileCov := cm[hunk.FilePath]

		for _, lineNo := range hunk.ChangedLines {
			if fileCov == nil {
				fm++
				continue
			}
			lc, ok := fileCov[lineNo]
			if !ok {
				// Line not in coverage report — could be non-executable
				continue
			}
			if lc.HitCount > 0 {
				if hasMissedBranch(lc.Branches) {
					fp++
				} else {
					fc++
				}
			} else {
				fm++
			}
		}

		covered += fc
		partial += fp
		missed += fm

		ft := fc + fp + fm
		if ft > 0 {
			fpct := 100.0 * float64(fc) / float64(ft)
			perFile = append(perFile, FileResult{
				Path: hunk.FilePath, Covered: fc, Partial: fp, Missed: fm, Total: ft, Pct: fpct,
			})
		}
	}

	total := covered + partial + missed
	pct := 100.0
	if total > 0 {
		pct = 100.0 * float64(covered) / float64(total)
	}

	return Result{
		File:      opts.CoverageFile,
		Format:    FormatDiff,
		Covered:   covered,
		Partial:   partial,
		Missed:    missed,
		Total:     total,
		Pct:       pct,
		Threshold: opts.Threshold,
		Passed:    opts.Threshold == 0 || pct >= opts.Threshold,
		Files:     perFile,
	}, nil
}

// getGitDiff runs git diff and returns the output.
var getGitDiff = func(base string, staged bool) (string, error) {
	var args []string
	if staged {
		args = []string{"diff", "--staged", "--unified=0"}
	} else {
		if base == "" {
			base = "main"
		}
		args = []string{"diff", "--unified=0", base + "...HEAD"}
	}

	cmd := exec.Command("git", args...)
	out, err := cmd.Output()
	if err != nil {
		// If diff fails (e.g., no commits), return empty diff
		if exitErr, ok := err.(*exec.ExitError); ok {
			return "", fmt.Errorf("git diff failed: %s", strings.TrimSpace(string(exitErr.Stderr)))
		}
		return "", err
	}
	return string(out), nil
}
