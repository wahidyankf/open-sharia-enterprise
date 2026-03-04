package speccoverage

import (
	"encoding/json"
	"fmt"
	"strings"

	"github.com/wahidyankf/open-sharia-enterprise/libs/golang-commons/timeutil"
)

// FormatText formats the check result as human-readable text.
func FormatText(result *CheckResult, verbose, quiet bool) string {
	var out strings.Builder

	if len(result.Gaps) == 0 {
		if !quiet {
			_, _ = fmt.Fprintf(&out, "✓ Spec coverage valid! %d specs checked, all have matching test files.\n", result.TotalSpecs)
		}
		return out.String()
	}

	_, _ = fmt.Fprintf(&out, "✗ Spec coverage gaps found! %d of %d specs have no matching test file:\n\n", len(result.Gaps), result.TotalSpecs)

	for _, gap := range result.Gaps {
		_, _ = fmt.Fprintf(&out, "  - %s\n    (expected test file with stem: %s)\n", gap.SpecFile, gap.Stem)
	}

	return out.String()
}

// JSONOutput represents the JSON output format for spec coverage.
type JSONOutput struct {
	Status     string    `json:"status"`
	Timestamp  string    `json:"timestamp"`
	TotalSpecs int       `json:"total_specs"`
	GapCount   int       `json:"gap_count"`
	DurationMS int64     `json:"duration_ms"`
	Gaps       []JSONGap `json:"gaps"`
}

// JSONGap represents a single coverage gap in JSON output.
type JSONGap struct {
	SpecFile string `json:"spec_file"`
	Stem     string `json:"stem"`
}

// FormatJSON formats the check result as JSON.
func FormatJSON(result *CheckResult) (string, error) {
	status := "success"
	if len(result.Gaps) > 0 {
		status = "failure"
	}

	gaps := make([]JSONGap, 0, len(result.Gaps))
	for _, g := range result.Gaps {
		gaps = append(gaps, JSONGap(g))
	}

	out := JSONOutput{
		Status:     status,
		Timestamp:  timeutil.Timestamp(),
		TotalSpecs: result.TotalSpecs,
		GapCount:   len(result.Gaps),
		DurationMS: result.Duration.Milliseconds(),
		Gaps:       gaps,
	}

	bytes, err := json.MarshalIndent(out, "", "  ")
	if err != nil {
		return "", err
	}

	return string(bytes), nil
}

// FormatMarkdown formats the check result as markdown (same as text).
func FormatMarkdown(result *CheckResult) string {
	return FormatText(result, false, false)
}
