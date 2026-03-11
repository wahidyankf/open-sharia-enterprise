// Package testcoverage provides test coverage measurement using Codecov's line coverage algorithm.
package testcoverage

// Format represents the coverage file format.
type Format string

const (
	// FormatGo represents Go cover.out format.
	FormatGo Format = "go"
	// FormatLCOV represents LCOV format.
	FormatLCOV Format = "lcov"
	// FormatJaCoCo represents JaCoCo XML format.
	FormatJaCoCo Format = "jacoco"
)

// Result holds the computed coverage statistics for a single coverage file.
type Result struct {
	File      string
	Format    Format
	Covered   int
	Partial   int
	Missed    int
	Total     int
	Pct       float64
	Threshold float64
	Passed    bool
}
