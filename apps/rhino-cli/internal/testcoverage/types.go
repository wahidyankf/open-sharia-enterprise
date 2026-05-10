// Package testcoverage provides test coverage measurement using a standard line-based algorithm.
package testcoverage

// Format is a sealed interface for the coverage file format.
//
//sumtype:decl
type Format interface {
	isFormat()
	Code() string
	String() string
}

// FormatGo represents Go cover.out format.
type FormatGo struct{}

func (FormatGo) isFormat() {}

// Code implements Format.
func (FormatGo) Code() string   { return "go" }
func (FormatGo) String() string { return "go" }

// FormatLCOV represents LCOV format.
type FormatLCOV struct{}

func (FormatLCOV) isFormat() {}

// Code implements Format.
func (FormatLCOV) Code() string   { return "lcov" }
func (FormatLCOV) String() string { return "lcov" }

// FormatJaCoCo represents JaCoCo XML format.
type FormatJaCoCo struct{}

func (FormatJaCoCo) isFormat() {}

// Code implements Format.
func (FormatJaCoCo) Code() string   { return "jacoco" }
func (FormatJaCoCo) String() string { return "jacoco" }

// FormatCobertura represents Cobertura XML format.
type FormatCobertura struct{}

func (FormatCobertura) isFormat() {}

// Code implements Format.
func (FormatCobertura) Code() string   { return "cobertura" }
func (FormatCobertura) String() string { return "cobertura" }

// FormatDiff represents diff coverage (computed, not a file format).
type FormatDiff struct{}

func (FormatDiff) isFormat() {}

// Code implements Format.
func (FormatDiff) Code() string   { return "diff" }
func (FormatDiff) String() string { return "diff" }

// FileResult holds coverage statistics for a single source file.
type FileResult struct {
	Path    string  `json:"path"`
	Covered int     `json:"covered"`
	Partial int     `json:"partial"`
	Missed  int     `json:"missed"`
	Total   int     `json:"total"`
	Pct     float64 `json:"pct"`
}

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
	Files     []FileResult `json:"files,omitempty"`
}
