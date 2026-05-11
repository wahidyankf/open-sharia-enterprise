// Package cliout provides the sealed OutputFormat enum and a generic
// Dispatcher that routes output to the correct format callback.
package cliout

import "io"

// OutputFormat is the sealed interface for CLI output formats.
//
//sumtype:decl
type OutputFormat interface {
	isOutputFormat()
	// Code returns the canonical lowercase string code for the format.
	Code() string
	// String returns the human-readable representation of the format.
	String() string
}

// FormatText represents plain-text output (default).
type FormatText struct{}

func (FormatText) isOutputFormat() {}

// Code implements OutputFormat.
func (FormatText) Code() string { return "text" }

// String implements OutputFormat.
func (FormatText) String() string { return "text" }

// FormatJSON represents JSON output.
type FormatJSON struct{}

func (FormatJSON) isOutputFormat() {}

// Code implements OutputFormat.
func (FormatJSON) Code() string { return "json" }

// String implements OutputFormat.
func (FormatJSON) String() string { return "json" }

// FormatMarkdown represents markdown output.
type FormatMarkdown struct{}

func (FormatMarkdown) isOutputFormat() {}

// Code implements OutputFormat.
func (FormatMarkdown) Code() string { return "markdown" }

// String implements OutputFormat.
func (FormatMarkdown) String() string { return "markdown" }

// Parse converts a raw string to an OutputFormat. "" and "text" return
// FormatText{}. "json" returns FormatJSON{}. "markdown" returns
// FormatMarkdown{}. All other inputs return nil, false.
func Parse(s string) (OutputFormat, bool) {
	switch s {
	case "", "text":
		return FormatText{}, true
	case "json":
		return FormatJSON{}, true
	case "markdown":
		return FormatMarkdown{}, true
	default:
		return nil, false
	}
}

// Dispatcher routes output to the appropriate format callback. T is the data
// type written by each callback.
type Dispatcher[T any] struct {
	// Text is the callback for plain-text output.
	Text func(w io.Writer, v T) error
	// JSON is the callback for JSON output.
	JSON func(w io.Writer, v T) error
	// Markdown is the callback for markdown output.
	Markdown func(w io.Writer, v T) error
}

// NewDispatcher constructs a Dispatcher with the three format callbacks.
func NewDispatcher[T any](
	text func(w io.Writer, v T) error,
	jsonFn func(w io.Writer, v T) error,
	markdown func(w io.Writer, v T) error,
) Dispatcher[T] {
	return Dispatcher[T]{
		Text:     text,
		JSON:     jsonFn,
		Markdown: markdown,
	}
}

// Write routes to the correct callback based on format and returns any error.
func (d Dispatcher[T]) Write(w io.Writer, format OutputFormat, data T) error {
	switch format.(type) { //nolint:gochecksumtype // exhaustive: all variants covered
	case FormatJSON:
		return d.JSON(w, data)
	case FormatMarkdown:
		return d.Markdown(w, data)
	default: // FormatText and any future text-like default
		return d.Text(w, data)
	}
}
