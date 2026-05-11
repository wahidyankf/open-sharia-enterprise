// Package cliout_test tests the OutputFormat sealed enum and Dispatcher.
package cliout_test

import (
	"bytes"
	"fmt"
	"io"
	"strings"
	"testing"

	"github.com/wahidyankf/ose-public/apps/rhino-cli/internal/cliout"
)

// TestParse_RecognisesThreeLiterals verifies that "text", "json", and "markdown"
// are accepted and map to the correct OutputFormat variant.
func TestParse_RecognisesThreeLiterals(t *testing.T) {
	t.Parallel()

	cases := []struct {
		input string
	}{
		{"text"},
		{"json"},
		{"markdown"},
	}
	for _, tc := range cases {
		t.Run(tc.input, func(t *testing.T) {
			t.Parallel()
			got, ok := cliout.Parse(tc.input)
			if !ok {
				t.Fatalf("Parse(%q) returned ok=false, want true", tc.input)
			}
			if got == nil {
				t.Fatalf("Parse(%q) returned nil format", tc.input)
			}
			if got.Code() != tc.input {
				t.Errorf("Parse(%q).Code() = %q, want %q", tc.input, got.Code(), tc.input)
			}
		})
	}
}

// TestParse_EmptyDefaultsToText verifies that an empty string maps to FormatText{}.
func TestParse_EmptyDefaultsToText(t *testing.T) {
	t.Parallel()

	got, ok := cliout.Parse("")
	if !ok {
		t.Fatalf("Parse('') returned ok=false, want true")
	}
	if _, isText := got.(cliout.FormatText); !isText {
		t.Errorf("Parse('') = %T, want cliout.FormatText", got)
	}
}

// TestParse_RejectsUnknown verifies that unrecognised format strings return nil, false.
func TestParse_RejectsUnknown(t *testing.T) {
	t.Parallel()

	unknowns := []string{"yaml", "xml"}
	for _, u := range unknowns {
		t.Run(u, func(t *testing.T) {
			t.Parallel()
			got, ok := cliout.Parse(u)
			if ok {
				t.Errorf("Parse(%q) returned ok=true, want false", u)
			}
			if got != nil {
				t.Errorf("Parse(%q) returned non-nil format %T, want nil", u, got)
			}
		})
	}
}

// TestDispatcher_Write_RoutesToCorrectFormatter verifies that the Dispatcher
// routes to the appropriate callback for each format.
func TestDispatcher_Write_RoutesToCorrectFormatter(t *testing.T) {
	t.Parallel()

	tests := []struct {
		format     cliout.OutputFormat
		wantPrefix string
	}{
		{cliout.FormatText{}, "text:"},
		{cliout.FormatJSON{}, "json:"},
		{cliout.FormatMarkdown{}, "markdown:"},
	}

	for _, tc := range tests {
		t.Run(tc.format.Code(), func(t *testing.T) {
			t.Parallel()
			var buf bytes.Buffer
			disp := cliout.NewDispatcher[string](
				func(w io.Writer, v string) error { _, _ = fmt.Fprint(w, "text:"+v); return nil },
				func(w io.Writer, v string) error { _, _ = fmt.Fprint(w, "json:"+v); return nil },
				func(w io.Writer, v string) error { _, _ = fmt.Fprint(w, "markdown:"+v); return nil },
			)
			if err := disp.Write(&buf, tc.format, "data"); err != nil {
				t.Fatalf("Write() error: %v", err)
			}
			if !strings.HasPrefix(buf.String(), tc.wantPrefix) {
				t.Errorf("output = %q, want prefix %q", buf.String(), tc.wantPrefix)
			}
		})
	}
}

// TestDispatcher_Write_JSONErrorBubblesUp verifies that a JSON formatter error
// is returned from Write.
func TestDispatcher_Write_JSONErrorBubblesUp(t *testing.T) {
	t.Parallel()

	var buf bytes.Buffer
	sentinel := fmt.Errorf("marshal failed")
	disp := cliout.NewDispatcher[string](
		func(w io.Writer, v string) error { return nil },
		func(w io.Writer, v string) error { return sentinel },
		func(w io.Writer, v string) error { return nil },
	)
	err := disp.Write(&buf, cliout.FormatJSON{}, "data")
	if err == nil {
		t.Fatal("expected non-nil error from Write, got nil")
	}
}
