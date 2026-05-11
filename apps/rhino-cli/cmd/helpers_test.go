package cmd

import (
	"bytes"
	"fmt"
	"testing"

	"github.com/spf13/cobra"
)

func TestWriteFormattedV2_JSONError(t *testing.T) {
	cmd := &cobra.Command{}
	buf := new(bytes.Buffer)
	cmd.SetOut(buf)
	output = "json"
	defer func() { output = "text" }()

	err := writeFormattedV2(cmd, false, false, outputFuncs{
		text:     func(v, q bool) string { return "text" },
		json:     func() (string, error) { return "", fmt.Errorf("marshal error") },
		markdown: func() string { return "markdown" },
	})

	if err == nil {
		t.Error("expected error from JSON formatter failure")
	}
}

func TestWriteFormattedV2_TextOutput(t *testing.T) {
	cmd := &cobra.Command{}
	buf := new(bytes.Buffer)
	cmd.SetOut(buf)
	output = "text"
	defer func() { output = "text" }()

	err := writeFormattedV2(cmd, false, false, outputFuncs{
		text:     func(v, q bool) string { return "text output" },
		json:     func() (string, error) { return "", nil },
		markdown: func() string { return "markdown output" },
	})

	if err != nil {
		t.Errorf("unexpected error: %v", err)
	}
	if buf.String() != "text output" {
		t.Errorf("expected 'text output', got: %q", buf.String())
	}
}

func TestWriteFormattedV2_UnknownFormat(t *testing.T) {
	cmd := &cobra.Command{}
	buf := new(bytes.Buffer)
	cmd.SetOut(buf)
	// Unknown format falls through to default (text) inside writeFormattedV2.
	output = "xml"
	defer func() { output = "text" }()

	err := writeFormattedV2(cmd, false, false, outputFuncs{
		text:     func(v, q bool) string { return "default text" },
		json:     func() (string, error) { return "", nil },
		markdown: func() string { return "markdown" },
	})

	if err != nil {
		t.Errorf("unexpected error: %v", err)
	}
}
