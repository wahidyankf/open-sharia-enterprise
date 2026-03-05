package cmd

import (
	"bytes"
	"fmt"
	"testing"

	"github.com/spf13/cobra"
)

func TestWriteFormatted_JSONError(t *testing.T) {
	cmd := &cobra.Command{}
	buf := new(bytes.Buffer)
	cmd.SetOut(buf)

	err := writeFormatted(cmd, "json", false, false, outputFuncs{
		text:     func(v, q bool) string { return "text" },
		json:     func() (string, error) { return "", fmt.Errorf("marshal error") },
		markdown: func() string { return "markdown" },
	})

	if err == nil {
		t.Error("expected error from JSON formatter failure")
	}
}

func TestWriteFormatted_TextOutput(t *testing.T) {
	cmd := &cobra.Command{}
	buf := new(bytes.Buffer)
	cmd.SetOut(buf)

	err := writeFormatted(cmd, "text", false, false, outputFuncs{
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

func TestWriteFormatted_UnknownFormat(t *testing.T) {
	cmd := &cobra.Command{}
	buf := new(bytes.Buffer)
	cmd.SetOut(buf)

	// Unknown format falls through to default (text)
	err := writeFormatted(cmd, "xml", false, false, outputFuncs{
		text:     func(v, q bool) string { return "default text" },
		json:     func() (string, error) { return "", nil },
		markdown: func() string { return "markdown" },
	})

	if err != nil {
		t.Errorf("unexpected error: %v", err)
	}
}
