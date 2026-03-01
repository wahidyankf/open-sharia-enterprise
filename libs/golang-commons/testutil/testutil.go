// Package testutil provides generic testing utilities for CLI commands.
package testutil

import (
	"io"
	"os"
	"testing"
)

// CaptureStdout redirects os.Stdout to a pipe and returns a function that
// restores stdout and returns whatever was written.
func CaptureStdout(t *testing.T) func() string {
	t.Helper()
	r, w, err := os.Pipe()
	if err != nil {
		t.Fatalf("os.Pipe: %v", err)
	}
	orig := os.Stdout
	os.Stdout = w
	t.Cleanup(func() { os.Stdout = orig })
	return func() string {
		_ = w.Close()
		out, _ := io.ReadAll(r)
		return string(out)
	}
}
