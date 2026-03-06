// Package testutil provides generic testing utilities for CLI commands.
package testutil

import (
	"fmt"
	"io"
	"os"
	"testing"
)

// osPipe is a package-level variable for dependency injection in tests.
var osPipe = os.Pipe

// CaptureStdout redirects os.Stdout to a pipe and returns a function that
// restores stdout and returns whatever was written.
func CaptureStdout(t *testing.T) func() string {
	t.Helper()
	r, w, err := osPipe()
	if err != nil {
		panic(fmt.Sprintf("testutil.CaptureStdout: os.Pipe failed: %v", err))
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
