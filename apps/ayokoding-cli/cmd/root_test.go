package cmd

import (
	"bytes"
	"strings"
	"testing"
)

func TestExecute_Help(t *testing.T) {
	var buf bytes.Buffer
	rootCmd.SetOut(&buf)
	rootCmd.SetErr(&buf)
	rootCmd.SetArgs([]string{"--help"})
	defer rootCmd.SetArgs(nil)

	// Execute() is safe here: --help always succeeds, so os.Exit is never called.
	// This covers the if-condition in Execute() (the happy path).
	Execute()

	if !strings.Contains(buf.String(), "ayokoding-cli") {
		t.Errorf("Expected help output to contain 'ayokoding-cli', got: %s", buf.String())
	}
}

func TestExecute_Error(t *testing.T) {
	var capturedCode int
	origOsExit := osExit
	osExit = func(code int) { capturedCode = code }
	defer func() { osExit = origOsExit }()

	var errBuf bytes.Buffer
	rootCmd.SetErr(&errBuf)
	rootCmd.SetArgs([]string{"--unknown-flag-xyz"})
	defer func() {
		rootCmd.SetErr(nil)
		rootCmd.SetArgs(nil)
	}()

	Execute()

	if capturedCode != 1 {
		t.Errorf("expected exit code 1, got %d", capturedCode)
	}
}
