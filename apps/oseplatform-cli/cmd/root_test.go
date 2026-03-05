package cmd

import (
	"bytes"
	"strings"
	"testing"
)

func TestRootCommand_Initialization(t *testing.T) {
	if rootCmd.Use != "oseplatform-cli" {
		t.Errorf("Expected use 'oseplatform-cli', got %s", rootCmd.Use)
	}

	if rootCmd.Short != "CLI tools for oseplatform-web Hugo site" {
		t.Errorf("Expected short description 'CLI tools for oseplatform-web Hugo site', got %s", rootCmd.Short)
	}

	if rootCmd.Version != "0.1.0" {
		t.Errorf("Expected version '0.1.0', got %s", rootCmd.Version)
	}
}

func TestRootCommand_GlobalFlags(t *testing.T) {
	flags := []struct {
		name      string
		shorthand string
	}{
		{"verbose", "v"},
		{"quiet", "q"},
		{"output", "o"},
		{"no-color", ""},
	}

	for _, f := range flags {
		flag := rootCmd.PersistentFlags().Lookup(f.name)
		if flag == nil {
			t.Errorf("Flag --%s not found", f.name)
			continue
		}
		if f.shorthand != "" {
			if rootCmd.PersistentFlags().ShorthandLookup(f.shorthand) == nil {
				t.Errorf("Shorthand flag -%s not found", f.shorthand)
			}
		}
	}
}

func TestExecute_Help(t *testing.T) {
	var buf bytes.Buffer
	rootCmd.SetOut(&buf)
	rootCmd.SetErr(&buf)
	rootCmd.SetArgs([]string{"--help"})
	defer rootCmd.SetArgs(nil)

	// Execute() is safe here: --help always succeeds, so os.Exit is never called.
	// This covers the if-condition in Execute() (the happy path).
	Execute()

	if !strings.Contains(buf.String(), "oseplatform-cli") {
		t.Errorf("Expected help output to contain 'oseplatform-cli', got: %s", buf.String())
	}
}
