package cmd

import (
	"bytes"
	"strings"
	"testing"
)

// TestSpecsHelpHasNoDriftRoutes asserts that drift-* placeholder commands
// have been removed from the `specs` subcommand tree (Fix #7).
func TestSpecsHelpHasNoDriftRoutes(t *testing.T) {
	buf := &bytes.Buffer{}
	specsCmd.SetOut(buf)
	specsCmd.SetErr(buf)
	if err := specsCmd.Help(); err != nil {
		t.Fatalf("specs --help failed: %v", err)
	}
	out := buf.String()

	for _, banned := range []string{"drift-routes", "drift-endpoints", "drift-contracts"} {
		if strings.Contains(out, banned) {
			t.Errorf("specs --help still contains %q — drift-* placeholder must be removed (Fix #7)", banned)
		}
	}
}

// TestSpecsHelpListsValidateCommands asserts that the four allowlist-driven
// validators are advertised by `specs --help` after the drift-* removal.
func TestSpecsHelpListsValidateCommands(t *testing.T) {
	buf := &bytes.Buffer{}
	specsCmd.SetOut(buf)
	specsCmd.SetErr(buf)
	if err := specsCmd.Help(); err != nil {
		t.Fatalf("specs --help failed: %v", err)
	}
	out := buf.String()

	for _, want := range []string{"validate-adoption", "validate-tree", "validate-counts", "validate-links"} {
		if !strings.Contains(out, want) {
			t.Errorf("specs --help missing expected subcommand %q", want)
		}
	}
}
