package cmd

import (
	"bytes"
	"fmt"
	"os"
	"testing"

	"github.com/wahidyankf/ose-public/apps/rhino-cli/internal/severity"
)

// TestNewDddCommand verifies that newDddCommand builds a cobra.Command with the
// Use, Short, Long, and Args specified in the spec, and that RunE delegates to
// the ValidatorFn, prints findings, and returns the correct exit status.
func TestNewDddCommand(t *testing.T) {
	t.Run("command metadata from spec", func(t *testing.T) {
		spec := dddCommandSpec{
			Use:   "bc <app>",
			Short: "short description",
			Long:  "long description",
			ValidatorFn: func(_ string, _ string) ([]dddFinding, error) {
				return nil, nil
			},
			FindingsLabel: "ddd bc",
		}
		cmd := newDddCommand(spec)

		if cmd.Use != "bc <app>" {
			t.Errorf("Use: got %q, want %q", cmd.Use, "bc <app>")
		}
		if cmd.Short != "short description" {
			t.Errorf("Short: got %q, want %q", cmd.Short, "short description")
		}
		if cmd.Long != "long description" {
			t.Errorf("Long: got %q, want %q", cmd.Long, "long description")
		}
		if cmd.Args == nil {
			t.Error("Args: expected cobra.ExactArgs(1), got nil")
		}
	})

	t.Run("RunE delegates to ValidatorFn and prints findings", func(t *testing.T) {
		called := false
		spec := dddCommandSpec{
			Use:   "bc <app>",
			Short: "short",
			Long:  "long",
			ValidatorFn: func(repoRoot, app string) ([]dddFinding, error) {
				called = true
				return []dddFinding{
					{
						File:     "some/file.go",
						Severity: severity.SeverityError{},
						Message:  "something is wrong",
					},
				}, nil
			},
			FindingsLabel: "ddd bc",
		}

		// Wire mocks so findGitRoot succeeds.
		origGetwd := osGetwd
		origStat := osStat
		defer func() {
			osGetwd = origGetwd
			osStat = origStat
		}()
		osGetwd = func() (string, error) { return "/mock-repo", nil }
		osStat = func(name string) (os.FileInfo, error) {
			if name == "/mock-repo/.git" {
				return &mockFileInfo{name: ".git", isDir: true}, nil
			}
			return nil, os.ErrNotExist
		}

		cmd := newDddCommand(spec)
		buf := new(bytes.Buffer)
		cmd.SetOut(buf)
		cmd.SetErr(buf)

		err := cmd.RunE(cmd, []string{"organiclever"})
		if !called {
			t.Error("ValidatorFn was not called")
		}
		if err == nil {
			t.Error("expected non-nil error for error findings, got nil")
		}
		if out := buf.String(); out == "" {
			t.Error("expected finding output, got empty string")
		}
	})

	t.Run("RunE returns nil when no error findings", func(t *testing.T) {
		origGetwd := osGetwd
		origStat := osStat
		defer func() {
			osGetwd = origGetwd
			osStat = origStat
		}()
		osGetwd = func() (string, error) { return "/mock-repo", nil }
		osStat = func(name string) (os.FileInfo, error) {
			if name == "/mock-repo/.git" {
				return &mockFileInfo{name: ".git", isDir: true}, nil
			}
			return nil, os.ErrNotExist
		}

		spec := dddCommandSpec{
			Use:   "bc <app>",
			Short: "short",
			Long:  "long",
			ValidatorFn: func(_, _ string) ([]dddFinding, error) {
				return nil, nil
			},
			FindingsLabel: "ddd bc",
		}
		cmd := newDddCommand(spec)
		buf := new(bytes.Buffer)
		cmd.SetOut(buf)
		cmd.SetErr(buf)

		if err := cmd.RunE(cmd, []string{"organiclever"}); err != nil {
			t.Errorf("expected nil error, got: %v", err)
		}
	})

	t.Run("RunE propagates ValidatorFn error", func(t *testing.T) {
		origGetwd := osGetwd
		origStat := osStat
		defer func() {
			osGetwd = origGetwd
			osStat = origStat
		}()
		osGetwd = func() (string, error) { return "/mock-repo", nil }
		osStat = func(name string) (os.FileInfo, error) {
			if name == "/mock-repo/.git" {
				return &mockFileInfo{name: ".git", isDir: true}, nil
			}
			return nil, os.ErrNotExist
		}

		spec := dddCommandSpec{
			Use:   "bc <app>",
			Short: "short",
			Long:  "long",
			ValidatorFn: func(_, _ string) ([]dddFinding, error) {
				return nil, fmt.Errorf("registry not found")
			},
			FindingsLabel: "ddd bc",
		}
		cmd := newDddCommand(spec)
		buf := new(bytes.Buffer)
		cmd.SetOut(buf)
		cmd.SetErr(buf)

		err := cmd.RunE(cmd, []string{"organiclever"})
		if err == nil {
			t.Error("expected error from ValidatorFn propagation, got nil")
		}
	})
}
