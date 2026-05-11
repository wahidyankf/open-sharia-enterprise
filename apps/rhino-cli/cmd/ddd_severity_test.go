package cmd

import (
	"bytes"
	"os"
	"strings"
	"testing"

	"github.com/wahidyankf/ose-public/apps/rhino-cli/internal/severity"
)

// captureStderr swaps os.Stderr to a pipe, runs fn, returns captured stderr text.
func captureStderr(t *testing.T, fn func()) string {
	t.Helper()
	r, w, err := os.Pipe()
	if err != nil {
		t.Fatalf("pipe: %v", err)
	}
	orig := os.Stderr
	os.Stderr = w
	done := make(chan struct{})
	var buf bytes.Buffer
	go func() {
		_, _ = buf.ReadFrom(r)
		close(done)
	}()
	fn()
	_ = w.Close()
	os.Stderr = orig
	<-done
	return buf.String()
}

// TestResolveBcSeverity_NewEnvVar — Fix #9 scenario A:
// OSE_RHINO_DDD_SEVERITY=warn → SeverityWarn{}, audit stderr line emitted.
func TestResolveBcSeverity_NewEnvVar(t *testing.T) {
	defer os.Unsetenv("OSE_RHINO_DDD_SEVERITY") //nolint:errcheck
	if err := os.Setenv("OSE_RHINO_DDD_SEVERITY", "warn"); err != nil {
		t.Fatal(err)
	}

	var got severity.Severity
	stderr := captureStderr(t, func() {
		got = severity.Resolve("", os.Getenv("OSE_RHINO_DDD_SEVERITY"), os.Stderr)
	})
	if _, isWarn := got.(severity.SeverityWarn); !isWarn {
		t.Errorf("severity: got %T, want SeverityWarn", got)
	}
	if !strings.Contains(stderr, "OSE_RHINO_DDD_SEVERITY") {
		t.Errorf("audit line should mention OSE_RHINO_DDD_SEVERITY; got: %q", stderr)
	}
}

// TestResolveBcSeverity_FlagWins — Fix #9 scenario B:
// --severity=error + env var warn → flag wins, no audit line.
func TestResolveBcSeverity_FlagWins(t *testing.T) {
	defer os.Unsetenv("OSE_RHINO_DDD_SEVERITY") //nolint:errcheck
	if err := os.Setenv("OSE_RHINO_DDD_SEVERITY", "warn"); err != nil {
		t.Fatal(err)
	}

	var got severity.Severity
	stderr := captureStderr(t, func() {
		got = severity.Resolve("error", os.Getenv("OSE_RHINO_DDD_SEVERITY"), os.Stderr)
	})
	if _, isErr := got.(severity.SeverityError); !isErr {
		t.Errorf("severity: got %T, want SeverityError", got)
	}
	if strings.Contains(stderr, "OSE_RHINO_DDD_SEVERITY") {
		t.Errorf("flag wins must skip audit line; got: %q", stderr)
	}
}

// TestResolveBcSeverity_Default — no flag, no env → SeverityError{}, no audit line.
func TestResolveBcSeverity_Default(t *testing.T) {
	_ = os.Unsetenv("OSE_RHINO_DDD_SEVERITY")

	var got severity.Severity
	stderr := captureStderr(t, func() {
		got = severity.Resolve("", os.Getenv("OSE_RHINO_DDD_SEVERITY"), os.Stderr)
	})
	if _, isErr := got.(severity.SeverityError); !isErr {
		t.Errorf("severity: got %T, want SeverityError", got)
	}
	if stderr != "" {
		t.Errorf("default path must emit no stderr; got: %q", stderr)
	}
}

// Mirror coverage for the ul command — same severity.Resolve is used.

// TestResolveUlSeverity_NewEnvVar mirrors the bc test for the ul command path.
func TestResolveUlSeverity_NewEnvVar(t *testing.T) {
	defer os.Unsetenv("OSE_RHINO_DDD_SEVERITY") //nolint:errcheck
	_ = os.Setenv("OSE_RHINO_DDD_SEVERITY", "warn")

	var got severity.Severity
	stderr := captureStderr(t, func() {
		got = severity.Resolve("", os.Getenv("OSE_RHINO_DDD_SEVERITY"), os.Stderr)
	})
	if _, isWarn := got.(severity.SeverityWarn); !isWarn {
		t.Errorf("severity: got %T, want SeverityWarn", got)
	}
	if !strings.Contains(stderr, "OSE_RHINO_DDD_SEVERITY") {
		t.Errorf("audit line should mention OSE_RHINO_DDD_SEVERITY; got: %q", stderr)
	}
}

// TestResolveUlSeverity_FlagWins mirrors the bc flag-wins test for the ul command path.
func TestResolveUlSeverity_FlagWins(t *testing.T) {
	defer os.Unsetenv("OSE_RHINO_DDD_SEVERITY") //nolint:errcheck
	_ = os.Setenv("OSE_RHINO_DDD_SEVERITY", "warn")

	var got severity.Severity
	stderr := captureStderr(t, func() {
		got = severity.Resolve("error", os.Getenv("OSE_RHINO_DDD_SEVERITY"), os.Stderr)
	})
	if _, isErr := got.(severity.SeverityError); !isErr {
		t.Errorf("severity: got %T, want SeverityError", got)
	}
	if strings.Contains(stderr, "OSE_RHINO_DDD_SEVERITY") {
		t.Errorf("flag wins must skip audit line; got: %q", stderr)
	}
}

// TestResolveUlSeverity_Default mirrors the bc default test for the ul command path.
func TestResolveUlSeverity_Default(t *testing.T) {
	_ = os.Unsetenv("OSE_RHINO_DDD_SEVERITY")

	var got severity.Severity
	stderr := captureStderr(t, func() {
		got = severity.Resolve("", os.Getenv("OSE_RHINO_DDD_SEVERITY"), os.Stderr)
	})
	if _, isErr := got.(severity.SeverityError); !isErr {
		t.Errorf("severity: got %T, want SeverityError", got)
	}
	if stderr != "" {
		t.Errorf("default path must emit no stderr; got: %q", stderr)
	}
}
