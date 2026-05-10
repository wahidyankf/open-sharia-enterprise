package cmd

import (
	"bytes"
	"os"
	"strings"
	"testing"
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
// OSE_RHINO_DDD_SEVERITY=warn → severity="warn", audit stderr line emitted.
func TestResolveBcSeverity_NewEnvVar(t *testing.T) {
	defer os.Unsetenv("OSE_RHINO_DDD_SEVERITY") //nolint:errcheck
	if err := os.Setenv("OSE_RHINO_DDD_SEVERITY", "warn"); err != nil {
		t.Fatal(err)
	}

	var got string
	stderr := captureStderr(t, func() { got = resolveBcSeverity("") })
	if got != "warn" {
		t.Errorf("severity: got %q, want warn", got)
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

	var got string
	stderr := captureStderr(t, func() { got = resolveBcSeverity("error") })
	if got != "error" {
		t.Errorf("severity: got %q, want error", got)
	}
	if strings.Contains(stderr, "OSE_RHINO_DDD_SEVERITY") {
		t.Errorf("flag wins must skip audit line; got: %q", stderr)
	}
}

// TestResolveBcSeverity_Default — no flag, no env → default "error", no audit line.
func TestResolveBcSeverity_Default(t *testing.T) {
	_ = os.Unsetenv("OSE_RHINO_DDD_SEVERITY")

	var got string
	stderr := captureStderr(t, func() { got = resolveBcSeverity("") })
	if got != "error" {
		t.Errorf("severity: got %q, want error", got)
	}
	if stderr != "" {
		t.Errorf("default path must emit no stderr; got: %q", stderr)
	}
}

// Mirror coverage on resolveUlSeverity.

func TestResolveUlSeverity_NewEnvVar(t *testing.T) {
	defer os.Unsetenv("OSE_RHINO_DDD_SEVERITY") //nolint:errcheck
	_ = os.Setenv("OSE_RHINO_DDD_SEVERITY", "warn")

	var got string
	stderr := captureStderr(t, func() { got = resolveUlSeverity("") })
	if got != "warn" {
		t.Errorf("severity: got %q, want warn", got)
	}
	if !strings.Contains(stderr, "OSE_RHINO_DDD_SEVERITY") {
		t.Errorf("audit line should mention OSE_RHINO_DDD_SEVERITY; got: %q", stderr)
	}
}

func TestResolveUlSeverity_FlagWins(t *testing.T) {
	defer os.Unsetenv("OSE_RHINO_DDD_SEVERITY") //nolint:errcheck
	_ = os.Setenv("OSE_RHINO_DDD_SEVERITY", "warn")

	var got string
	stderr := captureStderr(t, func() { got = resolveUlSeverity("error") })
	if got != "error" {
		t.Errorf("severity: got %q, want error", got)
	}
	if strings.Contains(stderr, "OSE_RHINO_DDD_SEVERITY") {
		t.Errorf("flag wins must skip audit line; got: %q", stderr)
	}
}

func TestResolveUlSeverity_Default(t *testing.T) {
	_ = os.Unsetenv("OSE_RHINO_DDD_SEVERITY")

	var got string
	stderr := captureStderr(t, func() { got = resolveUlSeverity("") })
	if got != "error" {
		t.Errorf("severity: got %q, want error", got)
	}
	if stderr != "" {
		t.Errorf("default path must emit no stderr; got: %q", stderr)
	}
}
