package cmd

import (
	"bytes"
	"context"
	"fmt"
	"os"
	"path/filepath"
	"runtime"
	"strings"
	"testing"

	"github.com/cucumber/godog"
	governance "github.com/wahidyankf/ose-public/apps/rhino-cli/internal/repo-governance"
)

// Step constant patterns for repo-governance agents-md-size scenarios. The
// size literal is captured so the same step impl drives all three scenarios.
const (
	stepAgentsMdSizeFile                      = `^a repository containing an AGENTS\.md file of (\d+) bytes$`
	stepDeveloperRunsAgentsMdSize             = `^the developer runs repo-governance agents-md-size$`
	stepOutputReportsAgentsMdWithinTarget     = `^the output reports the AGENTS\.md size as within target$`
	stepOutputIdentifiesAgentsMdOverTarget    = `^the output identifies AGENTS\.md as over the target size$`
	stepOutputIdentifiesAgentsMdOverHardLimit = `^the output identifies AGENTS\.md as over the hard limit$`
)

var specsDirUnitGovernanceAgentsMdSize = func() string {
	_, f, _, _ := runtime.Caller(0)
	return filepath.Join(filepath.Dir(f), "../../../specs/apps/rhino/behavior/cli/gherkin")
}()

type governanceAgentsMdSizeUnitSteps struct {
	cmdErr    error
	cmdOutput string
	lastSize  int64
}

func (s *governanceAgentsMdSizeUnitSteps) before(_ context.Context, _ *godog.Scenario) (context.Context, error) {
	verbose = false
	quiet = false
	output = "text"
	s.cmdErr = nil
	s.cmdOutput = ""
	s.lastSize = 0

	osGetwd = func() (string, error) { return "/mock-repo", nil }
	osStat = func(name string) (os.FileInfo, error) {
		if name == "/mock-repo/.git" {
			return &mockFileInfo{name: ".git", isDir: true}, nil
		}
		return nil, os.ErrNotExist
	}
	agentsMdSizeFn = func(_ string) (governance.AgentsMdSizeFinding, error) {
		return governance.AgentsMdSizeFinding{
			File:     "/mock-repo/AGENTS.md",
			Size:     0,
			Severity: "ok",
			Message:  "AGENTS.md is 0 bytes (within 30000-byte target)",
		}, nil
	}
	return context.Background(), nil
}

func (s *governanceAgentsMdSizeUnitSteps) after(_ context.Context, _ *godog.Scenario, _ error) (context.Context, error) {
	agentsMdSizeFn = governance.CheckAgentsMdSize
	osGetwd = os.Getwd
	osStat = os.Stat
	return context.Background(), nil
}

// agentsMdFileOfBytes sets the mock to return a finding consistent with the
// given size, classified against the production thresholds.
func (s *governanceAgentsMdSizeUnitSteps) agentsMdFileOfBytes(size int64) error {
	s.lastSize = size
	severity, message := classifyForMock(size)
	agentsMdSizeFn = func(_ string) (governance.AgentsMdSizeFinding, error) {
		return governance.AgentsMdSizeFinding{
			File:     "/mock-repo/AGENTS.md",
			Size:     size,
			Severity: severity,
			Message:  message,
		}, nil
	}
	return nil
}

// classifyForMock mirrors the production threshold logic for setting up the
// mock — kept local to the unit test so the mock returns plausible findings.
func classifyForMock(size int64) (severity, message string) {
	switch {
	case size <= governance.AgentsMdTargetSize:
		return "ok", fmt.Sprintf("AGENTS.md is %d bytes (within %d-byte target)", size, governance.AgentsMdTargetSize)
	case size <= governance.AgentsMdWarningSize:
		return "warn", fmt.Sprintf("AGENTS.md is %d bytes (over %d-byte target)", size, governance.AgentsMdTargetSize)
	case size <= governance.AgentsMdHardLimitSize:
		return "warn", fmt.Sprintf("AGENTS.md is %d bytes (over %d-byte warning threshold)", size, governance.AgentsMdWarningSize)
	default:
		return "fail", fmt.Sprintf("AGENTS.md is %d bytes (over %d-byte hard limit)", size, governance.AgentsMdHardLimitSize)
	}
}

func (s *governanceAgentsMdSizeUnitSteps) run() error {
	buf := new(bytes.Buffer)
	agentsMdSizeCmd.SetOut(buf)
	agentsMdSizeCmd.SetErr(buf)
	s.cmdErr = agentsMdSizeCmd.RunE(agentsMdSizeCmd, []string{})
	s.cmdOutput = buf.String()
	return nil
}

func (s *governanceAgentsMdSizeUnitSteps) exitsSuccessfully() error {
	if s.cmdErr != nil {
		return fmt.Errorf("expected success but got: %w\nOutput: %s", s.cmdErr, s.cmdOutput)
	}
	return nil
}

func (s *governanceAgentsMdSizeUnitSteps) exitsWithFailure() error {
	if s.cmdErr == nil {
		return fmt.Errorf("expected failure but succeeded\nOutput: %s", s.cmdOutput)
	}
	return nil
}

func (s *governanceAgentsMdSizeUnitSteps) outputReportsWithinTarget() error {
	if !strings.Contains(s.cmdOutput, "PASS") {
		return fmt.Errorf("expected PASS label in output, got: %s", s.cmdOutput)
	}
	if !strings.Contains(s.cmdOutput, "within") {
		return fmt.Errorf("expected 'within' in output, got: %s", s.cmdOutput)
	}
	return nil
}

func (s *governanceAgentsMdSizeUnitSteps) outputIdentifiesOverTarget() error {
	if !strings.Contains(s.cmdOutput, "WARN") {
		return fmt.Errorf("expected WARN label in output, got: %s", s.cmdOutput)
	}
	if !strings.Contains(s.cmdOutput, "over") {
		return fmt.Errorf("expected 'over' in output, got: %s", s.cmdOutput)
	}
	return nil
}

func (s *governanceAgentsMdSizeUnitSteps) outputIdentifiesOverHardLimit() error {
	if !strings.Contains(s.cmdOutput, "FAIL") {
		return fmt.Errorf("expected FAIL label in output, got: %s", s.cmdOutput)
	}
	if !strings.Contains(s.cmdOutput, "hard limit") {
		return fmt.Errorf("expected 'hard limit' in output, got: %s", s.cmdOutput)
	}
	return nil
}

func TestUnitGovernanceAgentsMdSize(t *testing.T) {
	s := &governanceAgentsMdSizeUnitSteps{}
	suite := godog.TestSuite{
		ScenarioInitializer: func(sc *godog.ScenarioContext) {
			sc.Before(s.before)
			sc.After(s.after)
			sc.Step(stepAgentsMdSizeFile, s.agentsMdFileOfBytes)
			sc.Step(stepDeveloperRunsAgentsMdSize, s.run)
			sc.Step(stepExitsSuccessfully, s.exitsSuccessfully)
			sc.Step(stepExitsWithFailure, s.exitsWithFailure)
			sc.Step(stepOutputReportsAgentsMdWithinTarget, s.outputReportsWithinTarget)
			sc.Step(stepOutputIdentifiesAgentsMdOverTarget, s.outputIdentifiesOverTarget)
			sc.Step(stepOutputIdentifiesAgentsMdOverHardLimit, s.outputIdentifiesOverHardLimit)
		},
		Options: &godog.Options{
			Format:   "pretty",
			Paths:    []string{specsDirUnitGovernanceAgentsMdSize},
			TestingT: t,
			Tags:     "repo-governance-agents-md-size",
		},
	}
	if suite.Run() != 0 {
		t.Fatal("non-zero status returned, failed to run unit feature tests")
	}
}

// TestGovernanceAgentsMdSize_MissingGitRoot verifies the command fails
// gracefully when not inside a git repository.
func TestGovernanceAgentsMdSize_MissingGitRoot(t *testing.T) {
	origGetwd := osGetwd
	origStat := osStat
	defer func() {
		osGetwd = origGetwd
		osStat = origStat
	}()

	osGetwd = func() (string, error) { return "/no-git-here", nil }
	osStat = func(_ string) (os.FileInfo, error) { return nil, os.ErrNotExist }

	buf := new(bytes.Buffer)
	agentsMdSizeCmd.SetOut(buf)
	agentsMdSizeCmd.SetErr(buf)

	err := agentsMdSizeCmd.RunE(agentsMdSizeCmd, []string{})
	if err == nil || !strings.Contains(err.Error(), "git") {
		t.Fatalf("expected git-root error, got: %v", err)
	}
}

// TestGovernanceAgentsMdSize_AuditError verifies an audit-side error (e.g.,
// missing AGENTS.md) is propagated wrapped.
func TestGovernanceAgentsMdSize_AuditError(t *testing.T) {
	origGetwd := osGetwd
	origStat := osStat
	origFn := agentsMdSizeFn
	defer func() {
		osGetwd = origGetwd
		osStat = origStat
		agentsMdSizeFn = origFn
	}()

	osGetwd = func() (string, error) { return "/mock-repo", nil }
	osStat = func(name string) (os.FileInfo, error) {
		if name == "/mock-repo/.git" {
			return &mockFileInfo{name: ".git", isDir: true}, nil
		}
		return nil, os.ErrNotExist
	}
	agentsMdSizeFn = func(_ string) (governance.AgentsMdSizeFinding, error) {
		return governance.AgentsMdSizeFinding{}, fmt.Errorf("stat AGENTS.md: file does not exist")
	}

	buf := new(bytes.Buffer)
	agentsMdSizeCmd.SetOut(buf)
	agentsMdSizeCmd.SetErr(buf)

	err := agentsMdSizeCmd.RunE(agentsMdSizeCmd, []string{})
	if err == nil || !strings.Contains(err.Error(), "agents-md-size audit failed") {
		t.Fatalf("expected wrapped audit error, got: %v", err)
	}
}

// TestGovernanceAgentsMdSize_OutputFormats exercises all three output
// formats end-to-end through the cobra command.
func TestGovernanceAgentsMdSize_OutputFormats(t *testing.T) {
	origGetwd := osGetwd
	origStat := osStat
	origFn := agentsMdSizeFn
	defer func() {
		osGetwd = origGetwd
		osStat = origStat
		agentsMdSizeFn = origFn
	}()

	osGetwd = func() (string, error) { return "/mock-repo", nil }
	osStat = func(name string) (os.FileInfo, error) {
		if name == "/mock-repo/.git" {
			return &mockFileInfo{name: ".git", isDir: true}, nil
		}
		return nil, os.ErrNotExist
	}
	agentsMdSizeFn = func(_ string) (governance.AgentsMdSizeFinding, error) {
		return governance.AgentsMdSizeFinding{
			File:     "/mock-repo/AGENTS.md",
			Size:     32000,
			Severity: "warn",
			Message:  "AGENTS.md is 32000 bytes (over 30000-byte target)",
		}, nil
	}

	for _, format := range []string{"json", "markdown", "text"} {
		t.Run(format, func(t *testing.T) {
			buf := new(bytes.Buffer)
			agentsMdSizeCmd.SetOut(buf)
			agentsMdSizeCmd.SetErr(buf)
			output = format
			verbose = false
			quiet = false
			_ = parseOutputFormat(nil, nil)
			_ = agentsMdSizeCmd.RunE(agentsMdSizeCmd, []string{})
			if buf.Len() == 0 {
				t.Errorf("format %s produced no output", format)
			}
			if format == "json" && !strings.Contains(buf.String(), "rhino-cli/agents-md-size/v1") {
				t.Errorf("JSON output missing schema, got: %s", buf.String())
			}
			if format == "markdown" && !strings.Contains(buf.String(), "## AGENTS.md Size Audit") {
				t.Errorf("markdown output missing heading, got: %s", buf.String())
			}
		})
	}
	output = "text"
	_ = parseOutputFormat(nil, nil)
}

// TestStatusLabelForSeverity covers each branch of the label helper.
func TestStatusLabelForSeverity(t *testing.T) {
	t.Parallel()
	cases := []struct {
		in, want string
	}{
		{"ok", "PASS"},
		{"warn", "WARN"},
		{"fail", "FAIL"},
		{"other", "other"},
	}
	for _, tc := range cases {
		if got := statusLabelForSeverity(tc.in); got != tc.want {
			t.Errorf("statusLabelForSeverity(%q) = %q, want %q", tc.in, got, tc.want)
		}
	}
}
