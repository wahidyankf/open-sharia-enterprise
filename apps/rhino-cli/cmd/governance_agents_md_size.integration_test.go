//go:build integration

package cmd

import (
	"bytes"
	"context"
	"fmt"
	"os"
	"path/filepath"
	"runtime"
	"strconv"
	"strings"
	"testing"

	"github.com/cucumber/godog"
)

var specsDirGovernanceAgentsMdSize = func() string {
	_, f, _, _ := runtime.Caller(0)
	return filepath.Join(filepath.Dir(f), "../../../specs/apps/rhino/behavior/cli/gherkin")
}()

type governanceAgentsMdSizeIntegSteps struct {
	originalWd string
	tmpDir     string
	cmdErr     error
	cmdOutput  string
}

func (s *governanceAgentsMdSizeIntegSteps) before(_ context.Context, _ *godog.Scenario) (context.Context, error) {
	s.originalWd, _ = os.Getwd()
	s.tmpDir, _ = os.MkdirTemp("", "agents-md-size-*")
	_ = os.MkdirAll(filepath.Join(s.tmpDir, ".git"), 0o755)
	verbose = false
	quiet = false
	output = "text"
	_ = os.Chdir(s.tmpDir)
	return context.Background(), nil
}

func (s *governanceAgentsMdSizeIntegSteps) after(_ context.Context, _ *godog.Scenario, _ error) (context.Context, error) {
	_ = os.Chdir(s.originalWd)
	_ = os.RemoveAll(s.tmpDir)
	return context.Background(), nil
}

// agentsMdFileOfBytes writes AGENTS.md of exactly `sizeStr` bytes at the tmp
// repo root. The Gherkin step captures the size as a string, so we parse here.
func (s *governanceAgentsMdSizeIntegSteps) agentsMdFileOfBytes(sizeStr string) error {
	size, err := strconv.Atoi(sizeStr)
	if err != nil {
		return fmt.Errorf("parse size %q: %w", sizeStr, err)
	}
	path := filepath.Join(s.tmpDir, "AGENTS.md")
	return os.WriteFile(path, bytes.Repeat([]byte("a"), size), 0o600)
}

func (s *governanceAgentsMdSizeIntegSteps) run() error {
	buf := new(bytes.Buffer)
	agentsMdSizeCmd.SetOut(buf)
	agentsMdSizeCmd.SetErr(buf)
	s.cmdErr = agentsMdSizeCmd.RunE(agentsMdSizeCmd, []string{})
	s.cmdOutput = buf.String()
	return nil
}

func (s *governanceAgentsMdSizeIntegSteps) exitsSuccessfully() error {
	if s.cmdErr != nil {
		return fmt.Errorf("expected success, got: %v\nOutput: %s", s.cmdErr, s.cmdOutput)
	}
	return nil
}

func (s *governanceAgentsMdSizeIntegSteps) exitsWithFailure() error {
	if s.cmdErr == nil {
		return fmt.Errorf("expected failure, output: %s", s.cmdOutput)
	}
	return nil
}

func (s *governanceAgentsMdSizeIntegSteps) outputReportsWithinTarget() error {
	if !strings.Contains(s.cmdOutput, "PASS") {
		return fmt.Errorf("expected PASS, got: %s", s.cmdOutput)
	}
	if !strings.Contains(s.cmdOutput, "within") {
		return fmt.Errorf("expected 'within', got: %s", s.cmdOutput)
	}
	return nil
}

func (s *governanceAgentsMdSizeIntegSteps) outputIdentifiesOverTarget() error {
	if !strings.Contains(s.cmdOutput, "WARN") {
		return fmt.Errorf("expected WARN, got: %s", s.cmdOutput)
	}
	if !strings.Contains(s.cmdOutput, "over") {
		return fmt.Errorf("expected 'over', got: %s", s.cmdOutput)
	}
	return nil
}

func (s *governanceAgentsMdSizeIntegSteps) outputIdentifiesOverHardLimit() error {
	if !strings.Contains(s.cmdOutput, "FAIL") {
		return fmt.Errorf("expected FAIL, got: %s", s.cmdOutput)
	}
	if !strings.Contains(s.cmdOutput, "hard limit") {
		return fmt.Errorf("expected 'hard limit', got: %s", s.cmdOutput)
	}
	return nil
}

// InitializeGovernanceAgentsMdSizeScenario wires the integration steps into
// the godog scenario context.
func InitializeGovernanceAgentsMdSizeScenario(sc *godog.ScenarioContext) {
	s := &governanceAgentsMdSizeIntegSteps{}
	sc.Before(s.before)
	sc.After(s.after)
	sc.Step(stepAgentsMdSizeFile, s.agentsMdFileOfBytes)
	sc.Step(stepDeveloperRunsAgentsMdSize, s.run)
	sc.Step(stepExitsSuccessfully, s.exitsSuccessfully)
	sc.Step(stepExitsWithFailure, s.exitsWithFailure)
	sc.Step(stepOutputReportsAgentsMdWithinTarget, s.outputReportsWithinTarget)
	sc.Step(stepOutputIdentifiesAgentsMdOverTarget, s.outputIdentifiesOverTarget)
	sc.Step(stepOutputIdentifiesAgentsMdOverHardLimit, s.outputIdentifiesOverHardLimit)
}

func TestIntegrationGovernanceAgentsMdSize(t *testing.T) {
	suite := godog.TestSuite{
		ScenarioInitializer: InitializeGovernanceAgentsMdSizeScenario,
		Options: &godog.Options{
			Format:   "pretty",
			Paths:    []string{specsDirGovernanceAgentsMdSize},
			Tags:     "repo-governance-agents-md-size",
			TestingT: t,
		},
	}
	if suite.Run() != 0 {
		t.Fatal("non-zero status returned, failed to run integration feature tests")
	}
}
