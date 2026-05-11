package cmd

import (
	"bytes"
	"context"
	"encoding/json"
	"fmt"
	"os"
	"path/filepath"
	"runtime"
	"strings"
	"testing"

	"github.com/cucumber/godog"
	"github.com/wahidyankf/ose-public/apps/rhino-cli/internal/agents"
)

// Step constant patterns for agents detect-duplication scenarios. The patterns
// match the Gherkin lines in
// specs/apps/rhino/behavior/cli/gherkin/agents-detect-duplication.feature.
const (
	stepAgentsTreeNoSharedWindows         = `^a repository with agent and skill files whose bodies share no 10-line verbatim windows$`
	stepAgentsTreeTwoAgentsShare12Lines   = `^a repository with two agent files that share 12 consecutive lines verbatim$`
	stepAgentsTreeAgentMatchesSkill       = `^a repository with an agent file whose body matches 11 consecutive lines of a SKILL\.md$`
	stepAgentsTreeHeadingOrBlankWindow    = `^a repository where two agent files share a 10-line window composed only of headings or blank lines$`
	stepDeveloperRunsAgentsDetectDup      = `^the developer runs agents detect-duplication$`
	stepOutputZeroDuplicationClusters     = `^the output reports zero duplication clusters$`
	stepOutputIdentifiesClusterTwoAgents  = `^the output identifies the duplicated cluster across both agents$`
	stepOutputIdentifiesClusterAgentSkill = `^the output identifies the duplicated cluster across the agent and the skill$`
)

var specsDirUnitAgentsDetectDuplication = func() string {
	_, f, _, _ := runtime.Caller(0)
	return filepath.Join(filepath.Dir(f), "../../../specs/apps/rhino/behavior/cli/gherkin")
}()

type agentsDetectDuplicationUnitSteps struct {
	cmdErr    error
	cmdOutput string
}

func (s *agentsDetectDuplicationUnitSteps) before(_ context.Context, _ *godog.Scenario) (context.Context, error) {
	verbose = false
	quiet = false
	output = "text"
	s.cmdErr = nil
	s.cmdOutput = ""

	osGetwd = func() (string, error) { return "/mock-repo", nil }
	osStat = func(name string) (os.FileInfo, error) {
		if name == "/mock-repo/.git" {
			return &mockFileInfo{name: ".git", isDir: true}, nil
		}
		return nil, os.ErrNotExist
	}
	// Default mock returns zero findings; scenarios override below.
	agentsDetectDuplicationFn = func(_ string) ([]agents.DuplicationFinding, error) { return nil, nil }
	return context.Background(), nil
}

func (s *agentsDetectDuplicationUnitSteps) after(_ context.Context, _ *godog.Scenario, _ error) (context.Context, error) {
	agentsDetectDuplicationFn = agents.DetectDuplication
	osGetwd = os.Getwd
	osStat = os.Stat
	return context.Background(), nil
}

func (s *agentsDetectDuplicationUnitSteps) noSharedWindows() error {
	agentsDetectDuplicationFn = func(_ string) ([]agents.DuplicationFinding, error) { return nil, nil }
	return nil
}

func (s *agentsDetectDuplicationUnitSteps) twoAgentsShare12Lines() error {
	agentsDetectDuplicationFn = func(_ string) ([]agents.DuplicationFinding, error) {
		return []agents.DuplicationFinding{{
			Files:      []string{"/mock-repo/.claude/agents/alpha-maker.md", "/mock-repo/.claude/agents/beta-checker.md"},
			StartLines: []int{6, 9},
			WindowSize: 10,
			Severity:   "high",
			Message:    "10-line verbatim duplication across 2 files",
		}}, nil
	}
	return nil
}

func (s *agentsDetectDuplicationUnitSteps) agentMatchesSkill() error {
	agentsDetectDuplicationFn = func(_ string) ([]agents.DuplicationFinding, error) {
		return []agents.DuplicationFinding{{
			Files:      []string{"/mock-repo/.claude/agents/alpha-maker.md", "/mock-repo/.claude/skills/shared-skill/SKILL.md"},
			StartLines: []int{5, 7},
			WindowSize: 10,
			Severity:   "high",
			Message:    "10-line verbatim duplication across 2 files",
		}}, nil
	}
	return nil
}

func (s *agentsDetectDuplicationUnitSteps) headingOrBlankWindow() error {
	// The internal detector excludes heading-only and blank-only windows; the
	// mock returns zero findings as the production behaviour would.
	agentsDetectDuplicationFn = func(_ string) ([]agents.DuplicationFinding, error) { return nil, nil }
	return nil
}

func (s *agentsDetectDuplicationUnitSteps) run() error {
	buf := new(bytes.Buffer)
	agentsDetectDuplicationCmd.SetOut(buf)
	agentsDetectDuplicationCmd.SetErr(buf)
	s.cmdErr = agentsDetectDuplicationCmd.RunE(agentsDetectDuplicationCmd, []string{})
	s.cmdOutput = buf.String()
	return nil
}

func (s *agentsDetectDuplicationUnitSteps) exitsSuccessfully() error {
	if s.cmdErr != nil {
		return fmt.Errorf("expected success but got: %w\nOutput: %s", s.cmdErr, s.cmdOutput)
	}
	return nil
}

func (s *agentsDetectDuplicationUnitSteps) exitsWithFailure() error {
	if s.cmdErr == nil {
		return fmt.Errorf("expected failure but succeeded\nOutput: %s", s.cmdOutput)
	}
	return nil
}

func (s *agentsDetectDuplicationUnitSteps) zeroClusters() error {
	if !strings.Contains(s.cmdOutput, "PASSED") {
		return fmt.Errorf("expected PASSED in output, got: %s", s.cmdOutput)
	}
	if !strings.Contains(s.cmdOutput, "0 clusters") {
		return fmt.Errorf("expected '0 clusters', got: %s", s.cmdOutput)
	}
	return nil
}

func (s *agentsDetectDuplicationUnitSteps) clusterAcrossBothAgents() error {
	if !strings.Contains(s.cmdOutput, "FAILED") {
		return fmt.Errorf("expected FAILED in output, got: %s", s.cmdOutput)
	}
	if !strings.Contains(s.cmdOutput, "alpha-maker.md") {
		return fmt.Errorf("expected alpha-maker.md, got: %s", s.cmdOutput)
	}
	if !strings.Contains(s.cmdOutput, "beta-checker.md") {
		return fmt.Errorf("expected beta-checker.md, got: %s", s.cmdOutput)
	}
	return nil
}

func (s *agentsDetectDuplicationUnitSteps) clusterAcrossAgentAndSkill() error {
	if !strings.Contains(s.cmdOutput, "FAILED") {
		return fmt.Errorf("expected FAILED in output, got: %s", s.cmdOutput)
	}
	if !strings.Contains(s.cmdOutput, "alpha-maker.md") {
		return fmt.Errorf("expected agent path, got: %s", s.cmdOutput)
	}
	if !strings.Contains(s.cmdOutput, "SKILL.md") {
		return fmt.Errorf("expected SKILL.md, got: %s", s.cmdOutput)
	}
	return nil
}

func TestUnitAgentsDetectDuplication(t *testing.T) {
	s := &agentsDetectDuplicationUnitSteps{}
	suite := godog.TestSuite{
		ScenarioInitializer: func(sc *godog.ScenarioContext) {
			sc.Before(s.before)
			sc.After(s.after)
			sc.Step(stepAgentsTreeNoSharedWindows, s.noSharedWindows)
			sc.Step(stepAgentsTreeTwoAgentsShare12Lines, s.twoAgentsShare12Lines)
			sc.Step(stepAgentsTreeAgentMatchesSkill, s.agentMatchesSkill)
			sc.Step(stepAgentsTreeHeadingOrBlankWindow, s.headingOrBlankWindow)
			sc.Step(stepDeveloperRunsAgentsDetectDup, s.run)
			sc.Step(stepExitsSuccessfully, s.exitsSuccessfully)
			sc.Step(stepExitsWithFailure, s.exitsWithFailure)
			sc.Step(stepOutputZeroDuplicationClusters, s.zeroClusters)
			sc.Step(stepOutputIdentifiesClusterTwoAgents, s.clusterAcrossBothAgents)
			sc.Step(stepOutputIdentifiesClusterAgentSkill, s.clusterAcrossAgentAndSkill)
		},
		Options: &godog.Options{
			Format:   "pretty",
			Paths:    []string{specsDirUnitAgentsDetectDuplication},
			TestingT: t,
			Tags:     "agents-detect-duplication",
		},
	}
	if suite.Run() != 0 {
		t.Fatal("non-zero status returned, failed to run unit feature tests")
	}
}

// TestAgentsDetectDuplication_MissingGitRoot verifies the command fails
// gracefully when not inside a git repository.
func TestAgentsDetectDuplication_MissingGitRoot(t *testing.T) {
	origGetwd := osGetwd
	origStat := osStat
	defer func() {
		osGetwd = origGetwd
		osStat = origStat
	}()

	osGetwd = func() (string, error) { return "/no-git-here", nil }
	osStat = func(_ string) (os.FileInfo, error) { return nil, os.ErrNotExist }

	buf := new(bytes.Buffer)
	agentsDetectDuplicationCmd.SetOut(buf)
	agentsDetectDuplicationCmd.SetErr(buf)
	err := agentsDetectDuplicationCmd.RunE(agentsDetectDuplicationCmd, []string{})
	if err == nil || !strings.Contains(err.Error(), "git") {
		t.Fatalf("expected git-root error, got: %v", err)
	}
}

// TestAgentsDetectDuplication_PropagatesDetectorError ensures internal-package
// errors surface as command errors with the expected wrapping.
func TestAgentsDetectDuplication_PropagatesDetectorError(t *testing.T) {
	origGetwd := osGetwd
	origStat := osStat
	origFn := agentsDetectDuplicationFn
	defer func() {
		osGetwd = origGetwd
		osStat = origStat
		agentsDetectDuplicationFn = origFn
	}()

	osGetwd = func() (string, error) { return "/mock-repo", nil }
	osStat = func(name string) (os.FileInfo, error) {
		if name == "/mock-repo/.git" {
			return &mockFileInfo{name: ".git", isDir: true}, nil
		}
		return nil, os.ErrNotExist
	}
	agentsDetectDuplicationFn = func(_ string) ([]agents.DuplicationFinding, error) {
		return nil, fmt.Errorf("boom")
	}

	buf := new(bytes.Buffer)
	agentsDetectDuplicationCmd.SetOut(buf)
	agentsDetectDuplicationCmd.SetErr(buf)
	err := agentsDetectDuplicationCmd.RunE(agentsDetectDuplicationCmd, []string{})
	if err == nil || !strings.Contains(err.Error(), "boom") {
		t.Fatalf("expected propagated error, got: %v", err)
	}
}

// TestAgentsDetectDuplication_OutputFormats exercises text, JSON, and markdown
// rendering against a fixture of one cluster.
func TestAgentsDetectDuplication_OutputFormats(t *testing.T) {
	origGetwd := osGetwd
	origStat := osStat
	origFn := agentsDetectDuplicationFn
	defer func() {
		osGetwd = origGetwd
		osStat = origStat
		agentsDetectDuplicationFn = origFn
		output = "text"
		_ = parseOutputFormat(nil, nil)
	}()

	osGetwd = func() (string, error) { return "/mock-repo", nil }
	osStat = func(name string) (os.FileInfo, error) {
		if name == "/mock-repo/.git" {
			return &mockFileInfo{name: ".git", isDir: true}, nil
		}
		return nil, os.ErrNotExist
	}
	agentsDetectDuplicationFn = func(_ string) ([]agents.DuplicationFinding, error) {
		return []agents.DuplicationFinding{{
			Files:      []string{"/mock-repo/.claude/agents/alpha-maker.md", "/mock-repo/.claude/agents/beta-checker.md"},
			StartLines: []int{6, 9},
			WindowSize: 10,
			Severity:   "high",
			Message:    "10-line verbatim duplication across 2 files",
		}}, nil
	}

	for _, format := range []string{"json", "markdown", "text"} {
		t.Run(format, func(t *testing.T) {
			buf := new(bytes.Buffer)
			agentsDetectDuplicationCmd.SetOut(buf)
			agentsDetectDuplicationCmd.SetErr(buf)
			output = format
			_ = parseOutputFormat(nil, nil)
			_ = agentsDetectDuplicationCmd.RunE(agentsDetectDuplicationCmd, []string{})
			if buf.Len() == 0 {
				t.Errorf("format %s produced no output", format)
			}
			if format == "json" {
				var env map[string]any
				if err := json.Unmarshal(buf.Bytes(), &env); err != nil {
					t.Fatalf("invalid JSON envelope: %v\n%s", err, buf.String())
				}
				if env["schema"] != agentsDetectDuplicationSchema {
					t.Errorf("expected schema %q, got %v", agentsDetectDuplicationSchema, env["schema"])
				}
				if env["status"] != "failed" {
					t.Errorf("expected status 'failed', got %v", env["status"])
				}
			}
			if format == "markdown" && !strings.Contains(buf.String(), "## Agents Duplication Detection") {
				t.Errorf("markdown output missing heading, got: %s", buf.String())
			}
		})
	}
}

// TestAgentsDetectDuplication_CleanCaseJSON verifies a clean run reports
// passed status in the JSON envelope.
func TestAgentsDetectDuplication_CleanCaseJSON(t *testing.T) {
	origGetwd := osGetwd
	origStat := osStat
	origFn := agentsDetectDuplicationFn
	defer func() {
		osGetwd = origGetwd
		osStat = origStat
		agentsDetectDuplicationFn = origFn
		output = "text"
		_ = parseOutputFormat(nil, nil)
	}()

	osGetwd = func() (string, error) { return "/mock-repo", nil }
	osStat = func(name string) (os.FileInfo, error) {
		if name == "/mock-repo/.git" {
			return &mockFileInfo{name: ".git", isDir: true}, nil
		}
		return nil, os.ErrNotExist
	}
	agentsDetectDuplicationFn = func(_ string) ([]agents.DuplicationFinding, error) { return nil, nil }

	buf := new(bytes.Buffer)
	agentsDetectDuplicationCmd.SetOut(buf)
	agentsDetectDuplicationCmd.SetErr(buf)
	output = "json"
	_ = parseOutputFormat(nil, nil)
	if err := agentsDetectDuplicationCmd.RunE(agentsDetectDuplicationCmd, []string{}); err != nil {
		t.Fatalf("expected clean run to succeed, got: %v", err)
	}
	var env map[string]any
	if err := json.Unmarshal(buf.Bytes(), &env); err != nil {
		t.Fatalf("invalid JSON envelope: %v\n%s", err, buf.String())
	}
	if env["status"] != "passed" {
		t.Errorf("expected status 'passed' for clean run, got %v", env["status"])
	}
}

// TestAgentsDetectDuplication_CleanCaseMarkdown verifies the markdown clean
// branch is exercised (it is otherwise short-circuited by the no-clusters
// early return in writeAgentsDetectDuplicationMarkdown).
func TestAgentsDetectDuplication_CleanCaseMarkdown(t *testing.T) {
	origGetwd := osGetwd
	origStat := osStat
	origFn := agentsDetectDuplicationFn
	defer func() {
		osGetwd = origGetwd
		osStat = origStat
		agentsDetectDuplicationFn = origFn
		output = "text"
		_ = parseOutputFormat(nil, nil)
	}()

	osGetwd = func() (string, error) { return "/mock-repo", nil }
	osStat = func(name string) (os.FileInfo, error) {
		if name == "/mock-repo/.git" {
			return &mockFileInfo{name: ".git", isDir: true}, nil
		}
		return nil, os.ErrNotExist
	}
	agentsDetectDuplicationFn = func(_ string) ([]agents.DuplicationFinding, error) { return nil, nil }

	buf := new(bytes.Buffer)
	agentsDetectDuplicationCmd.SetOut(buf)
	agentsDetectDuplicationCmd.SetErr(buf)
	output = "markdown"
	_ = parseOutputFormat(nil, nil)
	_ = agentsDetectDuplicationCmd.RunE(agentsDetectDuplicationCmd, []string{})
	if !strings.Contains(buf.String(), "**PASSED**") {
		t.Errorf("expected PASSED markdown, got: %s", buf.String())
	}
}
