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
	governance "github.com/wahidyankf/ose-public/apps/rhino-cli/internal/repo-governance"
)

// Step constant patterns for the repo-governance readme-index-audit scenarios.
const (
	stepReadmeIndexCleanDir      = `^a governance directory whose README\.md links to every sibling markdown file$`
	stepReadmeIndexOrphan        = `^a governance directory containing a markdown file that the README\.md does not link to$`
	stepReadmeIndexGhost         = `^a governance directory whose README\.md links to a markdown file that is not present on disk$`
	stepReadmeIndexNestedOrphan  = `^a governance directory with a nested subdirectory whose own README\.md omits a sibling markdown file$`
	stepDeveloperRunsReadmeIndex = `^the developer runs repo-governance readme-index-audit on the directory$`
	stepZeroReadmeIndexFindings  = `^the output reports zero readme-index findings$`
	stepIdentifiesOrphanFile     = `^the output identifies the orphan file and its location$`
	stepIdentifiesGhostReference = `^the output identifies the ghost reference and its location$`
	stepIdentifiesNestedOrphan   = `^the output identifies the orphan file inside the nested subdirectory$`
)

var specsDirUnitGovernanceReadmeIndexAudit = func() string {
	_, f, _, _ := runtime.Caller(0)
	return filepath.Join(filepath.Dir(f), "../../../specs/apps/rhino/behavior/cli/gherkin")
}()

type governanceReadmeIndexAuditUnitSteps struct {
	cmdErr    error
	cmdOutput string
	expectVar string
}

func (s *governanceReadmeIndexAuditUnitSteps) before(_ context.Context, _ *godog.Scenario) (context.Context, error) {
	verbose = false
	quiet = false
	output = "text"
	s.cmdErr = nil
	s.cmdOutput = ""
	s.expectVar = ""
	readmeIndexAuditExcludes = nil

	osGetwd = func() (string, error) { return "/mock-repo", nil }
	osStat = func(name string) (os.FileInfo, error) {
		if name == "/mock-repo/.git" {
			return &mockFileInfo{name: ".git", isDir: true}, nil
		}
		return nil, os.ErrNotExist
	}
	readmeIndexAuditFn = func(_, _ []string) ([]governance.ReadmeIndexFinding, error) { return nil, nil }
	return context.Background(), nil
}

func (s *governanceReadmeIndexAuditUnitSteps) after(_ context.Context, _ *godog.Scenario, _ error) (context.Context, error) {
	readmeIndexAuditFn = governance.AuditReadmeIndex
	osGetwd = os.Getwd
	osStat = os.Stat
	readmeIndexAuditExcludes = nil
	return context.Background(), nil
}

func (s *governanceReadmeIndexAuditUnitSteps) cleanDirectory() error {
	readmeIndexAuditFn = func(_, _ []string) ([]governance.ReadmeIndexFinding, error) { return nil, nil }
	return nil
}

func (s *governanceReadmeIndexAuditUnitSteps) orphanFile() error {
	s.expectVar = "orphan"
	readmeIndexAuditFn = func(_, _ []string) ([]governance.ReadmeIndexFinding, error) {
		return []governance.ReadmeIndexFinding{{
			File:     "/mock-repo/repo-governance/orphan.md",
			Severity: "high",
			Kind:     "orphan",
			Message:  "orphan: orphan.md exists but is not linked from /mock-repo/repo-governance/README.md",
		}}, nil
	}
	return nil
}

func (s *governanceReadmeIndexAuditUnitSteps) ghostReference() error {
	s.expectVar = "ghost"
	readmeIndexAuditFn = func(_, _ []string) ([]governance.ReadmeIndexFinding, error) {
		return []governance.ReadmeIndexFinding{{
			File:     "/mock-repo/repo-governance/missing.md",
			Severity: "high",
			Kind:     "ghost",
			Message:  "ghost: /mock-repo/repo-governance/README.md references missing.md but the target does not exist",
		}}, nil
	}
	return nil
}

func (s *governanceReadmeIndexAuditUnitSteps) nestedOrphan() error {
	s.expectVar = "structure/two.md"
	readmeIndexAuditFn = func(_, _ []string) ([]governance.ReadmeIndexFinding, error) {
		return []governance.ReadmeIndexFinding{{
			File:     "/mock-repo/repo-governance/structure/two.md",
			Severity: "high",
			Kind:     "orphan",
			Message:  "orphan: two.md exists but is not linked from /mock-repo/repo-governance/structure/README.md",
		}}, nil
	}
	return nil
}

func (s *governanceReadmeIndexAuditUnitSteps) run() error {
	buf := new(bytes.Buffer)
	governanceReadmeIndexAuditCmd.SetOut(buf)
	governanceReadmeIndexAuditCmd.SetErr(buf)
	s.cmdErr = governanceReadmeIndexAuditCmd.RunE(governanceReadmeIndexAuditCmd, []string{"repo-governance/"})
	s.cmdOutput = buf.String()
	return nil
}

func (s *governanceReadmeIndexAuditUnitSteps) exitsSuccessfully() error {
	if s.cmdErr != nil {
		return fmt.Errorf("expected success but got: %w\nOutput: %s", s.cmdErr, s.cmdOutput)
	}
	return nil
}

func (s *governanceReadmeIndexAuditUnitSteps) exitsWithFailure() error {
	if s.cmdErr == nil {
		return fmt.Errorf("expected failure but succeeded\nOutput: %s", s.cmdOutput)
	}
	return nil
}

func (s *governanceReadmeIndexAuditUnitSteps) zeroFindings() error {
	if !strings.Contains(s.cmdOutput, "PASSED") {
		return fmt.Errorf("expected PASSED in output, got: %s", s.cmdOutput)
	}
	return nil
}

func (s *governanceReadmeIndexAuditUnitSteps) outputIdentifies() error {
	if s.expectVar == "" {
		return fmt.Errorf("test setup error: expectVar not set")
	}
	if !strings.Contains(s.cmdOutput, s.expectVar) {
		return fmt.Errorf("expected output to contain %q, got: %s", s.expectVar, s.cmdOutput)
	}
	return nil
}

func TestUnitGovernanceReadmeIndexAudit(t *testing.T) {
	s := &governanceReadmeIndexAuditUnitSteps{}
	suite := godog.TestSuite{
		ScenarioInitializer: func(sc *godog.ScenarioContext) {
			sc.Before(s.before)
			sc.After(s.after)
			sc.Step(stepReadmeIndexCleanDir, s.cleanDirectory)
			sc.Step(stepReadmeIndexOrphan, s.orphanFile)
			sc.Step(stepReadmeIndexGhost, s.ghostReference)
			sc.Step(stepReadmeIndexNestedOrphan, s.nestedOrphan)
			sc.Step(stepDeveloperRunsReadmeIndex, s.run)
			sc.Step(stepZeroReadmeIndexFindings, s.zeroFindings)
			sc.Step(stepIdentifiesOrphanFile, s.outputIdentifies)
			sc.Step(stepIdentifiesGhostReference, s.outputIdentifies)
			sc.Step(stepIdentifiesNestedOrphan, s.outputIdentifies)
			sc.Step(stepExitsSuccessfully, s.exitsSuccessfully)
			sc.Step(stepExitsWithFailure, s.exitsWithFailure)
		},
		Options: &godog.Options{
			Format:   "pretty",
			Paths:    []string{specsDirUnitGovernanceReadmeIndexAudit},
			TestingT: t,
			Tags:     "repo-governance-readme-index-audit",
		},
	}
	if suite.Run() != 0 {
		t.Fatal("non-zero status returned, failed to run unit feature tests")
	}
}

// TestGovernanceReadmeIndexAudit_MissingGitRoot verifies the command fails
// gracefully when not inside a git repository.
func TestGovernanceReadmeIndexAudit_MissingGitRoot(t *testing.T) {
	origGetwd := osGetwd
	origStat := osStat
	defer func() {
		osGetwd = origGetwd
		osStat = origStat
	}()

	osGetwd = func() (string, error) { return "/no-git-here", nil }
	osStat = func(_ string) (os.FileInfo, error) { return nil, os.ErrNotExist }

	buf := new(bytes.Buffer)
	governanceReadmeIndexAuditCmd.SetOut(buf)
	governanceReadmeIndexAuditCmd.SetErr(buf)
	err := governanceReadmeIndexAuditCmd.RunE(governanceReadmeIndexAuditCmd, []string{})
	if err == nil || !strings.Contains(err.Error(), "git") {
		t.Fatalf("expected git-root error, got: %v", err)
	}
}

// TestGovernanceReadmeIndexAudit_AuditError propagates internal-package
// errors as a wrapped command error.
func TestGovernanceReadmeIndexAudit_AuditError(t *testing.T) {
	origGetwd := osGetwd
	origStat := osStat
	origFn := readmeIndexAuditFn
	defer func() {
		osGetwd = origGetwd
		osStat = origStat
		readmeIndexAuditFn = origFn
	}()

	osGetwd = func() (string, error) { return "/mock-repo", nil }
	osStat = func(name string) (os.FileInfo, error) {
		if name == "/mock-repo/.git" {
			return &mockFileInfo{name: ".git", isDir: true}, nil
		}
		return nil, os.ErrNotExist
	}
	readmeIndexAuditFn = func(_, _ []string) ([]governance.ReadmeIndexFinding, error) {
		return nil, fmt.Errorf("walk boom")
	}

	buf := new(bytes.Buffer)
	governanceReadmeIndexAuditCmd.SetOut(buf)
	governanceReadmeIndexAuditCmd.SetErr(buf)
	err := governanceReadmeIndexAuditCmd.RunE(governanceReadmeIndexAuditCmd, []string{})
	if err == nil || !strings.Contains(err.Error(), "readme-index audit failed") {
		t.Fatalf("expected wrapped audit error, got: %v", err)
	}
}

// TestGovernanceReadmeIndexAudit_OutputFormats exercises all three output
// formats end-to-end through the cobra command.
func TestGovernanceReadmeIndexAudit_OutputFormats(t *testing.T) {
	origGetwd := osGetwd
	origStat := osStat
	origFn := readmeIndexAuditFn
	defer func() {
		osGetwd = origGetwd
		osStat = origStat
		readmeIndexAuditFn = origFn
		output = "text"
	}()

	osGetwd = func() (string, error) { return "/mock-repo", nil }
	osStat = func(name string) (os.FileInfo, error) {
		if name == "/mock-repo/.git" {
			return &mockFileInfo{name: ".git", isDir: true}, nil
		}
		return nil, os.ErrNotExist
	}
	readmeIndexAuditFn = func(_, _ []string) ([]governance.ReadmeIndexFinding, error) {
		return []governance.ReadmeIndexFinding{{
			File:     "/mock-repo/repo-governance/orphan.md",
			Severity: "high",
			Kind:     "orphan",
			Message:  "orphan example",
		}}, nil
	}

	for _, format := range []string{"json", "markdown", "text"} {
		t.Run(format, func(t *testing.T) {
			buf := new(bytes.Buffer)
			governanceReadmeIndexAuditCmd.SetOut(buf)
			governanceReadmeIndexAuditCmd.SetErr(buf)
			output = format
			_ = parseOutputFormat(nil, nil)
			_ = governanceReadmeIndexAuditCmd.RunE(governanceReadmeIndexAuditCmd, []string{})
			if buf.Len() == 0 {
				t.Errorf("format %s produced no output", format)
			}
			if format == "json" {
				var env map[string]any
				if err := json.Unmarshal(buf.Bytes(), &env); err != nil {
					t.Fatalf("invalid JSON envelope: %v\n%s", err, buf.String())
				}
				if env["schema"] != readmeIndexAuditSchema {
					t.Errorf("expected schema %q, got %v", readmeIndexAuditSchema, env["schema"])
				}
				if env["status"] != "failed" {
					t.Errorf("expected status 'failed', got %v", env["status"])
				}
			}
			if format == "markdown" && !strings.Contains(buf.String(), "## README Index Audit") {
				t.Errorf("markdown output missing heading, got: %s", buf.String())
			}
		})
	}
	output = "text"
	_ = parseOutputFormat(nil, nil)
}

// TestGovernanceReadmeIndexAudit_CleanCaseJSON verifies the clean-case JSON
// envelope reports a passing status with an empty findings list.
func TestGovernanceReadmeIndexAudit_CleanCaseJSON(t *testing.T) {
	origGetwd := osGetwd
	origStat := osStat
	origFn := readmeIndexAuditFn
	defer func() {
		osGetwd = origGetwd
		osStat = origStat
		readmeIndexAuditFn = origFn
		output = "text"
	}()

	osGetwd = func() (string, error) { return "/mock-repo", nil }
	osStat = func(name string) (os.FileInfo, error) {
		if name == "/mock-repo/.git" {
			return &mockFileInfo{name: ".git", isDir: true}, nil
		}
		return nil, os.ErrNotExist
	}
	readmeIndexAuditFn = func(_, _ []string) ([]governance.ReadmeIndexFinding, error) { return nil, nil }

	buf := new(bytes.Buffer)
	governanceReadmeIndexAuditCmd.SetOut(buf)
	governanceReadmeIndexAuditCmd.SetErr(buf)
	output = "json"
	_ = parseOutputFormat(nil, nil)
	if err := governanceReadmeIndexAuditCmd.RunE(governanceReadmeIndexAuditCmd, []string{}); err != nil {
		t.Fatalf("expected clean run to succeed, got: %v", err)
	}

	var env map[string]any
	if err := json.Unmarshal(buf.Bytes(), &env); err != nil {
		t.Fatalf("invalid JSON envelope: %v\n%s", err, buf.String())
	}
	if env["status"] != "passed" {
		t.Errorf("expected status 'passed' for clean run, got %v", env["status"])
	}
	if env["schema"] != readmeIndexAuditSchema {
		t.Errorf("expected schema %q, got %v", readmeIndexAuditSchema, env["schema"])
	}
}

// TestGovernanceReadmeIndexAudit_MarkdownCleanCase covers the clean-case
// markdown render branch.
func TestGovernanceReadmeIndexAudit_MarkdownCleanCase(t *testing.T) {
	origGetwd := osGetwd
	origStat := osStat
	origFn := readmeIndexAuditFn
	defer func() {
		osGetwd = origGetwd
		osStat = origStat
		readmeIndexAuditFn = origFn
		output = "text"
	}()

	osGetwd = func() (string, error) { return "/mock-repo", nil }
	osStat = func(name string) (os.FileInfo, error) {
		if name == "/mock-repo/.git" {
			return &mockFileInfo{name: ".git", isDir: true}, nil
		}
		return nil, os.ErrNotExist
	}
	readmeIndexAuditFn = func(_, _ []string) ([]governance.ReadmeIndexFinding, error) { return nil, nil }

	buf := new(bytes.Buffer)
	governanceReadmeIndexAuditCmd.SetOut(buf)
	governanceReadmeIndexAuditCmd.SetErr(buf)
	output = "markdown"
	_ = parseOutputFormat(nil, nil)
	if err := governanceReadmeIndexAuditCmd.RunE(governanceReadmeIndexAuditCmd, []string{}); err != nil {
		t.Fatalf("expected clean run to succeed, got: %v", err)
	}
	if !strings.Contains(buf.String(), "PASSED") {
		t.Errorf("expected PASSED in markdown output, got: %s", buf.String())
	}
}

// TestResolveReadmeIndexAuditPaths verifies the precedence rule for path
// argument selection.
func TestResolveReadmeIndexAuditPaths(t *testing.T) {
	tests := []struct {
		name string
		args []string
		want []string
	}{
		{
			name: "default when empty",
			want: readmeIndexAuditDefaultPaths,
		},
		{
			name: "positional override",
			args: []string{"docs/"},
			want: []string{"docs/"},
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			got := resolveReadmeIndexAuditPaths(tt.args)
			if len(got) != len(tt.want) {
				t.Fatalf("got %v want %v", got, tt.want)
			}
			for i := range tt.want {
				if got[i] != tt.want[i] {
					t.Errorf("[%d] got %q want %q", i, got[i], tt.want[i])
				}
			}
		})
	}
}
