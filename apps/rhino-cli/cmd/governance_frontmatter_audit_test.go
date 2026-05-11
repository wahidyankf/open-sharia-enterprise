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

// Step constant patterns for repo-governance frontmatter-audit scenarios.
const (
	stepGovernanceDirNoForbiddenDates             = `^a governance directory with no forbidden date metadata in markdown files$`
	stepGovernanceFileForbiddenUpdatedField       = `^a governance markdown file whose frontmatter contains a forbidden updated field$`
	stepGovernanceFileLastUpdatedFooter           = `^a governance markdown file whose body contains a Last Updated footer block$`
	stepGovernanceFileInlineCreatedAnnotation     = `^a governance markdown file whose body contains a standalone Created date annotation$`
	stepWebsiteFileWithForbiddenDateMetadata      = `^a markdown file with forbidden date metadata under a website app directory$`
	stepDeveloperRunsFrontmatterAuditOnDir        = `^the developer runs repo-governance frontmatter-audit on the directory$`
	stepDeveloperRunsFrontmatterAuditOnFile       = `^the developer runs repo-governance frontmatter-audit on the file$`
	stepOutputZeroFrontmatterFindings             = `^the output reports zero frontmatter findings$`
	stepOutputIdentifiesForbiddenFrontmatterField = `^the output identifies the forbidden frontmatter field and its location$`
	stepOutputIdentifiesForbiddenFooterBlock      = `^the output identifies the forbidden footer block and its location$`
	stepOutputIdentifiesForbiddenInlineAnnotation = `^the output identifies the forbidden inline annotation and its location$`
)

var specsDirUnitGovernanceFrontmatterAudit = func() string {
	_, f, _, _ := runtime.Caller(0)
	return filepath.Join(filepath.Dir(f), "../../../specs/apps/rhino/behavior/cli/gherkin")
}()

type governanceFrontmatterAuditUnitSteps struct {
	cmdErr    error
	cmdOutput string
	expectMsg string
	expectVar string
}

func (s *governanceFrontmatterAuditUnitSteps) before(_ context.Context, _ *godog.Scenario) (context.Context, error) {
	verbose = false
	quiet = false
	output = "text"
	s.cmdErr = nil
	s.cmdOutput = ""
	s.expectMsg = ""
	s.expectVar = ""
	frontmatterAuditPaths = nil

	osGetwd = func() (string, error) { return "/mock-repo", nil }
	osStat = func(name string) (os.FileInfo, error) {
		if name == "/mock-repo/.git" {
			return &mockFileInfo{name: ".git", isDir: true}, nil
		}
		return nil, os.ErrNotExist
	}
	frontmatterAuditFn = func(_ []string) ([]governance.FrontmatterFinding, error) { return nil, nil }
	return context.Background(), nil
}

func (s *governanceFrontmatterAuditUnitSteps) after(_ context.Context, _ *godog.Scenario, _ error) (context.Context, error) {
	frontmatterAuditFn = governance.AuditFrontmatter
	osGetwd = os.Getwd
	osStat = os.Stat
	frontmatterAuditPaths = nil
	return context.Background(), nil
}

func (s *governanceFrontmatterAuditUnitSteps) cleanDirectory() error {
	frontmatterAuditFn = func(_ []string) ([]governance.FrontmatterFinding, error) { return nil, nil }
	return nil
}

func (s *governanceFrontmatterAuditUnitSteps) forbiddenUpdatedField() error {
	s.expectVar = "updated"
	s.expectMsg = "forbidden \"updated:\" field"
	frontmatterAuditFn = func(_ []string) ([]governance.FrontmatterFinding, error) {
		return []governance.FrontmatterFinding{{
			File:     "/mock-repo/repo-governance/foo.md",
			Line:     4,
			Severity: "high",
			Message:  `forbidden "updated:" field in YAML frontmatter; remove per no-date-metadata convention`,
		}}, nil
	}
	return nil
}

func (s *governanceFrontmatterAuditUnitSteps) lastUpdatedFooter() error {
	s.expectVar = "Last Updated"
	s.expectMsg = "Last Updated"
	frontmatterAuditFn = func(_ []string) ([]governance.FrontmatterFinding, error) {
		return []governance.FrontmatterFinding{{
			File:     "/mock-repo/repo-governance/foo.md",
			Line:     12,
			Severity: "high",
			Message:  "forbidden **Last Updated** footer marker in body; remove per no-date-metadata convention",
		}}, nil
	}
	return nil
}

func (s *governanceFrontmatterAuditUnitSteps) inlineCreatedAnnotation() error {
	s.expectVar = "inline date annotation"
	s.expectMsg = "inline date annotation"
	frontmatterAuditFn = func(_ []string) ([]governance.FrontmatterFinding, error) {
		return []governance.FrontmatterFinding{{
			File:     "/mock-repo/repo-governance/foo.md",
			Line:     9,
			Severity: "high",
			Message:  "forbidden inline date annotation in body; remove per no-date-metadata convention",
		}}, nil
	}
	return nil
}

func (s *governanceFrontmatterAuditUnitSteps) websiteExemption() error {
	// Website-exempt file: the audit function returns zero findings because
	// the walker filters that path out.
	frontmatterAuditFn = func(_ []string) ([]governance.FrontmatterFinding, error) { return nil, nil }
	return nil
}

func (s *governanceFrontmatterAuditUnitSteps) runOnDir() error {
	return s.runWithArgs([]string{"repo-governance/"})
}

func (s *governanceFrontmatterAuditUnitSteps) runOnFile() error {
	return s.runWithArgs([]string{"repo-governance/foo.md"})
}

func (s *governanceFrontmatterAuditUnitSteps) runWithArgs(args []string) error {
	buf := new(bytes.Buffer)
	governanceFrontmatterAuditCmd.SetOut(buf)
	governanceFrontmatterAuditCmd.SetErr(buf)
	s.cmdErr = governanceFrontmatterAuditCmd.RunE(governanceFrontmatterAuditCmd, args)
	s.cmdOutput = buf.String()
	return nil
}

func (s *governanceFrontmatterAuditUnitSteps) exitsSuccessfully() error {
	if s.cmdErr != nil {
		return fmt.Errorf("expected success but got: %w\nOutput: %s", s.cmdErr, s.cmdOutput)
	}
	return nil
}

func (s *governanceFrontmatterAuditUnitSteps) exitsWithFailure() error {
	if s.cmdErr == nil {
		return fmt.Errorf("expected failure but succeeded\nOutput: %s", s.cmdOutput)
	}
	return nil
}

func (s *governanceFrontmatterAuditUnitSteps) zeroFindings() error {
	if !strings.Contains(s.cmdOutput, "PASSED") {
		return fmt.Errorf("expected PASSED in output, got: %s", s.cmdOutput)
	}
	return nil
}

func (s *governanceFrontmatterAuditUnitSteps) outputIdentifiesViolation() error {
	if s.expectVar == "" {
		return fmt.Errorf("test setup error: expectVar not set")
	}
	if !strings.Contains(s.cmdOutput, "foo.md") {
		return fmt.Errorf("expected output to contain offending file path, got: %s", s.cmdOutput)
	}
	if !strings.Contains(s.cmdOutput, s.expectVar) {
		return fmt.Errorf("expected output to contain %q, got: %s", s.expectVar, s.cmdOutput)
	}
	return nil
}

func TestUnitGovernanceFrontmatterAudit(t *testing.T) {
	s := &governanceFrontmatterAuditUnitSteps{}
	suite := godog.TestSuite{
		ScenarioInitializer: func(sc *godog.ScenarioContext) {
			sc.Before(s.before)
			sc.After(s.after)
			sc.Step(stepGovernanceDirNoForbiddenDates, s.cleanDirectory)
			sc.Step(stepGovernanceFileForbiddenUpdatedField, s.forbiddenUpdatedField)
			sc.Step(stepGovernanceFileLastUpdatedFooter, s.lastUpdatedFooter)
			sc.Step(stepGovernanceFileInlineCreatedAnnotation, s.inlineCreatedAnnotation)
			sc.Step(stepWebsiteFileWithForbiddenDateMetadata, s.websiteExemption)
			sc.Step(stepDeveloperRunsFrontmatterAuditOnDir, s.runOnDir)
			sc.Step(stepDeveloperRunsFrontmatterAuditOnFile, s.runOnFile)
			sc.Step(stepOutputZeroFrontmatterFindings, s.zeroFindings)
			sc.Step(stepOutputIdentifiesForbiddenFrontmatterField, s.outputIdentifiesViolation)
			sc.Step(stepOutputIdentifiesForbiddenFooterBlock, s.outputIdentifiesViolation)
			sc.Step(stepOutputIdentifiesForbiddenInlineAnnotation, s.outputIdentifiesViolation)
			sc.Step(stepExitsSuccessfully, s.exitsSuccessfully)
			sc.Step(stepExitsWithFailure, s.exitsWithFailure)
		},
		Options: &godog.Options{
			Format:   "pretty",
			Paths:    []string{specsDirUnitGovernanceFrontmatterAudit},
			TestingT: t,
			Tags:     "repo-governance-frontmatter-audit",
		},
	}
	if suite.Run() != 0 {
		t.Fatal("non-zero status returned, failed to run unit feature tests")
	}
}

// TestGovernanceFrontmatterAudit_MissingGitRoot verifies the command fails
// gracefully when not inside a git repository.
func TestGovernanceFrontmatterAudit_MissingGitRoot(t *testing.T) {
	origGetwd := osGetwd
	origStat := osStat
	defer func() {
		osGetwd = origGetwd
		osStat = origStat
	}()

	osGetwd = func() (string, error) { return "/no-git-here", nil }
	osStat = func(_ string) (os.FileInfo, error) { return nil, os.ErrNotExist }

	buf := new(bytes.Buffer)
	governanceFrontmatterAuditCmd.SetOut(buf)
	governanceFrontmatterAuditCmd.SetErr(buf)
	err := governanceFrontmatterAuditCmd.RunE(governanceFrontmatterAuditCmd, []string{})
	if err == nil || !strings.Contains(err.Error(), "git") {
		t.Fatalf("expected git-root error, got: %v", err)
	}
}

// TestGovernanceFrontmatterAudit_OutputFormats checks that all three output
// formats render non-empty content.
func TestGovernanceFrontmatterAudit_OutputFormats(t *testing.T) {
	origGetwd := osGetwd
	origStat := osStat
	origFn := frontmatterAuditFn
	defer func() {
		osGetwd = origGetwd
		osStat = origStat
		frontmatterAuditFn = origFn
		output = "text"
	}()

	osGetwd = func() (string, error) { return "/mock-repo", nil }
	osStat = func(name string) (os.FileInfo, error) {
		if name == "/mock-repo/.git" {
			return &mockFileInfo{name: ".git", isDir: true}, nil
		}
		return nil, os.ErrNotExist
	}
	frontmatterAuditFn = func(_ []string) ([]governance.FrontmatterFinding, error) {
		return []governance.FrontmatterFinding{{
			File:     "/mock-repo/repo-governance/foo.md",
			Line:     4,
			Severity: "high",
			Message:  `forbidden "updated:" field`,
		}}, nil
	}

	for _, format := range []string{"json", "markdown", "text"} {
		t.Run(format, func(t *testing.T) {
			buf := new(bytes.Buffer)
			governanceFrontmatterAuditCmd.SetOut(buf)
			governanceFrontmatterAuditCmd.SetErr(buf)
			output = format
			verbose = false
			quiet = false
			_ = governanceFrontmatterAuditCmd.RunE(governanceFrontmatterAuditCmd, []string{})
			if buf.Len() == 0 {
				t.Errorf("format %s produced no output", format)
			}
			if format == "json" {
				// Round-trip the JSON envelope.
				var env map[string]any
				if err := json.Unmarshal(buf.Bytes(), &env); err != nil {
					t.Fatalf("invalid JSON envelope: %v\n%s", err, buf.String())
				}
				if env["schema"] != frontmatterAuditSchema {
					t.Errorf("expected schema %q, got %v", frontmatterAuditSchema, env["schema"])
				}
				if env["status"] != "failed" {
					t.Errorf("expected status 'failed', got %v", env["status"])
				}
			}
		})
	}
}

// TestGovernanceFrontmatterAudit_CleanCaseJSON verifies the clean-case
// envelope reports a passing status with an empty result array.
func TestGovernanceFrontmatterAudit_CleanCaseJSON(t *testing.T) {
	origGetwd := osGetwd
	origStat := osStat
	origFn := frontmatterAuditFn
	defer func() {
		osGetwd = origGetwd
		osStat = origStat
		frontmatterAuditFn = origFn
		output = "text"
	}()

	osGetwd = func() (string, error) { return "/mock-repo", nil }
	osStat = func(name string) (os.FileInfo, error) {
		if name == "/mock-repo/.git" {
			return &mockFileInfo{name: ".git", isDir: true}, nil
		}
		return nil, os.ErrNotExist
	}
	frontmatterAuditFn = func(_ []string) ([]governance.FrontmatterFinding, error) { return nil, nil }

	buf := new(bytes.Buffer)
	governanceFrontmatterAuditCmd.SetOut(buf)
	governanceFrontmatterAuditCmd.SetErr(buf)
	output = "json"
	if err := governanceFrontmatterAuditCmd.RunE(governanceFrontmatterAuditCmd, []string{}); err != nil {
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

// TestGovernanceFrontmatterAudit_AuditError ensures internal-package errors
// propagate as command errors.
func TestGovernanceFrontmatterAudit_AuditError(t *testing.T) {
	origGetwd := osGetwd
	origStat := osStat
	origFn := frontmatterAuditFn
	defer func() {
		osGetwd = origGetwd
		osStat = origStat
		frontmatterAuditFn = origFn
	}()

	osGetwd = func() (string, error) { return "/mock-repo", nil }
	osStat = func(name string) (os.FileInfo, error) {
		if name == "/mock-repo/.git" {
			return &mockFileInfo{name: ".git", isDir: true}, nil
		}
		return nil, os.ErrNotExist
	}
	frontmatterAuditFn = func(_ []string) ([]governance.FrontmatterFinding, error) {
		return nil, fmt.Errorf("boom")
	}

	buf := new(bytes.Buffer)
	governanceFrontmatterAuditCmd.SetOut(buf)
	governanceFrontmatterAuditCmd.SetErr(buf)
	err := governanceFrontmatterAuditCmd.RunE(governanceFrontmatterAuditCmd, []string{})
	if err == nil || !strings.Contains(err.Error(), "boom") {
		t.Fatalf("expected propagated error, got: %v", err)
	}
}

// TestResolveFrontmatterAuditPaths verifies the precedence rules for path
// argument selection.
func TestResolveFrontmatterAuditPaths(t *testing.T) {
	tests := []struct {
		name      string
		args      []string
		flagPaths []string
		want      []string
	}{
		{
			name: "default when both empty",
			want: frontmatterAuditDefaultPaths,
		},
		{
			name:      "flag-only",
			flagPaths: []string{"repo-governance/"},
			want:      []string{"repo-governance/"},
		},
		{
			name:      "positional overrides flag",
			args:      []string{"docs/"},
			flagPaths: []string{"repo-governance/"},
			want:      []string{"docs/"},
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			got := resolveFrontmatterAuditPaths(tt.args, tt.flagPaths)
			if !stringSlicesEqual(got, tt.want) {
				t.Errorf("got %v, want %v", got, tt.want)
			}
		})
	}
}

func stringSlicesEqual(a, b []string) bool {
	if len(a) != len(b) {
		return false
	}
	for i := range a {
		if a[i] != b[i] {
			return false
		}
	}
	return true
}

// TestGovernanceFrontmatterAudit_RealTree exercises the real filesystem
// walker against a small tmp fixture for coverage of the walk and parse
// logic.
func TestGovernanceFrontmatterAudit_RealTree(t *testing.T) {
	tmp := t.TempDir()
	govDir := filepath.Join(tmp, "repo-governance")

	writeFile := func(path, content string) {
		t.Helper()
		if err := os.MkdirAll(filepath.Dir(path), 0o755); err != nil {
			t.Fatal(err)
		}
		if err := os.WriteFile(path, []byte(content), 0o644); err != nil {
			t.Fatal(err)
		}
	}
	// Clean file.
	writeFile(filepath.Join(govDir, "clean.md"), "---\ntitle: Clean\n---\nbody\n")
	// Violation: updated field.
	writeFile(filepath.Join(govDir, "bad.md"), "---\ntitle: Bad\nupdated: 2026-04-01\n---\nbody\n")
	// Exempt: under apps/ayokoding-web/.
	exempt := filepath.Join(tmp, "apps", "ayokoding-web", "content", "post.md")
	writeFile(exempt, "---\nupdated: 2026-04-01\n---\n**Last Updated**: 2026-04-01\n")

	findings, err := governance.AuditFrontmatter([]string{govDir, filepath.Join(tmp, "apps")})
	if err != nil {
		t.Fatalf("AuditFrontmatter: %v", err)
	}
	if len(findings) != 1 {
		t.Fatalf("expected exactly 1 finding (bad.md), got %d: %+v", len(findings), findings)
	}
	if !strings.HasSuffix(findings[0].File, "bad.md") {
		t.Errorf("expected bad.md finding, got %q", findings[0].File)
	}
}
