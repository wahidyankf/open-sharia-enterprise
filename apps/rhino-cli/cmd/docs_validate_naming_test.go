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
	"github.com/wahidyankf/ose-public/apps/rhino-cli/internal/docs"
)

// Step constant patterns for docs validate-naming scenarios.
const (
	stepDocsTreeAllKebabCase                = `^a documentation tree where every markdown file uses lowercase kebab-case$`
	stepDocsTreeFileWithUppercase           = `^a documentation tree containing a markdown file whose basename has uppercase characters$`
	stepDocsTreeNestedReadmeOnly            = `^a documentation tree where a nested directory contains only a README\.md file$`
	stepDeveloperRunsDocsValidateNaming     = `^the developer runs docs validate-naming$`
	stepOutputZeroDocsNamingFindings        = `^the output reports zero docs naming findings$`
	stepOutputIdentifiesDocsNamingViolation = `^the output identifies the offending filename and its rule violation$`
)

var specsDirUnitDocsValidateNaming = func() string {
	_, f, _, _ := runtime.Caller(0)
	return filepath.Join(filepath.Dir(f), "../../../specs/apps/rhino/behavior/cli/gherkin")
}()

type docsValidateNamingUnitSteps struct {
	cmdErr    error
	cmdOutput string
	expectVar string
}

func (s *docsValidateNamingUnitSteps) before(_ context.Context, _ *godog.Scenario) (context.Context, error) {
	verbose = false
	quiet = false
	output = "text"
	s.cmdErr = nil
	s.cmdOutput = ""
	s.expectVar = ""
	docsValidateNamingExempts = nil

	osGetwd = func() (string, error) { return "/mock-repo", nil }
	osStat = func(name string) (os.FileInfo, error) {
		if name == "/mock-repo/.git" {
			return &mockFileInfo{name: ".git", isDir: true}, nil
		}
		return nil, os.ErrNotExist
	}
	docsValidateNamingFn = func(_ []string, _ []string) ([]docs.DocsNamingFinding, error) { return nil, nil }
	return context.Background(), nil
}

func (s *docsValidateNamingUnitSteps) after(_ context.Context, _ *godog.Scenario, _ error) (context.Context, error) {
	docsValidateNamingFn = docs.ValidateDocsNaming
	osGetwd = os.Getwd
	osStat = os.Stat
	docsValidateNamingExempts = nil
	return context.Background(), nil
}

func (s *docsValidateNamingUnitSteps) cleanTree() error {
	docsValidateNamingFn = func(_ []string, _ []string) ([]docs.DocsNamingFinding, error) {
		return nil, nil
	}
	return nil
}

func (s *docsValidateNamingUnitSteps) uppercaseViolation() error {
	s.expectVar = "MyDoc.md"
	docsValidateNamingFn = func(_ []string, _ []string) ([]docs.DocsNamingFinding, error) {
		return []docs.DocsNamingFinding{{
			File:     "/mock-repo/docs/MyDoc.md",
			Severity: "high",
			Message:  `filename "MyDoc.md" violates lowercase-kebab-case rule (^[a-z0-9-]+\.md$); rename to lowercase-kebab-case or add an exemption`,
		}}, nil
	}
	return nil
}

func (s *docsValidateNamingUnitSteps) nestedReadmeOnly() error {
	// README.md is exempt at any depth; validator returns zero findings.
	docsValidateNamingFn = func(_ []string, _ []string) ([]docs.DocsNamingFinding, error) {
		return nil, nil
	}
	return nil
}

func (s *docsValidateNamingUnitSteps) run() error {
	buf := new(bytes.Buffer)
	docsValidateNamingCmd.SetOut(buf)
	docsValidateNamingCmd.SetErr(buf)
	s.cmdErr = docsValidateNamingCmd.RunE(docsValidateNamingCmd, []string{})
	s.cmdOutput = buf.String()
	return nil
}

func (s *docsValidateNamingUnitSteps) exitsSuccessfully() error {
	if s.cmdErr != nil {
		return fmt.Errorf("expected success but got: %w\nOutput: %s", s.cmdErr, s.cmdOutput)
	}
	return nil
}

func (s *docsValidateNamingUnitSteps) exitsWithFailure() error {
	if s.cmdErr == nil {
		return fmt.Errorf("expected failure but succeeded\nOutput: %s", s.cmdOutput)
	}
	return nil
}

func (s *docsValidateNamingUnitSteps) zeroFindings() error {
	if !strings.Contains(s.cmdOutput, "PASSED") {
		return fmt.Errorf("expected PASSED in output, got: %s", s.cmdOutput)
	}
	return nil
}

func (s *docsValidateNamingUnitSteps) outputIdentifiesViolation() error {
	if s.expectVar == "" {
		return fmt.Errorf("test setup error: expectVar not set")
	}
	if !strings.Contains(s.cmdOutput, s.expectVar) {
		return fmt.Errorf("expected output to contain %q, got: %s", s.expectVar, s.cmdOutput)
	}
	if !strings.Contains(s.cmdOutput, "lowercase-kebab-case") {
		return fmt.Errorf("expected output to mention rule, got: %s", s.cmdOutput)
	}
	return nil
}

func TestUnitDocsValidateNaming(t *testing.T) {
	s := &docsValidateNamingUnitSteps{}
	suite := godog.TestSuite{
		ScenarioInitializer: func(sc *godog.ScenarioContext) {
			sc.Before(s.before)
			sc.After(s.after)
			sc.Step(stepDocsTreeAllKebabCase, s.cleanTree)
			sc.Step(stepDocsTreeFileWithUppercase, s.uppercaseViolation)
			sc.Step(stepDocsTreeNestedReadmeOnly, s.nestedReadmeOnly)
			sc.Step(stepDeveloperRunsDocsValidateNaming, s.run)
			sc.Step(stepExitsSuccessfully, s.exitsSuccessfully)
			sc.Step(stepExitsWithFailure, s.exitsWithFailure)
			sc.Step(stepOutputZeroDocsNamingFindings, s.zeroFindings)
			sc.Step(stepOutputIdentifiesDocsNamingViolation, s.outputIdentifiesViolation)
		},
		Options: &godog.Options{
			Format:   "pretty",
			Paths:    []string{specsDirUnitDocsValidateNaming},
			TestingT: t,
			Tags:     "docs-validate-naming",
		},
	}
	if suite.Run() != 0 {
		t.Fatal("non-zero status returned, failed to run unit feature tests")
	}
}

// TestDocsValidateNaming_MissingGitRoot exercises the git-root lookup failure
// path which is not covered by the BDD scenarios (they mock findGitRoot).
func TestDocsValidateNaming_MissingGitRoot(t *testing.T) {
	origGetwd := osGetwd
	origStat := osStat
	defer func() {
		osGetwd = origGetwd
		osStat = origStat
	}()

	osGetwd = func() (string, error) { return "/no-git-here", nil }
	osStat = func(_ string) (os.FileInfo, error) { return nil, os.ErrNotExist }

	buf := new(bytes.Buffer)
	docsValidateNamingCmd.SetOut(buf)
	docsValidateNamingCmd.SetErr(buf)
	err := docsValidateNamingCmd.RunE(docsValidateNamingCmd, []string{})
	if err == nil || !strings.Contains(err.Error(), "git") {
		t.Fatalf("expected git-root error, got: %v", err)
	}
}

// TestDocsValidateNaming_OutputFormats verifies that text, json, and markdown
// outputs all render non-empty content and the JSON branch emits a valid
// envelope.
func TestDocsValidateNaming_OutputFormats(t *testing.T) {
	origGetwd := osGetwd
	origStat := osStat
	origFn := docsValidateNamingFn
	defer func() {
		osGetwd = origGetwd
		osStat = origStat
		docsValidateNamingFn = origFn
		output = "text"
	}()

	osGetwd = func() (string, error) { return "/mock-repo", nil }
	osStat = func(name string) (os.FileInfo, error) {
		if name == "/mock-repo/.git" {
			return &mockFileInfo{name: ".git", isDir: true}, nil
		}
		return nil, os.ErrNotExist
	}
	docsValidateNamingFn = func(_ []string, _ []string) ([]docs.DocsNamingFinding, error) {
		return []docs.DocsNamingFinding{{
			File:     "/mock-repo/docs/MyDoc.md",
			Severity: "high",
			Message:  `filename "MyDoc.md" violates lowercase-kebab-case rule`,
		}}, nil
	}

	for _, format := range []string{"json", "markdown", "text"} {
		t.Run(format, func(t *testing.T) {
			buf := new(bytes.Buffer)
			docsValidateNamingCmd.SetOut(buf)
			docsValidateNamingCmd.SetErr(buf)
			output = format
			verbose = false
			quiet = false
			_ = docsValidateNamingCmd.RunE(docsValidateNamingCmd, []string{})
			if buf.Len() == 0 {
				t.Errorf("format %s produced no output", format)
			}
			if format == "json" {
				var env map[string]any
				if err := json.Unmarshal(buf.Bytes(), &env); err != nil {
					t.Fatalf("invalid JSON envelope: %v\n%s", err, buf.String())
				}
				if env["schema"] != docsValidateNamingSchema {
					t.Errorf("expected schema %q, got %v", docsValidateNamingSchema, env["schema"])
				}
				if env["status"] != "failed" {
					t.Errorf("expected status 'failed', got %v", env["status"])
				}
			}
		})
	}
}

// TestDocsValidateNaming_CleanCaseJSON verifies the JSON envelope for a clean
// run reports `status: "passed"` and an empty result array.
func TestDocsValidateNaming_CleanCaseJSON(t *testing.T) {
	origGetwd := osGetwd
	origStat := osStat
	origFn := docsValidateNamingFn
	defer func() {
		osGetwd = origGetwd
		osStat = origStat
		docsValidateNamingFn = origFn
		output = "text"
	}()

	osGetwd = func() (string, error) { return "/mock-repo", nil }
	osStat = func(name string) (os.FileInfo, error) {
		if name == "/mock-repo/.git" {
			return &mockFileInfo{name: ".git", isDir: true}, nil
		}
		return nil, os.ErrNotExist
	}
	docsValidateNamingFn = func(_ []string, _ []string) ([]docs.DocsNamingFinding, error) { return nil, nil }

	buf := new(bytes.Buffer)
	docsValidateNamingCmd.SetOut(buf)
	docsValidateNamingCmd.SetErr(buf)
	output = "json"
	if err := docsValidateNamingCmd.RunE(docsValidateNamingCmd, []string{}); err != nil {
		t.Fatalf("expected clean run to succeed, got: %v", err)
	}
	var env map[string]any
	if err := json.Unmarshal(buf.Bytes(), &env); err != nil {
		t.Fatalf("invalid JSON envelope: %v\n%s", err, buf.String())
	}
	if env["status"] != "passed" {
		t.Errorf("expected status 'passed' for clean run, got %v", env["status"])
	}
	if env["schema"] != docsValidateNamingSchema {
		t.Errorf("expected schema %q, got %v", docsValidateNamingSchema, env["schema"])
	}
}

// TestDocsValidateNaming_PropagatesValidatorError ensures internal-package
// errors surface as command errors with the expected wrapping.
func TestDocsValidateNaming_PropagatesValidatorError(t *testing.T) {
	origGetwd := osGetwd
	origStat := osStat
	origFn := docsValidateNamingFn
	defer func() {
		osGetwd = origGetwd
		osStat = origStat
		docsValidateNamingFn = origFn
	}()

	osGetwd = func() (string, error) { return "/mock-repo", nil }
	osStat = func(name string) (os.FileInfo, error) {
		if name == "/mock-repo/.git" {
			return &mockFileInfo{name: ".git", isDir: true}, nil
		}
		return nil, os.ErrNotExist
	}
	docsValidateNamingFn = func(_ []string, _ []string) ([]docs.DocsNamingFinding, error) {
		return nil, fmt.Errorf("boom")
	}

	buf := new(bytes.Buffer)
	docsValidateNamingCmd.SetOut(buf)
	docsValidateNamingCmd.SetErr(buf)
	err := docsValidateNamingCmd.RunE(docsValidateNamingCmd, []string{})
	if err == nil || !strings.Contains(err.Error(), "boom") {
		t.Fatalf("expected propagated error, got: %v", err)
	}
}

// TestResolveDocsValidateNamingPaths exercises the path-precedence rule
// (positional args override default set).
func TestResolveDocsValidateNamingPaths(t *testing.T) {
	tests := []struct {
		name string
		args []string
		want []string
	}{
		{name: "default when args empty", want: docsValidateNamingDefaultPaths},
		{name: "positional override", args: []string{"docs/"}, want: []string{"docs/"}},
		{name: "multiple positionals", args: []string{"a", "b"}, want: []string{"a", "b"}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			got := resolveDocsValidateNamingPaths(tt.args)
			if len(got) != len(tt.want) {
				t.Fatalf("got %v, want %v", got, tt.want)
			}
			for i := range got {
				if got[i] != tt.want[i] {
					t.Fatalf("got %v, want %v", got, tt.want)
				}
			}
		})
	}
}

// TestDocsValidateNaming_RealTree exercises the actual filesystem walker
// against a small tmp fixture for coverage of the walk and rule check logic.
func TestDocsValidateNaming_RealTree(t *testing.T) {
	tmp := t.TempDir()
	if err := os.MkdirAll(filepath.Join(tmp, "docs"), 0o755); err != nil {
		t.Fatal(err)
	}
	files := map[string]string{
		"docs/fine-name.md": "ok",
		"docs/MyDoc.md":     "bad",
		"docs/README.md":    "exempt",
	}
	for rel, content := range files {
		full := filepath.Join(tmp, rel)
		if err := os.WriteFile(full, []byte(content), 0o644); err != nil {
			t.Fatal(err)
		}
	}

	findings, err := docs.ValidateDocsNaming([]string{filepath.Join(tmp, "docs")}, nil)
	if err != nil {
		t.Fatalf("ValidateDocsNaming: %v", err)
	}
	if len(findings) != 1 {
		t.Fatalf("expected 1 finding (MyDoc.md), got %d: %+v", len(findings), findings)
	}
	if !strings.HasSuffix(findings[0].File, "MyDoc.md") {
		t.Errorf("expected MyDoc.md, got %q", findings[0].File)
	}
}
