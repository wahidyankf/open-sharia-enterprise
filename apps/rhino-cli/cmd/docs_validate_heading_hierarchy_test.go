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

// Step constant patterns for docs validate-heading-hierarchy scenarios.
const (
	stepDocsTreeAllHeadingsValid                = `^a documentation tree where every markdown file has exactly one H1 and no skipped heading levels$`
	stepDocsTreeTwoH1                           = `^a documentation tree containing a markdown file with two H1 headings$`
	stepDocsTreeH2ThenH4                        = `^a documentation tree containing a markdown file with an H2 followed directly by an H4$`
	stepDocsTreeSingleLineNoHeadings            = `^a documentation tree containing a single-line markdown file with no headings$`
	stepDeveloperRunsDocsValidateHeadings       = `^the developer runs docs validate-heading-hierarchy$`
	stepOutputZeroDocsHeadingFindings           = `^the output reports zero docs heading hierarchy findings$`
	stepOutputIdentifiesDuplicateH1Violation    = `^the output identifies the offending file and the duplicate H1 violation$`
	stepOutputIdentifiesSkippedHeadingViolation = `^the output identifies the offending file and the skipped heading level$`
)

var specsDirUnitDocsValidateHeadingHierarchy = func() string {
	_, f, _, _ := runtime.Caller(0)
	return filepath.Join(filepath.Dir(f), "../../../specs/apps/rhino/behavior/cli/gherkin")
}()

type docsValidateHeadingHierarchyUnitSteps struct {
	cmdErr     error
	cmdOutput  string
	expectFile string
	expectKind string
}

func (s *docsValidateHeadingHierarchyUnitSteps) before(_ context.Context, _ *godog.Scenario) (context.Context, error) {
	verbose = false
	quiet = false
	output = "text"
	s.cmdErr = nil
	s.cmdOutput = ""
	s.expectFile = ""
	s.expectKind = ""

	osGetwd = func() (string, error) { return "/mock-repo", nil }
	osStat = func(name string) (os.FileInfo, error) {
		if name == "/mock-repo/.git" {
			return &mockFileInfo{name: ".git", isDir: true}, nil
		}
		return nil, os.ErrNotExist
	}
	docsValidateHeadingHierarchyFn = func(_ []string) ([]docs.DocsHeadingFinding, error) { return nil, nil }
	return context.Background(), nil
}

func (s *docsValidateHeadingHierarchyUnitSteps) after(_ context.Context, _ *godog.Scenario, _ error) (context.Context, error) {
	docsValidateHeadingHierarchyFn = docs.ValidateDocsHeadingHierarchy
	osGetwd = os.Getwd
	osStat = os.Stat
	return context.Background(), nil
}

func (s *docsValidateHeadingHierarchyUnitSteps) cleanTree() error {
	docsValidateHeadingHierarchyFn = func(_ []string) ([]docs.DocsHeadingFinding, error) {
		return nil, nil
	}
	return nil
}

func (s *docsValidateHeadingHierarchyUnitSteps) twoH1Violation() error {
	s.expectFile = "/mock-repo/docs/two-h1.md"
	s.expectKind = "duplicate-h1"
	docsValidateHeadingHierarchyFn = func(_ []string) ([]docs.DocsHeadingFinding, error) {
		return []docs.DocsHeadingFinding{{
			File:     s.expectFile,
			Line:     5,
			Severity: "high",
			Kind:     s.expectKind,
			Message:  "markdown file has 2 H1 headings (first at line 1); every file must have exactly one H1",
		}}, nil
	}
	return nil
}

func (s *docsValidateHeadingHierarchyUnitSteps) skippedLevelViolation() error {
	s.expectFile = "/mock-repo/docs/skipped.md"
	s.expectKind = "skipped-level"
	docsValidateHeadingHierarchyFn = func(_ []string) ([]docs.DocsHeadingFinding, error) {
		return []docs.DocsHeadingFinding{{
			File:     s.expectFile,
			Line:     7,
			Severity: "high",
			Kind:     s.expectKind,
			Message:  "H4 heading follows H2, skipping H3; heading levels must not skip",
		}}, nil
	}
	return nil
}

func (s *docsValidateHeadingHierarchyUnitSteps) singleLineNoHeadings() error {
	// Files with no headings are clean — validator returns zero findings.
	docsValidateHeadingHierarchyFn = func(_ []string) ([]docs.DocsHeadingFinding, error) {
		return nil, nil
	}
	return nil
}

func (s *docsValidateHeadingHierarchyUnitSteps) run() error {
	buf := new(bytes.Buffer)
	docsValidateHeadingHierarchyCmd.SetOut(buf)
	docsValidateHeadingHierarchyCmd.SetErr(buf)
	s.cmdErr = docsValidateHeadingHierarchyCmd.RunE(docsValidateHeadingHierarchyCmd, []string{})
	s.cmdOutput = buf.String()
	return nil
}

func (s *docsValidateHeadingHierarchyUnitSteps) exitsSuccessfully() error {
	if s.cmdErr != nil {
		return fmt.Errorf("expected success but got: %w\nOutput: %s", s.cmdErr, s.cmdOutput)
	}
	return nil
}

func (s *docsValidateHeadingHierarchyUnitSteps) exitsWithFailure() error {
	if s.cmdErr == nil {
		return fmt.Errorf("expected failure but succeeded\nOutput: %s", s.cmdOutput)
	}
	return nil
}

func (s *docsValidateHeadingHierarchyUnitSteps) zeroFindings() error {
	if !strings.Contains(s.cmdOutput, "PASSED") {
		return fmt.Errorf("expected PASSED in output, got: %s", s.cmdOutput)
	}
	return nil
}

func (s *docsValidateHeadingHierarchyUnitSteps) outputIdentifiesViolation() error {
	if s.expectFile == "" || s.expectKind == "" {
		return fmt.Errorf("test setup error: expectFile/expectKind not set")
	}
	if !strings.Contains(s.cmdOutput, s.expectFile) {
		return fmt.Errorf("expected output to contain %q, got: %s", s.expectFile, s.cmdOutput)
	}
	if !strings.Contains(s.cmdOutput, s.expectKind) {
		return fmt.Errorf("expected output to mention kind %q, got: %s", s.expectKind, s.cmdOutput)
	}
	return nil
}

func TestUnitDocsValidateHeadingHierarchy(t *testing.T) {
	s := &docsValidateHeadingHierarchyUnitSteps{}
	suite := godog.TestSuite{
		ScenarioInitializer: func(sc *godog.ScenarioContext) {
			sc.Before(s.before)
			sc.After(s.after)
			sc.Step(stepDocsTreeAllHeadingsValid, s.cleanTree)
			sc.Step(stepDocsTreeTwoH1, s.twoH1Violation)
			sc.Step(stepDocsTreeH2ThenH4, s.skippedLevelViolation)
			sc.Step(stepDocsTreeSingleLineNoHeadings, s.singleLineNoHeadings)
			sc.Step(stepDeveloperRunsDocsValidateHeadings, s.run)
			sc.Step(stepExitsSuccessfully, s.exitsSuccessfully)
			sc.Step(stepExitsWithFailure, s.exitsWithFailure)
			sc.Step(stepOutputZeroDocsHeadingFindings, s.zeroFindings)
			sc.Step(stepOutputIdentifiesDuplicateH1Violation, s.outputIdentifiesViolation)
			sc.Step(stepOutputIdentifiesSkippedHeadingViolation, s.outputIdentifiesViolation)
		},
		Options: &godog.Options{
			Format:   "pretty",
			Paths:    []string{specsDirUnitDocsValidateHeadingHierarchy},
			TestingT: t,
			Tags:     "docs-validate-heading-hierarchy",
		},
	}
	if suite.Run() != 0 {
		t.Fatal("non-zero status returned, failed to run unit feature tests")
	}
}

// TestDocsValidateHeadingHierarchy_MissingGitRoot exercises the git-root
// lookup failure path (BDD scenarios mock findGitRoot).
func TestDocsValidateHeadingHierarchy_MissingGitRoot(t *testing.T) {
	origGetwd := osGetwd
	origStat := osStat
	defer func() {
		osGetwd = origGetwd
		osStat = origStat
	}()

	osGetwd = func() (string, error) { return "/no-git-here", nil }
	osStat = func(_ string) (os.FileInfo, error) { return nil, os.ErrNotExist }

	buf := new(bytes.Buffer)
	docsValidateHeadingHierarchyCmd.SetOut(buf)
	docsValidateHeadingHierarchyCmd.SetErr(buf)
	err := docsValidateHeadingHierarchyCmd.RunE(docsValidateHeadingHierarchyCmd, []string{})
	if err == nil || !strings.Contains(err.Error(), "git") {
		t.Fatalf("expected git-root error, got: %v", err)
	}
}

// TestDocsValidateHeadingHierarchy_OutputFormats covers all three formats
// and validates the JSON envelope's schema/status fields when findings
// exist.
func TestDocsValidateHeadingHierarchy_OutputFormats(t *testing.T) {
	origGetwd := osGetwd
	origStat := osStat
	origFn := docsValidateHeadingHierarchyFn
	defer func() {
		osGetwd = origGetwd
		osStat = origStat
		docsValidateHeadingHierarchyFn = origFn
		output = "text"
	}()

	osGetwd = func() (string, error) { return "/mock-repo", nil }
	osStat = func(name string) (os.FileInfo, error) {
		if name == "/mock-repo/.git" {
			return &mockFileInfo{name: ".git", isDir: true}, nil
		}
		return nil, os.ErrNotExist
	}
	docsValidateHeadingHierarchyFn = func(_ []string) ([]docs.DocsHeadingFinding, error) {
		return []docs.DocsHeadingFinding{{
			File:     "/mock-repo/docs/bad.md",
			Line:     7,
			Severity: "high",
			Kind:     "skipped-level",
			Message:  "H4 heading follows H2, skipping H3",
		}}, nil
	}

	for _, format := range []string{"json", "markdown", "text"} {
		t.Run(format, func(t *testing.T) {
			buf := new(bytes.Buffer)
			docsValidateHeadingHierarchyCmd.SetOut(buf)
			docsValidateHeadingHierarchyCmd.SetErr(buf)
			output = format
			verbose = false
			quiet = false
			_ = docsValidateHeadingHierarchyCmd.RunE(docsValidateHeadingHierarchyCmd, []string{})
			if buf.Len() == 0 {
				t.Errorf("format %s produced no output", format)
			}
			if format == "json" {
				var env map[string]any
				if err := json.Unmarshal(buf.Bytes(), &env); err != nil {
					t.Fatalf("invalid JSON envelope: %v\n%s", err, buf.String())
				}
				if env["schema"] != docsValidateHeadingHierarchySchema {
					t.Errorf("expected schema %q, got %v", docsValidateHeadingHierarchySchema, env["schema"])
				}
				if env["status"] != "failed" {
					t.Errorf("expected status 'failed', got %v", env["status"])
				}
			}
		})
	}
}

// TestDocsValidateHeadingHierarchy_CleanCaseJSON verifies a clean run's
// JSON envelope reports `status: passed` and an empty result array.
func TestDocsValidateHeadingHierarchy_CleanCaseJSON(t *testing.T) {
	origGetwd := osGetwd
	origStat := osStat
	origFn := docsValidateHeadingHierarchyFn
	defer func() {
		osGetwd = origGetwd
		osStat = origStat
		docsValidateHeadingHierarchyFn = origFn
		output = "text"
	}()

	osGetwd = func() (string, error) { return "/mock-repo", nil }
	osStat = func(name string) (os.FileInfo, error) {
		if name == "/mock-repo/.git" {
			return &mockFileInfo{name: ".git", isDir: true}, nil
		}
		return nil, os.ErrNotExist
	}
	docsValidateHeadingHierarchyFn = func(_ []string) ([]docs.DocsHeadingFinding, error) {
		return nil, nil
	}

	buf := new(bytes.Buffer)
	docsValidateHeadingHierarchyCmd.SetOut(buf)
	docsValidateHeadingHierarchyCmd.SetErr(buf)
	output = "json"
	if err := docsValidateHeadingHierarchyCmd.RunE(docsValidateHeadingHierarchyCmd, []string{}); err != nil {
		t.Fatalf("expected clean run to succeed, got: %v", err)
	}
	var env map[string]any
	if err := json.Unmarshal(buf.Bytes(), &env); err != nil {
		t.Fatalf("invalid JSON envelope: %v\n%s", err, buf.String())
	}
	if env["status"] != "passed" {
		t.Errorf("expected status 'passed' for clean run, got %v", env["status"])
	}
	if env["schema"] != docsValidateHeadingHierarchySchema {
		t.Errorf("expected schema %q, got %v", docsValidateHeadingHierarchySchema, env["schema"])
	}
}

// TestDocsValidateHeadingHierarchy_PropagatesValidatorError ensures
// internal-package errors surface as command errors.
func TestDocsValidateHeadingHierarchy_PropagatesValidatorError(t *testing.T) {
	origGetwd := osGetwd
	origStat := osStat
	origFn := docsValidateHeadingHierarchyFn
	defer func() {
		osGetwd = origGetwd
		osStat = origStat
		docsValidateHeadingHierarchyFn = origFn
	}()

	osGetwd = func() (string, error) { return "/mock-repo", nil }
	osStat = func(name string) (os.FileInfo, error) {
		if name == "/mock-repo/.git" {
			return &mockFileInfo{name: ".git", isDir: true}, nil
		}
		return nil, os.ErrNotExist
	}
	docsValidateHeadingHierarchyFn = func(_ []string) ([]docs.DocsHeadingFinding, error) {
		return nil, fmt.Errorf("boom")
	}

	buf := new(bytes.Buffer)
	docsValidateHeadingHierarchyCmd.SetOut(buf)
	docsValidateHeadingHierarchyCmd.SetErr(buf)
	err := docsValidateHeadingHierarchyCmd.RunE(docsValidateHeadingHierarchyCmd, []string{})
	if err == nil || !strings.Contains(err.Error(), "boom") {
		t.Fatalf("expected propagated error, got: %v", err)
	}
}

// TestResolveDocsValidateHeadingHierarchyPaths exercises the path-precedence
// rule (positional args override default set).
func TestResolveDocsValidateHeadingHierarchyPaths(t *testing.T) {
	tests := []struct {
		name string
		args []string
		want []string
	}{
		{name: "default when args empty", want: docsValidateHeadingHierarchyDefaultPaths},
		{name: "positional override", args: []string{"docs/"}, want: []string{"docs/"}},
		{name: "multiple positionals", args: []string{"a", "b"}, want: []string{"a", "b"}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			got := resolveDocsValidateHeadingHierarchyPaths(tt.args)
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

// TestDocsValidateHeadingHierarchy_RealTree exercises the actual filesystem
// walker against a small tmp fixture for coverage of the walk and rule check.
func TestDocsValidateHeadingHierarchy_RealTree(t *testing.T) {
	tmp := t.TempDir()
	if err := os.MkdirAll(filepath.Join(tmp, "docs"), 0o755); err != nil {
		t.Fatal(err)
	}
	files := map[string]string{
		"docs/clean.md":       "# Title\n\n## Section\n",
		"docs/two-h1.md":      "# A\n\n# B\n",
		"docs/skipped.md":     "# Title\n\n## H2\n\n#### Skipped\n",
		"docs/no-headings.md": "just prose\n",
	}
	for rel, content := range files {
		full := filepath.Join(tmp, rel)
		if err := os.WriteFile(full, []byte(content), 0o644); err != nil {
			t.Fatal(err)
		}
	}

	findings, err := docs.ValidateDocsHeadingHierarchy([]string{filepath.Join(tmp, "docs")})
	if err != nil {
		t.Fatalf("ValidateDocsHeadingHierarchy: %v", err)
	}
	if len(findings) != 2 {
		t.Fatalf("expected 2 findings (duplicate-h1 + skipped-level), got %d: %+v", len(findings), findings)
	}
}

// TestDocsValidateHeadingHierarchy_MarkdownCleanCase exercises the "PASSED"
// branch of the markdown formatter, which is otherwise skipped because the
// other format tests always inject findings.
func TestDocsValidateHeadingHierarchy_MarkdownCleanCase(t *testing.T) {
	out := formatDocsValidateHeadingHierarchyMarkdown(nil)
	if !strings.Contains(out, "PASSED") {
		t.Errorf("expected PASSED in markdown clean case, got: %s", out)
	}
}
