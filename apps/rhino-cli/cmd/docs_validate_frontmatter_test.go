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

// Step constant patterns for docs validate-frontmatter scenarios. Defined
// locally because no other command shares these step phrasings.
const (
	stepSoftwareDocAllFields                 = `^a software-engineering doc with title, description, category, subcategory, and tags frontmatter$`
	stepSoftwareDocAllFieldsTutorial         = `^a software-engineering doc with title, description, category tutorial, subcategory, and tags frontmatter$`
	stepSoftwareDocAllFieldsHowTo            = `^a software-engineering doc with title, description, category how-to, subcategory, and tags frontmatter$`
	stepSoftwareDocAllFieldsReference        = `^a software-engineering doc with title, description, category reference, subcategory, and tags frontmatter$`
	stepSoftwareDocAllFieldsExplanation      = `^a software-engineering doc with title, description, category explanation, subcategory, and tags frontmatter$`
	stepSoftwareDocMissingTitle              = `^a software-engineering doc whose frontmatter omits the title field$`
	stepSoftwareDocMissingCategory           = `^a software-engineering doc whose frontmatter omits the category field$`
	stepSoftwareDocWrongCategoryValue        = `^a software-engineering doc whose frontmatter declares category as something other than software$`
	stepGovernanceDocTitleOnly               = `^a governance doc carrying only a title frontmatter field$`
	stepDeveloperRunsDocsValidateFrontmatter = `^the developer runs docs validate-frontmatter$`
	stepFrontmatterOutputZeroFailFindings    = `^the frontmatter output reports zero fail-level findings$`
	stepFrontmatterOutputIdentifiesField     = `^the frontmatter output identifies the missing title field$`
	stepFrontmatterOutputIdentifiesCategory  = `^the frontmatter output identifies the missing category field$`
	stepFrontmatterOutputIdentifiesWrongCat  = `^the frontmatter output identifies the wrong category value$`
)

var specsDirUnitDocsValidateFrontmatter = func() string {
	_, f, _, _ := runtime.Caller(0)
	return filepath.Join(filepath.Dir(f), "../../../specs/apps/rhino/behavior/cli/gherkin")
}()

type docsValidateFrontmatterUnitSteps struct {
	cmdErr    error
	cmdOutput string
}

func (s *docsValidateFrontmatterUnitSteps) before(_ context.Context, _ *godog.Scenario) (context.Context, error) {
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
	docsValidateFrontmatterFn = func(_ []string) ([]docs.DocsFrontmatterFinding, error) { return nil, nil }
	return context.Background(), nil
}

func (s *docsValidateFrontmatterUnitSteps) after(_ context.Context, _ *godog.Scenario, _ error) (context.Context, error) {
	docsValidateFrontmatterFn = docs.ValidateDocsFrontmatter
	osGetwd = os.Getwd
	osStat = os.Stat
	return context.Background(), nil
}

// softwareDocClean wires the stub to return zero findings.
func (s *docsValidateFrontmatterUnitSteps) softwareDocClean() error {
	docsValidateFrontmatterFn = func(_ []string) ([]docs.DocsFrontmatterFinding, error) {
		return nil, nil
	}
	return nil
}

func (s *docsValidateFrontmatterUnitSteps) softwareDocMissingTitle() error {
	docsValidateFrontmatterFn = func(_ []string) ([]docs.DocsFrontmatterFinding, error) {
		return []docs.DocsFrontmatterFinding{{
			File:     "/mock-repo/docs/explanation/software-engineering/foo.md",
			Severity: "fail",
			Kind:     "missing-title",
			Message:  `required field "title" is missing or empty`,
		}}, nil
	}
	return nil
}

func (s *docsValidateFrontmatterUnitSteps) softwareDocMissingCategory() error {
	docsValidateFrontmatterFn = func(_ []string) ([]docs.DocsFrontmatterFinding, error) {
		return []docs.DocsFrontmatterFinding{{
			File:     "/mock-repo/docs/explanation/software-engineering/foo.md",
			Severity: "fail",
			Kind:     "missing-category",
			Message:  `required field "category" is missing or empty`,
		}}, nil
	}
	return nil
}

func (s *docsValidateFrontmatterUnitSteps) softwareDocWrongCategoryValue() error {
	docsValidateFrontmatterFn = func(_ []string) ([]docs.DocsFrontmatterFinding, error) {
		return []docs.DocsFrontmatterFinding{{
			File:     "/mock-repo/docs/explanation/software-engineering/foo.md",
			Severity: "fail",
			Kind:     "wrong-category-value",
			Message:  `field "category" must equal "software"; found "hardware"`,
		}}, nil
	}
	return nil
}

// diataxisDocClean returns a stub that yields zero findings (for valid Diátaxis
// category values).
func (s *docsValidateFrontmatterUnitSteps) diataxisDocClean() error {
	docsValidateFrontmatterFn = func(_ []string) ([]docs.DocsFrontmatterFinding, error) {
		return nil, nil
	}
	return nil
}

func (s *docsValidateFrontmatterUnitSteps) governanceDocTitleOnly() error {
	// Governance area with only title is clean (description warning is OK to
	// emit, but does not flip exit. For the purposes of this scenario we
	// model a doc carrying both title and description so output is fully
	// clean — but the scenario name says "only a title". To be faithful, we
	// emit a warn-only finding and verify the exit code stays 0.
	docsValidateFrontmatterFn = func(_ []string) ([]docs.DocsFrontmatterFinding, error) {
		return []docs.DocsFrontmatterFinding{{
			File:     "/mock-repo/repo-governance/conventions/structure/test.md",
			Severity: "warn",
			Kind:     "missing-description",
			Message:  `recommended field "description" is missing or empty`,
		}}, nil
	}
	return nil
}

func (s *docsValidateFrontmatterUnitSteps) run() error {
	buf := new(bytes.Buffer)
	docsValidateFrontmatterCmd.SetOut(buf)
	docsValidateFrontmatterCmd.SetErr(buf)
	s.cmdErr = docsValidateFrontmatterCmd.RunE(docsValidateFrontmatterCmd, []string{})
	s.cmdOutput = buf.String()
	return nil
}

func (s *docsValidateFrontmatterUnitSteps) exitsSuccessfully() error {
	if s.cmdErr != nil {
		return fmt.Errorf("expected success but got: %w\nOutput: %s", s.cmdErr, s.cmdOutput)
	}
	return nil
}

func (s *docsValidateFrontmatterUnitSteps) exitsWithFailure() error {
	if s.cmdErr == nil {
		return fmt.Errorf("expected failure but succeeded\nOutput: %s", s.cmdOutput)
	}
	return nil
}

func (s *docsValidateFrontmatterUnitSteps) zeroFailFindings() error {
	if strings.Contains(s.cmdOutput, "FAILED") {
		return fmt.Errorf("expected output not to contain FAILED, got: %s", s.cmdOutput)
	}
	if !strings.Contains(s.cmdOutput, "PASSED") {
		return fmt.Errorf("expected PASSED in output, got: %s", s.cmdOutput)
	}
	return nil
}

func (s *docsValidateFrontmatterUnitSteps) outputIdentifiesMissingTitle() error {
	if !strings.Contains(s.cmdOutput, "missing-title") {
		return fmt.Errorf("expected output to contain missing-title kind, got: %s", s.cmdOutput)
	}
	return nil
}

func (s *docsValidateFrontmatterUnitSteps) outputIdentifiesMissingCategory() error {
	if !strings.Contains(s.cmdOutput, "missing-category") {
		return fmt.Errorf("expected output to contain missing-category kind, got: %s", s.cmdOutput)
	}
	return nil
}

func (s *docsValidateFrontmatterUnitSteps) outputIdentifiesWrongCategory() error {
	if !strings.Contains(s.cmdOutput, "wrong-category-value") {
		return fmt.Errorf("expected output to contain wrong-category-value kind, got: %s", s.cmdOutput)
	}
	return nil
}

// TestUnitDocsValidateFrontmatter runs the godog feature suite against the
// docs-validate-frontmatter.feature file using the mocked
// docsValidateFrontmatterFn entrypoint.
//
// Scenario: Software-engineering doc with all required frontmatter fields passes.
// Scenario: Software-engineering doc missing title fails.
// Scenario: Software-engineering doc missing category field fails.
// Scenario: Software-engineering doc with category other than software fails.
// Scenario: Governance doc with only title passes the lighter schema.
func TestUnitDocsValidateFrontmatter(t *testing.T) {
	s := &docsValidateFrontmatterUnitSteps{}
	suite := godog.TestSuite{
		ScenarioInitializer: func(sc *godog.ScenarioContext) {
			sc.Before(s.before)
			sc.After(s.after)
			sc.Step(stepSoftwareDocAllFields, s.softwareDocClean)
			sc.Step(stepSoftwareDocAllFieldsTutorial, s.diataxisDocClean)
			sc.Step(stepSoftwareDocAllFieldsHowTo, s.diataxisDocClean)
			sc.Step(stepSoftwareDocAllFieldsReference, s.diataxisDocClean)
			sc.Step(stepSoftwareDocAllFieldsExplanation, s.diataxisDocClean)
			sc.Step(stepSoftwareDocMissingTitle, s.softwareDocMissingTitle)
			sc.Step(stepSoftwareDocMissingCategory, s.softwareDocMissingCategory)
			sc.Step(stepSoftwareDocWrongCategoryValue, s.softwareDocWrongCategoryValue)
			sc.Step(stepGovernanceDocTitleOnly, s.governanceDocTitleOnly)
			sc.Step(stepDeveloperRunsDocsValidateFrontmatter, s.run)
			sc.Step(stepExitsSuccessfully, s.exitsSuccessfully)
			sc.Step(stepExitsWithFailure, s.exitsWithFailure)
			sc.Step(stepFrontmatterOutputZeroFailFindings, s.zeroFailFindings)
			sc.Step(stepFrontmatterOutputIdentifiesField, s.outputIdentifiesMissingTitle)
			sc.Step(stepFrontmatterOutputIdentifiesCategory, s.outputIdentifiesMissingCategory)
			sc.Step(stepFrontmatterOutputIdentifiesWrongCat, s.outputIdentifiesWrongCategory)
		},
		Options: &godog.Options{
			Format:   "pretty",
			Paths:    []string{specsDirUnitDocsValidateFrontmatter},
			TestingT: t,
			Tags:     "docs-validate-frontmatter",
		},
	}
	if suite.Run() != 0 {
		t.Fatal("non-zero status returned, failed to run unit feature tests")
	}
}

// TestDocsValidateFrontmatter_MissingGitRoot exercises the git-root lookup
// failure path (not covered by the BDD scenarios — they mock findGitRoot).
func TestDocsValidateFrontmatter_MissingGitRoot(t *testing.T) {
	origGetwd := osGetwd
	origStat := osStat
	defer func() {
		osGetwd = origGetwd
		osStat = origStat
	}()

	osGetwd = func() (string, error) { return "/no-git-here", nil }
	osStat = func(_ string) (os.FileInfo, error) { return nil, os.ErrNotExist }

	buf := new(bytes.Buffer)
	docsValidateFrontmatterCmd.SetOut(buf)
	docsValidateFrontmatterCmd.SetErr(buf)
	err := docsValidateFrontmatterCmd.RunE(docsValidateFrontmatterCmd, []string{})
	if err == nil || !strings.Contains(err.Error(), "git") {
		t.Fatalf("expected git-root error, got: %v", err)
	}
}

// TestDocsValidateFrontmatter_PropagatesValidatorError ensures internal-package
// errors surface as command errors with the expected wrapping.
func TestDocsValidateFrontmatter_PropagatesValidatorError(t *testing.T) {
	origGetwd := osGetwd
	origStat := osStat
	origFn := docsValidateFrontmatterFn
	defer func() {
		osGetwd = origGetwd
		osStat = origStat
		docsValidateFrontmatterFn = origFn
	}()

	osGetwd = func() (string, error) { return "/mock-repo", nil }
	osStat = func(name string) (os.FileInfo, error) {
		if name == "/mock-repo/.git" {
			return &mockFileInfo{name: ".git", isDir: true}, nil
		}
		return nil, os.ErrNotExist
	}
	docsValidateFrontmatterFn = func(_ []string) ([]docs.DocsFrontmatterFinding, error) {
		return nil, fmt.Errorf("simulated validator failure")
	}

	buf := new(bytes.Buffer)
	docsValidateFrontmatterCmd.SetOut(buf)
	docsValidateFrontmatterCmd.SetErr(buf)
	err := docsValidateFrontmatterCmd.RunE(docsValidateFrontmatterCmd, []string{})
	if err == nil || !strings.Contains(err.Error(), "simulated validator failure") {
		t.Fatalf("expected wrapped validator error, got: %v", err)
	}
}

// TestDocsValidateFrontmatter_OutputFormats verifies that all three output
// formats produce non-empty output when fail-level findings are present and
// emit the correct JSON envelope schema.
func TestDocsValidateFrontmatter_OutputFormats(t *testing.T) {
	origGetwd := osGetwd
	origStat := osStat
	origFn := docsValidateFrontmatterFn
	defer func() {
		osGetwd = origGetwd
		osStat = origStat
		docsValidateFrontmatterFn = origFn
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
	docsValidateFrontmatterFn = func(_ []string) ([]docs.DocsFrontmatterFinding, error) {
		return []docs.DocsFrontmatterFinding{{
			File:     "/mock-repo/docs/explanation/software-engineering/foo.md",
			Severity: "fail",
			Kind:     "missing-title",
			Message:  `required field "title" is missing or empty`,
		}}, nil
	}

	for _, format := range []string{"json", "markdown", "text"} {
		t.Run(format, func(t *testing.T) {
			buf := new(bytes.Buffer)
			docsValidateFrontmatterCmd.SetOut(buf)
			docsValidateFrontmatterCmd.SetErr(buf)
			output = format
			if err := parseOutputFormat(nil, nil); err != nil {
				t.Fatalf("parseOutputFormat: %v", err)
			}
			verbose = false
			quiet = false
			_ = docsValidateFrontmatterCmd.RunE(docsValidateFrontmatterCmd, []string{})
			if buf.Len() == 0 {
				t.Errorf("format %s produced no output", format)
			}
			switch format {
			case "json":
				var env map[string]any
				if err := json.Unmarshal(buf.Bytes(), &env); err != nil {
					t.Fatalf("invalid JSON envelope: %v\n%s", err, buf.String())
				}
				if env["schema"] != docsValidateFrontmatterSchema {
					t.Errorf("expected schema %q, got %v", docsValidateFrontmatterSchema, env["schema"])
				}
				if env["status"] != "failed" {
					t.Errorf("expected status 'failed', got %v", env["status"])
				}
			case "markdown":
				if !strings.Contains(buf.String(), "## Docs Frontmatter Validation") {
					t.Errorf("markdown output missing heading, got: %s", buf.String())
				}
			}
		})
	}
}

// TestDocsValidateFrontmatter_WarnOnlyDoesNotFlipExit verifies that a run with
// only warn-level findings exits 0 and the text/markdown output reports
// PASSED with N warn finding(s).
func TestDocsValidateFrontmatter_WarnOnlyDoesNotFlipExit(t *testing.T) {
	origGetwd := osGetwd
	origStat := osStat
	origFn := docsValidateFrontmatterFn
	defer func() {
		osGetwd = origGetwd
		osStat = origStat
		docsValidateFrontmatterFn = origFn
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
	docsValidateFrontmatterFn = func(_ []string) ([]docs.DocsFrontmatterFinding, error) {
		return []docs.DocsFrontmatterFinding{{
			File:     "/mock-repo/repo-governance/conventions/structure/test.md",
			Severity: "warn",
			Kind:     "missing-description",
			Message:  `recommended field "description" is missing or empty`,
		}}, nil
	}

	for _, format := range []string{"text", "json", "markdown"} {
		t.Run(format, func(t *testing.T) {
			buf := new(bytes.Buffer)
			docsValidateFrontmatterCmd.SetOut(buf)
			docsValidateFrontmatterCmd.SetErr(buf)
			output = format
			if err := parseOutputFormat(nil, nil); err != nil {
				t.Fatalf("parseOutputFormat: %v", err)
			}
			err := docsValidateFrontmatterCmd.RunE(docsValidateFrontmatterCmd, []string{})
			if err != nil {
				t.Errorf("warn-only run should exit 0, got: %v", err)
			}
			if format == "json" {
				var env map[string]any
				if err := json.Unmarshal(buf.Bytes(), &env); err != nil {
					t.Fatalf("invalid JSON envelope: %v\n%s", err, buf.String())
				}
				if env["status"] != "passed" {
					t.Errorf("expected status 'passed' for warn-only, got %v", env["status"])
				}
			}
		})
	}
}

// TestDocsValidateFrontmatter_CleanJSON verifies the JSON envelope for a
// clean run reports status "passed" and zero counts.
func TestDocsValidateFrontmatter_CleanJSON(t *testing.T) {
	origGetwd := osGetwd
	origStat := osStat
	origFn := docsValidateFrontmatterFn
	defer func() {
		osGetwd = origGetwd
		osStat = origStat
		docsValidateFrontmatterFn = origFn
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
	docsValidateFrontmatterFn = func(_ []string) ([]docs.DocsFrontmatterFinding, error) { return nil, nil }

	buf := new(bytes.Buffer)
	docsValidateFrontmatterCmd.SetOut(buf)
	docsValidateFrontmatterCmd.SetErr(buf)
	output = "json"
	if err := parseOutputFormat(nil, nil); err != nil {
		t.Fatalf("parseOutputFormat: %v", err)
	}
	if err := docsValidateFrontmatterCmd.RunE(docsValidateFrontmatterCmd, []string{}); err != nil {
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

// TestResolveDocsValidateFrontmatterPaths exercises the path-precedence rule
// (positional args override the default set).
func TestResolveDocsValidateFrontmatterPaths(t *testing.T) {
	tests := []struct {
		name string
		args []string
		want []string
	}{
		{name: "default when args empty", want: docsValidateFrontmatterDefaultPaths},
		{name: "positional override", args: []string{"docs/"}, want: []string{"docs/"}},
		{name: "multiple positionals", args: []string{"a", "b"}, want: []string{"a", "b"}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			got := resolveDocsValidateFrontmatterPaths(tt.args)
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

// TestCountSeverity exercises the small helper used for status counters.
func TestCountSeverity(t *testing.T) {
	in := []docs.DocsFrontmatterFinding{
		{Severity: "fail"},
		{Severity: "warn"},
		{Severity: "fail"},
	}
	if got := countSeverity(in, "fail"); got != 2 {
		t.Errorf("fail count = %d, want 2", got)
	}
	if got := countSeverity(in, "warn"); got != 1 {
		t.Errorf("warn count = %d, want 1", got)
	}
	if got := countSeverity(in, "unknown"); got != 0 {
		t.Errorf("unknown count = %d, want 0", got)
	}
}
