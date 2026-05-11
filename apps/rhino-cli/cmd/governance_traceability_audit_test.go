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

// Step constant patterns for repo-governance traceability-audit scenarios.
// Defined locally (rather than in steps_common_test.go) because no other
// command currently shares these step phrasings.
const (
	stepTraceabilityCleanRepo                    = `^a repository where every governance document carries the required traceability sections$`
	stepTraceabilityPrincipleMissingVision       = `^a repository with a principle file that is missing the "## Vision Supported" heading$`
	stepTraceabilityConventionMissingPrinciples  = `^a repository with a convention file that is missing the "## Principles Implemented/Respected" heading$`
	stepTraceabilityDevelopmentMissingConvs      = `^a repository with a development file that is missing the "## Conventions Implemented/Respected" heading$`
	stepTraceabilityWorkflowMissingAgentRef      = `^a repository with a workflow file that contains no reference to any \.claude/agents/ file$`
	stepDeveloperRunsTraceabilityAudit           = `^the developer runs repo-governance traceability-audit$`
	stepTraceabilityOutputZeroFindings           = `^the traceability output reports zero findings$`
	stepTraceabilityOutputIdentifiesVision       = `^the traceability output identifies the missing Vision Supported section$`
	stepTraceabilityOutputIdentifiesPrinciples   = `^the traceability output identifies the missing Principles Implemented section$`
	stepTraceabilityOutputIdentifiesConventions  = `^the traceability output identifies the missing Conventions Implemented section$`
	stepTraceabilityOutputIdentifiesAgentMissing = `^the traceability output identifies the missing agent reference$`
)

var specsDirUnitTraceabilityAudit = func() string {
	_, f, _, _ := runtime.Caller(0)
	return filepath.Join(filepath.Dir(f), "../../../specs/apps/rhino/behavior/cli/gherkin")
}()

type traceabilityAuditUnitSteps struct {
	cmdErr    error
	cmdOutput string
}

func (s *traceabilityAuditUnitSteps) before(_ context.Context, _ *godog.Scenario) (context.Context, error) {
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
	traceabilityAuditFn = func(_ string) ([]governance.TraceabilityFinding, error) { return nil, nil }
	return context.Background(), nil
}

func (s *traceabilityAuditUnitSteps) after(_ context.Context, _ *godog.Scenario, _ error) (context.Context, error) {
	traceabilityAuditFn = governance.AuditTraceability
	osGetwd = os.Getwd
	osStat = os.Stat
	return context.Background(), nil
}

func (s *traceabilityAuditUnitSteps) cleanRepo() error {
	traceabilityAuditFn = func(_ string) ([]governance.TraceabilityFinding, error) { return nil, nil }
	return nil
}

func (s *traceabilityAuditUnitSteps) principleMissingVision() error {
	traceabilityAuditFn = func(_ string) ([]governance.TraceabilityFinding, error) {
		return []governance.TraceabilityFinding{{
			Path:    "/mock-repo/repo-governance/principles/general/sample.md",
			Line:    1,
			Kind:    governance.TraceabilityMissingVisionSupported,
			Message: `principle is missing required "## Vision Supported" heading`,
		}}, nil
	}
	return nil
}

func (s *traceabilityAuditUnitSteps) conventionMissingPrinciples() error {
	traceabilityAuditFn = func(_ string) ([]governance.TraceabilityFinding, error) {
		return []governance.TraceabilityFinding{{
			Path:    "/mock-repo/repo-governance/conventions/structure/sample.md",
			Line:    1,
			Kind:    governance.TraceabilityMissingPrinciplesImplemented,
			Message: `convention is missing required "## Principles Implemented/Respected" heading`,
		}}, nil
	}
	return nil
}

func (s *traceabilityAuditUnitSteps) developmentMissingConventions() error {
	traceabilityAuditFn = func(_ string) ([]governance.TraceabilityFinding, error) {
		return []governance.TraceabilityFinding{{
			Path:    "/mock-repo/repo-governance/development/workflow/sample.md",
			Line:    1,
			Kind:    governance.TraceabilityMissingConventionsImplemented,
			Message: `development doc is missing required "## Conventions Implemented/Respected" heading`,
		}}, nil
	}
	return nil
}

func (s *traceabilityAuditUnitSteps) workflowMissingAgentReference() error {
	traceabilityAuditFn = func(_ string) ([]governance.TraceabilityFinding, error) {
		return []governance.TraceabilityFinding{{
			Path:    "/mock-repo/repo-governance/workflows/plan/sample-execution.md",
			Line:    1,
			Kind:    governance.TraceabilityMissingAgentReference,
			Message: `workflow does not reference any .claude/agents/<name>.md file`,
		}}, nil
	}
	return nil
}

func (s *traceabilityAuditUnitSteps) run() error {
	buf := new(bytes.Buffer)
	governanceTraceabilityAuditCmd.SetOut(buf)
	governanceTraceabilityAuditCmd.SetErr(buf)
	s.cmdErr = governanceTraceabilityAuditCmd.RunE(governanceTraceabilityAuditCmd, []string{})
	s.cmdOutput = buf.String()
	return nil
}

func (s *traceabilityAuditUnitSteps) exitsSuccessfully() error {
	if s.cmdErr != nil {
		return fmt.Errorf("expected success but got: %w\nOutput: %s", s.cmdErr, s.cmdOutput)
	}
	return nil
}

func (s *traceabilityAuditUnitSteps) exitsWithFailure() error {
	if s.cmdErr == nil {
		return fmt.Errorf("expected failure but succeeded\nOutput: %s", s.cmdOutput)
	}
	return nil
}

func (s *traceabilityAuditUnitSteps) outputZeroFindings() error {
	if !strings.Contains(s.cmdOutput, "PASSED") {
		return fmt.Errorf("expected PASSED in output, got: %s", s.cmdOutput)
	}
	return nil
}

func (s *traceabilityAuditUnitSteps) outputIdentifiesKind(kind string) error {
	if !strings.Contains(s.cmdOutput, kind) {
		return fmt.Errorf("expected output to contain kind %q, got: %s", kind, s.cmdOutput)
	}
	return nil
}

func (s *traceabilityAuditUnitSteps) outputIdentifiesVision() error {
	return s.outputIdentifiesKind(governance.TraceabilityMissingVisionSupported)
}

func (s *traceabilityAuditUnitSteps) outputIdentifiesPrinciples() error {
	return s.outputIdentifiesKind(governance.TraceabilityMissingPrinciplesImplemented)
}

func (s *traceabilityAuditUnitSteps) outputIdentifiesConventions() error {
	return s.outputIdentifiesKind(governance.TraceabilityMissingConventionsImplemented)
}

func (s *traceabilityAuditUnitSteps) outputIdentifiesAgentMissing() error {
	return s.outputIdentifiesKind(governance.TraceabilityMissingAgentReference)
}

// TestUnitGovernanceTraceabilityAudit runs the godog feature suite against the
// repo-governance-traceability-audit.feature file using the mocked
// traceabilityAuditFn entrypoint.
//
// Scenario: A clean repository passes the traceability audit.
// Scenario: A principle missing the Vision Supported heading fails the audit.
// Scenario: A convention missing the Principles Implemented/Respected heading fails the audit.
// Scenario: A development document missing the Conventions Implemented/Respected heading fails the audit.
// Scenario: A workflow with no agent reference fails the audit.
func TestUnitGovernanceTraceabilityAudit(t *testing.T) {
	s := &traceabilityAuditUnitSteps{}
	suite := godog.TestSuite{
		ScenarioInitializer: func(sc *godog.ScenarioContext) {
			sc.Before(s.before)
			sc.After(s.after)
			sc.Step(stepTraceabilityCleanRepo, s.cleanRepo)
			sc.Step(stepTraceabilityPrincipleMissingVision, s.principleMissingVision)
			sc.Step(stepTraceabilityConventionMissingPrinciples, s.conventionMissingPrinciples)
			sc.Step(stepTraceabilityDevelopmentMissingConvs, s.developmentMissingConventions)
			sc.Step(stepTraceabilityWorkflowMissingAgentRef, s.workflowMissingAgentReference)
			sc.Step(stepDeveloperRunsTraceabilityAudit, s.run)
			sc.Step(stepExitsSuccessfully, s.exitsSuccessfully)
			sc.Step(stepExitsWithFailure, s.exitsWithFailure)
			sc.Step(stepTraceabilityOutputZeroFindings, s.outputZeroFindings)
			sc.Step(stepTraceabilityOutputIdentifiesVision, s.outputIdentifiesVision)
			sc.Step(stepTraceabilityOutputIdentifiesPrinciples, s.outputIdentifiesPrinciples)
			sc.Step(stepTraceabilityOutputIdentifiesConventions, s.outputIdentifiesConventions)
			sc.Step(stepTraceabilityOutputIdentifiesAgentMissing, s.outputIdentifiesAgentMissing)
		},
		Options: &godog.Options{
			Format:   "pretty",
			Paths:    []string{specsDirUnitTraceabilityAudit},
			TestingT: t,
			Tags:     "repo-governance-traceability-audit",
		},
	}
	if suite.Run() != 0 {
		t.Fatal("non-zero status returned, failed to run unit feature tests")
	}
}

// TestGovernanceTraceabilityAudit_MissingGitRoot verifies the command fails
// gracefully when not inside a git repository.
func TestGovernanceTraceabilityAudit_MissingGitRoot(t *testing.T) {
	origGetwd := osGetwd
	origStat := osStat
	defer func() {
		osGetwd = origGetwd
		osStat = origStat
	}()

	osGetwd = func() (string, error) { return "/no-git-here", nil }
	osStat = func(_ string) (os.FileInfo, error) { return nil, os.ErrNotExist }

	buf := new(bytes.Buffer)
	governanceTraceabilityAuditCmd.SetOut(buf)
	governanceTraceabilityAuditCmd.SetErr(buf)

	err := governanceTraceabilityAuditCmd.RunE(governanceTraceabilityAuditCmd, []string{})
	if err == nil || !strings.Contains(err.Error(), "git") {
		t.Fatalf("expected git-root error, got: %v", err)
	}
}

// TestGovernanceTraceabilityAudit_RealTree exercises the real filesystem
// audit against a small tmp fixture so coverage reflects the walk logic.
func TestGovernanceTraceabilityAudit_RealTree(t *testing.T) {
	tmp := t.TempDir()

	writeFile := func(path, content string) {
		if err := os.MkdirAll(filepath.Dir(path), 0o755); err != nil {
			t.Fatal(err)
		}
		if err := os.WriteFile(path, []byte(content), 0o644); err != nil {
			t.Fatal(err)
		}
	}
	// Principle missing Vision Supported.
	writeFile(filepath.Join(tmp, "repo-governance", "principles", "general", "bad.md"), "# Bad\n")
	// Convention with required heading — clean.
	writeFile(filepath.Join(tmp, "repo-governance", "conventions", "structure", "good.md"),
		"# Good\n\n## Principles Implemented/Respected\n\n- p\n")
	// README files exempt.
	writeFile(filepath.Join(tmp, "repo-governance", "principles", "README.md"), "# idx\n")

	findings, err := governance.AuditTraceability(tmp)
	if err != nil {
		t.Fatalf("AuditTraceability: %v", err)
	}
	if len(findings) != 1 {
		t.Fatalf("expected 1 finding (the bad principle), got %d: %+v", len(findings), findings)
	}
	if findings[0].Kind != governance.TraceabilityMissingVisionSupported {
		t.Errorf("expected %q, got %q", governance.TraceabilityMissingVisionSupported, findings[0].Kind)
	}
}

// TestGovernanceTraceabilityAudit_OutputFormats checks that all three output
// formats produce non-empty output.
func TestGovernanceTraceabilityAudit_OutputFormats(t *testing.T) {
	origGetwd := osGetwd
	origStat := osStat
	origFn := traceabilityAuditFn
	defer func() {
		osGetwd = origGetwd
		osStat = origStat
		traceabilityAuditFn = origFn
	}()

	osGetwd = func() (string, error) { return "/mock-repo", nil }
	osStat = func(name string) (os.FileInfo, error) {
		if name == "/mock-repo/.git" {
			return &mockFileInfo{name: ".git", isDir: true}, nil
		}
		return nil, os.ErrNotExist
	}
	traceabilityAuditFn = func(_ string) ([]governance.TraceabilityFinding, error) {
		return []governance.TraceabilityFinding{{
			Path:    "/mock-repo/repo-governance/principles/general/bad.md",
			Line:    1,
			Kind:    governance.TraceabilityMissingVisionSupported,
			Message: "missing heading",
		}}, nil
	}

	for _, format := range []string{"json", "markdown", "text"} {
		t.Run(format, func(t *testing.T) {
			buf := new(bytes.Buffer)
			governanceTraceabilityAuditCmd.SetOut(buf)
			governanceTraceabilityAuditCmd.SetErr(buf)
			output = format
			if err := parseOutputFormat(nil, nil); err != nil {
				t.Fatalf("parseOutputFormat: %v", err)
			}
			verbose = false
			quiet = false
			_ = governanceTraceabilityAuditCmd.RunE(governanceTraceabilityAuditCmd, []string{})
			if buf.Len() == 0 {
				t.Errorf("format %s produced no output", format)
			}
		})
	}
	output = "text"
	_ = parseOutputFormat(nil, nil)
}

// TestGovernanceTraceabilityAudit_CleanOutputFormats checks the zero-finding
// branches of every output formatter.
func TestGovernanceTraceabilityAudit_CleanOutputFormats(t *testing.T) {
	origGetwd := osGetwd
	origStat := osStat
	origFn := traceabilityAuditFn
	defer func() {
		osGetwd = origGetwd
		osStat = origStat
		traceabilityAuditFn = origFn
	}()

	osGetwd = func() (string, error) { return "/mock-repo", nil }
	osStat = func(name string) (os.FileInfo, error) {
		if name == "/mock-repo/.git" {
			return &mockFileInfo{name: ".git", isDir: true}, nil
		}
		return nil, os.ErrNotExist
	}
	traceabilityAuditFn = func(_ string) ([]governance.TraceabilityFinding, error) { return nil, nil }

	for _, format := range []string{"json", "markdown", "text"} {
		t.Run(format, func(t *testing.T) {
			buf := new(bytes.Buffer)
			governanceTraceabilityAuditCmd.SetOut(buf)
			governanceTraceabilityAuditCmd.SetErr(buf)
			output = format
			if err := parseOutputFormat(nil, nil); err != nil {
				t.Fatalf("parseOutputFormat: %v", err)
			}
			verbose = false
			quiet = false
			err := governanceTraceabilityAuditCmd.RunE(governanceTraceabilityAuditCmd, []string{})
			if err != nil {
				t.Errorf("expected nil error on zero findings (format %s), got: %v", format, err)
			}
			if buf.Len() == 0 {
				t.Errorf("format %s produced no output on zero findings", format)
			}
		})
	}
	output = "text"
	_ = parseOutputFormat(nil, nil)
}

// TestGovernanceTraceabilityAudit_AuditError verifies that an underlying audit
// error is surfaced as a command-level error.
func TestGovernanceTraceabilityAudit_AuditError(t *testing.T) {
	origGetwd := osGetwd
	origStat := osStat
	origFn := traceabilityAuditFn
	defer func() {
		osGetwd = origGetwd
		osStat = origStat
		traceabilityAuditFn = origFn
	}()

	osGetwd = func() (string, error) { return "/mock-repo", nil }
	osStat = func(name string) (os.FileInfo, error) {
		if name == "/mock-repo/.git" {
			return &mockFileInfo{name: ".git", isDir: true}, nil
		}
		return nil, os.ErrNotExist
	}
	traceabilityAuditFn = func(_ string) ([]governance.TraceabilityFinding, error) {
		return nil, fmt.Errorf("simulated walk failure")
	}

	buf := new(bytes.Buffer)
	governanceTraceabilityAuditCmd.SetOut(buf)
	governanceTraceabilityAuditCmd.SetErr(buf)
	err := governanceTraceabilityAuditCmd.RunE(governanceTraceabilityAuditCmd, []string{})
	if err == nil || !strings.Contains(err.Error(), "simulated walk failure") {
		t.Fatalf("expected wrapped walk error, got: %v", err)
	}
}
