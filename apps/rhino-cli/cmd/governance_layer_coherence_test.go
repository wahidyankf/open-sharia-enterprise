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

// Step constant patterns for repo-governance layer-coherence scenarios.
// Defined locally because no other command shares these step phrasings.
const (
	stepLayerCoherenceCleanRepo               = `^a repository where both governance docs list layers 0 through 5 with identical names$`
	stepLayerCoherenceNumberingGap            = `^a repository where the governance docs list layers 0, 1, and 3 with no layer 2$`
	stepLayerCoherenceNameDisagreement        = `^a repository where the two governance docs assign different names to the same layer number$`
	stepDeveloperRunsLayerCoherence           = `^the developer runs repo-governance layer-coherence$`
	stepLayerCoherenceOutputZeroFindings      = `^the layer-coherence output reports zero findings$`
	stepLayerCoherenceOutputIdentifiesGap     = `^the layer-coherence output identifies the numbering gap$`
	stepLayerCoherenceOutputIdentifiesNameDis = `^the layer-coherence output identifies the layer name disagreement$`
)

var specsDirUnitLayerCoherence = func() string {
	_, f, _, _ := runtime.Caller(0)
	return filepath.Join(filepath.Dir(f), "../../../specs/apps/rhino/behavior/cli/gherkin")
}()

type layerCoherenceUnitSteps struct {
	cmdErr    error
	cmdOutput string
}

func (s *layerCoherenceUnitSteps) before(_ context.Context, _ *godog.Scenario) (context.Context, error) {
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
	layerCoherenceFn = func(_ string) ([]governance.LayerCoherenceFinding, error) { return nil, nil }
	return context.Background(), nil
}

func (s *layerCoherenceUnitSteps) after(_ context.Context, _ *godog.Scenario, _ error) (context.Context, error) {
	layerCoherenceFn = governance.AuditLayerCoherence
	osGetwd = os.Getwd
	osStat = os.Stat
	return context.Background(), nil
}

func (s *layerCoherenceUnitSteps) cleanRepo() error {
	layerCoherenceFn = func(_ string) ([]governance.LayerCoherenceFinding, error) { return nil, nil }
	return nil
}

func (s *layerCoherenceUnitSteps) numberingGap() error {
	layerCoherenceFn = func(_ string) ([]governance.LayerCoherenceFinding, error) {
		return []governance.LayerCoherenceFinding{{
			File:     "/mock-repo/repo-governance/repository-governance-architecture.md+/mock-repo/repo-governance/README.md",
			Severity: "fail",
			Kind:     governance.LayerCoherenceNumberingGap,
			Message:  "layer numbering is not contiguous: Layer 2 is missing between 0 and 3",
		}}, nil
	}
	return nil
}

func (s *layerCoherenceUnitSteps) nameDisagreement() error {
	layerCoherenceFn = func(_ string) ([]governance.LayerCoherenceFinding, error) {
		return []governance.LayerCoherenceFinding{{
			File:     "/mock-repo/repo-governance/repository-governance-architecture.md+/mock-repo/repo-governance/README.md",
			Severity: "fail",
			Kind:     governance.LayerCoherenceCrossFileNameMismatch,
			Message:  `Layer 1 named "Principles" in arch but "Practices" in README`,
		}}, nil
	}
	return nil
}

func (s *layerCoherenceUnitSteps) run() error {
	buf := new(bytes.Buffer)
	governanceLayerCoherenceCmd.SetOut(buf)
	governanceLayerCoherenceCmd.SetErr(buf)
	s.cmdErr = governanceLayerCoherenceCmd.RunE(governanceLayerCoherenceCmd, []string{})
	s.cmdOutput = buf.String()
	return nil
}

func (s *layerCoherenceUnitSteps) exitsSuccessfully() error {
	if s.cmdErr != nil {
		return fmt.Errorf("expected success but got: %w\nOutput: %s", s.cmdErr, s.cmdOutput)
	}
	return nil
}

func (s *layerCoherenceUnitSteps) exitsWithFailure() error {
	if s.cmdErr == nil {
		return fmt.Errorf("expected failure but succeeded\nOutput: %s", s.cmdOutput)
	}
	return nil
}

func (s *layerCoherenceUnitSteps) outputZeroFindings() error {
	if !strings.Contains(s.cmdOutput, "PASSED") {
		return fmt.Errorf("expected PASSED in output, got: %s", s.cmdOutput)
	}
	return nil
}

func (s *layerCoherenceUnitSteps) outputIdentifiesKind(kind string) error {
	if !strings.Contains(s.cmdOutput, kind) {
		return fmt.Errorf("expected output to contain kind %q, got: %s", kind, s.cmdOutput)
	}
	return nil
}

func (s *layerCoherenceUnitSteps) outputIdentifiesGap() error {
	return s.outputIdentifiesKind(governance.LayerCoherenceNumberingGap)
}

func (s *layerCoherenceUnitSteps) outputIdentifiesNameDis() error {
	return s.outputIdentifiesKind(governance.LayerCoherenceCrossFileNameMismatch)
}

// TestUnitGovernanceLayerCoherence runs the godog feature suite against the
// repo-governance-layer-coherence.feature file using the mocked
// layerCoherenceFn entrypoint.
//
// Scenario: Both docs list identical layer numbers and names passes.
// Scenario: Layer numbering has a gap fails.
// Scenario: Two docs disagree on a layer name for the same number fails.
func TestUnitGovernanceLayerCoherence(t *testing.T) {
	s := &layerCoherenceUnitSteps{}
	suite := godog.TestSuite{
		ScenarioInitializer: func(sc *godog.ScenarioContext) {
			sc.Before(s.before)
			sc.After(s.after)
			sc.Step(stepLayerCoherenceCleanRepo, s.cleanRepo)
			sc.Step(stepLayerCoherenceNumberingGap, s.numberingGap)
			sc.Step(stepLayerCoherenceNameDisagreement, s.nameDisagreement)
			sc.Step(stepDeveloperRunsLayerCoherence, s.run)
			sc.Step(stepExitsSuccessfully, s.exitsSuccessfully)
			sc.Step(stepExitsWithFailure, s.exitsWithFailure)
			sc.Step(stepLayerCoherenceOutputZeroFindings, s.outputZeroFindings)
			sc.Step(stepLayerCoherenceOutputIdentifiesGap, s.outputIdentifiesGap)
			sc.Step(stepLayerCoherenceOutputIdentifiesNameDis, s.outputIdentifiesNameDis)
		},
		Options: &godog.Options{
			Format:   "pretty",
			Paths:    []string{specsDirUnitLayerCoherence},
			TestingT: t,
			Tags:     "repo-governance-layer-coherence",
		},
	}
	if suite.Run() != 0 {
		t.Fatal("non-zero status returned, failed to run unit feature tests")
	}
}

// TestGovernanceLayerCoherence_MissingGitRoot verifies the command fails
// gracefully when not inside a git repository.
func TestGovernanceLayerCoherence_MissingGitRoot(t *testing.T) {
	origGetwd := osGetwd
	origStat := osStat
	defer func() {
		osGetwd = origGetwd
		osStat = origStat
	}()

	osGetwd = func() (string, error) { return "/no-git-here", nil }
	osStat = func(_ string) (os.FileInfo, error) { return nil, os.ErrNotExist }

	buf := new(bytes.Buffer)
	governanceLayerCoherenceCmd.SetOut(buf)
	governanceLayerCoherenceCmd.SetErr(buf)

	err := governanceLayerCoherenceCmd.RunE(governanceLayerCoherenceCmd, []string{})
	if err == nil || !strings.Contains(err.Error(), "git") {
		t.Fatalf("expected git-root error, got: %v", err)
	}
}

// TestGovernanceLayerCoherence_AuditError verifies that an underlying audit
// error is surfaced as a command-level error.
func TestGovernanceLayerCoherence_AuditError(t *testing.T) {
	origGetwd := osGetwd
	origStat := osStat
	origFn := layerCoherenceFn
	defer func() {
		osGetwd = origGetwd
		osStat = origStat
		layerCoherenceFn = origFn
	}()

	osGetwd = func() (string, error) { return "/mock-repo", nil }
	osStat = func(name string) (os.FileInfo, error) {
		if name == "/mock-repo/.git" {
			return &mockFileInfo{name: ".git", isDir: true}, nil
		}
		return nil, os.ErrNotExist
	}
	layerCoherenceFn = func(_ string) ([]governance.LayerCoherenceFinding, error) {
		return nil, fmt.Errorf("simulated audit failure")
	}

	buf := new(bytes.Buffer)
	governanceLayerCoherenceCmd.SetOut(buf)
	governanceLayerCoherenceCmd.SetErr(buf)
	err := governanceLayerCoherenceCmd.RunE(governanceLayerCoherenceCmd, []string{})
	if err == nil || !strings.Contains(err.Error(), "simulated audit failure") {
		t.Fatalf("expected wrapped audit error, got: %v", err)
	}
}

// TestGovernanceLayerCoherence_OutputFormats checks that all three output
// formats produce non-empty output when findings are present.
func TestGovernanceLayerCoherence_OutputFormats(t *testing.T) {
	origGetwd := osGetwd
	origStat := osStat
	origFn := layerCoherenceFn
	defer func() {
		osGetwd = origGetwd
		osStat = origStat
		layerCoherenceFn = origFn
	}()

	osGetwd = func() (string, error) { return "/mock-repo", nil }
	osStat = func(name string) (os.FileInfo, error) {
		if name == "/mock-repo/.git" {
			return &mockFileInfo{name: ".git", isDir: true}, nil
		}
		return nil, os.ErrNotExist
	}
	layerCoherenceFn = func(_ string) ([]governance.LayerCoherenceFinding, error) {
		return []governance.LayerCoherenceFinding{{
			File:     "/mock-repo/repo-governance/README.md",
			Severity: "fail",
			Kind:     governance.LayerCoherenceNumberingGap,
			Message:  "Layer 2 missing",
		}}, nil
	}

	for _, format := range []string{"json", "markdown", "text"} {
		t.Run(format, func(t *testing.T) {
			buf := new(bytes.Buffer)
			governanceLayerCoherenceCmd.SetOut(buf)
			governanceLayerCoherenceCmd.SetErr(buf)
			output = format
			if err := parseOutputFormat(nil, nil); err != nil {
				t.Fatalf("parseOutputFormat: %v", err)
			}
			verbose = false
			quiet = false
			_ = governanceLayerCoherenceCmd.RunE(governanceLayerCoherenceCmd, []string{})
			if buf.Len() == 0 {
				t.Errorf("format %s produced no output", format)
			}
			if format == "json" && !strings.Contains(buf.String(), layerCoherenceSchema) {
				t.Errorf("JSON output missing schema, got: %s", buf.String())
			}
			if format == "markdown" && !strings.Contains(buf.String(), "## Layer Coherence Audit") {
				t.Errorf("markdown output missing heading, got: %s", buf.String())
			}
		})
	}
	output = "text"
	_ = parseOutputFormat(nil, nil)
}

// TestGovernanceLayerCoherence_CleanOutputFormats checks zero-finding branches
// of every output formatter (text, JSON, markdown all have an "passed" leg).
func TestGovernanceLayerCoherence_CleanOutputFormats(t *testing.T) {
	origGetwd := osGetwd
	origStat := osStat
	origFn := layerCoherenceFn
	defer func() {
		osGetwd = origGetwd
		osStat = origStat
		layerCoherenceFn = origFn
	}()

	osGetwd = func() (string, error) { return "/mock-repo", nil }
	osStat = func(name string) (os.FileInfo, error) {
		if name == "/mock-repo/.git" {
			return &mockFileInfo{name: ".git", isDir: true}, nil
		}
		return nil, os.ErrNotExist
	}
	layerCoherenceFn = func(_ string) ([]governance.LayerCoherenceFinding, error) { return nil, nil }

	for _, format := range []string{"json", "markdown", "text"} {
		t.Run(format, func(t *testing.T) {
			buf := new(bytes.Buffer)
			governanceLayerCoherenceCmd.SetOut(buf)
			governanceLayerCoherenceCmd.SetErr(buf)
			output = format
			if err := parseOutputFormat(nil, nil); err != nil {
				t.Fatalf("parseOutputFormat: %v", err)
			}
			verbose = false
			quiet = false
			err := governanceLayerCoherenceCmd.RunE(governanceLayerCoherenceCmd, []string{})
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
