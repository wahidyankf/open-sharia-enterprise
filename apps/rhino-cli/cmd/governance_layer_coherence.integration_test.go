//go:build integration

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

// Scenario: Both docs list identical layer numbers and names passes
// Scenario: Layer numbering has a gap fails
// Scenario: Two docs disagree on a layer name for the same number fails

var specsDirIntegrationLayerCoherence = func() string {
	_, f, _, _ := runtime.Caller(0)
	return filepath.Join(filepath.Dir(f), "../../../specs/apps/rhino/behavior/cli/gherkin")
}()

type layerCoherenceIntegSteps struct {
	originalWd string
	tmpDir     string
	cmdErr     error
	cmdOutput  string
}

// cleanIntegArch is a minimal architecture-doc fixture (H2-heading form).
const cleanIntegArch = `# Repository Governance Architecture

## Layer 0: Vision (WHY WE EXIST)
## Layer 1: Principles (WHY - Values)
## Layer 2: Conventions (WHAT - Rules)
## Layer 3: Development (HOW - Practices)
## Layer 4: AI Agents (WHO - Executors)
## Layer 5: Workflows (WHEN - Orchestrate)
`

// cleanIntegReadme is a minimal README fixture (bold-span form).
const cleanIntegReadme = `# repo-governance

- **Layer 0: Vision** - WHY we exist
- **Layer 1: Principles** - WHY we value approaches
- **Layer 2: Conventions** - WHAT rules
- **Layer 3: Development** - HOW we develop
- **Layer 4: AI Agents** - WHO enforces rules
- **Layer 5: Workflows** - WHEN orchestrate
`

func (s *layerCoherenceIntegSteps) before(_ context.Context, _ *godog.Scenario) (context.Context, error) {
	s.originalWd, _ = os.Getwd()
	s.tmpDir, _ = os.MkdirTemp("", "layer-coherence-*")
	_ = os.MkdirAll(filepath.Join(s.tmpDir, ".git"), 0o755)
	verbose = false
	quiet = false
	output = "text"
	_ = os.Chdir(s.tmpDir)
	return context.Background(), nil
}

func (s *layerCoherenceIntegSteps) after(_ context.Context, _ *godog.Scenario, _ error) (context.Context, error) {
	_ = os.Chdir(s.originalWd)
	_ = os.RemoveAll(s.tmpDir)
	return context.Background(), nil
}

func (s *layerCoherenceIntegSteps) writeFile(rel, content string) error {
	full := filepath.Join(s.tmpDir, rel)
	if err := os.MkdirAll(filepath.Dir(full), 0o755); err != nil {
		return fmt.Errorf("mkdir %s: %w", filepath.Dir(full), err)
	}
	return os.WriteFile(full, []byte(content), 0o644)
}

func (s *layerCoherenceIntegSteps) cleanRepo() error {
	if err := s.writeFile("repo-governance/repository-governance-architecture.md", cleanIntegArch); err != nil {
		return err
	}
	return s.writeFile("repo-governance/README.md", cleanIntegReadme)
}

func (s *layerCoherenceIntegSteps) numberingGap() error {
	arch := `# Arch

## Layer 0: Vision (a)
## Layer 1: Principles (b)
## Layer 3: Development (c)
`
	readme := `# README

- **Layer 0: Vision** - x
- **Layer 1: Principles** - x
- **Layer 3: Development** - x
`
	if err := s.writeFile("repo-governance/repository-governance-architecture.md", arch); err != nil {
		return err
	}
	return s.writeFile("repo-governance/README.md", readme)
}

func (s *layerCoherenceIntegSteps) nameDisagreement() error {
	arch := `# Arch

## Layer 0: Vision (a)
## Layer 1: Principles (b)
`
	readme := `# README

- **Layer 0: Vision** - x
- **Layer 1: Practices** - x
`
	if err := s.writeFile("repo-governance/repository-governance-architecture.md", arch); err != nil {
		return err
	}
	return s.writeFile("repo-governance/README.md", readme)
}

func (s *layerCoherenceIntegSteps) run() error {
	buf := new(bytes.Buffer)
	governanceLayerCoherenceCmd.SetOut(buf)
	governanceLayerCoherenceCmd.SetErr(buf)
	s.cmdErr = governanceLayerCoherenceCmd.RunE(governanceLayerCoherenceCmd, []string{})
	s.cmdOutput = buf.String()
	return nil
}

func (s *layerCoherenceIntegSteps) exitsSuccessfully() error {
	if s.cmdErr != nil {
		return fmt.Errorf("expected success, got: %v\nOutput: %s", s.cmdErr, s.cmdOutput)
	}
	return nil
}

func (s *layerCoherenceIntegSteps) exitsWithFailure() error {
	if s.cmdErr == nil {
		return fmt.Errorf("expected failure, output: %s", s.cmdOutput)
	}
	return nil
}

func (s *layerCoherenceIntegSteps) zeroFindings() error {
	if !strings.Contains(s.cmdOutput, "PASSED") {
		return fmt.Errorf("expected PASSED, got: %s", s.cmdOutput)
	}
	return nil
}

func (s *layerCoherenceIntegSteps) outputContainsKind(kind string) error {
	if !strings.Contains(s.cmdOutput, kind) {
		return fmt.Errorf("expected output to contain kind %q, got: %s", kind, s.cmdOutput)
	}
	return nil
}

func (s *layerCoherenceIntegSteps) identifiesGap() error {
	return s.outputContainsKind(governance.LayerCoherenceNumberingGap)
}

func (s *layerCoherenceIntegSteps) identifiesNameDis() error {
	return s.outputContainsKind(governance.LayerCoherenceCrossFileNameMismatch)
}

// InitializeGovernanceLayerCoherenceScenario wires the integration step
// definitions into the godog scenario context.
func InitializeGovernanceLayerCoherenceScenario(sc *godog.ScenarioContext) {
	s := &layerCoherenceIntegSteps{}
	sc.Before(s.before)
	sc.After(s.after)

	sc.Step(stepLayerCoherenceCleanRepo, s.cleanRepo)
	sc.Step(stepLayerCoherenceNumberingGap, s.numberingGap)
	sc.Step(stepLayerCoherenceNameDisagreement, s.nameDisagreement)
	sc.Step(stepDeveloperRunsLayerCoherence, s.run)
	sc.Step(stepExitsSuccessfully, s.exitsSuccessfully)
	sc.Step(stepExitsWithFailure, s.exitsWithFailure)
	sc.Step(stepLayerCoherenceOutputZeroFindings, s.zeroFindings)
	sc.Step(stepLayerCoherenceOutputIdentifiesGap, s.identifiesGap)
	sc.Step(stepLayerCoherenceOutputIdentifiesNameDis, s.identifiesNameDis)
}

// TestIntegrationGovernanceLayerCoherence runs the godog feature suite
// against /tmp fixtures so the real filesystem read and audit logic are
// exercised end-to-end.
func TestIntegrationGovernanceLayerCoherence(t *testing.T) {
	suite := godog.TestSuite{
		ScenarioInitializer: InitializeGovernanceLayerCoherenceScenario,
		Options: &godog.Options{
			Format:   "pretty",
			Paths:    []string{specsDirIntegrationLayerCoherence},
			Tags:     "repo-governance-layer-coherence",
			TestingT: t,
		},
	}
	if suite.Run() != 0 {
		t.Fatal("non-zero status returned, failed to run integration feature tests")
	}
}
