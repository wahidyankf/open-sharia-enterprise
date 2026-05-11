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

// Scenario: A clean repository passes the traceability audit
// Scenario: A principle missing the Vision Supported heading fails the audit
// Scenario: A convention missing the Principles Implemented/Respected heading fails the audit
// Scenario: A development document missing the Conventions Implemented/Respected heading fails the audit
// Scenario: A workflow with no agent reference fails the audit

var specsDirTraceabilityAuditIntegration = func() string {
	_, f, _, _ := runtime.Caller(0)
	return filepath.Join(filepath.Dir(f), "../../../specs/apps/rhino/behavior/cli/gherkin")
}()

type traceabilityAuditIntegSteps struct {
	originalWd string
	tmpDir     string
	cmdErr     error
	cmdOutput  string
}

func (s *traceabilityAuditIntegSteps) before(_ context.Context, _ *godog.Scenario) (context.Context, error) {
	s.originalWd, _ = os.Getwd()
	s.tmpDir, _ = os.MkdirTemp("", "traceability-audit-*")
	_ = os.MkdirAll(filepath.Join(s.tmpDir, ".git"), 0o755)
	verbose = false
	quiet = false
	output = "text"
	_ = os.Chdir(s.tmpDir)
	return context.Background(), nil
}

func (s *traceabilityAuditIntegSteps) after(_ context.Context, _ *godog.Scenario, _ error) (context.Context, error) {
	_ = os.Chdir(s.originalWd)
	_ = os.RemoveAll(s.tmpDir)
	return context.Background(), nil
}

func (s *traceabilityAuditIntegSteps) writeFile(rel, content string) error {
	full := filepath.Join(s.tmpDir, rel)
	if err := os.MkdirAll(filepath.Dir(full), 0o755); err != nil {
		return fmt.Errorf("mkdir %s: %w", filepath.Dir(full), err)
	}
	return os.WriteFile(full, []byte(content), 0o644)
}

func (s *traceabilityAuditIntegSteps) cleanRepo() error {
	// Every layer has one compliant non-README file.
	if err := s.writeFile("repo-governance/principles/general/sample.md",
		"# P\n\n## Vision Supported\n\nlink\n"); err != nil {
		return err
	}
	if err := s.writeFile("repo-governance/conventions/structure/sample.md",
		"# C\n\n## Principles Implemented/Respected\n\n- p\n"); err != nil {
		return err
	}
	if err := s.writeFile("repo-governance/development/workflow/sample.md",
		"# D\n\n## Principles Implemented/Respected\n\n- p\n\n## Conventions Implemented/Respected\n\n- c\n"); err != nil {
		return err
	}
	return s.writeFile("repo-governance/workflows/plan/sample-execution.md",
		"# W\n\nuses [.claude/agents/sample-maker.md](../../.claude/agents/sample-maker.md)\n")
}

func (s *traceabilityAuditIntegSteps) principleMissingVision() error {
	return s.writeFile("repo-governance/principles/general/missing-vision.md",
		"# Bad Principle\n\nNo Vision Supported heading.\n")
}

func (s *traceabilityAuditIntegSteps) conventionMissingPrinciples() error {
	return s.writeFile("repo-governance/conventions/structure/missing-principles.md",
		"# Bad Convention\n\nNo Principles Implemented section.\n")
}

func (s *traceabilityAuditIntegSteps) developmentMissingConventions() error {
	// Provide the principles heading so only the conventions heading is missing.
	return s.writeFile("repo-governance/development/workflow/missing-conv.md",
		"# D\n\n## Principles Implemented/Respected\n\n- p\n\nNo conventions section.\n")
}

func (s *traceabilityAuditIntegSteps) workflowMissingAgentReference() error {
	return s.writeFile("repo-governance/workflows/plan/no-agent-execution.md",
		"# Workflow\n\nNo agent reference here.\n")
}

func (s *traceabilityAuditIntegSteps) run() error {
	buf := new(bytes.Buffer)
	governanceTraceabilityAuditCmd.SetOut(buf)
	governanceTraceabilityAuditCmd.SetErr(buf)
	s.cmdErr = governanceTraceabilityAuditCmd.RunE(governanceTraceabilityAuditCmd, []string{})
	s.cmdOutput = buf.String()
	return nil
}

func (s *traceabilityAuditIntegSteps) exitsSuccessfully() error {
	if s.cmdErr != nil {
		return fmt.Errorf("expected success, got: %v\nOutput: %s", s.cmdErr, s.cmdOutput)
	}
	return nil
}

func (s *traceabilityAuditIntegSteps) exitsWithFailure() error {
	if s.cmdErr == nil {
		return fmt.Errorf("expected failure, output: %s", s.cmdOutput)
	}
	return nil
}

func (s *traceabilityAuditIntegSteps) zeroFindings() error {
	if !strings.Contains(s.cmdOutput, "PASSED") {
		return fmt.Errorf("expected PASSED, got: %s", s.cmdOutput)
	}
	return nil
}

func (s *traceabilityAuditIntegSteps) outputContainsKind(kind string) error {
	if !strings.Contains(s.cmdOutput, kind) {
		return fmt.Errorf("expected output to contain kind %q, got: %s", kind, s.cmdOutput)
	}
	return nil
}

func (s *traceabilityAuditIntegSteps) identifiesVision() error {
	return s.outputContainsKind(governance.TraceabilityMissingVisionSupported)
}

func (s *traceabilityAuditIntegSteps) identifiesPrinciples() error {
	return s.outputContainsKind(governance.TraceabilityMissingPrinciplesImplemented)
}

func (s *traceabilityAuditIntegSteps) identifiesConventions() error {
	return s.outputContainsKind(governance.TraceabilityMissingConventionsImplemented)
}

func (s *traceabilityAuditIntegSteps) identifiesAgentMissing() error {
	return s.outputContainsKind(governance.TraceabilityMissingAgentReference)
}

// InitializeGovernanceTraceabilityAuditScenario wires the integration step
// definitions into the godog scenario context.
func InitializeGovernanceTraceabilityAuditScenario(sc *godog.ScenarioContext) {
	s := &traceabilityAuditIntegSteps{}
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
	sc.Step(stepTraceabilityOutputZeroFindings, s.zeroFindings)
	sc.Step(stepTraceabilityOutputIdentifiesVision, s.identifiesVision)
	sc.Step(stepTraceabilityOutputIdentifiesPrinciples, s.identifiesPrinciples)
	sc.Step(stepTraceabilityOutputIdentifiesConventions, s.identifiesConventions)
	sc.Step(stepTraceabilityOutputIdentifiesAgentMissing, s.identifiesAgentMissing)
}

// TestIntegrationGovernanceTraceabilityAudit runs the godog feature suite
// against /tmp fixtures so the real filesystem walk and audit logic are
// exercised end-to-end.
func TestIntegrationGovernanceTraceabilityAudit(t *testing.T) {
	suite := godog.TestSuite{
		ScenarioInitializer: InitializeGovernanceTraceabilityAuditScenario,
		Options: &godog.Options{
			Format:   "pretty",
			Paths:    []string{specsDirTraceabilityAuditIntegration},
			Tags:     "repo-governance-traceability-audit",
			TestingT: t,
		},
	}
	if suite.Run() != 0 {
		t.Fatal("non-zero status returned, failed to run integration feature tests")
	}
}
