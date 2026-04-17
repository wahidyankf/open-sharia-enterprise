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
)

var specsDirWorkflowsValidateNaming = func() string {
	_, f, _, _ := runtime.Caller(0)
	return filepath.Join(filepath.Dir(f), "../../../specs/apps/rhino/cli/gherkin")
}()

type validateWorkflowsNamingIntegSteps struct {
	originalWd string
	tmpDir     string
	cmdErr     error
	cmdOutput  string
}

func (s *validateWorkflowsNamingIntegSteps) before(_ context.Context, _ *godog.Scenario) (context.Context, error) {
	s.originalWd, _ = os.Getwd()
	s.tmpDir, _ = os.MkdirTemp("", "validate-naming-workflows-*")
	_ = os.MkdirAll(filepath.Join(s.tmpDir, ".git"), 0755)
	_ = os.MkdirAll(filepath.Join(s.tmpDir, "governance", "workflows", "plan"), 0755)
	_ = os.MkdirAll(filepath.Join(s.tmpDir, "governance", "workflows", "meta"), 0755)
	verbose = false
	quiet = false
	output = "text"
	_ = os.Chdir(s.tmpDir)
	return context.Background(), nil
}

func (s *validateWorkflowsNamingIntegSteps) after(_ context.Context, _ *godog.Scenario, _ error) (context.Context, error) {
	_ = os.Chdir(s.originalWd)
	_ = os.RemoveAll(s.tmpDir)
	return context.Background(), nil
}

func (s *validateWorkflowsNamingIntegSteps) writeWorkflow(dir, filename, frontmatterName string) error {
	content := fmt.Sprintf("---\nname: %s\ngoal: test\n---\nbody\n", frontmatterName)
	path := filepath.Join(s.tmpDir, "governance", "workflows", dir, filename+".md")
	return os.WriteFile(path, []byte(content), 0644)
}

func (s *validateWorkflowsNamingIntegSteps) treeAllConform() error {
	return s.writeWorkflow("plan", "plan-execution", "plan-execution")
}

func (s *validateWorkflowsNamingIntegSteps) treeUnknownSuffix() error {
	// Conforming sibling + one offender.
	if err := s.writeWorkflow("plan", "plan-execution", "plan-execution"); err != nil {
		return err
	}
	return s.writeWorkflow("plan", "specs-validation", "specs-validation")
}

func (s *validateWorkflowsNamingIntegSteps) treeFrontmatterMismatch() error {
	return s.writeWorkflow("plan", "plan-execution", "wrong-name")
}

func (s *validateWorkflowsNamingIntegSteps) treeMetaExempt() error {
	// Conforming workflow + a meta/ reference doc whose name would otherwise violate.
	if err := s.writeWorkflow("plan", "plan-execution", "plan-execution"); err != nil {
		return err
	}
	return s.writeWorkflow("meta", "execution-modes", "execution-modes")
}

func (s *validateWorkflowsNamingIntegSteps) run() error {
	buf := new(bytes.Buffer)
	workflowsValidateNamingCmd.SetOut(buf)
	workflowsValidateNamingCmd.SetErr(buf)
	s.cmdErr = workflowsValidateNamingCmd.RunE(workflowsValidateNamingCmd, []string{})
	s.cmdOutput = buf.String()
	return nil
}

func (s *validateWorkflowsNamingIntegSteps) exitsSuccessfully() error {
	if s.cmdErr != nil {
		return fmt.Errorf("expected success, got: %v\nOutput: %s", s.cmdErr, s.cmdOutput)
	}
	return nil
}

func (s *validateWorkflowsNamingIntegSteps) exitsWithFailure() error {
	if s.cmdErr == nil {
		return fmt.Errorf("expected failure, output: %s", s.cmdOutput)
	}
	return nil
}

func (s *validateWorkflowsNamingIntegSteps) zeroViolations() error {
	if !strings.Contains(s.cmdOutput, "VALIDATION PASSED") {
		return fmt.Errorf("expected VALIDATION PASSED, got: %s", s.cmdOutput)
	}
	return nil
}

func (s *validateWorkflowsNamingIntegSteps) identifiesUnknownSuffix() error {
	lc := strings.ToLower(s.cmdOutput)
	if !strings.Contains(lc, "type-suffix") || !strings.Contains(lc, "specs-validation") {
		return fmt.Errorf("expected type-suffix violation naming specs-validation, got: %s", s.cmdOutput)
	}
	return nil
}

func (s *validateWorkflowsNamingIntegSteps) identifiesFrontmatterMismatch() error {
	lc := strings.ToLower(s.cmdOutput)
	if !strings.Contains(lc, "frontmatter-mismatch") {
		return fmt.Errorf("expected frontmatter-mismatch, got: %s", s.cmdOutput)
	}
	return nil
}

func InitializeValidateWorkflowsNamingScenario(sc *godog.ScenarioContext) {
	s := &validateWorkflowsNamingIntegSteps{}
	sc.Before(s.before)
	sc.After(s.after)

	sc.Step(stepWorkflowsTreeAllConform, s.treeAllConform)
	sc.Step(stepWorkflowsTreeUnknownSuffix, s.treeUnknownSuffix)
	sc.Step(stepWorkflowsTreeFrontmatterMismatch, s.treeFrontmatterMismatch)
	sc.Step(stepWorkflowsTreeMetaExempt, s.treeMetaExempt)
	sc.Step(stepDeveloperRunsWorkflowsValidateNaming, s.run)
	sc.Step(stepExitsSuccessfully, s.exitsSuccessfully)
	sc.Step(stepExitsWithFailure, s.exitsWithFailure)
	sc.Step(stepOutputZeroNamingViolations, s.zeroViolations)
	sc.Step(stepOutputIdentifiesWorkflowUnknownSuffix, s.identifiesUnknownSuffix)
	sc.Step(stepOutputIdentifiesFrontmatterMismatch, s.identifiesFrontmatterMismatch)
}

func TestIntegrationValidateWorkflowsNaming(t *testing.T) {
	suite := godog.TestSuite{
		ScenarioInitializer: InitializeValidateWorkflowsNamingScenario,
		Options: &godog.Options{
			Format:   "pretty",
			Paths:    []string{specsDirWorkflowsValidateNaming},
			Tags:     "workflows-validate-naming",
			TestingT: t,
		},
	}
	if suite.Run() != 0 {
		t.Fatal("non-zero status returned, failed to run integration feature tests")
	}
}
