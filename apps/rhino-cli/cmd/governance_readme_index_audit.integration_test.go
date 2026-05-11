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

// Scenario: Directory where README.md links cover every sibling .md passes
// Scenario: Orphan file: directory has a .md file the README.md does not link to (fail)
// Scenario: Ghost reference: README.md links to a .md file that does not exist (fail)
// Scenario: Nested subdirectory README.md is also audited

var specsDirGovernanceReadmeIndexAudit = func() string {
	_, f, _, _ := runtime.Caller(0)
	return filepath.Join(filepath.Dir(f), "../../../specs/apps/rhino/behavior/cli/gherkin")
}()

type governanceReadmeIndexAuditIntegSteps struct {
	originalWd string
	tmpDir     string
	cmdErr     error
	cmdOutput  string
	scanArgs   []string
	expectVar  string
}

func (s *governanceReadmeIndexAuditIntegSteps) before(_ context.Context, _ *godog.Scenario) (context.Context, error) {
	s.originalWd, _ = os.Getwd()
	s.tmpDir, _ = os.MkdirTemp("", "readme-index-audit-*")
	_ = os.MkdirAll(filepath.Join(s.tmpDir, ".git"), 0o755)
	_ = os.MkdirAll(filepath.Join(s.tmpDir, "repo-governance"), 0o755)
	verbose = false
	quiet = false
	output = "text"
	readmeIndexAuditExcludes = nil
	s.cmdErr = nil
	s.cmdOutput = ""
	s.scanArgs = []string{"repo-governance/"}
	s.expectVar = ""
	_ = os.Chdir(s.tmpDir)
	return context.Background(), nil
}

func (s *governanceReadmeIndexAuditIntegSteps) after(_ context.Context, _ *godog.Scenario, _ error) (context.Context, error) {
	_ = os.Chdir(s.originalWd)
	_ = os.RemoveAll(s.tmpDir)
	readmeIndexAuditExcludes = nil
	return context.Background(), nil
}

func (s *governanceReadmeIndexAuditIntegSteps) writeFile(rel, content string) error {
	full := filepath.Join(s.tmpDir, rel)
	if err := os.MkdirAll(filepath.Dir(full), 0o755); err != nil {
		return err
	}
	return os.WriteFile(full, []byte(content), 0o644)
}

func (s *governanceReadmeIndexAuditIntegSteps) cleanDirectory() error {
	if err := s.writeFile("repo-governance/README.md", "- [Alpha](alpha.md)\n- [Beta](beta.md)\n"); err != nil {
		return err
	}
	if err := s.writeFile("repo-governance/alpha.md", "# Alpha\n"); err != nil {
		return err
	}
	return s.writeFile("repo-governance/beta.md", "# Beta\n")
}

func (s *governanceReadmeIndexAuditIntegSteps) orphanFile() error {
	s.expectVar = "orphan"
	if err := s.writeFile("repo-governance/README.md", "- [Alpha](alpha.md)\n"); err != nil {
		return err
	}
	if err := s.writeFile("repo-governance/alpha.md", "# Alpha\n"); err != nil {
		return err
	}
	// beta.md exists on disk but is not linked from README.
	return s.writeFile("repo-governance/beta.md", "# Beta\n")
}

func (s *governanceReadmeIndexAuditIntegSteps) ghostReference() error {
	s.expectVar = "ghost"
	if err := s.writeFile("repo-governance/README.md", "- [Alpha](alpha.md)\n- [Phantom](missing.md)\n"); err != nil {
		return err
	}
	return s.writeFile("repo-governance/alpha.md", "# Alpha\n")
}

func (s *governanceReadmeIndexAuditIntegSteps) nestedOrphan() error {
	s.expectVar = "two.md"
	// Root README is complete.
	if err := s.writeFile("repo-governance/README.md", "- [Alpha](alpha.md)\n- [Structure](structure/README.md)\n"); err != nil {
		return err
	}
	if err := s.writeFile("repo-governance/alpha.md", "# Alpha\n"); err != nil {
		return err
	}
	// Nested README omits two.md.
	if err := s.writeFile("repo-governance/structure/README.md", "- [One](one.md)\n"); err != nil {
		return err
	}
	if err := s.writeFile("repo-governance/structure/one.md", "# One\n"); err != nil {
		return err
	}
	return s.writeFile("repo-governance/structure/two.md", "# Two\n")
}

func (s *governanceReadmeIndexAuditIntegSteps) run() error {
	buf := new(bytes.Buffer)
	governanceReadmeIndexAuditCmd.SetOut(buf)
	governanceReadmeIndexAuditCmd.SetErr(buf)
	s.cmdErr = governanceReadmeIndexAuditCmd.RunE(governanceReadmeIndexAuditCmd, s.scanArgs)
	s.cmdOutput = buf.String()
	return nil
}

func (s *governanceReadmeIndexAuditIntegSteps) exitsSuccessfully() error {
	if s.cmdErr != nil {
		return fmt.Errorf("expected success, got: %v\nOutput: %s", s.cmdErr, s.cmdOutput)
	}
	return nil
}

func (s *governanceReadmeIndexAuditIntegSteps) exitsWithFailure() error {
	if s.cmdErr == nil {
		return fmt.Errorf("expected failure, got success\nOutput: %s", s.cmdOutput)
	}
	return nil
}

func (s *governanceReadmeIndexAuditIntegSteps) zeroFindings() error {
	if !strings.Contains(s.cmdOutput, "PASSED") {
		return fmt.Errorf("expected PASSED, got: %s", s.cmdOutput)
	}
	return nil
}

func (s *governanceReadmeIndexAuditIntegSteps) outputIdentifies() error {
	if s.expectVar == "" {
		return fmt.Errorf("test setup error: expectVar not set")
	}
	if !strings.Contains(s.cmdOutput, s.expectVar) {
		return fmt.Errorf("expected output to mention %q, got: %s", s.expectVar, s.cmdOutput)
	}
	return nil
}

// InitializeGovernanceReadmeIndexAuditScenario binds the step handlers for
// the integration suite.
func InitializeGovernanceReadmeIndexAuditScenario(sc *godog.ScenarioContext) {
	s := &governanceReadmeIndexAuditIntegSteps{}
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
}

func TestIntegrationGovernanceReadmeIndexAudit(t *testing.T) {
	suite := godog.TestSuite{
		ScenarioInitializer: InitializeGovernanceReadmeIndexAuditScenario,
		Options: &godog.Options{
			Format:   "pretty",
			Paths:    []string{specsDirGovernanceReadmeIndexAudit},
			Tags:     "repo-governance-readme-index-audit",
			TestingT: t,
		},
	}
	if suite.Run() != 0 {
		t.Fatal("non-zero status returned, failed to run integration feature tests")
	}
}
