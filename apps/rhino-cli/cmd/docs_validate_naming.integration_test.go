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

// Scenario: Tree where every markdown file uses lowercase kebab-case passes
// Scenario: File with uppercase characters fails
// Scenario: README.md is exempt and passes regardless of placement

var specsDirDocsValidateNaming = func() string {
	_, f, _, _ := runtime.Caller(0)
	return filepath.Join(filepath.Dir(f), "../../../specs/apps/rhino/behavior/cli/gherkin")
}()

type docsValidateNamingIntegSteps struct {
	originalWd string
	tmpDir     string
	cmdErr     error
	cmdOutput  string
	scanArgs   []string
	expectVar  string
}

func (s *docsValidateNamingIntegSteps) before(_ context.Context, _ *godog.Scenario) (context.Context, error) {
	s.originalWd, _ = os.Getwd()
	s.tmpDir, _ = os.MkdirTemp("", "docs-validate-naming-*")
	_ = os.MkdirAll(filepath.Join(s.tmpDir, ".git"), 0o755)
	_ = os.MkdirAll(filepath.Join(s.tmpDir, "docs"), 0o755)
	verbose = false
	quiet = false
	output = "text"
	docsValidateNamingExempts = nil
	s.cmdErr = nil
	s.cmdOutput = ""
	s.scanArgs = nil
	s.expectVar = ""
	_ = os.Chdir(s.tmpDir)
	return context.Background(), nil
}

func (s *docsValidateNamingIntegSteps) after(_ context.Context, _ *godog.Scenario, _ error) (context.Context, error) {
	_ = os.Chdir(s.originalWd)
	_ = os.RemoveAll(s.tmpDir)
	docsValidateNamingExempts = nil
	return context.Background(), nil
}

func (s *docsValidateNamingIntegSteps) writeFile(rel, content string) error {
	full := filepath.Join(s.tmpDir, rel)
	if err := os.MkdirAll(filepath.Dir(full), 0o755); err != nil {
		return err
	}
	return os.WriteFile(full, []byte(content), 0o644)
}

func (s *docsValidateNamingIntegSteps) cleanTree() error {
	s.scanArgs = []string{"docs/"}
	if err := s.writeFile("docs/file-naming.md", "# clean\n"); err != nil {
		return err
	}
	if err := s.writeFile("docs/nested/three-level-testing-standard.md", "# nested\n"); err != nil {
		return err
	}
	return s.writeFile("docs/README.md", "# index\n")
}

func (s *docsValidateNamingIntegSteps) uppercaseViolation() error {
	s.scanArgs = []string{"docs/"}
	s.expectVar = "MyDoc.md"
	if err := s.writeFile("docs/ok-doc.md", "# ok\n"); err != nil {
		return err
	}
	return s.writeFile("docs/MyDoc.md", "# bad\n")
}

func (s *docsValidateNamingIntegSteps) nestedReadmeOnly() error {
	s.scanArgs = []string{"docs/"}
	// Only a deeply-nested README.md — must remain exempt.
	if err := s.writeFile("docs/nested/deep/README.md", "# deeply nested index\n"); err != nil {
		return err
	}
	return s.writeFile("docs/README.md", "# top index\n")
}

func (s *docsValidateNamingIntegSteps) run() error {
	buf := new(bytes.Buffer)
	docsValidateNamingCmd.SetOut(buf)
	docsValidateNamingCmd.SetErr(buf)
	s.cmdErr = docsValidateNamingCmd.RunE(docsValidateNamingCmd, s.scanArgs)
	s.cmdOutput = buf.String()
	return nil
}

func (s *docsValidateNamingIntegSteps) exitsSuccessfully() error {
	if s.cmdErr != nil {
		return fmt.Errorf("expected success, got: %v\nOutput: %s", s.cmdErr, s.cmdOutput)
	}
	return nil
}

func (s *docsValidateNamingIntegSteps) exitsWithFailure() error {
	if s.cmdErr == nil {
		return fmt.Errorf("expected failure, got success\nOutput: %s", s.cmdOutput)
	}
	return nil
}

func (s *docsValidateNamingIntegSteps) zeroFindings() error {
	if !strings.Contains(s.cmdOutput, "PASSED") {
		return fmt.Errorf("expected PASSED in output, got: %s", s.cmdOutput)
	}
	return nil
}

func (s *docsValidateNamingIntegSteps) outputIdentifiesViolation() error {
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

// InitializeDocsValidateNamingScenario binds the step handlers for the
// integration suite.
func InitializeDocsValidateNamingScenario(sc *godog.ScenarioContext) {
	s := &docsValidateNamingIntegSteps{}
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
}

func TestIntegrationDocsValidateNaming(t *testing.T) {
	suite := godog.TestSuite{
		ScenarioInitializer: InitializeDocsValidateNamingScenario,
		Options: &godog.Options{
			Format:   "pretty",
			Paths:    []string{specsDirDocsValidateNaming},
			Tags:     "docs-validate-naming",
			TestingT: t,
		},
	}
	if suite.Run() != 0 {
		t.Fatal("non-zero status returned, failed to run integration feature tests")
	}
}
