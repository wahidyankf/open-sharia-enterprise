//go:build !integration

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

var specsDirUnitSpecsValidateTree = func() string {
	_, f, _, _ := runtime.Caller(0)
	return filepath.Join(filepath.Dir(f), "../../../specs/apps/rhino/behavior/cli/gherkin/specs")
}()

type specsValidateTreeUnitSteps struct {
	cmdErr    error
	cmdOutput string
}

func (s *specsValidateTreeUnitSteps) before(_ context.Context, _ *godog.Scenario) (context.Context, error) {
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

	specsValidateTreeFn = func(_, _ string) []SpecFinding {
		return nil
	}
	return context.Background(), nil
}

func (s *specsValidateTreeUnitSteps) after(_ context.Context, _ *godog.Scenario, _ error) (context.Context, error) {
	specsValidateTreeFn = validateSpecTree
	osGetwd = os.Getwd
	osStat = os.Stat
	return context.Background(), nil
}

// Scenario: app with complete spec tree passes validation
func (s *specsValidateTreeUnitSteps) specTreeWithAllFiveFolders() error {
	specsValidateTreeFn = func(_, _ string) []SpecFinding {
		return nil
	}
	return nil
}

// Scenario: app missing a required folder reports a finding
func (s *specsValidateTreeUnitSteps) specTreeMissingBehavior() error {
	specsValidateTreeFn = func(_, _ string) []SpecFinding {
		return []SpecFinding{{
			Category:    "tree-shape",
			Criticality: "HIGH",
			File:        "specs/apps/testapp",
			Evidence:    "missing required folder: behavior",
			Expected:    "create specs/apps/testapp/behavior/ with README.md",
		}}
	}
	return nil
}

// Scenario: app with folder missing README.md reports a finding
func (s *specsValidateTreeUnitSteps) specTreeProductFolderNoReadme() error {
	specsValidateTreeFn = func(_, _ string) []SpecFinding {
		return []SpecFinding{{
			Category:    "tree-shape",
			Criticality: "HIGH",
			File:        "specs/apps/testapp/product",
			Evidence:    "missing README.md in required folder: product",
			Expected:    "create specs/apps/testapp/product/README.md",
		}}
	}
	return nil
}

// Scenario: app with no spec tree at all
func (s *specsValidateTreeUnitSteps) noSpecTreeForUnknownapp() error {
	specsValidateTreeFn = func(_, _ string) []SpecFinding {
		var findings []SpecFinding
		for _, folder := range requiredSpecFolders {
			findings = append(findings, SpecFinding{
				Category:    "tree-shape",
				Criticality: "HIGH",
				File:        "specs/apps/unknownapp",
				Evidence:    fmt.Sprintf("missing required folder: %s", folder),
				Expected:    fmt.Sprintf("create specs/apps/unknownapp/%s/ with README.md", folder),
			})
		}
		return findings
	}
	return nil
}

func (s *specsValidateTreeUnitSteps) runValidateTreeTestapp() error {
	buf := new(bytes.Buffer)
	specsValidateTreeCmd.SetOut(buf)
	specsValidateTreeCmd.SetErr(buf)
	s.cmdErr = specsValidateTreeCmd.RunE(specsValidateTreeCmd, []string{"testapp"})
	s.cmdOutput = buf.String()
	return nil
}

func (s *specsValidateTreeUnitSteps) runValidateTreeUnknownapp() error {
	buf := new(bytes.Buffer)
	specsValidateTreeCmd.SetOut(buf)
	specsValidateTreeCmd.SetErr(buf)
	s.cmdErr = specsValidateTreeCmd.RunE(specsValidateTreeCmd, []string{"unknownapp"})
	s.cmdOutput = buf.String()
	return nil
}

func (s *specsValidateTreeUnitSteps) exitsSuccessfully() error {
	if s.cmdErr != nil {
		return fmt.Errorf("expected success but got: %v\nOutput: %s", s.cmdErr, s.cmdOutput)
	}
	return nil
}

func (s *specsValidateTreeUnitSteps) exitsWithFailure() error {
	if s.cmdErr == nil {
		return fmt.Errorf("expected failure but succeeded\nOutput: %s", s.cmdOutput)
	}
	return nil
}

func (s *specsValidateTreeUnitSteps) outputContainsZeroFinding() error {
	if !strings.Contains(s.cmdOutput, "0 finding") {
		return fmt.Errorf("expected '0 finding' in output but got: %s", s.cmdOutput)
	}
	return nil
}

func (s *specsValidateTreeUnitSteps) outputContainsMissingBehavior() error {
	if !strings.Contains(s.cmdOutput, "missing required folder: behavior") {
		return fmt.Errorf("expected 'missing required folder: behavior' in output but got: %s", s.cmdOutput)
	}
	return nil
}

func (s *specsValidateTreeUnitSteps) outputContainsMissingReadme() error {
	if !strings.Contains(s.cmdOutput, "missing README.md") {
		return fmt.Errorf("expected 'missing README.md' in output but got: %s", s.cmdOutput)
	}
	return nil
}

func (s *specsValidateTreeUnitSteps) outputContainsMissingProduct() error {
	if !strings.Contains(s.cmdOutput, "missing required folder: product") {
		return fmt.Errorf("expected 'missing required folder: product' in output but got: %s", s.cmdOutput)
	}
	return nil
}

func TestUnitSpecsValidateTree(t *testing.T) {
	s := &specsValidateTreeUnitSteps{}
	suite := godog.TestSuite{
		ScenarioInitializer: func(sc *godog.ScenarioContext) {
			sc.Before(s.before)
			sc.After(s.after)
			sc.Step(stepSpecsTreeAllFiveFolders, s.specTreeWithAllFiveFolders)
			sc.Step(stepSpecsTreeMissingBehavior, s.specTreeMissingBehavior)
			sc.Step(stepSpecsTreeProductFolderNoReadme, s.specTreeProductFolderNoReadme)
			sc.Step(stepSpecsTreeNoSpecTreeForUnknownapp, s.noSpecTreeForUnknownapp)
			sc.Step(stepSpecsRunValidateTreeTestapp, s.runValidateTreeTestapp)
			sc.Step(stepSpecsRunValidateTreeUnknownapp, s.runValidateTreeUnknownapp)
			sc.Step(stepExitsSuccessfully, s.exitsSuccessfully)
			sc.Step(stepExitsWithFailure, s.exitsWithFailure)
			sc.Step(stepSpecsOutputContainsZeroFinding, s.outputContainsZeroFinding)
			sc.Step(stepSpecsOutputContainsMissingBehavior, s.outputContainsMissingBehavior)
			sc.Step(stepSpecsOutputContainsMissingReadme, s.outputContainsMissingReadme)
			sc.Step(stepSpecsOutputContainsMissingProduct, s.outputContainsMissingProduct)
		},
		Options: &godog.Options{
			Format:   "pretty",
			Paths:    []string{specsDirUnitSpecsValidateTree},
			TestingT: t,
			Tags:     "specs-validate-tree",
		},
	}
	if suite.Run() != 0 {
		t.Fatal("non-zero status returned, failed to run unit feature tests")
	}
}

func TestSpecsValidateTreeCmd_MissingGitRoot(t *testing.T) {
	origGetwd := osGetwd
	origStat := osStat
	defer func() {
		osGetwd = origGetwd
		osStat = origStat
	}()

	osGetwd = func() (string, error) { return "/no-git-here", nil }
	osStat = func(_ string) (os.FileInfo, error) { return nil, os.ErrNotExist }

	buf := new(bytes.Buffer)
	specsValidateTreeCmd.SetOut(buf)
	specsValidateTreeCmd.SetErr(buf)

	err := specsValidateTreeCmd.RunE(specsValidateTreeCmd, []string{"testapp"})
	if err == nil || !strings.Contains(err.Error(), "git") {
		t.Fatalf("expected git-root error, got: %v", err)
	}
}

func TestValidateSpecTree_Logic(t *testing.T) {
	tests := []struct {
		name         string
		statFunc     func(string) (os.FileInfo, error)
		wantFindings int
		wantEvidence []string
	}{
		{
			name: "all folders present with README",
			statFunc: func(name string) (os.FileInfo, error) {
				return &mockFileInfo{name: filepath.Base(name), isDir: true}, nil
			},
			wantFindings: 0,
		},
		{
			name: "behavior folder missing",
			statFunc: func(name string) (os.FileInfo, error) {
				if strings.HasSuffix(name, "behavior") || strings.HasSuffix(name, "behavior/README.md") {
					return nil, os.ErrNotExist
				}
				return &mockFileInfo{name: filepath.Base(name), isDir: true}, nil
			},
			wantFindings: 1,
			wantEvidence: []string{"missing required folder: behavior"},
		},
		{
			name: "product folder missing README",
			statFunc: func(name string) (os.FileInfo, error) {
				if strings.HasSuffix(name, "product/README.md") {
					return nil, os.ErrNotExist
				}
				if strings.HasSuffix(name, ".git") {
					return &mockFileInfo{name: ".git", isDir: true}, nil
				}
				return &mockFileInfo{name: filepath.Base(name), isDir: true}, nil
			},
			wantFindings: 1,
			wantEvidence: []string{"missing README.md in required folder: product"},
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			origStat := osStat
			osStat = tt.statFunc
			defer func() { osStat = origStat }()

			findings := validateSpecTree("/mock-repo", "testapp")
			if len(findings) != tt.wantFindings {
				t.Errorf("got %d findings, want %d; findings: %v", len(findings), tt.wantFindings, findings)
			}
			for _, ev := range tt.wantEvidence {
				found := false
				for _, f := range findings {
					if strings.Contains(f.Evidence, ev) {
						found = true
						break
					}
				}
				if !found {
					t.Errorf("expected evidence %q not found in findings: %v", ev, findings)
				}
			}
		})
	}
}
