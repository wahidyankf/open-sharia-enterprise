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

var specsDirUnitSpecsValidateCounts = func() string {
	_, f, _, _ := runtime.Caller(0)
	return filepath.Join(filepath.Dir(f), "../../../specs/apps/rhino/behavior/cli/gherkin/specs")
}()

type specsValidateCountsUnitSteps struct {
	cmdErr    error
	cmdOutput string
}

func (s *specsValidateCountsUnitSteps) before(_ context.Context, _ *godog.Scenario) (context.Context, error) {
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

	specsValidateCountsFn = func(_, _ string) []SpecFinding {
		return nil
	}
	return context.Background(), nil
}

func (s *specsValidateCountsUnitSteps) after(_ context.Context, _ *godog.Scenario, _ error) (context.Context, error) {
	specsValidateCountsFn = validateSpecCounts
	osGetwd = os.Getwd
	osStat = os.Stat
	return context.Background(), nil
}

// Scenario: folder with spec files in all subfolders passes validation
func (s *specsValidateCountsUnitSteps) specFolderWithSpecFiles() error {
	specsValidateCountsFn = func(_, _ string) []SpecFinding {
		return nil
	}
	return nil
}

// Scenario: empty subfolder reports a finding
func (s *specsValidateCountsUnitSteps) specFolderProductOnlyReadme() error {
	specsValidateCountsFn = func(_, _ string) []SpecFinding {
		return []SpecFinding{{
			Category:    "count",
			Criticality: "MEDIUM",
			File:        "specs/apps/testapp/product",
			Evidence:    "empty subfolder: product contains no spec files (only README.md or nothing)",
			Expected:    "add at least one non-README .md spec file to specs/apps/testapp/product/",
		}}
	}
	return nil
}

// Scenario: missing subfolder reports a finding
func (s *specsValidateCountsUnitSteps) specFolderBehaviorMissing() error {
	specsValidateCountsFn = func(_, _ string) []SpecFinding {
		return []SpecFinding{{
			Category:    "count",
			Criticality: "MEDIUM",
			File:        "specs/apps/testapp/behavior",
			Evidence:    "missing required folder: behavior",
			Expected:    "create specs/apps/testapp/behavior/README.md plus at least one spec .md file",
		}}
	}
	return nil
}

// Scenario: folder path that does not exist
func (s *specsValidateCountsUnitSteps) noDirectoryNosuchapp() error {
	specsValidateCountsFn = func(_, _ string) []SpecFinding {
		return []SpecFinding{{
			Category:    "count",
			Criticality: "HIGH",
			File:        "specs/apps/nosuchapp",
			Evidence:    "spec folder does not exist: specs/apps/nosuchapp",
			Expected:    "create the spec folder with required subfolders",
		}}
	}
	return nil
}

func (s *specsValidateCountsUnitSteps) runValidateCountsTestapp() error {
	buf := new(bytes.Buffer)
	specsValidateCountsCmd.SetOut(buf)
	specsValidateCountsCmd.SetErr(buf)
	s.cmdErr = specsValidateCountsCmd.RunE(specsValidateCountsCmd, []string{"specs/apps/testapp"})
	s.cmdOutput = buf.String()
	return nil
}

func (s *specsValidateCountsUnitSteps) runValidateCountsNosuchapp() error {
	buf := new(bytes.Buffer)
	specsValidateCountsCmd.SetOut(buf)
	specsValidateCountsCmd.SetErr(buf)
	s.cmdErr = specsValidateCountsCmd.RunE(specsValidateCountsCmd, []string{"specs/apps/nosuchapp"})
	s.cmdOutput = buf.String()
	return nil
}

func (s *specsValidateCountsUnitSteps) exitsSuccessfully() error {
	if s.cmdErr != nil {
		return fmt.Errorf("expected success but got: %v\nOutput: %s", s.cmdErr, s.cmdOutput)
	}
	return nil
}

func (s *specsValidateCountsUnitSteps) exitsWithFailure() error {
	if s.cmdErr == nil {
		return fmt.Errorf("expected failure but succeeded\nOutput: %s", s.cmdOutput)
	}
	return nil
}

func (s *specsValidateCountsUnitSteps) outputContainsZeroFinding() error {
	if !strings.Contains(s.cmdOutput, "0 finding") {
		return fmt.Errorf("expected '0 finding' in output but got: %s", s.cmdOutput)
	}
	return nil
}

func (s *specsValidateCountsUnitSteps) outputContainsEmptySubfolder() error {
	if !strings.Contains(s.cmdOutput, "empty subfolder") {
		return fmt.Errorf("expected 'empty subfolder' in output but got: %s", s.cmdOutput)
	}
	return nil
}

func (s *specsValidateCountsUnitSteps) outputContainsMissingBehavior() error {
	if !strings.Contains(s.cmdOutput, "missing required folder: behavior") {
		return fmt.Errorf("expected 'missing required folder: behavior' in output but got: %s", s.cmdOutput)
	}
	return nil
}

func (s *specsValidateCountsUnitSteps) outputContainsDoesNotExist() error {
	if !strings.Contains(s.cmdOutput, "does not exist") {
		return fmt.Errorf("expected 'does not exist' in output but got: %s", s.cmdOutput)
	}
	return nil
}

func TestUnitSpecsValidateCounts(t *testing.T) {
	s := &specsValidateCountsUnitSteps{}
	suite := godog.TestSuite{
		ScenarioInitializer: func(sc *godog.ScenarioContext) {
			sc.Before(s.before)
			sc.After(s.after)
			sc.Step(stepSpecsCountsFolderWithSpecFiles, s.specFolderWithSpecFiles)
			sc.Step(stepSpecsCountsFolderProductOnlyReadme, s.specFolderProductOnlyReadme)
			sc.Step(stepSpecsCountsFolderBehaviorMissing, s.specFolderBehaviorMissing)
			sc.Step(stepSpecsCountsFolderNosuchapp, s.noDirectoryNosuchapp)
			sc.Step(stepSpecsRunValidateCountsTestapp, s.runValidateCountsTestapp)
			sc.Step(stepSpecsRunValidateCountsNosuchapp, s.runValidateCountsNosuchapp)
			sc.Step(stepExitsSuccessfully, s.exitsSuccessfully)
			sc.Step(stepExitsWithFailure, s.exitsWithFailure)
			sc.Step(stepSpecsCountsOutputContainsZeroFinding, s.outputContainsZeroFinding)
			sc.Step(stepSpecsCountsOutputContainsEmptySubfolder, s.outputContainsEmptySubfolder)
			sc.Step(stepSpecsCountsOutputContainsMissingBehavior, s.outputContainsMissingBehavior)
			sc.Step(stepSpecsCountsOutputContainsDoesNotExist, s.outputContainsDoesNotExist)
		},
		Options: &godog.Options{
			Format:   "pretty",
			Paths:    []string{specsDirUnitSpecsValidateCounts},
			TestingT: t,
			Tags:     "specs-validate-counts",
		},
	}
	if suite.Run() != 0 {
		t.Fatal("non-zero status returned, failed to run unit feature tests")
	}
}

func TestCountNonReadmeMdFiles(t *testing.T) {
	// Uses real temp directories since the implementation uses filepath.Walk recursively.
	write := func(dir, name string) {
		if err := os.WriteFile(filepath.Join(dir, name), []byte("x"), 0644); err != nil {
			t.Fatal(err)
		}
	}
	mkdir := func(dir, name string) string {
		p := filepath.Join(dir, name)
		if err := os.Mkdir(p, 0755); err != nil {
			t.Fatal(err)
		}
		return p
	}

	t.Run("only README.md", func(t *testing.T) {
		dir := t.TempDir()
		write(dir, "README.md")
		if got := countNonReadmeMdFiles(dir); got != 0 {
			t.Errorf("got %d, want 0", got)
		}
	})

	t.Run("one spec file and README", func(t *testing.T) {
		dir := t.TempDir()
		write(dir, "README.md")
		write(dir, "overview.md")
		if got := countNonReadmeMdFiles(dir); got != 1 {
			t.Errorf("got %d, want 1", got)
		}
	})

	t.Run("counts feature files recursively", func(t *testing.T) {
		dir := t.TempDir()
		write(dir, "README.md")
		sub := mkdir(dir, "gherkin")
		write(sub, "health.feature")
		write(sub, "README.md")
		if got := countNonReadmeMdFiles(dir); got != 1 {
			t.Errorf("got %d, want 1", got)
		}
	})

	t.Run("non-md non-feature files are skipped", func(t *testing.T) {
		dir := t.TempDir()
		write(dir, "spec.txt")
		write(dir, "spec.md")
		if got := countNonReadmeMdFiles(dir); got != 1 {
			t.Errorf("got %d, want 1", got)
		}
	})

	t.Run("nonexistent dir returns 0", func(t *testing.T) {
		if got := countNonReadmeMdFiles("/nonexistent/path/xyz"); got != 0 {
			t.Errorf("got %d, want 0", got)
		}
	})
}

func TestSpecsValidateCountsCmd_MissingGitRoot(t *testing.T) {
	origGetwd := osGetwd
	origStat := osStat
	defer func() {
		osGetwd = origGetwd
		osStat = origStat
	}()

	osGetwd = func() (string, error) { return "/no-git-here", nil }
	osStat = func(_ string) (os.FileInfo, error) { return nil, os.ErrNotExist }

	buf := new(bytes.Buffer)
	specsValidateCountsCmd.SetOut(buf)
	specsValidateCountsCmd.SetErr(buf)

	err := specsValidateCountsCmd.RunE(specsValidateCountsCmd, []string{"specs/apps/testapp"})
	if err == nil || !strings.Contains(err.Error(), "git") {
		t.Fatalf("expected git-root error, got: %v", err)
	}
}

func TestValidateSpecCounts_Logic(t *testing.T) {
	tests := []struct {
		name         string
		statFunc     func(string) (os.FileInfo, error)
		readDirFunc  func(string) ([]os.DirEntry, error)
		folder       string
		wantFindings int
		wantEvidence []string
	}{
		{
			name:   "folder does not exist",
			folder: "specs/apps/nosuchapp",
			statFunc: func(name string) (os.FileInfo, error) {
				return nil, os.ErrNotExist
			},
			readDirFunc: func(_ string) ([]os.DirEntry, error) {
				return nil, os.ErrNotExist
			},
			wantFindings: 1,
			wantEvidence: []string{"does not exist"},
		},
		{
			name:   "all subfolders have spec files",
			folder: "specs/apps/testapp",
			statFunc: func(name string) (os.FileInfo, error) {
				return &mockFileInfo{name: filepath.Base(name), isDir: true}, nil
			},
			readDirFunc: func(_ string) ([]os.DirEntry, error) {
				return []os.DirEntry{
					&mockDirEntry{name: "README.md", isDir: false},
					&mockDirEntry{name: "overview.md", isDir: false},
				}, nil
			},
			wantFindings: 0,
		},
		{
			name:   "product subfolder is empty (only README)",
			folder: "specs/apps/testapp",
			statFunc: func(name string) (os.FileInfo, error) {
				return &mockFileInfo{name: filepath.Base(name), isDir: true}, nil
			},
			readDirFunc: func(dir string) ([]os.DirEntry, error) {
				if strings.HasSuffix(dir, "product") {
					return []os.DirEntry{
						&mockDirEntry{name: "README.md", isDir: false},
					}, nil
				}
				return []os.DirEntry{
					&mockDirEntry{name: "README.md", isDir: false},
					&mockDirEntry{name: "spec.md", isDir: false},
				}, nil
			},
			wantFindings: 1,
			wantEvidence: []string{"empty subfolder"},
		},
		{
			name:   "absolute folder path is accepted",
			folder: "/absolute/path/to/specs/apps/testapp",
			statFunc: func(name string) (os.FileInfo, error) {
				return &mockFileInfo{name: filepath.Base(name), isDir: true}, nil
			},
			readDirFunc: func(_ string) ([]os.DirEntry, error) {
				return []os.DirEntry{
					&mockDirEntry{name: "README.md", isDir: false},
					&mockDirEntry{name: "spec.md", isDir: false},
				}, nil
			},
			wantFindings: 0,
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			origStat := osStat
			origReadDir := readDirFn
			origCountFn := specCountNonReadmeMdFilesFn

			osStat = tt.statFunc
			readDirFn = tt.readDirFunc
			specCountNonReadmeMdFilesFn = func(dir string) int {
				entries, err := tt.readDirFunc(dir)
				if err != nil {
					return 0
				}
				count := 0
				for _, e := range entries {
					if e.IsDir() {
						continue
					}
					name := e.Name()
					if strings.HasSuffix(strings.ToLower(name), ".md") && !strings.EqualFold(name, "README.md") {
						count++
					}
				}
				return count
			}

			defer func() {
				osStat = origStat
				readDirFn = origReadDir
				specCountNonReadmeMdFilesFn = origCountFn
			}()

			findings := validateSpecCounts("/mock-repo", tt.folder)
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
