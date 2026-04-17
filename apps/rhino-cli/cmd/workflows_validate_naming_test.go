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
	"github.com/wahidyankf/ose-public/apps/rhino-cli/internal/naming"
)

var specsDirUnitValidateWorkflowsNaming = func() string {
	_, f, _, _ := runtime.Caller(0)
	return filepath.Join(filepath.Dir(f), "../../../specs/apps/rhino/cli/gherkin")
}()

type validateWorkflowsNamingUnitSteps struct {
	cmdErr    error
	cmdOutput string
}

func (s *validateWorkflowsNamingUnitSteps) before(_ context.Context, _ *godog.Scenario) (context.Context, error) {
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
	workflowsValidateNamingFn = func(_ string) ([]naming.Violation, error) { return nil, nil }
	return context.Background(), nil
}

func (s *validateWorkflowsNamingUnitSteps) after(_ context.Context, _ *godog.Scenario, _ error) (context.Context, error) {
	workflowsValidateNamingFn = workflowsValidateNaming
	osGetwd = os.Getwd
	osStat = os.Stat
	return context.Background(), nil
}

func (s *validateWorkflowsNamingUnitSteps) treeAllConform() error {
	workflowsValidateNamingFn = func(_ string) ([]naming.Violation, error) { return nil, nil }
	return nil
}

func (s *validateWorkflowsNamingUnitSteps) treeUnknownSuffix() error {
	workflowsValidateNamingFn = func(_ string) ([]naming.Violation, error) {
		return []naming.Violation{{
			Path:    "/mock-repo/governance/workflows/specs/specs-validation.md",
			Kind:    "type-suffix",
			Message: `filename "specs-validation" does not end with any allowed suffix (quality-gate, execution, setup)`,
		}}, nil
	}
	return nil
}

func (s *validateWorkflowsNamingUnitSteps) treeFrontmatterMismatch() error {
	workflowsValidateNamingFn = func(_ string) ([]naming.Violation, error) {
		return []naming.Violation{{
			Path:    "/mock-repo/governance/workflows/plan/plan-execution.md",
			Kind:    "frontmatter-mismatch",
			Message: `frontmatter name "wrong" does not match filename "plan-execution"`,
		}}, nil
	}
	return nil
}

func (s *validateWorkflowsNamingUnitSteps) treeMetaExempt() error {
	// Files under meta/ are filtered before validation, so no violations.
	workflowsValidateNamingFn = func(_ string) ([]naming.Violation, error) { return nil, nil }
	return nil
}

func (s *validateWorkflowsNamingUnitSteps) run() error {
	buf := new(bytes.Buffer)
	workflowsValidateNamingCmd.SetOut(buf)
	workflowsValidateNamingCmd.SetErr(buf)
	s.cmdErr = workflowsValidateNamingCmd.RunE(workflowsValidateNamingCmd, []string{})
	s.cmdOutput = buf.String()
	return nil
}

func (s *validateWorkflowsNamingUnitSteps) exitsSuccessfully() error {
	if s.cmdErr != nil {
		return fmt.Errorf("expected success but got: %v\nOutput: %s", s.cmdErr, s.cmdOutput)
	}
	return nil
}

func (s *validateWorkflowsNamingUnitSteps) exitsWithFailure() error {
	if s.cmdErr == nil {
		return fmt.Errorf("expected failure but succeeded\nOutput: %s", s.cmdOutput)
	}
	return nil
}

func (s *validateWorkflowsNamingUnitSteps) zeroViolations() error {
	if !strings.Contains(s.cmdOutput, "VALIDATION PASSED") {
		return fmt.Errorf("expected VALIDATION PASSED, got: %s", s.cmdOutput)
	}
	return nil
}

func (s *validateWorkflowsNamingUnitSteps) identifiesUnknownSuffix() error {
	lc := strings.ToLower(s.cmdOutput)
	if !strings.Contains(lc, "type-suffix") || !strings.Contains(lc, "specs-validation") {
		return fmt.Errorf("expected type-suffix + offending filename, got: %s", s.cmdOutput)
	}
	return nil
}

func (s *validateWorkflowsNamingUnitSteps) identifiesFrontmatterMismatch() error {
	lc := strings.ToLower(s.cmdOutput)
	if !strings.Contains(lc, "frontmatter-mismatch") {
		return fmt.Errorf("expected frontmatter-mismatch, got: %s", s.cmdOutput)
	}
	return nil
}

func TestUnitValidateWorkflowsNaming(t *testing.T) {
	s := &validateWorkflowsNamingUnitSteps{}
	suite := godog.TestSuite{
		ScenarioInitializer: func(sc *godog.ScenarioContext) {
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
		},
		Options: &godog.Options{
			Format:   "pretty",
			Paths:    []string{specsDirUnitValidateWorkflowsNaming},
			TestingT: t,
			Tags:     "workflows-validate-naming",
		},
	}
	if suite.Run() != 0 {
		t.Fatal("non-zero status returned, failed to run unit feature tests")
	}
}

func TestValidateWorkflowsNaming_MissingGitRoot(t *testing.T) {
	origGetwd := osGetwd
	origStat := osStat
	defer func() {
		osGetwd = origGetwd
		osStat = origStat
	}()

	osGetwd = func() (string, error) { return "/no-git-here", nil }
	osStat = func(_ string) (os.FileInfo, error) { return nil, os.ErrNotExist }

	buf := new(bytes.Buffer)
	workflowsValidateNamingCmd.SetOut(buf)
	workflowsValidateNamingCmd.SetErr(buf)

	err := workflowsValidateNamingCmd.RunE(workflowsValidateNamingCmd, []string{})
	if err == nil || !strings.Contains(err.Error(), "git") {
		t.Fatalf("expected git-root error, got: %v", err)
	}
}

// TestWorkflowsValidateNaming_RealTree exercises the real filesystem
// walker against a small tmp fixture so coverage reflects the walk logic.
func TestWorkflowsValidateNaming_RealTree(t *testing.T) {
	tmp := t.TempDir()
	root := filepath.Join(tmp, "governance", "workflows")
	planDir := filepath.Join(root, "plan")
	metaDir := filepath.Join(root, "meta")
	if err := os.MkdirAll(planDir, 0755); err != nil {
		t.Fatal(err)
	}
	if err := os.MkdirAll(metaDir, 0755); err != nil {
		t.Fatal(err)
	}

	writeFile := func(path, content string) {
		if err := os.WriteFile(path, []byte(content), 0644); err != nil {
			t.Fatal(err)
		}
	}
	writeFile(filepath.Join(root, "README.md"), "# idx\n")
	writeFile(filepath.Join(planDir, "README.md"), "# idx\n")
	writeFile(filepath.Join(planDir, "plan-execution.md"),
		"---\nname: plan-execution\n---\nbody\n")
	writeFile(filepath.Join(planDir, "specs-validation.md"),
		"---\nname: specs-validation\n---\nbody\n") // bad suffix
	writeFile(filepath.Join(planDir, "plan-quality-gate.md"),
		"---\nname: wrong-name\n---\nbody\n") // frontmatter mismatch
	// Meta file with non-conforming name must be IGNORED.
	writeFile(filepath.Join(metaDir, "execution-modes.md"),
		"---\nname: execution-modes\n---\nbody\n")
	writeFile(filepath.Join(metaDir, "some-reference.md"),
		"---\nname: some-reference\n---\nbody\n")

	got, err := workflowsValidateNaming(tmp)
	if err != nil {
		t.Fatalf("workflowsValidateNaming: %v", err)
	}
	kinds := map[string]int{}
	for _, v := range got {
		kinds[v.Kind]++
		if strings.Contains(v.Path, "/meta/") {
			t.Errorf("meta/ file should be exempt but was flagged: %+v", v)
		}
	}
	if kinds["type-suffix"] != 1 {
		t.Errorf("expected 1 type-suffix, got kinds=%v", kinds)
	}
	if kinds["frontmatter-mismatch"] != 1 {
		t.Errorf("expected 1 frontmatter-mismatch, got kinds=%v", kinds)
	}
}

// TestWorkflowsValidateNaming_MissingRoot verifies a missing workflows tree
// surfaces as empty-violations, not an error.
func TestWorkflowsValidateNaming_MissingRoot(t *testing.T) {
	tmp := t.TempDir()
	got, err := workflowsValidateNaming(tmp)
	if err != nil {
		t.Fatalf("workflowsValidateNaming: %v", err)
	}
	if len(got) != 0 {
		t.Fatalf("expected zero violations for empty tree, got %+v", got)
	}
}

func TestWorkflowsNaming_OutputFormats(t *testing.T) {
	origGetwd := osGetwd
	origStat := osStat
	origFn := workflowsValidateNamingFn
	defer func() {
		osGetwd = origGetwd
		osStat = origStat
		workflowsValidateNamingFn = origFn
	}()

	osGetwd = func() (string, error) { return "/mock-repo", nil }
	osStat = func(name string) (os.FileInfo, error) {
		if name == "/mock-repo/.git" {
			return &mockFileInfo{name: ".git", isDir: true}, nil
		}
		return nil, os.ErrNotExist
	}
	workflowsValidateNamingFn = func(_ string) ([]naming.Violation, error) {
		return []naming.Violation{{Path: "/x/y.md", Kind: "type-suffix", Message: "m"}}, nil
	}

	for _, format := range []string{"json", "markdown", "text"} {
		t.Run(format, func(t *testing.T) {
			buf := new(bytes.Buffer)
			workflowsValidateNamingCmd.SetOut(buf)
			workflowsValidateNamingCmd.SetErr(buf)
			output = format
			verbose = format == "text"
			quiet = false
			_ = workflowsValidateNamingCmd.RunE(workflowsValidateNamingCmd, []string{})
			if buf.Len() == 0 {
				t.Errorf("format %s produced no output", format)
			}
		})
	}
	output = "text"
	verbose = false
}
