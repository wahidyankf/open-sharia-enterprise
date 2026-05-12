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

// Scenario: Tree where every .md has exactly one H1 and no skipped levels passes
// Scenario: File with two H1 headings fails
// Scenario: File with H2 followed directly by H4 (skipping H3) fails
// Scenario: Single-line file with no headings is ignored (passes)

var specsDirDocsValidateHeadingHierarchy = func() string {
	_, f, _, _ := runtime.Caller(0)
	return filepath.Join(filepath.Dir(f), "../../../specs/apps/rhino/behavior/cli/gherkin")
}()

type docsValidateHeadingHierarchyIntegSteps struct {
	originalWd string
	tmpDir     string
	cmdErr     error
	cmdOutput  string
	scanArgs   []string
	expectFile string
	expectKind string
}

func (s *docsValidateHeadingHierarchyIntegSteps) before(_ context.Context, _ *godog.Scenario) (context.Context, error) {
	s.originalWd, _ = os.Getwd()
	s.tmpDir, _ = os.MkdirTemp("", "docs-validate-heading-hierarchy-*")
	_ = os.MkdirAll(filepath.Join(s.tmpDir, ".git"), 0o755)
	_ = os.MkdirAll(filepath.Join(s.tmpDir, "docs"), 0o755)
	verbose = false
	quiet = false
	output = "text"
	s.cmdErr = nil
	s.cmdOutput = ""
	s.scanArgs = nil
	s.expectFile = ""
	s.expectKind = ""
	_ = os.Chdir(s.tmpDir)
	return context.Background(), nil
}

func (s *docsValidateHeadingHierarchyIntegSteps) after(_ context.Context, _ *godog.Scenario, _ error) (context.Context, error) {
	_ = os.Chdir(s.originalWd)
	_ = os.RemoveAll(s.tmpDir)
	return context.Background(), nil
}

func (s *docsValidateHeadingHierarchyIntegSteps) writeFile(rel, content string) error {
	full := filepath.Join(s.tmpDir, rel)
	if err := os.MkdirAll(filepath.Dir(full), 0o755); err != nil {
		return err
	}
	return os.WriteFile(full, []byte(content), 0o644)
}

func (s *docsValidateHeadingHierarchyIntegSteps) cleanTree() error {
	s.scanArgs = []string{"docs/"}
	if err := s.writeFile("docs/clean.md", "# Title\n\n## Section\n\n### Subsection\n"); err != nil {
		return err
	}
	if err := s.writeFile("docs/nested/another.md", "# Another\n\n## B\n"); err != nil {
		return err
	}
	return s.writeFile("docs/README.md", "# Index\n")
}

func (s *docsValidateHeadingHierarchyIntegSteps) twoH1Violation() error {
	s.scanArgs = []string{"docs/"}
	s.expectFile = "docs/two-h1.md"
	s.expectKind = "duplicate-h1"
	if err := s.writeFile("docs/ok.md", "# OK\n"); err != nil {
		return err
	}
	return s.writeFile("docs/two-h1.md", "# First\n\nbody\n\n# Second\n")
}

func (s *docsValidateHeadingHierarchyIntegSteps) skippedLevelViolation() error {
	s.scanArgs = []string{"docs/"}
	s.expectFile = "docs/skipped.md"
	s.expectKind = "skipped-level"
	return s.writeFile("docs/skipped.md", "# Title\n\n## Section\n\n#### Skipped H3\n")
}

func (s *docsValidateHeadingHierarchyIntegSteps) singleLineNoHeadings() error {
	s.scanArgs = []string{"docs/"}
	return s.writeFile("docs/no-headings.md", "just prose, no headings here")
}

func (s *docsValidateHeadingHierarchyIntegSteps) run() error {
	buf := new(bytes.Buffer)
	docsValidateHeadingHierarchyCmd.SetOut(buf)
	docsValidateHeadingHierarchyCmd.SetErr(buf)
	s.cmdErr = docsValidateHeadingHierarchyCmd.RunE(docsValidateHeadingHierarchyCmd, s.scanArgs)
	s.cmdOutput = buf.String()
	return nil
}

func (s *docsValidateHeadingHierarchyIntegSteps) exitsSuccessfully() error {
	if s.cmdErr != nil {
		return fmt.Errorf("expected success, got: %v\nOutput: %s", s.cmdErr, s.cmdOutput)
	}
	return nil
}

func (s *docsValidateHeadingHierarchyIntegSteps) exitsWithFailure() error {
	if s.cmdErr == nil {
		return fmt.Errorf("expected failure, got success\nOutput: %s", s.cmdOutput)
	}
	return nil
}

func (s *docsValidateHeadingHierarchyIntegSteps) zeroFindings() error {
	if !strings.Contains(s.cmdOutput, "PASSED") {
		return fmt.Errorf("expected PASSED in output, got: %s", s.cmdOutput)
	}
	return nil
}

func (s *docsValidateHeadingHierarchyIntegSteps) outputIdentifiesViolation() error {
	if s.expectFile == "" || s.expectKind == "" {
		return fmt.Errorf("test setup error: expectFile/expectKind not set")
	}
	if !strings.Contains(s.cmdOutput, s.expectFile) {
		return fmt.Errorf("expected output to contain %q, got: %s", s.expectFile, s.cmdOutput)
	}
	if !strings.Contains(s.cmdOutput, s.expectKind) {
		return fmt.Errorf("expected output to mention kind %q, got: %s", s.expectKind, s.cmdOutput)
	}
	return nil
}

// TestIntegration_DocsValidateHeadingHierarchy_NFence directly tests that
// N-fence outer blocks containing shorter inner fences are handled correctly.
func TestIntegration_DocsValidateHeadingHierarchy_NFence(t *testing.T) {
	cases := []struct {
		name    string
		content string
	}{
		{
			name:    "4-backtick outer with 3-backtick inner",
			content: "# Title\n\n## Section\n\n```` markdown\n```go\n# not a heading\n```\n````\n\n### Subsection\n",
		},
		{
			name:    "5-backtick outer fence",
			content: "# Title\n\n````` markdown\n```go\n# not a heading\n```\n`````\n\n## Section\n",
		},
		{
			name:    "4-tilde outer with 3-backtick inner",
			content: "# Title\n\n~~~~ text\n```\n# not a heading\n```\n~~~~\n\n## Section\n",
		},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			dir := t.TempDir()
			full := filepath.Join(dir, "docs", "test.md")
			if err := os.MkdirAll(filepath.Dir(full), 0o755); err != nil {
				t.Fatalf("mkdir: %v", err)
			}
			if err := os.WriteFile(full, []byte(tc.content), 0o644); err != nil {
				t.Fatalf("write: %v", err)
			}
			origWd, _ := os.Getwd()
			if err := os.MkdirAll(filepath.Join(dir, ".git"), 0o755); err != nil {
				t.Fatalf("mkdir .git: %v", err)
			}
			if err := os.Chdir(dir); err != nil {
				t.Fatalf("chdir: %v", err)
			}
			defer func() { _ = os.Chdir(origWd) }()

			var buf bytes.Buffer
			docsValidateHeadingHierarchyCmd.SetOut(&buf)
			docsValidateHeadingHierarchyCmd.SetErr(&buf)
			err := docsValidateHeadingHierarchyCmd.RunE(docsValidateHeadingHierarchyCmd, []string{"docs/"})
			if err != nil {
				t.Errorf("expected success, got: %v\nOutput: %s", err, buf.String())
			}
			if !strings.Contains(buf.String(), "PASSED") {
				t.Errorf("expected PASSED in output, got: %s", buf.String())
			}
		})
	}
}

// InitializeDocsValidateHeadingHierarchyScenario binds the step handlers
// for the integration suite.
func InitializeDocsValidateHeadingHierarchyScenario(sc *godog.ScenarioContext) {
	s := &docsValidateHeadingHierarchyIntegSteps{}
	sc.Before(s.before)
	sc.After(s.after)

	sc.Step(stepDocsTreeAllHeadingsValid, s.cleanTree)
	sc.Step(stepDocsTreeTwoH1, s.twoH1Violation)
	sc.Step(stepDocsTreeH2ThenH4, s.skippedLevelViolation)
	sc.Step(stepDocsTreeSingleLineNoHeadings, s.singleLineNoHeadings)
	sc.Step(stepDeveloperRunsDocsValidateHeadings, s.run)
	sc.Step(stepExitsSuccessfully, s.exitsSuccessfully)
	sc.Step(stepExitsWithFailure, s.exitsWithFailure)
	sc.Step(stepOutputZeroDocsHeadingFindings, s.zeroFindings)
	sc.Step(stepOutputIdentifiesDuplicateH1Violation, s.outputIdentifiesViolation)
	sc.Step(stepOutputIdentifiesSkippedHeadingViolation, s.outputIdentifiesViolation)
}

func TestIntegrationDocsValidateHeadingHierarchy(t *testing.T) {
	suite := godog.TestSuite{
		ScenarioInitializer: InitializeDocsValidateHeadingHierarchyScenario,
		Options: &godog.Options{
			Format:   "pretty",
			Paths:    []string{specsDirDocsValidateHeadingHierarchy},
			Tags:     "docs-validate-heading-hierarchy",
			TestingT: t,
		},
	}
	if suite.Run() != 0 {
		t.Fatal("non-zero status returned, failed to run integration feature tests")
	}
}
