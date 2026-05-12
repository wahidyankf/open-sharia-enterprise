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

// Scenario: Software-engineering doc with all required frontmatter fields passes
// Scenario: Software-engineering doc missing title fails
// Scenario: Software-engineering doc missing category field fails
// Scenario: Software-engineering doc with category other than software fails
// Scenario: Governance doc with only title passes the lighter schema

var specsDirIntegrationDocsValidateFrontmatter = func() string {
	_, f, _, _ := runtime.Caller(0)
	return filepath.Join(filepath.Dir(f), "../../../specs/apps/rhino/behavior/cli/gherkin")
}()

type docsValidateFrontmatterIntegSteps struct {
	originalWd string
	tmpDir     string
	cmdErr     error
	cmdOutput  string
	scanArgs   []string
}

func (s *docsValidateFrontmatterIntegSteps) before(_ context.Context, _ *godog.Scenario) (context.Context, error) {
	s.originalWd, _ = os.Getwd()
	s.tmpDir, _ = os.MkdirTemp("", "docs-validate-frontmatter-*")
	_ = os.MkdirAll(filepath.Join(s.tmpDir, ".git"), 0o755)
	verbose = false
	quiet = false
	output = "text"
	s.cmdErr = nil
	s.cmdOutput = ""
	s.scanArgs = nil
	_ = os.Chdir(s.tmpDir)
	return context.Background(), nil
}

func (s *docsValidateFrontmatterIntegSteps) after(_ context.Context, _ *godog.Scenario, _ error) (context.Context, error) {
	_ = os.Chdir(s.originalWd)
	_ = os.RemoveAll(s.tmpDir)
	return context.Background(), nil
}

func (s *docsValidateFrontmatterIntegSteps) writeFile(rel, content string) error {
	full := filepath.Join(s.tmpDir, rel)
	if err := os.MkdirAll(filepath.Dir(full), 0o755); err != nil {
		return fmt.Errorf("mkdir %s: %w", filepath.Dir(full), err)
	}
	return os.WriteFile(full, []byte(content), 0o644)
}

func (s *docsValidateFrontmatterIntegSteps) softwareDocClean() error {
	s.scanArgs = []string{"docs/"}
	return s.writeFile("docs/explanation/software-engineering/clean.md", `---
title: Clean Doc
description: A fully populated doc.
category: software
subcategory: testing
tags:
  - go
  - testing
---

# Clean Doc

Body.
`)
}

func (s *docsValidateFrontmatterIntegSteps) softwareDocMissingTitle() error {
	s.scanArgs = []string{"docs/"}
	return s.writeFile("docs/explanation/software-engineering/no-title.md", `---
description: missing title
category: software
subcategory: testing
tags: [go]
---

Body.
`)
}

func (s *docsValidateFrontmatterIntegSteps) softwareDocMissingCategory() error {
	s.scanArgs = []string{"docs/"}
	return s.writeFile("docs/explanation/software-engineering/no-cat.md", `---
title: No Cat
description: missing category
subcategory: testing
tags: [go]
---

Body.
`)
}

func (s *docsValidateFrontmatterIntegSteps) softwareDocWrongCategoryValue() error {
	s.scanArgs = []string{"docs/"}
	return s.writeFile("docs/explanation/software-engineering/wrong-cat.md", `---
title: Wrong Cat
description: wrong category value
category: hardware
subcategory: testing
tags: [go]
---

Body.
`)
}

func (s *docsValidateFrontmatterIntegSteps) governanceDocTitleOnly() error {
	s.scanArgs = []string{"repo-governance/"}
	return s.writeFile("repo-governance/conventions/structure/test.md", `---
title: Some Convention
---

Body.
`)
}

func (s *docsValidateFrontmatterIntegSteps) run() error {
	buf := new(bytes.Buffer)
	docsValidateFrontmatterCmd.SetOut(buf)
	docsValidateFrontmatterCmd.SetErr(buf)
	s.cmdErr = docsValidateFrontmatterCmd.RunE(docsValidateFrontmatterCmd, s.scanArgs)
	s.cmdOutput = buf.String()
	return nil
}

func (s *docsValidateFrontmatterIntegSteps) exitsSuccessfully() error {
	if s.cmdErr != nil {
		return fmt.Errorf("expected success, got: %v\nOutput: %s", s.cmdErr, s.cmdOutput)
	}
	return nil
}

func (s *docsValidateFrontmatterIntegSteps) exitsWithFailure() error {
	if s.cmdErr == nil {
		return fmt.Errorf("expected failure, got success\nOutput: %s", s.cmdOutput)
	}
	return nil
}

func (s *docsValidateFrontmatterIntegSteps) zeroFailFindings() error {
	if strings.Contains(s.cmdOutput, "FAILED") {
		return fmt.Errorf("expected output not to contain FAILED, got: %s", s.cmdOutput)
	}
	if !strings.Contains(s.cmdOutput, "PASSED") {
		return fmt.Errorf("expected PASSED in output, got: %s", s.cmdOutput)
	}
	return nil
}

func (s *docsValidateFrontmatterIntegSteps) outputIdentifiesMissingTitle() error {
	if !strings.Contains(s.cmdOutput, "missing-title") {
		return fmt.Errorf("expected output to contain missing-title, got: %s", s.cmdOutput)
	}
	return nil
}

func (s *docsValidateFrontmatterIntegSteps) outputIdentifiesMissingCategory() error {
	if !strings.Contains(s.cmdOutput, "missing-category") {
		return fmt.Errorf("expected output to contain missing-category, got: %s", s.cmdOutput)
	}
	return nil
}

func (s *docsValidateFrontmatterIntegSteps) outputIdentifiesWrongCategory() error {
	if !strings.Contains(s.cmdOutput, "wrong-category-value") {
		return fmt.Errorf("expected output to contain wrong-category-value, got: %s", s.cmdOutput)
	}
	return nil
}

func (s *docsValidateFrontmatterIntegSteps) diataxisCategoryDoc(category string) error {
	s.scanArgs = []string{"docs/"}
	return s.writeFile("docs/explanation/software-engineering/diataxis-test.md", "---\ntitle: Test Doc\ndescription: A test.\ncategory: "+category+"\nsubcategory: testing\ntags:\n  - go\n---\n\n# Test\n\nBody.\n")
}

func (s *docsValidateFrontmatterIntegSteps) diataxisTutorial() error {
	return s.diataxisCategoryDoc("tutorial")
}
func (s *docsValidateFrontmatterIntegSteps) diataxisHowTo() error {
	return s.diataxisCategoryDoc("how-to")
}
func (s *docsValidateFrontmatterIntegSteps) diataxisReference() error {
	return s.diataxisCategoryDoc("reference")
}
func (s *docsValidateFrontmatterIntegSteps) diataxisExplanation() error {
	return s.diataxisCategoryDoc("explanation")
}
func (s *docsValidateFrontmatterIntegSteps) diataxisDeprecatedSoftware() error {
	return s.diataxisCategoryDoc("software")
}
func (s *docsValidateFrontmatterIntegSteps) diataxisUnknownCategory() error {
	return s.diataxisCategoryDoc("foobar")
}

func (s *docsValidateFrontmatterIntegSteps) outputContainsDeprecatedWarning() error {
	if !strings.Contains(s.cmdOutput, "category-deprecated") {
		return fmt.Errorf("expected output to contain 'category-deprecated', got: %s", s.cmdOutput)
	}
	return nil
}

// TestIntegration_DocsValidateFrontmatter_Diataxis verifies Diátaxis category
// values produce the correct pass/warn/fail outcomes end-to-end.
func TestIntegration_DocsValidateFrontmatter_Diataxis(t *testing.T) {
	cases := []struct {
		name         string
		category     string
		wantSuccess  bool
		wantContains string
	}{
		{name: "tutorial passes", category: "tutorial", wantSuccess: true, wantContains: "PASSED"},
		{name: "how-to passes", category: "how-to", wantSuccess: true, wantContains: "PASSED"},
		{name: "reference passes", category: "reference", wantSuccess: true, wantContains: "PASSED"},
		{name: "explanation passes", category: "explanation", wantSuccess: true, wantContains: "PASSED"},
		{name: "deprecated software warns", category: "software", wantSuccess: true, wantContains: "category-deprecated"},
		{name: "unknown fails", category: "foobar", wantSuccess: false, wantContains: "wrong-category-value"},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			dir := t.TempDir()
			relPath := "docs/explanation/software-engineering/diataxis-test.md"
			fullPath := filepath.Join(dir, relPath)
			if err := os.MkdirAll(filepath.Dir(fullPath), 0o755); err != nil {
				t.Fatalf("mkdir: %v", err)
			}
			content := "---\ntitle: Test Doc\ndescription: A test.\ncategory: " + tc.category + "\nsubcategory: testing\ntags:\n  - go\n---\n\n# Test\n\nBody.\n"
			if err := os.WriteFile(fullPath, []byte(content), 0o644); err != nil {
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
			docsValidateFrontmatterCmd.SetOut(&buf)
			docsValidateFrontmatterCmd.SetErr(&buf)
			err := docsValidateFrontmatterCmd.RunE(docsValidateFrontmatterCmd, []string{"docs/"})

			if tc.wantSuccess && err != nil {
				t.Errorf("expected success, got: %v\nOutput: %s", err, buf.String())
			}
			if !tc.wantSuccess && err == nil {
				t.Errorf("expected failure, got success\nOutput: %s", buf.String())
			}
			if !strings.Contains(buf.String(), tc.wantContains) {
				t.Errorf("expected output to contain %q, got: %s", tc.wantContains, buf.String())
			}
		})
	}
}

// InitializeDocsValidateFrontmatterScenario binds step handlers for the
// integration suite.
func InitializeDocsValidateFrontmatterScenario(sc *godog.ScenarioContext) {
	s := &docsValidateFrontmatterIntegSteps{}
	sc.Before(s.before)
	sc.After(s.after)

	sc.Step(stepSoftwareDocAllFields, s.softwareDocClean)
	sc.Step(stepSoftwareDocAllFieldsTutorial, s.diataxisTutorial)
	sc.Step(stepSoftwareDocAllFieldsHowTo, s.diataxisHowTo)
	sc.Step(stepSoftwareDocAllFieldsReference, s.diataxisReference)
	sc.Step(stepSoftwareDocAllFieldsExplanation, s.diataxisExplanation)
	sc.Step(stepSoftwareDocMissingTitle, s.softwareDocMissingTitle)
	sc.Step(stepSoftwareDocMissingCategory, s.softwareDocMissingCategory)
	sc.Step(stepSoftwareDocWrongCategoryValue, s.softwareDocWrongCategoryValue)
	sc.Step(stepGovernanceDocTitleOnly, s.governanceDocTitleOnly)
	sc.Step(stepDeveloperRunsDocsValidateFrontmatter, s.run)
	sc.Step(stepExitsSuccessfully, s.exitsSuccessfully)
	sc.Step(stepExitsWithFailure, s.exitsWithFailure)
	sc.Step(stepFrontmatterOutputZeroFailFindings, s.zeroFailFindings)
	sc.Step(stepFrontmatterOutputIdentifiesField, s.outputIdentifiesMissingTitle)
	sc.Step(stepFrontmatterOutputIdentifiesCategory, s.outputIdentifiesMissingCategory)
	sc.Step(stepFrontmatterOutputIdentifiesWrongCat, s.outputIdentifiesWrongCategory)
}

// TestIntegrationDocsValidateFrontmatter runs the godog feature suite against
// /tmp fixtures so the real filesystem walk, YAML parser, and validator logic
// are exercised end-to-end.
func TestIntegrationDocsValidateFrontmatter(t *testing.T) {
	suite := godog.TestSuite{
		ScenarioInitializer: InitializeDocsValidateFrontmatterScenario,
		Options: &godog.Options{
			Format:   "pretty",
			Paths:    []string{specsDirIntegrationDocsValidateFrontmatter},
			Tags:     "docs-validate-frontmatter",
			TestingT: t,
		},
	}
	if suite.Run() != 0 {
		t.Fatal("non-zero status returned, failed to run integration feature tests")
	}
}
