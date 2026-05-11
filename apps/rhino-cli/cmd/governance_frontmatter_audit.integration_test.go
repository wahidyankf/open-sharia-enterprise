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

// Scenario: Clean directory passes the audit
// Scenario: Frontmatter with forbidden updated field fails
// Scenario: Body containing Last Updated footer block fails
// Scenario: Body containing standalone Created annotation fails
// Scenario: File under website app directory is exempt and passes

var specsDirGovernanceFrontmatterAudit = func() string {
	_, f, _, _ := runtime.Caller(0)
	return filepath.Join(filepath.Dir(f), "../../../specs/apps/rhino/behavior/cli/gherkin")
}()

type governanceFrontmatterAuditIntegSteps struct {
	originalWd string
	tmpDir     string
	cmdErr     error
	cmdOutput  string
	scanArgs   []string
	expectVar  string
}

func (s *governanceFrontmatterAuditIntegSteps) before(_ context.Context, _ *godog.Scenario) (context.Context, error) {
	s.originalWd, _ = os.Getwd()
	s.tmpDir, _ = os.MkdirTemp("", "frontmatter-audit-*")
	_ = os.MkdirAll(filepath.Join(s.tmpDir, ".git"), 0o755)
	_ = os.MkdirAll(filepath.Join(s.tmpDir, "repo-governance"), 0o755)
	verbose = false
	quiet = false
	output = "text"
	frontmatterAuditPaths = nil
	s.cmdErr = nil
	s.cmdOutput = ""
	s.scanArgs = nil
	s.expectVar = ""
	_ = os.Chdir(s.tmpDir)
	return context.Background(), nil
}

func (s *governanceFrontmatterAuditIntegSteps) after(_ context.Context, _ *godog.Scenario, _ error) (context.Context, error) {
	_ = os.Chdir(s.originalWd)
	_ = os.RemoveAll(s.tmpDir)
	frontmatterAuditPaths = nil
	return context.Background(), nil
}

func (s *governanceFrontmatterAuditIntegSteps) writeFile(rel, content string) error {
	full := filepath.Join(s.tmpDir, rel)
	if err := os.MkdirAll(filepath.Dir(full), 0o755); err != nil {
		return err
	}
	return os.WriteFile(full, []byte(content), 0o644)
}

func (s *governanceFrontmatterAuditIntegSteps) cleanDirectory() error {
	s.scanArgs = []string{"repo-governance/"}
	return s.writeFile("repo-governance/clean.md", `---
title: Clean
created: 2026-01-01
---

# Clean

Body.
`)
}

func (s *governanceFrontmatterAuditIntegSteps) forbiddenUpdatedField() error {
	s.scanArgs = []string{"repo-governance/bad.md"}
	s.expectVar = "updated"
	return s.writeFile("repo-governance/bad.md", `---
title: Bad
created: 2026-01-01
updated: 2026-04-01
---

# Bad

Body.
`)
}

func (s *governanceFrontmatterAuditIntegSteps) lastUpdatedFooter() error {
	s.scanArgs = []string{"repo-governance/footer.md"}
	s.expectVar = "Last Updated"
	return s.writeFile("repo-governance/footer.md", `---
title: Footer
---

# Footer

Body.

---

**Last Updated**: 2026-04-01
`)
}

func (s *governanceFrontmatterAuditIntegSteps) inlineCreatedAnnotation() error {
	s.scanArgs = []string{"repo-governance/inline.md"}
	s.expectVar = "inline date annotation"
	return s.writeFile("repo-governance/inline.md", `---
title: Inline
---

# Inline

## Document History

- **Created**: 2026-04-01
- **Last Updated**: 2026-04-02
`)
}

func (s *governanceFrontmatterAuditIntegSteps) websiteExemption() error {
	// Scan a path that contains a website-app subdirectory; the exempt file
	// has violations but must be ignored.
	s.scanArgs = []string{"apps/"}
	return s.writeFile("apps/ayokoding-web/content/post.md", `---
title: Post
updated: 2026-04-01
---

# Post

**Last Updated**: 2026-04-01
`)
}

func (s *governanceFrontmatterAuditIntegSteps) runOnDir() error {
	return s.run()
}

func (s *governanceFrontmatterAuditIntegSteps) runOnFile() error {
	return s.run()
}

func (s *governanceFrontmatterAuditIntegSteps) run() error {
	buf := new(bytes.Buffer)
	governanceFrontmatterAuditCmd.SetOut(buf)
	governanceFrontmatterAuditCmd.SetErr(buf)
	s.cmdErr = governanceFrontmatterAuditCmd.RunE(governanceFrontmatterAuditCmd, s.scanArgs)
	s.cmdOutput = buf.String()
	return nil
}

func (s *governanceFrontmatterAuditIntegSteps) exitsSuccessfully() error {
	if s.cmdErr != nil {
		return fmt.Errorf("expected success, got: %v\nOutput: %s", s.cmdErr, s.cmdOutput)
	}
	return nil
}

func (s *governanceFrontmatterAuditIntegSteps) exitsWithFailure() error {
	if s.cmdErr == nil {
		return fmt.Errorf("expected failure, got success\nOutput: %s", s.cmdOutput)
	}
	return nil
}

func (s *governanceFrontmatterAuditIntegSteps) zeroFindings() error {
	if !strings.Contains(s.cmdOutput, "PASSED") {
		return fmt.Errorf("expected PASSED, got: %s", s.cmdOutput)
	}
	return nil
}

func (s *governanceFrontmatterAuditIntegSteps) outputIdentifiesViolation() error {
	if s.expectVar == "" {
		return fmt.Errorf("test setup error: expectVar not set")
	}
	if !strings.Contains(s.cmdOutput, s.expectVar) {
		return fmt.Errorf("expected output to mention %q, got: %s", s.expectVar, s.cmdOutput)
	}
	return nil
}

// InitializeGovernanceFrontmatterAuditScenario binds the step handlers for
// the integration suite.
func InitializeGovernanceFrontmatterAuditScenario(sc *godog.ScenarioContext) {
	s := &governanceFrontmatterAuditIntegSteps{}
	sc.Before(s.before)
	sc.After(s.after)

	sc.Step(stepGovernanceDirNoForbiddenDates, s.cleanDirectory)
	sc.Step(stepGovernanceFileForbiddenUpdatedField, s.forbiddenUpdatedField)
	sc.Step(stepGovernanceFileLastUpdatedFooter, s.lastUpdatedFooter)
	sc.Step(stepGovernanceFileInlineCreatedAnnotation, s.inlineCreatedAnnotation)
	sc.Step(stepWebsiteFileWithForbiddenDateMetadata, s.websiteExemption)
	sc.Step(stepDeveloperRunsFrontmatterAuditOnDir, s.runOnDir)
	sc.Step(stepDeveloperRunsFrontmatterAuditOnFile, s.runOnFile)
	sc.Step(stepOutputZeroFrontmatterFindings, s.zeroFindings)
	sc.Step(stepOutputIdentifiesForbiddenFrontmatterField, s.outputIdentifiesViolation)
	sc.Step(stepOutputIdentifiesForbiddenFooterBlock, s.outputIdentifiesViolation)
	sc.Step(stepOutputIdentifiesForbiddenInlineAnnotation, s.outputIdentifiesViolation)
	sc.Step(stepExitsSuccessfully, s.exitsSuccessfully)
	sc.Step(stepExitsWithFailure, s.exitsWithFailure)
}

func TestIntegrationGovernanceFrontmatterAudit(t *testing.T) {
	suite := godog.TestSuite{
		ScenarioInitializer: InitializeGovernanceFrontmatterAuditScenario,
		Options: &godog.Options{
			Format:   "pretty",
			Paths:    []string{specsDirGovernanceFrontmatterAudit},
			Tags:     "repo-governance-frontmatter-audit",
			TestingT: t,
		},
	}
	if suite.Run() != 0 {
		t.Fatal("non-zero status returned, failed to run integration feature tests")
	}
}
