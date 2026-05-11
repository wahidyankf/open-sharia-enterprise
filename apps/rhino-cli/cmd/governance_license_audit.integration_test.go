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

var specsDirIntegrationGovernanceLicenseAudit = func() string {
	_, f, _, _ := runtime.Caller(0)
	return filepath.Join(filepath.Dir(f), "../../../specs/apps/rhino/behavior/cli/gherkin")
}()

type governanceLicenseAuditIntegSteps struct {
	originalWd string
	tmpDir     string
	cmdErr     error
	cmdOutput  string
}

const integMITLicense = "MIT License\n\nCopyright (c) 2025-2026 wahidyankf\n"
const integApacheLicense = "Apache License 2.0\n\nCopyright (c) 2025-2026 wahidyankf\n"

func (s *governanceLicenseAuditIntegSteps) before(_ context.Context, _ *godog.Scenario) (context.Context, error) {
	s.originalWd, _ = os.Getwd()
	s.tmpDir, _ = os.MkdirTemp("", "license-audit-*")
	_ = os.MkdirAll(filepath.Join(s.tmpDir, ".git"), 0o755)
	verbose = false
	quiet = false
	output = "text"
	s.cmdErr = nil
	s.cmdOutput = ""
	_ = os.Chdir(s.tmpDir)
	return context.Background(), nil
}

func (s *governanceLicenseAuditIntegSteps) after(_ context.Context, _ *godog.Scenario, _ error) (context.Context, error) {
	_ = os.Chdir(s.originalWd)
	_ = os.RemoveAll(s.tmpDir)
	return context.Background(), nil
}

func (s *governanceLicenseAuditIntegSteps) writeFile(rel, content string) error {
	full := filepath.Join(s.tmpDir, rel)
	if err := os.MkdirAll(filepath.Dir(full), 0o755); err != nil {
		return err
	}
	return os.WriteFile(full, []byte(content), 0o644)
}

func (s *governanceLicenseAuditIntegSteps) writeCleanLayout() error {
	if err := s.writeFile("apps/ayokoding-web/LICENSE", integMITLicense); err != nil {
		return err
	}
	if err := s.writeFile("libs/web-ui/LICENSE", integMITLicense); err != nil {
		return err
	}
	return s.writeFile("specs/LICENSE", integMITLicense)
}

func (s *governanceLicenseAuditIntegSteps) repoAllMatching() error {
	if err := s.writeCleanLayout(); err != nil {
		return err
	}
	notice := "# Notice\n\n" +
		"| Path | License | Copyright |\n" +
		"| --- | --- | --- |\n" +
		"| `apps/ayokoding-web` | MIT | wahidyankf |\n" +
		"| `libs/web-ui` | MIT | wahidyankf |\n"
	return s.writeFile("LICENSING-NOTICE.md", notice)
}

func (s *governanceLicenseAuditIntegSteps) appMissing() error {
	// Two apps, only one has a LICENSE.
	if err := s.writeFile("apps/ayokoding-web/LICENSE", integMITLicense); err != nil {
		return err
	}
	if err := os.MkdirAll(filepath.Join(s.tmpDir, "apps", "organiclever-web"), 0o755); err != nil {
		return err
	}
	if err := s.writeFile("libs/web-ui/LICENSE", integMITLicense); err != nil {
		return err
	}
	return s.writeFile("specs/LICENSE", integMITLicense)
}

func (s *governanceLicenseAuditIntegSteps) libMissing() error {
	if err := s.writeFile("apps/ayokoding-web/LICENSE", integMITLicense); err != nil {
		return err
	}
	if err := os.MkdirAll(filepath.Join(s.tmpDir, "libs", "web-ui"), 0o755); err != nil {
		return err
	}
	return s.writeFile("specs/LICENSE", integMITLicense)
}

func (s *governanceLicenseAuditIntegSteps) noticeMismatch() error {
	if err := s.writeFile("apps/ayokoding-web/LICENSE", integMITLicense); err != nil {
		return err
	}
	// libs/golang-commons holds an Apache LICENSE...
	if err := s.writeFile("libs/golang-commons/LICENSE", integApacheLicense); err != nil {
		return err
	}
	if err := s.writeFile("specs/LICENSE", integMITLicense); err != nil {
		return err
	}
	// ...but LICENSING-NOTICE claims MIT.
	notice := "# Notice\n\n" +
		"| Path | License | Copyright |\n" +
		"| --- | --- | --- |\n" +
		"| `libs/golang-commons` | MIT | wahidyankf |\n"
	return s.writeFile("LICENSING-NOTICE.md", notice)
}

func (s *governanceLicenseAuditIntegSteps) run() error {
	buf := new(bytes.Buffer)
	governanceLicenseAuditCmd.SetOut(buf)
	governanceLicenseAuditCmd.SetErr(buf)
	s.cmdErr = governanceLicenseAuditCmd.RunE(governanceLicenseAuditCmd, []string{})
	s.cmdOutput = buf.String()
	return nil
}

func (s *governanceLicenseAuditIntegSteps) exitsSuccessfully() error {
	if s.cmdErr != nil {
		return fmt.Errorf("expected success, got: %v\nOutput: %s", s.cmdErr, s.cmdOutput)
	}
	return nil
}

func (s *governanceLicenseAuditIntegSteps) exitsWithFailure() error {
	if s.cmdErr == nil {
		return fmt.Errorf("expected failure, output: %s", s.cmdOutput)
	}
	return nil
}

func (s *governanceLicenseAuditIntegSteps) zeroFindings() error {
	if !strings.Contains(s.cmdOutput, "PASSED") {
		return fmt.Errorf("expected PASSED, got: %s", s.cmdOutput)
	}
	return nil
}

func (s *governanceLicenseAuditIntegSteps) identifiesAppMissing() error {
	lc := strings.ToLower(s.cmdOutput)
	if !strings.Contains(lc, "missing-license") || !strings.Contains(lc, "apps/organiclever-web") {
		return fmt.Errorf("expected missing-license for apps/organiclever-web, got: %s", s.cmdOutput)
	}
	return nil
}

func (s *governanceLicenseAuditIntegSteps) identifiesLibMissing() error {
	lc := strings.ToLower(s.cmdOutput)
	if !strings.Contains(lc, "missing-license") || !strings.Contains(lc, "libs/web-ui") {
		return fmt.Errorf("expected missing-license for libs/web-ui, got: %s", s.cmdOutput)
	}
	return nil
}

func (s *governanceLicenseAuditIntegSteps) identifiesSPDXMismatch() error {
	lc := strings.ToLower(s.cmdOutput)
	if !strings.Contains(lc, "spdx-mismatch") {
		return fmt.Errorf("expected spdx-mismatch, got: %s", s.cmdOutput)
	}
	if !strings.Contains(lc, "libs/golang-commons") {
		return fmt.Errorf("expected libs/golang-commons in output, got: %s", s.cmdOutput)
	}
	return nil
}

// InitializeGovernanceLicenseAuditScenario wires step definitions for the
// integration godog suite.
func InitializeGovernanceLicenseAuditScenario(sc *godog.ScenarioContext) {
	s := &governanceLicenseAuditIntegSteps{}
	sc.Before(s.before)
	sc.After(s.after)

	sc.Step(stepLicenseRepoAllMatching, s.repoAllMatching)
	sc.Step(stepLicenseAppMissing, s.appMissing)
	sc.Step(stepLicenseLibMissing, s.libMissing)
	sc.Step(stepLicenseNoticeMismatch, s.noticeMismatch)
	sc.Step(stepLicenseRunAudit, s.run)
	sc.Step(stepLicenseOutputZeroFindings, s.zeroFindings)
	sc.Step(stepLicenseOutputAppMissing, s.identifiesAppMissing)
	sc.Step(stepLicenseOutputLibMissing, s.identifiesLibMissing)
	sc.Step(stepLicenseOutputSPDXMismatch, s.identifiesSPDXMismatch)
	sc.Step(stepExitsSuccessfully, s.exitsSuccessfully)
	sc.Step(stepExitsWithFailure, s.exitsWithFailure)
}

func TestIntegrationGovernanceLicenseAudit(t *testing.T) {
	suite := godog.TestSuite{
		ScenarioInitializer: InitializeGovernanceLicenseAuditScenario,
		Options: &godog.Options{
			Format:   "pretty",
			Paths:    []string{specsDirIntegrationGovernanceLicenseAudit},
			Tags:     "repo-governance-license-audit",
			TestingT: t,
		},
	}
	if suite.Run() != 0 {
		t.Fatal("non-zero status returned, failed to run integration feature tests")
	}
}
