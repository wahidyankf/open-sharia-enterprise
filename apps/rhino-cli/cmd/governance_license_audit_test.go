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
	governance "github.com/wahidyankf/ose-public/apps/rhino-cli/internal/repo-governance"
)

// Step constants for repo-governance license-audit unit scenarios.
const (
	stepLicenseRepoAllMatching    = `^a repository where every required directory has a matching MIT LICENSE file$`
	stepLicenseAppMissing         = `^a repository where one app directory is missing its LICENSE file$`
	stepLicenseLibMissing         = `^a repository where one lib directory is missing its LICENSE file$`
	stepLicenseNoticeMismatch     = `^a repository where a LICENSING-NOTICE\.md table row claims a license that disagrees with the on-disk LICENSE file$`
	stepLicenseRunAudit           = `^the developer runs repo-governance license-audit$`
	stepLicenseOutputZeroFindings = `^the output reports zero license findings$`
	stepLicenseOutputAppMissing   = `^the output identifies the missing LICENSE app directory$`
	stepLicenseOutputLibMissing   = `^the output identifies the missing LICENSE lib directory$`
	stepLicenseOutputSPDXMismatch = `^the output identifies the SPDX mismatch$`
)

var specsDirUnitGovernanceLicenseAudit = func() string {
	_, f, _, _ := runtime.Caller(0)
	return filepath.Join(filepath.Dir(f), "../../../specs/apps/rhino/behavior/cli/gherkin")
}()

type governanceLicenseAuditUnitSteps struct {
	cmdErr    error
	cmdOutput string
}

func (s *governanceLicenseAuditUnitSteps) before(_ context.Context, _ *godog.Scenario) (context.Context, error) {
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
	licenseAuditFn = func(_ string) ([]governance.LicenseFinding, error) { return nil, nil }
	return context.Background(), nil
}

func (s *governanceLicenseAuditUnitSteps) after(_ context.Context, _ *godog.Scenario, _ error) (context.Context, error) {
	licenseAuditFn = governance.AuditLicense
	osGetwd = os.Getwd
	osStat = os.Stat
	return context.Background(), nil
}

func (s *governanceLicenseAuditUnitSteps) repoAllMatching() error {
	licenseAuditFn = func(_ string) ([]governance.LicenseFinding, error) { return nil, nil }
	return nil
}

func (s *governanceLicenseAuditUnitSteps) appMissing() error {
	licenseAuditFn = func(_ string) ([]governance.LicenseFinding, error) {
		return []governance.LicenseFinding{{
			Path:    "apps/organiclever-web",
			Kind:    "missing-license",
			Message: `required directory "apps/organiclever-web" has no LICENSE file`,
		}}, nil
	}
	return nil
}

func (s *governanceLicenseAuditUnitSteps) libMissing() error {
	licenseAuditFn = func(_ string) ([]governance.LicenseFinding, error) {
		return []governance.LicenseFinding{{
			Path:    "libs/web-ui",
			Kind:    "missing-license",
			Message: `required directory "libs/web-ui" has no LICENSE file`,
		}}, nil
	}
	return nil
}

func (s *governanceLicenseAuditUnitSteps) noticeMismatch() error {
	licenseAuditFn = func(_ string) ([]governance.LicenseFinding, error) {
		return []governance.LicenseFinding{{
			Path:    "libs/golang-commons",
			Kind:    "spdx-mismatch",
			Message: `LICENSING-NOTICE.md claims "MIT" for "libs/golang-commons" but LICENSE identifies "Apache-2.0"`,
		}}, nil
	}
	return nil
}

func (s *governanceLicenseAuditUnitSteps) run() error {
	buf := new(bytes.Buffer)
	governanceLicenseAuditCmd.SetOut(buf)
	governanceLicenseAuditCmd.SetErr(buf)
	s.cmdErr = governanceLicenseAuditCmd.RunE(governanceLicenseAuditCmd, []string{})
	s.cmdOutput = buf.String()
	return nil
}

func (s *governanceLicenseAuditUnitSteps) exitsSuccessfully() error {
	if s.cmdErr != nil {
		return fmt.Errorf("expected success but got: %w\nOutput: %s", s.cmdErr, s.cmdOutput)
	}
	return nil
}

func (s *governanceLicenseAuditUnitSteps) exitsWithFailure() error {
	if s.cmdErr == nil {
		return fmt.Errorf("expected failure but succeeded\nOutput: %s", s.cmdOutput)
	}
	return nil
}

func (s *governanceLicenseAuditUnitSteps) zeroFindings() error {
	if !strings.Contains(s.cmdOutput, "PASSED") {
		return fmt.Errorf("expected PASSED, got: %s", s.cmdOutput)
	}
	return nil
}

func (s *governanceLicenseAuditUnitSteps) identifiesAppMissing() error {
	lc := strings.ToLower(s.cmdOutput)
	if !strings.Contains(lc, "missing-license") || !strings.Contains(lc, "apps/organiclever-web") {
		return fmt.Errorf("expected missing-license naming apps/organiclever-web, got: %s", s.cmdOutput)
	}
	return nil
}

func (s *governanceLicenseAuditUnitSteps) identifiesLibMissing() error {
	lc := strings.ToLower(s.cmdOutput)
	if !strings.Contains(lc, "missing-license") || !strings.Contains(lc, "libs/web-ui") {
		return fmt.Errorf("expected missing-license naming libs/web-ui, got: %s", s.cmdOutput)
	}
	return nil
}

func (s *governanceLicenseAuditUnitSteps) identifiesSPDXMismatch() error {
	lc := strings.ToLower(s.cmdOutput)
	if !strings.Contains(lc, "spdx-mismatch") {
		return fmt.Errorf("expected spdx-mismatch, got: %s", s.cmdOutput)
	}
	if !strings.Contains(lc, "libs/golang-commons") {
		return fmt.Errorf("expected libs/golang-commons in output, got: %s", s.cmdOutput)
	}
	return nil
}

func TestUnitGovernanceLicenseAudit(t *testing.T) {
	s := &governanceLicenseAuditUnitSteps{}
	suite := godog.TestSuite{
		ScenarioInitializer: func(sc *godog.ScenarioContext) {
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
		},
		Options: &godog.Options{
			Format:   "pretty",
			Paths:    []string{specsDirUnitGovernanceLicenseAudit},
			TestingT: t,
			Tags:     "repo-governance-license-audit",
		},
	}
	if suite.Run() != 0 {
		t.Fatal("non-zero status returned, failed to run unit feature tests")
	}
}

// TestGovernanceLicenseAudit_MissingGitRoot verifies graceful failure outside a git repo.
func TestGovernanceLicenseAudit_MissingGitRoot(t *testing.T) {
	origGetwd := osGetwd
	origStat := osStat
	defer func() {
		osGetwd = origGetwd
		osStat = origStat
	}()

	osGetwd = func() (string, error) { return "/no-git-here", nil }
	osStat = func(_ string) (os.FileInfo, error) { return nil, os.ErrNotExist }

	buf := new(bytes.Buffer)
	governanceLicenseAuditCmd.SetOut(buf)
	governanceLicenseAuditCmd.SetErr(buf)

	err := governanceLicenseAuditCmd.RunE(governanceLicenseAuditCmd, []string{})
	if err == nil || !strings.Contains(err.Error(), "git") {
		t.Fatalf("expected git-root error, got: %v", err)
	}
}

// TestGovernanceLicenseAudit_RealTree exercises the real AuditLicense path
// against a small tmp fixture for coverage of the walker logic.
func TestGovernanceLicenseAudit_RealTree(t *testing.T) {
	tmp := t.TempDir()

	writeFile := func(path, content string) {
		if err := os.MkdirAll(filepath.Dir(path), 0o755); err != nil {
			t.Fatal(err)
		}
		if err := os.WriteFile(path, []byte(content), 0o644); err != nil {
			t.Fatal(err)
		}
	}

	// Clean repo: one app + one lib + specs, all MIT.
	mit := "MIT License\n\nCopyright (c) 2025-2026 wahidyankf\n"
	writeFile(filepath.Join(tmp, "apps", "ayokoding-web", "LICENSE"), mit)
	writeFile(filepath.Join(tmp, "libs", "web-ui", "LICENSE"), mit)
	writeFile(filepath.Join(tmp, "specs", "LICENSE"), mit)

	findings, err := governance.AuditLicense(tmp)
	if err != nil {
		t.Fatalf("AuditLicense: %v", err)
	}
	if len(findings) != 0 {
		t.Fatalf("expected zero findings for clean tree, got %+v", findings)
	}

	// Drop the lib LICENSE and re-run — expect one missing-license finding.
	if err := os.Remove(filepath.Join(tmp, "libs", "web-ui", "LICENSE")); err != nil {
		t.Fatal(err)
	}
	findings, err = governance.AuditLicense(tmp)
	if err != nil {
		t.Fatalf("AuditLicense after delete: %v", err)
	}
	if len(findings) != 1 || findings[0].Kind != "missing-license" || findings[0].Path != "libs/web-ui" {
		t.Fatalf("expected single missing-license for libs/web-ui, got %+v", findings)
	}
}

// TestGovernanceLicenseAudit_OutputFormats verifies all three output formats render.
func TestGovernanceLicenseAudit_OutputFormats(t *testing.T) {
	origGetwd := osGetwd
	origStat := osStat
	origFn := licenseAuditFn
	defer func() {
		osGetwd = origGetwd
		osStat = origStat
		licenseAuditFn = origFn
	}()

	osGetwd = func() (string, error) { return "/mock-repo", nil }
	osStat = func(name string) (os.FileInfo, error) {
		if name == "/mock-repo/.git" {
			return &mockFileInfo{name: ".git", isDir: true}, nil
		}
		return nil, os.ErrNotExist
	}
	licenseAuditFn = func(_ string) ([]governance.LicenseFinding, error) {
		return []governance.LicenseFinding{{
			Path:    "apps/foo",
			Kind:    "missing-license",
			Message: `required directory "apps/foo" has no LICENSE file`,
		}}, nil
	}

	for _, format := range []string{"json", "markdown", "text"} {
		t.Run(format, func(t *testing.T) {
			buf := new(bytes.Buffer)
			governanceLicenseAuditCmd.SetOut(buf)
			governanceLicenseAuditCmd.SetErr(buf)
			output = format
			verbose = false
			quiet = false
			_ = governanceLicenseAuditCmd.RunE(governanceLicenseAuditCmd, []string{})
			if buf.Len() == 0 {
				t.Errorf("format %s produced no output", format)
			}
		})
	}
	output = "text"
}

// TestGovernanceLicenseAudit_CleanFormatsAcrossOutputs verifies the clean
// (zero-finding) rendering for each output format covers the markdown and
// JSON code paths.
func TestGovernanceLicenseAudit_CleanFormatsAcrossOutputs(t *testing.T) {
	origGetwd := osGetwd
	origStat := osStat
	origFn := licenseAuditFn
	defer func() {
		osGetwd = origGetwd
		osStat = origStat
		licenseAuditFn = origFn
	}()

	osGetwd = func() (string, error) { return "/mock-repo", nil }
	osStat = func(name string) (os.FileInfo, error) {
		if name == "/mock-repo/.git" {
			return &mockFileInfo{name: ".git", isDir: true}, nil
		}
		return nil, os.ErrNotExist
	}
	licenseAuditFn = func(_ string) ([]governance.LicenseFinding, error) { return nil, nil }

	for _, format := range []string{"json", "markdown", "text"} {
		t.Run(format, func(t *testing.T) {
			buf := new(bytes.Buffer)
			governanceLicenseAuditCmd.SetOut(buf)
			governanceLicenseAuditCmd.SetErr(buf)
			output = format
			verbose = false
			quiet = false
			if err := governanceLicenseAuditCmd.RunE(governanceLicenseAuditCmd, []string{}); err != nil {
				t.Fatalf("expected clean success, got %v", err)
			}
			if buf.Len() == 0 {
				t.Errorf("format %s produced no output", format)
			}
		})
	}
	output = "text"
}

// TestGovernanceLicenseAudit_QuietMode verifies the text formatter emits nothing
// on success when quiet is set.
func TestGovernanceLicenseAudit_QuietMode(t *testing.T) {
	origFn := licenseAuditFn
	defer func() { licenseAuditFn = origFn }()
	licenseAuditFn = func(_ string) ([]governance.LicenseFinding, error) { return nil, nil }

	origGetwd := osGetwd
	origStat := osStat
	defer func() {
		osGetwd = origGetwd
		osStat = origStat
	}()
	osGetwd = func() (string, error) { return "/mock-repo", nil }
	osStat = func(name string) (os.FileInfo, error) {
		if name == "/mock-repo/.git" {
			return &mockFileInfo{name: ".git", isDir: true}, nil
		}
		return nil, os.ErrNotExist
	}

	buf := new(bytes.Buffer)
	governanceLicenseAuditCmd.SetOut(buf)
	governanceLicenseAuditCmd.SetErr(buf)
	output = "text"
	quiet = true
	verbose = false
	if err := governanceLicenseAuditCmd.RunE(governanceLicenseAuditCmd, []string{}); err != nil {
		t.Fatalf("expected success, got %v", err)
	}
	if buf.Len() != 0 {
		t.Errorf("expected zero output in quiet mode, got: %q", buf.String())
	}
	quiet = false
}

// TestGovernanceLicenseAudit_PropagatesInternalError verifies the command
// surfaces audit errors as command errors.
func TestGovernanceLicenseAudit_PropagatesInternalError(t *testing.T) {
	origGetwd := osGetwd
	origStat := osStat
	origFn := licenseAuditFn
	defer func() {
		osGetwd = origGetwd
		osStat = origStat
		licenseAuditFn = origFn
	}()
	osGetwd = func() (string, error) { return "/mock-repo", nil }
	osStat = func(name string) (os.FileInfo, error) {
		if name == "/mock-repo/.git" {
			return &mockFileInfo{name: ".git", isDir: true}, nil
		}
		return nil, os.ErrNotExist
	}
	licenseAuditFn = func(_ string) ([]governance.LicenseFinding, error) {
		return nil, fmt.Errorf("simulated internal error")
	}

	buf := new(bytes.Buffer)
	governanceLicenseAuditCmd.SetOut(buf)
	governanceLicenseAuditCmd.SetErr(buf)
	err := governanceLicenseAuditCmd.RunE(governanceLicenseAuditCmd, []string{})
	if err == nil || !strings.Contains(err.Error(), "simulated internal error") {
		t.Fatalf("expected wrapped internal error, got: %v", err)
	}
}
