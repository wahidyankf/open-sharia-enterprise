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

var specsDirUnitSpecsValidateLinks = func() string {
	_, f, _, _ := runtime.Caller(0)
	return filepath.Join(filepath.Dir(f), "../../../specs/apps/rhino/behavior/cli/gherkin/specs")
}()

type specsValidateLinksUnitSteps struct {
	cmdErr    error
	cmdOutput string
}

func (s *specsValidateLinksUnitSteps) before(_ context.Context, _ *godog.Scenario) (context.Context, error) {
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

	specsValidateLinksFn = func(_, _ string) []SpecFinding {
		return nil
	}
	return context.Background(), nil
}

func (s *specsValidateLinksUnitSteps) after(_ context.Context, _ *godog.Scenario, _ error) (context.Context, error) {
	specsValidateLinksFn = validateSpecLinks
	osGetwd = os.Getwd
	osStat = os.Stat
	return context.Background(), nil
}

// Scenario: folder with all valid internal links passes validation
func (s *specsValidateLinksUnitSteps) allLinksValid() error {
	specsValidateLinksFn = func(_, _ string) []SpecFinding {
		return nil
	}
	return nil
}

// Scenario: markdown file with broken internal link reports a finding
func (s *specsValidateLinksUnitSteps) brokenInternalLink() error {
	specsValidateLinksFn = func(_, _ string) []SpecFinding {
		return []SpecFinding{{
			Category:    "links",
			Criticality: "HIGH",
			File:        "specs/apps/testapp/behavior/overview.md",
			Evidence:    "broken link: overview.md -> ../nonexistent.md (file not found)",
			Expected:    "fix or remove the link to ../nonexistent.md",
		}}
	}
	return nil
}

// Scenario: markdown file with only external HTTPS links passes validation
func (s *specsValidateLinksUnitSteps) onlyExternalLinks() error {
	specsValidateLinksFn = func(_, _ string) []SpecFinding {
		return nil
	}
	return nil
}

// Scenario: folder path that does not exist
func (s *specsValidateLinksUnitSteps) noSuchapp() error {
	specsValidateLinksFn = func(_, _ string) []SpecFinding {
		return []SpecFinding{{
			Category:    "links",
			Criticality: "HIGH",
			File:        "specs/apps/nosuchapp",
			Evidence:    "spec folder does not exist: specs/apps/nosuchapp",
			Expected:    "create the spec folder",
		}}
	}
	return nil
}

func (s *specsValidateLinksUnitSteps) runValidateLinksTestapp() error {
	buf := new(bytes.Buffer)
	specsValidateLinksCmd.SetOut(buf)
	specsValidateLinksCmd.SetErr(buf)
	s.cmdErr = specsValidateLinksCmd.RunE(specsValidateLinksCmd, []string{"specs/apps/testapp"})
	s.cmdOutput = buf.String()
	return nil
}

func (s *specsValidateLinksUnitSteps) runValidateLinksNosuchapp() error {
	buf := new(bytes.Buffer)
	specsValidateLinksCmd.SetOut(buf)
	specsValidateLinksCmd.SetErr(buf)
	s.cmdErr = specsValidateLinksCmd.RunE(specsValidateLinksCmd, []string{"specs/apps/nosuchapp"})
	s.cmdOutput = buf.String()
	return nil
}

func (s *specsValidateLinksUnitSteps) exitsSuccessfully() error {
	if s.cmdErr != nil {
		return fmt.Errorf("expected success but got: %v\nOutput: %s", s.cmdErr, s.cmdOutput)
	}
	return nil
}

func (s *specsValidateLinksUnitSteps) exitsWithFailure() error {
	if s.cmdErr == nil {
		return fmt.Errorf("expected failure but succeeded\nOutput: %s", s.cmdOutput)
	}
	return nil
}

func (s *specsValidateLinksUnitSteps) outputContainsZeroFinding() error {
	if !strings.Contains(s.cmdOutput, "0 finding") {
		return fmt.Errorf("expected '0 finding' in output but got: %s", s.cmdOutput)
	}
	return nil
}

func (s *specsValidateLinksUnitSteps) outputContainsBrokenLink() error {
	if !strings.Contains(s.cmdOutput, "broken link") {
		return fmt.Errorf("expected 'broken link' in output but got: %s", s.cmdOutput)
	}
	return nil
}

func (s *specsValidateLinksUnitSteps) outputContainsDoesNotExist() error {
	if !strings.Contains(s.cmdOutput, "does not exist") {
		return fmt.Errorf("expected 'does not exist' in output but got: %s", s.cmdOutput)
	}
	return nil
}

func TestUnitSpecsValidateLinks(t *testing.T) {
	s := &specsValidateLinksUnitSteps{}
	suite := godog.TestSuite{
		ScenarioInitializer: func(sc *godog.ScenarioContext) {
			sc.Before(s.before)
			sc.After(s.after)
			sc.Step(stepSpecsLinksAllLinksValid, s.allLinksValid)
			sc.Step(stepSpecsLinksBrokenInternalLink, s.brokenInternalLink)
			sc.Step(stepSpecsLinksOnlyExternalLinks, s.onlyExternalLinks)
			sc.Step(stepSpecsLinksNoSuchapp, s.noSuchapp)
			sc.Step(stepSpecsRunValidateLinksTestapp, s.runValidateLinksTestapp)
			sc.Step(stepSpecsRunValidateLinksNosuchapp, s.runValidateLinksNosuchapp)
			sc.Step(stepExitsSuccessfully, s.exitsSuccessfully)
			sc.Step(stepExitsWithFailure, s.exitsWithFailure)
			sc.Step(stepSpecsLinksOutputContainsZeroFinding, s.outputContainsZeroFinding)
			sc.Step(stepSpecsLinksOutputContainsBrokenLink, s.outputContainsBrokenLink)
			sc.Step(stepSpecsLinksOutputContainsDoesNotExist, s.outputContainsDoesNotExist)
		},
		Options: &godog.Options{
			Format:   "pretty",
			Paths:    []string{specsDirUnitSpecsValidateLinks},
			TestingT: t,
			Tags:     "specs-validate-links",
		},
	}
	if suite.Run() != 0 {
		t.Fatal("non-zero status returned, failed to run unit feature tests")
	}
}

func TestSpecsValidateLinksCmd_MissingGitRoot(t *testing.T) {
	origGetwd := osGetwd
	origStat := osStat
	defer func() {
		osGetwd = origGetwd
		osStat = origStat
	}()

	osGetwd = func() (string, error) { return "/no-git-here", nil }
	osStat = func(_ string) (os.FileInfo, error) { return nil, os.ErrNotExist }

	buf := new(bytes.Buffer)
	specsValidateLinksCmd.SetOut(buf)
	specsValidateLinksCmd.SetErr(buf)

	err := specsValidateLinksCmd.RunE(specsValidateLinksCmd, []string{"specs/apps/testapp"})
	if err == nil || !strings.Contains(err.Error(), "git") {
		t.Fatalf("expected git-root error, got: %v", err)
	}
}

func TestValidateSpecLinks_Logic(t *testing.T) {
	tests := []struct {
		name         string
		statFunc     func(string) (os.FileInfo, error)
		walkMdFn     func(string) []string
		readFileFn   func(string) ([]byte, error)
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
			walkMdFn:     func(_ string) []string { return nil },
			readFileFn:   func(_ string) ([]byte, error) { return nil, os.ErrNotExist },
			wantFindings: 1,
			wantEvidence: []string{"does not exist"},
		},
		{
			name:   "no md files = no findings",
			folder: "specs/apps/testapp",
			statFunc: func(name string) (os.FileInfo, error) {
				return &mockFileInfo{name: filepath.Base(name), isDir: true}, nil
			},
			walkMdFn:     func(_ string) []string { return nil },
			readFileFn:   func(_ string) ([]byte, error) { return nil, nil },
			wantFindings: 0,
		},
		{
			name:   "external links only = no findings",
			folder: "specs/apps/testapp",
			statFunc: func(name string) (os.FileInfo, error) {
				return &mockFileInfo{name: filepath.Base(name), isDir: true}, nil
			},
			walkMdFn: func(_ string) []string {
				return []string{"/mock-repo/specs/apps/testapp/README.md"}
			},
			readFileFn: func(_ string) ([]byte, error) {
				return []byte(`See [external](https://example.com) for details.`), nil
			},
			wantFindings: 0,
		},
		{
			name:   "broken internal link = one finding",
			folder: "specs/apps/testapp",
			statFunc: func(name string) (os.FileInfo, error) {
				// The folder itself exists; the linked file does not.
				if strings.Contains(name, "nonexistent") {
					return nil, os.ErrNotExist
				}
				return &mockFileInfo{name: filepath.Base(name), isDir: true}, nil
			},
			walkMdFn: func(_ string) []string {
				return []string{"/mock-repo/specs/apps/testapp/overview.md"}
			},
			readFileFn: func(_ string) ([]byte, error) {
				return []byte(`See [something](./nonexistent.md) for details.`), nil
			},
			wantFindings: 1,
			wantEvidence: []string{"broken link"},
		},
		{
			name:   "valid internal link = no findings",
			folder: "specs/apps/testapp",
			statFunc: func(name string) (os.FileInfo, error) {
				return &mockFileInfo{name: filepath.Base(name), isDir: false}, nil
			},
			walkMdFn: func(_ string) []string {
				return []string{"/mock-repo/specs/apps/testapp/overview.md"}
			},
			readFileFn: func(_ string) ([]byte, error) {
				return []byte(`See [something](./existing.md) for details.`), nil
			},
			wantFindings: 0,
		},
		{
			name:   "link with fragment only = no findings",
			folder: "specs/apps/testapp",
			statFunc: func(name string) (os.FileInfo, error) {
				return &mockFileInfo{name: filepath.Base(name), isDir: false}, nil
			},
			walkMdFn: func(_ string) []string {
				return []string{"/mock-repo/specs/apps/testapp/overview.md"}
			},
			readFileFn: func(_ string) ([]byte, error) {
				return []byte(`Jump to [section](#heading)`), nil
			},
			wantFindings: 0,
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			origStat := osStat
			origWalkMd := specsWalkMdFilesFn
			origReadFile := readFileFn

			osStat = tt.statFunc
			specsWalkMdFilesFn = tt.walkMdFn
			readFileFn = tt.readFileFn

			defer func() {
				osStat = origStat
				specsWalkMdFilesFn = origWalkMd
				readFileFn = origReadFile
			}()

			findings := validateSpecLinks("/mock-repo", tt.folder)
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

func TestWalkMdFiles(t *testing.T) {
	tests := []struct {
		name      string
		readDirFn func(string) ([]os.DirEntry, error)
		wantCount int
	}{
		{
			name: "single md file",
			readDirFn: func(dir string) ([]os.DirEntry, error) {
				if dir == "/root" {
					return []os.DirEntry{
						&mockDirEntry{name: "README.md", isDir: false},
					}, nil
				}
				return nil, os.ErrNotExist
			},
			wantCount: 1,
		},
		{
			name: "readdir error returns empty",
			readDirFn: func(_ string) ([]os.DirEntry, error) {
				return nil, os.ErrNotExist
			},
			wantCount: 0,
		},
		{
			name: "non-md files are skipped",
			readDirFn: func(dir string) ([]os.DirEntry, error) {
				if dir == "/root" {
					return []os.DirEntry{
						&mockDirEntry{name: "spec.txt", isDir: false},
						&mockDirEntry{name: "spec.md", isDir: false},
					}, nil
				}
				return nil, os.ErrNotExist
			},
			wantCount: 1,
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			origReadDir := readDirFn
			readDirFn = tt.readDirFn
			defer func() { readDirFn = origReadDir }()

			got := walkMdFiles("/root")
			if len(got) != tt.wantCount {
				t.Errorf("got %d files, want %d", len(got), tt.wantCount)
			}
		})
	}
}
