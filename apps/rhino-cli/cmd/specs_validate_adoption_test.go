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

var specsDirUnitSpecsValidateAdoption = func() string {
	_, f, _, _ := runtime.Caller(0)
	return filepath.Join(filepath.Dir(f), "../../../specs/apps/rhino/behavior/cli/gherkin/specs")
}()

type specsValidateAdoptionUnitSteps struct {
	cmdErr    error
	cmdOutput string
}

func (s *specsValidateAdoptionUnitSteps) before(_ context.Context, _ *godog.Scenario) (context.Context, error) {
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

	specsValidateAdoptionFn = func(_, _ string) []SpecFinding {
		return nil
	}
	return context.Background(), nil
}

func (s *specsValidateAdoptionUnitSteps) after(_ context.Context, _ *godog.Scenario, _ error) (context.Context, error) {
	specsValidateAdoptionFn = validateSpecAdoption
	osGetwd = os.Getwd
	osStat = os.Stat
	return context.Background(), nil
}

// Scenario: app with BDD feature files and bounded-contexts.yaml passes validation
func (s *specsValidateAdoptionUnitSteps) bothPresent() error {
	specsValidateAdoptionFn = func(_, _ string) []SpecFinding {
		return nil
	}
	return nil
}

// Scenario: app missing behavior feature files reports a finding
func (s *specsValidateAdoptionUnitSteps) noFeatureFiles() error {
	specsValidateAdoptionFn = func(_, _ string) []SpecFinding {
		return []SpecFinding{{
			Category:    "adoption",
			Criticality: "HIGH",
			File:        "specs/apps/testapp/behavior",
			Evidence:    "no feature files found under specs/apps/testapp/behavior/",
			Expected:    "add at least one .feature file under specs/apps/testapp/behavior/",
		}}
	}
	return nil
}

// Scenario: app missing bounded-contexts.yaml reports a finding
func (s *specsValidateAdoptionUnitSteps) noBcYaml() error {
	specsValidateAdoptionFn = func(_, _ string) []SpecFinding {
		return []SpecFinding{{
			Category:    "adoption",
			Criticality: "HIGH",
			File:        "specs/apps/testapp/ddd",
			Evidence:    "missing bounded-contexts.yaml at specs/apps/testapp/ddd/bounded-contexts.yaml",
			Expected:    "create specs/apps/testapp/ddd/bounded-contexts.yaml",
		}}
	}
	return nil
}

// Scenario: unknown app with no spec tree at all
func (s *specsValidateAdoptionUnitSteps) noSpecTree() error {
	specsValidateAdoptionFn = func(_, _ string) []SpecFinding {
		return []SpecFinding{
			{
				Category:    "adoption",
				Criticality: "HIGH",
				File:        "specs/apps/unknownapp/behavior",
				Evidence:    "no feature files found under specs/apps/unknownapp/behavior/ (directory does not exist)",
				Expected:    "create specs/apps/unknownapp/behavior/ with at least one .feature file",
			},
			{
				Category:    "adoption",
				Criticality: "HIGH",
				File:        "specs/apps/unknownapp/ddd",
				Evidence:    "missing bounded-contexts.yaml at specs/apps/unknownapp/ddd/bounded-contexts.yaml",
				Expected:    "create specs/apps/unknownapp/ddd/bounded-contexts.yaml",
			},
		}
	}
	return nil
}

func (s *specsValidateAdoptionUnitSteps) runValidateAdoptionTestapp() error {
	buf := new(bytes.Buffer)
	specsValidateAdoptionCmd.SetOut(buf)
	specsValidateAdoptionCmd.SetErr(buf)
	s.cmdErr = specsValidateAdoptionCmd.RunE(specsValidateAdoptionCmd, []string{"testapp"})
	s.cmdOutput = buf.String()
	return nil
}

func (s *specsValidateAdoptionUnitSteps) runValidateAdoptionUnknownapp() error {
	buf := new(bytes.Buffer)
	specsValidateAdoptionCmd.SetOut(buf)
	specsValidateAdoptionCmd.SetErr(buf)
	s.cmdErr = specsValidateAdoptionCmd.RunE(specsValidateAdoptionCmd, []string{"unknownapp"})
	s.cmdOutput = buf.String()
	return nil
}

func (s *specsValidateAdoptionUnitSteps) exitsSuccessfully() error {
	if s.cmdErr != nil {
		return fmt.Errorf("expected success but got: %w\nOutput: %s", s.cmdErr, s.cmdOutput)
	}
	return nil
}

func (s *specsValidateAdoptionUnitSteps) exitsWithFailure() error {
	if s.cmdErr == nil {
		return fmt.Errorf("expected failure but succeeded\nOutput: %s", s.cmdOutput)
	}
	return nil
}

func (s *specsValidateAdoptionUnitSteps) outputContainsZeroFinding() error {
	if !strings.Contains(s.cmdOutput, "0 finding") {
		return fmt.Errorf("expected '0 finding' in output but got: %s", s.cmdOutput)
	}
	return nil
}

func (s *specsValidateAdoptionUnitSteps) outputContainsNoFeatureFiles() error {
	if !strings.Contains(s.cmdOutput, "no feature files") {
		return fmt.Errorf("expected 'no feature files' in output but got: %s", s.cmdOutput)
	}
	return nil
}

func (s *specsValidateAdoptionUnitSteps) outputContainsBcYaml() error {
	if !strings.Contains(s.cmdOutput, "bounded-contexts.yaml") {
		return fmt.Errorf("expected 'bounded-contexts.yaml' in output but got: %s", s.cmdOutput)
	}
	return nil
}

func TestUnitSpecsValidateAdoption(t *testing.T) {
	s := &specsValidateAdoptionUnitSteps{}
	suite := godog.TestSuite{
		ScenarioInitializer: func(sc *godog.ScenarioContext) {
			sc.Before(s.before)
			sc.After(s.after)
			sc.Step(stepSpecsAdoptionBothPresent, s.bothPresent)
			sc.Step(stepSpecsAdoptionNoFeatureFiles, s.noFeatureFiles)
			sc.Step(stepSpecsAdoptionNoBcYaml, s.noBcYaml)
			sc.Step(stepSpecsAdoptionNoSpecTree, s.noSpecTree)
			sc.Step(stepSpecsRunValidateAdoptionTestapp, s.runValidateAdoptionTestapp)
			sc.Step(stepSpecsRunValidateAdoptionUnknownapp, s.runValidateAdoptionUnknownapp)
			sc.Step(stepExitsSuccessfully, s.exitsSuccessfully)
			sc.Step(stepExitsWithFailure, s.exitsWithFailure)
			sc.Step(stepSpecsAdoptionOutputContainsZeroFinding, s.outputContainsZeroFinding)
			sc.Step(stepSpecsAdoptionOutputContainsNoFeatureFiles, s.outputContainsNoFeatureFiles)
			sc.Step(stepSpecsAdoptionOutputContainsBcYaml, s.outputContainsBcYaml)
		},
		Options: &godog.Options{
			Format:   "pretty",
			Paths:    []string{specsDirUnitSpecsValidateAdoption},
			TestingT: t,
			Tags:     "specs-validate-adoption",
		},
	}
	if suite.Run() != 0 {
		t.Fatal("non-zero status returned, failed to run unit feature tests")
	}
}

func TestSpecsValidateAdoptionCmd_MissingGitRoot(t *testing.T) {
	origGetwd := osGetwd
	origStat := osStat
	defer func() {
		osGetwd = origGetwd
		osStat = origStat
	}()

	osGetwd = func() (string, error) { return "/no-git-here", nil }
	osStat = func(_ string) (os.FileInfo, error) { return nil, os.ErrNotExist }

	buf := new(bytes.Buffer)
	specsValidateAdoptionCmd.SetOut(buf)
	specsValidateAdoptionCmd.SetErr(buf)

	err := specsValidateAdoptionCmd.RunE(specsValidateAdoptionCmd, []string{"testapp"})
	if err == nil || !strings.Contains(err.Error(), "git") {
		t.Fatalf("expected git-root error, got: %v", err)
	}
}

func TestValidateSpecAdoption_Logic(t *testing.T) {
	tests := []struct {
		name           string
		statFunc       func(string) (os.FileInfo, error)
		walkFeaturesFn func(string) []string
		app            string
		wantFindings   int
		wantEvidence   []string
	}{
		{
			name: "both present = no findings",
			app:  "testapp",
			statFunc: func(name string) (os.FileInfo, error) {
				return &mockFileInfo{name: filepath.Base(name), isDir: true}, nil
			},
			walkFeaturesFn: func(_ string) []string {
				return []string{"/mock-repo/specs/apps/testapp/behavior/cli/test.feature"}
			},
			wantFindings: 0,
		},
		{
			name: "behavior dir missing = one finding for feature files",
			app:  "testapp",
			statFunc: func(name string) (os.FileInfo, error) {
				if strings.HasSuffix(name, "/behavior") {
					return nil, os.ErrNotExist
				}
				return &mockFileInfo{name: filepath.Base(name), isDir: true}, nil
			},
			walkFeaturesFn: func(_ string) []string { return nil },
			wantFindings:   1,
			wantEvidence:   []string{"no feature files"},
		},
		{
			name: "behavior dir exists but empty = one finding",
			app:  "testapp",
			statFunc: func(name string) (os.FileInfo, error) {
				return &mockFileInfo{name: filepath.Base(name), isDir: true}, nil
			},
			walkFeaturesFn: func(_ string) []string { return nil },
			wantFindings:   1,
			wantEvidence:   []string{"no feature files"},
		},
		{
			name: "bc.yaml missing = one finding",
			app:  "testapp",
			statFunc: func(name string) (os.FileInfo, error) {
				if strings.HasSuffix(name, "bounded-contexts.yaml") {
					return nil, os.ErrNotExist
				}
				return &mockFileInfo{name: filepath.Base(name), isDir: true}, nil
			},
			walkFeaturesFn: func(_ string) []string {
				return []string{"/mock-repo/specs/apps/testapp/behavior/test.feature"}
			},
			wantFindings: 1,
			wantEvidence: []string{"bounded-contexts.yaml"},
		},
		{
			name: "no spec tree at all = two findings",
			app:  "unknownapp",
			statFunc: func(name string) (os.FileInfo, error) {
				return nil, os.ErrNotExist
			},
			walkFeaturesFn: func(_ string) []string { return nil },
			wantFindings:   2,
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			origStat := osStat
			origWalkFeatures := specsWalkFeatureFilesFn

			osStat = tt.statFunc
			specsWalkFeatureFilesFn = tt.walkFeaturesFn

			defer func() {
				osStat = origStat
				specsWalkFeatureFilesFn = origWalkFeatures
			}()

			findings := validateSpecAdoption("/mock-repo", tt.app)
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

func TestResolveAdoptionApps(t *testing.T) {
	tests := []struct {
		name       string
		positional []string
		appsFlag   []string
		want       []string
	}{
		{
			name:       "no positional, no flag → defaults to allowlist",
			positional: nil,
			appsFlag:   nil,
			want:       []string{"organiclever", "wahidyankf", "oseplatform", "ayokoding"},
		},
		{
			name:       "explicit positional preserved as single-app list",
			positional: []string{"organiclever"},
			appsFlag:   nil,
			want:       []string{"organiclever"},
		},
		{
			name:       "--apps flag overrides defaults",
			positional: nil,
			appsFlag:   []string{"organiclever", "wahidyankf"},
			want:       []string{"organiclever", "wahidyankf"},
		},
		{
			name:       "--apps flag with single app",
			positional: nil,
			appsFlag:   []string{"organiclever"},
			want:       []string{"organiclever"},
		},
		{
			name:       "positional wins over --apps flag (back-compat priority)",
			positional: []string{"oseplatform"},
			appsFlag:   []string{"organiclever", "wahidyankf"},
			want:       []string{"oseplatform"},
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			got := resolveAdoptionApps(tt.positional, tt.appsFlag)
			if len(got) != len(tt.want) {
				t.Fatalf("resolveAdoptionApps() len = %d, want %d; got = %v, want = %v",
					len(got), len(tt.want), got, tt.want)
			}
			for i := range tt.want {
				if got[i] != tt.want[i] {
					t.Errorf("resolveAdoptionApps()[%d] = %q, want %q", i, got[i], tt.want[i])
				}
			}
		})
	}
}

// TestSpecsValidateAdoption_NoArgsDefaultsToAllowlist verifies that calling
// the command with zero positional args and no --apps flag iterates over
// the allowlist. The fake validator counts how many apps it was called for.
func TestSpecsValidateAdoption_NoArgsDefaultsToAllowlist(t *testing.T) {
	origGetwd := osGetwd
	origStat := osStat
	origFn := specsValidateAdoptionFn
	defer func() {
		osGetwd = origGetwd
		osStat = origStat
		specsValidateAdoptionFn = origFn
		// Reset --apps flag for next test.
		_ = specsValidateAdoptionCmd.Flags().Set("apps", "")
	}()

	osGetwd = func() (string, error) { return "/mock-repo", nil }
	osStat = func(name string) (os.FileInfo, error) {
		if name == "/mock-repo/.git" {
			return &mockFileInfo{name: ".git", isDir: true}, nil
		}
		return nil, os.ErrNotExist
	}

	calledFor := []string{}
	specsValidateAdoptionFn = func(_, app string) []SpecFinding {
		calledFor = append(calledFor, app)
		return nil
	}

	buf := new(bytes.Buffer)
	specsValidateAdoptionCmd.SetOut(buf)
	specsValidateAdoptionCmd.SetErr(buf)

	if err := specsValidateAdoptionCmd.RunE(specsValidateAdoptionCmd, []string{}); err != nil {
		t.Fatalf("expected success, got: %v\nOutput: %s", err, buf.String())
	}

	if len(calledFor) != 4 {
		t.Errorf("expected validator called for 4 allowlist apps, got %d: %v",
			len(calledFor), calledFor)
	}
}

// TestSpecsValidateAdoption_AppsFlagOverrides verifies that --apps flag
// invokes the validator for exactly the flag values.
func TestSpecsValidateAdoption_AppsFlagOverrides(t *testing.T) {
	origGetwd := osGetwd
	origStat := osStat
	origFn := specsValidateAdoptionFn
	defer func() {
		osGetwd = origGetwd
		osStat = origStat
		specsValidateAdoptionFn = origFn
		_ = specsValidateAdoptionCmd.Flags().Set("apps", "")
	}()

	osGetwd = func() (string, error) { return "/mock-repo", nil }
	osStat = func(name string) (os.FileInfo, error) {
		if name == "/mock-repo/.git" {
			return &mockFileInfo{name: ".git", isDir: true}, nil
		}
		return nil, os.ErrNotExist
	}

	calledFor := []string{}
	specsValidateAdoptionFn = func(_, app string) []SpecFinding {
		calledFor = append(calledFor, app)
		return nil
	}

	if err := specsValidateAdoptionCmd.Flags().Set("apps", "organiclever,wahidyankf"); err != nil {
		t.Fatalf("failed to set --apps flag: %v", err)
	}

	buf := new(bytes.Buffer)
	specsValidateAdoptionCmd.SetOut(buf)
	specsValidateAdoptionCmd.SetErr(buf)

	if err := specsValidateAdoptionCmd.RunE(specsValidateAdoptionCmd, []string{}); err != nil {
		t.Fatalf("expected success, got: %v\nOutput: %s", err, buf.String())
	}

	if len(calledFor) != 2 {
		t.Errorf("expected validator called for 2 apps, got %d: %v", len(calledFor), calledFor)
	}
	if len(calledFor) >= 2 && (calledFor[0] != "organiclever" || calledFor[1] != "wahidyankf") {
		t.Errorf("expected [organiclever wahidyankf], got %v", calledFor)
	}
}

// TestSpecsValidateAdoption_PositionalPreserved verifies that a single
// positional arg keeps today's single-app behavior unchanged.
func TestSpecsValidateAdoption_PositionalPreserved(t *testing.T) {
	origGetwd := osGetwd
	origStat := osStat
	origFn := specsValidateAdoptionFn
	defer func() {
		osGetwd = origGetwd
		osStat = origStat
		specsValidateAdoptionFn = origFn
		_ = specsValidateAdoptionCmd.Flags().Set("apps", "")
	}()

	osGetwd = func() (string, error) { return "/mock-repo", nil }
	osStat = func(name string) (os.FileInfo, error) {
		if name == "/mock-repo/.git" {
			return &mockFileInfo{name: ".git", isDir: true}, nil
		}
		return nil, os.ErrNotExist
	}

	calledFor := []string{}
	specsValidateAdoptionFn = func(_, app string) []SpecFinding {
		calledFor = append(calledFor, app)
		return nil
	}

	buf := new(bytes.Buffer)
	specsValidateAdoptionCmd.SetOut(buf)
	specsValidateAdoptionCmd.SetErr(buf)

	if err := specsValidateAdoptionCmd.RunE(specsValidateAdoptionCmd, []string{"organiclever"}); err != nil {
		t.Fatalf("expected success, got: %v\nOutput: %s", err, buf.String())
	}

	if len(calledFor) != 1 || calledFor[0] != "organiclever" {
		t.Errorf("expected validator called once for [organiclever], got %v", calledFor)
	}
}

func TestWalkFeatureFiles(t *testing.T) {
	tests := []struct {
		name      string
		readDirFn func(string) ([]os.DirEntry, error)
		wantCount int
	}{
		{
			name: "single feature file",
			readDirFn: func(dir string) ([]os.DirEntry, error) {
				if dir == "/root" {
					return []os.DirEntry{
						&mockDirEntry{name: "test.feature", isDir: false},
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
			name: "non-feature files are skipped",
			readDirFn: func(dir string) ([]os.DirEntry, error) {
				if dir == "/root" {
					return []os.DirEntry{
						&mockDirEntry{name: "README.md", isDir: false},
						&mockDirEntry{name: "test.feature", isDir: false},
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

			got := walkFeatureFiles("/root")
			if len(got) != tt.wantCount {
				t.Errorf("got %d files, want %d", len(got), tt.wantCount)
			}
		})
	}
}
