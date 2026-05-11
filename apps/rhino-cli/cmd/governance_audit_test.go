package cmd

import (
	"bytes"
	"context"
	"crypto/sha256"
	"encoding/hex"
	"encoding/json"
	"fmt"
	"os"
	"path/filepath"
	"runtime"
	"strings"
	"testing"

	"github.com/cucumber/godog"
	governance "github.com/wahidyankf/ose-public/apps/rhino-cli/internal/repo-governance"
)

// Step constant patterns for repo-governance audit scenarios. Each constant
// matches a Gherkin line in
// specs/apps/rhino/behavior/cli/gherkin/repo-governance-audit.feature.
const (
	stepAuditRepoAllCategoriesPass             = `^a repository where every deterministic governance category reports zero findings$`
	stepAuditRepoMixedFindings                 = `^a repository where two deterministic governance categories report findings and the rest pass$`
	stepAuditRepoFixedFindingSet               = `^a repository where deterministic governance categories return a fixed finding set$`
	stepAuditRepoKeyMatchesFalsePositive       = `^a repository where a finding key matches a known-false-positives entry$`
	stepAuditRepoAnyFindingSet                 = `^a repository where deterministic governance categories return any finding set$`
	stepDeveloperRunsRepoGovernanceAudit       = `^the developer runs repo-governance audit$`
	stepDeveloperRunsAuditTenTimesFixedClock   = `^the developer runs repo-governance audit ten consecutive times with a fixed clock$`
	stepDeveloperRunsAuditIncludeOneCategory   = `^the developer runs repo-governance audit with include-category limited to one category$`
	stepOutputTotalFindingsZero                = `^the output reports total_findings equal to zero across all categories$`
	stepOutputTotalFindingsSumOfCategories     = `^the output reports total_findings equal to the sum of category findings$`
	stepEveryRunProducesByteIdenticalJSON      = `^every run produces byte-identical JSON output$`
	stepMatchingFindingAppearsAsSkipped        = `^the matching finding appears under skipped_false_positives$`
	stepMatchingFindingDoesNotCountTowardTotal = `^the matching finding does not count toward total_findings$`
	stepOnlyListedCategoryAppearsInResult      = `^only the listed category appears in the result categories list$`
)

var specsDirUnitGovernanceAudit = func() string {
	_, f, _, _ := runtime.Caller(0)
	return filepath.Join(filepath.Dir(f), "../../../specs/apps/rhino/behavior/cli/gherkin")
}()

type governanceAuditUnitSteps struct {
	cmdErr    error
	cmdOutput string
	envelopes []governance.AuditEnvelope
}

func (s *governanceAuditUnitSteps) before(_ context.Context, _ *godog.Scenario) (context.Context, error) {
	verbose = false
	quiet = false
	output = "json"
	_ = parseOutputFormat(nil, nil)
	governanceAuditSkip = nil
	governanceAuditIncludeOnly = nil
	s.cmdErr = nil
	s.cmdOutput = ""
	s.envelopes = nil

	osGetwd = func() (string, error) { return "/mock-repo", nil }
	osStat = func(name string) (os.FileInfo, error) {
		if name == "/mock-repo/.git" {
			return &mockFileInfo{name: ".git", isDir: true}, nil
		}
		return nil, os.ErrNotExist
	}
	runAuditFn = func(_ governance.AuditOptions) (governance.AuditEnvelope, error) {
		return governance.AuditEnvelope{Schema: governance.AuditEnvelopeSchema, Status: "ok"}, nil
	}
	return context.Background(), nil
}

func (s *governanceAuditUnitSteps) after(_ context.Context, _ *godog.Scenario, _ error) (context.Context, error) {
	runAuditFn = governance.RunAudit
	osGetwd = os.Getwd
	osStat = os.Stat
	governanceAuditSkip = nil
	governanceAuditIncludeOnly = nil
	output = "text"
	_ = parseOutputFormat(nil, nil)
	return context.Background(), nil
}

// allCategoriesPass wires a mock that returns an envelope with all 11 categories
// present, every category passed=true, total_findings=0.
func (s *governanceAuditUnitSteps) allCategoriesPass() error {
	runAuditFn = func(_ governance.AuditOptions) (governance.AuditEnvelope, error) {
		cats := make([]governance.AuditCategoryResult, 0, len(governance.AuditCategoryOrder))
		for _, name := range governance.AuditCategoryOrder {
			cats = append(cats, governance.AuditCategoryResult{
				Name:    name,
				Command: governance.AuditCategoryCommand[name],
				Passed:  true,
			})
		}
		return governance.AuditEnvelope{
			Schema: governance.AuditEnvelopeSchema,
			Status: "ok",
			Result: governance.AuditResult{
				GitSHA:        "fixedsha",
				RanAt:         "2026-05-12T00:00:00Z",
				TotalFindings: 0,
				BySeverity:    map[string]int{},
				ByCategory:    map[string]int{},
				Categories:    cats,
			},
		}, nil
	}
	return nil
}

// mixedFindings wires a mock that returns 2 failing categories among 11.
func (s *governanceAuditUnitSteps) mixedFindings() error {
	runAuditFn = func(_ governance.AuditOptions) (governance.AuditEnvelope, error) {
		cats := make([]governance.AuditCategoryResult, 0, len(governance.AuditCategoryOrder))
		total := 0
		bySev := map[string]int{}
		byCat := map[string]int{}
		for i, name := range governance.AuditCategoryOrder {
			var findings []governance.AuditFinding
			if i == 0 || i == 5 {
				findings = []governance.AuditFinding{{
					Key:         name + "|x|cafef00d",
					Severity:    "high",
					Criticality: "HIGH",
					File:        "x",
					Message:     "synthetic " + name,
				}}
				total++
				bySev["high"]++
				byCat[name]++
			}
			cats = append(cats, governance.AuditCategoryResult{
				Name:     name,
				Command:  governance.AuditCategoryCommand[name],
				Passed:   len(findings) == 0,
				Findings: findings,
			})
		}
		return governance.AuditEnvelope{
			Schema: governance.AuditEnvelopeSchema,
			Status: "failed",
			Result: governance.AuditResult{
				GitSHA: "fixedsha", RanAt: "2026-05-12T00:00:00Z",
				TotalFindings: total, BySeverity: bySev, ByCategory: byCat, Categories: cats,
			},
		}, nil
	}
	return nil
}

// fixedFindingSet wires a mock that always returns the same envelope regardless
// of input.
func (s *governanceAuditUnitSteps) fixedFindingSet() error { return s.mixedFindings() }

// keyMatchesFalsePositive wires a mock that places the only finding under
// skipped_false_positives — simulating the orchestrator's skip-list logic.
func (s *governanceAuditUnitSteps) keyMatchesFalsePositive() error {
	runAuditFn = func(_ governance.AuditOptions) (governance.AuditEnvelope, error) {
		cats := make([]governance.AuditCategoryResult, 0, len(governance.AuditCategoryOrder))
		for _, name := range governance.AuditCategoryOrder {
			cats = append(cats, governance.AuditCategoryResult{
				Name: name, Command: governance.AuditCategoryCommand[name], Passed: true,
			})
		}
		return governance.AuditEnvelope{
			Schema: governance.AuditEnvelopeSchema,
			Status: "ok",
			Result: governance.AuditResult{
				GitSHA: "fixedsha", RanAt: "2026-05-12T00:00:00Z",
				TotalFindings: 0,
				BySeverity:    map[string]int{},
				ByCategory:    map[string]int{},
				Categories:    cats,
				SkippedFalsePositives: []governance.AuditFinding{{
					Key:         "agents-md-size|AGENTS.md|deadbeef",
					Severity:    "high",
					Criticality: "HIGH",
					File:        "AGENTS.md",
					Message:     "AGENTS.md exceeds hard limit",
				}},
			},
		}, nil
	}
	return nil
}

// anyFindingSet is the same fixture used by the include-category scenario.
func (s *governanceAuditUnitSteps) anyFindingSet() error { return s.mixedFindings() }

// runOnce invokes the cobra command once and captures stdout + error.
func (s *governanceAuditUnitSteps) runOnce() error {
	buf := new(bytes.Buffer)
	governanceAuditCmd.SetOut(buf)
	governanceAuditCmd.SetErr(buf)
	s.cmdErr = governanceAuditCmd.RunE(governanceAuditCmd, []string{})
	s.cmdOutput = buf.String()
	return nil
}

// runTenTimesFixedClock invokes the command 10 times and stores each output
// for the byte-determinism comparator.
func (s *governanceAuditUnitSteps) runTenTimesFixedClock() error {
	outputs := make([]string, 0, 10)
	for range 10 {
		buf := new(bytes.Buffer)
		governanceAuditCmd.SetOut(buf)
		governanceAuditCmd.SetErr(buf)
		_ = governanceAuditCmd.RunE(governanceAuditCmd, []string{})
		outputs = append(outputs, buf.String())
	}
	s.cmdOutput = outputs[0]
	// Store as JSON joined by tab so the comparator can split.
	s.cmdOutput = strings.Join(outputs, "\x1f")
	return nil
}

// runIncludeOneCategory sets the include-category flag and invokes the command.
func (s *governanceAuditUnitSteps) runIncludeOneCategory() error {
	governanceAuditIncludeOnly = []string{"agents-md-size"}
	// Use a mock that respects IncludeOnly.
	prev := runAuditFn
	runAuditFn = func(opts governance.AuditOptions) (governance.AuditEnvelope, error) {
		base, err := prev(opts)
		if err != nil {
			return base, err
		}
		// Filter to only the listed names.
		if len(opts.IncludeOnly) == 0 {
			return base, nil
		}
		want := map[string]struct{}{}
		for _, n := range opts.IncludeOnly {
			want[n] = struct{}{}
		}
		filtered := make([]governance.AuditCategoryResult, 0, len(opts.IncludeOnly))
		for _, c := range base.Result.Categories {
			if _, ok := want[c.Name]; ok {
				filtered = append(filtered, c)
			}
		}
		base.Result.Categories = filtered
		return base, nil
	}
	return s.runOnce()
}

func (s *governanceAuditUnitSteps) exitsSuccessfully() error {
	if s.cmdErr != nil {
		return fmt.Errorf("expected success, got: %w\nOutput: %s", s.cmdErr, s.cmdOutput)
	}
	return nil
}

func (s *governanceAuditUnitSteps) exitsWithFailure() error {
	if s.cmdErr == nil {
		return fmt.Errorf("expected failure, output: %s", s.cmdOutput)
	}
	return nil
}

func (s *governanceAuditUnitSteps) totalFindingsZero() error {
	tf, _, _, err := s.envelopeTotals()
	if err != nil {
		return err
	}
	if tf != 0 {
		return fmt.Errorf("expected total_findings=0, got %d", tf)
	}
	return nil
}

func (s *governanceAuditUnitSteps) totalFindingsSumOfCategories() error {
	tf, _, cats, err := s.envelopeTotals()
	if err != nil {
		return err
	}
	sum := 0
	for _, c := range cats {
		m, ok := c.(map[string]any)
		if !ok {
			continue
		}
		findings, _ := m["findings"].([]any)
		sum += len(findings)
	}
	if tf != sum {
		return fmt.Errorf("total_findings %d != sum of category findings %d", tf, sum)
	}
	if tf == 0 {
		return fmt.Errorf("expected total_findings > 0 for mixed scenario")
	}
	return nil
}

// envelopeTotals decodes s.cmdOutput as a JSON envelope and returns
// (total_findings, skipped_false_positives, categories) with safe assertions.
func (s *governanceAuditUnitSteps) envelopeTotals() (int, []any, []any, error) {
	var env map[string]any
	if err := json.Unmarshal([]byte(s.cmdOutput), &env); err != nil {
		return 0, nil, nil, fmt.Errorf("invalid JSON: %w\n%s", err, s.cmdOutput)
	}
	result, ok := env["result"].(map[string]any)
	if !ok {
		return 0, nil, nil, fmt.Errorf("missing result; got: %v", env)
	}
	tfRaw, _ := result["total_findings"].(float64)
	skipped, _ := result["skipped_false_positives"].([]any)
	cats, _ := result["categories"].([]any)
	return int(tfRaw), skipped, cats, nil
}

func (s *governanceAuditUnitSteps) byteIdenticalJSON() error {
	parts := strings.Split(s.cmdOutput, "\x1f")
	if len(parts) != 10 {
		return fmt.Errorf("expected 10 captured outputs, got %d", len(parts))
	}
	hashes := make(map[string]struct{})
	for _, p := range parts {
		h := sha256.Sum256([]byte(p))
		hashes[hex.EncodeToString(h[:])] = struct{}{}
	}
	if len(hashes) != 1 {
		return fmt.Errorf("expected 1 unique SHA-256 across 10 runs, got %d", len(hashes))
	}
	return nil
}

func (s *governanceAuditUnitSteps) matchingFindingAsSkipped() error {
	_, skipped, _, err := s.envelopeTotals()
	if err != nil {
		return err
	}
	if len(skipped) != 1 {
		return fmt.Errorf("expected 1 skipped finding, got %d", len(skipped))
	}
	return nil
}

func (s *governanceAuditUnitSteps) matchingFindingNotCountedTotal() error {
	tf, _, _, err := s.envelopeTotals()
	if err != nil {
		return err
	}
	if tf != 0 {
		return fmt.Errorf("expected total_findings=0 (skipped does not count), got %d", tf)
	}
	return nil
}

func (s *governanceAuditUnitSteps) onlyListedCategoryAppears() error {
	_, _, cats, err := s.envelopeTotals()
	if err != nil {
		return err
	}
	if len(cats) != 1 {
		return fmt.Errorf("expected 1 category in result, got %d", len(cats))
	}
	m, ok := cats[0].(map[string]any)
	if !ok {
		return fmt.Errorf("category 0 is not a map: %v", cats[0])
	}
	if m["name"] != "agents-md-size" {
		return fmt.Errorf("expected agents-md-size, got %v", m["name"])
	}
	return nil
}

func TestUnitGovernanceAudit(t *testing.T) {
	s := &governanceAuditUnitSteps{}
	suite := godog.TestSuite{
		ScenarioInitializer: func(sc *godog.ScenarioContext) {
			sc.Before(s.before)
			sc.After(s.after)
			sc.Step(stepAuditRepoAllCategoriesPass, s.allCategoriesPass)
			sc.Step(stepAuditRepoMixedFindings, s.mixedFindings)
			sc.Step(stepAuditRepoFixedFindingSet, s.fixedFindingSet)
			sc.Step(stepAuditRepoKeyMatchesFalsePositive, s.keyMatchesFalsePositive)
			sc.Step(stepAuditRepoAnyFindingSet, s.anyFindingSet)
			sc.Step(stepDeveloperRunsRepoGovernanceAudit, s.runOnce)
			sc.Step(stepDeveloperRunsAuditTenTimesFixedClock, s.runTenTimesFixedClock)
			sc.Step(stepDeveloperRunsAuditIncludeOneCategory, s.runIncludeOneCategory)
			sc.Step(stepExitsSuccessfully, s.exitsSuccessfully)
			sc.Step(stepExitsWithFailure, s.exitsWithFailure)
			sc.Step(stepOutputTotalFindingsZero, s.totalFindingsZero)
			sc.Step(stepOutputTotalFindingsSumOfCategories, s.totalFindingsSumOfCategories)
			sc.Step(stepEveryRunProducesByteIdenticalJSON, s.byteIdenticalJSON)
			sc.Step(stepMatchingFindingAppearsAsSkipped, s.matchingFindingAsSkipped)
			sc.Step(stepMatchingFindingDoesNotCountTowardTotal, s.matchingFindingNotCountedTotal)
			sc.Step(stepOnlyListedCategoryAppearsInResult, s.onlyListedCategoryAppears)
		},
		Options: &godog.Options{
			Format:   "pretty",
			Paths:    []string{specsDirUnitGovernanceAudit},
			TestingT: t,
			Tags:     "repo-governance-audit",
		},
	}
	if suite.Run() != 0 {
		t.Fatal("non-zero status returned, failed to run unit feature tests")
	}
}

// TestGovernanceAudit_MissingGitRoot verifies the command fails gracefully
// when not inside a git repository.
func TestGovernanceAudit_MissingGitRoot(t *testing.T) {
	origGetwd := osGetwd
	origStat := osStat
	defer func() {
		osGetwd = origGetwd
		osStat = origStat
	}()

	osGetwd = func() (string, error) { return "/no-git-here", nil }
	osStat = func(_ string) (os.FileInfo, error) { return nil, os.ErrNotExist }

	buf := new(bytes.Buffer)
	governanceAuditCmd.SetOut(buf)
	governanceAuditCmd.SetErr(buf)
	err := governanceAuditCmd.RunE(governanceAuditCmd, []string{})
	if err == nil || !strings.Contains(err.Error(), "git") {
		t.Fatalf("expected git-root error, got: %v", err)
	}
}

// TestGovernanceAudit_PropagatesAuditError ensures internal-package errors
// surface as wrapped command errors.
func TestGovernanceAudit_PropagatesAuditError(t *testing.T) {
	origGetwd := osGetwd
	origStat := osStat
	origFn := runAuditFn
	defer func() {
		osGetwd = origGetwd
		osStat = origStat
		runAuditFn = origFn
	}()

	osGetwd = func() (string, error) { return "/mock-repo", nil }
	osStat = func(name string) (os.FileInfo, error) {
		if name == "/mock-repo/.git" {
			return &mockFileInfo{name: ".git", isDir: true}, nil
		}
		return nil, os.ErrNotExist
	}
	runAuditFn = func(_ governance.AuditOptions) (governance.AuditEnvelope, error) {
		return governance.AuditEnvelope{}, fmt.Errorf("boom")
	}
	buf := new(bytes.Buffer)
	governanceAuditCmd.SetOut(buf)
	governanceAuditCmd.SetErr(buf)
	err := governanceAuditCmd.RunE(governanceAuditCmd, []string{})
	if err == nil || !strings.Contains(err.Error(), "boom") {
		t.Fatalf("expected propagated error, got: %v", err)
	}
}

// TestGovernanceAudit_OutputFormats exercises text, JSON, and markdown
// renderers against a small fixture.
func TestGovernanceAudit_OutputFormats(t *testing.T) {
	origGetwd := osGetwd
	origStat := osStat
	origFn := runAuditFn
	defer func() {
		osGetwd = origGetwd
		osStat = origStat
		runAuditFn = origFn
		output = "text"
		_ = parseOutputFormat(nil, nil)
	}()

	osGetwd = func() (string, error) { return "/mock-repo", nil }
	osStat = func(name string) (os.FileInfo, error) {
		if name == "/mock-repo/.git" {
			return &mockFileInfo{name: ".git", isDir: true}, nil
		}
		return nil, os.ErrNotExist
	}
	runAuditFn = func(_ governance.AuditOptions) (governance.AuditEnvelope, error) {
		return governance.AuditEnvelope{
			Schema: governance.AuditEnvelopeSchema,
			Status: "failed",
			Result: governance.AuditResult{
				GitSHA: "abc1234", RanAt: "2026-05-12T00:00:00Z",
				TotalFindings: 1,
				BySeverity:    map[string]int{"high": 1},
				ByCategory:    map[string]int{"agents-md-size": 1},
				Categories: []governance.AuditCategoryResult{{
					Name: "agents-md-size", Command: "repo-governance agents-md-size", Passed: false,
					Findings: []governance.AuditFinding{{
						Key: "k", Severity: "high", Criticality: "HIGH", File: "AGENTS.md", Line: 1, Message: "oversize",
					}},
				}},
			},
		}, nil
	}

	for _, format := range []string{"json", "markdown", "text"} {
		t.Run(format, func(t *testing.T) {
			buf := new(bytes.Buffer)
			governanceAuditCmd.SetOut(buf)
			governanceAuditCmd.SetErr(buf)
			output = format
			_ = parseOutputFormat(nil, nil)
			_ = governanceAuditCmd.RunE(governanceAuditCmd, []string{})
			if buf.Len() == 0 {
				t.Errorf("format %s produced no output", format)
			}
			if format == "json" {
				var env map[string]any
				if err := json.Unmarshal(buf.Bytes(), &env); err != nil {
					t.Fatalf("invalid JSON envelope: %v\n%s", err, buf.String())
				}
				if env["schema"] != governance.AuditEnvelopeSchema {
					t.Errorf("expected schema %q, got %v", governance.AuditEnvelopeSchema, env["schema"])
				}
				if env["status"] != "failed" {
					t.Errorf("expected status 'failed', got %v", env["status"])
				}
			}
			if format == "markdown" && !strings.Contains(buf.String(), "## Governance Audit") {
				t.Errorf("markdown output missing heading, got: %s", buf.String())
			}
		})
	}
}

// TestGovernanceAudit_TextCleanOutput verifies the text formatter's PASS path.
func TestGovernanceAudit_TextCleanOutput(t *testing.T) {
	origGetwd := osGetwd
	origStat := osStat
	origFn := runAuditFn
	defer func() {
		osGetwd = origGetwd
		osStat = origStat
		runAuditFn = origFn
		output = "text"
		_ = parseOutputFormat(nil, nil)
	}()

	osGetwd = func() (string, error) { return "/mock-repo", nil }
	osStat = func(name string) (os.FileInfo, error) {
		if name == "/mock-repo/.git" {
			return &mockFileInfo{name: ".git", isDir: true}, nil
		}
		return nil, os.ErrNotExist
	}
	runAuditFn = func(_ governance.AuditOptions) (governance.AuditEnvelope, error) {
		return governance.AuditEnvelope{
			Schema: governance.AuditEnvelopeSchema, Status: "ok",
			Result: governance.AuditResult{
				GitSHA: "abc1234", RanAt: "2026-05-12T00:00:00Z",
				SkippedFalsePositives: []governance.AuditFinding{{Key: "k", Severity: "high"}},
			},
		}, nil
	}
	buf := new(bytes.Buffer)
	governanceAuditCmd.SetOut(buf)
	governanceAuditCmd.SetErr(buf)
	output = "text"
	_ = parseOutputFormat(nil, nil)
	if err := governanceAuditCmd.RunE(governanceAuditCmd, []string{}); err != nil {
		t.Fatalf("expected success, got: %v", err)
	}
	if !strings.Contains(buf.String(), "PASSED") {
		t.Errorf("expected PASSED in text output, got: %s", buf.String())
	}
	if !strings.Contains(buf.String(), "skipped false-positive") {
		t.Errorf("expected skipped count in text output, got: %s", buf.String())
	}
}
