//go:build integration

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
	"time"

	"github.com/cucumber/godog"
	governance "github.com/wahidyankf/ose-public/apps/rhino-cli/internal/repo-governance"
)

// Scenario: Clean repository: all 11 categories pass, total_findings is 0, exit 0
// Scenario: Mixed findings: some categories pass, some fail; total_findings is the sum; exit 1
// Scenario: Byte-determinism: running the orchestrator 10 times in a row produces byte-identical JSON
// Scenario: Skip list honored: false-positive entries do not count toward total_findings
// Scenario: Include-category filter: only listed categories run

var specsDirGovernanceAudit = func() string {
	_, f, _, _ := runtime.Caller(0)
	return filepath.Join(filepath.Dir(f), "../../../specs/apps/rhino/behavior/cli/gherkin")
}()

type governanceAuditIntegSteps struct {
	originalWd string
	tmpDir     string
	cmdErr     error
	cmdOutput  string
	outputs    []string // captured outputs for the byte-determinism scenario
}

func (s *governanceAuditIntegSteps) before(_ context.Context, _ *godog.Scenario) (context.Context, error) {
	s.originalWd, _ = os.Getwd()
	s.tmpDir, _ = os.MkdirTemp("", "governance-audit-*")
	if err := os.MkdirAll(filepath.Join(s.tmpDir, ".git"), 0o755); err != nil {
		return context.Background(), err
	}
	verbose = false
	quiet = false
	output = "json"
	_ = parseOutputFormat(nil, nil)
	governanceAuditSkip = nil
	governanceAuditIncludeOnly = nil
	s.cmdErr = nil
	s.cmdOutput = ""
	s.outputs = nil

	// Override runAuditFn with a real orchestrator call wired to a fixed clock
	// so RanAt is stable across runs.
	runAuditFn = func(opts governance.AuditOptions) (governance.AuditEnvelope, error) {
		opts.Now = func() time.Time { return time.Date(2026, time.May, 12, 0, 0, 0, 0, time.UTC) }
		return governance.RunAudit(opts)
	}

	_ = os.Chdir(s.tmpDir)
	// On macOS os.Getwd() returns the canonical path under /private/var/...
	// instead of /var/...; resolve tmpDir so any direct calls to
	// governance.RunAudit see the same root that findGitRoot returns.
	if resolved, err := os.Getwd(); err == nil {
		s.tmpDir = resolved
	}
	return context.Background(), nil
}

func (s *governanceAuditIntegSteps) after(_ context.Context, _ *godog.Scenario, _ error) (context.Context, error) {
	_ = os.Chdir(s.originalWd)
	_ = os.RemoveAll(s.tmpDir)
	runAuditFn = governance.RunAudit
	governanceAuditSkip = nil
	governanceAuditIncludeOnly = nil
	output = "text"
	_ = parseOutputFormat(nil, nil)
	return context.Background(), nil
}

// writeAt writes content under tmpDir creating parent directories as needed.
func (s *governanceAuditIntegSteps) writeAt(rel, content string) error {
	full := filepath.Join(s.tmpDir, rel)
	if err := os.MkdirAll(filepath.Dir(full), 0o755); err != nil {
		return err
	}
	return os.WriteFile(full, []byte(content), 0o600)
}

// seedCleanRepo seeds a synthetic clean repo passing every category.
func (s *governanceAuditIntegSteps) seedCleanRepo() error {
	if err := s.writeAt("AGENTS.md", strings.Repeat("a", 1000)); err != nil {
		return err
	}
	// LICENSE for specs.
	if err := s.writeAt("specs/LICENSE", "MIT License\n\nCopyright (c) 2026\n"); err != nil {
		return err
	}
	if err := s.writeAt("LICENSING-NOTICE.md", "# Licensing\n\n| Path | License |\n| ---- | ------- |\n| `specs` | MIT |\n"); err != nil {
		return err
	}
	// Layer-coherence: two governance docs declaring layers 0..5 consistently.
	layers := strings.Join([]string{
		"**Layer 0: Vision**", "**Layer 1: Principles**", "**Layer 2: Conventions**",
		"**Layer 3: Development**", "**Layer 4: Agents**", "**Layer 5: Workflows**",
	}, "\n")
	if err := s.writeAt("repo-governance/README.md",
		"# Index\n\n"+layers+"\n\n- [Architecture](./repository-governance-architecture.md)\n"); err != nil {
		return err
	}
	if err := s.writeAt("repo-governance/repository-governance-architecture.md",
		"# Architecture\n\n"+layers+"\n"); err != nil {
		return err
	}
	return nil
}

// seedRepoFailingCategories seeds a repo where two categories report findings.
func (s *governanceAuditIntegSteps) seedRepoFailingCategories() error {
	if err := s.seedCleanRepo(); err != nil {
		return err
	}
	// Force agents-md-size hard-limit failure (>40 KB).
	if err := s.writeAt("AGENTS.md", strings.Repeat("x", 50000)); err != nil {
		return err
	}
	// Force emoji-audit failure: a smiling face in a Go file.
	if err := s.writeAt("apps/emoji.go", "package emoji\n\nconst greeting = \"hello \U0001F600 world\"\n"); err != nil {
		return err
	}
	return nil
}

// scenarioFixedFindingSet uses the same fixture as failing categories; the
// orchestrator with a fixed clock produces a byte-deterministic envelope.
func (s *governanceAuditIntegSteps) scenarioFixedFindingSet() error {
	return s.seedRepoFailingCategories()
}

// scenarioKeyMatchesFalsePositive seeds a single failing finding then writes a
// matching skip-list entry. Runs the orchestrator twice — once to discover the
// real key, once to assert the skip-list partition.
func (s *governanceAuditIntegSteps) scenarioKeyMatchesFalsePositive() error {
	if err := s.seedCleanRepo(); err != nil {
		return err
	}
	if err := s.writeAt("AGENTS.md", strings.Repeat("x", 50000)); err != nil {
		return err
	}
	// Discover the key by running the orchestrator directly.
	env, err := governance.RunAudit(governance.AuditOptions{
		RepoRoot: s.tmpDir,
		Now:      func() time.Time { return time.Date(2026, time.May, 12, 0, 0, 0, 0, time.UTC) },
	})
	if err != nil {
		return fmt.Errorf("preflight discover: %w", err)
	}
	var key string
	for _, c := range env.Result.Categories {
		if c.Name == "agents-md-size" && len(c.Findings) > 0 {
			key = c.Findings[0].Key
			break
		}
	}
	if key == "" {
		return fmt.Errorf("no agents-md-size finding discovered to seed skip-list")
	}
	return s.writeAt("generated-reports/.known-false-positives.md", "# Skip list\n\n- `"+key+"`\n")
}

// scenarioAnyFindingSet is reused by the include-category scenario.
func (s *governanceAuditIntegSteps) scenarioAnyFindingSet() error {
	return s.seedRepoFailingCategories()
}

// runOnce invokes the cobra command once and captures output + error.
func (s *governanceAuditIntegSteps) runOnce() error {
	buf := new(bytes.Buffer)
	governanceAuditCmd.SetOut(buf)
	governanceAuditCmd.SetErr(buf)
	s.cmdErr = governanceAuditCmd.RunE(governanceAuditCmd, []string{})
	s.cmdOutput = buf.String()
	return nil
}

// runTenTimes invokes the command 10 times against a fixed clock and stores
// every captured output for the byte-determinism comparator.
func (s *governanceAuditIntegSteps) runTenTimes() error {
	s.outputs = make([]string, 0, 10)
	for i := 0; i < 10; i++ {
		buf := new(bytes.Buffer)
		governanceAuditCmd.SetOut(buf)
		governanceAuditCmd.SetErr(buf)
		_ = governanceAuditCmd.RunE(governanceAuditCmd, []string{})
		s.outputs = append(s.outputs, buf.String())
	}
	s.cmdOutput = s.outputs[0]
	return nil
}

func (s *governanceAuditIntegSteps) runIncludeOneCategory() error {
	governanceAuditIncludeOnly = []string{"agents-md-size"}
	return s.runOnce()
}

func (s *governanceAuditIntegSteps) exitsSuccessfully() error {
	if s.cmdErr != nil {
		return fmt.Errorf("expected success, got: %v\nOutput: %s", s.cmdErr, s.cmdOutput)
	}
	return nil
}

func (s *governanceAuditIntegSteps) exitsWithFailure() error {
	if s.cmdErr == nil {
		return fmt.Errorf("expected failure, output: %s", s.cmdOutput)
	}
	return nil
}

func (s *governanceAuditIntegSteps) totalFindingsZero() error {
	env := s.parseEnvelope()
	tf := int(env["result"].(map[string]any)["total_findings"].(float64))
	if tf != 0 {
		return fmt.Errorf("expected total_findings=0, got %d\noutput: %s", tf, s.cmdOutput)
	}
	return nil
}

func (s *governanceAuditIntegSteps) totalFindingsSumOfCategories() error {
	env := s.parseEnvelope()
	result := env["result"].(map[string]any)
	tf := int(result["total_findings"].(float64))
	cats := result["categories"].([]any)
	sum := 0
	for _, c := range cats {
		findings, _ := c.(map[string]any)["findings"].([]any)
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

func (s *governanceAuditIntegSteps) byteIdenticalJSON() error {
	if len(s.outputs) != 10 {
		return fmt.Errorf("expected 10 captured outputs, got %d", len(s.outputs))
	}
	hashes := make(map[string]struct{})
	for i, p := range s.outputs {
		// Sanity: each output must be valid JSON.
		var any map[string]any
		if err := json.Unmarshal([]byte(p), &any); err != nil {
			return fmt.Errorf("run %d output is not valid JSON: %v\n%s", i, err, p)
		}
		h := sha256.Sum256([]byte(p))
		hashes[hex.EncodeToString(h[:])] = struct{}{}
	}
	if len(hashes) != 1 {
		return fmt.Errorf("expected 1 unique SHA-256 across 10 runs, got %d", len(hashes))
	}
	return nil
}

func (s *governanceAuditIntegSteps) matchingFindingAsSkipped() error {
	env := s.parseEnvelope()
	result := env["result"].(map[string]any)
	skipped, _ := result["skipped_false_positives"].([]any)
	if len(skipped) == 0 {
		return fmt.Errorf("expected ≥1 skipped finding, got 0\noutput: %s", s.cmdOutput)
	}
	return nil
}

func (s *governanceAuditIntegSteps) matchingFindingNotCountedTotal() error {
	env := s.parseEnvelope()
	tf := int(env["result"].(map[string]any)["total_findings"].(float64))
	if tf != 0 {
		return fmt.Errorf("expected total_findings=0 (skipped does not count), got %d\noutput: %s", tf, s.cmdOutput)
	}
	return nil
}

func (s *governanceAuditIntegSteps) onlyListedCategoryAppears() error {
	env := s.parseEnvelope()
	cats := env["result"].(map[string]any)["categories"].([]any)
	if len(cats) != 1 {
		return fmt.Errorf("expected 1 category in result, got %d", len(cats))
	}
	name := cats[0].(map[string]any)["name"]
	if name != "agents-md-size" {
		return fmt.Errorf("expected agents-md-size, got %v", name)
	}
	return nil
}

// parseEnvelope unmarshals s.cmdOutput as a generic JSON map; fatal-on-error
// for test simplicity.
func (s *governanceAuditIntegSteps) parseEnvelope() map[string]any {
	var env map[string]any
	if err := json.Unmarshal([]byte(s.cmdOutput), &env); err != nil {
		panic(fmt.Sprintf("invalid JSON envelope: %v\n%s", err, s.cmdOutput))
	}
	return env
}

// InitializeGovernanceAuditScenario wires the integration steps into the
// godog scenario context.
func InitializeGovernanceAuditScenario(sc *godog.ScenarioContext) {
	s := &governanceAuditIntegSteps{}
	sc.Before(s.before)
	sc.After(s.after)
	sc.Step(stepAuditRepoAllCategoriesPass, s.seedCleanRepo)
	sc.Step(stepAuditRepoMixedFindings, s.seedRepoFailingCategories)
	sc.Step(stepAuditRepoFixedFindingSet, s.scenarioFixedFindingSet)
	sc.Step(stepAuditRepoKeyMatchesFalsePositive, s.scenarioKeyMatchesFalsePositive)
	sc.Step(stepAuditRepoAnyFindingSet, s.scenarioAnyFindingSet)
	sc.Step(stepDeveloperRunsRepoGovernanceAudit, s.runOnce)
	sc.Step(stepDeveloperRunsAuditTenTimesFixedClock, s.runTenTimes)
	sc.Step(stepDeveloperRunsAuditIncludeOneCategory, s.runIncludeOneCategory)
	sc.Step(stepExitsSuccessfully, s.exitsSuccessfully)
	sc.Step(stepExitsWithFailure, s.exitsWithFailure)
	sc.Step(stepOutputTotalFindingsZero, s.totalFindingsZero)
	sc.Step(stepOutputTotalFindingsSumOfCategories, s.totalFindingsSumOfCategories)
	sc.Step(stepEveryRunProducesByteIdenticalJSON, s.byteIdenticalJSON)
	sc.Step(stepMatchingFindingAppearsAsSkipped, s.matchingFindingAsSkipped)
	sc.Step(stepMatchingFindingDoesNotCountTowardTotal, s.matchingFindingNotCountedTotal)
	sc.Step(stepOnlyListedCategoryAppearsInResult, s.onlyListedCategoryAppears)
}

func TestIntegrationGovernanceAudit(t *testing.T) {
	suite := godog.TestSuite{
		ScenarioInitializer: InitializeGovernanceAuditScenario,
		Options: &godog.Options{
			Format:   "pretty",
			Paths:    []string{specsDirGovernanceAudit},
			Tags:     "repo-governance-audit",
			TestingT: t,
		},
	}
	if suite.Run() != 0 {
		t.Fatal("non-zero status returned, failed to run integration feature tests")
	}
}
