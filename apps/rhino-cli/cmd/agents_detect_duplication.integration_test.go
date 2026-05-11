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

var specsDirAgentsDetectDuplication = func() string {
	_, f, _, _ := runtime.Caller(0)
	return filepath.Join(filepath.Dir(f), "../../../specs/apps/rhino/behavior/cli/gherkin")
}()

type agentsDetectDuplicationIntegSteps struct {
	originalWd string
	tmpDir     string
	cmdErr     error
	cmdOutput  string
}

func (s *agentsDetectDuplicationIntegSteps) before(_ context.Context, _ *godog.Scenario) (context.Context, error) {
	s.originalWd, _ = os.Getwd()
	s.tmpDir, _ = os.MkdirTemp("", "agents-detect-duplication-*")
	if err := os.MkdirAll(filepath.Join(s.tmpDir, ".git"), 0o755); err != nil {
		return context.Background(), err
	}
	verbose = false
	quiet = false
	output = "text"
	_ = parseOutputFormat(nil, nil)
	_ = os.Chdir(s.tmpDir)
	return context.Background(), nil
}

func (s *agentsDetectDuplicationIntegSteps) after(_ context.Context, _ *godog.Scenario, _ error) (context.Context, error) {
	_ = os.Chdir(s.originalWd)
	_ = os.RemoveAll(s.tmpDir)
	return context.Background(), nil
}

// writeAt writes content to a relative path under the integration tmp dir,
// creating parent directories. Returns the absolute path.
func (s *agentsDetectDuplicationIntegSteps) writeAt(rel, content string) string {
	full := filepath.Join(s.tmpDir, rel)
	_ = os.MkdirAll(filepath.Dir(full), 0o755)
	_ = os.WriteFile(full, []byte(content), 0o644)
	return full
}

// integShared returns the 12-line shared prose block used to seed
// cross-file duplication. Distinct prose lines (not headings/blanks) so the
// detector's exclusion rules do not suppress the match.
func (s *agentsDetectDuplicationIntegSteps) integShared() string {
	return strings.Join([]string{
		"Shared paragraph alpha describing a concrete behaviour.",
		"Shared paragraph beta explaining an invariant.",
		"Shared paragraph gamma giving a worked example.",
		"Shared paragraph delta calling out an edge case.",
		"Shared paragraph epsilon noting a non-goal.",
		"Shared paragraph zeta restating the contract.",
		"Shared paragraph eta with a step-by-step list.",
		"Shared paragraph theta with a caveat to remember.",
		"Shared paragraph iota linking to a sibling document.",
		"Shared paragraph kappa with a footnote-style aside.",
		"Shared paragraph lambda summarising the outcome.",
		"Shared paragraph mu wrapping up the section.",
	}, "\n") + "\n"
}

// integUnique returns N distinct non-heading lines to pad around the shared
// block so the surrounding context cannot itself form duplicate windows.
func (s *agentsDetectDuplicationIntegSteps) integUnique(prefix string, n int) string {
	var sb strings.Builder
	for i := 0; i < n; i++ {
		fmt.Fprintf(&sb, "%s prose line %d unique to this file.\n", prefix, i)
	}
	return sb.String()
}

func (s *agentsDetectDuplicationIntegSteps) noSharedWindows() error {
	s.writeAt(".claude/agents/alpha-maker.md", s.integUnique("alpha", 30))
	s.writeAt(".claude/agents/beta-checker.md", s.integUnique("beta", 30))
	s.writeAt(".claude/skills/gamma/SKILL.md", s.integUnique("gamma", 30))
	return nil
}

func (s *agentsDetectDuplicationIntegSteps) twoAgentsShare12Lines() error {
	shared := s.integShared()
	s.writeAt(".claude/agents/alpha-maker.md",
		s.integUnique("alpha-pre", 5)+shared+s.integUnique("alpha-post", 5))
	s.writeAt(".claude/agents/beta-checker.md",
		s.integUnique("beta-pre", 8)+shared+s.integUnique("beta-post", 3))
	return nil
}

func (s *agentsDetectDuplicationIntegSteps) agentMatchesSkill() error {
	shared := s.integShared()
	s.writeAt(".claude/agents/alpha-maker.md",
		s.integUnique("alpha-pre", 4)+shared+s.integUnique("alpha-post", 4))
	s.writeAt(".claude/skills/shared-skill/SKILL.md",
		s.integUnique("skill-pre", 6)+shared+s.integUnique("skill-post", 6))
	return nil
}

func (s *agentsDetectDuplicationIntegSteps) headingOrBlankWindow() error {
	headings := strings.Join([]string{
		"# Title",
		"## Section A",
		"### Subsection A.1",
		"#### Detail A",
		"## Section B",
		"### Subsection B.1",
		"#### Detail B",
		"## Section C",
		"### Subsection C.1",
		"#### Detail C",
	}, "\n") + "\n"
	s.writeAt(".claude/agents/alpha-maker.md",
		s.integUnique("alpha", 5)+headings+s.integUnique("alpha-post", 5))
	s.writeAt(".claude/agents/beta-checker.md",
		s.integUnique("beta", 5)+headings+s.integUnique("beta-post", 5))
	return nil
}

func (s *agentsDetectDuplicationIntegSteps) run() error {
	buf := new(bytes.Buffer)
	agentsDetectDuplicationCmd.SetOut(buf)
	agentsDetectDuplicationCmd.SetErr(buf)
	s.cmdErr = agentsDetectDuplicationCmd.RunE(agentsDetectDuplicationCmd, []string{})
	s.cmdOutput = buf.String()
	return nil
}

func (s *agentsDetectDuplicationIntegSteps) exitsSuccessfully() error {
	if s.cmdErr != nil {
		return fmt.Errorf("expected success, got: %v\nOutput: %s", s.cmdErr, s.cmdOutput)
	}
	return nil
}

func (s *agentsDetectDuplicationIntegSteps) exitsWithFailure() error {
	if s.cmdErr == nil {
		return fmt.Errorf("expected failure, output: %s", s.cmdOutput)
	}
	return nil
}

func (s *agentsDetectDuplicationIntegSteps) zeroClusters() error {
	if !strings.Contains(s.cmdOutput, "PASSED") {
		return fmt.Errorf("expected PASSED in output, got: %s", s.cmdOutput)
	}
	if !strings.Contains(s.cmdOutput, "0 clusters") {
		return fmt.Errorf("expected '0 clusters', got: %s", s.cmdOutput)
	}
	return nil
}

func (s *agentsDetectDuplicationIntegSteps) clusterAcrossBothAgents() error {
	if !strings.Contains(s.cmdOutput, "FAILED") {
		return fmt.Errorf("expected FAILED in output, got: %s", s.cmdOutput)
	}
	if !strings.Contains(s.cmdOutput, "alpha-maker.md") {
		return fmt.Errorf("expected alpha-maker.md path, got: %s", s.cmdOutput)
	}
	if !strings.Contains(s.cmdOutput, "beta-checker.md") {
		return fmt.Errorf("expected beta-checker.md path, got: %s", s.cmdOutput)
	}
	return nil
}

func (s *agentsDetectDuplicationIntegSteps) clusterAcrossAgentAndSkill() error {
	if !strings.Contains(s.cmdOutput, "FAILED") {
		return fmt.Errorf("expected FAILED in output, got: %s", s.cmdOutput)
	}
	if !strings.Contains(s.cmdOutput, "alpha-maker.md") {
		return fmt.Errorf("expected agent path, got: %s", s.cmdOutput)
	}
	if !strings.Contains(s.cmdOutput, "SKILL.md") {
		return fmt.Errorf("expected SKILL.md, got: %s", s.cmdOutput)
	}
	return nil
}

// InitializeAgentsDetectDuplicationScenario wires the integration steps into
// the godog scenario context.
func InitializeAgentsDetectDuplicationScenario(sc *godog.ScenarioContext) {
	s := &agentsDetectDuplicationIntegSteps{}
	sc.Before(s.before)
	sc.After(s.after)
	sc.Step(stepAgentsTreeNoSharedWindows, s.noSharedWindows)
	sc.Step(stepAgentsTreeTwoAgentsShare12Lines, s.twoAgentsShare12Lines)
	sc.Step(stepAgentsTreeAgentMatchesSkill, s.agentMatchesSkill)
	sc.Step(stepAgentsTreeHeadingOrBlankWindow, s.headingOrBlankWindow)
	sc.Step(stepDeveloperRunsAgentsDetectDup, s.run)
	sc.Step(stepExitsSuccessfully, s.exitsSuccessfully)
	sc.Step(stepExitsWithFailure, s.exitsWithFailure)
	sc.Step(stepOutputZeroDuplicationClusters, s.zeroClusters)
	sc.Step(stepOutputIdentifiesClusterTwoAgents, s.clusterAcrossBothAgents)
	sc.Step(stepOutputIdentifiesClusterAgentSkill, s.clusterAcrossAgentAndSkill)
}

func TestIntegrationAgentsDetectDuplication(t *testing.T) {
	suite := godog.TestSuite{
		ScenarioInitializer: InitializeAgentsDetectDuplicationScenario,
		Options: &godog.Options{
			Format:   "pretty",
			Paths:    []string{specsDirAgentsDetectDuplication},
			Tags:     "agents-detect-duplication",
			TestingT: t,
		},
	}
	if suite.Run() != 0 {
		t.Fatal("non-zero status returned, failed to run integration feature tests")
	}
}
