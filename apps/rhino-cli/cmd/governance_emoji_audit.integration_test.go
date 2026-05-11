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

// Scenario: Clean source tree passes
// Scenario: Emoji codepoint in a JSON file fails
// Scenario: Emoji codepoint in a Go source file fails
// Scenario: Multibyte non-emoji unicode does not trigger a finding

var specsDirGovernanceEmojiAudit = func() string {
	_, f, _, _ := runtime.Caller(0)
	return filepath.Join(filepath.Dir(f), "../../../specs/apps/rhino/behavior/cli/gherkin")
}()

type governanceEmojiAuditIntegSteps struct {
	originalWd string
	tmpDir     string
	cmdErr     error
	cmdOutput  string
	scanArgs   []string
	expectMsg  string
}

func (s *governanceEmojiAuditIntegSteps) before(_ context.Context, _ *godog.Scenario) (context.Context, error) {
	s.originalWd, _ = os.Getwd()
	s.tmpDir, _ = os.MkdirTemp("", "emoji-audit-*")
	_ = os.MkdirAll(filepath.Join(s.tmpDir, ".git"), 0o755)
	verbose = false
	quiet = false
	output = "text"
	emojiAuditPaths = nil
	s.cmdErr = nil
	s.cmdOutput = ""
	s.scanArgs = nil
	s.expectMsg = ""
	_ = os.Chdir(s.tmpDir)
	return context.Background(), nil
}

func (s *governanceEmojiAuditIntegSteps) after(_ context.Context, _ *godog.Scenario, _ error) (context.Context, error) {
	_ = os.Chdir(s.originalWd)
	_ = os.RemoveAll(s.tmpDir)
	emojiAuditPaths = nil
	return context.Background(), nil
}

func (s *governanceEmojiAuditIntegSteps) writeFile(rel, content string) error {
	full := filepath.Join(s.tmpDir, rel)
	if err := os.MkdirAll(filepath.Dir(full), 0o755); err != nil {
		return err
	}
	return os.WriteFile(full, []byte(content), 0o600)
}

func (s *governanceEmojiAuditIntegSteps) cleanTree() error {
	s.scanArgs = []string{"."}
	if err := s.writeFile("apps/main.go", "package main\nfunc main() {}\n"); err != nil {
		return err
	}
	return s.writeFile("apps/config.json", "{\"a\": 1}\n")
}

func (s *governanceEmojiAuditIntegSteps) jsonFileWithEmoji() error {
	s.scanArgs = []string{"bad.json"}
	s.expectMsg = "U+1F600"
	// Smiling face U+1F600 inside a JSON string value.
	return s.writeFile("bad.json", "{\"greeting\": \"hello \U0001F600 world\"}\n")
}

func (s *governanceEmojiAuditIntegSteps) goFileWithEmoji() error {
	s.scanArgs = []string{"bad.go"}
	s.expectMsg = "U+2705"
	// Heavy check mark U+2705 in a Go comment.
	return s.writeFile("bad.go", "package main\n// \U00002705 marker\nfunc main() {}\n")
}

func (s *governanceEmojiAuditIntegSteps) forbiddenFileWithArabic() error {
	s.scanArgs = []string{"lang.go"}
	// Arabic, CJK, Cyrillic, Hebrew — all language scripts, not emoji.
	return s.writeFile("lang.go", "package main\n// العربية 中文 Привет שלום\nfunc main() {}\n")
}

func (s *governanceEmojiAuditIntegSteps) runOnTree() error {
	return s.run()
}

func (s *governanceEmojiAuditIntegSteps) runOnFile() error {
	return s.run()
}

func (s *governanceEmojiAuditIntegSteps) run() error {
	buf := new(bytes.Buffer)
	governanceEmojiAuditCmd.SetOut(buf)
	governanceEmojiAuditCmd.SetErr(buf)
	s.cmdErr = governanceEmojiAuditCmd.RunE(governanceEmojiAuditCmd, s.scanArgs)
	s.cmdOutput = buf.String()
	return nil
}

func (s *governanceEmojiAuditIntegSteps) exitsSuccessfully() error {
	if s.cmdErr != nil {
		return fmt.Errorf("expected success, got: %v\nOutput: %s", s.cmdErr, s.cmdOutput)
	}
	return nil
}

func (s *governanceEmojiAuditIntegSteps) exitsWithFailure() error {
	if s.cmdErr == nil {
		return fmt.Errorf("expected failure, got success\nOutput: %s", s.cmdOutput)
	}
	return nil
}

func (s *governanceEmojiAuditIntegSteps) zeroFindings() error {
	if !strings.Contains(s.cmdOutput, "PASSED") {
		return fmt.Errorf("expected PASSED, got: %s", s.cmdOutput)
	}
	return nil
}

func (s *governanceEmojiAuditIntegSteps) outputIdentifiesViolation() error {
	if s.expectMsg == "" {
		return fmt.Errorf("test setup error: expectMsg not set")
	}
	if !strings.Contains(s.cmdOutput, s.expectMsg) {
		return fmt.Errorf("expected output to contain %q, got: %s", s.expectMsg, s.cmdOutput)
	}
	return nil
}

// InitializeGovernanceEmojiAuditScenario binds the step handlers for the
// integration suite.
func InitializeGovernanceEmojiAuditScenario(sc *godog.ScenarioContext) {
	s := &governanceEmojiAuditIntegSteps{}
	sc.Before(s.before)
	sc.After(s.after)

	sc.Step(stepEmojiCleanSourceTree, s.cleanTree)
	sc.Step(stepEmojiJSONFileWithEmoji, s.jsonFileWithEmoji)
	sc.Step(stepEmojiGoFileWithEmoji, s.goFileWithEmoji)
	sc.Step(stepEmojiForbiddenFileWithNonEmojiMulti, s.forbiddenFileWithArabic)
	sc.Step(stepDeveloperRunsEmojiAuditOnTree, s.runOnTree)
	sc.Step(stepDeveloperRunsEmojiAuditOnFile, s.runOnFile)
	sc.Step(stepOutputZeroEmojiFindings, s.zeroFindings)
	sc.Step(stepOutputIdentifiesEmojiFileLineCp, s.outputIdentifiesViolation)
	sc.Step(stepExitsSuccessfully, s.exitsSuccessfully)
	sc.Step(stepExitsWithFailure, s.exitsWithFailure)
}

func TestIntegrationGovernanceEmojiAudit(t *testing.T) {
	suite := godog.TestSuite{
		ScenarioInitializer: InitializeGovernanceEmojiAuditScenario,
		Options: &godog.Options{
			Format:   "pretty",
			Paths:    []string{specsDirGovernanceEmojiAudit},
			Tags:     "repo-governance-emoji-audit",
			TestingT: t,
		},
	}
	if suite.Run() != 0 {
		t.Fatal("non-zero status returned, failed to run integration feature tests")
	}
}
