package cmd

import (
	"bytes"
	"context"
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

// Step constant patterns for repo-governance emoji-audit scenarios.
const (
	stepEmojiCleanSourceTree                = `^a source tree containing no emoji codepoints in forbidden file types$`
	stepEmojiJSONFileWithEmoji              = `^a JSON file containing an emoji codepoint$`
	stepEmojiGoFileWithEmoji                = `^a Go source file containing an emoji codepoint$`
	stepEmojiForbiddenFileWithNonEmojiMulti = `^a forbidden file containing multibyte non-emoji unicode such as Arabic$`
	stepEmojiTreeWithArchivedEmojiFile      = `^a source tree with an emoji-containing file inside the archived directory$`
	stepDeveloperRunsEmojiAuditOnTree       = `^the developer runs repo-governance emoji-audit on the tree$`
	stepDeveloperRunsEmojiAuditOnFile       = `^the developer runs repo-governance emoji-audit on the file$`
	stepOutputZeroEmojiFindings             = `^the output reports zero emoji findings$`
	stepOutputIdentifiesEmojiFileLineCp     = `^the output identifies the offending file line and codepoint$`
)

var specsDirUnitGovernanceEmojiAudit = func() string {
	_, f, _, _ := runtime.Caller(0)
	return filepath.Join(filepath.Dir(f), "../../../specs/apps/rhino/behavior/cli/gherkin")
}()

type governanceEmojiAuditUnitSteps struct {
	cmdErr    error
	cmdOutput string
	expectMsg string
}

func (s *governanceEmojiAuditUnitSteps) before(_ context.Context, _ *godog.Scenario) (context.Context, error) {
	verbose = false
	quiet = false
	output = "text"
	s.cmdErr = nil
	s.cmdOutput = ""
	s.expectMsg = ""
	emojiAuditPaths = nil

	osGetwd = func() (string, error) { return "/mock-repo", nil }
	osStat = func(name string) (os.FileInfo, error) {
		if name == "/mock-repo/.git" {
			return &mockFileInfo{name: ".git", isDir: true}, nil
		}
		return nil, os.ErrNotExist
	}
	emojiAuditFn = func(_ []string) ([]governance.EmojiFinding, error) { return nil, nil }
	return context.Background(), nil
}

func (s *governanceEmojiAuditUnitSteps) after(_ context.Context, _ *godog.Scenario, _ error) (context.Context, error) {
	emojiAuditFn = governance.AuditEmoji
	osGetwd = os.Getwd
	osStat = os.Stat
	emojiAuditPaths = nil
	return context.Background(), nil
}

func (s *governanceEmojiAuditUnitSteps) cleanTree() error {
	emojiAuditFn = func(_ []string) ([]governance.EmojiFinding, error) { return nil, nil }
	return nil
}

func (s *governanceEmojiAuditUnitSteps) jsonFileWithEmoji() error {
	s.expectMsg = "U+1F600"
	emojiAuditFn = func(_ []string) ([]governance.EmojiFinding, error) {
		return []governance.EmojiFinding{{
			File:      "/mock-repo/bad.json",
			Line:      1,
			Column:    16,
			Codepoint: "U+1F600",
			Severity:  "high",
		}}, nil
	}
	return nil
}

func (s *governanceEmojiAuditUnitSteps) goFileWithEmoji() error {
	s.expectMsg = "U+2705"
	emojiAuditFn = func(_ []string) ([]governance.EmojiFinding, error) {
		return []governance.EmojiFinding{{
			File:      "/mock-repo/bad.go",
			Line:      2,
			Column:    4,
			Codepoint: "U+2705",
			Severity:  "high",
		}}, nil
	}
	return nil
}

func (s *governanceEmojiAuditUnitSteps) forbiddenFileWithArabic() error {
	// Arabic characters are not emoji; the audit returns zero findings.
	emojiAuditFn = func(_ []string) ([]governance.EmojiFinding, error) { return nil, nil }
	return nil
}

func (s *governanceEmojiAuditUnitSteps) treeWithArchivedEmojiFile() error {
	// The archived directory is skipped; the audit returns zero findings.
	emojiAuditFn = func(_ []string) ([]governance.EmojiFinding, error) { return nil, nil }
	return nil
}

func (s *governanceEmojiAuditUnitSteps) runOnTree() error {
	return s.runWithArgs([]string{"."})
}

func (s *governanceEmojiAuditUnitSteps) runOnFile() error {
	return s.runWithArgs([]string{"bad-fixture"})
}

func (s *governanceEmojiAuditUnitSteps) runWithArgs(args []string) error {
	buf := new(bytes.Buffer)
	governanceEmojiAuditCmd.SetOut(buf)
	governanceEmojiAuditCmd.SetErr(buf)
	s.cmdErr = governanceEmojiAuditCmd.RunE(governanceEmojiAuditCmd, args)
	s.cmdOutput = buf.String()
	return nil
}

func (s *governanceEmojiAuditUnitSteps) exitsSuccessfully() error {
	if s.cmdErr != nil {
		return fmt.Errorf("expected success but got: %w\nOutput: %s", s.cmdErr, s.cmdOutput)
	}
	return nil
}

func (s *governanceEmojiAuditUnitSteps) exitsWithFailure() error {
	if s.cmdErr == nil {
		return fmt.Errorf("expected failure but succeeded\nOutput: %s", s.cmdOutput)
	}
	return nil
}

func (s *governanceEmojiAuditUnitSteps) zeroFindings() error {
	if !strings.Contains(s.cmdOutput, "PASSED") {
		return fmt.Errorf("expected PASSED in output, got: %s", s.cmdOutput)
	}
	return nil
}

func (s *governanceEmojiAuditUnitSteps) outputIdentifiesViolation() error {
	if s.expectMsg == "" {
		return fmt.Errorf("test setup error: expectMsg not set")
	}
	if !strings.Contains(s.cmdOutput, s.expectMsg) {
		return fmt.Errorf("expected output to contain %q, got: %s", s.expectMsg, s.cmdOutput)
	}
	return nil
}

func TestUnitGovernanceEmojiAudit(t *testing.T) {
	s := &governanceEmojiAuditUnitSteps{}
	suite := godog.TestSuite{
		ScenarioInitializer: func(sc *godog.ScenarioContext) {
			sc.Before(s.before)
			sc.After(s.after)
			sc.Step(stepEmojiCleanSourceTree, s.cleanTree)
			sc.Step(stepEmojiJSONFileWithEmoji, s.jsonFileWithEmoji)
			sc.Step(stepEmojiGoFileWithEmoji, s.goFileWithEmoji)
			sc.Step(stepEmojiForbiddenFileWithNonEmojiMulti, s.forbiddenFileWithArabic)
			sc.Step(stepEmojiTreeWithArchivedEmojiFile, s.treeWithArchivedEmojiFile)
			sc.Step(stepDeveloperRunsEmojiAuditOnTree, s.runOnTree)
			sc.Step(stepDeveloperRunsEmojiAuditOnFile, s.runOnFile)
			sc.Step(stepOutputZeroEmojiFindings, s.zeroFindings)
			sc.Step(stepOutputIdentifiesEmojiFileLineCp, s.outputIdentifiesViolation)
			sc.Step(stepExitsSuccessfully, s.exitsSuccessfully)
			sc.Step(stepExitsWithFailure, s.exitsWithFailure)
		},
		Options: &godog.Options{
			Format:   "pretty",
			Paths:    []string{specsDirUnitGovernanceEmojiAudit},
			TestingT: t,
			Tags:     "repo-governance-emoji-audit",
		},
	}
	if suite.Run() != 0 {
		t.Fatal("non-zero status returned, failed to run unit feature tests")
	}
}

// TestGovernanceEmojiAudit_MissingGitRoot verifies the command fails
// gracefully when not inside a git repository.
func TestGovernanceEmojiAudit_MissingGitRoot(t *testing.T) {
	origGetwd := osGetwd
	origStat := osStat
	defer func() {
		osGetwd = origGetwd
		osStat = origStat
	}()

	osGetwd = func() (string, error) { return "/no-git-here", nil }
	osStat = func(_ string) (os.FileInfo, error) { return nil, os.ErrNotExist }

	buf := new(bytes.Buffer)
	governanceEmojiAuditCmd.SetOut(buf)
	governanceEmojiAuditCmd.SetErr(buf)
	err := governanceEmojiAuditCmd.RunE(governanceEmojiAuditCmd, []string{})
	if err == nil || !strings.Contains(err.Error(), "git") {
		t.Fatalf("expected git-root error, got: %v", err)
	}
}

// TestGovernanceEmojiAudit_OutputFormats checks that all three output formats
// render non-empty content and that the JSON envelope round-trips.
func TestGovernanceEmojiAudit_OutputFormats(t *testing.T) {
	origGetwd := osGetwd
	origStat := osStat
	origFn := emojiAuditFn
	defer func() {
		osGetwd = origGetwd
		osStat = origStat
		emojiAuditFn = origFn
		output = "text"
	}()

	osGetwd = func() (string, error) { return "/mock-repo", nil }
	osStat = func(name string) (os.FileInfo, error) {
		if name == "/mock-repo/.git" {
			return &mockFileInfo{name: ".git", isDir: true}, nil
		}
		return nil, os.ErrNotExist
	}
	emojiAuditFn = func(_ []string) ([]governance.EmojiFinding, error) {
		return []governance.EmojiFinding{{
			File:      "/mock-repo/bad.go",
			Line:      1,
			Column:    5,
			Codepoint: "U+1F600",
			Severity:  "high",
		}}, nil
	}

	for _, format := range []string{"json", "markdown", "text"} {
		t.Run(format, func(t *testing.T) {
			buf := new(bytes.Buffer)
			governanceEmojiAuditCmd.SetOut(buf)
			governanceEmojiAuditCmd.SetErr(buf)
			output = format
			verbose = false
			quiet = false
			_ = governanceEmojiAuditCmd.RunE(governanceEmojiAuditCmd, []string{})
			if buf.Len() == 0 {
				t.Errorf("format %s produced no output", format)
			}
			if format == "json" {
				var env map[string]any
				if err := json.Unmarshal(buf.Bytes(), &env); err != nil {
					t.Fatalf("invalid JSON envelope: %v\n%s", err, buf.String())
				}
				if env["schema"] != emojiAuditSchema {
					t.Errorf("expected schema %q, got %v", emojiAuditSchema, env["schema"])
				}
				if env["status"] != "failed" {
					t.Errorf("expected status 'failed', got %v", env["status"])
				}
			}
			if format == "markdown" && !strings.Contains(buf.String(), "Governance Emoji Audit") {
				t.Errorf("markdown output missing heading, got: %s", buf.String())
			}
		})
	}
}

// TestGovernanceEmojiAudit_CleanCaseJSON verifies the clean-case envelope
// reports a passing status with an empty result array.
func TestGovernanceEmojiAudit_CleanCaseJSON(t *testing.T) {
	origGetwd := osGetwd
	origStat := osStat
	origFn := emojiAuditFn
	defer func() {
		osGetwd = origGetwd
		osStat = origStat
		emojiAuditFn = origFn
		output = "text"
	}()

	osGetwd = func() (string, error) { return "/mock-repo", nil }
	osStat = func(name string) (os.FileInfo, error) {
		if name == "/mock-repo/.git" {
			return &mockFileInfo{name: ".git", isDir: true}, nil
		}
		return nil, os.ErrNotExist
	}
	emojiAuditFn = func(_ []string) ([]governance.EmojiFinding, error) { return nil, nil }

	buf := new(bytes.Buffer)
	governanceEmojiAuditCmd.SetOut(buf)
	governanceEmojiAuditCmd.SetErr(buf)
	output = "json"
	if err := governanceEmojiAuditCmd.RunE(governanceEmojiAuditCmd, []string{}); err != nil {
		t.Fatalf("expected clean run to succeed, got: %v", err)
	}

	var env map[string]any
	if err := json.Unmarshal(buf.Bytes(), &env); err != nil {
		t.Fatalf("invalid JSON envelope: %v\n%s", err, buf.String())
	}
	if env["status"] != "passed" {
		t.Errorf("expected status 'passed' for clean run, got %v", env["status"])
	}
}

// TestGovernanceEmojiAudit_AuditError ensures internal-package errors
// propagate as command errors.
func TestGovernanceEmojiAudit_AuditError(t *testing.T) {
	origGetwd := osGetwd
	origStat := osStat
	origFn := emojiAuditFn
	defer func() {
		osGetwd = origGetwd
		osStat = origStat
		emojiAuditFn = origFn
	}()

	osGetwd = func() (string, error) { return "/mock-repo", nil }
	osStat = func(name string) (os.FileInfo, error) {
		if name == "/mock-repo/.git" {
			return &mockFileInfo{name: ".git", isDir: true}, nil
		}
		return nil, os.ErrNotExist
	}
	emojiAuditFn = func(_ []string) ([]governance.EmojiFinding, error) {
		return nil, fmt.Errorf("boom")
	}

	buf := new(bytes.Buffer)
	governanceEmojiAuditCmd.SetOut(buf)
	governanceEmojiAuditCmd.SetErr(buf)
	err := governanceEmojiAuditCmd.RunE(governanceEmojiAuditCmd, []string{})
	if err == nil || !strings.Contains(err.Error(), "boom") {
		t.Fatalf("expected propagated error, got: %v", err)
	}
}

// TestResolveEmojiAuditPaths verifies the precedence rules for path argument
// selection.
func TestResolveEmojiAuditPaths(t *testing.T) {
	tests := []struct {
		name      string
		args      []string
		flagPaths []string
		want      []string
	}{
		{
			name: "default when both empty",
			want: emojiAuditDefaultPaths,
		},
		{
			name:      "flag-only",
			flagPaths: []string{"apps/"},
			want:      []string{"apps/"},
		},
		{
			name:      "positional overrides flag",
			args:      []string{"libs/"},
			flagPaths: []string{"apps/"},
			want:      []string{"libs/"},
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			got := resolveEmojiAuditPaths(tt.args, tt.flagPaths)
			if !emojiStringSlicesEqual(got, tt.want) {
				t.Errorf("got %v, want %v", got, tt.want)
			}
		})
	}
}

func emojiStringSlicesEqual(a, b []string) bool {
	if len(a) != len(b) {
		return false
	}
	for i := range a {
		if a[i] != b[i] {
			return false
		}
	}
	return true
}

// TestFormatEmojiAuditMarkdown_Clean verifies the markdown clean-case branch
// renders the PASSED header.
func TestFormatEmojiAuditMarkdown_Clean(t *testing.T) {
	t.Parallel()
	out := formatEmojiAuditMarkdown(nil)
	if !strings.Contains(out, "PASSED") {
		t.Errorf("expected PASSED in clean markdown output, got: %s", out)
	}
}

// TestFormatEmojiAuditText_Clean verifies the text clean-case branch.
func TestFormatEmojiAuditText_Clean(t *testing.T) {
	t.Parallel()
	out := formatEmojiAuditText(nil)
	if !strings.Contains(out, "PASSED") {
		t.Errorf("expected PASSED in clean text output, got: %s", out)
	}
}

// TestFormatEmojiAuditJSON_Clean verifies the JSON clean-case branch
// returns an envelope with status "passed".
func TestFormatEmojiAuditJSON_Clean(t *testing.T) {
	t.Parallel()
	out, err := formatEmojiAuditJSON(nil)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	if !strings.Contains(out, `"status": "passed"`) {
		t.Errorf("expected status passed in JSON, got: %s", out)
	}
}

// TestGovernanceEmojiAudit_AbsolutePathArg verifies an absolute positional
// path is passed through unchanged (not re-rooted under the git root).
func TestGovernanceEmojiAudit_AbsolutePathArg(t *testing.T) {
	origGetwd := osGetwd
	origStat := osStat
	origFn := emojiAuditFn
	defer func() {
		osGetwd = origGetwd
		osStat = origStat
		emojiAuditFn = origFn
	}()

	osGetwd = func() (string, error) { return "/mock-repo", nil }
	osStat = func(name string) (os.FileInfo, error) {
		if name == "/mock-repo/.git" {
			return &mockFileInfo{name: ".git", isDir: true}, nil
		}
		return nil, os.ErrNotExist
	}

	var capturedPaths []string
	emojiAuditFn = func(paths []string) ([]governance.EmojiFinding, error) {
		capturedPaths = paths
		return nil, nil
	}

	buf := new(bytes.Buffer)
	governanceEmojiAuditCmd.SetOut(buf)
	governanceEmojiAuditCmd.SetErr(buf)
	if err := governanceEmojiAuditCmd.RunE(governanceEmojiAuditCmd, []string{"/tmp/some-absolute"}); err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	if len(capturedPaths) != 1 || capturedPaths[0] != "/tmp/some-absolute" {
		t.Errorf("absolute path not preserved: %v", capturedPaths)
	}
}
