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
	"github.com/wahidyankf/ose-public/apps/rhino-cli/internal/mermaid"
)

var specsDirUnitDocsValidateMermaid = func() string {
	_, f, _, _ := runtime.Caller(0)
	return filepath.Join(filepath.Dir(f), "../../../specs/apps/rhino/cli/gherkin")
}()

type validateMermaidUnitSteps struct {
	cmdErr    error
	cmdOutput string
}

func (s *validateMermaidUnitSteps) before(_ context.Context, _ *godog.Scenario) (context.Context, error) {
	verbose = false
	quiet = false
	output = "text"
	validateMermaidStagedOnly = false
	validateMermaidChangedOnly = false
	validateMermaidMaxLabelLen = 30
	validateMermaidMaxWidth = 3
	validateMermaidMaxDepth = 5
	s.cmdErr = nil
	s.cmdOutput = ""

	// Mock findGitRoot.
	osGetwd = func() (string, error) { return "/mock-repo", nil }
	osStat = func(name string) (os.FileInfo, error) {
		if name == "/mock-repo/.git" {
			return &mockFileInfo{name: ".git", isDir: true}, nil
		}
		return nil, os.ErrNotExist
	}

	// Default: no blocks, no violations.
	docsValidateMermaidFn = func(_ []mermaid.MermaidBlock, _ mermaid.ValidateOptions) mermaid.ValidationResult {
		return mermaid.ValidationResult{}
	}

	// Default readFileFn: return empty content so ExtractBlocks finds nothing.
	readFileFn = func(_ string) ([]byte, error) {
		return []byte(""), nil
	}

	// Default staged/changed file functions: return empty list so no git call is made.
	getMermaidStagedFilesFn = func(_ string) ([]string, error) {
		return nil, nil
	}
	getMermaidChangedFilesFn = func(_ string) ([]string, error) {
		return nil, nil
	}

	return context.Background(), nil
}

func (s *validateMermaidUnitSteps) after(_ context.Context, _ *godog.Scenario, _ error) (context.Context, error) {
	docsValidateMermaidFn = mermaid.ValidateBlocks
	osGetwd = os.Getwd
	osStat = os.Stat
	readFileFn = os.ReadFile
	getMermaidStagedFilesFn = getMermaidStagedFiles
	getMermaidChangedFilesFn = getMermaidChangedFiles
	return context.Background(), nil
}

// --- Given steps ---

func (s *validateMermaidUnitSteps) aMarkdownFileContainingFlowchartAllLabelsWithinLimit() error {
	docsValidateMermaidFn = func(_ []mermaid.MermaidBlock, _ mermaid.ValidateOptions) mermaid.ValidationResult {
		return mermaid.ValidationResult{}
	}
	return nil
}

func (s *validateMermaidUnitSteps) aMarkdownFileContainingFlowchartNodeLabelLongerThanLimit() error {
	docsValidateMermaidFn = func(_ []mermaid.MermaidBlock, _ mermaid.ValidateOptions) mermaid.ValidationResult {
		return mermaid.ValidationResult{
			Violations: []mermaid.Violation{
				{
					Kind:        mermaid.ViolationLabelTooLong,
					FilePath:    "/mock-repo/docs/a.md",
					BlockIndex:  0,
					NodeID:      "A",
					LabelText:   "This label is way too long for the limit",
					LabelLen:    40,
					MaxLabelLen: 30,
				},
			},
		}
	}
	return nil
}

func (s *validateMermaidUnitSteps) aMarkdownFileContainingFlowchartNodeLabel35Chars() error {
	// 35-char label — within limit of 40, beyond default 30.
	docsValidateMermaidFn = func(_ []mermaid.MermaidBlock, opts mermaid.ValidateOptions) mermaid.ValidationResult {
		if opts.MaxLabelLen >= 35 {
			return mermaid.ValidationResult{}
		}
		return mermaid.ValidationResult{
			Violations: []mermaid.Violation{
				{
					Kind:        mermaid.ViolationLabelTooLong,
					FilePath:    "/mock-repo/docs/a.md",
					BlockIndex:  0,
					NodeID:      "A",
					LabelText:   "This is exactly thirty-five chars!!",
					LabelLen:    35,
					MaxLabelLen: opts.MaxLabelLen,
				},
			},
		}
	}
	return nil
}

func (s *validateMermaidUnitSteps) aMarkdownFileContainingTBFlowchartTenNodesChainedSequentially() error {
	docsValidateMermaidFn = func(_ []mermaid.MermaidBlock, _ mermaid.ValidateOptions) mermaid.ValidationResult {
		return mermaid.ValidationResult{}
	}
	return nil
}

func (s *validateMermaidUnitSteps) aMarkdownFileContainingTBFlowchartNoRankMoreThan3() error {
	docsValidateMermaidFn = func(_ []mermaid.MermaidBlock, _ mermaid.ValidateOptions) mermaid.ValidationResult {
		return mermaid.ValidationResult{}
	}
	return nil
}

func (s *validateMermaidUnitSteps) aMarkdownFileContainingTBFlowchartOneRank4ParallelNodes() error {
	docsValidateMermaidFn = func(_ []mermaid.MermaidBlock, _ mermaid.ValidateOptions) mermaid.ValidationResult {
		return mermaid.ValidationResult{
			Violations: []mermaid.Violation{
				{
					Kind:        mermaid.ViolationWidthExceeded,
					FilePath:    "/mock-repo/docs/a.md",
					BlockIndex:  0,
					ActualWidth: 4,
					MaxWidth:    3,
				},
			},
		}
	}
	return nil
}

func (s *validateMermaidUnitSteps) aMarkdownFileContainingLRFlowchartNoRankMoreThan3() error {
	docsValidateMermaidFn = func(_ []mermaid.MermaidBlock, _ mermaid.ValidateOptions) mermaid.ValidationResult {
		return mermaid.ValidationResult{}
	}
	return nil
}

func (s *validateMermaidUnitSteps) aMarkdownFileContainingLRFlowchart4NodesAtSameDepth() error {
	docsValidateMermaidFn = func(_ []mermaid.MermaidBlock, _ mermaid.ValidateOptions) mermaid.ValidationResult {
		return mermaid.ValidationResult{
			Violations: []mermaid.Violation{
				{
					Kind:        mermaid.ViolationWidthExceeded,
					FilePath:    "/mock-repo/docs/a.md",
					BlockIndex:  0,
					ActualWidth: 4,
					MaxWidth:    3,
				},
			},
		}
	}
	return nil
}

func (s *validateMermaidUnitSteps) aMarkdownFileContainingFlowchart4NodesAtOneRank() error {
	// For the --max-width 5 scenario: 4 nodes exceeds default max-width 3 but passes at 5.
	docsValidateMermaidFn = func(_ []mermaid.MermaidBlock, opts mermaid.ValidateOptions) mermaid.ValidationResult {
		if opts.MaxWidth >= 4 {
			return mermaid.ValidationResult{}
		}
		return mermaid.ValidationResult{
			Violations: []mermaid.Violation{
				{
					Kind:        mermaid.ViolationWidthExceeded,
					FilePath:    "/mock-repo/docs/a.md",
					BlockIndex:  0,
					ActualWidth: 4,
					MaxWidth:    opts.MaxWidth,
				},
			},
		}
	}
	return nil
}

func (s *validateMermaidUnitSteps) aMarkdownFileContainingFlowchart4NodesAtOneRankMoreThan5Ranks() error {
	// Both span>MaxWidth AND depth>MaxDepth: emits warning, no violation.
	docsValidateMermaidFn = func(_ []mermaid.MermaidBlock, _ mermaid.ValidateOptions) mermaid.ValidationResult {
		return mermaid.ValidationResult{
			Warnings: []mermaid.Warning{
				{
					Kind:        mermaid.WarningComplexDiagram,
					FilePath:    "/mock-repo/docs/a.md",
					BlockIndex:  0,
					ActualWidth: 4,
					ActualDepth: 6,
					MaxWidth:    3,
					MaxDepth:    5,
				},
			},
		}
	}
	return nil
}

func (s *validateMermaidUnitSteps) aMarkdownFileContainingFlowchart4NodesAtOneRankExactly4RanksDeep() error {
	// For --max-depth 3 scenario: depth=4 > max-depth=3 AND span=4 > max-width=3 → warning.
	docsValidateMermaidFn = func(_ []mermaid.MermaidBlock, opts mermaid.ValidateOptions) mermaid.ValidationResult {
		if opts.MaxDepth < 4 {
			return mermaid.ValidationResult{
				Warnings: []mermaid.Warning{
					{
						Kind:        mermaid.WarningComplexDiagram,
						FilePath:    "/mock-repo/docs/a.md",
						BlockIndex:  0,
						ActualWidth: 4,
						ActualDepth: 4,
						MaxWidth:    opts.MaxWidth,
						MaxDepth:    opts.MaxDepth,
					},
				},
			}
		}
		// depth=4 <= max-depth=5 (default): span alone exceeded → violation.
		return mermaid.ValidationResult{
			Violations: []mermaid.Violation{
				{
					Kind:        mermaid.ViolationWidthExceeded,
					FilePath:    "/mock-repo/docs/a.md",
					BlockIndex:  0,
					ActualWidth: 4,
					MaxWidth:    opts.MaxWidth,
				},
			},
		}
	}
	return nil
}

func (s *validateMermaidUnitSteps) aMarkdownFileContainingMermaidBlockTwoFlowchartDeclarations() error {
	docsValidateMermaidFn = func(_ []mermaid.MermaidBlock, _ mermaid.ValidateOptions) mermaid.ValidationResult {
		return mermaid.ValidationResult{
			Violations: []mermaid.Violation{
				{
					Kind:       mermaid.ViolationMultipleDiagrams,
					FilePath:   "/mock-repo/docs/a.md",
					BlockIndex: 0,
				},
			},
		}
	}
	return nil
}

func (s *validateMermaidUnitSteps) aMarkdownFileContainingGraphKeywordNoViolations() error {
	docsValidateMermaidFn = func(_ []mermaid.MermaidBlock, _ mermaid.ValidateOptions) mermaid.ValidationResult {
		return mermaid.ValidationResult{}
	}
	return nil
}

func (s *validateMermaidUnitSteps) aMarkdownFileContainingOnlySequenceDiagramAndClassDiagram() error {
	docsValidateMermaidFn = func(_ []mermaid.MermaidBlock, _ mermaid.ValidateOptions) mermaid.ValidationResult {
		return mermaid.ValidationResult{}
	}
	return nil
}

func (s *validateMermaidUnitSteps) aMarkdownFileContainingNoMermaidCodeBlocks() error {
	docsValidateMermaidFn = func(_ []mermaid.MermaidBlock, _ mermaid.ValidateOptions) mermaid.ValidationResult {
		return mermaid.ValidationResult{}
	}
	return nil
}

func (s *validateMermaidUnitSteps) aMarkdownFileWithMermaidViolationNotStagedInGit() error {
	// Staged-only: the broken file is not staged so no violation is found.
	docsValidateMermaidFn = func(_ []mermaid.MermaidBlock, _ mermaid.ValidateOptions) mermaid.ValidationResult {
		return mermaid.ValidationResult{}
	}
	return nil
}

func (s *validateMermaidUnitSteps) aMarkdownFileWithMermaidViolationNotInPushRange() error {
	// Changed-only: the file is not in the push range so no violation is found.
	docsValidateMermaidFn = func(_ []mermaid.MermaidBlock, _ mermaid.ValidateOptions) mermaid.ValidationResult {
		return mermaid.ValidationResult{}
	}
	return nil
}

func (s *validateMermaidUnitSteps) aMarkdownFileContainingFlowchartWithLabelLengthViolation() error {
	docsValidateMermaidFn = func(_ []mermaid.MermaidBlock, _ mermaid.ValidateOptions) mermaid.ValidationResult {
		return mermaid.ValidationResult{
			Violations: []mermaid.Violation{
				{
					Kind:        mermaid.ViolationLabelTooLong,
					FilePath:    "/mock-repo/docs/a.md",
					BlockIndex:  0,
					NodeID:      "A",
					LabelText:   "This label is way too long for the limit",
					LabelLen:    40,
					MaxLabelLen: 30,
				},
			},
		}
	}
	return nil
}

func (s *validateMermaidUnitSteps) aMarkdownFileContainingFlowchartNoViolations() error {
	docsValidateMermaidFn = func(_ []mermaid.MermaidBlock, _ mermaid.ValidateOptions) mermaid.ValidationResult {
		return mermaid.ValidationResult{}
	}
	return nil
}

func (s *validateMermaidUnitSteps) aMarkdownFileUnderPlansLongLabel() error {
	docsValidateMermaidFn = func(_ []mermaid.MermaidBlock, _ mermaid.ValidateOptions) mermaid.ValidationResult {
		return mermaid.ValidationResult{
			Violations: []mermaid.Violation{
				{
					Kind:        mermaid.ViolationLabelTooLong,
					FilePath:    "/mock-repo/plans/sample/diagram.md",
					BlockIndex:  0,
					NodeID:      "A",
					LabelText:   "This is exactly thirty-five chars!!",
					LabelLen:    35,
					MaxLabelLen: 30,
				},
			},
		}
	}
	return nil
}

// --- When steps ---

func (s *validateMermaidUnitSteps) theDeveloperRunsDocsValidateMermaid() error {
	buf := new(bytes.Buffer)
	validateMermaidCmd.SetOut(buf)
	validateMermaidCmd.SetErr(buf)
	s.cmdErr = validateMermaidCmd.RunE(validateMermaidCmd, []string{})
	s.cmdOutput = buf.String()
	return nil
}

func (s *validateMermaidUnitSteps) theDeveloperRunsDocsValidateMermaidNoArgs() error {
	return s.theDeveloperRunsDocsValidateMermaid()
}

func (s *validateMermaidUnitSteps) theOutputIdentifiesFileUnderPlans() error {
	if s.cmdErr == nil {
		return fmt.Errorf("expected violation, got success; output: %s", s.cmdOutput)
	}
	if !strings.Contains(s.cmdOutput, "plans/") {
		return fmt.Errorf("expected output to mention plans/, got: %s", s.cmdOutput)
	}
	return nil
}

func (s *validateMermaidUnitSteps) theDeveloperRunsDocsValidateMermaidWithMaxLabelLen40() error {
	validateMermaidMaxLabelLen = 40
	buf := new(bytes.Buffer)
	validateMermaidCmd.SetOut(buf)
	validateMermaidCmd.SetErr(buf)
	s.cmdErr = validateMermaidCmd.RunE(validateMermaidCmd, []string{})
	s.cmdOutput = buf.String()
	return nil
}

func (s *validateMermaidUnitSteps) theDeveloperRunsDocsValidateMermaidWithMaxWidth5() error {
	validateMermaidMaxWidth = 5
	buf := new(bytes.Buffer)
	validateMermaidCmd.SetOut(buf)
	validateMermaidCmd.SetErr(buf)
	s.cmdErr = validateMermaidCmd.RunE(validateMermaidCmd, []string{})
	s.cmdOutput = buf.String()
	return nil
}

func (s *validateMermaidUnitSteps) theDeveloperRunsDocsValidateMermaidWithMaxDepth3() error {
	validateMermaidMaxDepth = 3
	buf := new(bytes.Buffer)
	validateMermaidCmd.SetOut(buf)
	validateMermaidCmd.SetErr(buf)
	s.cmdErr = validateMermaidCmd.RunE(validateMermaidCmd, []string{})
	s.cmdOutput = buf.String()
	return nil
}

func (s *validateMermaidUnitSteps) theDeveloperRunsDocsValidateMermaidWithStagedOnlyFlag() error {
	validateMermaidStagedOnly = true
	buf := new(bytes.Buffer)
	validateMermaidCmd.SetOut(buf)
	validateMermaidCmd.SetErr(buf)
	s.cmdErr = validateMermaidCmd.RunE(validateMermaidCmd, []string{})
	s.cmdOutput = buf.String()
	return nil
}

func (s *validateMermaidUnitSteps) theDeveloperRunsDocsValidateMermaidWithChangedOnlyFlag() error {
	validateMermaidChangedOnly = true
	buf := new(bytes.Buffer)
	validateMermaidCmd.SetOut(buf)
	validateMermaidCmd.SetErr(buf)
	s.cmdErr = validateMermaidCmd.RunE(validateMermaidCmd, []string{})
	s.cmdOutput = buf.String()
	return nil
}

func (s *validateMermaidUnitSteps) theDeveloperRunsDocsValidateMermaidWithJSONOutput() error {
	output = "json"
	buf := new(bytes.Buffer)
	validateMermaidCmd.SetOut(buf)
	validateMermaidCmd.SetErr(buf)
	s.cmdErr = validateMermaidCmd.RunE(validateMermaidCmd, []string{})
	s.cmdOutput = buf.String()
	return nil
}

func (s *validateMermaidUnitSteps) theDeveloperRunsDocsValidateMermaidWithMarkdownOutput() error {
	output = "markdown"
	buf := new(bytes.Buffer)
	validateMermaidCmd.SetOut(buf)
	validateMermaidCmd.SetErr(buf)
	s.cmdErr = validateMermaidCmd.RunE(validateMermaidCmd, []string{})
	s.cmdOutput = buf.String()
	return nil
}

func (s *validateMermaidUnitSteps) theDeveloperRunsDocsValidateMermaidWithVerbose() error {
	verbose = true
	buf := new(bytes.Buffer)
	validateMermaidCmd.SetOut(buf)
	validateMermaidCmd.SetErr(buf)
	s.cmdErr = validateMermaidCmd.RunE(validateMermaidCmd, []string{})
	s.cmdOutput = buf.String()
	return nil
}

func (s *validateMermaidUnitSteps) theDeveloperRunsDocsValidateMermaidWithQuiet() error {
	quiet = true
	buf := new(bytes.Buffer)
	validateMermaidCmd.SetOut(buf)
	validateMermaidCmd.SetErr(buf)
	s.cmdErr = validateMermaidCmd.RunE(validateMermaidCmd, []string{})
	s.cmdOutput = buf.String()
	return nil
}

// --- Then steps ---

func (s *validateMermaidUnitSteps) theCommandExitsSuccessfully() error {
	if s.cmdErr != nil {
		return fmt.Errorf("expected success but got: %v\nOutput: %s", s.cmdErr, s.cmdOutput)
	}
	return nil
}

func (s *validateMermaidUnitSteps) theCommandExitsWithAFailureCode() error {
	if s.cmdErr == nil {
		return fmt.Errorf("expected failure but succeeded\nOutput: %s", s.cmdOutput)
	}
	return nil
}

func (s *validateMermaidUnitSteps) theOutputReportsNoViolations() error {
	if s.cmdErr != nil {
		return fmt.Errorf("expected no violations, got error: %v (output: %s)", s.cmdErr, s.cmdOutput)
	}
	return nil
}

func (s *validateMermaidUnitSteps) theOutputIdentifiesFileBlockAndNodeWithOversizedLabel() error {
	if s.cmdErr == nil {
		return fmt.Errorf("expected label-too-long violation error, but command succeeded")
	}
	return nil
}

func (s *validateMermaidUnitSteps) theOutputIdentifiesFileAndBlockWithExcessiveWidth() error {
	if s.cmdErr == nil {
		return fmt.Errorf("expected width-exceeded violation error, but command succeeded")
	}
	return nil
}

func (s *validateMermaidUnitSteps) theOutputIdentifiesFileAndBlockWithMultipleDiagrams() error {
	if s.cmdErr == nil {
		return fmt.Errorf("expected multiple-diagrams violation error, but command succeeded")
	}
	return nil
}

func (s *validateMermaidUnitSteps) theOutputContainsWarningAboutDiagramComplexity() error {
	if s.cmdErr != nil {
		return fmt.Errorf("expected success (warning only), got error: %v\nOutput: %s", s.cmdErr, s.cmdOutput)
	}
	if !strings.Contains(s.cmdOutput, "warning") && !strings.Contains(s.cmdOutput, "complex_diagram") {
		return fmt.Errorf("expected output to mention warning or complex_diagram, got: %s", s.cmdOutput)
	}
	return nil
}

func (s *validateMermaidUnitSteps) theOutputIsValidJSON() error {
	if !json.Valid([]byte(s.cmdOutput)) {
		return fmt.Errorf("expected valid JSON output, got: %s", s.cmdOutput)
	}
	return nil
}

func (s *validateMermaidUnitSteps) theJSONContainsViolationKindFilePathBlockIndexAndNodeID() error {
	var result map[string]interface{}
	if err := json.Unmarshal([]byte(s.cmdOutput), &result); err != nil {
		return fmt.Errorf("failed to parse JSON: %w (output: %s)", err, s.cmdOutput)
	}
	if _, ok := result["violations"]; !ok {
		return fmt.Errorf("JSON missing 'violations' field, got: %s", s.cmdOutput)
	}
	return nil
}

func (s *validateMermaidUnitSteps) theOutputContainsTableWithExpectedColumns() error {
	expected := []string{"File", "Block", "Line", "Severity", "Kind", "Detail"}
	for _, col := range expected {
		if !strings.Contains(s.cmdOutput, col) {
			return fmt.Errorf("expected markdown output to contain column %q, got: %s", col, s.cmdOutput)
		}
	}
	return nil
}

func (s *validateMermaidUnitSteps) theOutputIncludesPerFileScanDetailLines() error {
	if s.cmdErr != nil {
		return fmt.Errorf("expected success with verbose output, got: %v", s.cmdErr)
	}
	// In verbose mode, even clean runs should emit the summary footer.
	if s.cmdOutput == "" {
		return fmt.Errorf("expected non-empty verbose output, got empty string")
	}
	return nil
}

func (s *validateMermaidUnitSteps) theOutputContainsNoText() error {
	if s.cmdOutput != "" {
		return fmt.Errorf("expected empty output in quiet mode, got: %s", s.cmdOutput)
	}
	return nil
}

func TestUnitDocsValidateMermaid(t *testing.T) {
	s := &validateMermaidUnitSteps{}
	suite := godog.TestSuite{
		ScenarioInitializer: func(sc *godog.ScenarioContext) {
			sc.Before(s.before)
			sc.After(s.after)

			// Given steps.
			sc.Step(stepMermaidFileCleanFlowchart, s.aMarkdownFileContainingFlowchartAllLabelsWithinLimit)
			sc.Step(stepMermaidFileLabelTooLong, s.aMarkdownFileContainingFlowchartNodeLabelLongerThanLimit)
			sc.Step(stepMermaidFileNodeLabel35Chars, s.aMarkdownFileContainingFlowchartNodeLabel35Chars)
			sc.Step(stepMermaidFileTBChainedSequentially, s.aMarkdownFileContainingTBFlowchartTenNodesChainedSequentially)
			sc.Step(stepMermaidFileTBNoRankMoreThan3, s.aMarkdownFileContainingTBFlowchartNoRankMoreThan3)
			sc.Step(stepMermaidFileTBOneRank4Nodes, s.aMarkdownFileContainingTBFlowchartOneRank4ParallelNodes)
			sc.Step(stepMermaidFileLRNoRankMoreThan3, s.aMarkdownFileContainingLRFlowchartNoRankMoreThan3)
			sc.Step(stepMermaidFileLR4NodesSameDepth, s.aMarkdownFileContainingLRFlowchart4NodesAtSameDepth)
			sc.Step(stepMermaidFileFlowchart4NodesOneRank, s.aMarkdownFileContainingFlowchart4NodesAtOneRank)
			sc.Step(stepMermaidFile4NodesMoreThan5Ranks, s.aMarkdownFileContainingFlowchart4NodesAtOneRankMoreThan5Ranks)
			sc.Step(stepMermaidFile4NodesExactly4RanksDeep, s.aMarkdownFileContainingFlowchart4NodesAtOneRankExactly4RanksDeep)
			sc.Step(stepMermaidFileSingleFlowchart, s.aMarkdownFileContainingMermaidBlockWithExactlyOneDiagram)
			sc.Step(stepMermaidFileTwoFlowchartDeclarations, s.aMarkdownFileContainingMermaidBlockTwoFlowchartDeclarations)
			sc.Step(stepMermaidFileGraphKeywordNoViolations, s.aMarkdownFileContainingGraphKeywordNoViolations)
			sc.Step(stepMermaidFileOnlyNonFlowchart, s.aMarkdownFileContainingOnlySequenceDiagramAndClassDiagram)
			sc.Step(stepMermaidFileNoMermaidBlocks, s.aMarkdownFileContainingNoMermaidCodeBlocks)
			sc.Step(stepMermaidViolationNotStagedInGit, s.aMarkdownFileWithMermaidViolationNotStagedInGit)
			sc.Step(stepMermaidViolationNotInPushRange, s.aMarkdownFileWithMermaidViolationNotInPushRange)
			sc.Step(stepMermaidFileLabelLengthViolation, s.aMarkdownFileContainingFlowchartWithLabelLengthViolation)
			sc.Step(stepMermaidFileNoViolations, s.aMarkdownFileContainingFlowchartNoViolations)
			sc.Step(stepMermaidFileUnderPlansLongLabel, s.aMarkdownFileUnderPlansLongLabel)

			// When steps.
			sc.Step(stepDeveloperRunsDocsValidateMermaid, s.theDeveloperRunsDocsValidateMermaid)
			sc.Step(stepDeveloperRunsDocsValidateMermaidNoArgs, s.theDeveloperRunsDocsValidateMermaidNoArgs)
			sc.Step(stepDeveloperRunsDocsValidateMermaidMaxLabelLen40, s.theDeveloperRunsDocsValidateMermaidWithMaxLabelLen40)
			sc.Step(stepDeveloperRunsDocsValidateMermaidMaxWidth5, s.theDeveloperRunsDocsValidateMermaidWithMaxWidth5)
			sc.Step(stepDeveloperRunsDocsValidateMermaidMaxDepth3, s.theDeveloperRunsDocsValidateMermaidWithMaxDepth3)
			sc.Step(stepDeveloperRunsDocsValidateMermaidStagedOnly, s.theDeveloperRunsDocsValidateMermaidWithStagedOnlyFlag)
			sc.Step(stepDeveloperRunsDocsValidateMermaidChangedOnly, s.theDeveloperRunsDocsValidateMermaidWithChangedOnlyFlag)
			sc.Step(stepDeveloperRunsDocsValidateMermaidJSONOutput, s.theDeveloperRunsDocsValidateMermaidWithJSONOutput)
			sc.Step(stepDeveloperRunsDocsValidateMermaidMarkdownOutput, s.theDeveloperRunsDocsValidateMermaidWithMarkdownOutput)
			sc.Step(stepDeveloperRunsDocsValidateMermaidVerbose, s.theDeveloperRunsDocsValidateMermaidWithVerbose)
			sc.Step(stepDeveloperRunsDocsValidateMermaidQuiet, s.theDeveloperRunsDocsValidateMermaidWithQuiet)

			// Then steps.
			sc.Step(stepExitsSuccessfully, s.theCommandExitsSuccessfully)
			sc.Step(stepExitsWithFailure, s.theCommandExitsWithAFailureCode)
			sc.Step(stepMermaidOutputNoViolations, s.theOutputReportsNoViolations)
			sc.Step(stepMermaidOutputIdentifiesOversizedLabel, s.theOutputIdentifiesFileBlockAndNodeWithOversizedLabel)
			sc.Step(stepMermaidOutputIdentifiesExcessiveWidth, s.theOutputIdentifiesFileAndBlockWithExcessiveWidth)
			sc.Step(stepMermaidOutputIdentifiesMultipleDiagrams, s.theOutputIdentifiesFileAndBlockWithMultipleDiagrams)
			sc.Step(stepMermaidOutputContainsWarning, s.theOutputContainsWarningAboutDiagramComplexity)
			sc.Step(stepOutputIsValidJSON, s.theOutputIsValidJSON)
			sc.Step(stepMermaidJSONContainsViolationFields, s.theJSONContainsViolationKindFilePathBlockIndexAndNodeID)
			sc.Step(stepMermaidOutputContainsTable, s.theOutputContainsTableWithExpectedColumns)
			sc.Step(stepMermaidOutputIncludesPerFileDetail, s.theOutputIncludesPerFileScanDetailLines)
			sc.Step(stepMermaidOutputContainsNoText, s.theOutputContainsNoText)
			sc.Step(stepMermaidOutputIdentifiesFileUnderPlans, s.theOutputIdentifiesFileUnderPlans)
		},
		Options: &godog.Options{
			Format:   "pretty",
			Paths:    []string{specsDirUnitDocsValidateMermaid},
			TestingT: t,
			Tags:     "@docs-validate-mermaid",
		},
	}
	if suite.Run() != 0 {
		t.Fatal("non-zero status returned, failed to run unit feature tests")
	}
}

// aMarkdownFileContainingMermaidBlockWithExactlyOneDiagram is the step for the
// "a markdown file containing a mermaid code block with exactly one flowchart diagram" scenario.
func (s *validateMermaidUnitSteps) aMarkdownFileContainingMermaidBlockWithExactlyOneDiagram() error {
	docsValidateMermaidFn = func(_ []mermaid.MermaidBlock, _ mermaid.ValidateOptions) mermaid.ValidationResult {
		return mermaid.ValidationResult{}
	}
	return nil
}
