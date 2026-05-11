// Package cmd tests the golden output of every rhino-cli subcommand.
//
// TestGolden reads testdata/golden/manifest.yaml, runs each fixture
// in-process via RunE (using the same dependency-injection variables
// that all other unit tests use), captures stdout, and compares the
// result against a persisted golden file at testdata/golden/<name>.stdout.
//
// Seed golden files (required before any behaviour-changing refactor):
//
//	UPDATE_GOLDEN=1 go test ./cmd -run TestGolden
package cmd

import (
	"bytes"
	"fmt"
	"io/fs"
	"os"
	"path/filepath"
	"regexp"
	"runtime"
	"strings"
	"testing"

	"gopkg.in/yaml.v3"

	"github.com/wahidyankf/ose-public/apps/rhino-cli/internal/agents"
	"github.com/wahidyankf/ose-public/apps/rhino-cli/internal/bcregistry"
	"github.com/wahidyankf/ose-public/apps/rhino-cli/internal/docs"
	"github.com/wahidyankf/ose-public/apps/rhino-cli/internal/doctor"
	"github.com/wahidyankf/ose-public/apps/rhino-cli/internal/envbackup"
	"github.com/wahidyankf/ose-public/apps/rhino-cli/internal/glossary"
	"github.com/wahidyankf/ose-public/apps/rhino-cli/internal/governance"
	"github.com/wahidyankf/ose-public/apps/rhino-cli/internal/mermaid"
	"github.com/wahidyankf/ose-public/apps/rhino-cli/internal/naming"
	"github.com/wahidyankf/ose-public/apps/rhino-cli/internal/speccoverage"
	"github.com/wahidyankf/ose-public/apps/rhino-cli/internal/testcoverage"
)

// goldenFixture mirrors the YAML structure of manifest.yaml.
type goldenFixture struct {
	// Name is the unique identifier; golden file is testdata/golden/<name>.stdout.
	Name string `yaml:"name"`
	// Args are positional arguments passed to RunE.
	Args []string `yaml:"args"`
	// ExitOK is true when RunE must return nil, false when it must return an error.
	ExitOK bool `yaml:"exit_ok"`
	// MockID selects which pre-wired mock scenario to activate.
	MockID string `yaml:"mock_id"`
	// CommandRef is a human-readable description (not used by the test).
	CommandRef string `yaml:"command_ref"`
}

// goldenManifest is the top-level manifest structure.
type goldenManifest struct {
	Fixtures []goldenFixture `yaml:"fixtures"`
}

// goldenDir returns the path to testdata/golden/ relative to this file.
var goldenDir = func() string {
	_, f, _, _ := runtime.Caller(0)
	return filepath.Join(filepath.Dir(f), "testdata", "golden")
}()

// TestGolden runs every fixture in manifest.yaml and compares output to golden files.
func TestGolden(t *testing.T) {
	t.Helper()

	update := os.Getenv("UPDATE_GOLDEN") == "1"

	// Load manifest.
	manifestPath := filepath.Join(goldenDir, "manifest.yaml")
	manifestData, err := os.ReadFile(manifestPath) //nolint:gosec
	if err != nil {
		t.Fatalf("golden: cannot read manifest: %v", err)
	}
	var manifest goldenManifest
	if err := yaml.Unmarshal(manifestData, &manifest); err != nil {
		t.Fatalf("golden: cannot parse manifest: %v", err)
	}
	if len(manifest.Fixtures) == 0 {
		t.Fatal("golden: manifest.yaml has no fixtures")
	}

	for _, fix := range manifest.Fixtures {
		t.Run(fix.Name, func(t *testing.T) {
			runGoldenFixture(t, fix, update)
		})
	}
}

// runGoldenFixture executes one fixture scenario.
func runGoldenFixture(t *testing.T, fix goldenFixture, update bool) {
	t.Helper()

	// Reset all package-level state before each fixture.
	resetGoldenState()

	// Activate the mock scenario.
	activateGoldenMock(t, fix.MockID)

	// Capture output.
	buf := new(bytes.Buffer)

	// Run the appropriate cobra command.
	runErr := dispatchGoldenCommand(t, fix, buf)

	// Check exit expectation.
	if fix.ExitOK && runErr != nil {
		t.Errorf("expected nil error but got: %v\nOutput: %s", runErr, buf.String())
	}
	if !fix.ExitOK && runErr == nil {
		t.Errorf("expected non-nil error but got nil\nOutput: %s", buf.String())
	}

	got := normaliseGoldenOutput(buf.String())

	goldenFile := filepath.Join(goldenDir, fix.Name+".stdout")

	if update {
		if err := os.MkdirAll(goldenDir, 0o755); err != nil {
			t.Fatalf("golden: cannot create dir: %v", err)
		}
		if err := os.WriteFile(goldenFile, []byte(got), 0o644); err != nil {
			t.Fatalf("golden: cannot write %s: %v", goldenFile, err)
		}
		t.Logf("golden: updated %s", goldenFile)
		return
	}

	// Compare with persisted golden.
	wantBytes, err := os.ReadFile(goldenFile) //nolint:gosec
	if err != nil {
		if os.IsNotExist(err) {
			t.Errorf("golden file %s does not exist — run UPDATE_GOLDEN=1 go test ./cmd -run TestGolden to seed it", goldenFile)
			return
		}
		t.Fatalf("golden: cannot read %s: %v", goldenFile, err)
	}
	want := string(wantBytes)
	if got != want {
		t.Errorf("golden output mismatch for %s:\nGOT:\n%s\nWANT:\n%s", fix.Name, got, want)
	}
}

// rfc3339Re matches RFC 3339 / ISO 8601 timestamp strings in output.
// Used to replace wall-clock timestamps with a stable placeholder.
var rfc3339Re = regexp.MustCompile(`\d{4}-\d{2}-\d{2}T\d{2}:\d{2}:\d{2}(?:\.\d+)?(?:Z|[+-]\d{2}:\d{2})`)

// normaliseGoldenOutput replaces content that varies between runs (such as
// timestamps) with stable placeholders so golden comparisons are deterministic.
func normaliseGoldenOutput(s string) string {
	// Replace ISO 8601 / RFC 3339 timestamps, e.g. "2026-05-11T11:26:22+07:00".
	return rfc3339Re.ReplaceAllString(s, "TIMESTAMP")
}

// resetGoldenState resets all package-level flag and injectable variables to
// their clean defaults.
func resetGoldenState() {
	// Global flags.
	verbose = false
	quiet = false
	output = "text"
	noColor = false
	sayMsg = ""

	// doctor flags.
	scope = "full"
	fix = false
	dryRun = false

	// test-coverage flags.
	perFile = false
	belowThreshold = 0
	excludePatterns = nil

	// test-coverage merge flags.
	mergeOutFile = ""
	mergeValidate = ""
	mergeExcludePatterns = nil

	// test-coverage diff flags.
	diffBase = "main"
	diffThreshold = 0
	diffStaged = false
	diffPerFile = false
	diffExcludePatterns = nil

	// agents flags.
	agentsOnly = false
	skillsOnly = false
	syncDryRun = false
	syncAgentsOnly = false
	syncSkillsOnly = false

	// ddd flags.
	bcSeverity = ""
	ulSeverity = ""

	// env flags — force=true avoids interactive TTY prompts in tests.
	envBackupDir = ""
	envBackupWorktreeAware = false
	envBackupForce = true
	envBackupIncludeConfig = false
	envRestoreDir = ""
	envRestoreWorktreeAware = false
	envRestoreForce = true
	envRestoreIncludeConfig = false
	envInitForce = false

	// mermaid flags.
	validateMermaidStagedOnly = false
	validateMermaidChangedOnly = false
	validateMermaidMaxLabelLen = 30
	validateMermaidMaxWidth = 4
	validateMermaidMaxDepth = 0
	validateMermaidMaxSubgraphNodes = 6

	// spec-coverage flags.
	sharedSteps = false
	excludeDirs = nil

	// specs validate flags.
	specsValidateTreeAppsFlag = nil
	specsValidateCountsApps = nil
	specsValidateLinksApps = nil
	specsValidateAdoptionAppsFlag = nil
	validateDocsLinksStagedOnly = false

	// Mock findGitRoot for all tests.
	osGetwd = func() (string, error) { return "/mock-repo", nil }
	osStat = func(name string) (os.FileInfo, error) {
		if name == "/mock-repo/.git" {
			return &mockFileInfo{name: ".git", isDir: true}, nil
		}
		return nil, os.ErrNotExist
	}

	// Restore all real implementations (mocks will override per scenario).
	doctorCheckAllFn = doctor.CheckAll
	doctorFixAllFn = doctor.FixAll
	agentsSyncAllFn = agents.SyncAll
	agentsValidateClaudeFn = agents.ValidateClaude
	agentsValidateSyncFn = agents.ValidateSync
	agentsValidateNamingFn = agentsValidateNaming
	docsValidateAllLinksFn = docs.ValidateAllLinks
	specCoverageCheckAllFn = speccoverage.CheckAll
	testCoverageComputeLCOVResultFn = testcoverage.ComputeLCOVResult
	testCoverageComputeJaCoCoResultFn = testcoverage.ComputeJaCoCoResult
	testCoverageComputeCoberturaResultFn = testcoverage.ComputeCoberturaResult
	testCoverageComputeGoResultFn = testcoverage.ComputeGoResult
	testCoverageComputeDiffCoverageFn = testcoverage.ComputeDiffCoverage
	testCoverageToCoverageMapFn = testcoverage.ToCoverageMap
	envBackupFn = envbackup.Backup
	envRestoreFn = envbackup.Restore
	confirmFn = envbackup.DefaultConfirmFn
	docsValidateMermaidFn = mermaid.ValidateBlocks
	bcValidateAllFn = bcregistry.ValidateAll
	ulValidateAllFn = glossary.ValidateAll
	specsValidateTreeFn = validateSpecTree
	specsValidateCountsFn = validateSpecCounts
	specsValidateLinksFn = validateSpecLinks
	specsValidateAdoptionFn = validateSpecAdoption
	specsWalkMdFilesFn = walkMdFiles
	specsWalkFeatureFilesFn = walkFeatureFiles
	specCountNonReadmeMdFilesFn = countNonReadmeMdFiles
	governanceVendorAuditFn = governanceVendorAudit
	workflowsValidateNamingFn = workflowsValidateNaming
	readFileFn = os.ReadFile
	getMermaidStagedFilesFn = getMermaidStagedFiles
	getMermaidChangedFilesFn = getMermaidChangedFiles
	readDirFn = os.ReadDir
}

// dispatchGoldenCommand routes to the correct cobra command's RunE based on mock_id prefix.
func dispatchGoldenCommand(t *testing.T, fix goldenFixture, buf *bytes.Buffer) error {
	t.Helper()

	id := fix.MockID

	switch {
	case strings.HasPrefix(id, "doctor_"):
		doctorCmd.SetOut(buf)
		doctorCmd.SetErr(buf)
		return doctorCmd.RunE(doctorCmd, fix.Args)

	case strings.HasPrefix(id, "tc_validate_"):
		validateTestCoverageCmd.SetOut(buf)
		validateTestCoverageCmd.SetErr(buf)
		return validateTestCoverageCmd.RunE(validateTestCoverageCmd, fix.Args)

	case strings.HasPrefix(id, "tc_merge_"):
		mergeTestCoverageCmd.SetOut(buf)
		mergeTestCoverageCmd.SetErr(buf)
		return mergeTestCoverageCmd.RunE(mergeTestCoverageCmd, fix.Args)

	case strings.HasPrefix(id, "tc_diff_"):
		diffTestCoverageCmd.SetOut(buf)
		diffTestCoverageCmd.SetErr(buf)
		return diffTestCoverageCmd.RunE(diffTestCoverageCmd, fix.Args)

	case strings.HasPrefix(id, "spec_coverage_"):
		validateSpecCoverageCmd.SetOut(buf)
		validateSpecCoverageCmd.SetErr(buf)
		return validateSpecCoverageCmd.RunE(validateSpecCoverageCmd, fix.Args)

	case strings.HasPrefix(id, "docs_links_"):
		validateDocsLinksCmd.SetOut(buf)
		validateDocsLinksCmd.SetErr(buf)
		return validateDocsLinksCmd.RunE(validateDocsLinksCmd, fix.Args)

	case strings.HasPrefix(id, "docs_mermaid_"):
		validateMermaidCmd.SetOut(buf)
		validateMermaidCmd.SetErr(buf)
		return validateMermaidCmd.RunE(validateMermaidCmd, fix.Args)

	case strings.HasPrefix(id, "agents_validate_claude_"):
		validateClaudeCmd.SetOut(buf)
		validateClaudeCmd.SetErr(buf)
		return validateClaudeCmd.RunE(validateClaudeCmd, fix.Args)

	case strings.HasPrefix(id, "agents_validate_naming_"):
		agentsValidateNamingCmd.SetOut(buf)
		agentsValidateNamingCmd.SetErr(buf)
		return agentsValidateNamingCmd.RunE(agentsValidateNamingCmd, fix.Args)

	case strings.HasPrefix(id, "agents_validate_sync_"):
		validateSyncCmd.SetOut(buf)
		validateSyncCmd.SetErr(buf)
		return validateSyncCmd.RunE(validateSyncCmd, fix.Args)

	case strings.HasPrefix(id, "agents_sync_"):
		syncAgentsCmd.SetOut(buf)
		syncAgentsCmd.SetErr(buf)
		return syncAgentsCmd.RunE(syncAgentsCmd, fix.Args)

	case strings.HasPrefix(id, "ddd_bc_"):
		dddBcCmd.SetOut(buf)
		dddBcCmd.SetErr(buf)
		return dddBcCmd.RunE(dddBcCmd, fix.Args)

	case strings.HasPrefix(id, "ddd_ul_"):
		dddUlCmd.SetOut(buf)
		dddUlCmd.SetErr(buf)
		return dddUlCmd.RunE(dddUlCmd, fix.Args)

	case strings.HasPrefix(id, "env_backup_"):
		envBackupCmd.SetOut(buf)
		envBackupCmd.SetErr(buf)
		return envBackupCmd.RunE(envBackupCmd, fix.Args)

	case strings.HasPrefix(id, "env_restore_"):
		envRestoreCmd.SetOut(buf)
		envRestoreCmd.SetErr(buf)
		return envRestoreCmd.RunE(envRestoreCmd, fix.Args)

	case strings.HasPrefix(id, "env_init_"):
		envInitCmd.SetOut(buf)
		envInitCmd.SetErr(buf)
		return envInitCmd.RunE(envInitCmd, fix.Args)

	case strings.HasPrefix(id, "governance_vendor_audit_"):
		governanceVendorAuditCmd.SetOut(buf)
		governanceVendorAuditCmd.SetErr(buf)
		return governanceVendorAuditCmd.RunE(governanceVendorAuditCmd, fix.Args)

	case strings.HasPrefix(id, "specs_validate_tree_"):
		specsValidateTreeCmd.SetOut(buf)
		specsValidateTreeCmd.SetErr(buf)
		return specsValidateTreeCmd.RunE(specsValidateTreeCmd, fix.Args)

	case strings.HasPrefix(id, "specs_validate_counts_"):
		specsValidateCountsCmd.SetOut(buf)
		specsValidateCountsCmd.SetErr(buf)
		return specsValidateCountsCmd.RunE(specsValidateCountsCmd, fix.Args)

	case strings.HasPrefix(id, "specs_validate_links_"):
		specsValidateLinksCmd.SetOut(buf)
		specsValidateLinksCmd.SetErr(buf)
		return specsValidateLinksCmd.RunE(specsValidateLinksCmd, fix.Args)

	case strings.HasPrefix(id, "specs_validate_adoption_"):
		specsValidateAdoptionCmd.SetOut(buf)
		specsValidateAdoptionCmd.SetErr(buf)
		return specsValidateAdoptionCmd.RunE(specsValidateAdoptionCmd, fix.Args)

	case strings.HasPrefix(id, "workflows_validate_naming_"):
		workflowsValidateNamingCmd.SetOut(buf)
		workflowsValidateNamingCmd.SetErr(buf)
		return workflowsValidateNamingCmd.RunE(workflowsValidateNamingCmd, fix.Args)

	default:
		t.Fatalf("golden: unknown mock_id %q — add a case to dispatchGoldenCommand", id)
		return nil
	}
}

// activateGoldenMock sets up the injectable function variables for each mock scenario.
//
//nolint:cyclop,funlen // long switch is intentional — one case per scenario.
func activateGoldenMock(t *testing.T, mockID string) {
	t.Helper()

	switch mockID {
	// ── doctor ─────────────────────────────────────────────────────────────
	case "doctor_all_ok":
		doctorCheckAllFn = func(_ doctor.CheckOptions) (*doctor.DoctorResult, error) {
			return &doctor.DoctorResult{OKCount: 3, Checks: makeAllOKChecks(3)}, nil
		}

	case "doctor_all_ok_json":
		output = "json"
		doctorCheckAllFn = func(_ doctor.CheckOptions) (*doctor.DoctorResult, error) {
			return &doctor.DoctorResult{OKCount: 3, Checks: makeAllOKChecks(3)}, nil
		}

	case "doctor_all_ok_markdown":
		output = "markdown"
		doctorCheckAllFn = func(_ doctor.CheckOptions) (*doctor.DoctorResult, error) {
			return &doctor.DoctorResult{OKCount: 3, Checks: makeAllOKChecks(3)}, nil
		}

	case "doctor_missing_tool":
		doctorCheckAllFn = func(_ doctor.CheckOptions) (*doctor.DoctorResult, error) {
			return &doctor.DoctorResult{
				MissingCount: 1,
				OKCount:      2,
				Checks: append(makeAllOKChecks(2), doctor.ToolCheck{
					Name:   "golang",
					Binary: "go",
					Status: doctor.StatusMissing{},
					Note:   "not found in PATH",
				}),
			}, nil
		}

	// ── test-coverage validate ──────────────────────────────────────────────
	case "tc_validate_go_pass":
		testCoverageComputeGoResultFn = func(_ string, threshold float64) (testcoverage.Result, error) {
			return testcoverage.Result{
				File: "/mock-repo/cover.out", Format: testcoverage.FormatGo{},
				Covered: 17, Missed: 3, Total: 20, Pct: 85.0, Threshold: threshold, Passed: true,
			}, nil
		}

	case "tc_validate_go_fail":
		testCoverageComputeGoResultFn = func(_ string, threshold float64) (testcoverage.Result, error) {
			return testcoverage.Result{
				File: "/mock-repo/cover.out", Format: testcoverage.FormatGo{},
				Covered: 17, Missed: 3, Total: 20, Pct: 85.0, Threshold: threshold, Passed: false,
			}, nil
		}

	case "tc_validate_lcov_pass":
		testCoverageComputeLCOVResultFn = func(_ string, threshold float64) (testcoverage.Result, error) {
			return testcoverage.Result{
				File: "/mock-repo/lcov.info", Format: testcoverage.FormatLCOV{},
				Covered: 17, Missed: 3, Total: 20, Pct: 85.0, Threshold: threshold, Passed: true,
			}, nil
		}

	case "tc_validate_jacoco_pass":
		testCoverageComputeJaCoCoResultFn = func(_ string, threshold float64) (testcoverage.Result, error) {
			return testcoverage.Result{
				File: "/mock-repo/jacoco.xml", Format: testcoverage.FormatJaCoCo{},
				Covered: 17, Missed: 3, Total: 20, Pct: 85.0, Threshold: threshold, Passed: true,
			}, nil
		}

	case "tc_validate_cobertura_pass":
		testCoverageComputeCoberturaResultFn = func(_ string, threshold float64) (testcoverage.Result, error) {
			return testcoverage.Result{
				File: "/mock-repo/cobertura.xml", Format: testcoverage.FormatCobertura{},
				Covered: 17, Missed: 3, Total: 20, Pct: 85.0, Threshold: threshold, Passed: true,
			}, nil
		}

	// ── test-coverage merge ─────────────────────────────────────────────────
	case "tc_merge_two_lcov":
		testCoverageToCoverageMapFn = func(_ string) (testcoverage.CoverageMap, error) {
			return testcoverage.CoverageMap{
				"src/main.go": {
					10: {HitCount: 5},
					11: {HitCount: 3},
				},
			}, nil
		}

	// ── test-coverage diff ──────────────────────────────────────────────────
	case "tc_diff_no_threshold":
		testCoverageComputeDiffCoverageFn = func(opts testcoverage.DiffCoverageOptions) (testcoverage.Result, error) {
			return testcoverage.Result{
				Format: testcoverage.FormatDiff{}, Pct: 100.0, Threshold: opts.Threshold,
				Passed: true, Total: 5, Covered: 5,
			}, nil
		}

	case "tc_diff_per_file":
		diffPerFile = true
		testCoverageComputeDiffCoverageFn = func(opts testcoverage.DiffCoverageOptions) (testcoverage.Result, error) {
			return testcoverage.Result{
				Format: testcoverage.FormatDiff{}, Pct: 80.0, Threshold: opts.Threshold,
				Passed: true, Total: 10, Covered: 8, Missed: 2,
			}, nil
		}

	// ── spec-coverage validate ──────────────────────────────────────────────
	case "spec_coverage_pass":
		specCoverageCheckAllFn = func(_ speccoverage.ScanOptions) (*speccoverage.CheckResult, error) {
			return &speccoverage.CheckResult{TotalSpecs: 3, TotalScenarios: 5}, nil
		}

	case "spec_coverage_fail":
		specCoverageCheckAllFn = func(_ speccoverage.ScanOptions) (*speccoverage.CheckResult, error) {
			return &speccoverage.CheckResult{
				TotalSpecs: 3, TotalScenarios: 5,
				Gaps: []speccoverage.CoverageGap{{SpecFile: "login.feature"}},
			}, nil
		}

	// ── docs validate-links ─────────────────────────────────────────────────
	case "docs_links_clean":
		docsValidateAllLinksFn = func(_ docs.ScanOptions) (*docs.LinkValidationResult, error) {
			return &docs.LinkValidationResult{TotalFiles: 5, TotalLinks: 20, BrokenLinks: nil}, nil
		}

	case "docs_links_broken":
		docsValidateAllLinksFn = func(_ docs.ScanOptions) (*docs.LinkValidationResult, error) {
			return &docs.LinkValidationResult{
				TotalFiles: 5, TotalLinks: 20,
				BrokenLinks: []docs.BrokenLink{
					{SourceFile: "docs/guide.md", LinkText: "./missing.md", Category: "internal"},
				},
			}, nil
		}

	// ── docs validate-mermaid ───────────────────────────────────────────────
	case "docs_mermaid_clean":
		// Return empty file list so no blocks are validated.
		getMermaidStagedFilesFn = func(_ string) ([]string, error) { return nil, nil }
		getMermaidChangedFilesFn = func(_ string) ([]string, error) { return nil, nil }
		readFileFn = func(_ string) ([]byte, error) { return nil, fmt.Errorf("no files") }
		docsValidateMermaidFn = func(_ []mermaid.MermaidBlock, _ mermaid.ValidateOptions) mermaid.ValidationResult {
			return mermaid.ValidationResult{FilesScanned: 0, BlocksScanned: 0}
		}

	case "docs_mermaid_violations":
		getMermaidStagedFilesFn = func(_ string) ([]string, error) { return nil, nil }
		getMermaidChangedFilesFn = func(_ string) ([]string, error) { return nil, nil }
		readFileFn = func(_ string) ([]byte, error) { return nil, fmt.Errorf("no files") }
		docsValidateMermaidFn = func(_ []mermaid.MermaidBlock, _ mermaid.ValidateOptions) mermaid.ValidationResult {
			return mermaid.ValidationResult{
				FilesScanned: 1, BlocksScanned: 1,
				Violations: []mermaid.Violation{
					{
						Kind:        mermaid.ViolationLabelTooLong{},
						FilePath:    "docs/diagram.md",
						BlockIndex:  0,
						LabelText:   "this label is way too long for any diagram",
						LabelLen:    43,
						MaxLabelLen: 30,
					},
				},
			}
		}

	// ── agents validate-claude ──────────────────────────────────────────────
	case "agents_validate_claude_pass":
		agentsValidateClaudeFn = func(_ agents.ValidateClaudeOptions) (*agents.ValidationResult, error) {
			return &agents.ValidationResult{
				TotalChecks: 5, PassedChecks: 5, FailedChecks: 0,
				Checks: makeAllPassedChecks(5),
			}, nil
		}

	case "agents_validate_claude_fail":
		agentsValidateClaudeFn = func(_ agents.ValidateClaudeOptions) (*agents.ValidationResult, error) {
			return &agents.ValidationResult{
				TotalChecks: 5, PassedChecks: 4, FailedChecks: 1,
				Checks: append(makeAllPassedChecks(4), agents.ValidationCheck{
					Name:    "agent-format",
					Status:  "failed",
					Message: "missing required field: description",
				}),
			}, nil
		}

	// ── agents validate-naming ──────────────────────────────────────────────
	case "agents_validate_naming_pass":
		agentsValidateNamingFn = func(_ string) ([]naming.Violation, error) {
			return nil, nil
		}

	case "agents_validate_naming_violations":
		agentsValidateNamingFn = func(_ string) ([]naming.Violation, error) {
			return []naming.Violation{
				{Path: ".claude/agents/bad_name.md", Kind: "role-suffix", Message: "filename must end with a valid role suffix"},
			}, nil
		}

	// ── agents validate-sync ────────────────────────────────────────────────
	case "agents_validate_sync_pass":
		agentsValidateSyncFn = func(_ string) (*agents.ValidationResult, error) {
			return &agents.ValidationResult{
				TotalChecks: 3, PassedChecks: 3, FailedChecks: 0,
				Checks: makeAllPassedChecks(3),
			}, nil
		}

	case "agents_validate_sync_drift":
		agentsValidateSyncFn = func(_ string) (*agents.ValidationResult, error) {
			return &agents.ValidationResult{
				TotalChecks: 3, PassedChecks: 2, FailedChecks: 1,
				Checks: append(makeAllPassedChecks(2), agents.ValidationCheck{
					Name:    "agent-sync",
					Status:  "failed",
					Message: "description mismatch for plan-maker",
				}),
			}, nil
		}

	// ── agents sync ─────────────────────────────────────────────────────────
	case "agents_sync_dry_run":
		syncDryRun = true
		agentsSyncAllFn = func(_ agents.SyncOptions) (*agents.SyncResult, error) {
			return &agents.SyncResult{AgentsConverted: 2}, nil
		}

	// ── ddd bc ──────────────────────────────────────────────────────────────
	case "ddd_bc_no_findings":
		bcValidateAllFn = func(_ bcregistry.ValidateOptions) ([]bcregistry.Finding, error) {
			return nil, nil
		}

	case "ddd_bc_with_findings":
		bcValidateAllFn = func(_ bcregistry.ValidateOptions) ([]bcregistry.Finding, error) {
			return []bcregistry.Finding{
				{File: "apps/organiclever/contexts/journal", Severity: "error", Message: "missing layer: infrastructure"},
			}, nil
		}

	case "ddd_bc_severity_warn":
		bcSeverity = "warn"
		bcValidateAllFn = func(_ bcregistry.ValidateOptions) ([]bcregistry.Finding, error) {
			return []bcregistry.Finding{
				{File: "apps/organiclever/contexts/journal", Severity: "warn", Message: "missing layer: infrastructure"},
			}, nil
		}

	// ── ddd ul ──────────────────────────────────────────────────────────────
	case "ddd_ul_no_findings":
		ulValidateAllFn = func(_ glossary.ValidateOptions) ([]glossary.Finding, error) {
			return nil, nil
		}

	case "ddd_ul_with_findings":
		ulValidateAllFn = func(_ glossary.ValidateOptions) ([]glossary.Finding, error) {
			return []glossary.Finding{
				{File: "specs/apps/organiclever/ddd/glossary.md", Severity: "error", Message: "missing term header"},
			}, nil
		}

	case "ddd_ul_severity_warn":
		ulSeverity = "warn"
		ulValidateAllFn = func(_ glossary.ValidateOptions) ([]glossary.Finding, error) {
			return []glossary.Finding{
				{File: "specs/apps/organiclever/ddd/glossary.md", Severity: "warn", Message: "missing term header"},
			}, nil
		}

	// ── env backup ──────────────────────────────────────────────────────────
	case "env_backup_success":
		envBackupFn = func(_ envbackup.Options) (*envbackup.Result, error) {
			return &envbackup.Result{
				Direction: "backup",
				Dir:       "/tmp/test-env-backup",
				Files: []envbackup.FileEntry{
					{RelPath: ".env", Size: 120},
					{RelPath: "apps/web/.env", Size: 80},
				},
				Copied: 2,
			}, nil
		}

	// ── env restore ─────────────────────────────────────────────────────────
	case "env_restore_success":
		envRestoreFn = func(_ envbackup.Options) (*envbackup.Result, error) {
			return &envbackup.Result{
				Direction: "restore",
				Dir:       "/tmp/test-env-backup",
				Files: []envbackup.FileEntry{
					{RelPath: ".env", Size: 120},
				},
				Copied: 1,
			}, nil
		}

	// ── env init ────────────────────────────────────────────────────────────
	case "env_init_no_examples":
		// env init walks infra/dev/ — override to return no entries.
		envInitWalkDir = func(_ string, _ fs.WalkDirFunc) error {
			return nil
		}

	// ── governance vendor-audit ─────────────────────────────────────────────
	case "governance_vendor_audit_clean":
		governanceVendorAuditFn = func(_ string) ([]governance.Finding, error) {
			return nil, nil
		}

	case "governance_vendor_audit_violations":
		governanceVendorAuditFn = func(_ string) ([]governance.Finding, error) {
			return []governance.Finding{
				{Path: "governance/foo.md", Line: 5, Match: "Claude Code", Replacement: `"the coding agent"`},
			}, nil
		}

	// ── specs validate-tree ─────────────────────────────────────────────────
	case "specs_validate_tree_pass":
		specsValidateTreeFn = func(_, _ string) []SpecFinding { return nil }

	case "specs_validate_tree_fail":
		specsValidateTreeFn = func(_, app string) []SpecFinding {
			return []SpecFinding{
				{
					Category: "tree-shape", Criticality: "HIGH",
					File:     fmt.Sprintf("specs/apps/%s", app),
					Evidence: "missing required folder: behavior",
					Expected: fmt.Sprintf("create specs/apps/%s/behavior/ with README.md", app),
				},
			}
		}

	// ── specs validate-counts ───────────────────────────────────────────────
	case "specs_validate_counts_pass":
		specsValidateCountsFn = func(_, _ string) []SpecFinding { return nil }

	case "specs_validate_counts_fail":
		specsValidateCountsFn = func(_, folder string) []SpecFinding {
			return []SpecFinding{
				{
					Category: "count", Criticality: "MEDIUM",
					File:     fmt.Sprintf("%s/product", folder),
					Evidence: "empty subfolder: product contains no spec files",
					Expected: "add at least one non-README .md spec file",
				},
			}
		}

	// ── specs validate-links ────────────────────────────────────────────────
	case "specs_validate_links_pass":
		specsValidateLinksFn = func(_, _ string) []SpecFinding { return nil }

	case "specs_validate_links_fail":
		specsValidateLinksFn = func(_, app string) []SpecFinding {
			return []SpecFinding{
				{
					Category: "link", Criticality: "HIGH",
					File:     fmt.Sprintf("specs/apps/%s/product/prd.md", app),
					Evidence: "broken link: ./missing.md",
					Expected: "fix or remove the broken link",
				},
			}
		}

	// ── specs validate-adoption ─────────────────────────────────────────────
	case "specs_validate_adoption_pass":
		specsValidateAdoptionFn = func(_, _ string) []SpecFinding { return nil }

	case "specs_validate_adoption_fail":
		specsValidateAdoptionFn = func(_, app string) []SpecFinding {
			return []SpecFinding{
				{
					Category: "adoption", Criticality: "HIGH",
					File:     fmt.Sprintf("specs/apps/%s/behavior", app),
					Evidence: "no .feature file found",
					Expected: "add at least one .feature file under behavior/",
				},
			}
		}

	// ── workflows validate-naming ───────────────────────────────────────────
	case "workflows_validate_naming_pass":
		workflowsValidateNamingFn = func(_ string) ([]naming.Violation, error) {
			return nil, nil
		}

	case "workflows_validate_naming_violations":
		workflowsValidateNamingFn = func(_ string) ([]naming.Violation, error) {
			return []naming.Violation{
				{Path: "governance/workflows/bad.md", Kind: "type-suffix", Message: "filename must end with a valid type suffix"},
			}, nil
		}

	default:
		t.Fatalf("golden: unknown mock_id %q — add a case to activateGoldenMock", mockID)
	}
}
