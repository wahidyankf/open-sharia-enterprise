package repogovernance

import (
	"bufio"
	"crypto/sha256"
	"encoding/hex"
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"regexp"
	"sort"
	"strings"
	"time"

	agentspkg "github.com/wahidyankf/ose-public/apps/rhino-cli/internal/agents"
	"github.com/wahidyankf/ose-public/apps/rhino-cli/internal/docs"
)

// AuditCategoryOrder is the canonical, deterministic sequence in which the
// orchestrator invokes the 11 deterministic governance categories. The order
// is fixed so byte-determinism holds across runs and across machines.
var AuditCategoryOrder = []string{
	"agents-md-size",
	"frontmatter-audit",
	"traceability-audit",
	"license-audit",
	"readme-index-audit",
	"emoji-audit",
	"layer-coherence",
	"docs-validate-naming",
	"docs-validate-frontmatter",
	"docs-validate-heading-hierarchy",
	"agents-detect-duplication",
}

// AuditCategoryCommand maps each category name to its human-facing CLI
// invocation, embedded verbatim into the envelope's Categories[].Command field.
var AuditCategoryCommand = map[string]string{
	"agents-md-size":                  "repo-governance agents-md-size",
	"frontmatter-audit":               "repo-governance frontmatter-audit",
	"traceability-audit":              "repo-governance traceability-audit",
	"license-audit":                   "repo-governance license-audit",
	"readme-index-audit":              "repo-governance readme-index-audit",
	"emoji-audit":                     "repo-governance emoji-audit",
	"layer-coherence":                 "repo-governance layer-coherence",
	"docs-validate-naming":            "docs validate-naming",
	"docs-validate-frontmatter":       "docs validate-frontmatter",
	"docs-validate-heading-hierarchy": "docs validate-heading-hierarchy",
	"agents-detect-duplication":       "agents detect-duplication",
}

// AuditOptions parameterizes RunAudit. RepoRoot pins the repository root used
// for the git-sha lookup and all per-category invocations. Skip removes named
// categories from the run; IncludeOnly, when non-empty, restricts the run to
// the listed categories. Now is an injectable clock used for ran_at — tests
// pass a fixed-time function for byte-determinism.
type AuditOptions struct {
	// RepoRoot is the absolute path to the repository root.
	RepoRoot string
	// Skip is the set of category names to omit. Names not in AuditCategoryOrder
	// are silently ignored.
	Skip []string
	// IncludeOnly, when non-empty, restricts the run to the listed categories.
	// Names not in AuditCategoryOrder are silently ignored.
	IncludeOnly []string
	// Now is the clock used to derive RanAt. Defaults to time.Now when nil.
	Now func() time.Time
	// FrontmatterAuditPaths, when non-empty, overrides the default scan paths
	// passed to AuditFrontmatter. Each entry is resolved relative to RepoRoot.
	FrontmatterAuditPaths []string
	// ReadmeIndexAuditPaths, when non-empty, overrides the default scan paths
	// passed to AuditReadmeIndex.
	ReadmeIndexAuditPaths []string
	// EmojiAuditPaths, when non-empty, overrides the default scan paths passed
	// to AuditEmoji.
	EmojiAuditPaths []string
	// DocsValidateNamingPaths, when non-empty, overrides the default scan paths
	// passed to ValidateDocsNaming.
	DocsValidateNamingPaths []string
	// DocsValidateFrontmatterPaths, when non-empty, overrides the default scan
	// paths passed to ValidateDocsFrontmatter.
	DocsValidateFrontmatterPaths []string
	// DocsValidateHeadingHierarchyPaths, when non-empty, overrides the default
	// scan paths passed to ValidateDocsHeadingHierarchy.
	DocsValidateHeadingHierarchyPaths []string
	// KnownFalsePositivesPath, when non-empty, overrides the default skip-list
	// file path (generated-reports/.known-false-positives.md under RepoRoot).
	KnownFalsePositivesPath string
}

// auditDefaultFrontmatterPaths lists the same defaults as the standalone
// frontmatter-audit command for parity.
var auditDefaultFrontmatterPaths = []string{
	"repo-governance/",
	"docs/explanation/software-engineering/",
	".claude/agents/",
	".claude/skills/",
	"plans/",
}

// auditDefaultReadmeIndexPaths lists the same defaults as the standalone
// readme-index-audit command for parity.
var auditDefaultReadmeIndexPaths = []string{
	"repo-governance/",
	".claude/agents/",
	".claude/skills/",
	"docs/explanation/software-engineering/",
}

// auditDefaultEmojiPaths lists the same default as the standalone emoji-audit
// command: walk from the repo root.
var auditDefaultEmojiPaths = []string{"."}

// auditDefaultDocsValidateNamingPaths lists the same defaults as the standalone
// docs validate-naming command for parity.
var auditDefaultDocsValidateNamingPaths = []string{
	"docs/",
	"repo-governance/",
}

// auditDefaultDocsValidateFrontmatterPaths lists the same defaults as the
// standalone docs validate-frontmatter command for parity.
var auditDefaultDocsValidateFrontmatterPaths = []string{
	"docs/explanation/software-engineering/",
	"repo-governance/conventions/",
	"repo-governance/principles/",
	"repo-governance/development/",
	"repo-governance/workflows/",
}

// auditDefaultDocsValidateHeadingHierarchyPaths lists the same defaults as the
// standalone docs validate-heading-hierarchy command for parity.
var auditDefaultDocsValidateHeadingHierarchyPaths = []string{
	"docs/",
	"repo-governance/",
}

// auditCriticalityHigh is the AI-checker-aligned criticality tier emitted by
// every deterministic category. All deterministic findings are HIGH per the
// quality-gate convention; AI-only findings carry CRITICAL/MEDIUM/LOW tiers
// separately.
const auditCriticalityHigh = "HIGH"

// auditSeverityHigh is the wire severity emitted by every deterministic
// category. The internal Finding types use slightly different severity
// vocabularies ("fail", "warn", "high", etc.); the orchestrator normalizes
// every deterministic finding to "high" for the envelope.
const auditSeverityHigh = "high"

// RunAudit executes the 11 deterministic governance audits in fixed order,
// aggregates the findings into an AuditEnvelope, and returns it. The envelope
// is byte-deterministic when opts.Now is a fixed-time function and the
// repository contents are unchanged.
func RunAudit(opts AuditOptions) (AuditEnvelope, error) {
	now := opts.Now
	if now == nil {
		now = time.Now
	}
	ranAt := now().UTC().Format(time.RFC3339)

	skipSet := map[string]struct{}{}
	for _, name := range opts.Skip {
		skipSet[name] = struct{}{}
	}
	includeSet := map[string]struct{}{}
	for _, name := range opts.IncludeOnly {
		includeSet[name] = struct{}{}
	}

	categories := make([]AuditCategoryResult, 0, len(AuditCategoryOrder))
	for _, name := range AuditCategoryOrder {
		if _, skipped := skipSet[name]; skipped {
			continue
		}
		if len(includeSet) > 0 {
			if _, ok := includeSet[name]; !ok {
				continue
			}
		}
		findings, err := runCategory(name, opts)
		if err != nil {
			return AuditEnvelope{}, fmt.Errorf("category %q: %w", name, err)
		}
		sortAuditFindings(findings)
		categories = append(categories, AuditCategoryResult{
			Name:     name,
			Command:  AuditCategoryCommand[name],
			Passed:   len(findings) == 0,
			Findings: findings,
		})
	}

	skipList, err := loadKnownFalsePositives(opts)
	if err != nil {
		return AuditEnvelope{}, fmt.Errorf("load known-false-positives: %w", err)
	}

	categories, skipped := partitionFalsePositives(categories, skipList)

	total := 0
	bySev := map[string]int{}
	byCat := map[string]int{}
	for _, c := range categories {
		total += len(c.Findings)
		byCat[c.Name] = len(c.Findings)
		for _, f := range c.Findings {
			bySev[f.Severity]++
		}
	}

	status := "ok"
	if total > 0 {
		status = "failed"
	}

	gitSHA := readGitSHA(opts.RepoRoot)

	return AuditEnvelope{
		Schema: AuditEnvelopeSchema,
		Status: status,
		Result: AuditResult{
			GitSHA:                gitSHA,
			RanAt:                 ranAt,
			TotalFindings:         total,
			BySeverity:            bySev,
			ByCategory:            byCat,
			Categories:            categories,
			SkippedFalsePositives: skipped,
		},
	}, nil
}

// runCategory dispatches to the per-category function, converts its native
// finding type to the generic AuditFinding shape, and returns the result.
func runCategory(name string, opts AuditOptions) ([]AuditFinding, error) {
	switch name {
	case "agents-md-size":
		return runAgentsMdSizeCategory(opts.RepoRoot)
	case "frontmatter-audit":
		return runFrontmatterAuditCategory(opts)
	case "traceability-audit":
		return runTraceabilityAuditCategory(opts.RepoRoot)
	case "license-audit":
		return runLicenseAuditCategory(opts.RepoRoot)
	case "readme-index-audit":
		return runReadmeIndexAuditCategory(opts)
	case "emoji-audit":
		return runEmojiAuditCategory(opts)
	case "layer-coherence":
		return runLayerCoherenceCategory(opts.RepoRoot)
	case "docs-validate-naming":
		return runDocsValidateNamingCategory(opts)
	case "docs-validate-frontmatter":
		return runDocsValidateFrontmatterCategory(opts)
	case "docs-validate-heading-hierarchy":
		return runDocsValidateHeadingHierarchyCategory(opts)
	case "agents-detect-duplication":
		return runAgentsDetectDuplicationCategory(opts.RepoRoot)
	default:
		return nil, fmt.Errorf("unknown category %q", name)
	}
}

// runAgentsMdSizeCategory runs CheckAgentsMdSize and converts the result.
// A missing AGENTS.md file is treated as a fail-level finding rather than an
// orchestrator error, so an absent file does not abort the rest of the run.
func runAgentsMdSizeCategory(repoRoot string) ([]AuditFinding, error) {
	path := filepath.Join(repoRoot, "AGENTS.md")
	finding, err := CheckAgentsMdSize(path)
	if err != nil {
		if os.IsNotExist(err) {
			return []AuditFinding{newAuditFinding("agents-md-size", path, 0, "AGENTS.md is missing")}, nil
		}
		return nil, err
	}
	if finding.Severity != "fail" {
		return nil, nil
	}
	return []AuditFinding{newAuditFinding("agents-md-size", finding.File, 0, finding.Message)}, nil
}

// runFrontmatterAuditCategory runs AuditFrontmatter against the configured
// paths and projects the per-file findings to AuditFinding.
func runFrontmatterAuditCategory(opts AuditOptions) ([]AuditFinding, error) {
	paths := resolveAuditPaths(opts.RepoRoot, opts.FrontmatterAuditPaths, auditDefaultFrontmatterPaths)
	findings, err := AuditFrontmatter(paths)
	if err != nil {
		return nil, err
	}
	out := make([]AuditFinding, 0, len(findings))
	for _, f := range findings {
		out = append(out, newAuditFinding("frontmatter-audit", f.File, f.Line, f.Message))
	}
	return out, nil
}

// runTraceabilityAuditCategory runs AuditTraceability and projects findings.
func runTraceabilityAuditCategory(repoRoot string) ([]AuditFinding, error) {
	findings, err := AuditTraceability(repoRoot)
	if err != nil {
		return nil, err
	}
	out := make([]AuditFinding, 0, len(findings))
	for _, f := range findings {
		out = append(out, newAuditFinding("traceability-audit", f.Path, f.Line, f.Message))
	}
	return out, nil
}

// runLicenseAuditCategory runs AuditLicense and projects findings.
func runLicenseAuditCategory(repoRoot string) ([]AuditFinding, error) {
	findings, err := AuditLicense(repoRoot)
	if err != nil {
		return nil, err
	}
	out := make([]AuditFinding, 0, len(findings))
	for _, f := range findings {
		out = append(out, newAuditFinding("license-audit", f.Path, 0, f.Message))
	}
	return out, nil
}

// runReadmeIndexAuditCategory runs AuditReadmeIndex and projects findings.
func runReadmeIndexAuditCategory(opts AuditOptions) ([]AuditFinding, error) {
	paths := resolveAuditPaths(opts.RepoRoot, opts.ReadmeIndexAuditPaths, auditDefaultReadmeIndexPaths)
	findings, err := AuditReadmeIndex(paths, nil)
	if err != nil {
		return nil, err
	}
	out := make([]AuditFinding, 0, len(findings))
	for _, f := range findings {
		out = append(out, newAuditFinding("readme-index-audit", f.File, 0, f.Message))
	}
	return out, nil
}

// runEmojiAuditCategory runs AuditEmoji and projects findings.
func runEmojiAuditCategory(opts AuditOptions) ([]AuditFinding, error) {
	paths := resolveAuditPaths(opts.RepoRoot, opts.EmojiAuditPaths, auditDefaultEmojiPaths)
	findings, err := AuditEmoji(paths)
	if err != nil {
		return nil, err
	}
	out := make([]AuditFinding, 0, len(findings))
	for _, f := range findings {
		msg := fmt.Sprintf("forbidden emoji codepoint %s at column %d", f.Codepoint, f.Column)
		out = append(out, newAuditFinding("emoji-audit", f.File, f.Line, msg))
	}
	return out, nil
}

// runLayerCoherenceCategory runs AuditLayerCoherence and projects findings.
func runLayerCoherenceCategory(repoRoot string) ([]AuditFinding, error) {
	findings, err := AuditLayerCoherence(repoRoot)
	if err != nil {
		return nil, err
	}
	out := make([]AuditFinding, 0, len(findings))
	for _, f := range findings {
		out = append(out, newAuditFinding("layer-coherence", f.File, 0, f.Message))
	}
	return out, nil
}

// runDocsValidateNamingCategory runs docs.ValidateDocsNaming and projects findings.
func runDocsValidateNamingCategory(opts AuditOptions) ([]AuditFinding, error) {
	paths := resolveAuditPaths(opts.RepoRoot, opts.DocsValidateNamingPaths, auditDefaultDocsValidateNamingPaths)
	findings, err := docs.ValidateDocsNaming(paths, nil)
	if err != nil {
		return nil, err
	}
	out := make([]AuditFinding, 0, len(findings))
	for _, f := range findings {
		out = append(out, newAuditFinding("docs-validate-naming", f.File, 0, f.Message))
	}
	return out, nil
}

// runDocsValidateFrontmatterCategory runs docs.ValidateDocsFrontmatter and
// projects only the fail-severity findings; warn-level findings are reported
// by the standalone command but do not flip its exit code, so for orchestrator
// parity they are excluded from the deterministic finding count.
func runDocsValidateFrontmatterCategory(opts AuditOptions) ([]AuditFinding, error) {
	paths := resolveAuditPaths(opts.RepoRoot, opts.DocsValidateFrontmatterPaths, auditDefaultDocsValidateFrontmatterPaths)
	findings, err := docs.ValidateDocsFrontmatter(paths)
	if err != nil {
		return nil, err
	}
	out := make([]AuditFinding, 0, len(findings))
	for _, f := range findings {
		if f.Severity != "fail" {
			continue
		}
		out = append(out, newAuditFinding("docs-validate-frontmatter", f.File, 0, f.Message))
	}
	return out, nil
}

// runDocsValidateHeadingHierarchyCategory runs docs.ValidateDocsHeadingHierarchy
// and projects findings.
func runDocsValidateHeadingHierarchyCategory(opts AuditOptions) ([]AuditFinding, error) {
	paths := resolveAuditPaths(opts.RepoRoot, opts.DocsValidateHeadingHierarchyPaths, auditDefaultDocsValidateHeadingHierarchyPaths)
	findings, err := docs.ValidateDocsHeadingHierarchy(paths)
	if err != nil {
		return nil, err
	}
	out := make([]AuditFinding, 0, len(findings))
	for _, f := range findings {
		out = append(out, newAuditFinding("docs-validate-heading-hierarchy", f.File, f.Line, f.Message))
	}
	return out, nil
}

// runAgentsDetectDuplicationCategory runs the agents-package detector and
// projects findings; the duplication finding's "file" is the first member of
// its cross-file cluster (deterministic sort guarantees stability).
func runAgentsDetectDuplicationCategory(repoRoot string) ([]AuditFinding, error) {
	findings, err := agentspkg.DetectDuplication(repoRoot)
	if err != nil {
		return nil, err
	}
	out := make([]AuditFinding, 0, len(findings))
	for _, f := range findings {
		var file string
		var line int
		if len(f.Files) > 0 {
			file = f.Files[0]
		}
		if len(f.StartLines) > 0 {
			line = f.StartLines[0]
		}
		msg := f.Message
		if len(f.Files) > 1 {
			msg = fmt.Sprintf("%s (files: %s)", f.Message, strings.Join(f.Files, ", "))
		}
		out = append(out, newAuditFinding("agents-detect-duplication", file, line, msg))
	}
	return out, nil
}

// resolveAuditPaths returns the absolute scan paths for a category. When
// override is non-empty it wins; otherwise the defaults are used. Each entry
// is joined to repoRoot unless it is already absolute.
func resolveAuditPaths(repoRoot string, override, defaults []string) []string {
	rel := override
	if len(rel) == 0 {
		rel = defaults
	}
	out := make([]string, 0, len(rel))
	for _, p := range rel {
		if filepath.IsAbs(p) {
			out = append(out, p)
		} else {
			out = append(out, filepath.Join(repoRoot, p))
		}
	}
	return out
}

// newAuditFinding builds an AuditFinding with a deterministic Key derived from
// category + file + the first 8 chars of SHA-256(message). Severity and
// Criticality are normalized to the orchestrator-wide constants.
func newAuditFinding(category, file string, line int, message string) AuditFinding {
	return AuditFinding{
		Key:         buildAuditKey(category, file, message),
		Severity:    auditSeverityHigh,
		Criticality: auditCriticalityHigh,
		File:        file,
		Line:        line,
		Message:     message,
	}
}

// buildAuditKey returns "<category>|<file>|<hash-8>" where hash-8 is the first
// 8 hex characters of SHA-256(message). The key is stable across runs as long
// as category, file, and message text are identical.
func buildAuditKey(category, file, message string) string {
	sum := sha256.Sum256([]byte(message))
	short := hex.EncodeToString(sum[:])[:8]
	return fmt.Sprintf("%s|%s|%s", category, file, short)
}

// sortAuditFindings sorts findings in canonical order (File ascending, then
// Line ascending, then Key ascending) so the orchestrator's output is
// byte-deterministic regardless of the upstream category's sort order.
func sortAuditFindings(findings []AuditFinding) {
	sort.SliceStable(findings, func(i, j int) bool {
		if findings[i].File != findings[j].File {
			return findings[i].File < findings[j].File
		}
		if findings[i].Line != findings[j].Line {
			return findings[i].Line < findings[j].Line
		}
		return findings[i].Key < findings[j].Key
	})
}

// knownFalsePositiveLineRe matches the canonical bullet form
// "- `<key>`" (any indentation) anywhere in a markdown bullet line. The
// captured key is the contents of the backtick span.
var knownFalsePositiveLineRe = regexp.MustCompile(`(?m)^\s*-\s+` + "`([^`]+)`")

// loadKnownFalsePositives reads the skip-list markdown file (defaulting to
// generated-reports/.known-false-positives.md under RepoRoot) and returns the
// set of key strings extracted from backtick spans on bullet lines. A missing
// file yields an empty set, not an error.
func loadKnownFalsePositives(opts AuditOptions) (map[string]struct{}, error) {
	path := opts.KnownFalsePositivesPath
	if path == "" {
		path = filepath.Join(opts.RepoRoot, "generated-reports", ".known-false-positives.md")
	}
	f, err := os.Open(path) //nolint:gosec // controlled internal path
	if err != nil {
		if os.IsNotExist(err) {
			return map[string]struct{}{}, nil
		}
		return nil, err
	}
	defer func() { _ = f.Close() }()

	keys := map[string]struct{}{}
	scanner := bufio.NewScanner(f)
	scanner.Buffer(make([]byte, 0, 64*1024), 1024*1024)
	for scanner.Scan() {
		line := scanner.Text()
		m := knownFalsePositiveLineRe.FindStringSubmatch(line)
		if m == nil {
			continue
		}
		keys[m[1]] = struct{}{}
	}
	if err := scanner.Err(); err != nil {
		return nil, err
	}
	return keys, nil
}

// partitionFalsePositives walks every finding under categories and moves the
// ones whose Key is in skipSet to the returned skipped slice. The category's
// Passed flag is recomputed afterward so categories with all findings skipped
// flip to passed=true.
func partitionFalsePositives(categories []AuditCategoryResult, skipSet map[string]struct{}) ([]AuditCategoryResult, []AuditFinding) {
	var skipped []AuditFinding
	for i := range categories {
		kept := make([]AuditFinding, 0, len(categories[i].Findings))
		for _, f := range categories[i].Findings {
			if _, ok := skipSet[f.Key]; ok {
				skipped = append(skipped, f)
				continue
			}
			kept = append(kept, f)
		}
		categories[i].Findings = kept
		categories[i].Passed = len(kept) == 0
	}
	sort.SliceStable(skipped, func(i, j int) bool {
		return skipped[i].Key < skipped[j].Key
	})
	return categories, skipped
}

// readGitSHA shells out to git rev-parse --short HEAD against repoRoot and
// returns the trimmed output, or "unknown" on any failure. The shell-out is
// the only non-deterministic source in RunAudit; tests that need a fixed
// value pass a repo root whose git short SHA is known.
func readGitSHA(repoRoot string) string {
	cmd := exec.Command("git", "-C", repoRoot, "rev-parse", "--short", "HEAD")
	out, err := cmd.Output()
	if err != nil {
		return "unknown"
	}
	return strings.TrimSpace(string(out))
}
