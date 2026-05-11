package cmd

import (
	"encoding/json"
	"fmt"
	"path/filepath"
	"strings"

	"github.com/spf13/cobra"
	"github.com/wahidyankf/ose-public/apps/rhino-cli/internal/cliout"
	governance "github.com/wahidyankf/ose-public/apps/rhino-cli/internal/repo-governance"
)

// emojiAuditFn is the test-mockable entrypoint for the emoji audit.
var emojiAuditFn = governance.AuditEmoji

// emojiAuditPaths holds the --path repeatable flag values.
var emojiAuditPaths []string

// emojiAuditDefaultPaths is the set of repo-relative paths scanned when the
// caller does not pass positional arguments or --path explicitly. The single
// default of "." instructs AuditEmoji to walk from the repo root.
var emojiAuditDefaultPaths = []string{"."}

// emojiAuditSchema is the JSON envelope schema identifier for this command's
// JSON output.
const emojiAuditSchema = "rhino-cli/emoji-audit/v1"

var governanceEmojiAuditCmd = &cobra.Command{
	Use:   "emoji-audit [path...]",
	Short: "Audit forbidden file types for emoji codepoints",
	Long: `Walk every file with a forbidden extension under the given paths and
report any emoji codepoints found. Forbidden extensions cover config files and
source code per the Emoji Convention:

  .json .yaml .yml .toml
  .go .ts .tsx .js .jsx .py .java .kt .rs .fs .cs .dart .exs .ex .clj

Vendored directories (node_modules, .git, .next, dist, build, target) are
skipped entirely.

Natural-language scripts such as Arabic, Hebrew, Cyrillic, and regular CJK
are NOT treated as emoji and do not trigger findings.

When neither positional arguments nor --path flags are supplied, the command
scans from the git root.

Exit codes:
  0 — clean (no findings)
  1 — at least one finding present
  2 — invocation error (cannot find git root, I/O failure, etc.)`,
	Example: `  # Audit the entire repository
  rhino-cli repo-governance emoji-audit

  # Audit a single path explicitly
  rhino-cli repo-governance emoji-audit apps/rhino-cli/

  # Audit multiple paths via repeatable --path flag
  rhino-cli repo-governance emoji-audit --path apps/ --path libs/

  # Emit JSON envelope output
  rhino-cli repo-governance emoji-audit -o json`,
	SilenceErrors: true,
	RunE:          runGovernanceEmojiAudit,
}

func init() {
	governanceEmojiAuditCmd.Flags().StringArrayVarP(
		&emojiAuditPaths,
		"path",
		"p",
		nil,
		"path to scan (repeatable; relative to git root)",
	)
	repoGovernanceCmd.AddCommand(governanceEmojiAuditCmd)
}

// runGovernanceEmojiAudit resolves the active path list, delegates to the
// mockable audit function, and dispatches output through writeFormattedV2.
func runGovernanceEmojiAudit(cmd *cobra.Command, args []string) error {
	repoRoot, err := findGitRoot()
	if err != nil {
		return fmt.Errorf("failed to find git repository root: %w", err)
	}

	relPaths := resolveEmojiAuditPaths(args, emojiAuditPaths)
	fullPaths := make([]string, 0, len(relPaths))
	for _, p := range relPaths {
		if filepath.IsAbs(p) {
			fullPaths = append(fullPaths, p)
		} else {
			fullPaths = append(fullPaths, filepath.Join(repoRoot, p))
		}
	}

	findings, err := emojiAuditFn(fullPaths)
	if err != nil {
		return fmt.Errorf("emoji audit failed: %w", err)
	}

	if err := writeFormattedV2(cmd, verbose, quiet, outputFuncs{
		text:     func(_, _ bool) string { return formatEmojiAuditText(findings) },
		json:     func() (string, error) { return formatEmojiAuditJSON(findings) },
		markdown: func() string { return formatEmojiAuditMarkdown(findings) },
	}); err != nil {
		return err
	}

	if len(findings) > 0 {
		// Reset the flag slice for repeat invocations in tests, then return a
		// non-nil error so the exit code reflects the finding.
		emojiAuditPaths = nil
		return fmt.Errorf("%d emoji finding(s) found", len(findings))
	}
	emojiAuditPaths = nil
	return nil
}

// resolveEmojiAuditPaths picks the active scan path list, preferring
// positional args, then --path flags, then the default set.
func resolveEmojiAuditPaths(args, flagPaths []string) []string {
	switch {
	case len(args) > 0:
		return args
	case len(flagPaths) > 0:
		return flagPaths
	default:
		return emojiAuditDefaultPaths
	}
}

// formatEmojiAuditText returns the human-readable rendering of findings.
func formatEmojiAuditText(findings []governance.EmojiFinding) string {
	if len(findings) == 0 {
		return "EMOJI AUDIT PASSED: no emoji codepoints found in forbidden file types\n"
	}
	var sb strings.Builder
	fmt.Fprintf(&sb, "EMOJI AUDIT FAILED: %d emoji codepoint(s) found\n", len(findings))
	for _, f := range findings {
		fmt.Fprintf(&sb, "  %s:%d:%d  [%s]  %s\n", f.File, f.Line, f.Column, f.Severity, f.Codepoint)
	}
	return sb.String()
}

// emojiAuditJSONFinding is the wire shape for one finding in JSON output.
type emojiAuditJSONFinding struct {
	File      string `json:"file"`
	Line      int    `json:"line"`
	Column    int    `json:"column"`
	Codepoint string `json:"codepoint"`
	Severity  string `json:"severity"`
}

// formatEmojiAuditJSON returns the canonical JSON envelope for findings.
func formatEmojiAuditJSON(findings []governance.EmojiFinding) (string, error) {
	jf := make([]emojiAuditJSONFinding, 0, len(findings))
	for _, f := range findings {
		jf = append(jf, emojiAuditJSONFinding{
			File:      f.File,
			Line:      f.Line,
			Column:    f.Column,
			Codepoint: f.Codepoint,
			Severity:  f.Severity,
		})
	}
	status := "passed"
	if len(findings) > 0 {
		status = "failed"
	}
	env := cliout.Envelope[[]emojiAuditJSONFinding]{
		Schema: emojiAuditSchema,
		Status: status,
		Result: jf,
	}
	data, err := json.MarshalIndent(env, "", "  ")
	if err != nil {
		return "", err
	}
	return string(data) + "\n", nil
}

// formatEmojiAuditMarkdown returns a markdown table of findings.
func formatEmojiAuditMarkdown(findings []governance.EmojiFinding) string {
	if len(findings) == 0 {
		return "## Governance Emoji Audit\n\n**PASSED**: no emoji codepoints found in forbidden file types\n"
	}
	var sb strings.Builder
	fmt.Fprintf(&sb, "## Governance Emoji Audit\n\n**FAILED**: %d emoji codepoint(s) found\n\n", len(findings))
	sb.WriteString("| File | Line | Column | Codepoint | Severity |\n")
	sb.WriteString("|------|------|--------|-----------|----------|\n")
	for _, f := range findings {
		fmt.Fprintf(&sb, "| %s | %d | %d | %s | %s |\n", f.File, f.Line, f.Column, f.Codepoint, f.Severity)
	}
	return sb.String()
}
