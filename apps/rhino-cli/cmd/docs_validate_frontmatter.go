package cmd

import (
	"encoding/json"
	"fmt"
	"io"
	"path/filepath"
	"strings"

	"github.com/spf13/cobra"
	"github.com/wahidyankf/ose-public/apps/rhino-cli/internal/cliout"
	"github.com/wahidyankf/ose-public/apps/rhino-cli/internal/docs"
)

// docsValidateFrontmatterFn is the test-mockable entrypoint for the docs
// frontmatter validator. Tests assign a stub to short-circuit filesystem
// walking; production code delegates to the internal package.
var docsValidateFrontmatterFn = docs.ValidateDocsFrontmatter

// docsValidateFrontmatterDefaultPaths is the repo-relative path set scanned
// when the caller passes no positional args. Both areas the validator knows
// about (software-engineering docs and governance docs) are rooted under
// these two paths.
var docsValidateFrontmatterDefaultPaths = []string{
	"docs/",
	"repo-governance/",
}

// docsValidateFrontmatterSchema is the JSON envelope schema identifier
// emitted by the --output json branch of this command.
const docsValidateFrontmatterSchema = "rhino-cli/docs-validate-frontmatter/v1"

var docsValidateFrontmatterCmd = &cobra.Command{
	Use:   "validate-frontmatter [path...]",
	Short: "Validate documentation YAML frontmatter against area-specific schemas",
	Long: `Walk every .md file under the given paths and validate its YAML
frontmatter against the schema for its content area:

  - Software-engineering docs (under docs/explanation/software-engineering/)
    require: title, description, category (must equal "software"),
    subcategory, and tags (non-empty list). Each missing or invalid field is
    a fail-level finding that flips the exit code to 1.
  - Governance docs (under repo-governance/conventions/, principles/,
    development/, workflows/) require: title. A missing description emits
    a warn-level finding — reported but does NOT flip the exit code.

Files outside both areas have no required fields and pass automatically.

When no positional arguments are supplied, the command scans docs/ and
repo-governance/ relative to the git root.

Exit codes:
  0 — clean (no fail-level findings; warn-level findings may still be reported)
  1 — at least one fail-level finding present
  2 — invocation error (cannot find git root, I/O failure, etc.)`,
	Example: `  # Audit default paths
  rhino-cli docs validate-frontmatter

  # Audit an explicit path
  rhino-cli docs validate-frontmatter docs/explanation/software-engineering/

  # JSON envelope output
  rhino-cli docs validate-frontmatter -o json

  # Markdown summary table
  rhino-cli docs validate-frontmatter -o markdown`,
	SilenceErrors: true,
	SilenceUsage:  true,
	RunE:          runDocsValidateFrontmatter,
}

func init() {
	docsCmd.AddCommand(docsValidateFrontmatterCmd)
}

// runDocsValidateFrontmatter resolves the scan paths, runs the validator,
// dispatches the result through cliout.Dispatcher, and returns a non-nil
// error when any fail-severity finding is reported (driving exit code 1).
// Warn-level findings are rendered but do not flip the exit code.
func runDocsValidateFrontmatter(cmd *cobra.Command, args []string) error {
	repoRoot, err := findGitRoot()
	if err != nil {
		return fmt.Errorf("failed to find git repository root: %w", err)
	}

	relPaths := resolveDocsValidateFrontmatterPaths(args)
	fullPaths := make([]string, 0, len(relPaths))
	for _, p := range relPaths {
		fullPaths = append(fullPaths, filepath.Join(repoRoot, p))
	}

	findings, err := docsValidateFrontmatterFn(fullPaths)
	if err != nil {
		return fmt.Errorf("docs validate-frontmatter failed: %w", err)
	}

	dispatcher := cliout.NewDispatcher(
		writeDocsValidateFrontmatterText,
		writeDocsValidateFrontmatterJSON,
		writeDocsValidateFrontmatterMarkdown,
	)
	if err := dispatcher.Write(cmd.OutOrStdout(), outputFormat, findings); err != nil {
		return fmt.Errorf("write output: %w", err)
	}

	if docs.HasFailFindings(findings) {
		return fmt.Errorf("%d docs frontmatter fail-level finding(s) found", countSeverity(findings, "fail"))
	}
	return nil
}

// resolveDocsValidateFrontmatterPaths picks the active scan path list,
// preferring positional args, falling back to the default set.
func resolveDocsValidateFrontmatterPaths(args []string) []string {
	if len(args) > 0 {
		return args
	}
	return docsValidateFrontmatterDefaultPaths
}

// countSeverity returns the number of findings with the given severity.
func countSeverity(findings []docs.DocsFrontmatterFinding, severity string) int {
	n := 0
	for _, f := range findings {
		if f.Severity == severity {
			n++
		}
	}
	return n
}

// writeDocsValidateFrontmatterText writes a plain-text rendering of findings
// to w. Both fail- and warn-level findings appear in the output; only fail
// findings drive the exit code.
func writeDocsValidateFrontmatterText(w io.Writer, findings []docs.DocsFrontmatterFinding) error {
	if !docs.HasFailFindings(findings) && len(findings) == 0 {
		_, err := fmt.Fprintln(w, "DOCS FRONTMATTER VALIDATION PASSED: no findings")
		return err
	}
	failN := countSeverity(findings, "fail")
	warnN := countSeverity(findings, "warn")
	var sb strings.Builder
	if failN > 0 {
		fmt.Fprintf(&sb, "DOCS FRONTMATTER VALIDATION FAILED: %d fail finding(s)", failN)
		if warnN > 0 {
			fmt.Fprintf(&sb, ", %d warn finding(s)", warnN)
		}
		sb.WriteString("\n")
	} else {
		fmt.Fprintf(&sb, "DOCS FRONTMATTER VALIDATION PASSED with %d warn finding(s)\n", warnN)
	}
	for _, f := range findings {
		fmt.Fprintf(&sb, "  %s  [%s]  %s — %s\n", f.File, f.Severity, f.Kind, f.Message)
	}
	_, err := fmt.Fprint(w, sb.String())
	return err
}

// docsFrontmatterJSONFinding is the per-finding JSON representation inside the
// envelope's Result payload. Field tags fix the wire format.
type docsFrontmatterJSONFinding struct {
	File     string `json:"file"`
	Severity string `json:"severity"`
	Kind     string `json:"kind"`
	Message  string `json:"message"`
}

// docsFrontmatterJSONResult is the inner payload for the JSON envelope.
type docsFrontmatterJSONResult struct {
	Status   string                       `json:"status"`
	Count    int                          `json:"count"`
	FailN    int                          `json:"fail_count"`
	WarnN    int                          `json:"warn_count"`
	Findings []docsFrontmatterJSONFinding `json:"findings"`
}

// writeDocsValidateFrontmatterJSON writes a cliout.Envelope-wrapped JSON
// document to w. The envelope enforces canonical key ordering of schema,
// status, result.
func writeDocsValidateFrontmatterJSON(w io.Writer, findings []docs.DocsFrontmatterFinding) error {
	status := "passed"
	if docs.HasFailFindings(findings) {
		status = "failed"
	}
	jf := make([]docsFrontmatterJSONFinding, 0, len(findings))
	for _, f := range findings {
		jf = append(jf, docsFrontmatterJSONFinding{
			File:     f.File,
			Severity: f.Severity,
			Kind:     f.Kind,
			Message:  f.Message,
		})
	}
	env := cliout.Envelope[docsFrontmatterJSONResult]{
		Schema: docsValidateFrontmatterSchema,
		Status: status,
		Result: docsFrontmatterJSONResult{
			Status:   status,
			Count:    len(findings),
			FailN:    countSeverity(findings, "fail"),
			WarnN:    countSeverity(findings, "warn"),
			Findings: jf,
		},
	}
	data, err := json.MarshalIndent(env, "", "  ")
	if err != nil {
		return fmt.Errorf("marshal envelope: %w", err)
	}
	if _, err := w.Write(data); err != nil {
		return err
	}
	_, err = w.Write([]byte("\n"))
	return err
}

// writeDocsValidateFrontmatterMarkdown writes a markdown summary table to w.
func writeDocsValidateFrontmatterMarkdown(w io.Writer, findings []docs.DocsFrontmatterFinding) error {
	if len(findings) == 0 {
		_, err := fmt.Fprint(w, "## Docs Frontmatter Validation\n\n**PASSED**: no findings\n")
		return err
	}
	failN := countSeverity(findings, "fail")
	warnN := countSeverity(findings, "warn")
	var sb strings.Builder
	if failN > 0 {
		fmt.Fprintf(&sb, "## Docs Frontmatter Validation\n\n**FAILED**: %d fail finding(s), %d warn finding(s)\n\n", failN, warnN)
	} else {
		fmt.Fprintf(&sb, "## Docs Frontmatter Validation\n\n**PASSED** with %d warn finding(s)\n\n", warnN)
	}
	sb.WriteString("| File | Severity | Kind | Message |\n")
	sb.WriteString("|------|----------|------|---------|\n")
	for _, f := range findings {
		fmt.Fprintf(&sb, "| %s | %s | %s | %s |\n", f.File, f.Severity, f.Kind, f.Message)
	}
	_, err := fmt.Fprint(w, sb.String())
	return err
}
