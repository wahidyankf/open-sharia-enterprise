package cmd

import (
	"fmt"
	"io/fs"
	"os"
	"path/filepath"
	"sort"
	"strings"

	"github.com/spf13/cobra"
	"github.com/wahidyankf/ose-public/apps/rhino-cli/internal/naming"
)

// workflowTypes enumerates the trailing type tokens permitted by the
// workflow naming convention.
var workflowTypes = []string{"quality-gate", "execution", "setup"}

// workflowsValidateNamingFn is the test-mockable entrypoint for workflow
// naming validation.
var workflowsValidateNamingFn = workflowsValidateNaming

var workflowsValidateNamingCmd = &cobra.Command{
	Use:   "validate-naming",
	Short: "Validate workflow filename suffixes and frontmatter name consistency",
	Long: `Validate that every workflow file under governance/workflows/ follows the
naming convention documented in governance/conventions/structure/workflow-naming.md.

The command enforces two rules:
- Filename (sans .md) ends with one of: quality-gate, execution, setup.
- Frontmatter 'name:' field equals the filename (without .md).

Exempt from validation:
- Any README.md.
- Any file under governance/workflows/meta/ (reference docs, not workflows).`,
	Example: `  # Validate workflow naming across the governance tree
  rhino-cli workflows validate-naming

  # Output as JSON
  rhino-cli workflows validate-naming -o json

  # Markdown output
  rhino-cli workflows validate-naming -o markdown`,
	SilenceErrors: true,
	RunE:          runValidateWorkflowsNaming,
}

func init() {
	workflowsCmd.AddCommand(workflowsValidateNamingCmd)
}

func runValidateWorkflowsNaming(cmd *cobra.Command, args []string) error {
	repoRoot, err := findGitRoot()
	if err != nil {
		return fmt.Errorf("failed to find git repository root: %w", err)
	}

	violations, err := workflowsValidateNamingFn(repoRoot)
	if err != nil {
		return fmt.Errorf("validation failed: %w", err)
	}

	if err := writeFormatted(cmd, output, verbose, quiet, outputFuncs{
		text:     func(v, q bool) string { return formatNamingText("Workflows", violations, v, q) },
		json:     func() (string, error) { return formatNamingJSON("workflows", violations) },
		markdown: func() string { return formatNamingMarkdown("Workflows", violations) },
	}); err != nil {
		return err
	}

	if len(violations) > 0 {
		return fmt.Errorf("%d naming violation(s) found", len(violations))
	}
	return nil
}

// workflowsValidateNaming walks governance/workflows/ recursively, excluding
// README.md files and anything under `meta/`, and returns every naming
// violation.
func workflowsValidateNaming(repoRoot string) ([]naming.Violation, error) {
	root := filepath.Join(repoRoot, "governance", "workflows")
	files, err := listWorkflowFiles(root)
	if err != nil {
		return nil, err
	}

	var violations []naming.Violation
	for _, path := range files {
		if v := naming.ValidateSuffix(path, workflowTypes, "type-suffix"); v != nil {
			violations = append(violations, *v)
		}
		content, err := os.ReadFile(path) //nolint:gosec // trusted repo path
		if err != nil {
			return nil, fmt.Errorf("read %s: %w", path, err)
		}
		if v := naming.ValidateFrontmatterName(path, content); v != nil {
			violations = append(violations, *v)
		}
	}

	sort.SliceStable(violations, func(i, j int) bool {
		if violations[i].Path == violations[j].Path {
			return violations[i].Kind < violations[j].Kind
		}
		return violations[i].Path < violations[j].Path
	})

	return violations, nil
}

// listWorkflowFiles returns every `.md` file under `root` eligible for
// validation. Files named README.md and anything under a `meta/` directory
// (at any depth below `root`) are filtered out per convention. A missing
// root yields an empty slice, not an error.
func listWorkflowFiles(root string) ([]string, error) {
	var files []string
	err := filepath.WalkDir(root, func(path string, d fs.DirEntry, err error) error {
		if err != nil {
			if os.IsNotExist(err) {
				return filepath.SkipAll
			}
			return err
		}
		if d.IsDir() {
			// Skip the meta/ reference tree at any depth beneath root.
			if d.Name() == "meta" && path != root {
				return filepath.SkipDir
			}
			return nil
		}
		name := d.Name()
		if name == "README.md" {
			return nil
		}
		if !strings.HasSuffix(name, ".md") {
			return nil
		}
		files = append(files, path)
		return nil
	})
	if err != nil {
		return nil, err
	}
	sort.Strings(files)
	return files, nil
}
