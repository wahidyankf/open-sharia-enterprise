package cmd

import (
	"fmt"

	"github.com/spf13/cobra"
	"github.com/wahidyankf/ose-public/apps/rhino-cli/internal/severity"
)

// dddFinding is the common finding shape shared by all ddd subcommands.
// Both bcregistry.Finding and glossary.Finding carry the same three fields, so
// adapters in ddd_bc.go and ddd_ul.go convert to this type before handing
// control to runDdd.
type dddFinding struct {
	// File is the repository-relative path where the finding originated.
	File string
	// Message is the human-readable description of the finding.
	Message string
	// Severity is the resolved severity (error or warn) of the finding.
	Severity severity.Severity
}

// dddCommandSpec captures what varies between ddd subcommands (bc and ul).
type dddCommandSpec struct {
	// Use is the cobra Use string, e.g. "bc <app>".
	Use string
	// Short is the one-line description shown in help listings.
	Short string
	// Long is the full description shown in command-specific help.
	Long string
	// Example is the optional cobra Example block.
	Example string
	// ValidatorFn is called with the resolved (repoRoot, app) and returns
	// findings. Tests inject a fake here; production code injects the real
	// validator wrapped via an adapter that applies the severity flag.
	ValidatorFn func(repoRoot, app string) ([]dddFinding, error)
	// FindingsLabel appears in the "N error finding(s) found by …" message.
	FindingsLabel string
}

// newDddCommand constructs a cobra.Command from a dddCommandSpec.
// The caller is responsible for registering any command-specific flags
// (e.g. --severity) on the returned command before adding it to the parent.
func newDddCommand(spec dddCommandSpec) *cobra.Command {
	cmd := &cobra.Command{
		Use:           spec.Use,
		Short:         spec.Short,
		Long:          spec.Long,
		Example:       spec.Example,
		Args:          cobra.ExactArgs(1),
		SilenceErrors: true,
	}

	cmd.RunE = func(c *cobra.Command, args []string) error {
		return runDdd(c, args, spec)
	}

	return cmd
}

// runDdd contains the shared finding-print and exit-code logic for all ddd
// subcommands. It finds the git root, calls the validator, prints each finding,
// and returns an error when any error-level finding exists.
func runDdd(cmd *cobra.Command, args []string, spec dddCommandSpec) error {
	repoRoot, err := findGitRoot()
	if err != nil {
		return fmt.Errorf("failed to find git repository root: %w", err)
	}

	app := args[0]

	findings, err := spec.ValidatorFn(repoRoot, app)
	if err != nil {
		return err
	}

	for _, f := range findings {
		_, _ = fmt.Fprintf(cmd.OutOrStdout(), "%s: %s: %s\n", f.File, f.Severity.Code(), f.Message)
	}

	errCount := 0
	for _, f := range findings {
		if _, isErr := f.Severity.(severity.SeverityError); isErr {
			errCount++
		}
	}
	if errCount > 0 {
		return fmt.Errorf("%d error finding(s) found by %s", errCount, spec.FindingsLabel)
	}
	return nil
}
