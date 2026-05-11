package cmd

import (
	"fmt"

	"github.com/spf13/cobra"
	"github.com/wahidyankf/ose-public/apps/rhino-cli/internal/cliout"
)

// outputFormat is the sealed-enum mirror of the --output string flag. It is
// populated by parseOutputFormat (called from rootCmd.PersistentPreRunE) before
// each subcommand's RunE executes. writeFormattedV2 reads it directly.
var outputFormat cliout.OutputFormat = cliout.FormatText{}

// parseOutputFormat parses the current value of the `output` package-level
// string into outputFormat. It is called from rootCmd.PersistentPreRunE so that
// every subcommand's RunE sees a valid sealed-enum value. When the string is
// unrecognised it returns an error that Cobra propagates.
func parseOutputFormat(_ *cobra.Command, _ []string) error {
	f, ok := cliout.Parse(output)
	if !ok {
		return fmt.Errorf("unknown output format %q: must be text, json, or markdown", output)
	}
	outputFormat = f
	return nil
}

// writeFormattedV2 selects the correct formatter using the sealed OutputFormat enum,
// writes to cmd.OutOrStdout(), and returns any error. It always re-derives the
// format from the current `output` string so that test scenarios that set only
// the string variable work without also setting outputFormat. When the string is
// unrecognised it falls back to text output.
func writeFormattedV2(cmd *cobra.Command, verbose, quiet bool, f outputFuncs) error {
	// Derive the format from the sealed-enum var (set by PersistentPreRunE in
	// production), falling back to re-parsing the output string for test contexts
	// where PersistentPreRunE does not run.
	format := outputFormat
	if format.Code() != output {
		// output string was changed after PersistentPreRunE ran (e.g. in tests) —
		// re-parse to keep the two in sync.
		if reparsed, ok := cliout.Parse(output); ok {
			format = reparsed
		}
	}

	switch format.(type) { //nolint:gochecksumtype // exhaustive: all three variants covered
	case cliout.FormatJSON:
		out, err := f.json()
		if err != nil {
			return fmt.Errorf("failed to format JSON: %w", err)
		}
		_, _ = fmt.Fprint(cmd.OutOrStdout(), out)
	case cliout.FormatMarkdown:
		_, _ = fmt.Fprint(cmd.OutOrStdout(), f.markdown())
	default: // cliout.FormatText
		_, _ = fmt.Fprint(cmd.OutOrStdout(), f.text(verbose, quiet))
	}
	return nil
}
