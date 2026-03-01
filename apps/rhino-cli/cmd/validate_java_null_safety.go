package cmd

import (
	"fmt"
	"path/filepath"

	"github.com/spf13/cobra"
	"github.com/wahidyankf/open-sharia-enterprise/apps/rhino-cli/internal/java"
)

var (
	validateJavaNullSafetyAnnotation string
)

var validateJavaNullSafetyCmd = &cobra.Command{
	Use:   "validate-java-null-safety <source-root>",
	Short: "Validate Java packages have required null safety annotation",
	Long: `Check that every Java package directory contains a package-info.java
with the required null safety annotation (default: @NullMarked).

For each directory under <source-root> that contains at least one .java file,
the command verifies:
  1. package-info.java exists
  2. package-info.java contains @<annotation>

Two violation types are reported:
  missing_package_info  - directory has .java files but no package-info.java
  missing_annotation    - package-info.java exists but lacks @<annotation>

<source-root> is resolved relative to the current working directory.
When invoked via Nx, the working directory is the workspace root.`,
	Example: `  # Validate from workspace root (Nx usage)
  rhino-cli validate-java-null-safety apps/organiclever-be/src/main/java

  # Use a different annotation
  rhino-cli validate-java-null-safety src/main/java --annotation NonNullByDefault

  # Output as JSON
  rhino-cli validate-java-null-safety src/main/java -o json

  # Output as markdown report
  rhino-cli validate-java-null-safety src/main/java -o markdown`,
	Args:          cobra.ExactArgs(1),
	SilenceErrors: true,
	RunE:          runValidateJavaNullSafety,
}

func init() {
	rootCmd.AddCommand(validateJavaNullSafetyCmd)
	validateJavaNullSafetyCmd.Flags().StringVar(
		&validateJavaNullSafetyAnnotation,
		"annotation",
		"NullMarked",
		"annotation name to require in package-info.java (without @)",
	)
}

func runValidateJavaNullSafety(cmd *cobra.Command, args []string) error {
	sourceRootArg := args[0]

	// Resolve source root relative to CWD
	sourceRoot, err := filepath.Abs(sourceRootArg)
	if err != nil {
		return fmt.Errorf("failed to resolve source root %q: %w", sourceRootArg, err)
	}

	opts := java.ValidationOptions{
		SourceRoot: sourceRoot,
		Annotation: validateJavaNullSafetyAnnotation,
	}

	result, err := java.ValidateAll(opts)
	if err != nil {
		return fmt.Errorf("validation failed: %w", err)
	}

	if err := writeFormatted(cmd, output, verbose, quiet, outputFuncs{
		text:     func(v, q bool) string { return java.FormatText(result, v, q) },
		json:     func() (string, error) { return java.FormatJSON(result) },
		markdown: func() string { return java.FormatMarkdown(result) },
	}); err != nil {
		return err
	}

	numViolations := result.TotalPackages - result.ValidPackages
	if numViolations > 0 {
		if !quiet && output == "text" {
			_, _ = fmt.Fprintf(cmd.OutOrStderr(), "\n❌ Found %d null safety violation(s)\n", numViolations)
		}
		return fmt.Errorf("found %d null safety violation(s)", numViolations)
	}

	return nil
}
