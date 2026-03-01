package main

import (
	"flag"
	"fmt"
	"os"
	"path/filepath"

	"github.com/wahidyankf/open-sharia-enterprise/apps/jprojectvalidator-cli/internal/java"
)

func main() {
	os.Exit(run())
}

func run() int {
	fs := flag.NewFlagSet("jprojectvalidator-cli", flag.ContinueOnError)
	annotation := fs.String("annotation", "NullMarked", "annotation name to require in package-info.java (without @)")
	outputFmt := fs.String("o", "text", "output format: text, json, or markdown")
	verbose := fs.Bool("v", false, "verbose output")
	quiet := fs.Bool("q", false, "quiet output (suppress summary on success)")

	if err := fs.Parse(os.Args[1:]); err != nil {
		if err == flag.ErrHelp {
			return 0
		}
		// flag.ContinueOnError already wrote the error to stderr
		return 2
	}

	if *verbose && *quiet {
		fmt.Fprintln(os.Stderr, "error: -v and -q are mutually exclusive")
		return 2
	}

	switch *outputFmt {
	case "text", "json", "markdown":
		// valid
	default:
		fmt.Fprintf(os.Stderr, "error: invalid output format %q (want text, json, or markdown)\n", *outputFmt)
		return 2
	}

	args := fs.Args()
	if len(args) != 1 {
		fmt.Fprintln(os.Stderr, "usage: jprojectvalidator-cli [flags] <source-root>")
		fs.PrintDefaults()
		return 2
	}

	sourceRoot, err := filepath.Abs(args[0])
	if err != nil {
		fmt.Fprintf(os.Stderr, "error: failed to resolve source root %q: %v\n", args[0], err)
		return 2
	}

	opts := java.ValidationOptions{
		SourceRoot: sourceRoot,
		Annotation: *annotation,
	}

	result, err := java.ValidateAll(opts)
	if err != nil {
		fmt.Fprintf(os.Stderr, "error: validation failed: %v\n", err)
		return 2
	}

	switch *outputFmt {
	case "json":
		out, err := java.FormatJSON(result)
		if err != nil {
			fmt.Fprintf(os.Stderr, "error: failed to format JSON: %v\n", err)
			return 2
		}
		fmt.Println(out)
	case "markdown":
		fmt.Print(java.FormatMarkdown(result))
	default:
		fmt.Print(java.FormatText(result, *verbose, *quiet))
	}

	numViolations := result.TotalPackages - result.ValidPackages
	if numViolations > 0 {
		if !*quiet && *outputFmt == "text" {
			fmt.Fprintf(os.Stderr, "\n❌ Found %d null safety violation(s)\n", numViolations)
		}
		return 1
	}

	return 0
}
