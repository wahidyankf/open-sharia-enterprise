---
title: "Cli Applications"
date: 2026-02-04T00:00:00+07:00
draft: false
description: "Building command-line applications with flag package, limitations, and production CLI frameworks (cobra/urfave)"
weight: 1000054
tags: ["golang", "cli", "command-line", "cobra", "urfave", "production"]
---

## Why CLI Applications Matter in Go

Go excels at building command-line tools due to single-binary deployment, fast compilation, and cross-platform support. Understanding standard library flag parsing before adopting CLI frameworks prevents over-engineering simple tools and enables informed framework selection for complex CLIs with subcommands.

**Core benefits**:

- **Single binary**: Deploy without runtime dependencies
- **Fast startup**: No VM warmup, instant execution
- **Cross-compilation**: Build for any OS/architecture from one machine
- **Standard library sufficient**: Many tools need only flag package

**Problem**: Many developers immediately use heavyweight frameworks (cobra) for simple scripts, adding unnecessary complexity. Conversely, complex CLIs using only flag package become unmaintainable as features grow.

**Solution**: Start with `flag` package for fundamentals, recognize limitations (no subcommands, limited help), then introduce production frameworks (cobra/urfave) with clear rationale based on CLI complexity.

## Standard Library First: flag Package Basics

Go's `flag` package provides command-line flag parsing with automatic help generation. Suitable for simple CLIs without subcommands.

**Basic flag parsing pattern**:

```go
package main

import (
    "flag"
    // => Standard library for command-line flag parsing
    // => No external dependencies required
    "fmt"
    // => Standard library for formatted output
    "os"
    // => Standard library for OS operations (exit, env vars)
)

func main() {
    // flag.String() defines string flag with default value and description
    // => Returns pointer to string that will hold flag value
    // => Format: flag.String(name, defaultValue, description)

    name := flag.String("name", "World", "name to greet")
    // => name is *string (pointer to string)
    // => Default value: "World"
    // => Description shown in -h output
    // => Usage: -name=Alice or -name Alice

    verbose := flag.Bool("verbose", false, "enable verbose output")
    // => verbose is *bool (pointer to bool)
    // => Default value: false
    // => Usage: -verbose or -verbose=true

    count := flag.Int("count", 1, "number of greetings")
    // => count is *int (pointer to int)
    // => Default value: 1
    // => Usage: -count=3 or -count 3

    flag.Parse()
    // => Parses command-line flags from os.Args[1:]
    // => Must call before accessing flag values
    // => Populates name, verbose, count pointers
    // => Stops at first non-flag argument
    // => Prints error and exits if invalid flags

    if *verbose {
        // => Dereference pointer to access value
        // => verbose is true if -verbose flag provided
        fmt.Printf("Parsed flags: name=%s, count=%d\n", *name, *count)
        // => Output: Parsed flags: name=Alice, count=3
    }

    for i := 0; i < *count; i++ {
        // => Loop count times
        // => *count is dereferenced int value
        fmt.Printf("Hello, %s!\n", *name)
        // => Output: Hello, Alice!
        // => Printed count times
    }
}
```

**Positional arguments pattern**:

```go
package main

import (
    "flag"
    "fmt"
    "os"
)

func main() {
    verbose := flag.Bool("verbose", false, "enable verbose output")
    // => verbose flag applies to all arguments

    flag.Parse()
    // => Parses flags from os.Args[1:]
    // => Stops at first non-flag argument

    args := flag.Args()
    // => flag.Args() returns non-flag arguments as []string
    // => Everything after flags
    // => Example: app -verbose file1.txt file2.txt → args is [file1.txt, file2.txt]

    if len(args) == 0 {
        // => len(args) is number of positional arguments
        // => No arguments provided
        fmt.Fprintln(os.Stderr, "Error: no files specified")
        // => fmt.Fprintln writes to stderr
        // => os.Stderr is standard error stream
        flag.Usage()
        // => flag.Usage() prints auto-generated help
        // => Includes all flag descriptions
        os.Exit(1)
        // => os.Exit(1) exits with error code 1
        // => Non-zero indicates error
    }

    for _, file := range args {
        // => Iterate over positional arguments
        // => file is each argument string
        processFile(file, *verbose)
        // => Process each file with verbose flag
    }
}

func processFile(filename string, verbose bool) {
    // => filename is positional argument
    // => verbose is dereferenced bool value
    if verbose {
        fmt.Printf("Processing: %s\n", filename)
    }
    // ... file processing logic
}
```

**Custom flag usage message**:

```go
package main

import (
    "flag"
    "fmt"
    "os"
)

func main() {
    flag.Usage = func() {
        // => Override default Usage function
        // => Custom help message
        fmt.Fprintf(os.Stderr, "Usage: %s [options] <file1> <file2> ...\n", os.Args[0])
        // => os.Args[0] is program name
        // => fmt.Fprintf writes to stderr
        fmt.Fprintln(os.Stderr, "\nOptions:")
        // => Blank line before options
        flag.PrintDefaults()
        // => flag.PrintDefaults() prints all flag descriptions
        // => Format: -flag=value: description
    }

    output := flag.String("output", "result.txt", "output file")
    // => output flag with custom usage message
    parallel := flag.Int("parallel", 1, "number of parallel workers")
    // => parallel flag for concurrency control

    flag.Parse()
    // => Parse flags with custom usage

    if flag.NArg() == 0 {
        // => flag.NArg() returns number of positional arguments
        // => Equivalent to len(flag.Args())
        flag.Usage()
        // => Prints custom usage message
        os.Exit(1)
    }

    fmt.Printf("Output: %s, Parallel: %d\n", *output, *parallel)
    // => Shows parsed flag values
    fmt.Printf("Input files: %v\n", flag.Args())
    // => Shows positional arguments as slice
}
```

**Environment variable fallback pattern**:

```go
package main

import (
    "flag"
    "fmt"
    "os"
)

func main() {
    // Standard library supports flags OR environment variables
    // => No automatic env var parsing (manual fallback)

    var apiKey string
    flag.StringVar(&apiKey, "api-key", "", "API key for authentication")
    // => flag.StringVar stores value in existing variable
    // => &apiKey is pointer to apiKey variable
    // => No default value (empty string)

    flag.Parse()
    // => Parse command-line flags

    if apiKey == "" {
        // => Flag not provided, check environment variable
        apiKey = os.Getenv("API_KEY")
        // => os.Getenv retrieves environment variable
        // => Returns "" if not set
        // => Fallback pattern: flag → env var → default
    }

    if apiKey == "" {
        // => Neither flag nor env var provided
        fmt.Fprintln(os.Stderr, "Error: API key required (use -api-key or API_KEY env var)")
        os.Exit(1)
    }

    fmt.Printf("Using API key: %s...\n", apiKey[:8])
    // => Show first 8 characters (security: don't print full key)
}
```

**Command execution pattern**:

```go
package main

import (
    "flag"
    "fmt"
    "os"
    "os/exec"
    // => Standard library for running external commands
)

func main() {
    command := flag.String("command", "ls", "command to execute")
    // => command flag specifies external command
    dryRun := flag.Bool("dry-run", false, "print command without executing")
    // => dry-run flag for testing

    flag.Parse()
    // => Parse flags

    args := flag.Args()
    // => Positional arguments passed to command

    cmd := exec.Command(*command, args...)
    // => exec.Command creates *exec.Cmd
    // => *command is command name
    // => args... spreads slice as variadic arguments
    // => Example: exec.Command("ls", "-la", "/tmp")

    cmd.Stdout = os.Stdout
    // => cmd.Stdout connects to process stdout
    // => os.Stdout is standard output stream
    // => Command output prints directly

    cmd.Stderr = os.Stderr
    // => cmd.Stderr connects to process stderr
    // => Errors print directly

    if *dryRun {
        // => Dry run mode: print without executing
        fmt.Printf("Would execute: %s %v\n", *command, args)
        return
    }

    if err := cmd.Run(); err != nil {
        // => cmd.Run() executes command and waits for completion
        // => Returns error if command fails
        // => Blocks until command finishes
        fmt.Fprintf(os.Stderr, "Error executing command: %v\n", err)
        os.Exit(1)
    }
}
```

**Limitations for production CLI applications**:

- **No subcommands**: Cannot create `git commit`, `docker run` style CLIs
- **Limited help text**: Basic auto-generated help, no rich formatting
- **No shell completion**: Users cannot tab-complete commands/flags
- **No persistent flags**: Cannot apply flags globally across subcommands
- **No command aliases**: Cannot create shortcuts (e.g., `ps` for `process status`)
- **Manual validation**: No built-in flag validation or required flags
- **No command categories**: Cannot organize commands into groups

## Production Framework: Cobra for Complex CLIs

Cobra is the most popular Go CLI framework, used by Kubernetes, Hugo, and GitHub CLI. Provides subcommands, persistent flags, shell completion, and rich help generation.

**Why cobra over flag package**:

- **Subcommands**: Essential for complex CLIs (git, docker, kubectl)
- **Persistent flags**: Global flags inherited by all subcommands
- **Auto-generated help**: Professional help text with usage examples
- **Shell completion**: Bash/Zsh/Fish/PowerShell completion scripts
- **Command aliases**: Alternative command names
- **PreRun/PostRun hooks**: Setup/teardown logic per command

**Basic cobra CLI pattern**:

```go
package main

import (
    "fmt"
    "os"

    "github.com/spf13/cobra"
    // => External dependency: github.com/spf13/cobra
    // => Most popular Go CLI framework
    // => Install: go get github.com/spf13/cobra/cobra
)

func main() {
    var rootCmd = &cobra.Command{
        // => cobra.Command represents command or subcommand
        // => rootCmd is application entry point
        Use:   "app",
        // => Use is command name and argument pattern
        // => Shown in help text
        Short: "A brief description of your application",
        // => Short is one-line description
        // => Shown in command list
        Long: `A longer description that spans multiple lines and likely contains
examples and usage of using your application.`,
        // => Long is detailed description
        // => Shown in command help
        Run: func(cmd *cobra.Command, args []string) {
            // => Run is command execution function
            // => cmd is *cobra.Command (current command)
            // => args is []string (positional arguments)
            fmt.Println("Hello from root command")
        },
    }

    if err := rootCmd.Execute(); err != nil {
        // => rootCmd.Execute() parses flags and runs command
        // => Returns error if command fails
        // => Handles help (-h) and version (--version) automatically
        fmt.Fprintln(os.Stderr, err)
        os.Exit(1)
    }
}
```

**Subcommands pattern**:

```go
package main

import (
    "fmt"
    "os"

    "github.com/spf13/cobra"
)

func main() {
    var rootCmd = &cobra.Command{
        Use:   "git",
        // => Root command: git
        Short: "Git command-line tool",
    }

    var commitCmd = &cobra.Command{
        // => Subcommand: git commit
        Use:   "commit",
        // => Use is subcommand name
        Short: "Record changes to the repository",
        // => Short shown in git help
        Run: func(cmd *cobra.Command, args []string) {
            // => Executes for: git commit
            message, _ := cmd.Flags().GetString("message")
            // => cmd.Flags().GetString retrieves flag value
            // => message is -m flag value
            all, _ := cmd.Flags().GetBool("all")
            // => all is -a flag value

            fmt.Printf("Committing with message: %s (all=%v)\n", message, all)
            // => Output: Committing with message: initial commit (all=false)
        },
    }

    commitCmd.Flags().StringP("message", "m", "", "commit message")
    // => StringP adds string flag with short name
    // => Format: --message=text or -m text
    // => "m" is short flag (single dash: -m)
    // => "" is default value
    commitCmd.Flags().BoolP("all", "a", false, "commit all changes")
    // => BoolP adds bool flag with short name
    // => Format: --all or -a

    var pushCmd = &cobra.Command{
        // => Subcommand: git push
        Use:   "push [remote] [branch]",
        // => Use documents positional arguments
        // => [remote] and [branch] are optional
        Short: "Update remote refs",
        Args:  cobra.MaximumNArgs(2),
        // => Args validates argument count
        // => MaximumNArgs(2) allows 0-2 arguments
        // => Cobra provides: NoArgs, ExactArgs(n), MinimumNArgs(n)
        Run: func(cmd *cobra.Command, args []string) {
            // => args contains positional arguments
            remote := "origin"
            branch := "main"
            if len(args) >= 1 {
                remote = args[0]
                // => First argument overrides remote
            }
            if len(args) >= 2 {
                branch = args[1]
                // => Second argument overrides branch
            }

            force, _ := cmd.Flags().GetBool("force")
            fmt.Printf("Pushing to %s/%s (force=%v)\n", remote, branch, force)
        },
    }

    pushCmd.Flags().BoolP("force", "f", false, "force push")
    // => Local flag: only for push command

    rootCmd.AddCommand(commitCmd)
    // => Add commit subcommand to root
    // => Makes git commit available
    rootCmd.AddCommand(pushCmd)
    // => Add push subcommand to root
    // => Makes git push available

    if err := rootCmd.Execute(); err != nil {
        fmt.Fprintln(os.Stderr, err)
        os.Exit(1)
    }
}
```

**Persistent flags pattern**:

```go
package main

import (
    "fmt"
    "os"

    "github.com/spf13/cobra"
)

var (
    verbose bool
    config  string
)
// => Package-level variables for persistent flags
// => Accessible across all commands
// => Alternative: use cmd.Flags() in each Run function

func main() {
    var rootCmd = &cobra.Command{
        Use:   "app",
        Short: "Application with persistent flags",
    }

    rootCmd.PersistentFlags().BoolVarP(&verbose, "verbose", "v", false, "verbose output")
    // => PersistentFlags() returns persistent flag set
    // => BoolVarP stores value in existing variable (&verbose)
    // => Inherited by all subcommands
    // => Available globally: app -v command1, app command2 -v

    rootCmd.PersistentFlags().StringVar(&config, "config", "", "config file")
    // => StringVar for string persistent flag
    // => Available to root and all subcommands

    var serveCmd = &cobra.Command{
        Use:   "serve",
        Short: "Start server",
        Run: func(cmd *cobra.Command, args []string) {
            // => verbose and config available here
            if verbose {
                fmt.Printf("Starting server with config: %s\n", config)
            }
            // ... server logic
        },
    }

    var migrateCmd = &cobra.Command{
        Use:   "migrate",
        Short: "Run database migrations",
        Run: func(cmd *cobra.Command, args []string) {
            // => Same persistent flags available
            if verbose {
                fmt.Printf("Running migrations with config: %s\n", config)
            }
            // ... migration logic
        },
    }

    rootCmd.AddCommand(serveCmd)
    rootCmd.AddCommand(migrateCmd)

    if err := rootCmd.Execute(); err != nil {
        fmt.Fprintln(os.Stderr, err)
        os.Exit(1)
    }
}
```

**Required flags and validation**:

```go
package main

import (
    "fmt"
    "os"

    "github.com/spf13/cobra"
)

func main() {
    var rootCmd = &cobra.Command{Use: "app"}

    var deployCmd = &cobra.Command{
        Use:   "deploy",
        Short: "Deploy application",
        PreRunE: func(cmd *cobra.Command, args []string) error {
            // => PreRunE runs before Run
            // => E suffix means returns error
            // => Used for validation
            apiKey, _ := cmd.Flags().GetString("api-key")
            if apiKey == "" {
                // => Custom validation logic
                return fmt.Errorf("--api-key is required")
            }
            return nil
            // => nil means validation passed
        },
        Run: func(cmd *cobra.Command, args []string) {
            apiKey, _ := cmd.Flags().GetString("api-key")
            environment, _ := cmd.Flags().GetString("environment")

            fmt.Printf("Deploying to %s with key %s...\n", environment, apiKey[:8])
            // => Deployment logic here
        },
    }

    deployCmd.Flags().String("api-key", "", "deployment API key (required)")
    deployCmd.MarkFlagRequired("api-key")
    // => MarkFlagRequired makes flag required
    // => Cobra validates before running command
    // => Error if flag not provided

    deployCmd.Flags().String("environment", "production", "target environment")
    // => Optional flag with default value

    rootCmd.AddCommand(deployCmd)

    if err := rootCmd.Execute(); err != nil {
        fmt.Fprintln(os.Stderr, err)
        os.Exit(1)
    }
}
```

**Shell completion generation**:

```go
package main

import (
    "fmt"
    "os"

    "github.com/spf13/cobra"
)

func main() {
    var rootCmd = &cobra.Command{
        Use:   "app",
        Short: "Application with shell completion",
    }

    var completionCmd = &cobra.Command{
        Use:   "completion [bash|zsh|fish|powershell]",
        Short: "Generate shell completion script",
        Long: `To load completions:

Bash:
  $ source <(app completion bash)
  # Add to ~/.bashrc: eval "$(app completion bash)"

Zsh:
  $ source <(app completion zsh)
  # Add to ~/.zshrc: eval "$(app completion zsh)"
`,
        Args: cobra.ExactArgs(1),
        // => Requires exactly one argument (shell type)
        Run: func(cmd *cobra.Command, args []string) {
            switch args[0] {
            case "bash":
                rootCmd.GenBashCompletion(os.Stdout)
                // => GenBashCompletion generates bash script
                // => Writes to stdout for piping
            case "zsh":
                rootCmd.GenZshCompletion(os.Stdout)
                // => GenZshCompletion for zsh
            case "fish":
                rootCmd.GenFishCompletion(os.Stdout, true)
                // => GenFishCompletion for fish shell
                // => true includes descriptions
            case "powershell":
                rootCmd.GenPowerShellCompletion(os.Stdout)
                // => GenPowerShellCompletion for Windows PowerShell
            default:
                fmt.Fprintf(os.Stderr, "Unsupported shell: %s\n", args[0])
                os.Exit(1)
            }
        },
    }

    rootCmd.AddCommand(completionCmd)

    if err := rootCmd.Execute(); err != nil {
        fmt.Fprintln(os.Stderr, err)
        os.Exit(1)
    }
}
```

## Alternative: urfave/cli Framework

`urfave/cli` is a lightweight alternative to cobra, offering simpler API at the cost of fewer features.

**When to use urfave/cli over cobra**:

- Simpler subcommand structure (fewer nested commands)
- Prefer declarative command definitions
- Don't need shell completion
- Smaller binary size desired

**Basic urfave/cli pattern**:

```go
package main

import (
    "fmt"
    "os"

    "github.com/urfave/cli/v2"
    // => External dependency: github.com/urfave/cli/v2
    // => Lightweight CLI framework
    // => Install: go get github.com/urfave/cli/v2
)

func main() {
    app := &cli.App{
        // => cli.App represents application
        Name:  "greet",
        Usage: "greet someone",
        Flags: []cli.Flag{
            // => Global flags (available to all commands)
            &cli.BoolFlag{
                Name:    "verbose",
                Aliases: []string{"v"},
                // => Aliases is []string of short names
                // => Usage: -v or --verbose
                Usage:   "enable verbose output",
            },
        },
        Commands: []*cli.Command{
            // => Commands is slice of subcommands
            {
                Name:    "hello",
                Aliases: []string{"hi"},
                // => Aliases for subcommand
                // => Usage: greet hello OR greet hi
                Usage:   "say hello",
                Flags: []cli.Flag{
                    &cli.StringFlag{
                        Name:     "name",
                        Aliases:  []string{"n"},
                        Value:    "World",
                        // => Default value
                        Usage:    "name to greet",
                        Required: false,
                    },
                },
                Action: func(c *cli.Context) error {
                    // => Action is command handler
                    // => c is *cli.Context (command context)
                    // => Returns error on failure

                    name := c.String("name")
                    // => c.String retrieves flag value
                    // => Returns Value if flag not provided

                    if c.Bool("verbose") {
                        // => c.Bool retrieves global flag
                        // => Inherited from app.Flags
                        fmt.Printf("Verbose: greeting %s\n", name)
                    }

                    fmt.Printf("Hello, %s!\n", name)
                    return nil
                    // => nil indicates success
                },
            },
        },
    }

    if err := app.Run(os.Args); err != nil {
        // => app.Run parses args and executes commands
        // => os.Args is command-line arguments
        fmt.Fprintln(os.Stderr, err)
        os.Exit(1)
    }
}
```

## Trade-offs Comparison

| Aspect               | flag Package                        | cobra                                      | urfave/cli                        |
| -------------------- | ----------------------------------- | ------------------------------------------ | --------------------------------- |
| **Complexity**       | Low (stdlib)                        | Medium (external, well-documented)         | Low-Medium (external, simple API) |
| **Subcommands**      | ❌ Manual implementation            | ✅ Built-in with nesting                   | ✅ Built-in, flat structure       |
| **Help Generation**  | Basic auto-generated                | Rich with examples                         | Auto-generated with usage         |
| **Shell Completion** | ❌ None                             | ✅ Bash/Zsh/Fish/PowerShell                | ❌ Limited                        |
| **Persistent Flags** | ❌ Manual propagation               | ✅ Built-in inheritance                    | ✅ Global flags                   |
| **Learning Curve**   | Minimal (standard library)          | Moderate (framework concepts)              | Low (straightforward API)         |
| **Binary Size**      | Smallest (no dependencies)          | Larger (~2MB added)                        | Small (~500KB added)              |
| **Use Cases**        | Simple scripts, single command CLIs | Complex CLIs with subcommands              | Moderate CLIs, simpler than cobra |
| **Examples**         | One-off utilities, build scripts    | kubectl, docker, hugo, gh (GitHub CLI)     | Intermediate tools                |
| **Validation**       | Manual                              | Built-in (required flags, argument counts) | Built-in (required flags)         |
| **Command Aliases**  | ❌ None                             | ✅ Per command                             | ✅ Per command                    |

## Best Practices

**Progressive adoption strategy**:

1. **Start with flag**: Simple CLIs without subcommands
2. **Add cobra**: When needing 3+ subcommands or shell completion
3. **Consider urfave/cli**: If cobra feels heavy and no completion needed
4. **Stay with flag**: Avoid frameworks for simple utilities

**When flag package sufficient**:

- Single-purpose tools (formatters, converters)
- Build scripts and automation utilities
- Internal tools with no user-facing help requirements
- Performance-critical tools (minimize dependencies)

**When cobra justified**:

- Multi-level subcommands (git, docker, kubectl style)
- Shell completion required for user experience
- Complex help documentation with examples
- Public-facing CLI tools with broad user base
- Commands with persistent global flags

**When urfave/cli appropriate**:

- Moderate subcommand structure (2-5 commands)
- Don't need shell completion
- Prefer simpler API over cobra's features
- Binary size matters but need subcommands

**Command organization patterns**:

```go
// File: cmd/root.go
package cmd

var rootCmd = &cobra.Command{
    Use:   "app",
    Short: "Application root command",
}

func Execute() {
    if err := rootCmd.Execute(); err != nil {
        os.Exit(1)
    }
}

// File: cmd/serve.go
package cmd

func init() {
    rootCmd.AddCommand(serveCmd)
}

var serveCmd = &cobra.Command{
    Use:   "serve",
    Short: "Start server",
    Run:   runServe,
}

func runServe(cmd *cobra.Command, args []string) {
    // Implementation
}

// File: main.go
package main

import "yourapp/cmd"

func main() {
    cmd.Execute()
}
```

**Configuration precedence**:

```go
// Best practice: flags > environment > config file > defaults
func getConfig(cmd *cobra.Command) Config {
    cfg := DefaultConfig()  // 1. Defaults

    if configFile, _ := cmd.Flags().GetString("config"); configFile != "" {
        // 2. Config file
        cfg.LoadFromFile(configFile)
    }

    // 3. Environment variables
    if apiKey := os.Getenv("API_KEY"); apiKey != "" {
        cfg.APIKey = apiKey
    }

    // 4. Flags (highest priority)
    if cmd.Flags().Changed("api-key") {
        cfg.APIKey, _ = cmd.Flags().GetString("api-key")
    }

    return cfg
}
```

**Error handling patterns**:

```go
// Return errors, don't os.Exit in library code
func runCommand(cmd *cobra.Command, args []string) error {
    if len(args) == 0 {
        return fmt.Errorf("no arguments provided")
    }
    // ... command logic
    return nil
}

// Handle errors in main or Execute
func main() {
    if err := rootCmd.Execute(); err != nil {
        // Log error details
        fmt.Fprintf(os.Stderr, "Error: %v\n", err)
        os.Exit(1)
    }
}
```

**Testing CLI commands**:

```go
// Testable command structure
func runServe(cfg Config) error {
    // Business logic testable without cobra
    return startServer(cfg)
}

var serveCmd = &cobra.Command{
    Use: "serve",
    RunE: func(cmd *cobra.Command, args []string) error {
        cfg := parseConfig(cmd)
        return runServe(cfg)  // Call testable function
    },
}

// Test without cobra
func TestServe(t *testing.T) {
    cfg := Config{Port: 8080}
    err := runServe(cfg)
    // Assert error is nil
}
```

**Version information pattern**:

```go
package main

import (
    "fmt"
    "github.com/spf13/cobra"
)

var (
    version = "dev"      // Set by linker: -ldflags "-X main.version=1.0.0"
    commit  = "none"     // Set by linker: -X main.commit=abc123
    date    = "unknown"  // Set by linker: -X main.date=2024-01-01
)

func main() {
    var versionCmd = &cobra.Command{
        Use:   "version",
        Short: "Print version information",
        Run: func(cmd *cobra.Command, args []string) {
            fmt.Printf("Version: %s\nCommit: %s\nBuilt: %s\n", version, commit, date)
        },
    }

    rootCmd.AddCommand(versionCmd)
    rootCmd.Execute()
}

// Build with: go build -ldflags "-X main.version=1.0.0 -X main.commit=$(git rev-parse --short HEAD) -X main.date=$(date -u +%Y-%m-%d)"
```
