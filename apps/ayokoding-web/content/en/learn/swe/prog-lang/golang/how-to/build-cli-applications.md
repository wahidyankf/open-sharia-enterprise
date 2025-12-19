---
title: "How to Build CLI Applications"
date: 2025-12-17T13:19:07+07:00
draft: false
weight: 616
description: "Create command-line applications with cobra, proper flag handling, and native binary distribution"
tags: ["golang", "cli", "cobra", "command-line", "flags"]
---

## Problem

Command-line applications need robust argument parsing, subcommands, help text generation, and standard CLI behaviors. The flag package handles simple cases but becomes unwieldy with complex command structures. Building production-quality CLIs requires proper error handling, exit codes, and professional UX.

This guide shows how to build effective CLI applications in Go.

## Cobra Framework

### Basic CLI Application

```go
package main

import (
    "fmt"
    "os"

    "github.com/spf13/cobra"
)

// ✅ Root command
var rootCmd = &cobra.Command{
    Use:   "myapp",
    Short: "MyApp is a CLI tool for data processing",
    Long: `A longer description that shows in help with more details about
what the application does and how to use it.`,
    Run: func(cmd *cobra.Command, args []string) {
        fmt.Println("Use 'myapp --help' for more information")
    },
}

func main() {
    if err := rootCmd.Execute(); err != nil {
        fmt.Fprintln(os.Stderr, err)
        os.Exit(1)
    }
}
```

### Adding Flags

```go
var (
    verbose bool
    config  string
    port    int
)

func init() {
    // ✅ Persistent flags (available to all subcommands)
    rootCmd.PersistentFlags().BoolVarP(&verbose, "verbose", "v", false, "verbose output")
    rootCmd.PersistentFlags().StringVar(&config, "config", "", "config file path")

    // ✅ Local flags (only this command)
    rootCmd.Flags().IntVarP(&port, "port", "p", 8080, "server port")

    // ✅ Required flags
    rootCmd.MarkFlagRequired("config")
}

// Usage:
// myapp --config=app.yaml --verbose --port=3000
// myapp -v --config=app.yaml -p 3000
```

## Subcommands

### Creating Subcommands

```go
// ✅ User management subcommand
var userCmd = &cobra.Command{
    Use:   "user",
    Short: "User management commands",
    Long:  "Commands for creating, listing, and deleting users",
}

// ✅ Create user subcommand
var userCreateCmd = &cobra.Command{
    Use:   "create [email]",
    Short: "Create a new user",
    Args:  cobra.ExactArgs(1), // Requires exactly 1 argument
    Run: func(cmd *cobra.Command, args []string) {
        email := args[0]

        name, _ := cmd.Flags().GetString("name")
        admin, _ := cmd.Flags().GetBool("admin")

        fmt.Printf("Creating user: %s (%s)\n", email, name)
        if admin {
            fmt.Println("User will be admin")
        }

        // Create user logic...
    },
}

// ✅ List users subcommand
var userListCmd = &cobra.Command{
    Use:   "list",
    Short: "List all users",
    Run: func(cmd *cobra.Command, args []string) {
        limit, _ := cmd.Flags().GetInt("limit")
        adminOnly, _ := cmd.Flags().GetBool("admin-only")

        fmt.Printf("Listing users (limit: %d, admin-only: %v)\n", limit, adminOnly)
        // List users logic...
    },
}

// ✅ Delete user subcommand
var userDeleteCmd = &cobra.Command{
    Use:   "delete [email]",
    Short: "Delete a user",
    Args:  cobra.ExactArgs(1),
    Run: func(cmd *cobra.Command, args []string) {
        email := args[0]
        force, _ := cmd.Flags().GetBool("force")

        if !force {
            fmt.Print("Are you sure? (y/N): ")
            var response string
            fmt.Scanln(&response)
            if response != "y" && response != "Y" {
                fmt.Println("Cancelled")
                return
            }
        }

        fmt.Printf("Deleting user: %s\n", email)
        // Delete user logic...
    },
}

func init() {
    // Add flags to subcommands
    userCreateCmd.Flags().String("name", "", "user name")
    userCreateCmd.Flags().Bool("admin", false, "create as admin")
    userCreateCmd.MarkFlagRequired("name")

    userListCmd.Flags().Int("limit", 10, "maximum users to show")
    userListCmd.Flags().Bool("admin-only", false, "show only admin users")

    userDeleteCmd.Flags().Bool("force", false, "skip confirmation")

    // Build command hierarchy
    userCmd.AddCommand(userCreateCmd)
    userCmd.AddCommand(userListCmd)
    userCmd.AddCommand(userDeleteCmd)

    rootCmd.AddCommand(userCmd)
}
```

**Usage:**

```bash
# Create user
myapp user create alice@example.com --name "Alice Smith"
myapp user create bob@example.com --name "Bob" --admin

# List users
myapp user list
myapp user list --limit 5 --admin-only

# Delete user
myapp user delete alice@example.com
myapp user delete bob@example.com --force
```

## Argument Validation

### Built-in Validators

```go
var cmdWithArgs = &cobra.Command{
    Use:   "process [files...]",
    Short: "Process files",

    // ✅ No arguments required
    Args: cobra.NoArgs,

    // ✅ Exactly N arguments
    Args: cobra.ExactArgs(1),

    // ✅ At least N arguments
    Args: cobra.MinimumNArgs(1),

    // ✅ At most N arguments
    Args: cobra.MaximumNArgs(3),

    // ✅ Range of arguments
    Args: cobra.RangeArgs(1, 5),

    // ✅ Only valid args (from ValidArgs list)
    ValidArgs: []string{"json", "xml", "csv"},
    Args:      cobra.OnlyValidArgs,

    Run: func(cmd *cobra.Command, args []string) {
        fmt.Println("Processing:", args)
    },
}
```

### Custom Validation

```go
// ✅ Custom argument validator
func validateEmail(cmd *cobra.Command, args []string) error {
    if len(args) != 1 {
        return fmt.Errorf("requires exactly 1 email argument")
    }

    email := args[0]
    if !strings.Contains(email, "@") {
        return fmt.Errorf("invalid email: %s", email)
    }

    return nil
}

var emailCmd = &cobra.Command{
    Use:   "send-email [email]",
    Short: "Send email to user",
    Args:  validateEmail,
    Run: func(cmd *cobra.Command, args []string) {
        email := args[0]
        fmt.Printf("Sending email to: %s\n", email)
    },
}
```

## Error Handling and Exit Codes

### Proper Error Handling

```go
// ✅ Return errors from commands
var processCmd = &cobra.Command{
    Use:   "process [file]",
    Short: "Process a file",
    Args:  cobra.ExactArgs(1),
    RunE: func(cmd *cobra.Command, args []string) error {
        filename := args[0]

        // Check file exists
        if _, err := os.Stat(filename); os.IsNotExist(err) {
            return fmt.Errorf("file not found: %s", filename)
        }

        // Process file
        if err := processFile(filename); err != nil {
            return fmt.Errorf("processing file: %w", err)
        }

        fmt.Println("File processed successfully")
        return nil
    },
}

// ✅ Custom exit codes
const (
    ExitSuccess     = 0
    ExitError       = 1
    ExitFileNotFound = 2
    ExitInvalidInput = 3
)

func processFile(filename string) error {
    // Implementation...
    return nil
}
```

### SilenceErrors and SilenceUsage

```go
func main() {
    rootCmd.SilenceErrors = true // Don't print error twice
    rootCmd.SilenceUsage = true  // Don't show usage on error

    if err := rootCmd.Execute(); err != nil {
        fmt.Fprintf(os.Stderr, "Error: %v\n", err)

        // Custom exit codes based on error type
        if os.IsNotExist(err) {
            os.Exit(ExitFileNotFound)
        }

        os.Exit(ExitError)
    }
}
```

## Input and Output

### Reading from stdin

```go
var processCmd = &cobra.Command{
    Use:   "process [file]",
    Short: "Process input from file or stdin",
    Args:  cobra.MaximumNArgs(1),
    RunE: func(cmd *cobra.Command, args []string) error {
        var input io.Reader

        if len(args) == 0 {
            // ✅ Read from stdin
            input = os.Stdin
            fmt.Fprintln(os.Stderr, "Reading from stdin...")
        } else {
            // ✅ Read from file
            filename := args[0]
            file, err := os.Open(filename)
            if err != nil {
                return fmt.Errorf("opening file: %w", err)
            }
            defer file.Close()
            input = file
        }

        // Process input
        scanner := bufio.NewScanner(input)
        for scanner.Scan() {
            line := scanner.Text()
            fmt.Println(processLine(line))
        }

        return scanner.Err()
    },
}

func processLine(line string) string {
    return strings.ToUpper(line)
}
```

**Usage:**

```bash
# From file
myapp process input.txt

# From stdin
echo "hello" | myapp process

# From stdin (interactive)
myapp process
hello
HELLO
^D
```

### Output Formatting

```go
var (
    outputFormat string
)

func init() {
    listCmd.Flags().StringVarP(&outputFormat, "output", "o", "text", "output format (text|json|table)")
}

var listCmd = &cobra.Command{
    Use:   "list",
    Short: "List items",
    RunE: func(cmd *cobra.Command, args []string) error {
        items := fetchItems()

        switch outputFormat {
        case "json":
            return outputJSON(items)
        case "table":
            return outputTable(items)
        case "text":
            return outputText(items)
        default:
            return fmt.Errorf("unknown format: %s", outputFormat)
        }
    },
}

func outputJSON(items []Item) error {
    enc := json.NewEncoder(os.Stdout)
    enc.SetIndent("", "  ")
    return enc.Encode(items)
}

func outputTable(items []Item) error {
    fmt.Printf("%-20s | %s\n", "Name", "Status")
    fmt.Println(strings.Repeat("-", 40))
    for _, item := range items {
        fmt.Printf("%-20s | %s\n", item.Name, item.Status)
    }
    return nil
}

func outputText(items []Item) error {
    for _, item := range items {
        fmt.Printf("%s: %s\n", item.Name, item.Status)
    }
    return nil
}
```

## Configuration

### Viper Integration

```go
import (
    "github.com/spf13/cobra"
    "github.com/spf13/viper"
)

var cfgFile string

func init() {
    cobra.OnInitialize(initConfig)
    rootCmd.PersistentFlags().StringVar(&cfgFile, "config", "", "config file path")
}

func initConfig() {
    if cfgFile != "" {
        viper.SetConfigFile(cfgFile)
    } else {
        viper.SetConfigName("config")
        viper.AddConfigPath(".")
        viper.AddConfigPath("$HOME/.myapp")
    }

    viper.AutomaticEnv()

    if err := viper.ReadInConfig(); err == nil {
        fmt.Println("Using config file:", viper.ConfigFileUsed())
    }
}

// ✅ Bind flags to viper
func init() {
    rootCmd.PersistentFlags().String("database", "", "database URL")
    viper.BindPFlag("database", rootCmd.PersistentFlags().Lookup("database"))
}
```

## Building and Distribution

### Building Binaries

```bash
# Build for current platform
go build -o myapp

# Build with version info
go build -ldflags="-X 'main.Version=1.0.0'" -o myapp

# Cross-compile for different platforms
GOOS=linux GOARCH=amd64 go build -o myapp-linux
GOOS=darwin GOARCH=arm64 go build -o myapp-mac
GOOS=windows GOARCH=amd64 go build -o myapp.exe
```

### Version Command

```go
var (
    Version   = "dev"
    BuildTime = "unknown"
    GitCommit = "unknown"
)

var versionCmd = &cobra.Command{
    Use:   "version",
    Short: "Print version information",
    Run: func(cmd *cobra.Command, args []string) {
        fmt.Printf("Version:    %s\n", Version)
        fmt.Printf("Build Time: %s\n", BuildTime)
        fmt.Printf("Git Commit: %s\n", GitCommit)
    },
}

func init() {
    rootCmd.AddCommand(versionCmd)
}
```

**Build with version:**

```bash
go build -ldflags="-X 'main.Version=1.0.0' -X 'main.BuildTime=$(date)' -X 'main.GitCommit=$(git rev-parse HEAD)'"
```

## Testing CLI Commands

### Testing Commands

```go
import (
    "bytes"
    "testing"

    "github.com/spf13/cobra"
)

func TestUserCreateCommand(t *testing.T) {
    cmd := &cobra.Command{Use: "root"}
    cmd.AddCommand(userCreateCmd)

    // ✅ Capture output
    buf := new(bytes.Buffer)
    cmd.SetOut(buf)
    cmd.SetErr(buf)

    // ✅ Set arguments
    cmd.SetArgs([]string{"user", "create", "alice@example.com", "--name", "Alice"})

    // ✅ Execute command
    err := cmd.Execute()
    if err != nil {
        t.Fatalf("command failed: %v", err)
    }

    // ✅ Check output
    output := buf.String()
    if !strings.Contains(output, "alice@example.com") {
        t.Errorf("expected email in output, got: %s", output)
    }
}

func TestUserCreateInvalidEmail(t *testing.T) {
    cmd := &cobra.Command{Use: "root"}
    cmd.AddCommand(userCreateCmd)

    cmd.SetArgs([]string{"user", "create", "invalid-email", "--name", "Alice"})

    err := cmd.Execute()
    if err == nil {
        t.Error("expected error for invalid email")
    }
}
```

## Autocomplete

### Shell Completion

```go
// ✅ Add completion command
var completionCmd = &cobra.Command{
    Use:   "completion [bash|zsh|fish|powershell]",
    Short: "Generate completion script",
    Long: `To load completions:

Bash:
  $ source <(myapp completion bash)
  $ myapp completion bash > /etc/bash_completion.d/myapp

Zsh:
  $ myapp completion zsh > "${fpath[1]}/_myapp"

Fish:
  $ myapp completion fish | source
  $ myapp completion fish > ~/.config/fish/completions/myapp.fish
`,
    ValidArgs: []string{"bash", "zsh", "fish", "powershell"},
    Args:      cobra.ExactValidArgs(1),
    Run: func(cmd *cobra.Command, args []string) {
        switch args[0] {
        case "bash":
            cmd.Root().GenBashCompletion(os.Stdout)
        case "zsh":
            cmd.Root().GenZshCompletion(os.Stdout)
        case "fish":
            cmd.Root().GenFishCompletion(os.Stdout, true)
        case "powershell":
            cmd.Root().GenPowerShellCompletion(os.Stdout)
        }
    },
}

func init() {
    rootCmd.AddCommand(completionCmd)
}
```

## Summary

Cobra framework simplifies CLI application development with command hierarchy, flag handling, and automatic help generation. Root commands define application entry points, subcommands organize functionality, and flags provide configuration options.

Commands use Use for syntax, Short for brief description, Long for detailed help, and Run or RunE for implementation. RunE returns errors enabling proper error handling. Args validators ensure correct argument counts and types.

Flags come in persistent (available to all subcommands) and local (specific command only) varieties. String, Int, Bool, and other typed flags parse arguments automatically. Mark required flags with MarkFlagRequired.

Subcommands create hierarchical command structures like git or docker. User commands group related operations - create, list, delete. Build command trees with AddCommand, organizing functionality logically.

Error handling uses RunE to return errors and proper exit codes. Check for specific error types to return appropriate exit codes. SilenceErrors and SilenceUsage prevent duplicate error messages.

Input handling supports both files and stdin. Check argument count to determine source - no arguments means stdin, one argument means file. This pattern enables piping and flexible usage.

Output formatting provides multiple formats based on flags. JSON for programmatic consumption, tables for human readability, plain text for simplicity. Switch output format with flags.

Configuration integration with viper enables config files, environment variables, and flag binding. Initialize viper in cobra.OnInitialize, bind flags to viper keys, access unified configuration.

Build binaries with version information using ldflags. Cross-compile for different platforms with GOOS and GOARCH. Distribute single binary files requiring no dependencies.

Testing CLI commands captures output and sets arguments programmatically. Test both success and error cases. Verify output contains expected content and errors return appropriate codes.

Shell completion generates autocomplete scripts for bash, zsh, fish, and PowerShell. Users install completion to get argument and flag suggestions. Generate with dedicated completion subcommand.

Cobra applications follow Unix conventions - read from stdin or files, write results to stdout, errors to stderr, return meaningful exit codes. These patterns create professional CLIs users expect.

## Related Content

- [Go Best Practices and Idioms](/en/learn/swe/prog-lang/golang/explanation/best-practices)
- [How to Handle Errors Effectively](/en/learn/swe/prog-lang/golang/how-to/handle-errors-effectively)
- [How to Manage Configuration](/en/learn/swe/prog-lang/golang/how-to/manage-configuration)
- [How to Write Effective Tests](/en/learn/swe/prog-lang/golang/how-to/write-effective-tests)
