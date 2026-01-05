# butler-cli

Command-line tools for repository management and automation.

## What is butler-cli?

A Go-based CLI tool that provides utilities for repository management and automation tasks. Built with Cobra CLI framework for powerful command-line interfaces.

## Quick Start

```bash
# Echo a message
butler-cli --say "hello world"

# Verbose output with timestamps
butler-cli --say "hello" --verbose

# Quiet mode (errors only)
butler-cli --say "hello" --quiet
```

## Installation

Build the CLI tool from the repository root:

```bash
cd apps/butler-cli
go build -o dist/butler-cli
```

The binary will be created at `apps/butler-cli/dist/butler-cli`.

## Global Flags

### Say Flag

Echo a message to standard output.

```bash
# Basic usage
butler-cli --say "hello world"

# Verbose output with timestamps
butler-cli --say "hello" --verbose

# Quiet mode
butler-cli --say "hello" --quiet

# Custom output format
butler-cli --say "hello" -o json
```

**What it does:**

- Prints the provided message to stdout
- Supports verbose mode with timestamps
- Supports quiet mode for minimal output
- Handles special characters (quotes, newlines, tabs, etc.)

**Global Flags:**

- `--say` - Echo a message to stdout
- `--verbose, -v` - Verbose output with timestamps
- `--quiet, -q` - Quiet mode (errors only)
- `--output, -o` - Output format: text, json, markdown
- `--no-color` - Disable colored output

## Help Commands

```bash
# General help
butler-cli --help
butler-cli help

# Command-specific help
butler-cli say --help

# Version
butler-cli --version
```

## Architecture

```
apps/butler-cli/
├── cmd/
│   ├── root.go               # Cobra root command, global flags
│   └── root_test.go          # Tests for root command
├── internal/                 # Internal business logic (future)
├── dist/                     # Built binary (gitignored)
├── main.go                   # CLI entry point
├── go.mod                    # Go module definition
├── go.sum                    # Go module checksums
├── project.json              # Nx project configuration
└── README.md                 # Documentation
```

## Development

### Build

```bash
go build -o dist/butler-cli
```

### Test

```bash
# Run all tests
go test ./...

# Run tests with coverage
go test ./... -cover

# Run tests with verbose output
go test ./... -v
```

**Test Coverage:**

- `cmd`: 100% coverage (root command tests)

### Run without building

```bash
go run main.go say "hello"
```

## Nx Integration

The CLI is integrated into the Nx workspace:

```bash
# Build via Nx
nx build butler-cli

# Run unit tests via Nx
nx test:quick butler-cli

# Run via Nx
nx run butler-cli

# Install dependencies via Nx
nx install butler-cli
```

**Available Nx Targets:**

- `build` - Build the CLI binary to `dist/`
- `test:quick` - Run unit tests (`go test ./...`)
- `run` - Run the CLI directly (`go run main.go`)
- `install` - Install Go dependencies (`go mod tidy`)

## Testing

The project includes comprehensive unit tests:

### Test Suite

**Root Command Tests (`cmd/root_test.go`):**

- Command initialization
- Global flags parsing (--verbose, --quiet, --output, --no-color, --say)
- Version output
- Say flag functionality (basic output, multiple words, empty message)
- Say flag with verbose mode (timestamps)
- Say flag with quiet mode
- Say flag with combined flags
- Say flag special characters handling
- Say flag long message handling

### Running Tests

```bash
# Run all tests
go test ./... -v

# Run tests with coverage
go test ./... -coverprofile=coverage.out

# View coverage report
go tool cover -html=coverage.out
```

## Say Flag Behavior

**Basic usage:**

```bash
butler-cli --say "hello world"
# Output: hello world
```

**With verbose flag:**

```bash
butler-cli --say "hello" --verbose
# Output: [2026-01-05 14:30:00] INFO: Executing say command
#         [2026-01-05 14:30:00] INFO: Message: hello
#         hello
```

**With quiet flag:**

```bash
butler-cli --say "hello" --quiet
# Output: hello
```

**With verbose flag:**

```bash
butler-cli say "hello" --verbose
# Output: [2025-01-05 12:00:00] INFO: Executing say command
#         [2025-01-05 12:00:00] INFO: Message: hello
#         hello
```

**With quiet flag:**

```bash
butler-cli say "hello" --quiet
# Output: hello
```

**Error case:**

```bash
butler-cli say
# Output: Error: requires at least 1 arg(s), only received 0
```

## Version History

### v0.1.0 (2026-01-05)

- Initial release
- `--say` global flag for echoing messages
- Global flags: --verbose, --quiet, --output, --no-color
- Nx integration
- Comprehensive unit tests (100% coverage)
