# butler-cli

Command-line tools for repository management and automation.

## What is butler-cli?

A Go-based CLI tool that provides utilities for repository management and automation tasks. Built with Cobra CLI framework for powerful command-line interfaces.

## Quick Start

```bash
# Validate markdown links in the repository
butler-cli validate-links

# Validate only staged files (useful in git hooks)
butler-cli validate-links --staged-only

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

## Commands

### validate-links

Validate markdown links in the repository. Scans markdown files for broken internal links and generates a categorized report.

```bash
# Validate all markdown files
butler-cli validate-links

# Validate only staged files (useful in pre-commit hooks)
butler-cli validate-links --staged-only

# Output as JSON
butler-cli validate-links -o json

# Output as markdown report
butler-cli validate-links -o markdown

# Verbose mode
butler-cli validate-links -v

# Quiet mode (errors only)
butler-cli validate-links -q
```

**What it does:**

- Scans markdown files in core directories (docs/, governance/, .claude/, and root)
- Validates that all internal links point to existing files
- Automatically skips external URLs (http://, https://)
- Automatically skips Hugo paths (starting with /)
- Automatically skips placeholder links (path.md, target, etc.)
- Automatically skips example patterns (tu**\*, ex**\*, etc.)
- Categorizes broken links for easier fixing
- Supports multiple output formats (text, json, markdown)

**Flags:**

- `--staged-only` - Only validate staged files from git
- `-o, --output` - Output format: text, json, markdown (default: text)
- `-v, --verbose` - Verbose output with timestamps
- `-q, --quiet` - Quiet mode (errors only)

**Exit codes:**

- `0` - All links valid
- `1` - Broken links found

**Output categories:**

1. **Old ex-ru-\* prefixes** - Links containing `ex-ru-` or `ex__ru__`
2. **Missing files** - Common files like CODE_OF_CONDUCT.md, CHANGELOG.md
3. **General/other paths** - All other broken links
4. **workflows/ paths** - Links to workflows/ (not governance/workflows/)
5. **vision/ paths** - Links to vision/ (not governance/vision/)
6. **conventions README** - Links to conventions/README.md

**Example output (text):**

```
# Broken Links Report

**Total broken links**: 3

## General/other paths (3 links)

### docs/file.md

- Line 10: `../missing.md`
- Line 20: `./another-missing.md`

### README.md

- Line 5: `./nonexistent.md`
```

**Example output (JSON):**

```json
{
  "status": "failure",
  "timestamp": "2026-01-21T15:30:00+07:00",
  "total_files": 350,
  "total_links": 3920,
  "broken_count": 3,
  "duration_ms": 234,
  "categories": {
    "General/other paths": [
      {
        "source_file": "docs/file.md",
        "line_number": 10,
        "link_text": "../missing.md",
        "target_path": "/path/to/missing.md"
      }
    ]
  }
}
```

**Replaces:**

This command replaces the Python script at `scripts/validate-links.py` with a faster, more maintainable Go implementation.

## Help Commands

```bash
# General help
butler-cli --help
butler-cli help

# Command-specific help
butler-cli validate-links --help

# Version
butler-cli --version
```

## Architecture

```
apps/butler-cli/
├── cmd/
│   ├── root.go               # Cobra root command, global flags
│   ├── root_test.go          # Tests for root command
│   ├── validate_links.go     # Link validation command
│   └── validate_links_test.go # Integration tests
├── internal/
│   └── links/                # Link validation logic
│       ├── types.go          # Core type definitions
│       ├── scanner.go        # Link extraction from markdown
│       ├── scanner_test.go
│       ├── validator.go      # Link validation logic
│       ├── validator_test.go
│       ├── categorizer.go    # Link categorization
│       ├── categorizer_test.go
│       ├── reporter.go       # Output formatting
│       └── reporter_test.go
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

- `cmd`: Root command tests, validate-links integration tests
- `internal/links`: 85%+ coverage (scanner, validator, categorizer, reporter)

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

### v0.2.0 (2026-01-21)

- Added `validate-links` command for markdown link validation
- Ported from Python to Go for better performance and maintainability
- Comprehensive test suite (85%+ coverage for link validation)
- Multiple output formats (text, JSON, markdown)
- Staged-only mode for git hooks
- Replaces `scripts/validate-links.py`

### v0.1.0 (2026-01-05)

- Initial release
- `--say` global flag for echoing messages
- Global flags: --verbose, --quiet, --output, --no-color
- Nx integration
- Comprehensive unit tests (100% coverage)
