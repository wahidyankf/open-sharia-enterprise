# ayokoding-cli

Command-line tools for ayokoding-web Hugo site maintenance and automation.

## What is ayokoding-cli?

A Go-based CLI tool that automates repetitive tasks for the ayokoding-web Hugo site. Provides fast navigation regeneration with support for multiple output formats and verbose logging.

**Why Go instead of bash?** The original navigation regeneration was done by an AI agent making hundreds of file I/O calls. Moving this logic to a compiled binary provides 100-1000x performance improvement (50ms vs several seconds for 74 files).

## Quick Start

```bash
# Regenerate all navigation
ayokoding-cli nav regen

# Regenerate specific directory
ayokoding-cli nav regen --path apps/ayokoding-web/content/en/learn

# Verbose output with timestamps
ayokoding-cli nav regen --verbose

# JSON output for scripting
ayokoding-cli nav regen -o json
```

## Installation

Build the CLI tool from the repository root:

```bash
cd apps/ayokoding-cli
go build -o dist/ayokoding-cli
```

The binary will be created at `apps/ayokoding-cli/dist/ayokoding-cli`.

## Commands

### Navigation Management

#### Regenerate Navigation

```bash
# Basic usage
ayokoding-cli nav regen

# Custom path (flag or positional)
ayokoding-cli nav regen --path /custom/path
ayokoding-cli nav regen /custom/path

# Verbose output
ayokoding-cli nav regen --verbose
```

**What it does:**

- Scans all `_index.md` files in `apps/ayokoding-web/content/`
- Excludes root language files (`en/_index.md`, `id/_index.md`)
- Extracts frontmatter from each file (preserves exactly)
- Scans file structure 3 layers deep
- Generates DFS navigation tree with proper indentation
- Sorts items by weight within each level
- Writes updated navigation back to files

**Flags:**

- `--path, -p` - Content directory (default: apps/ayokoding-web/content)
- `--exclude` - Files to exclude (default: en/\_index.md, id/\_index.md)

**Global Flags** (available to all commands):

- `--verbose, -v` - Verbose output with timestamps
- `--quiet, -q` - Quiet mode (errors only)
- `--output, -o` - Output format: text, json, markdown
- `--no-color` - Disable colored output

## Help Commands

```bash
# General help
ayokoding-cli --help
ayokoding-cli help

# Command-specific help
ayokoding-cli nav --help
ayokoding-cli nav regen --help

# Version
ayokoding-cli --version
```

## Architecture

```
apps/ayokoding-cli/
├── cmd/
│   ├── root.go               # Cobra root command, global flags
│   ├── nav.go                # Navigation command group
│   └── nav_regen.go          # nav regen - regenerate navigation
├── internal/
│   ├── navigation/           # Navigation generation logic
│   │   ├── scanner.go        # File structure scanner (3 layers)
│   │   ├── generator.go      # Markdown DFS tree generator
│   │   └── regenerate.go     # Main orchestration logic
│   └── markdown/             # Markdown utilities
│       └── frontmatter.go    # YAML frontmatter extraction
├── dist/                     # Built binary (gitignored)
├── main.go                   # CLI entry point (Cobra execution)
├── go.mod                    # Go module definition (+ Cobra)
└── project.json              # Nx project configuration
```

## Migration Notes

### v0.2.0 → v0.3.0 (Breaking Change)

**BREAKING**: Legacy `regen-nav` command removed. Use new grouped syntax only:

```bash
# ❌ Old syntax (NO LONGER SUPPORTED)
ayokoding-cli regen-nav [path]

# ✅ Current syntax (REQUIRED)
ayokoding-cli nav regen [--path=path]
```

**Impact**:

- All scripts and agents must be updated to use new syntax
- `ayokoding-navigation-maker` agent updated to use `nav regen`

### v0.1.0 → v0.2.0

- **Grouped subcommands**: Navigation commands under `nav` group
- **Global flags**: --verbose, --quiet, --output, --no-color
- **Output formats**: JSON and Markdown in addition to text
- **Better help**: Context-aware help with examples

## Integration with AI Agents

The `ayokoding-navigation-maker` agent calls this CLI tool using the current syntax:

```bash
./apps/ayokoding-cli/dist/ayokoding-cli nav regen
```

**Performance**: ~25ms for 74 files

## Development

### Build

```bash
go build -o dist/ayokoding-cli
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

- `internal/markdown`: 97.5% coverage
- `internal/navigation`: 88.2% coverage

**Note**: Unit tests automatically run via pre-push git hook for affected projects (see [Code Quality Convention](../../docs/explanation/development/ex-de__code-quality.md)).

### Run without building

```bash
go run main.go nav regen
```

## Nx Integration

The CLI is integrated into the Nx workspace:

```bash
# Build via Nx
nx build ayokoding-cli

# Run unit tests via Nx
nx test:quick ayokoding-cli

# Run via Nx
nx run ayokoding-cli
```

**Available Nx Targets:**

- `build` - Build the CLI binary to `dist/`
- `test:quick` - Run unit tests (`go test ./...`)
- `run` - Run the CLI directly (`go run main.go`)
- `install` - Install Go dependencies (`go mod tidy`)

## Performance

Navigation regeneration performance comparison:

- **AI Agent (bash/awk)**: ~3-5 seconds for 74 files
- **Go CLI**: ~50ms for 74 files
- **Speedup**: 60-100x faster

## References

- [ayokoding-navigation-maker Agent](../../.claude/agents/ayokoding-navigation-maker.md)
- [Hugo Content Convention - ayokoding-web](../../docs/explanation/conventions/ex-co__hugo-content-ayokoding.md)
- [AI Agents Convention](../../docs/explanation/development/ex-de__ai-agents.md)
