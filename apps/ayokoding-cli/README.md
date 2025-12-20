# ayokoding-cli

Command-line tools for ayokoding-web Hugo site maintenance and automation.

## What is ayokoding-cli?

A Go-based CLI tool that automates repetitive tasks for the ayokoding-web Hugo site. Currently provides fast navigation regeneration with plans to expand to other utilities like link checking, weight validation, and content scaffolding.

**Why Go instead of bash?** The original navigation regeneration was done by an AI agent making hundreds of file I/O calls. Moving this logic to a compiled binary provides 100-1000x performance improvement (50ms vs several seconds for 74 files).

## Installation

Build the CLI tool from the repository root:

```bash
cd apps/ayokoding-cli
go build -o dist/ayokoding-cli
```

The binary will be created at `apps/ayokoding-cli/dist/ayokoding-cli`.

## Commands

### Navigation Regeneration

Regenerate 3-layer navigation listings in all `_index.md` files:

```bash
./apps/ayokoding-cli/dist/ayokoding-cli regen-nav
```

**What it does:**

- Scans all `_index.md` files in `apps/ayokoding-web/content/`
- Excludes root language files (`en/_index.md`, `id/_index.md`)
- Extracts frontmatter from each file (preserves exactly)
- Scans file structure 3 layers deep
- Generates DFS navigation tree with proper indentation
- Sorts items by weight within each level
- Writes updated navigation back to files

**Regenerate specific directory only:**

```bash
./apps/ayokoding-cli/dist/ayokoding-cli regen-nav apps/ayokoding-web/content/en/learn
```

**Example output:**

```
Regenerating navigation for: /path/to/apps/ayokoding-web/content
---

Navigation Regeneration Complete
=================================
Processed: 74 files
Skipped:   0 files
Errors:    0 files
Duration:  56.7465ms

Completed at: 2025-12-20T22:45:34+07:00
```

### Help

View all available commands:

```bash
./apps/ayokoding-cli/dist/ayokoding-cli help
```

### Version

Check CLI version:

```bash
./apps/ayokoding-cli/dist/ayokoding-cli version
```

## Architecture

```
apps/ayokoding-cli/
├── cmd/
│   └── regen_nav.go          # Navigation regeneration command
├── internal/
│   ├── navigation/           # Navigation generation logic
│   │   ├── scanner.go        # File structure scanner (3 layers)
│   │   ├── generator.go      # Markdown DFS tree generator
│   │   └── regenerate.go     # Main orchestration logic
│   └── markdown/             # Markdown utilities
│       └── frontmatter.go    # YAML frontmatter extraction
├── dist/                     # Built binary (gitignored)
├── main.go                   # CLI entry point
├── go.mod                    # Go module definition
└── project.json              # Nx project configuration
```

## Integration with AI Agents

The `ayokoding-navigation-maker` agent now calls this CLI tool instead of implementing navigation generation directly:

```bash
# Agent workflow
if [ ! -f apps/ayokoding-cli/dist/ayokoding-cli ]; then
  cd apps/ayokoding-cli && go build -o dist/ayokoding-cli && cd ../..
fi

./apps/ayokoding-cli/dist/ayokoding-cli regen-nav
```

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

### Run without building

```bash
go run main.go regen-nav
```

## Nx Integration

The CLI is integrated into the Nx workspace:

```bash
# Build via Nx
nx build ayokoding-cli

# Test via Nx
nx test ayokoding-cli

# Run via Nx
nx run ayokoding-cli
```

## Future Commands (Planned)

- `validate-nav` - Validate navigation structure and ordering
- `validate-weights` - Check for missing or incorrect weight values
- `validate-links` - Verify internal and external links
- `scaffold-content` - Create new content files with proper frontmatter
- `check-frontmatter` - Validate frontmatter completeness

## Performance

Navigation regeneration performance comparison:

- **AI Agent (bash/awk)**: ~3-5 seconds for 74 files
- **Go CLI**: ~50ms for 74 files
- **Speedup**: 60-100x faster

## References

- [ayokoding-navigation-maker Agent](../../.claude/agents/ayokoding-navigation-maker.md)
- [Hugo Content Convention - ayokoding-web](../../docs/explanation/conventions/ex-co__hugo-content-ayokoding.md)
- [AI Agents Convention](../../docs/explanation/development/ex-de__ai-agents.md)
