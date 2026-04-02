# ayokoding-cli

Command-line tools for ayokoding-web content validation.

## What is ayokoding-cli?

A Go-based CLI tool that validates internal links in the ayokoding-web content
directory. Provides fast link checking with support for multiple output formats
and verbose logging.

## Quick Start

```bash
# Check all internal links (default content directory)
ayokoding-cli links check

# Check specific content directory
ayokoding-cli links check --content apps/ayokoding-web/content

# JSON output for scripting or CI
ayokoding-cli links check -o json

# Quiet mode (errors/broken links only; no output on success)
ayokoding-cli links check --quiet
```

## Installation

Build the CLI tool from the repository root:

```bash
cd apps/ayokoding-cli
go build -o dist/ayokoding-cli
```

The binary will be created at `apps/ayokoding-cli/dist/ayokoding-cli`.

## Commands

### Link Validation

#### Check Internal Links

```bash
# Check all internal links (default content directory)
ayokoding-cli links check

# Check specific content directory
ayokoding-cli links check --content apps/ayokoding-web/content

# JSON output for scripting or CI
ayokoding-cli links check -o json

# Quiet mode (errors/broken links only; no output on success)
ayokoding-cli links check --quiet
```

**What it does:**

- Walks all `.md` files in the content directory
- Extracts every markdown link (`[text](target)`) from non-code-block lines
- Skips external links (`http://`, `https://`, `mailto:`, `//`) — use the
  `apps-ayokoding-web-link-checker` AI agent for those
- Skips same-page anchors (`#section`)
- Strips `#fragment` and `?query` from internal link targets before resolving
- Resolves each internal link against the content directory:
  - `/en/learn/overview` → `content/en/learn/overview.md` OR `content/en/learn/overview/_index.md`
- Reports all broken links with source file, line number, link text, and target
- **Exits with code 1** when broken links are found

**Internal vs External links:**

| Type                     | Example                  | Handled by                              |
| ------------------------ | ------------------------ | --------------------------------------- |
| Internal (Hugo absolute) | `/en/learn/swe/overview` | `ayokoding-cli links check`             |
| External URL             | `https://example.com`    | `apps-ayokoding-web-link-checker` agent |
| Same-page anchor         | `#section-name`          | Not validated                           |

**Exit codes:**

- `0` — All internal links resolve to real files
- `1` — One or more broken internal links found

**Flags:**

- `--content` — Content directory path (default: `apps/ayokoding-web/content`)

**Global Flags** (available to all commands):

- `--verbose, -v` — Verbose output with timestamps
- `--quiet, -q` — Quiet mode (errors only)
- `--output, -o` — Output format: text, json, markdown
- `--no-color` — Disable colored output

**Nx integration:**

```bash
# Run standalone (builds ayokoding-cli first automatically)
nx run ayokoding-web:links:check

# Runs automatically as part of test:quick
nx run ayokoding-web:test:quick
```

**Performance:** ~100ms for 850+ files / 3000+ links

## Help Commands

```bash
# General help
ayokoding-cli --help
ayokoding-cli help

# Command-specific help
ayokoding-cli links --help
ayokoding-cli links check --help

# Version
ayokoding-cli --version
```

## Architecture

```
apps/ayokoding-cli/
├── cmd/
│   ├── root.go               # Cobra root command, global flags
│   ├── links.go              # Link management command group
│   └── links_check.go        # links check - validate internal links
├── internal/
│   └── links/                # Link validation logic
│       └── checker.go        # Internal link checker (walk, extract, resolve)
├── dist/                     # Built binary (gitignored)
├── main.go                   # CLI entry point (Cobra execution)
├── go.mod                    # Go module definition (+ Cobra)
└── project.json              # Nx project configuration
```

## Migration Notes

### v0.4.0 → v0.5.0

**Removed**: `nav regen` and `titles update` commands.

The ayokoding-web Hugo site has been replaced by a Next.js app. The navigation
regeneration and title update commands were Hugo-specific and are no longer
needed. Only the `links check` command is retained.

- No breaking changes to `links check`
- Scripts or agents calling `nav regen` or `titles update` must be updated

### v0.3.0 → v0.4.0

**New**: `links check` command for internal link validation.

- No breaking changes
- `nx run ayokoding-web:test:quick` now runs `links:check` before the Hugo build
- Fix broken internal links to keep CI green: `nx run ayokoding-web:links:check`

## Integration with AI Agents

The `apps-ayokoding-web-link-checker` agent validates external links separately.
Internal link validation is handled by this CLI during `test:quick`.

## Pre-commit Automation

Navigation and title automation via pre-commit hook has been removed as of
v0.5.0 (ayokoding-web migrated from Hugo to Next.js). The `run-pre-commit`
target no longer exists for ayokoding-web.

The ayokoding-cli binary is still auto-built by Nx when `ayokoding-web:test:quick`
runs, because `ayokoding-web/project.json` declares `ayokoding-cli` as an
implicit dependency.

## Testing

Two test tiers consume the same Gherkin specs from `specs/apps/ayokoding/cli/` via
[godog](https://github.com/cucumber/godog) — only the step implementations differ:

| Level       | Test File Pattern                   | Step Implementation                             | Nx Target          |
| ----------- | ----------------------------------- | ----------------------------------------------- | ------------------ |
| Unit        | `cmd/{command}_test.go`             | Mocked I/O via package-level function variables | `test:unit`        |
| Integration | `cmd/{command}.integration_test.go` | Real filesystem via `/tmp` fixtures             | `test:integration` |

This pattern will be implemented as part of the CLI testing alignment plan.

### Unit Tests

```bash
# Run unit tests (includes godog BDD scenarios with mocked I/O)
nx run ayokoding-cli:test:quick

# Run unit tests directly
cd apps/ayokoding-cli
go test -v -run TestUnit ./cmd/...
```

Unit tests run godog BDD scenarios with all I/O mocked via package-level function variables.
They also cover pure function logic and edge cases not reachable from integration tests.
Coverage threshold: ≥90% line coverage.

### Integration Tests

```bash
# Run all BDD integration tests
nx run ayokoding-cli:test:integration

# Run a specific suite during development
cd apps/ayokoding-cli
go test -v -tags=integration -run TestIntegrationLinksCheck ./cmd/...
```

Integration tests use godog to drive commands in-process via `cmd.RunE()` against controlled
`/tmp` filesystem fixtures.

| Test function               | Feature file                                           | Scenarios |
| --------------------------- | ------------------------------------------------------ | --------- |
| `TestIntegrationLinksCheck` | `specs/apps/ayokoding/cli/gherkin/links-check.feature` | 4         |

The `test:integration` target is cached — it only re-runs when `cmd/**/*.go` or
`specs/apps/ayokoding/**/*.feature` files change.

## Development

### Build

```bash
go build -o dist/ayokoding-cli
```

### Lint

```bash
# Run directly
golangci-lint run ./...

# Run via Nx
nx lint ayokoding-cli
```

Linting uses the shared configuration at `.golangci.yml` in the repository root.
golangci-lint discovers it automatically by walking up parent directories from
the app's working directory.

### Run without building

```bash
go run main.go links check
```

## Nx Integration

The CLI is integrated into the Nx workspace:

```bash
# Build via Nx
nx build ayokoding-cli

# Run fast quality gate via Nx
nx run ayokoding-cli:test:quick

# Run via Nx
nx run ayokoding-cli
```

**Available Nx Targets:**

- `build` - Build the CLI binary to `dist/`
- `test:quick` - Run unit tests (`go test ./...`) with coverage validation
- `test:integration` - Run BDD integration tests (godog)
- `lint` - Static analysis via golangci-lint
- `run` - Run the CLI directly (`go run main.go`)
- `install` - Install Go dependencies (`go mod tidy`)

## References

- [Hugo Content Convention - ayokoding-web](../../governance/conventions/hugo/ayokoding.md)
- [AI Agents Convention](../../governance/development/agents/ai-agents.md)
