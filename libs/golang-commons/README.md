# golang-commons

Shared Go utilities for Open Sharia Enterprise CLI tools.

## Purpose

Provides common packages used across multiple Go CLI applications (`ayokoding-cli`, `oseplatform-cli`, and future Go apps).

## Packages

### `links`

Link-checking utilities for Hugo site CLIs.

**Import path**: `github.com/wahidyankf/open-sharia-enterprise/libs/golang-commons/links`

**Exports**:

- `BrokenLink` — broken link representation (source file, line, text, target)
- `CheckResult` — aggregate result (checked count, error count, errors, broken links)
- `CheckLinks(contentDir string) (*CheckResult, error)` — walks all `.md` files and validates internal links
- `JakartaTimestamp() string` — current time in Asia/Jakarta timezone (ISO 8601)
- `OutputLinksText(result, elapsed, quiet, verbose)` — human-readable stdout report
- `OutputLinksJSON(result, elapsed) error` — JSON stdout report
- `OutputLinksMarkdown(result, elapsed)` — Markdown stdout report

## Usage

```go
import "github.com/wahidyankf/open-sharia-enterprise/libs/golang-commons/links"

result, err := links.CheckLinks("apps/ayokoding-web/content")
if err != nil {
    return err
}
links.OutputLinksText(result, elapsed, quiet, verbose)
```

## Commands

```bash
# Run tests
nx run golang-commons:test:quick

# Lint
nx run golang-commons:lint

# Tidy dependencies
nx run golang-commons:install
```
