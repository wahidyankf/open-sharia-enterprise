# PRD: Mermaid Diagram Validation

## Command API

```
rhino-cli docs validate-mermaid [flags] [paths...]
```

When no `[paths...]` given, scans default directories: `docs/`, `governance/`, `.claude/`,
and repo root `*.md`.

### Flags

| Flag                | Default | Description                                                |
| ------------------- | ------- | ---------------------------------------------------------- |
| `--staged-only`     | false   | Only validate files staged in git (pre-commit use)         |
| `--changed-only`    | false   | Only validate files changed in `@{u}..HEAD` (pre-push use) |
| `--max-label-len N` | 30      | Max chars in a node label before violation                 |
| `--max-width N`     | 3       | Max nodes at the same rank before violation                |
| `-o, --output`      | text    | Output format: `text`, `json`, `markdown`                  |
| `-v, --verbose`     | false   | Include per-file detail in text output                     |
| `-q, --quiet`       | false   | Suppress non-error output                                  |

### Exit codes

| Code | Meaning                      |
| ---- | ---------------------------- |
| 0    | No violations                |
| 1    | One or more violations found |
| 2    | Command invocation error     |

---

## Acceptance Criteria (Gherkin)

```gherkin
@docs-validate-mermaid
Feature: Mermaid Flowchart Structural Validation

  As a documentation author
  I want to detect structural issues in Mermaid flowchart diagrams
  So that diagrams render correctly and are readable in all viewers

  # ── Rule 1: Label length ────────────────────────────────────────────────

  Scenario: A flowchart with all short node labels passes validation
    Given a markdown file containing a flowchart where every node label is within the limit
    When the developer runs docs validate-mermaid
    Then the command exits successfully
    And the output reports no violations

  Scenario: A node label exceeding the character limit is flagged
    Given a markdown file containing a flowchart with a node label longer than the limit
    When the developer runs docs validate-mermaid
    Then the command exits with a failure code
    And the output identifies the file, block, and node with the oversized label

  Scenario: The max label length is configurable via flag
    Given a markdown file containing a flowchart with a node label of 35 characters
    When the developer runs docs validate-mermaid with --max-label-len 40
    Then the command exits successfully

  # ── Rule 2: Flowchart width (perpendicular span only — depth is unlimited) ──

  Scenario: A deep sequential flowchart (long chain) passes validation regardless of depth
    Given a markdown file containing a TB flowchart with 10 nodes chained sequentially
    When the developer runs docs validate-mermaid
    Then the command exits successfully
    And the output reports no violations

  Scenario: A TB flowchart with at most 3 nodes per rank passes validation
    Given a markdown file containing a TB flowchart where no rank has more than 3 nodes
    When the developer runs docs validate-mermaid
    Then the command exits successfully
    And the output reports no violations

  Scenario: A TB flowchart with 4 nodes at one rank is flagged
    Given a markdown file containing a TB flowchart where one rank has 4 parallel nodes
    When the developer runs docs validate-mermaid
    Then the command exits with a failure code
    And the output identifies the file and block with the excessive width

  Scenario: A LR flowchart with at most 3 nodes per rank passes validation
    Given a markdown file containing an LR flowchart where no rank has more than 3 nodes
    When the developer runs docs validate-mermaid
    Then the command exits successfully
    And the output reports no violations

  Scenario: A LR flowchart with 4 nodes at one rank is flagged
    Given a markdown file containing an LR flowchart where one rank has 4 nodes at the same depth
    When the developer runs docs validate-mermaid
    Then the command exits with a failure code
    And the output identifies the file and block with the excessive width

  Scenario: The max width is configurable via flag
    Given a markdown file containing a flowchart with 4 nodes at one rank
    When the developer runs docs validate-mermaid with --max-width 5
    Then the command exits successfully

  # ── Rule 3: Single diagram per code block ────────────────────────────────

  Scenario: A mermaid block with a single flowchart passes validation
    Given a markdown file containing a mermaid code block with exactly one flowchart diagram
    When the developer runs docs validate-mermaid
    Then the command exits successfully
    And the output reports no violations

  Scenario: A mermaid block with two flowchart declarations is flagged
    Given a markdown file containing a mermaid code block with two flowchart declarations
    When the developer runs docs validate-mermaid
    Then the command exits with a failure code
    And the output identifies the file and block with multiple diagrams

  Scenario: A mermaid block using the graph keyword alias is validated identically
    Given a markdown file containing a mermaid block using the graph keyword instead of flowchart
    When the developer runs docs validate-mermaid
    Then the command validates it using the same three rules

  # ── Non-flowchart blocks ─────────────────────────────────────────────────

  Scenario: Non-flowchart mermaid blocks are ignored
    Given a markdown file containing only sequenceDiagram and classDiagram mermaid blocks
    When the developer runs docs validate-mermaid
    Then the command exits successfully
    And the output reports no violations

  Scenario: A markdown file with no mermaid blocks passes validation
    Given a markdown file containing no mermaid code blocks
    When the developer runs docs validate-mermaid
    Then the command exits successfully
    And the output reports no violations

  # ── Staged / changed-only filtering ─────────────────────────────────────

  Scenario: With --staged-only only staged markdown files are checked
    Given a markdown file with a mermaid violation that has not been staged in git
    When the developer runs docs validate-mermaid with the --staged-only flag
    Then the command exits successfully

  Scenario: With --changed-only only files changed since upstream are checked
    Given a markdown file with a mermaid violation that is not in the push range
    When the developer runs docs validate-mermaid with the --changed-only flag
    Then the command exits successfully

  # ── Output formats ────────────────────────────────────────────────────────

  Scenario: JSON output contains structured violation data
    Given a markdown file containing a flowchart with a label length violation
    When the developer runs docs validate-mermaid with -o json
    Then the output is valid JSON
    And the JSON contains the violation kind, file path, block index, and node id
```

---

## Definition of Done

- All Gherkin scenarios above have passing unit tests (godog, no build tag) and
  integration tests (`//go:build integration`).
- `nx run rhino-cli:test:quick` passes with ≥ 90% coverage.
- `nx run rhino-cli:spec-coverage` passes (all scenarios covered by step definitions).
- Pre-push hook updated; a branch with a bad `.md` diagram is rejected at push time.
- `specs/apps/rhino/cli/gherkin/README.md` feature-file table updated with new entry.
- `apps/rhino-cli/README.md` docs subcommand section updated with `validate-mermaid`.
- `governance/conventions/formatting/diagrams.md` updated to reference the new CLI
  validator (so authors know to run it, not just read the convention manually).
