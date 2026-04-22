# Delivery Checklist: Mermaid Diagram Validation

All work in `ose-public` subrepo. Run commands from `apps/rhino-cli/` unless noted.

---

## Phase 1 — Gherkin Spec

- [ ] Create `specs/apps/rhino/cli/gherkin/docs-validate-mermaid.feature`
  - [ ] Add `@docs-validate-mermaid` tag
  - [ ] Scenario: clean flowchart (all short labels, width ≤ 3) passes
  - [ ] Scenario: node label exceeding limit is flagged with file + block + node info
  - [ ] Scenario: `--max-label-len` flag overrides default (35-char label passes at 40)
  - [ ] Scenario: deep sequential chain (10 nodes in sequence) passes — depth unlimited
  - [ ] Scenario: TB flowchart with ≤ 3 nodes per rank passes
  - [ ] Scenario: TB flowchart with 4 nodes at one rank is flagged
  - [ ] Scenario: LR flowchart with ≤ 3 nodes per rank passes
  - [ ] Scenario: LR flowchart with 4 nodes at one rank is flagged
  - [ ] Scenario: `--max-width` flag overrides default (4-wide flowchart passes at 5)
  - [ ] Scenario: single diagram per block passes
  - [ ] Scenario: two `flowchart` declarations in one block are flagged
  - [ ] Scenario: `graph` keyword alias validates identically to `flowchart`
  - [ ] Scenario: non-flowchart mermaid blocks (sequenceDiagram, classDiagram) ignored
  - [ ] Scenario: markdown file with no mermaid blocks passes
  - [ ] Scenario: `--staged-only` skips unstaged files with violations
  - [ ] Scenario: `--changed-only` skips files not in push range
  - [ ] Scenario: JSON output contains structured violation fields
- [ ] Update `specs/apps/rhino/cli/gherkin/README.md`:
  - [ ] Add row to Feature Files table: `docs-validate-mermaid.feature` | `docs validate-mermaid` | count
- [ ] Verify feature file lint passes: `npm run lint:md`

---

## Phase 2 — Internal Package `internal/mermaid/`

- [ ] Create `internal/mermaid/types.go`
  - [ ] `Direction` type + constants (TB, TD, BT, LR, RL)
  - [ ] `ViolationKind` type + constants (label_too_long, width_exceeded,
        multiple_diagrams)
  - [ ] `MermaidBlock`, `Node`, `Edge`, `ParsedDiagram` structs
  - [ ] `Violation` struct with all fields per tech-docs spec
  - [ ] `ValidationResult` struct

- [ ] Create `internal/mermaid/extractor.go`
  - [ ] `ExtractBlocks(filePath string, content string) []MermaidBlock`
  - [ ] State-machine line scanner: OUTSIDE → INSIDE on ` ```mermaid `, INSIDE → OUTSIDE
        on ` ``` `
  - [ ] Track `BlockIndex` (0-based) and `StartLine` (1-based)
  - [ ] Handle empty blocks (emit with empty source)

- [ ] Create `internal/mermaid/extractor_test.go`
  - [ ] File with zero mermaid blocks → empty slice
  - [ ] File with one flowchart block → one MermaidBlock, correct StartLine
  - [ ] File with two separate mermaid blocks → two MermaidBlocks
  - [ ] Non-mermaid fenced blocks not extracted
  - [ ] Block at end of file with no trailing newline

- [ ] Create `internal/mermaid/parser.go`
  - [ ] `ParseDiagram(block MermaidBlock) (ParsedDiagram, int, error)`
    - [ ] Returns diagram count as second value (for Rule 3 detection)
  - [ ] Flowchart header regex: `(?m)^\s*(flowchart|graph)\s+(TB|TD|BT|LR|RL)\s*$`
  - [ ] Direction extraction from first header match
  - [ ] Node label extraction — Pass A (standalone declarations):
    - [ ] Rectangle `[...]`
    - [ ] Round edges `(...)`
    - [ ] Stadium `([...])`
    - [ ] Subroutine `[[...]]`
    - [ ] Cylinder `[(...)]`
    - [ ] Circle `((...))` and double-circle `(((...)))`
    - [ ] Diamond `{...}` and hexagon `{{...}}`
    - [ ] Asymmetric `>...]`
    - [ ] Parallelogram `[/.../]`, `[\...\]`
    - [ ] Trapezoid `[/...\]`, `[\/.../]`
    - [ ] Modern API `@{ ... label: "..." ... }`
  - [ ] Node label extraction — Pass B (inline in edge lines)
  - [ ] Label normalization: strip outer quotes, strip backtick-markdown wrapper
  - [ ] Last-declaration-wins for duplicate node IDs
  - [ ] Subgraph `subgraph ... end` lines skipped (not counted as nodes)
  - [ ] Edge extraction: split on arrow tokens (`-->`, `---`, `-.->`, `==>`,
        `--o`, `--x`, `<-->`, `-- text -->`, etc.)

- [ ] Create `internal/mermaid/parser_test.go`
  - [ ] Empty source → zero nodes, zero edges, diagram count 0
  - [ ] Non-flowchart block (sequenceDiagram) → diagram count 0
  - [ ] Simple `A --> B` → two nodes, one edge
  - [ ] Node with label `A[Hello World]` → label "Hello World"
  - [ ] Node with quoted label `A["Long label text"]` → label "Long label text"
  - [ ] Duplicate node ID `A[First]` then `A[Second]` → label "Second"
  - [ ] Edge with link text `A -- text --> B` → edge From=A To=B
  - [ ] Two flowchart headers → diagram count 2
  - [ ] `graph LR` keyword → Direction LR
  - [ ] Subgraph block does not produce a node with subgraph name

- [ ] Create `internal/mermaid/graph.go`
  - [ ] `MaxWidth(nodes []Node, edges []Edge) int`
  - [ ] Build adjacency list + in-degree map from edges
  - [ ] Kahn's topological sort for rank assignment
    - [ ] Sources (in-degree 0) start at rank 0
    - [ ] `rank[v] = max(rank[v], rank[u]+1)` for each edge u→v
  - [ ] Cycle fallback: unvisited nodes after Kahn's assigned rank 0
  - [ ] Group nodes by rank; return `max(len(group))`
  - [ ] Disconnected nodes (no edges) return maxWidth = total node count at rank 0

- [ ] Create `internal/mermaid/graph_test.go`
  - [ ] Empty graph → maxWidth 0
  - [ ] Single node, no edges → maxWidth 1
  - [ ] Linear chain A→B→C → maxWidth 1 (depth=2, width=1 — depth irrelevant)
  - [ ] Long sequential chain (10 nodes) → maxWidth 1 (confirms depth is unlimited)
  - [ ] Fan-out A→B, A→C, A→D → maxWidth 3
  - [ ] Fan-out A→B, A→C, A→D, A→E → maxWidth 4
  - [ ] Diamond A→B, A→C, B→D, C→D → maxWidth 2
  - [ ] Two disconnected chains → maxWidth = max of the two widths
  - [ ] Cycle A→B→A → no panic, returns fallback maxWidth

- [ ] Create `internal/mermaid/validator.go`
  - [ ] `ValidateOptions` struct (MaxLabelLen int, MaxWidth int)
  - [ ] `DefaultValidateOptions() ValidateOptions` → {30, 3}
  - [ ] `ValidateBlocks(blocks []MermaidBlock, opts ValidateOptions) ValidationResult`
  - [ ] Rule 3 check: diagram count > 1 → ViolationMultipleDiagrams
  - [ ] Skip non-flowchart blocks (diagram count == 0)
  - [ ] Rule 1 check: label length > MaxLabelLen → ViolationLabelTooLong
  - [ ] Rule 2 check: MaxWidth(nodes, edges) > MaxWidth → ViolationWidthExceeded
  - [ ] Populate all Violation fields correctly

- [ ] Create `internal/mermaid/validator_test.go`
  - [ ] Clean block → zero violations
  - [ ] Label exactly at limit → no violation; label at limit+1 → violation
  - [ ] Width exactly at limit → no violation; width at limit+1 → violation
  - [ ] Non-flowchart block → zero violations
  - [ ] Multiple diagrams block → ViolationMultipleDiagrams regardless of other rules
  - [ ] Custom opts respected (MaxLabelLen=40, MaxWidth=5)

- [ ] Create `internal/mermaid/reporter.go`
  - [ ] `FormatText(result ValidationResult, verbose, quiet bool) string`
  - [ ] `FormatJSON(result ValidationResult) (string, error)`
  - [ ] `FormatMarkdown(result ValidationResult) string`
  - [ ] Text: per-file summary lines + violation detail lines + summary footer
  - [ ] JSON: matches schema in tech-docs
  - [ ] Markdown: table with File | Block | Line | Kind | Detail columns

- [ ] Create `internal/mermaid/reporter_test.go`
  - [ ] Zero violations → success message, no table rows
  - [ ] One of each ViolationKind renders correct detail string
  - [ ] JSON output is valid JSON and contains all required fields
  - [ ] Markdown output contains table header row

---

## Phase 3 — Command

- [ ] Create `cmd/docs_validate_mermaid.go`
  - [ ] Declare `validateMermaidStagedOnly`, `validateMermaidChangedOnly`,
        `validateMermaidMaxLabelLen`, `validateMermaidMaxWidth` package-level vars
  - [ ] Declare `docsValidateMermaidFn` testable function variable
        (mirrors `docsValidateAllLinksFn` pattern)
  - [ ] Define `validateMermaidCmd` cobra command with Use, Short, Long, Example,
        SilenceErrors, RunE
  - [ ] `init()`: register under `docsCmd`, register all four flags
  - [ ] `runValidateMermaid`: resolve file list (staged / changed / default / args),
        call `docsValidateMermaidFn`, format output, return error if violations found
  - [ ] `--staged-only`: call `git diff --cached --name-only --diff-filter=ACMR`,
        filter to `*.md`
  - [ ] `--changed-only`: call `git diff --name-only @{u}..HEAD`, filter to `*.md`;
        fallback to default dirs if `@{u}` unavailable

- [ ] Create `cmd/docs_validate_mermaid_test.go`
  - [ ] Mock `docsValidateMermaidFn`
  - [ ] godog step definitions for all scenarios in feature file (unit level, no build
        tag, mock filesystem)
  - [ ] Test `--staged-only` flag wires through to file-list resolver
  - [ ] Test `--changed-only` flag wires through to file-list resolver
  - [ ] Test JSON output flag

- [ ] Create `cmd/docs_validate_mermaid.integration_test.go`
  - [ ] `//go:build integration`
  - [ ] godog integration scenarios with real temp-dir markdown files
  - [ ] Scenarios covering all three violation kinds with real file I/O
  - [ ] `--staged-only` scenario: stage clean file, leave dirty file unstaged → passes
  - [ ] `--changed-only` scenario: requires git init in temp dir with upstream branch

---

## Phase 4 — Nx Target

- [ ] Add `validate:mermaid` target to `apps/rhino-cli/project.json`:

  ```json
  "validate:mermaid": {
    "command": "CGO_ENABLED=0 go run -C apps/rhino-cli main.go docs validate-mermaid",
    "cache": true,
    "inputs": [
      "{projectRoot}/**/*.go",
      "{workspaceRoot}/docs/**/*.md",
      "{workspaceRoot}/governance/**/*.md",
      "{workspaceRoot}/.claude/**/*.md"
    ],
    "outputs": []
  }
  ```

- [ ] Verify `nx run rhino-cli:validate:mermaid` runs without error on current repo

---

## Phase 5 — Pre-push Hook

- [ ] Edit `.husky/pre-push` — add after the existing naming-validator block:

  ```bash
  if echo "$CHANGED" | grep -qE '\.md$'; then
    npx nx run rhino-cli:validate:mermaid -- --changed-only
  fi
  ```

- [ ] Manual smoke test: create branch, add a `.md` file with a label-too-long
      violation, attempt push → confirm pre-push rejects with clear error message
- [ ] Manual smoke test: same branch but fix the violation → confirm push succeeds

---

## Phase 6 — Documentation

- [ ] Add `validate-mermaid` entry to `apps/rhino-cli/README.md` docs subcommand table
      with all flags documented (including `--max-label-len` default 30, `--max-width` default 3,
      `--staged-only`, `--changed-only`)
- [ ] Update `governance/conventions/formatting/diagrams.md`:
  - [ ] Add a note in the relevant section that `rhino-cli docs validate-mermaid` enforces
        the label-length and width rules mechanically — authors should run it instead of
        checking manually
  - [ ] Note that `--max-label-len 20` matches the stricter 20-char Hugo/Hextra production limit

---

## Phase 7 — Quality Gate

- [ ] `nx run rhino-cli:test:quick` passes with ≥ 90% coverage
- [ ] `nx run rhino-cli:spec-coverage` passes (all feature scenarios covered)
- [ ] `nx run rhino-cli:test:integration` passes
- [ ] `nx run rhino-cli:typecheck` passes
- [ ] `nx run rhino-cli:lint` passes
- [ ] `nx run rhino-cli:validate:mermaid` passes on current `ose-public` repo
      (no pre-existing violations, or document and fix any found)

---

## Commit Plan

Split into four commits (Trunk Based Development):

1. `feat(rhino-cli): add internal/mermaid package (extractor, parser, graph, validator, reporter)`
2. `feat(rhino-cli): add docs validate-mermaid command with staged-only and changed-only flags`
3. `chore(rhino-cli): wire validate-mermaid into pre-push hook and Nx target`
4. `docs(rhino-cli): update specs README, rhino-cli README, and diagrams convention to reference validator`
