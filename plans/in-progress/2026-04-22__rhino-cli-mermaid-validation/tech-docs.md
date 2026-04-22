# Tech Docs: Mermaid Diagram Validation

## Architecture

Follows the existing rhino-cli layered pattern:

```
cmd/docs_validate_mermaid.go          ← cobra command, flags, I/O
internal/mermaid/
  types.go                            ← shared types (ViolationKind, Violation, ...)
  extractor.go                        ← markdown → []MermaidBlock
  parser.go                           ← mermaid source → ParsedDiagram (nodes, edges, direction)
  graph.go                            ← ParsedDiagram → rank assignment → maxWidth
  validator.go                        ← []ParsedDiagram → []Violation (applies 3 rules)
  reporter.go                         ← []Violation → text / JSON / markdown strings
```

No external Mermaid library. Both available Go options (`sammcj/go-mermaid`,
`tetrafolium/mermaid-check`) are pre-v0.1 with very low adoption — custom parser is
safer for a production validator.

---

## Types (`internal/mermaid/types.go`)

````go
type Direction string

const (
    DirectionTB Direction = "TB"
    DirectionTD Direction = "TD"
    DirectionBT Direction = "BT"
    DirectionLR Direction = "LR"
    DirectionRL Direction = "RL"
)

type ViolationKind string

const (
    ViolationLabelTooLong    ViolationKind = "label_too_long"
    ViolationWidthExceeded   ViolationKind = "width_exceeded"
    ViolationMultipleDiagrams ViolationKind = "multiple_diagrams"
)

type MermaidBlock struct {
    FilePath   string
    BlockIndex int    // 0-based index of this mermaid block within the file
    Source     string // raw content inside the ```mermaid fence
    StartLine  int    // 1-based line number of the opening fence in the file
}

type Node struct {
    ID    string
    Label string // raw label text stripped of bracket syntax and outer quotes
}

type Edge struct {
    From string // node ID
    To   string // node ID
}

type ParsedDiagram struct {
    Block     MermaidBlock
    Direction Direction
    Nodes     []Node
    Edges     []Edge
}

type Violation struct {
    Kind        ViolationKind
    FilePath    string
    BlockIndex  int
    StartLine   int
    NodeID      string // populated for ViolationLabelTooLong
    LabelText   string // populated for ViolationLabelTooLong
    LabelLen    int    // populated for ViolationLabelTooLong
    MaxLabelLen int    // populated for ViolationLabelTooLong
    ActualWidth int    // populated for ViolationWidthExceeded
    MaxWidth    int    // populated for ViolationWidthExceeded
}

type ValidationResult struct {
    FilesScanned int
    BlocksScanned int
    Violations   []Violation
}
````

---

## Markdown Extractor (`internal/mermaid/extractor.go`)

Scan file line-by-line. Detect fenced code blocks opened with ` ```mermaid `.
Collect all lines until the closing ` ``` `. Return one `MermaidBlock` per fence.

````
State machine:
  OUTSIDE  →  see "```mermaid" line  →  INSIDE (record StartLine, BlockIndex++)
  INSIDE   →  see "```" line         →  OUTSIDE (emit MermaidBlock)
  INSIDE   →  accumulate source lines
````

Edge cases:

- Indented fences (4 spaces) are NOT code fences per CommonMark — skip them.
- Fences with extra backticks (` ````mermaid `) close on matching count — for
  simplicity, match any closing ` ``` ` line (no-attribute). This covers 99% of real
  docs.
- Empty mermaid blocks: emit block with empty source; validator treats as no-diagram.

---

## Parser (`internal/mermaid/parser.go`)

### Step 1 — Detect diagram count (Rule 3)

Count matches of the regex:

```
(?m)^\s*(flowchart|graph)\s+(TB|TD|BT|LR|RL)\s*$
```

If count > 1 → emit `ViolationMultipleDiagrams` immediately; still parse first diagram
for Rules 1 and 2.

If count == 0 → block is not a flowchart; skip all validation (non-flowchart mermaid).

### Step 2 — Extract direction

Capture group 2 from the first match of the regex above.
Normalize `TD` → treated same as `TB` in the graph algorithm (identical layout axis).

### Step 3 — Extract nodes and labels

Lines that declare a standalone node or appear as endpoints in edges can carry labels.
Use a two-pass scan:

**Pass A — standalone node declarations** (line has no arrow token):

```
Bracket shapes and their label-extraction regexes (applied in order):
  Double-circle:  \w+\(\(\(([^)]*)\)\)\)
  Stadium:        \w+\(\[([^\]]*)\]\)
  Circle:         \w+\(\(([^)]*)\)\)
  Subroutine:     \w+\[\[([^\]]*)\]\]
  Cylinder:       \w+\[\(([^)]*)\)\]
  Round:          \w+\(([^)]*)\)
  Hexagon:        \w+\{\{([^}]*)\}\}
  Diamond:        \w+\{([^}]*)\}
  Asymmetric:     \w+>([^\]]*)\]
  Trapezoid:      \w+\[/([^/\\]*)[/\\]\]
  Parallelogram:  \w+\[/([^/]*)/\]  or  \w+\[\\([^\\]*)\\]
  Rectangle:      \w+\[([^\]]*)\]
  Modern API:     \w+@\{\s*[^}]*label:\s*"([^"]*)"\s*[^}]*\}
```

For each match, record `Node{ID: capture[0], Label: stripQuotes(capture[1])}`.

**Pass B — nodes declared inline in edge lines**:

Edge lines contain arrow tokens (`-->`, `---`, `-.->`, `==>`, `--o`, `--x`, `<-->`,
etc.). Split on arrow tokens. Each segment before/after the arrow may carry a node
declaration with brackets. Apply the same bracket regexes from Pass A.

If a node ID is seen multiple times (last-wins per Mermaid spec), update its label.
Nodes seen only as bare IDs (no brackets, no label) get `Label: ""` — length 0, never
flagged for Rule 1.

### Label normalization

After extracting raw label text:

1. Strip surrounding `"` or `'` quotes.
2. Strip surrounding markdown backtick blocks: ``"`text`"`` → `text`.
3. Do NOT strip HTML entities (count as characters — rendering engines expand them,
   making the visual label longer).

Label length = `len([]rune(normalizedLabel))` (Unicode-safe).

---

## Graph Algorithm (`internal/mermaid/graph.go`)

Computes the maximum number of nodes at any single rank (perpendicular span).

**Direction semantics** (confirmed against Dagre source and G6 AntV docs):

| Direction          | Each rank is a… | Perpendicular span = | Limit controls |
| ------------------ | --------------- | -------------------- | -------------- |
| `TB` / `TD` / `BT` | horizontal row  | nodes side-by-side   | diagram WIDTH  |
| `LR` / `RL`        | vertical column | nodes stacked        | diagram HEIGHT |

The rank-assignment algorithm is **direction-independent**: Dagre (the Mermaid default
layout engine) computes ranks purely from graph topology. The `rankdir` direction keyword
is applied as a post-computation rendering transform — it rotates which physical screen
axis carries rank depth vs. perpendicular span, but does not change which nodes share a
rank. Therefore `MaxWidth` in `graph.go` takes no `Direction` parameter and runs
identically for all five direction keywords.

```
Input:  nodes []Node, edges []Edge
Output: maxWidth int

Algorithm (longest-path rank assignment on DAG):

1. Build adjacency list: outgoing[u] = [v1, v2, ...]
2. Compute in-degree for each node ID.
3. Topological sort (Kahn's algorithm):
   - Queue all nodes with in-degree 0 (sources).
   - Process in BFS order; for each node u dequeued:
       rank[u] = max(rank[u], 0) if source
       for each v in outgoing[u]:
           rank[v] = max(rank[v], rank[u]+1)
           in-degree[v]--
           if in-degree[v] == 0: enqueue v
4. If any node unvisited after Kahn's (cycle detected):
   - Assign rank 0 to all unranked nodes. (Cycles are rare; safe fallback.)
5. Group node IDs by rank value.
6. maxWidth = max(len(group) for each rank)
```

**Subgraph handling**: `subgraph ... end` blocks are stripped from the source before
parsing. Node declarations and edges inside subgraphs are still included — they are
regular graph nodes with a visual grouping overlay, not a separate rank level. Per
Mermaid docs, a subgraph `direction` override is **voided** when any subgraph node
links outside the subgraph boundary (the subgraph inherits the parent direction). To
keep v1 simple and conservative, the validator treats all nodes as belonging to the
parent diagram's rank structure regardless of subgraph direction declarations.

**Disconnected nodes** (declared but no edges): assigned rank 0 (source).

**Depth is not validated**: The number of ranks (depth = longest path from source to
sink) is unbounded. A 20-step sequential chain has maxWidth = 1 at every rank and
passes Rule 2 regardless of its length. Only the perpendicular span is checked.

---

## Validator (`internal/mermaid/validator.go`)

```go
type ValidateOptions struct {
    MaxLabelLen int // default 30 — tied to Mermaid wrappingWidth:200px at 16px font (~28–30 chars)
    MaxWidth    int // default 3
}

func ValidateBlocks(blocks []MermaidBlock, opts ValidateOptions) ValidationResult
```

For each block:

1. Parse → `ParsedDiagram`. If parse error, record as internal warning (not user
   violation); continue.
2. **Rule 3**: diagram count check (done inside parser; violation emitted there).
3. **Rule 1**: for each node, if `len([]rune(node.Label)) > opts.MaxLabelLen` →
   emit `ViolationLabelTooLong`.
4. **Rule 2**: call `graph.MaxWidth(diagram.Nodes, diagram.Edges)`. If result >
   `opts.MaxWidth` → emit `ViolationWidthExceeded`.

---

## Reporter (`internal/mermaid/reporter.go`)

### Text (default)

```
✓  docs/tutorials/getting-started.md — 2 blocks, no violations
✗  docs/explanation/architecture.md — 3 blocks, 2 violations
     Block 1 (line 42): label_too_long — node "A" label "Deploy to production Kubernetes" (31 chars, max 30)
     Block 2 (line 87): width_exceeded — max width 5, limit 3
```

Summary line: `Found N violation(s) in M file(s) (K block(s) scanned).`

### JSON

```json
{
  "filesScanned": 3,
  "blocksScanned": 5,
  "violations": [
    {
      "kind": "label_too_long",
      "filePath": "docs/explanation/architecture.md",
      "blockIndex": 0,
      "startLine": 42,
      "nodeId": "A",
      "labelText": "Deploy to production Kubernetes",
      "labelLen": 31,
      "maxLabelLen": 30
    }
  ]
}
```

### Markdown

Table with columns: File | Block | Line | Kind | Detail.

---

## Command (`cmd/docs_validate_mermaid.go`)

```go
var (
    validateMermaidStagedOnly  bool
    validateMermaidChangedOnly bool
    validateMermaidMaxLabelLen int
    validateMermaidMaxWidth    int
)

var validateMermaidCmd = &cobra.Command{
    Use:   "validate-mermaid",
    Short: "Validate Mermaid flowchart diagrams in markdown files",
    ...
    RunE: runValidateMermaid,
}

func init() {
    docsCmd.AddCommand(validateMermaidCmd)
    validateMermaidCmd.Flags().BoolVar(&validateMermaidStagedOnly, "staged-only", false,
        "only validate staged files (pre-commit use)")
    validateMermaidCmd.Flags().BoolVar(&validateMermaidChangedOnly, "changed-only", false,
        "only validate files changed since upstream (pre-push use)")
    validateMermaidCmd.Flags().IntVar(&validateMermaidMaxLabelLen, "max-label-len", 30,
        "max characters in a node label (default 30 ≈ Mermaid wrappingWidth:200px at 16px font)")
    validateMermaidCmd.Flags().IntVar(&validateMermaidMaxWidth, "max-width", 3,
        "max nodes at the same rank")
}
```

File discovery mirrors `docs validate-links`:

- `--staged-only`: `git diff --cached --name-only --diff-filter=ACMR` filtered to `*.md`
- `--changed-only`: `git diff --name-only @{u}..HEAD` filtered to `*.md`; if no
  upstream (`@{u}` fails), falls back to scanning default directories
- No flags: scan default directories (`docs/`, `governance/`, `.claude/`, root `*.md`)
- Positional args: scan the given paths (files or directories)

---

## Nx Target (`apps/rhino-cli/project.json`)

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

---

## Pre-push Hook Integration (`.husky/pre-push`)

Add after the existing naming-validator block:

```bash
if echo "$CHANGED" | grep -qE '\.md$'; then
  npx nx run rhino-cli:validate:mermaid -- --changed-only
fi
```

The `-- --changed-only` passes the flag through `npx nx run` to the underlying
`rhino-cli docs validate-mermaid --changed-only` invocation.

---

## Testing Strategy

Three-level standard (matching all other rhino-cli commands):

| Level       | File pattern                                       | What it tests                                                                            |
| ----------- | -------------------------------------------------- | ---------------------------------------------------------------------------------------- |
| Unit        | `*_test.go` (no build tag)                         | Pure functions in `internal/mermaid/`; godog BDD scenarios via mock filesystem           |
| Integration | `*.integration_test.go` (`//go:build integration`) | Real temp-dir `.md` files, real git staging; drives commands in-process via `cmd.RunE()` |
| E2E         | n/a                                                | CLI apps have no E2E tier                                                                |

**Coverage target**: ≥ 90% (enforced by `test:quick` via `rhino-cli test-coverage validate`).

### Key unit test cases for `graph.go`

```
Linear chain A→B→C          → maxWidth = 1
Fan-out A→B, A→C, A→D       → maxWidth = 3 (B,C,D all rank 1)
Fan-out A→B, A→C, A→D, A→E  → maxWidth = 4
Diamond A→B, A→C, B→D, C→D  → maxWidth = 2 (B,C both rank 1)
Disconnected nodes           → rank 0, counted in rank-0 group
Cycle A→B→A                  → fallback rank 0 for both
```
