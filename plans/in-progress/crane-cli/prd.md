# crane-cli — Product Requirements Document

## Command Inventory

```
crane
├── pdf
│   ├── info       Get PDF metadata (pages, type, title, size)
│   ├── type       Detect "text" or "image" PDF
│   └── extract    Extract text from page range via pdftotext -layout
├── text
│   ├── check      Chunk-by-chunk text completeness + accuracy check
│   └── search     Normalized fuzzy search for a segment in Markdown
├── heading
│   ├── infer      Infer heading depth from layout text (section numbering or font-size)
│   └── check      Compare heading depths PDF vs Markdown
├── nesting
│   ├── infer      Infer list nesting levels from pdftotext -layout column offsets
│   └── check      Compare list nesting PDF vs Markdown
├── table
│   ├── detect     Detect columnar tables in layout text
│   └── check      Verify table presence and row/column structure in Markdown
├── figure
│   ├── detect     Find all figure/diagram references in PDF text
│   └── check      Verify each figure has Mermaid block or placeholder in Markdown
├── mermaid
│   └── validate   Validate syntax of every Mermaid block in Markdown
├── ocr
│   ├── quality    Assess OCR quality of <!-- OCR: page N -->-tagged sections
│   └── extract    OCR a single PDF page via tesseract (for manual inspection)
├── report
│   ├── init       Create audit report file with UUID chain + UTC+7 timestamp
│   └── finalize   Update report status header (In Progress → PASS/FAIL/PARTIAL)
└── skiplist
    ├── add        Add false positive entry (deduplicating)
    ├── check      Exit 0 if entry matches skip list, 1 otherwise
    └── list       Print all skip list entries for a given md-basename
```

## Output Contract

All commands output **JSON** by default (machine-readable for agents).
Pass `--human` for rich terminal output via Rich.

**Standard JSON shapes**:

- Check commands: `[{finding object}, ...]` — empty array `[]` on no findings
- Info commands: `{metadata object}`
- Path commands (report init): `{"path": "generated-reports/..."}`
- Boolean commands (skiplist check): `{"match": true|false}`

**Exit codes**:

| Code | Meaning                                                              |
| ---- | -------------------------------------------------------------------- |
| 0    | Success; no threshold findings (or boolean true)                     |
| 1    | Findings present; or boolean false; or operation failed              |
| 2    | Required system tool not found (`pdftotext`, `pdfinfo`, `tesseract`) |

## Requirements by Phase

### Phase 1: Core PDF Commands

**REQ-PDF-1** — `crane pdf info <pdf>` returns JSON object with `pages` (int), `type`
("text"\|"image"), `title` (str\|null), `size_bytes` (int). Exits 0. Exits 2 if pdfinfo
not found.

**REQ-PDF-2** — `crane pdf type <pdf>` returns JSON `{"type": "text"}` or `{"type": "image"}`.
Exits 0 for text-based, 1 for image-only. Text is detected by extracting pages 1–3 and checking
for > 100 non-whitespace characters. Exits 2 if pdftotext not found.

**REQ-PDF-3** — `crane pdf extract <pdf> --start N --end M [--output path]` extracts text from
pages N–M using `pdftotext -layout`. Writes to stdout or `--output` file. Exits 2 if pdftotext
not found.

### Phase 2: Analysis Commands

**REQ-TEXT-1** — `crane text check <pdf> <md>` extracts PDF text in `--chunk-size` (default 50)
page chunks, splits into segments (≥ 10 words), normalizes whitespace, performs fuzzy search in
Markdown text (similarity threshold 0.85), and returns JSON findings array.

**REQ-TEXT-2** — Missing segment criticality classification:
heading/section-start → CRITICAL; paragraph (≥ 10 words) → HIGH; footnote/reference → HIGH;
short phrase or page header/footer → MEDIUM.

**REQ-TEXT-3** — `crane text search <md> <segment>` performs normalized fuzzy search; returns
`{"found": true, "line": N}` or `{"found": false}`. Exits 0 if found, 1 if not.

**REQ-HEADING-1** — `crane heading check <pdf> <md>` compares each Markdown heading's `#` depth
against the PDF visual hierarchy. Depth inferred from section numbering (primary: count dots in
`1.2.3` → H4) or font-size heuristic (fallback). Returns JSON findings array.

**REQ-HEADING-2** — Heading depth criticality: off by 2+ levels or systematic family error → HIGH;
off by 1 level, isolated → MEDIUM; ambiguous PDF hierarchy (no numbering, uniform font) → LOW.

**REQ-HEADING-3** — `crane heading infer <text>` returns `{"depth": N, "confidence": "HIGH"|"MEDIUM",
"method": "section-numbering"|"font-heuristic"}`.

**REQ-NESTING-1** — `crane nesting check <pdf> <md>` extracts list item column offsets from
`pdftotext -layout` output, maps to nesting levels (minimum offset = level 1), and compares
against Markdown list nesting depth. Returns JSON findings.

**REQ-NESTING-2** — Nesting criticality: hierarchy inverted → HIGH; off by one level → MEDIUM;
single-item or ambiguous indentation → LOW.

**REQ-TABLE-1** — `crane table check <pdf> <md>` detects column-aligned tables in PDF layout
text (consecutive lines with ≥ 3 columns of whitespace-separated content), then verifies each
table is present in Markdown with matching header row and comparable row count.

**REQ-TABLE-2** — Table finding criticality: table entirely absent → CRITICAL; wrong cell data
(spot-checked 3–5 cells) → HIGH; row count mismatch → MEDIUM.

### Phase 3: Coverage + Validation Commands

**REQ-FIGURE-1** — `crane figure check <pdf> <md>` finds all figure references in PDF text
(patterns: `Figure N`, `Fig. N`, `Exhibit N`, `Diagram N`, `Chart N`) and verifies each has
either a Mermaid code block or `[FIGURE N: ...]` placeholder at a nearby location in Markdown.

**REQ-FIGURE-2** — Figure criticality: no representation at all → HIGH; placeholder present
but Mermaid was determinable → MEDIUM.

**REQ-MERMAID-1** — `crane mermaid validate <md>` finds all fenced ` ```mermaid ` blocks and
validates each: checks diagram type keyword against known set, checks bracket balance, checks
arrow syntax. Returns JSON findings. Exits 1 if any HIGH finding exists.

**REQ-MERMAID-2** — Valid diagram types: `graph`, `flowchart`, `sequenceDiagram`, `stateDiagram`,
`stateDiagram-v2`, `classDiagram`, `gantt`, `pie`, `erDiagram`, `journey`, `gitGraph`, `mindmap`,
`timeline`, `quadrantChart`, `xychart-beta`, `sankey-beta`, `block-beta`, `architecture-beta`.

**REQ-OCR-1** — `crane ocr quality <md>` scans all `<!-- OCR: page N -->` tagged sections and
for each estimates error rate using confusion-character patterns. Criticality: > 10% → CRITICAL;
5–10% → HIGH; 2–5% → MEDIUM; ≤ 2% → no finding.

### Phase 4: Workflow Commands

**REQ-REPORT-1** — `crane report init --scope S --pdf P --md M` creates a new Markdown audit
report at `generated-reports/{scope}__{uuid-chain}__{timestamp}__audit.md` with "In Progress"
status header. Returns `{"path": "generated-reports/..."}` JSON. Creates `generated-reports/`
directory if absent.

**REQ-REPORT-2** — UUID chain: reads `.execution-chain-{scope}` file; if exists and mtime < 30s,
appends `__{new-6char-uuid}` to form a chain; otherwise creates new 6-char UUID. Timestamp in
UTC+7, format `YYYY-MM-DD--HH-MM`.

**REQ-REPORT-3** — `crane report finalize <report-path> --status PASS|PARTIAL|FAIL` replaces the
"In Progress" token in the report header with the given status. Exits 1 if file not found.

**REQ-SKIPLIST-1** — `crane skiplist add <md-basename> <category> <description>` appends a new
entry to `generated-reports/pdf-to-md__{md-basename}__known-false-positives.md` using stable key
format `[{category}] | {md-basename} | {description}`. Skips write if identical key already
present (dedup). Creates the file if absent.

**REQ-SKIPLIST-2** — `crane skiplist check <md-basename> <category> <description>` exits 0 and
returns `{"match": true}` if the stable key matches any line in the skip list; exits 1 and
`{"match": false}` otherwise.

**REQ-SKIPLIST-3** — `crane skiplist list <md-basename>` returns JSON array of all skip list
entries: `[{"category": "...", "description": "..."}, ...]`.

## Acceptance Criteria (Gherkin)

Feature files live at `specs/apps/crane/gherkin/`. Step definitions live at
`apps/crane-cli/tests/unit/steps/`.

### Feature: PDF Type Detection

```gherkin
Feature: PDF type detection
  As a pdf-to-md-maker agent
  I want to reliably detect whether a PDF is text-based or image-only
  So that I choose the correct extraction path

  Scenario: Text-based PDF is detected
    Given a text-based PDF fixture exists
    When I run "crane pdf type" on the fixture
    Then the JSON output contains type "text"
    And the exit code is 0

  Scenario: Image-only PDF is detected
    Given an image-only PDF fixture exists
    When I run "crane pdf type" on the fixture
    Then the JSON output contains type "image"
    And the exit code is 1

  Scenario: pdftotext not on PATH
    Given pdftotext is not available
    When I run "crane pdf type" on any PDF
    Then stderr mentions "pdftotext"
    And the exit code is 2
```

### Feature: PDF Metadata

```gherkin
Feature: PDF metadata extraction
  As a pdf-to-md-maker agent
  I want structured PDF metadata in one command
  So that I can plan chunk sizes and validate extraction completeness

  Scenario: Get page count from text PDF
    Given a text-based PDF fixture with a known page count
    When I run "crane pdf info" on the fixture
    Then the JSON output is valid
    And the JSON field "pages" matches the known page count
    And the JSON field "type" is "text"
    And the JSON field "size_bytes" is greater than 0
```

### Feature: Text Completeness Check

```gherkin
Feature: Text completeness validation
  As a pdf-to-md-checker agent
  I want to verify that all PDF text exists in the Markdown
  So that no content is silently lost during conversion

  Scenario: Complete conversion produces no findings
    Given a PDF fixture and its complete Markdown pair
    When I run "crane text check" on the pair
    Then the JSON output is an empty array
    And the exit code is 0

  Scenario: Missing section produces a CRITICAL finding
    Given a PDF fixture and a Markdown missing one section
    When I run "crane text check" on the pair
    Then the JSON output contains a finding
    And the finding criticality is "CRITICAL"
    And the finding category is "text-completeness"
    And the exit code is 1

  Scenario: Whitespace normalization prevents false positives
    Given a PDF with multiple consecutive spaces and its normalized Markdown
    When I run "crane text check" on the pair
    Then the JSON output is an empty array
    And the exit code is 0

  Scenario: Fuzzy match accepts minor OCR spelling variation
    Given a PDF with "Organisation" and a Markdown with "Organization"
    When I run "crane text check" on the pair
    Then no CRITICAL or HIGH finding is raised for that word
```

### Feature: Heading Depth Accuracy

```gherkin
Feature: Heading level accuracy check
  As a pdf-to-md-checker agent
  I want to verify heading depths match PDF visual hierarchy
  So that document structure is faithfully represented

  Scenario: Section "2.3.1" expects H4 and MD has H3 — HIGH finding
    Given a PDF fixture where heading "2.3.1 Title" implies depth 4
    And the Markdown has that heading at depth 3
    When I run "crane heading check" on the pair
    Then a finding with criticality "HIGH" is returned
    And the finding states expected_depth 4 and found_depth 3

  Scenario: Correct heading depth produces no finding
    Given a PDF fixture where heading "2.3 Overview" implies depth 3
    And the Markdown has that heading at depth 3
    When I run "crane heading check" on the pair
    Then the JSON output is an empty array

  Scenario: Heading depth inference from section number
    Given the text "3.1.2 Details"
    When I run "crane heading infer" on that text
    Then the JSON output shows depth 4 and confidence "HIGH"
```

### Feature: Content Nesting Accuracy

```gherkin
Feature: Content nesting accuracy check
  As a pdf-to-md-checker agent
  I want to verify list nesting depths match the PDF layout
  So that nested content hierarchy is faithfully represented

  Scenario: Correct single-level list produces no finding
    Given a PDF fixture with a single-level bullet list
    And its Markdown conversion with matching single-level nesting
    When I run "crane nesting check" on the pair
    Then the JSON output is an empty array

  Scenario: Inverted nesting detected as HIGH finding
    Given a PDF fixture where nested items appear under a parent
    And a Markdown with those items at the wrong nesting level
    When I run "crane nesting check" on the pair
    Then a finding with criticality "HIGH" is returned

  Scenario: Off-by-one nesting detected as MEDIUM finding
    Given a PDF fixture with two-level nesting
    And a Markdown with the second level at depth three instead of two
    When I run "crane nesting check" on the pair
    Then a finding with criticality "MEDIUM" is returned
```

### Feature: Table Integrity Check

```gherkin
Feature: Table integrity check
  As a pdf-to-md-checker agent
  I want to verify all tables are present and correctly structured in the Markdown
  So that tabular data is not lost or corrupted during conversion

  Scenario: Present table with matching structure produces no finding
    Given a PDF fixture with a 3-column table
    And its Markdown conversion with a matching 3-column table
    When I run "crane table check" on the pair
    Then the JSON output is an empty array

  Scenario: Missing table produces a CRITICAL finding
    Given a PDF fixture with a table
    And a Markdown missing that table entirely
    When I run "crane table check" on the pair
    Then a finding with criticality "CRITICAL" is returned

  Scenario: Table with wrong row count produces a MEDIUM finding
    Given a PDF fixture with a 5-row table
    And a Markdown with a matching header but only 3 rows
    When I run "crane table check" on the pair
    Then a finding with criticality "MEDIUM" is returned

  Scenario: detect command identifies tables in layout text
    Given layout text containing a 3-column columnar table
    When I run "crane table detect" on the text
    Then the JSON output lists one table with col_count 3
```

### Feature: Figure Coverage Check

```gherkin
Feature: Figure coverage check
  As a pdf-to-md-checker agent
  I want to verify every figure reference has a representation in the Markdown
  So that diagrams are not silently dropped during conversion

  Scenario: Figure with Mermaid block produces no finding
    Given a PDF fixture referencing "Figure 1"
    And its Markdown with a Mermaid code block near that reference
    When I run "crane figure check" on the pair
    Then the JSON output is an empty array

  Scenario: Figure with placeholder produces no finding
    Given a PDF fixture referencing "Figure 2"
    And its Markdown with a "[FIGURE 2: ...]" placeholder
    When I run "crane figure check" on the pair
    Then the JSON output is an empty array

  Scenario: Figure with no representation produces a HIGH finding
    Given a PDF fixture referencing "Figure 3"
    And a Markdown with no Mermaid block or placeholder for Figure 3
    When I run "crane figure check" on the pair
    Then a finding with criticality "HIGH" is returned
```

### Feature: Mermaid Syntax Validation

```gherkin
Feature: Mermaid block validation
  As a pdf-to-md-checker agent
  I want to validate Mermaid syntax in the Markdown
  So that generated diagrams are renderable

  Scenario: Valid graph TD block produces no finding
    Given a Markdown fixture with a syntactically valid "graph TD" block
    When I run "crane mermaid validate" on the fixture
    Then the JSON output is an empty array
    And the exit code is 0

  Scenario: Unknown diagram type produces a HIGH finding
    Given a Markdown fixture with a Mermaid block starting with "xyz"
    When I run "crane mermaid validate" on the fixture
    Then a finding with criticality "HIGH" and category "mermaid-syntax" is returned
    And the exit code is 1

  Scenario: Unmatched bracket produces a HIGH finding
    Given a Markdown fixture with a Mermaid block containing unbalanced "["
    When I run "crane mermaid validate" on the fixture
    Then a finding with criticality "HIGH" is returned
    And the finding description mentions "bracket"

  Scenario: All known diagram type keywords are accepted
    Given a Markdown fixture with one block per known diagram type
    When I run "crane mermaid validate" on the fixture
    Then the JSON output is an empty array
```

### Feature: OCR Quality Assessment

```gherkin
Feature: OCR quality assessment
  As a pdf-to-md-checker agent
  I want to quantify OCR quality in image-extracted sections
  So that unreadably garbled pages are flagged for manual review

  Scenario: High error rate produces CRITICAL finding
    Given a Markdown fixture with an OCR-tagged section at 15% estimated error rate
    When I run "crane ocr quality" on the fixture
    Then a finding with criticality "CRITICAL" is returned
    And the finding includes the OCR page number

  Scenario: Clean OCR section produces no finding
    Given a Markdown fixture with an OCR-tagged section at 1% estimated error rate
    When I run "crane ocr quality" on the fixture
    Then the JSON output is an empty array

  Scenario: No OCR-tagged sections produces no finding
    Given a Markdown fixture with no OCR page tags
    When I run "crane ocr quality" on the fixture
    Then the JSON output is an empty array
```

### Feature: Audit Report Management

```gherkin
Feature: Audit report initialization
  As the pdf-to-md-quality-gate workflow
  I want consistent UUID-chained reports with UTC+7 timestamps
  So that iterations are traceable and reports are uniquely named

  Scenario: New chain creates a 6-character UUID report
    Given no existing chain file for scope "pdf-to-md"
    When I run "crane report init" with scope "pdf-to-md"
    Then a report file is created in "generated-reports/"
    And the filename matches the pattern "pdf-to-md__{6-hex}__{YYYY-MM-DD--HH-MM}__audit.md"
    And the JSON output contains the report path

  Scenario: Chain extends when chain file is fresh (< 30s)
    Given a chain file for "pdf-to-md" created 5 seconds ago with UUID "abc123"
    When I run "crane report init" with scope "pdf-to-md"
    Then the report filename contains "abc123__" followed by a new 6-hex UUID

  Scenario: Chain resets when chain file is stale (>= 30s)
    Given a chain file for "pdf-to-md" created 60 seconds ago with UUID "abc123"
    When I run "crane report init" with scope "pdf-to-md"
    Then the report filename contains only the new 6-hex UUID (no "abc123")
```

### Feature: Skip List Management

```gherkin
Feature: False positive skip list
  As a pdf-to-md-fixer agent
  I want to persist known false positives with deduplication
  So that the checker does not re-report already-accepted non-issues

  Scenario: New entry is written to the skip list file
    Given no existing skip list for "nist-sp-800-53"
    When I run "crane skiplist add nist-sp-800-53 text-completeness 'Page header on p.3'"
    Then the skip list file is created
    And it contains one entry with category "text-completeness"

  Scenario: Duplicate entry is not written twice
    Given a skip list for "nist-sp-800-53" already containing the entry for text-completeness "Page header on p.3"
    When I run "crane skiplist add" with the same arguments
    Then the skip list file contains exactly one matching entry

  Scenario: Known false positive returns match true
    Given a skip list containing "mermaid-syntax | nist-sp-800-53 | invalid arrow in Figure 3"
    When I run "crane skiplist check nist-sp-800-53 mermaid-syntax 'invalid arrow in Figure 3'"
    Then the JSON output contains match true
    And the exit code is 0

  Scenario: Unknown entry returns match false
    When I run "crane skiplist check nist-sp-800-53 text-completeness 'never added entry'"
    Then the JSON output contains match false
    And the exit code is 1
```
