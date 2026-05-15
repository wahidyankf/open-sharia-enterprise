# crane-cli — Delivery Checklist

TDD workflow: **Red → Green → Refactor** per item. Mark `[x]` when done.

---

## Worktree

Worktree path: `worktrees/crane-cli/`

Provision before execution (run from repo root):

```bash
claude --worktree crane-cli
```

See [Worktree Path Convention](../../repo-governance/conventions/structure/worktree-path.md) and
[Plans Organization Convention §Worktree Specification](../../repo-governance/conventions/structure/plans.md#worktree-specification).

---

## Environment Setup

- [ ] Verify Go 1.26+: `go version` — shows go1.26.x or newer
- [ ] Verify golangci-lint: `golangci-lint --version` — exits 0; if absent: `brew install golangci-lint`
- [ ] Verify pdftotext available: `which pdftotext` — exits 0; if absent: `brew install poppler`
- [ ] Verify pdfinfo available: `which pdfinfo` — exits 0 (installed with poppler)
- [ ] Run `npm install && npm run doctor -- --fix` from repo root (worktree) — provisions all tools
- [ ] Confirm existing tests pass before starting: `npx nx affected -t test:quick` — exits 0

---

## Phase 0: Project Scaffold

### P0.1 — Directory scaffold

_Suggested executor: swe-golang-dev_

- [ ] Create `apps/crane-cli/cmd/crane/main.go` — cobra root command skeleton with `Use: "crane"`,
      `Short` description, and empty subcommand groups registered: `pdf`, `text`, `heading`,
      `nesting`, `table`, `figure`, `mermaid`, `ocr`, `report`, `skiplist`
- [ ] Create `apps/crane-cli/internal/commands/` — one empty `.go` file with package declaration
      per subcommand group (pdf.go, text.go, … skiplist.go)
- [ ] Create `apps/crane-cli/internal/core/` — one empty `.go` stub per analysis module
- [ ] Create `apps/crane-cli/internal/adapters/` — pdftotext.go, pdfinfo.go, tesseract.go stubs
- [ ] Create `apps/crane-cli/internal/models/` — finding.go, pdf_metadata.go, report.go with type
      declarations exactly as specified in tech-docs.md
- [ ] Create `apps/crane-cli/tests/unit/` — one `_test.go` file per core module (all empty bodies)
- [ ] Create `apps/crane-cli/tests/integration/pdf_commands_test.go` — build tag `//go:build Integration`
- [ ] Create `apps/crane-cli/tests/bdd/suite_test.go` — godog runner (see tech-docs.md)
- [ ] Create `apps/crane-cli/tests/bdd/steps/init.go` — `InitializeScenario` wires all step packages
- [ ] Create `apps/crane-cli/tests/fixtures/` — empty dir with `.gitkeep`

- [ ] Verify scaffold: `find apps/crane-cli -name '*.go' | wc -l` returns ≥ 25;
      `test -f apps/crane-cli/tests/fixtures/.gitkeep` exits 0

### P0.2 — Configuration files

_Suggested executor: swe-golang-dev_

- [ ] Write `apps/crane-cli/go.mod` — module path `github.com/wahidyankf/ose-public/apps/crane-cli`,
      Go 1.26, dependencies as specified in tech-docs.md (cobra, godog, testify, uuid, go-diff)
- [ ] Write `apps/crane-cli/project.json` — exactly as specified in tech-docs.md (`build`, `dev`,
      `test:quick`, `test:unit`, `test:integration`, `lint`, `typecheck`, `spec-coverage` targets)
- [ ] Write `apps/crane-cli/README.md` — one-paragraph description, `go build`, `crane --help` instructions
- [ ] Run `cd apps/crane-cli && go mod tidy` — downloads deps, creates go.sum; exits 0

### P0.3 — Bootstrap verification

_Suggested executor: swe-golang-dev_

- [ ] `nx run crane-cli:build` passes — `dist/crane` binary created; `file dist/crane` shows ELF/Mach-O
- [ ] `go run ./cmd/crane/... --help` shows all 10 subcommand groups
- [ ] `nx run crane-cli:test:unit` completes (0 tests, 0 failures)
- [ ] `nx run crane-cli:lint` passes on scaffold

---

## Phase 1: Core PDF Commands

### P1.1 — Adapter: pdfinfo

_Suggested executor: swe-golang-dev_

- [ ] **RED** Write `tests/bdd/steps/pdf_steps.go` — `InitializePDFSteps` with step stubs
      for all `pdf-commands.feature` scenarios; `go test -run BDD ./tests/bdd/...` fails
- [ ] Write `internal/adapters/pdfinfo.go` — `GetInfo(pdf string) (map[string]string, error)`;
      `exec.Command("pdfinfo", pdf)`; returns `ErrToolNotFound` when exit code 127
- [ ] **RED** Write `tests/unit/pdf_adapter_test.go::TestUnitGetInfo_ParsesPageCount` — fails
- [ ] **GREEN** `TestUnitGetInfo_ParsesPageCount` passes (mock exec.Command output)
- [ ] Write `TestUnitGetInfo_ToolNotFound` — returns `ErrToolNotFound` when binary absent
- [ ] **GREEN** all pdfinfo adapter unit tests pass

### P1.2 — Adapter: pdftotext

_Suggested executor: swe-golang-dev_

- [ ] Write `internal/adapters/pdftotext.go`:
  - `Sample(pdf string, pages int) (string, error)` — `exec.Command("pdftotext", "-f", "1", "-l", strconv.Itoa(pages), pdf, "-")`
  - `Extract(pdf string, start, end int) (string, error)` — `exec.Command("pdftotext", "-layout", …, "-")`
  - Returns `ErrToolNotFound` on exit 127
- [ ] **RED** Write `tests/unit/pdftotext_adapter_test.go::TestUnitSample_ReturnsText` — fails
- [ ] **GREEN** all pdftotext adapter unit tests pass (mock exec via interface)
- [ ] **REFACTOR** Extract `runPDFTool(name string, args ...string) (string, error)` shared helper

### P1.3 — `crane pdf info` command

_Suggested executor: swe-golang-dev_

- [ ] **RED** Add step implementations in `pdf_steps.go` for `pdf-commands.feature`
      "PDF metadata extraction" scenario; `go test -run BDD` fails with "undefined step"
- [ ] Write `internal/commands/pdf.go` `infoCmd` — calls pdfinfo adapter + pdftotext sample;
      marshals `PDFMetadata` to JSON; exits 0 (exit 2 on `ErrToolNotFound`)
- [ ] **GREEN** "PDF metadata extraction" BDD scenario passes
- [ ] **REFACTOR** Extract `buildMetadata(pdf string) (models.PDFMetadata, error)` helper

### P1.4 — `crane pdf type` command

_Suggested executor: swe-golang-dev_

- [ ] **RED** Add step implementations for "PDF type detection" scenarios; `go test -run BDD` fails
- [ ] Write `internal/commands/pdf.go` `typeCmd` — calls `pdftotext.Sample`; counts non-whitespace
      chars; prints `{"type":"text"}` or `{"type":"image"}`; exits 0/1 (exit 2 on tool absent)
- [ ] **GREEN** all "PDF type detection" BDD scenarios pass

### P1.5 — `crane pdf extract` command

_Suggested executor: swe-golang-dev_

- [ ] **RED** Write `tests/unit/pdf_commands_test.go::TestUnitExtract_WritesToStdout` — fails
- [ ] Write `internal/commands/pdf.go` `extractCmd` — calls `pdftotext.Extract`; writes to
      stdout or `--output` file; exits 2 on `ErrToolNotFound`
- [ ] **GREEN** `TestUnitExtract_WritesToStdout` passes

### P1.6 — Phase 1 gate

- [ ] `nx run crane-cli:test:unit` — all Phase 1 BDD + unit tests pass
- [ ] `nx run crane-cli:lint` clean
- [ ] `nx run crane-cli:test:integration` — `crane pdf info tests/fixtures/sample-text.pdf`
      returns JSON with correct page count (requires pdftotext on PATH)

---

## Phase 2: Analysis Commands

### P2.1 — Core: text checker

_Suggested executor: swe-golang-dev_

- [ ] **RED** Write `tests/bdd/steps/text_steps.go` — step stubs for all `text-check.feature`
      scenarios; `go test -run BDD` fails
- [ ] Write `internal/core/text_checker.go`:
  - `Normalize(text string) string`
  - `Similarity(a, b string) float64`
  - `SegmentIsPresent(segment, mdText string) bool`
  - `ClassifyMissing(segment string) models.Criticality`
  - `CheckText(pdfChunks []string, mdText string) []models.Finding`
- [ ] **RED** Write `tests/unit/text_checker_test.go`:
  - `TestUnitNormalize_CollapsesWhitespace`
  - `TestUnitNormalize_StripsLeadingTrailing`
  - `TestUnitSimilarity_ExactIs1`
  - `TestUnitSimilarity_BelowThreshold`
  - `TestUnitFuzzyMatch_AcceptsMinorVariation`
  - `TestUnitMissingHeading_IsCritical`
  - `TestUnitMissingParagraph_IsHigh`
  - `TestUnitPresentText_NoFinding`
- [ ] **GREEN** all text checker unit tests pass
- [ ] **REFACTOR** Extract `windowMatch(seg, text string) bool` private helper
- [ ] Write `internal/commands/text.go` `checkCmd` and `searchCmd`
- [ ] **GREEN** all `text-check.feature` BDD scenarios pass

### P2.2 — Core: heading checker

_Suggested executor: swe-golang-dev_

- [ ] **RED** Write `tests/bdd/steps/heading_steps.go` — step stubs; fails
- [ ] Write `internal/core/heading_checker.go`:
  - `InferDepthFromNumbering(heading string) (depth int, confidence string, ok bool)`
  - `ExtractMDHeadings(mdText string) []HeadingEntry` — (lineNo, depth, text)
  - `CheckHeadings(pdfLayoutText, mdText string) []models.Finding`
- [ ] **RED** Write `tests/unit/heading_checker_test.go`:
  - `TestUnitInferDepth_SingleNumber` — "1. Title" → 2
  - `TestUnitInferDepth_TwoComponents` — "2.3 Title" → 3
  - `TestUnitInferDepth_ThreeComponents` — "2.3.1 Title" → 4
  - `TestUnitInferDepth_NoNumber` — "Introduction" → ok=false
  - `TestUnitInferDepth_Appendix` — "A. Appendix" → 2
  - `TestUnitWrongDepth_OffByTwo_IsHigh`
  - `TestUnitCorrectDepth_NoFinding`
- [ ] **GREEN** all heading unit tests pass
- [ ] Write `internal/commands/heading.go` `inferCmd` and `checkCmd`
- [ ] **GREEN** all `heading-check.feature` BDD scenarios pass

### P2.3 — Core: nesting checker

_Suggested executor: swe-golang-dev_

- [ ] **RED** Write `tests/bdd/steps/nesting_steps.go` — step stubs; fails
- [ ] Write `internal/core/nesting_checker.go`:
  - `ExtractNestingLevels(layoutText string) []NestingItem` — (colOffset, depth)
  - `CheckNesting(pdfLayoutText, mdText string) []models.Finding`
- [ ] **RED** Write `tests/unit/nesting_checker_test.go`:
  - `TestUnitExtractNesting_SingleLevel`
  - `TestUnitExtractNesting_TwoLevels`
  - `TestUnitWrongNesting_OffByOne_IsMedium`
  - `TestUnitInvertedNesting_IsHigh`
- [ ] **GREEN** all nesting unit tests pass
- [ ] Write `internal/commands/nesting.go` `inferCmd` and `checkCmd`
- [ ] **GREEN** all `nesting-check.feature` BDD scenarios pass

### P2.4 — Core: table checker

_Suggested executor: swe-golang-dev_

- [ ] **RED** Write `tests/bdd/steps/table_steps.go` — step stubs; fails
- [ ] Write `internal/core/table_checker.go`:
  - `DetectTables(layoutText string) []TableSpec`
  - `CheckTables(pdfLayoutText, mdText string) []models.Finding`
- [ ] **RED** Write `tests/unit/table_checker_test.go`:
  - `TestUnitDetect3ColTable_ReturnsOne`
  - `TestUnitDetectProse_ReturnsEmpty`
  - `TestUnitDetectNumericOnlyTable`
  - `TestUnitMissingTable_IsCritical`
  - `TestUnitPresentTable_NoFinding`
  - `TestUnitWrongRowCount_IsMedium`
- [ ] **GREEN** all table unit tests pass
- [ ] Write `internal/commands/table.go` `detectCmd` and `checkCmd`
- [ ] **GREEN** all `table-check.feature` BDD scenarios pass

### P2.5 — Phase 2 gate

- [ ] `nx run crane-cli:test:unit` — all Phase 2 unit + BDD tests pass
- [ ] `nx run crane-cli:lint` clean

---

## Phase 3: Coverage + Validation Commands

### P3.1 — Core: figure checker

_Suggested executor: swe-golang-dev_

- [ ] **RED** Write `tests/bdd/steps/figure_steps.go` — step stubs; fails
- [ ] Write `internal/core/figure_checker.go`:
  - `DetectFigures(text string) []FigureRef` — regex: `Figure \d+`, `Fig\. \d+`, `Exhibit \d+`,
    `Diagram \d+`, `Chart \d+`
  - `CheckFigures(pdfText, mdText string) []models.Finding`
- [ ] **RED** Write `tests/unit/figure_checker_test.go`:
  - `TestUnitDetectFigureN_Pattern`
  - `TestUnitDetectFigDotN_Pattern`
  - `TestUnitNoFigures_ReturnsEmpty`
  - `TestUnitMissingFigure_IsHigh`
  - `TestUnitPlaceholder_SatisfiesCoverage`
  - `TestUnitMermaidBlock_SatisfiesCoverage`
- [ ] **GREEN** all figure unit tests pass
- [ ] Write `internal/commands/figure.go` `detectCmd` and `checkCmd`
- [ ] **GREEN** all `figure-check.feature` BDD scenarios pass

### P3.2 — Core: mermaid validator

_Suggested executor: swe-golang-dev_

- [ ] **RED** Write `tests/bdd/steps/mermaid_steps.go` — step stubs; fails
- [ ] Write `internal/core/mermaid_validator.go`:
  - `validMermaidTypes` map (18 types — see tech-docs.md)
  - `ValidateMermaidBlock(content string) (bool, string)`
  - `ExtractBlocks(mdText string) []MermaidBlock` — (lineNo, content)
  - `ValidateMD(mdText string) []models.Finding`
- [ ] **RED** Write `tests/unit/mermaid_validator_test.go`:
  - `TestUnitValidGraphTD_NoFinding`
  - `TestUnitValidFlowchartLR_NoFinding`
  - `TestUnitAllKnownTypes_Accepted` — table-driven over validMermaidTypes
  - `TestUnitUnknownType_IsHigh`
  - `TestUnitEmptyBlock_IsHigh`
  - `TestUnitUnmatchedBracket_IsHigh`
  - `TestUnitUnmatchedParen_IsHigh`
  - `TestUnitFinding_IncludesLineNumber`
- [ ] **GREEN** all mermaid unit tests pass
- [ ] Write `internal/commands/mermaid.go` `validateCmd`
- [ ] **GREEN** all `mermaid-validate.feature` BDD scenarios pass

### P3.3 — Core: OCR assessor

_Suggested executor: swe-golang-dev_

- [ ] **RED** Write `tests/bdd/steps/ocr_steps.go` — step stubs; fails
- [ ] Write `internal/core/ocr_assessor.go`:
  - `ocrErrorPatterns` slice (4 patterns — see tech-docs.md)
  - `EstimateOCRErrorRate(text string) float64`
  - `ExtractOCRSections(mdText string) []OCRSection` — (pageNo, text) from `<!-- OCR: page N -->`
  - `CheckOCRQuality(mdText string) []models.Finding`
- [ ] **RED** Write `tests/unit/ocr_assessor_test.go`:
  - `TestUnitCleanText_RateNearZero`
  - `TestUnitRepeatedL_RaisesRate`
  - `TestUnitNonASCIIRuns_RaisesRate`
  - `TestUnitRateAbove10Pct_IsCritical`
  - `TestUnitRate5to10Pct_IsHigh`
  - `TestUnitRate2to5Pct_IsMedium`
  - `TestUnitRateBelow2Pct_NoFinding`
  - `TestUnitNoOCRTags_ReturnsEmpty`
  - `TestUnitFinding_IncludesPageNumber`
- [ ] **GREEN** all OCR assessor unit tests pass
- [ ] Write `internal/commands/ocr.go` `qualityCmd` and `extractCmd`
- [ ] **GREEN** all `ocr-quality.feature` BDD scenarios pass

### P3.4 — Phase 3 gate

- [ ] `nx run crane-cli:test:unit` — all Phase 3 unit + BDD tests pass; coverage ≥ 85%
- [ ] `nx run crane-cli:lint` clean

---

## Phase 4: Workflow Commands

### P4.1 — Core: report manager

_Suggested executor: swe-golang-dev_

- [ ] **RED** Write `tests/bdd/steps/report_steps.go` — step stubs; fails
- [ ] Write `internal/core/report_manager.go`:
  - `GetOrExtendChain(scope string) string`
  - `UTC7Timestamp() string`
  - `InitReport(scope, pdf, md string) (string, error)` — creates file, returns path
  - `FinalizeReport(reportPath, status string) error`
- [ ] **RED** Write `tests/unit/report_manager_test.go`:
  - `TestUnitNewChain_Is6HexChars`
  - `TestUnitChain_ExtendsWhenFresh` — mock `time.Now` to simulate 5s age
  - `TestUnitChain_ResetsWhenStale` — mock `time.Now` to simulate 60s age
  - `TestUnitUTC7Timestamp_Format` — assert `YYYY-MM-DD--HH-MM` format
  - `TestUnitInitReport_CreatesFileInGeneratedReports`
  - `TestUnitInitReport_FilenameMatchesPattern`
  - `TestUnitFinalizeReport_ReplacesInProgressWithPass`
  - `TestUnitFinalizeReport_ErrorsOnMissingFile`
- [ ] **GREEN** all report manager unit tests pass
- [ ] Write `internal/commands/report.go` `initCmd` and `finalizeCmd`
- [ ] **GREEN** all `report-management.feature` BDD scenarios pass

### P4.2 — Core: skiplist manager

_Suggested executor: swe-golang-dev_

- [ ] **RED** Write `tests/bdd/steps/skiplist_steps.go` — step stubs; fails
- [ ] Write `internal/core/skiplist_manager.go`:
  - `StableKey(mdBasename, category, description string) string`
  - `Add(mdBasename, category, description string) (bool, error)` — true if added, false if dup
  - `Check(mdBasename, category, description string) (bool, error)`
  - `List(mdBasename string) ([]models.SkipListEntry, error)`
- [ ] **RED** Write `tests/unit/skiplist_manager_test.go`:
  - `TestUnitStableKey_Format`
  - `TestUnitAdd_CreatesFile`
  - `TestUnitAdd_ReturnsTrueOnNewEntry`
  - `TestUnitAdd_ReturnsFalseOnDuplicate`
  - `TestUnitAdd_DoesNotDuplicateFileLine`
  - `TestUnitCheck_ReturnsTrueOnKnownEntry`
  - `TestUnitCheck_ReturnsFalseOnUnknown`
  - `TestUnitList_ReturnsAllEntries`
  - `TestUnitList_ReturnsEmptyOnMissingFile`
- [ ] **GREEN** all skiplist manager unit tests pass
- [ ] Write `internal/commands/skiplist.go` `addCmd`, `checkCmd`, `listCmd`
- [ ] **GREEN** all `skiplist-management.feature` BDD scenarios pass

### P4.3 — Phase 4 gate

- [ ] `nx run crane-cli:test:unit` — all Phase 4 unit + BDD tests pass; coverage ≥ 85%
- [ ] `nx run crane-cli:lint` clean
- [ ] `nx run crane-cli:spec-coverage` — passes (all feature scenarios implemented)
- [ ] `crane --help` shows all 10 subcommand groups with correct subcommands listed

---

## Phase 5: Agent Integration

### P5.1 — Update pdf-to-md-maker

_Suggested executor: swe-golang-dev_

- [ ] Edit `.claude/agents/pdf-to-md-maker.md` Step 1: replace `pdftotext` sample + `wc -c` with
      `crane pdf type "$PDF_FILE"`; verify `grep -q 'crane pdf type' .claude/agents/pdf-to-md-maker.md` exits 0
- [ ] Edit `.claude/agents/pdf-to-md-maker.md` Step 2a: replace bash loop with
      `crane pdf extract "$PDF_FILE" --start $FIRST --end $LAST`; verify `grep -q 'crane pdf extract'` exits 0
- [ ] Edit `.claude/agents/pdf-to-md-maker.md`: replace `pdfinfo | awk` page count with
      `crane pdf info "$PDF_FILE" | jq .pages`; verify `grep -q 'crane pdf info'` exits 0
- [ ] Verify no remaining raw `pdftotext -layout` or `wc -c` analysis lines:
      `grep -n 'wc -c\|pdftotext -layout' .claude/agents/pdf-to-md-maker.md` exits 1

### P5.2 — Update pdf-to-md-checker

_Suggested executor: swe-golang-dev_

- [ ] Edit `.claude/agents/pdf-to-md-checker.md` Step 0: replace bash UUID + timestamp + file
      create with `crane report init --scope pdf-to-md --pdf "$PDF_FILE" --md "$MD_FILE" | jq -r .path`;
      verify `grep -q 'crane report init' .claude/agents/pdf-to-md-checker.md` exits 0
- [ ] Edit Step 2: replace `grep -F` segment loop with `crane text check "$PDF_FILE" "$MD_FILE"`;
      verify `grep -q 'crane text check'` exits 0
- [ ] Edit Step 3: replace manual grep heading extraction with `crane heading check "$PDF_FILE" "$MD_FILE"`;
      verify `grep -q 'crane heading check'` exits 0
- [ ] Edit Step 4: replace column-offset inspection with `crane nesting check "$PDF_FILE" "$MD_FILE"`;
      verify `grep -q 'crane nesting check'` exits 0
- [ ] Edit Step 5: replace brittle table grep with `crane table check "$PDF_FILE" "$MD_FILE"`;
      verify `grep -q 'crane table check'` exits 0
- [ ] Edit Step 6: replace figure grep count with `crane figure check "$PDF_FILE" "$MD_FILE"`;
      verify `grep -q 'crane figure check'` exits 0
- [ ] Edit Step 7: replace manual Mermaid checks with `crane mermaid validate "$MD_FILE"`;
      verify `grep -q 'crane mermaid validate'` exits 0
- [ ] Edit Step 8: replace absent OCR logic with `crane ocr quality "$MD_FILE"`;
      verify `grep -q 'crane ocr quality'` exits 0
- [ ] Edit skip list loading: replace file grep with `crane skiplist check "$MD_BASENAME"`;
      verify `grep -q 'crane skiplist check'` exits 0
- [ ] Confirm no remaining inline `grep -F` analysis: `grep -c 'grep -F' .claude/agents/pdf-to-md-checker.md` outputs 0

### P5.3 — Update pdf-to-md-fixer

_Suggested executor: swe-golang-dev_

- [ ] Edit `.claude/agents/pdf-to-md-fixer.md`: replace all re-validation `grep -F` calls with
      `crane text search "$MD_FILE" "$SEGMENT"`; verify `grep -q 'crane text search'` exits 0
- [ ] Edit `.claude/agents/pdf-to-md-fixer.md`: replace `echo >>` skip list appends with
      `crane skiplist add "$MD_BASENAME" ...`; verify `grep -q 'crane skiplist add'` exits 0
- [ ] Edit `.claude/agents/pdf-to-md-fixer.md`: replace fix report init bash with `crane report init`;
      verify `grep -q 'crane report init'` exits 0

### P5.4 — Update workflow documentation

_Suggested executor: swe-golang-dev_

- [ ] Edit `repo-governance/workflows/content/pdf-to-md-quality-gate.md` Tool Dependencies section:
      add `nx run crane-cli:build` (builds `apps/crane-cli/dist/crane`) and `export PATH="$PWD/apps/crane-cli/dist:$PATH"` and `crane --version` verification line;
      verify `grep -q 'crane --version' repo-governance/workflows/content/pdf-to-md-quality-gate.md` exits 0
- [ ] Edit Validation Dimensions Summary table: add crane command column for each dimension;
      verify `grep -q 'crane text check' repo-governance/workflows/content/pdf-to-md-quality-gate.md` exits 0

### P5.5 — End-to-end validation

- [ ] Copy or create a small freely-licensed text-based PDF at
      `apps/crane-cli/tests/fixtures/sample-text.pdf`; verify:
      `crane pdf type apps/crane-cli/tests/fixtures/sample-text.pdf | jq -r .type` outputs `text`
- [ ] Run the `pdf-to-md-quality-gate` workflow on the fixture:
      `pdf_file=apps/crane-cli/tests/fixtures/sample-text.pdf mode=normal`;
      verify the resulting audit report in `generated-reports/` has status "PASS"
- [ ] Run `grep -rn 'crane ' .claude/agents/pdf-to-md-*.md` — output shows crane commands;
      `grep -rn 'grep -F\|wc -c\|openssl rand' .claude/agents/pdf-to-md-*.md` outputs nothing

---

## Pre-Push Local Gate

Before every push to `origin/main`, run and fix **all** failures — including preexisting issues
not caused by current changes (root cause orientation; do not suppress):

```bash
npx nx affected -t typecheck lint test:quick spec-coverage
```

Fix everything the gate reports before pushing. A CI failure after push means the pre-push gate
was not fully clean — investigate and fix the root cause.

## Post-Push CI Verification

After pushing to `origin/main`:

```bash
gh run list --branch main --limit 5
gh run watch <run-id>
```

Monitor until green. If any workflow fails: investigate root cause, fix locally, re-run the
local gate, push the fix. Do not proceed to next delivery item until CI is green.

## Commit Guidelines

Commit thematically per Conventional Commits format. Do not bundle unrelated changes. Suggested
split:

- `feat(crane-cli): scaffold Go module — Phase 0`
- `feat(crane-cli): add PDF commands — Phase 1`
- `feat(crane-cli): add analysis commands — Phase 2`
- `feat(crane-cli): add coverage+validation commands — Phase 3`
- `feat(crane-cli): add workflow commands — Phase 4`
- `feat(crane-cli): integrate into pdf-to-md agents — Phase 5`

## Plan Archival

When all Final Gate items are checked:

```bash
git mv plans/in-progress/crane-cli plans/done/2026-MM-DD__crane-cli
```

Update `plans/done/README.md` and `plans/in-progress/README.md` accordingly.

---

## Final Gate

- [ ] **F1** All 5 phases complete; all items above checked
- [ ] **F2** `nx run crane-cli:test:quick` passes — coverage ≥ 85%, rhino-cli validates threshold
- [ ] **F3** `nx run crane-cli:test:integration` passes with pdftotext on PATH
- [ ] **F4** `nx run crane-cli:lint` clean — zero golangci-lint violations
- [ ] **F5** `nx run crane-cli:spec-coverage` passes — all Gherkin scenarios implemented
- [ ] **F6** `crane --help` shows all 10 subcommand groups
- [ ] **F7** `crane pdf type apps/crane-cli/tests/fixtures/sample-text.pdf | jq -r .type` outputs `text`
- [ ] **F8** pdf-to-md agents contain no inline `grep -F`, `pdfinfo | awk`, or UUID bash
- [ ] **F9** `nx affected -t typecheck lint test:quick spec-coverage` passes (pre-push gate)
- [ ] **F10** Post-push CI workflows green on `origin/main`
- [ ] **F11** Plan archival complete: folder moved to `plans/done/YYYY-MM-DD__crane-cli/`
