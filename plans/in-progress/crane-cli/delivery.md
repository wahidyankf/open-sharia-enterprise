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

- [ ] Verify .NET 8 SDK: `dotnet --version` — shows 8.x.x or newer
- [ ] Verify Fantomas: `dotnet fantomas --version` — exits 0; if absent:
      `dotnet tool install --global fantomas`
- [ ] Verify tesseract (for OCR tests only): `tesseract --version` — exits 0;
      if absent: `brew install tesseract` (macOS) or `apt-get install tesseract-ocr` (Linux)
- [ ] Run `npm install && npm run doctor -- --fix` from repo root (worktree) — provisions all tools
- [ ] Confirm existing tests pass before starting: `npx nx affected -t test:quick` — exits 0

---

## Phase 0: Project Scaffold

### P0.1 — Directory scaffold

_Suggested executor: swe-fsharp-dev_

- [ ] Create directory structure: `apps/crane-cli/{Commands,Core,Adapters,Models}/`
- [ ] Create `apps/crane-cli/Models/Finding.fs` — Criticality, Confidence, Category DUs + Finding
      record exactly as specified in tech-docs.md.
      Verify: `test -f apps/crane-cli/Models/Finding.fs` exits 0
- [ ] Create `apps/crane-cli/Models/PdfMetadata.fs` — PdfMetadata record.
      Verify: `test -f apps/crane-cli/Models/PdfMetadata.fs` exits 0
- [ ] Create `apps/crane-cli/Models/Report.fs` — SkipListEntry record.
      Verify: `test -f apps/crane-cli/Models/Report.fs` exits 0
- [ ] Create `apps/crane-cli/Adapters/PdfAdapter.fs` — PdfPig wrapper stub (module declaration only).
      Verify: `test -f apps/crane-cli/Adapters/PdfAdapter.fs` exits 0
- [ ] Create `apps/crane-cli/Adapters/OcrAdapter.fs` — TesseractOCR wrapper stub.
      Verify: `test -f apps/crane-cli/Adapters/OcrAdapter.fs` exits 0
- [ ] Create `apps/crane-cli/Core/` — one `.fs` stub per module: TextChecker, HeadingChecker,
      NestingChecker, TableChecker, FigureChecker, MermaidValidator, OcrAssessor, ReportManager,
      SkiplistManager (module declaration + empty `let placeholder () = ()`)
- [ ] Create `apps/crane-cli/Commands/` — one `.fs` stub per command group (10 files)
- [ ] Create `apps/crane-cli/Program.fs` — minimal Argu root with `[<EntryPoint>]`
- [ ] Create `apps/crane-cli/tests/unit/Steps/` — one empty `.fs` step file per domain
      (PdfSteps, TextSteps, HeadingSteps, NestingSteps, TableSteps, FigureSteps, MermaidSteps,
      OcrSteps, ReportSteps, SkiplistSteps)
- [ ] Create `apps/crane-cli/tests/unit/Suite.fs` — TickSpec xUnit runner with fake adapter
      (see tech-docs.md Unit Suite pattern)
- [ ] Create `apps/crane-cli/tests/integration/Suite.fs` — TickSpec xUnit runner with real adapter
- [ ] Create `apps/crane-cli/tests/integration/Steps/PdfSteps.fs` — real PdfPig steps
- [ ] Create `apps/crane-cli/tests/integration/Steps/OcrSteps.fs` — real TesseractOCR steps
- [ ] Create `apps/crane-cli/tests/integration/fixtures/` — empty dir with `.gitkeep`

- [ ] Verify scaffold: `find apps/crane-cli -name '*.fs' | wc -l` returns ≥ 30;
      `test -f apps/crane-cli/tests/integration/fixtures/.gitkeep` exits 0

### P0.2 — Configuration files

_Suggested executor: swe-fsharp-dev_

- [ ] Write `apps/crane-cli/crane-cli.fsproj` — exactly as specified in tech-docs.md:
      `net8.0`, `PublishSingleFile`, `SelfContained`, all `<Compile>` items in dependency order,
      NuGet packages: Argu 6.2.5, PdfPig 0.1.14, TesseractOCR 5.5.2,
      FSharp.SystemTextJson 1.4.36, F23.StringSimilarity 7.0.1
- [ ] Write `apps/crane-cli/.config/dotnet-tools.json` — exactly as specified in tech-docs.md:
      `altcover.global` 9.0.102 as the sole dotnet tool (NO `<PackageReference Include="altcover">` in any .fsproj; Fantomas runs as a globally-installed tool, not via dotnet-tools.json)
- [ ] Write `apps/crane-cli/tests/unit/crane-cli-unit-tests.fsproj` — exactly as specified in
      tech-docs.md: xUnit, TickSpec 2.0.4, ProjectReference to crane-cli.fsproj (altcover installed
      via dotnet-tools.json, not PackageReference)
- [ ] Write `apps/crane-cli/tests/integration/crane-cli-integration-tests.fsproj` — same deps
      as unit test project; ProjectReference to crane-cli.fsproj
- [ ] Write `apps/crane-cli/project.json` — exactly as specified in tech-docs.md
      (`build`, `dev`, `test:quick`, `test:unit`, `test:integration`, `lint`, `typecheck`,
      `spec-coverage`; tag `lang:fsharp`)
- [ ] Write `apps/crane-cli/README.md` — one-paragraph description,
      `dotnet run --project crane-cli.fsproj -- --help` instructions
- [ ] Run `cd apps/crane-cli && dotnet restore` — downloads NuGet packages; exits 0

- [ ] Verify `lang:fsharp` tag is the correct tag for F# projects in this repo:
      `grep -r 'lang:fsharp' apps/ose-app-be/project.json` —
      confirms convention (update tag to match if different)

### P0.3 — Gherkin feature files

_Suggested executor: swe-fsharp-dev_

Write each feature file at `specs/apps/crane/gherkin/<name>.feature`, containing the `Feature:`
header and the scenarios specified in `prd.md §Acceptance Criteria` for that command domain.

- [ ] Write `specs/apps/crane/gherkin/pdf-commands.feature` — Feature header + all PDF command
      scenarios from prd.md (pdf info, pdf type)
- [ ] Write `specs/apps/crane/gherkin/text-check.feature` — Feature header + all text completeness
      scenarios from prd.md
- [ ] Write `specs/apps/crane/gherkin/heading-check.feature` — Feature header + all heading
      check scenarios from prd.md
- [ ] Write `specs/apps/crane/gherkin/nesting-check.feature` — Feature header + all nesting
      check scenarios from prd.md
- [ ] Write `specs/apps/crane/gherkin/table-check.feature` — Feature header + all table check
      scenarios from prd.md
- [ ] Write `specs/apps/crane/gherkin/figure-check.feature` — Feature header + all figure check
      scenarios from prd.md
- [ ] Write `specs/apps/crane/gherkin/mermaid-validate.feature` — Feature header + all mermaid
      validation scenarios from prd.md
- [ ] Write `specs/apps/crane/gherkin/ocr-quality.feature` — Feature header + all OCR quality
      scenarios from prd.md
- [ ] Write `specs/apps/crane/gherkin/report-management.feature` — Feature header + all report
      management scenarios from prd.md
- [ ] Write `specs/apps/crane/gherkin/skiplist-management.feature` — Feature header + all
      skiplist management scenarios from prd.md

- [ ] Verify all 10 feature files exist:
      `find specs/apps/crane/gherkin -name '*.feature' | wc -l` returns 10

### P0.4 — Test fixtures

_Suggested executor: swe-fsharp-dev_

Acquire the PDF fixture required by all integration tests from Phase 1 onward.

- [ ] Download a public-domain text-based PDF:

  ```bash
  mkdir -p apps/crane-cli/tests/integration/fixtures
  curl -L "https://www.w3.org/WAI/WCAG21/Techniques/pdf/sample.pdf" \
    -o apps/crane-cli/tests/integration/fixtures/sample-text.pdf
  ```

  If that URL is unavailable, substitute any small public-domain text PDF (e.g., from
  <https://www.w3.org/WAI/WCAG21/Techniques/pdf/>) and update this step with the actual URL used.

- [ ] Create the Markdown pair from the fixture using `crane` after Phase 1 is implemented:

  ```bash
  crane pdf extract apps/crane-cli/tests/integration/fixtures/sample-text.pdf \
    --output apps/crane-cli/tests/integration/fixtures/sample-text.md
  ```

  If crane is not yet built, create `sample-text.md` with representative content from the
  PDF manually as a placeholder — integration tests will replace it in Phase 1.

- [ ] Verify fixture is present: `test -f apps/crane-cli/tests/integration/fixtures/sample-text.pdf` exits 0

### P0.5 — CI workflow

_Suggested executor: swe-fsharp-dev_

Note: The quality gate (typecheck + lint + test:quick + spec-coverage) is handled automatically
by the existing `pr-quality-gate.yml` via the `lang:fsharp` tag — no new workflow file needed
for quality. Only the integration job requires a new file (needs tesseract for OCR tests).

- [ ] Write `.github/workflows/crane-cli-integration.yml` — exactly as specified in tech-docs.md
      (single `integration` job using `.github/actions/setup-dotnet` +
      `apt-get install tesseract-ocr libtesseract-dev` + `npx nx run crane-cli:test:integration`)
- [ ] Verify workflow syntax:
      `python3 -c "import sys,yaml; yaml.safe_load(open('.github/workflows/crane-cli-integration.yml'))"` exits 0
- [ ] Confirm path filters cover `apps/crane-cli/**`:
      `grep -c 'apps/crane-cli' .github/workflows/crane-cli-integration.yml` returns ≥ 1
- [ ] Confirm `lang:fsharp` in project.json so `pr-quality-gate.yml` picks up crane-cli:
      `grep -q 'lang:fsharp' apps/crane-cli/project.json` exits 0

### P0.6 — Bootstrap verification

_Suggested executor: swe-fsharp-dev_

- [ ] `npx nx run crane-cli:build` passes — `dotnet build` compiles without errors
- [ ] `dotnet run --project apps/crane-cli/crane-cli.fsproj -- --help` shows all 10 subcommand groups
- [ ] `npx nx run crane-cli:test:unit` completes (0 tests, 0 failures — stubs only)
- [ ] `npx nx run crane-cli:lint` passes on scaffold

---

## Standalone Binary Requirement

crane-cli must be self-contained: AI agents running `crane` do not install additional runtime
dependencies. This is achieved via:

1. **PdfPig** — pure managed .NET; automatically bundled in `PublishSingleFile + SelfContained` ✓
2. **TesseractOCR** — NuGet package ships native `libleptonica` + `libtesseract` binaries for
   Linux/macOS/Windows via platform-specific runtime packages; bundled automatically ✓
3. **Tessdata (English OCR model)** — include `tessdata/eng.traineddata` as embedded resource or
   content file in the project; OR use `Tesseract.Data.English` NuGet package if available;
   required for `crane ocr` commands to work without external tessdata directory

Implementation in `crane-cli.fsproj`:
```xml
<!-- Bundle tessdata as content file alongside the binary -->
<ItemGroup>
  <Content Include="tessdata/eng.traineddata">
    <CopyToOutputDirectory>PreserveNewest</CopyToOutputDirectory>
    <CopyToPublishDirectory>Always</CopyToPublishDirectory>
  </Content>
</ItemGroup>
```

In `OcrAdapter.fs`, point TesseractOCR at the bundled tessdata path relative to the assembly:
```fsharp
let private tessDataPath =
    let assemblyDir = Path.GetDirectoryName(System.Reflection.Assembly.GetExecutingAssembly().Location)
    Path.Combine(assemblyDir, "tessdata")
```

- [ ] Download English tessdata during P0.4:
      `mkdir -p apps/crane-cli/tessdata && curl -L "https://github.com/tesseract-ocr/tessdata/raw/main/eng.traineddata" -o apps/crane-cli/tessdata/eng.traineddata`
- [ ] Edit `apps/crane-cli/crane-cli.fsproj`: add the tessdata content ItemGroup exactly as shown
      in the Standalone Binary Requirement section above (the `<Content Include="tessdata/eng.traineddata">` block).
      Verify: `grep -q 'tessdata/eng.traineddata' apps/crane-cli/crane-cli.fsproj` exits 0
- [ ] Build standalone binary for current platform and verify it works:

  ```bash
  dotnet publish apps/crane-cli/crane-cli.fsproj -c Release \
    -r $(dotnet --info | grep 'RID:' | awk '{print $2}' | head -1) \
    --self-contained true -o apps/crane-cli/dist/
  ```

  Then verify: `apps/crane-cli/dist/crane ocr quality sample.md` works without system tesseract
  on PATH (uses bundled tessdata)

---

## Phase 1: Core PDF Commands

### P1.1 — Adapter: PdfPig (text extraction + metadata)

_Suggested executor: swe-fsharp-dev_

- [ ] **RED** Write step stubs in `tests/unit/Steps/PdfSteps.fs` for all `pdf-commands.feature`
      scenarios; `dotnet test tests/unit/crane-cli-unit-tests.fsproj` fails with pending steps
- [ ] Write `Adapters/PdfAdapter.fs`:
  - `type IPdfAdapter` — interface with `GetMetadata`, `SampleText`, `ExtractPages` members
  - `type RealPdfAdapter` — PdfPig implementation
  - `type FakePdfAdapter` — in-memory implementation for unit tests
  - `GetMetadata(path: string) : Result<PdfMetadata, string>`
  - `SampleText(path: string, pageCount: int) : Result<string, string>`
  - `ExtractPages(path: string, startPage: int, endPage: int) : Result<string, string>`
- [ ] **RED** Write unit tests in `tests/unit/Steps/PdfSteps.fs`:
      `TestUnitGetMetadata_ParsesPageCount` — fails
- [ ] **GREEN** `TestUnitGetMetadata_ParsesPageCount` passes using FakePdfAdapter
- [ ] Write `TestUnitSampleText_ReturnsText`, `TestUnitExtractPages_ReturnsRange`
- [ ] **GREEN** all PdfAdapter unit tests pass

### P1.2 — `crane pdf info` command

_Suggested executor: swe-fsharp-dev_

- [ ] **RED** Add step implementations in `PdfSteps.fs` for `pdf-commands.feature`
      "PDF metadata extraction" scenario; tests fail
- [ ] Write `Commands/PdfCommands.fs` `infoCmd` — calls `PdfAdapter.GetMetadata`; marshals
      `PdfMetadata` to JSON; exits 0
- [ ] **GREEN** "PDF metadata extraction" BDD scenario passes
- [ ] **REFACTOR** Extract `outputJson` helper for consistent JSON serialization

### P1.3 — `crane pdf type` command

_Suggested executor: swe-fsharp-dev_

- [ ] **RED** Add step implementations for "PDF type detection" scenarios; tests fail
- [ ] Write `Commands/PdfCommands.fs` `typeCmd` — calls `PdfAdapter.SampleText`; counts
      non-whitespace chars; prints `{"type":"text"}` or `{"type":"image"}`; exits 0/1
- [ ] **GREEN** all "PDF type detection" BDD scenarios pass

### P1.4 — `crane pdf extract` command

_Suggested executor: swe-fsharp-dev_

- [ ] **RED** Write `TestUnitExtract_WritesToStdout` — fails
- [ ] Write `Commands/PdfCommands.fs` `extractCmd` — calls `PdfAdapter.ExtractPages`; writes to
      stdout or `--output` file
- [ ] **GREEN** `TestUnitExtract_WritesToStdout` passes

### P1.5 — Phase 1 gate

- [ ] `npx nx run crane-cli:test:unit` — all Phase 1 BDD + unit tests pass
- [ ] `npx nx run crane-cli:lint` clean
- [ ] `npx nx run crane-cli:test:integration` — integration TickSpec suite runs against real
      `apps/crane-cli/tests/integration/fixtures/sample-text.pdf`; PDF type detection and info
      scenarios pass with actual PdfPig output (no subprocess needed)
- [ ] Create `tests/integration/fixtures/sample-text.md` from fixture if not yet done:
      `crane pdf extract apps/crane-cli/tests/integration/fixtures/sample-text.pdf --output apps/crane-cli/tests/integration/fixtures/sample-text.md`

---

## Phase 2: Analysis Commands

### P2.1 — Core: text checker

_Suggested executor: swe-fsharp-dev_

- [ ] **RED** Write step stubs in `tests/unit/Steps/TextSteps.fs` for `text-check.feature`; fails
- [ ] **RED** Write unit tests in `tests/unit/Steps/TextSteps.fs` (no implementation yet — all fail):
  - `TestUnitNormalize_CollapsesWhitespace`
  - `TestUnitNormalize_StripsLeadingTrailing`
  - `TestUnitSimilarity_ExactIs1`
  - `TestUnitSimilarity_BelowThreshold`
  - `TestUnitFuzzyMatch_AcceptsMinorVariation`
  - `TestUnitMissingHeading_IsCritical`
  - `TestUnitMissingParagraph_IsHigh`
  - `TestUnitPresentText_NoFinding`
- [ ] Write `Core/TextChecker.fs`:
  - `normalize (text: string) : string`
  - `computeSimilarity (a: string) (b: string) : float`
  - `segmentIsPresent (segment: string) (mdText: string) : bool`
  - `classifyMissing (segment: string) : Criticality`
  - `checkText (pdfChunks: string list) (mdText: string) : Finding list`
- [ ] **GREEN** all text checker unit tests pass
- [ ] **REFACTOR** Extract `windowMatch (seg: string) (text: string) : bool` private helper
- [ ] Write `Commands/TextCommands.fs` `checkCmd` and `searchCmd`
- [ ] **GREEN** all `text-check.feature` BDD scenarios pass

### P2.2 — Core: heading checker

_Suggested executor: swe-fsharp-dev_

- [ ] **RED** Write step stubs in `tests/unit/Steps/HeadingSteps.fs`; fails
- [ ] **RED** Write unit tests (no implementation yet — all fail):
  - `TestUnitInferDepth_SingleNumber` — "1. Title" → 2
  - `TestUnitInferDepth_TwoComponents` — "2.3 Title" → 3
  - `TestUnitInferDepth_ThreeComponents` — "2.3.1 Title" → 4
  - `TestUnitInferDepth_NoNumber` — "Introduction" → None
  - `TestUnitWrongDepth_OffByTwo_IsHigh`
  - `TestUnitCorrectDepth_NoFinding`
- [ ] Write `Core/HeadingChecker.fs`:
  - `inferDepthFromNumbering (heading: string) : (int * string) option`
  - `extractMdHeadings (mdText: string) : HeadingEntry list`
  - `checkHeadings (pdfLayoutText: string) (mdText: string) : Finding list`
- [ ] **GREEN** all heading unit tests pass
- [ ] **REFACTOR** Ensure pure function signatures with no I/O side effects in HeadingChecker module
- [ ] Write `Commands/HeadingCommands.fs` `inferCmd` and `checkCmd`
- [ ] **GREEN** all `heading-check.feature` BDD scenarios pass

### P2.3 — Core: nesting checker

_Suggested executor: swe-fsharp-dev_

- [ ] **RED** Write step stubs in `tests/unit/Steps/NestingSteps.fs`; fails
- [ ] **RED** Write unit tests (no implementation yet — all fail):
  - `TestUnitExtractNesting_SingleLevel`
  - `TestUnitExtractNesting_TwoLevels`
  - `TestUnitWrongNesting_OffByOne_IsMedium`
  - `TestUnitInvertedNesting_IsHigh`
- [ ] Write `Core/NestingChecker.fs`:
  - `extractNestingLevels (layoutText: string) : NestingItem list`
  - `checkNesting (pdfLayoutText: string) (mdText: string) : Finding list`
- [ ] **GREEN** all nesting unit tests pass
- [ ] **REFACTOR** Ensure pure function signatures with no I/O side effects in NestingChecker module
- [ ] Write `Commands/NestingCommands.fs` `inferCmd` and `checkCmd`
- [ ] **GREEN** all `nesting-check.feature` BDD scenarios pass

### P2.4 — Core: table checker

_Suggested executor: swe-fsharp-dev_

- [ ] **RED** Write step stubs in `tests/unit/Steps/TableSteps.fs`; fails
- [ ] **RED** Write unit tests (no implementation yet — all fail):
  - `TestUnitDetect3ColTable_ReturnsOne`
  - `TestUnitDetectProse_ReturnsEmpty`
  - `TestUnitMissingTable_IsCritical`
  - `TestUnitPresentTable_NoFinding`
  - `TestUnitWrongRowCount_IsMedium`
- [ ] Write `Core/TableChecker.fs`:
  - `detectTables (layoutText: string) : TableSpec list`
  - `checkTables (pdfLayoutText: string) (mdText: string) : Finding list`
- [ ] **GREEN** all table unit tests pass
- [ ] **REFACTOR** Ensure pure function signatures with no I/O side effects in TableChecker module
- [ ] Write `Commands/TableCommands.fs` `detectCmd` and `checkCmd`
- [ ] **GREEN** all `table-check.feature` BDD scenarios pass

### P2.5 — Phase 2 gate

- [ ] `npx nx run crane-cli:test:unit` — all Phase 2 unit + BDD tests pass
- [ ] `npx nx run crane-cli:lint` clean

---

## Phase 3: Coverage + Validation Commands

### P3.1 — Core: figure checker

_Suggested executor: swe-fsharp-dev_

- [ ] **RED** Write step stubs in `tests/unit/Steps/FigureSteps.fs`; fails
- [ ] **RED** Write unit tests (no implementation yet — all fail):
  - `TestUnitDetectFigureN_Pattern`
  - `TestUnitDetectFigDotN_Pattern`
  - `TestUnitNoFigures_ReturnsEmpty`
  - `TestUnitMissingFigure_IsHigh`
  - `TestUnitPlaceholder_SatisfiesCoverage`
  - `TestUnitMermaidBlock_SatisfiesCoverage`
- [ ] Write `Core/FigureChecker.fs`:
  - `detectFigures (text: string) : FigureRef list`
  - `checkFigures (pdfText: string) (mdText: string) : Finding list`
- [ ] **GREEN** all figure unit tests pass
- [ ] **REFACTOR** Ensure pure function signatures with no I/O side effects in FigureChecker module
- [ ] Write `Commands/FigureCommands.fs` `detectCmd` and `checkCmd`
- [ ] **GREEN** all `figure-check.feature` BDD scenarios pass

### P3.2 — Core: mermaid validator

_Suggested executor: swe-fsharp-dev_

- [ ] **RED** Write step stubs in `tests/unit/Steps/MermaidSteps.fs`; fails
- [ ] **RED** Write unit tests (no implementation yet — all fail):
  - `TestUnitValidGraphTD_NoFinding`
  - `TestUnitAllKnownTypes_Accepted` — table-driven over validTypes
  - `TestUnitUnknownType_IsHigh`
  - `TestUnitEmptyBlock_IsHigh`
  - `TestUnitUnmatchedBracket_IsHigh`
  - `TestUnitUnmatchedParen_IsHigh`
  - `TestUnitFinding_IncludesLineNumber`
- [ ] Write `Core/MermaidValidator.fs`:
  - `validTypes` Set (18 types — see tech-docs.md)
  - `validateBlock (content: string) : Result<unit, string>`
  - `extractBlocks (mdText: string) : MermaidBlock list`
  - `validateMd (mdText: string) : Finding list`
- [ ] **GREEN** all mermaid unit tests pass
- [ ] **REFACTOR** Ensure pure function signatures with no I/O side effects in MermaidValidator module
- [ ] Write `Commands/MermaidCommands.fs` `validateCmd`
- [ ] **GREEN** all `mermaid-validate.feature` BDD scenarios pass

### P3.3 — Core: OCR assessor

_Suggested executor: swe-fsharp-dev_

- [ ] **RED** Write step stubs in `tests/unit/Steps/OcrSteps.fs`; fails
- [ ] **RED** Write unit tests (no implementation yet — all fail):
  - `TestUnitCleanText_RateNearZero`
  - `TestUnitRepeatedL_RaisesRate`
  - `TestUnitNonASCIIRuns_RaisesRate`
  - `TestUnitRateAbove10Pct_IsCritical`
  - `TestUnitRate5to10Pct_IsHigh`
  - `TestUnitRate2to5Pct_IsMedium`
  - `TestUnitRateBelow2Pct_NoFinding`
  - `TestUnitNoOCRTags_ReturnsEmpty`
- [ ] Write `Core/OcrAssessor.fs`:
  - `ocrErrorPatterns` array (4 patterns — see tech-docs.md)
  - `estimateOCRErrorRate (text: string) : float`
  - `extractOCRSections (mdText: string) : OCRSection list`
  - `checkOCRQuality (mdText: string) : Finding list`
- [ ] **GREEN** all OCR assessor unit tests pass
- [ ] **REFACTOR** Ensure pure function signatures with no I/O side effects in OcrAssessor module
- [ ] Write `Commands/OcrCommands.fs` `qualityCmd` and `extractCmd`
- [ ] **GREEN** all `ocr-quality.feature` BDD scenarios pass

### P3.4 — Phase 3 gate

- [ ] `npx nx run crane-cli:test:unit` — all Phase 3 unit + BDD tests pass; coverage ≥ 95%
- [ ] `npx nx run crane-cli:lint` clean

---

## Phase 4: Workflow Commands

### P4.1 — Core: report manager

_Suggested executor: swe-fsharp-dev_

- [ ] **RED** Write step stubs in `tests/unit/Steps/ReportSteps.fs`; fails
- [ ] **RED** Write unit tests (no implementation yet — all fail):
  - `TestUnitNewChain_Is6HexChars`
  - `TestUnitChain_ExtendsWhenFresh`
  - `TestUnitChain_ResetsWhenStale`
  - `TestUnitUTC7Timestamp_Format` — assert `yyyy-MM-dd--HH-mm` format
  - `TestUnitInitReport_CreatesFileInGeneratedReports`
  - `TestUnitFinalizeReport_ReplacesInProgressWithPass`
  - `TestUnitFinalizeReport_ErrorsOnMissingFile`
- [ ] Write `Core/ReportManager.fs`:
  - `getOrExtendChain (scope: string) : string`
  - `utc7Timestamp () : string`
  - `initReport (scope: string) (pdf: string) (md: string) : Result<string, string>`
  - `finalizeReport (reportPath: string) (status: string) : Result<unit, string>`
- [ ] **GREEN** all report manager unit tests pass
- [ ] **REFACTOR** Ensure pure function signatures with no I/O side effects in ReportManager module
- [ ] Write `Commands/ReportCommands.fs` `initCmd` and `finalizeCmd`
- [ ] **GREEN** all `report-management.feature` BDD scenarios pass

### P4.2 — Core: skiplist manager

_Suggested executor: swe-fsharp-dev_

- [ ] **RED** Write step stubs in `tests/unit/Steps/SkiplistSteps.fs`; fails
- [ ] **RED** Write unit tests (no implementation yet — all fail):
  - `TestUnitStableKey_Format`
  - `TestUnitAdd_CreatesFile`
  - `TestUnitAdd_ReturnsTrueOnNewEntry`
  - `TestUnitAdd_ReturnsFalseOnDuplicate`
  - `TestUnitAdd_DoesNotDuplicateFileLine`
  - `TestUnitCheck_ReturnsTrueOnKnownEntry`
  - `TestUnitCheck_ReturnsFalseOnUnknown`
  - `TestUnitList_ReturnsAllEntries`
  - `TestUnitList_ReturnsEmptyOnMissingFile`
- [ ] Write `Core/SkiplistManager.fs`:
  - `stableKey (mdBasename: string) (category: string) (description: string) : string`
  - `add (mdBasename: string) (category: string) (description: string) : Result<bool, string>`
  - `check (mdBasename: string) (category: string) (description: string) : Result<bool, string>`
  - `list (mdBasename: string) : Result<SkipListEntry list, string>`
- [ ] **GREEN** all skiplist manager unit tests pass
- [ ] **REFACTOR** Ensure pure function signatures with no I/O side effects in SkiplistManager module
- [ ] Write `Commands/SkiplistCommands.fs` `addCmd`, `checkCmd`, `listCmd`
- [ ] **GREEN** all `skiplist-management.feature` BDD scenarios pass

### P4.3 — Phase 4 gate

- [ ] `npx nx run crane-cli:test:unit` — all Phase 4 unit + BDD tests pass; coverage ≥ 95%
- [ ] `npx nx run crane-cli:lint` clean
- [ ] `npx nx run crane-cli:spec-coverage` — passes (all feature scenarios implemented)
- [ ] `crane --help` shows all 10 subcommand groups with correct subcommands listed

---

## Phase 5: Agent Integration

> **Development note**: During Phase 5, the self-contained binary may not yet be built.
> Agents can invoke crane via `dotnet run` instead:
> `dotnet run --project apps/crane-cli/crane-cli.fsproj -- <cmd>` (equivalent to running the
> binary). Build the binary first with `npx nx run crane-cli:build` if needed.

### P5.1 — Update pdf-to-md-maker

_Suggested executor: swe-fsharp-dev_

- [ ] Edit `.claude/agents/pdf-to-md-maker.md` Step 1: replace `pdftotext` sample + `wc -c` with
      `crane pdf type "$PDF_FILE"`; verify `grep -q 'crane pdf type' .claude/agents/pdf-to-md-maker.md` exits 0
- [ ] Edit `.claude/agents/pdf-to-md-maker.md` Step 2a: replace bash loop with
      `crane pdf extract "$PDF_FILE" --start $FIRST --end $LAST`; verify `grep -q 'crane pdf extract'` exits 0
- [ ] Edit `.claude/agents/pdf-to-md-maker.md`: replace `pdfinfo | awk` page count with
      `crane pdf info "$PDF_FILE" | jq .pages`; verify `grep -q 'crane pdf info'` exits 0
- [ ] Verify no remaining raw `pdftotext -layout` or `wc -c` analysis lines:
      `grep -n 'wc -c\|pdftotext -layout' .claude/agents/pdf-to-md-maker.md` exits 1

### P5.2 — Update pdf-to-md-checker

_Suggested executor: swe-fsharp-dev_

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
- [ ] Confirm no remaining inline `grep -F` analysis:
      `grep -c 'grep -F' .claude/agents/pdf-to-md-checker.md` outputs 0

### P5.3 — Update pdf-to-md-fixer

_Suggested executor: swe-fsharp-dev_

- [ ] Edit `.claude/agents/pdf-to-md-fixer.md`: replace all re-validation `grep -F` calls with
      `crane text search "$MD_FILE" "$SEGMENT"`; verify `grep -q 'crane text search'` exits 0
- [ ] Edit `.claude/agents/pdf-to-md-fixer.md`: replace `echo >>` skip list appends with
      `crane skiplist add "$MD_BASENAME" ...`; verify `grep -q 'crane skiplist add'` exits 0
- [ ] Edit `.claude/agents/pdf-to-md-fixer.md`: replace fix report init bash with `crane report init`;
      verify `grep -q 'crane report init'` exits 0

### P5.4 — Update workflow documentation

- [ ] Edit `repo-governance/workflows/content/pdf-to-md-quality-gate.md` Tool Dependencies section:
      add `npx nx run crane-cli:build` (builds `apps/crane-cli/dist/crane`) and
      `export PATH="$PWD/apps/crane-cli/dist:$PATH"` and `crane --version` verification line;
      verify `grep -q 'crane --version' repo-governance/workflows/content/pdf-to-md-quality-gate.md` exits 0
- [ ] Edit `repo-governance/workflows/content/pdf-to-md-quality-gate.md`: in the
      "Validation Dimensions Summary" table (or equivalent table listing all 8 dimensions),
      add a "crane command" column with the appropriate `crane <subcmd>` for each dimension.
      Verify: `grep -q 'crane text check\|crane heading check' repo-governance/workflows/content/pdf-to-md-quality-gate.md`
      exits 0

### P5.5 — End-to-end validation

- [ ] Verify fixture is present:
      `test -f apps/crane-cli/tests/integration/fixtures/sample-text.pdf` exits 0 and
      `crane pdf type apps/crane-cli/tests/integration/fixtures/sample-text.pdf | jq -r .type` outputs `text`
- [ ] Run the `pdf-to-md-quality-gate` workflow on the fixture:
      `pdf_file=apps/crane-cli/tests/integration/fixtures/sample-text.pdf mode=normal`;
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

Monitor until green. Verify both workflows pass:

- `pr-quality-gate.yml` — .NET quality gate (typecheck, lint, test:quick, spec-coverage for `lang:fsharp`)
- `crane-cli-integration.yml` — integration tests (real PdfPig + tesseract OCR)

If any workflow fails: investigate root cause, fix locally, re-run the
local gate, push the fix. Do not proceed to next delivery item until CI is green.

## Commit Guidelines

Commit thematically per Conventional Commits format. Do not bundle unrelated changes. Suggested
split:

- `ci(crane-cli): add GitHub Actions integration workflow — Phase 0`
- `feat(crane-cli): scaffold F# project — Phase 0`
- `feat(crane-cli): add PDF commands via PdfPig — Phase 1`
- `feat(crane-cli): add analysis commands — Phase 2`
- `feat(crane-cli): add coverage+validation commands — Phase 3`
- `feat(crane-cli): add workflow commands — Phase 4`
- `feat(crane-cli): integrate into pdf-to-md agents — Phase 5`

## Plan Archival

When all Final Gate items are checked:

```bash
git mv plans/in-progress/crane-cli plans/done/YYYY-MM-DD__crane-cli
```

Update `plans/done/README.md` and `plans/in-progress/README.md` accordingly.

---

## Final Gate

- [ ] **F1** All 5 phases complete; all items above checked
- [ ] **F2** `npx nx run crane-cli:test:quick` passes — coverage ≥ 95% (altcover + rhino-cli validate)
- [ ] **F3** `npx nx run crane-cli:test:integration` passes (PdfPig reads real PDF; tesseract OCR tests pass)
- [ ] **F4** `npx nx run crane-cli:lint` clean — zero Fantomas violations
- [ ] **F5** `npx nx run crane-cli:spec-coverage` passes — all Gherkin scenarios implemented
- [ ] **F6** `crane --help` shows all 10 subcommand groups
- [ ] **F7** `crane pdf type apps/crane-cli/tests/integration/fixtures/sample-text.pdf | jq -r .type` outputs `text`
- [ ] **F8** pdf-to-md agents contain no inline `grep -F`, `pdfinfo | awk`, or UUID bash
- [ ] **F9** `npx nx affected -t typecheck lint test:quick spec-coverage` passes (pre-push gate)
- [ ] **F10** `.github/workflows/crane-cli-integration.yml` exists;
      `gh workflow list` shows `crane-cli integration`
- [ ] **F11** Post-push: `pr-quality-gate.yml` fsharp job passes for crane-cli;
      `crane-cli-integration.yml` integration job passes
- [ ] **F12** `dist/crane` binary runs standalone without system tesseract/pdftotext on PATH
      (tessdata bundled; PdfPig pure managed)
- [ ] **F13** Plan archival complete: folder moved to `plans/done/YYYY-MM-DD__crane-cli/`
