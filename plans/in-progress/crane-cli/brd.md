# crane-cli — Business Requirements Document

## Problem Statement

The `pdf-to-md-quality-gate` workflow and its three agents (`pdf-to-md-maker`,
`pdf-to-md-checker`, `pdf-to-md-fixer`) rely on inline bash one-liners and ad-hoc
subprocess calls for complex, domain-specific operations. This creates a set of structural
problems that compound with each new PDF processed.

## Pain Points

### 1. Fragile Text Comparison

Agents use `grep -F "$SEGMENT" "$MD_FILE"` to verify text completeness. This breaks silently on:
multi-line segments (grep matches single lines), regex-special characters in PDF text, normalized
whitespace differences (PDF has double-space, MD has single-space), soft hyphens, and OCR variant
spellings. The result: false negatives (content present but not detected) inflate finding counts
and waste fix iterations.

### 2. Brittle Table Detection

The bash pattern `grep -n "^\s\+[A-Za-z].*\s\{2,\}.*\s\{2,\}"` in the checker has a high
false-positive rate on flowing prose and misses tables with numeric-only rows. Proper table
detection requires analyzing column-aligned structure across consecutive lines — a task suited
to Python's line-by-line parsing, not a single-line regex.

### 3. Unimplemented Heading Depth Inference

The agents describe the heading-depth algorithm (count dots in `1.2.3` → H4) in their prompt
text but provide no working bash implementation. In practice, agents skip this step, use the
wrong heuristic, or fabricate depth values. The algorithm is deterministic and testable — it
belongs in code, not in prose.

### 4. UUID Chain Race Conditions

Report UUID chain management uses `date`, `openssl rand`, and file `mtime` checks in bash.
Under concurrent agent runs, two agents can read the same mtime window and produce conflicting
chain files. Python's `time.time()` + atomic write + `uuid.uuid4()` eliminates the race.

### 5. Untestable Agent Logic

Complex analysis logic embedded in `.md` agent definition files cannot be unit-tested. Regressions
are discovered only during live PDF processing runs. Moving deterministic logic to a Python package
makes every algorithm independently testable before deployment.

### 6. OCR Quality Metric Not Implemented

The checker agent describes OCR error rate estimation ("count garbled characters") as a validation
dimension but no working bash implementation exists — agents skip it or mark it manually. Python
regex makes this straightforward and reproducible.

### 7. Skip List Fragility

False positive persistence uses `echo "..." >> file` appends with no deduplication logic. Re-runs
accumulate duplicate entries; skip list checks are grep-based and miss normalized variants. Python
enables proper dedup and normalized key matching.

## Solution

crane-cli provides a tested, type-safe Python CLI exposing every deterministic operation in the
pdf-to-md pipeline as a discrete, composable command. Agents become thin orchestrators:

```
Before (agent inline):
  TOTAL=$(pdfinfo "$PDF" | grep Pages | awk '{print $2}')
  for i in $(seq 0 $(( (TOTAL + 49) / 50 - 1 ))); do
    pdftotext -layout -f $((i*50+1)) -l $(((i+1)*50)) "$PDF" /tmp/chunk_$i.txt
    grep -F "$SEGMENT" "$MD" >/dev/null 2>&1 || echo "MISSING"
  done

After (agent delegates):
  crane text check "$PDF" "$MD" --chunk-size 50
```

All finding details, criticality, confidence, and location are in the JSON output. Agent reads
structured data, not raw text.

## Business Value

| Value         | How crane-cli Delivers                                                             |
| ------------- | ---------------------------------------------------------------------------------- |
| Reliability   | Every algorithm tested; predictable exit codes; no silent `2>/dev/null` swallowing |
| Debuggability | `crane --debug` shows intermediate extraction steps; structured JSON findings      |
| Testability   | Unit tests for each analysis module; coverage enforced by rhino-cli spec-coverage  |
| Reusability   | Future workflows (doc normalization, web scraping) call same crane commands        |
| Correctness   | Fuzzy matching prevents false positives; proper column analysis finds real tables  |
| Speed         | Cached Nx targets; no redundant pdftotext calls across checker iterations          |

## Success Criteria

- All 8 pdf-to-md validation dimensions covered by crane commands with unit tests
- pdf-to-md agents contain no inline bash analysis logic after Phase 5
- `nx run crane-cli:test:quick` passes with ≥ 85% line coverage enforced by rhino-cli
- End-to-end quality gate on a real text-based PDF produces a `PASS` result
- `nx run crane-cli:spec-coverage` passes (all Gherkin scenarios implemented)

## Constraints

- Wrap system tools (`pdftotext`, `pdfinfo`, `tesseract`) — do not replace them; they are
  already installed (verified: `/opt/homebrew/bin/pdftotext` [Repo-grounded], `/opt/homebrew/bin/pdfinfo` [Repo-grounded])
- Default output is JSON for AI agent parsing; `--human` flag for rich terminal display
- Follow ose-primer Python wiring: `uv`, `src/` layout, `hatchling`, `ruff`, `pyright`,
  `pytest-bdd`, `nx:run-commands` executors with `cwd`
- Must run on macOS (darwin, primary dev) and Linux (GitHub Actions CI)
- Python 3.13+ (already installed at `/Users/wkf/.pyenv/versions/3.13.1` [Repo-grounded])
- No Nx Python plugin dependency — use `nx:run-commands` with `uv run` per primer pattern

## Affected Roles

| Role                            | Impact                                                                                            |
| ------------------------------- | ------------------------------------------------------------------------------------------------- |
| `pdf-to-md-maker` agent         | Gains crane commands for PDF type detection, extraction, and metadata; loses inline bash analysis |
| `pdf-to-md-checker` agent       | Gains structured JSON findings from crane; loses fragile bash grep/awk one-liners                 |
| `pdf-to-md-fixer` agent         | Gains deduplicating skiplist and fuzzy text search; loses undeduped echo-appends                  |
| Human developer / plan executor | Must have `pdftotext`, `pdfinfo`, Python 3.13+, and `uv` installed to run integration tests       |
| CI/CD pipeline (GitHub Actions) | Must install `poppler` (pdftotext/pdfinfo) and Python 3.13 in test runner                         |

## Business Risks

| Risk                                                   | Severity | Mitigation                                                                                                             |
| ------------------------------------------------------ | -------- | ---------------------------------------------------------------------------------------------------------------------- |
| `pdftotext`/`pdfinfo` not available in CI              | HIGH     | Integration tests guarded by `pytest.importorskip` / skip marker; unit tests use mocked adapters                       |
| Python 3.13 incompatibility on CI runner               | HIGH     | Declare `requires-python = ">=3.13"` in `pyproject.toml`; pin Python via `.python-version` or Volta-equivalent         |
| Performance regression from per-chunk subprocess calls | MEDIUM   | Adapter layer caches subprocess output; future caching target can be added if needed                                   |
| Phase 5 agent API breakage during transition           | MEDIUM   | Phase 5 items are incremental per-agent; if crane is not installed, agents fall back gracefully until Phase 5 complete |
| Fuzzy threshold too permissive (false negatives)       | MEDIUM   | Threshold 0.85 chosen conservatively; unit tests cover boundary cases; adjustable via `--threshold` flag (Phase 2)     |
| Skip list key collision (deduplication failure)        | LOW      | Stable key format is deterministic; unit test `test_add_does_not_duplicate_file_line` guards this                      |

## Non-Scope (Future Plans)

- Processing formats other than PDF (docx, html, epub)
- Web content retrieval or URL scraping
- NLP semantic analysis (summarization, sentiment, topic extraction)
- Non-English OCR quality heuristics beyond ASCII confusion-character detection
- Streaming / async pipeline mode
