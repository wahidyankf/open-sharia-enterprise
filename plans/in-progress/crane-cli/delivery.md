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

- [ ] Verify Python 3.13+: `python3 --version` — shows 3.13.x
- [ ] Verify uv installed: `uv --version` — shows uv version
- [ ] Verify pdftotext available: `which pdftotext` — exits 0
- [ ] Verify pdfinfo available: `which pdfinfo` — exits 0
- [ ] Run `npm install && npm run doctor -- --fix` from repo root (worktree)
- [ ] Confirm existing tests pass before starting: `npx nx affected -t test:quick` — exits 0

---

## Phase 0: Project Scaffold

### P0.1 — Directory scaffold

- [ ] Create `apps/crane-cli/src/crane_cli/__init__.py`
- [ ] Create `apps/crane-cli/src/crane_cli/main.py` — Typer app skeleton with empty
      subcommand groups: `pdf`, `text`, `heading`, `nesting`, `table`, `figure`, `mermaid`,
      `ocr`, `report`, `skiplist`
- [ ] Create `apps/crane-cli/src/crane_cli/commands/__init__.py`
- [ ] Create `apps/crane-cli/src/crane_cli/core/__init__.py`
- [ ] Create `apps/crane-cli/src/crane_cli/adapters/__init__.py`
- [ ] Create `apps/crane-cli/src/crane_cli/models/__init__.py`
- [ ] Create `apps/crane-cli/tests/__init__.py`
- [ ] Create `apps/crane-cli/tests/conftest.py` — empty top-level conftest
- [ ] Create `apps/crane-cli/tests/unit/__init__.py`
- [ ] Create `apps/crane-cli/tests/unit/conftest.py` — `GHERKIN_ROOT` path (ose-primer pattern)
- [ ] Create `apps/crane-cli/tests/unit/steps/__init__.py`
- [ ] Create `apps/crane-cli/tests/integration/__init__.py`
- [ ] Create `apps/crane-cli/tests/integration/conftest.py` — skip marker if `pdftotext` absent
- [ ] Create `apps/crane-cli/tests/fixtures/` — empty directory with `.gitkeep`

- [ ] Verify all scaffold files exist:
      `find apps/crane-cli/src apps/crane-cli/tests -name '*.py' | wc -l` returns ≥ 13;
      `test -f apps/crane-cli/tests/fixtures/.gitkeep` exits 0.

### P0.2 — Configuration files

- [ ] Write `apps/crane-cli/pyproject.toml` — exactly as specified in tech-docs.md (uv, hatchling,
      ruff, pyright, pytest-bdd, coverage, `crane` entrypoint)
- [ ] Write `apps/crane-cli/project.json` — exactly as specified in tech-docs.md (`build`, `dev`,
      `test:quick`, `test:unit`, `test:integration`, `lint`, `typecheck`, `spec-coverage` targets)
- [ ] Write `apps/crane-cli/README.md` — one-paragraph description, install/run instructions

### P0.3 — Models scaffold

- [ ] Write `apps/crane-cli/src/crane_cli/models/finding.py` — `Criticality`, `Confidence`,
      `Category` enums, `Finding` Pydantic model (as specified in tech-docs.md)
- [ ] Write `apps/crane-cli/src/crane_cli/models/pdf_metadata.py` — `PDFMetadata` Pydantic model
- [ ] Write `apps/crane-cli/src/crane_cli/models/report.py` — `SkipListEntry` Pydantic model

### P0.4 — uv workspace setup

- [ ] Run `cd apps/crane-cli && uv sync` — creates `.venv`, installs deps
- [ ] Verify `uv run crane --help` shows all 10 empty subcommand groups
- [ ] Verify `nx run crane-cli:test:unit` completes (0 tests, 0 failures)
- [ ] Verify `nx run crane-cli:lint` passes on scaffold
- [ ] Verify `nx run crane-cli:typecheck` passes on scaffold

---

## Phase 1: Core PDF Commands

### P1.1 — Adapter: pdfinfo

- [ ] **RED** Write `apps/crane-cli/tests/unit/steps/pdf_steps.py` — import `scenarios` for
      `pdf-commands.feature`; write step stubs that fail
  - _Suggested executor: `swe-python-dev`_
- [ ] Write `apps/crane-cli/src/crane_cli/adapters/pdfinfo.py` — `get_info(pdf: Path) -> dict[str, str]`
      subprocess wrapper; raises `ToolNotFoundError(tool="pdfinfo")` on exit 127
  - _Suggested executor: `swe-python-dev`_
- [ ] Write unit test `apps/crane-cli/tests/unit/test_pdfinfo_adapter.py` — mock subprocess; assert
      page count parsed from `pdfinfo` output; assert `ToolNotFoundError` on missing tool
  - _Suggested executor: `swe-python-dev`_
- [ ] **GREEN** `nx run crane-cli:test:unit` — pdfinfo adapter unit tests pass

### P1.2 — Adapter: pdftotext

- [ ] Write `apps/crane-cli/src/crane_cli/adapters/pdftotext.py`:
  - `sample(pdf: Path, pages: int = 3) -> str` — extract first N pages, return text
  - `extract(pdf: Path, start: int, end: int) -> str` — extract page range with `-layout`
  - Raises `ToolNotFoundError(tool="pdftotext")` on exit 127
  - _Suggested executor: `swe-python-dev`_
- [ ] Write unit test `apps/crane-cli/tests/unit/test_pdftotext_adapter.py` — mock subprocess;
      assert text returned; assert `ToolNotFoundError` on missing tool
  - _Suggested executor: `swe-python-dev`_
- [ ] **GREEN** `nx run crane-cli:test:unit` — pdftotext adapter unit tests pass

### P1.3 — `crane pdf info` command

- [ ] **RED** Add step implementations in `pdf_steps.py` for `pdf-commands.feature`
      "PDF metadata extraction" scenarios — run, watch fail
- [ ] Write `src/crane_cli/commands/pdf.py::info` — calls pdfinfo adapter + pdftotext sample;
      assembles `PDFMetadata`; prints JSON; exits 0
- [ ] **GREEN** `nx run crane-cli:test:unit` — "PDF metadata extraction" scenarios pass
- [ ] **REFACTOR** Extract `_build_metadata(pdf: Path) -> PDFMetadata` helper

### P1.4 — `crane pdf type` command

- [ ] **RED** Add step implementations for "PDF type detection" scenarios — run, watch fail
- [ ] Write `src/crane_cli/commands/pdf.py::type_` — calls `pdftotext.sample`; computes
      non-whitespace char count; prints `{"type": "text"|"image"}`; exits 0 or 1
- [ ] **GREEN** "PDF type detection" scenarios pass
- [ ] Handle `ToolNotFoundError` → stderr message + exit 2 (unit test: mock ToolNotFoundError)

### P1.5 — `crane pdf extract` command

- [ ] **RED** Write `apps/crane-cli/tests/unit/test_pdf_extract.py` — add test
      `test_extract_writes_to_stdout` and `test_extract_writes_to_file`; run
      `nx run crane-cli:test:unit`, watch fail
  - _Suggested executor: `swe-python-dev`_
- [ ] Write `apps/crane-cli/src/crane_cli/commands/pdf.py::extract` — calls
      `pdftotext.extract`; writes to stdout or `--output` file; exits 2 on `ToolNotFoundError`
  - _Suggested executor: `swe-python-dev`_
- [ ] **GREEN** `nx run crane-cli:test:unit` — both extract tests pass

### P1.6 — Phase 1 gate

- [ ] `nx run crane-cli:test:unit` — all Phase 1 BDD scenarios pass
- [ ] `nx run crane-cli:lint` clean
- [ ] `nx run crane-cli:typecheck` clean
- [ ] `nx run crane-cli:test:integration` — real PDF fixture; `crane pdf info` returns correct
      page count (requires pdftotext on PATH)

---

## Phase 2: Analysis Commands

### P2.1 — Core: text checker

- [ ] **RED** Write failing BDD steps in `apps/crane-cli/tests/unit/steps/text_steps.py` for
      all `text-check.feature` scenarios
  - _Suggested executor: `swe-python-dev`_
- [ ] Write `apps/crane-cli/src/crane_cli/core/text_checker.py`:
  - `normalize(text: str) -> str`
  - `similarity(a: str, b: str) -> float`
  - `segment_is_present(segment: str, md_text: str) -> bool`
  - `classify_missing(segment: str) -> Criticality`
  - `check_text(pdf_chunks: list[str], md_text: str) -> list[Finding]`
- [ ] Unit test `tests/unit/test_text_checker.py`:
  - `test_normalize_collapses_whitespace`
  - `test_normalize_strips_leading_trailing`
  - `test_similarity_exact_is_1`
  - `test_similarity_transposed_words`
  - `test_similarity_below_threshold_returns_false`
  - `test_fuzzy_match_accepts_minor_variation`
  - `test_missing_section_heading_is_critical`
  - `test_missing_paragraph_is_high`
  - `test_present_text_produces_no_finding`
- [ ] **GREEN** BDD scenarios and unit tests pass
- [ ] **REFACTOR** Extract sliding-window fuzzy match to `_window_match` private function

### P2.2 — `crane text check` command

- [ ] Write `src/crane_cli/commands/text.py::check` — calls `pdftotext.extract` per chunk,
      calls `check_text`, serializes findings to JSON, exits 0 (no findings) or 1
- [ ] Write `src/crane_cli/commands/text.py::search` — single segment fuzzy search; exits 0
      if found, 1 if not
- [ ] **GREEN** All `text-check.feature` BDD scenarios pass

### P2.3 — Core: heading checker

- [ ] **RED** Write failing BDD steps in `heading_steps.py` for all `heading-check.feature`
      scenarios
- [ ] Write `src/crane_cli/core/heading_checker.py`:
  - `infer_depth_from_numbering(text: str) -> tuple[int, str] | None`
  - `extract_md_headings(md_text: str) -> list[tuple[int, int, str]]` — (line, depth, text)
  - `check_headings(pdf_layout_text: str, md_text: str) -> list[Finding]`
- [ ] Unit test `tests/unit/test_heading_checker.py`:
  - `test_infer_depth_single_number` — "1. Title" → depth 2
  - `test_infer_depth_two_components` — "2.3 Title" → depth 3
  - `test_infer_depth_three_components` — "2.3.1 Title" → depth 4
  - `test_infer_depth_no_number` — "Introduction" → None
  - `test_infer_depth_appendix` — "A. Appendix" → depth 2
  - `test_wrong_depth_off_by_two_is_high`
  - `test_correct_depth_no_finding`
- [ ] **GREEN** All heading scenarios pass
- [ ] Write `src/crane_cli/commands/heading.py::infer` and `::check`

### P2.4 — Core: nesting checker

- [ ] **RED** Write failing BDD steps in `nesting_steps.py` for all `nesting-check.feature`
      scenarios
- [ ] Write `src/crane_cli/core/nesting_checker.py`:
  - `extract_nesting_levels(layout_text: str) -> list[tuple[int, int]]` — (col_offset, depth)
  - `check_nesting(pdf_layout_text: str, md_text: str) -> list[Finding]`
- [ ] Unit test `tests/unit/test_nesting_checker.py`:
  - `test_extract_single_level_list`
  - `test_extract_two_level_nested_list`
  - `test_wrong_nesting_off_by_one_is_medium`
  - `test_inverted_nesting_is_high`
- [ ] Write `src/crane_cli/commands/nesting.py::infer` and `::check`

### P2.5 — Core: table checker

- [ ] **RED** Write failing BDD steps in `table_steps.py` for all `table-check.feature`
      scenarios
- [ ] Write `src/crane_cli/core/table_checker.py`:
  - `detect_tables(layout_text: str) -> list[TableSpec]`
  - `check_tables(pdf_layout_text: str, md_text: str) -> list[Finding]`
- [ ] Unit test `tests/unit/test_table_checker.py`:
  - `test_detect_3col_table_returns_one_spec`
  - `test_detect_prose_returns_empty`
  - `test_detect_numeric_only_table`
  - `test_missing_table_is_critical`
  - `test_present_table_no_finding`
  - `test_wrong_row_count_is_medium`
- [ ] Write `src/crane_cli/commands/table.py::detect` and `::check`

### P2.6 — Phase 2 gate

- [ ] `nx run crane-cli:test:unit` — all Phase 2 scenarios and unit tests pass
- [ ] `nx run crane-cli:lint` clean
- [ ] `nx run crane-cli:typecheck` clean

---

## Phase 3: Coverage + Validation Commands

### P3.1 — Core: figure checker

- [ ] **RED** Write failing BDD steps in `apps/crane-cli/tests/unit/steps/figure_steps.py` for
      all `figure-check.feature` scenarios
  - _Suggested executor: `swe-python-dev`_
- [ ] Write `apps/crane-cli/src/crane_cli/core/figure_checker.py`:
  - `detect_figures(text: str) -> list[FigureRef]` — regex: `Figure \d+`, `Fig\. \d+`,
    `Exhibit \d+`, `Diagram \d+`, `Chart \d+`
  - `check_figures(pdf_text: str, md_text: str) -> list[Finding]`
- [ ] Unit test `tests/unit/test_figure_checker.py`:
  - `test_detect_figure_n_pattern`
  - `test_detect_fig_dot_n_pattern`
  - `test_no_figures_returns_empty`
  - `test_missing_figure_is_high`
  - `test_placeholder_satisfies_coverage`
  - `test_mermaid_block_satisfies_coverage`
- [ ] Write `src/crane_cli/commands/figure.py::detect` and `::check`

### P3.2 — Core: mermaid validator

- [ ] **RED** Write failing BDD steps in `mermaid_steps.py` for all `mermaid-validate.feature`
      scenarios
- [ ] Write `src/crane_cli/core/mermaid_validator.py`:
  - `VALID_TYPES` frozenset (18 diagram types)
  - `validate_block(block_content: str) -> tuple[bool, str | None]`
  - `extract_blocks(md_text: str) -> list[tuple[int, str]]` — (line_no, content)
  - `validate_md(md_text: str) -> list[Finding]`
- [ ] Unit test `tests/unit/test_mermaid_validator.py`:
  - `test_valid_graph_td_no_finding`
  - `test_valid_flowchart_lr_no_finding`
  - `test_all_known_types_accepted` — parameterized over VALID_TYPES
  - `test_unknown_type_is_high`
  - `test_empty_block_is_high`
  - `test_unmatched_bracket_is_high`
  - `test_unmatched_paren_is_high`
  - `test_finding_includes_line_number`
- [ ] **GREEN** All mermaid BDD scenarios pass
- [ ] Write `src/crane_cli/commands/mermaid.py::validate`

### P3.3 — Core: OCR assessor

- [ ] **RED** Write failing BDD steps in `ocr_steps.py` for all `ocr-quality.feature` scenarios
- [ ] Write `src/crane_cli/core/ocr_assessor.py`:
  - `estimate_error_rate(text: str) -> float` — returns 0.0–1.0
  - `extract_ocr_sections(md_text: str) -> list[tuple[int, str]]` — (page_no, text)
  - `check_ocr_quality(md_text: str) -> list[Finding]`
- [ ] Unit test `tests/unit/test_ocr_assessor.py`:
  - `test_clean_text_rate_near_zero`
  - `test_repeated_l_characters_raises_rate`
  - `test_non_ascii_runs_raises_rate`
  - `test_rate_above_10_percent_is_critical`
  - `test_rate_5_to_10_percent_is_high`
  - `test_rate_2_to_5_percent_is_medium`
  - `test_rate_below_2_percent_no_finding`
  - `test_no_ocr_tags_returns_empty`
  - `test_finding_includes_page_number`
- [ ] **GREEN** All OCR BDD scenarios pass
- [ ] Write `src/crane_cli/commands/ocr.py::quality` and `::extract`

### P3.4 — Phase 3 gate

- [ ] `nx run crane-cli:test:unit` — all Phase 3 scenarios and unit tests pass, coverage ≥ 85%
- [ ] `nx run crane-cli:lint` clean
- [ ] `nx run crane-cli:typecheck` clean

---

## Phase 4: Workflow Commands

### P4.1 — Core: report manager

- [ ] **RED** Write failing BDD steps in `apps/crane-cli/tests/unit/steps/report_steps.py` for
      all `report-management.feature` scenarios
  - _Suggested executor: `swe-python-dev`_
- [ ] Write `apps/crane-cli/src/crane_cli/core/report_manager.py`:
  - `get_or_extend_chain(scope: str) -> str`
  - `utc7_timestamp() -> str`
  - `init_report(scope: str, pdf: Path, md: Path) -> Path` — creates file, returns path
  - `finalize_report(report_path: Path, status: str) -> None`
- [ ] Unit test `tests/unit/test_report_manager.py`:
  - `test_new_chain_is_6_hex_chars`
  - `test_chain_extends_when_fresh` — mock `time.time` to simulate 5s age
  - `test_chain_resets_when_stale` — mock `time.time` to simulate 60s age
  - `test_utc7_timestamp_format` — assert `YYYY-MM-DD--HH-MM` format
  - `test_init_creates_file_in_generated_reports`
  - `test_init_filename_matches_pattern`
  - `test_init_returns_path_as_string`
  - `test_finalize_replaces_in_progress_with_pass`
  - `test_finalize_raises_on_missing_file`
- [ ] **GREEN** All report BDD scenarios pass
- [ ] Write `src/crane_cli/commands/report.py::init` and `::finalize`

### P4.2 — Core: skiplist manager

- [ ] **RED** Write failing BDD steps in `skiplist_steps.py` for all `skiplist-management.feature`
      scenarios
- [ ] Write `src/crane_cli/core/skiplist_manager.py`:
  - `stable_key(md_basename: str, category: str, description: str) -> str`
  - `add(md_basename: str, category: str, description: str) -> bool` — True if added, False if dup
  - `check(md_basename: str, category: str, description: str) -> bool`
  - `list_entries(md_basename: str) -> list[SkipListEntry]`
- [ ] Unit test `tests/unit/test_skiplist_manager.py`:
  - `test_stable_key_format`
  - `test_add_creates_file`
  - `test_add_returns_true_on_new_entry`
  - `test_add_returns_false_on_duplicate`
  - `test_add_does_not_duplicate_file_line`
  - `test_check_returns_true_on_known_entry`
  - `test_check_returns_false_on_unknown_entry`
  - `test_list_returns_all_entries`
  - `test_list_returns_empty_on_missing_file`
- [ ] **GREEN** All skiplist BDD scenarios pass
- [ ] Write `src/crane_cli/commands/skiplist.py::add`, `::check`, `::list`

### P4.3 — Phase 4 gate

- [ ] `nx run crane-cli:test:unit` — all Phase 4 scenarios pass; coverage ≥ 85%
- [ ] `nx run crane-cli:lint` clean
- [ ] `nx run crane-cli:typecheck` clean
- [ ] `nx run crane-cli:spec-coverage` — passes (all feature scenarios implemented in steps)
- [ ] `crane --help` shows all 10 subcommand groups with correct subcommands listed

---

## Phase 5: Agent Integration

### P5.1 — Update pdf-to-md-maker

- [ ] Edit `.claude/agents/pdf-to-md-maker.md` Step 1: replace `pdftotext` sample + `wc -c`
      with `crane pdf type "$PDF_FILE"`. Verify: `grep -q 'crane pdf type' .claude/agents/pdf-to-md-maker.md` exits 0.
  - _Suggested executor: `swe-python-dev`_
- [ ] Edit `.claude/agents/pdf-to-md-maker.md` Step 2a: replace bash extraction loop with
      `crane pdf extract "$PDF_FILE" --start $FIRST --end $LAST`. Verify: `grep -q 'crane pdf extract' .claude/agents/pdf-to-md-maker.md` exits 0.
  - _Suggested executor: `swe-python-dev`_
- [ ] Edit `.claude/agents/pdf-to-md-maker.md`: replace `pdfinfo | awk` page count with
      `crane pdf info "$PDF_FILE" | jq .pages`. Verify: `grep -q 'crane pdf info' .claude/agents/pdf-to-md-maker.md` exits 0.
  - _Suggested executor: `swe-python-dev`_
- [ ] Run `grep -n 'pdftotext\|pdfinfo\|grep -F' .claude/agents/pdf-to-md-maker.md` — output
      must be empty (zero lines). If any remain, add a targeted replacement item per occurrence.

### P5.2 — Update pdf-to-md-checker

- [ ] Edit `.claude/agents/pdf-to-md-checker.md` Step 0: replace bash UUID + timestamp + file
      create with `REPORT=$(crane report init --scope pdf-to-md --pdf "$PDF_FILE" --md "$MD_FILE" | jq -r .path)`.
      Verify: `grep -q 'crane report init' .claude/agents/pdf-to-md-checker.md` exits 0.
  - _Suggested executor: `swe-python-dev`_
- [ ] Edit `.claude/agents/pdf-to-md-checker.md` Step 2: replace `grep -F` segment loop with
      `crane text check "$PDF_FILE" "$MD_FILE"`. Verify: `grep -q 'crane text check' .claude/agents/pdf-to-md-checker.md` exits 0.
  - _Suggested executor: `swe-python-dev`_
- [ ] Edit `.claude/agents/pdf-to-md-checker.md` Step 3: replace manual grep heading extraction
      with `crane heading check "$PDF_FILE" "$MD_FILE"`. Verify: `grep -q 'crane heading check' .claude/agents/pdf-to-md-checker.md` exits 0.
  - _Suggested executor: `swe-python-dev`_
- [ ] Edit `.claude/agents/pdf-to-md-checker.md` Step 4: replace manual column-offset inspection
      with `crane nesting check "$PDF_FILE" "$MD_FILE"`. Verify: `grep -q 'crane nesting check' .claude/agents/pdf-to-md-checker.md` exits 0.
  - _Suggested executor: `swe-python-dev`_
- [ ] Edit `.claude/agents/pdf-to-md-checker.md` Step 5: replace brittle table grep with
      `crane table check "$PDF_FILE" "$MD_FILE"`. Verify: `grep -q 'crane table check' .claude/agents/pdf-to-md-checker.md` exits 0.
  - _Suggested executor: `swe-python-dev`_
- [ ] Edit `.claude/agents/pdf-to-md-checker.md` Step 6: replace figure grep count with
      `crane figure check "$PDF_FILE" "$MD_FILE"`. Verify: `grep -q 'crane figure check' .claude/agents/pdf-to-md-checker.md` exits 0.
  - _Suggested executor: `swe-python-dev`_
- [ ] Edit `.claude/agents/pdf-to-md-checker.md` Step 7: replace manual Mermaid keyword checks
      with `crane mermaid validate "$MD_FILE"`. Verify: `grep -q 'crane mermaid validate' .claude/agents/pdf-to-md-checker.md` exits 0.
  - _Suggested executor: `swe-python-dev`_
- [ ] Edit `.claude/agents/pdf-to-md-checker.md` Step 8: replace described-but-absent OCR logic
      with `crane ocr quality "$MD_FILE"`. Verify: `grep -q 'crane ocr quality' .claude/agents/pdf-to-md-checker.md` exits 0.
  - _Suggested executor: `swe-python-dev`_
- [ ] Edit `.claude/agents/pdf-to-md-checker.md` skip list loading: replace file grep with
      `crane skiplist check "$MD_BASENAME" <category> <description>`. Verify:
      `grep -q 'crane skiplist check' .claude/agents/pdf-to-md-checker.md` exits 0.
  - _Suggested executor: `swe-python-dev`_

### P5.3 — Update pdf-to-md-fixer

- [ ] Edit `.claude/agents/pdf-to-md-fixer.md`: replace all re-validation `grep -F` calls with
      `crane text search "$MD_FILE" "$SEGMENT"`. Verify: `grep -q 'crane text search' .claude/agents/pdf-to-md-fixer.md` exits 0.
  - _Suggested executor: `swe-python-dev`_
- [ ] Edit `.claude/agents/pdf-to-md-fixer.md`: replace false positive persistence `echo >>`
      with `crane skiplist add "$MD_BASENAME" <category> <description>`. Verify:
      `grep -q 'crane skiplist add' .claude/agents/pdf-to-md-fixer.md` exits 0.
  - _Suggested executor: `swe-python-dev`_
- [ ] Edit `.claude/agents/pdf-to-md-fixer.md`: replace fix report file init bash block with
      `REPORT=$(crane report init --scope pdf-to-md --pdf "$PDF_FILE" --md "$MD_FILE" | jq -r .path)`.
      Verify: `grep -q 'crane report init' .claude/agents/pdf-to-md-fixer.md` exits 0.
  - _Suggested executor: `swe-python-dev`_

### P5.4 — Update workflow documentation

- [ ] Edit `repo-governance/workflows/content/pdf-to-md-quality-gate.md` Tool Dependencies
      section: add `crane` with install command `uv tool install crane-cli` and verify command
      `crane --version`. Verify: `grep -q 'crane' repo-governance/workflows/content/pdf-to-md-quality-gate.md` exits 0.
- [ ] Edit `repo-governance/workflows/content/pdf-to-md-quality-gate.md` Validation Dimensions
      Summary table: add crane command column per dimension (e.g., "text check: `crane text check`").
      Verify: table in that section references `crane` for each dimension.

### P5.5 — End-to-end validation

- [ ] Copy or create a small freely-licensed text-based PDF at
      `apps/crane-cli/tests/fixtures/sample-text.pdf`. Verify:
      `crane pdf type apps/crane-cli/tests/fixtures/sample-text.pdf | jq -r .type` outputs `text`.
- [ ] Run the `pdf-to-md-quality-gate` workflow on the fixture:
      `pdf_file=apps/crane-cli/tests/fixtures/sample-text.pdf mode=normal`;
      verify the resulting audit report in `generated-reports/` has status "PASS".
- [ ] Run `grep -rn 'crane ' .claude/agents/pdf-to-md-*.md` — output shows crane commands;
      run `grep -rn 'pdftotext\|pdfinfo\b\|grep -F' .claude/agents/pdf-to-md-*.md` — output
      must be empty (zero analysis-bash lines remain).

---

## Commit Guidelines

- [ ] Commit Phase 0–4 scaffold and implementation thematically — one commit per phase or
      sub-phase (e.g., `feat(crane-cli): add project scaffold`, `feat(crane-cli): add PDF commands`)
- [ ] Use Conventional Commits format: `feat(crane-cli): <description>`
- [ ] Phase 5 agent edits: one commit per agent file modified (e.g., `refactor(pdf-to-md-maker): delegate to crane`)
- [ ] Do NOT bundle scaffold + implementation + agent updates into a single commit

## Pre-Push Local Gate

- [ ] Run `npx nx affected -t typecheck lint test:quick spec-coverage` — passes for all affected
      projects
- [ ] Fix ALL failures found, including those not caused by crane-cli changes (root-cause
      orientation principle)
- [ ] Re-run until zero failures

> **Important**: Fix ALL failures found during quality gates, not just those caused by
> crane-cli changes. This follows the root cause orientation principle — proactively fix
> preexisting errors encountered during work.

## Final Gate

- [ ] **F1** All 5 phases complete; all items checked above
- [ ] **F2** `nx run crane-cli:test:quick` passes (coverage ≥ 85%, rhino-cli validates threshold)
- [ ] **F3** `nx run crane-cli:test:integration` passes with pdftotext on PATH
- [ ] **F4** `nx run crane-cli:lint` clean — zero ruff violations
- [ ] **F5** `nx run crane-cli:typecheck` clean — zero pyright errors
- [ ] **F6** `nx run crane-cli:spec-coverage` passes — all Gherkin scenarios implemented
- [ ] **F7** `crane --help` shows all 10 subcommand groups
- [ ] **F8** pdf-to-md agents contain no inline `grep -F`, `pdfinfo | awk`, or analysis bash
- [ ] **F9** Push to `origin/main`: `git push origin main`
- [ ] **F10** Monitor GitHub Actions: `gh run list --branch main --limit 3` every 3–5 min until green
- [ ] **F11** If any CI check fails, diagnose root cause and push a fix commit before marking plan done

## Plan Archival

- [ ] Verify ALL delivery checklist items are ticked
- [ ] Verify ALL quality gates pass (local + CI)
- [ ] Run `git mv plans/in-progress/crane-cli plans/done/$(date +%Y-%m-%d)__crane-cli`
- [ ] Update `plans/in-progress/README.md` — remove crane-cli entry
- [ ] Update `plans/done/README.md` — add crane-cli entry with completion date
- [ ] Commit: `chore(plans): archive crane-cli to done`
