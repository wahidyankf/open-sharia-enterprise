# crane-cli — Content Retrieval And Normalization Engine

## What Is This?

crane-cli (**C**ontent **R**etrieval **A**nd **N**ormalization **E**ngine) is a Python CLI
that provides reliable, deterministic operations for the PDF-to-Markdown conversion pipeline.
It replaces fragile bash one-liners in the `pdf-to-md` agents with well-tested, type-safe
Python commands, turning agents into thin orchestrators of a tested analysis engine.

## Why

The `pdf-to-md-quality-gate` workflow agents currently embed complex analysis logic as inline
bash: text comparison via `grep -F`, table detection via column-regex, OCR quality estimation
as a described-but-unimplemented stub, UUID chain management via racy `date` + `openssl rand`,
and heading depth inference with no working implementation. All of these fail silently on edge
cases and cannot be unit-tested. crane-cli relocates every deterministic operation to Python
where it can be tested, debugged, and maintained independently of agent prompt text.

## Scope

| Field           | Value               |
| --------------- | ------------------- |
| Repository      | `ose-public`        |
| App path        | `apps/crane-cli/`   |
| Specs path      | `specs/apps/crane/` |
| Language        | Python 3.13+        |
| CLI framework   | Typer + Rich        |
| Models          | Pydantic v2         |
| Package manager | uv                  |
| Linter          | ruff                |
| Type checker    | pyright             |
| Test framework  | pytest + pytest-bdd |

## Phases

| #   | Scope                                                                             | Status |
| --- | --------------------------------------------------------------------------------- | ------ |
| 0   | Project scaffold, Nx wiring, uv workspace                                         | ☐      |
| 1   | Core PDF commands — `pdf info`, `pdf type`, `pdf extract`                         | ☐      |
| 2   | Analysis commands — `text check`, `heading check`, `nesting check`, `table check` | ☐      |
| 3   | Coverage commands — `figure check`, `mermaid validate`, `ocr quality`             | ☐      |
| 4   | Workflow commands — `report init/finalize`, `skiplist add/check/list`             | ☐      |
| 5   | Agent integration — update all three pdf-to-md agents                             | ☐      |

## Plan Documents

- [Business Requirements](./brd.md) — Problem, solution, value, constraints
- [Product Requirements](./prd.md) — Command inventory, output contract, Gherkin acceptance criteria
- [Technical Design](./tech-docs.md) — Architecture, project layout, pyproject.toml, project.json, algorithms
- [Delivery Checklist](./delivery.md) — Granular TDD step-by-step implementation checklist

## Key Outputs

1. `apps/crane-cli/` — `crane` CLI entrypoint with 10 subcommand groups
2. `specs/apps/crane/gherkin/` — Gherkin feature files consumed by BDD unit tests
3. Updated `.claude/agents/pdf-to-md-maker.md` — uses crane instead of bash analysis
4. Updated `.claude/agents/pdf-to-md-checker.md` — uses crane for all 8 validation dimensions
5. Updated `.claude/agents/pdf-to-md-fixer.md` — uses crane for re-validation and skiplist
6. Updated `repo-governance/workflows/content/pdf-to-md-quality-gate.md` — adds crane as tool dependency
