# crane-cli — Technical Design

## Architecture

Three-layer design: thin CLI commands delegate to pure-Python core logic; adapters isolate
subprocess calls to system tools.

```
┌───────────────────────────────────────────────┐
│  CLI Layer  crane/commands/*.py                │
│  Typer commands, JSON serialization, exit codes│
├───────────────────────────────────────────────┤
│  Core Layer  crane/core/*.py                   │
│  Pure business logic, deterministic analysis   │
│  No subprocess; accepts str/Path args only     │
├───────────────────────────────────────────────┤
│  Adapter Layer  crane/adapters/*.py            │
│  Subprocess wrappers for pdftotext/pdfinfo/    │
│  tesseract; raises ToolNotFoundError on 127    │
└───────────────────────────────────────────────┘
```

## Project Structure

```
apps/crane-cli/
├── src/
│   └── crane_cli/
│       ├── __init__.py
│       ├── main.py                  # Typer app, subcommand group registration
│       ├── commands/
│       │   ├── __init__.py
│       │   ├── pdf.py               # crane pdf *
│       │   ├── text.py              # crane text *
│       │   ├── heading.py           # crane heading *
│       │   ├── nesting.py           # crane nesting *
│       │   ├── table.py             # crane table *
│       │   ├── figure.py            # crane figure *
│       │   ├── mermaid.py           # crane mermaid *
│       │   ├── ocr.py               # crane ocr *
│       │   ├── report.py            # crane report *
│       │   └── skiplist.py          # crane skiplist *
│       ├── core/
│       │   ├── __init__.py
│       │   ├── text_checker.py      # Completeness + accuracy analysis
│       │   ├── heading_checker.py   # Heading depth inference + comparison
│       │   ├── nesting_checker.py   # List nesting column-offset analysis
│       │   ├── table_checker.py     # Columnar table detection + comparison
│       │   ├── figure_checker.py    # Figure reference detection + coverage
│       │   ├── mermaid_validator.py # Mermaid syntax validation
│       │   ├── ocr_assessor.py      # OCR confusion-character error rate
│       │   ├── report_manager.py    # UUID chain, UTC+7 timestamp, report init
│       │   └── skiplist_manager.py  # Skip list CRUD with dedup
│       ├── adapters/
│       │   ├── __init__.py
│       │   ├── pdftotext.py         # pdftotext -layout subprocess wrapper
│       │   ├── pdfinfo.py           # pdfinfo subprocess wrapper
│       │   └── tesseract.py         # tesseract subprocess wrapper
│       └── models/
│           ├── __init__.py
│           ├── finding.py           # Finding, Criticality, Confidence, Category
│           ├── pdf_metadata.py      # PDFMetadata
│           └── report.py           # SkipListEntry
├── tests/
│   ├── __init__.py
│   ├── conftest.py                  # Top-level shared fixtures
│   ├── unit/
│   │   ├── __init__.py
│   │   ├── conftest.py              # GHERKIN_ROOT, shared step fixtures, fake adapters
│   │   └── steps/
│   │       ├── __init__.py
│   │       ├── pdf_steps.py         # BDD steps for pdf-commands.feature
│   │       ├── text_steps.py        # BDD steps for text-check.feature
│   │       ├── heading_steps.py     # BDD steps for heading-check.feature
│   │       ├── nesting_steps.py     # BDD steps for nesting-check.feature
│   │       ├── table_steps.py       # BDD steps for table-check.feature
│   │       ├── figure_steps.py      # BDD steps for figure-check.feature
│   │       ├── mermaid_steps.py     # BDD steps for mermaid-validate.feature
│   │       ├── ocr_steps.py         # BDD steps for ocr-quality.feature
│   │       ├── report_steps.py      # BDD steps for report-management.feature
│   │       └── skiplist_steps.py    # BDD steps for skiplist-management.feature
│   ├── integration/
│   │   ├── __init__.py
│   │   └── conftest.py              # Real pdftotext required; skips if absent
│   └── fixtures/
│       ├── sample-text.pdf          # Small text-based PDF (public domain)
│       ├── sample-text.md           # Complete Markdown conversion of above
│       ├── sample-text-missing.md   # MD with one section removed
│       └── sample-text-headings-wrong.md  # MD with wrong heading depths
├── pyproject.toml
├── project.json
└── README.md
```

## Tech Stack

| Component       | Choice                        | Reason                                           |
| --------------- | ----------------------------- | ------------------------------------------------ |
| CLI framework   | Typer 0.12+                   | Type-safe, built on Click, natural with Pydantic |
| Terminal output | Rich 13+                      | Tables, progress, colored human output           |
| Data models     | Pydantic v2                   | JSON serialization, validation, type safety      |
| Package manager | uv                            | Platform standard (ose-primer)                   |
| Build backend   | hatchling                     | Platform standard (ose-primer)                   |
| Linter          | ruff                          | Platform standard (ose-primer)                   |
| Type checker    | pyright                       | Platform standard (ose-primer); NOT mypy         |
| Test framework  | pytest + pytest-bdd           | Platform standard (ose-primer)                   |
| Coverage        | coverage[toml]                | lcov output for rhino-cli threshold validation   |
| Nx executor     | nx:run-commands with `uv run` | Platform standard; no Nx Python plugin           |

## pyproject.toml

```toml
[project]
name = "crane-cli"
version = "0.1.0"
description = "Content Retrieval And Normalization Engine CLI"
requires-python = ">=3.13"
dependencies = [
    "typer>=0.12",
    "rich>=13",
    "pydantic>=2",
]

[project.scripts]
crane = "crane_cli.main:app"

[build-system]
requires = ["hatchling"]
build-backend = "hatchling.build"

[tool.hatch.build.targets.wheel]
packages = ["src/crane_cli"]

[dependency-groups]
dev = [
    "pytest>=8.3",
    "pytest-bdd>=7.3",
    "coverage[toml]>=7.6",
    "ruff>=0.8",
    "pyright>=1.1",
]

[tool.pytest.ini_options]
testpaths = ["tests"]
python_files = ["test_*.py", "*_steps.py"]
markers = [
    "unit: unit tests (no real system tools required)",
    "integration: integration tests (requires pdftotext on PATH)",
]

[tool.coverage.run]
source = ["crane_cli"]

[tool.coverage.report]
show_missing = true

[tool.coverage.lcov]
output = "coverage/lcov.info"

[tool.ruff]
src = ["src"]
line-length = 100

[tool.ruff.lint]
select = ["E", "F", "I", "N", "UP", "B", "A", "C4", "PT"]

[tool.ruff.lint.per-file-ignores]
"tests/**/steps/*_steps.py" = ["E501"]

[tool.pyright]
include = ["src", "tests"]
pythonVersion = "3.13"
typeCheckingMode = "basic"
venvPath = "."
venv = ".venv"
```

## project.json

```json
{
  "name": "crane-cli",
  "$schema": "../../node_modules/nx/schemas/project-schema.json",
  "sourceRoot": "apps/crane-cli/src",
  "projectType": "application",
  "targets": {
    "build": {
      "executor": "nx:run-commands",
      "options": {
        "command": "uv build",
        "cwd": "apps/crane-cli"
      },
      "outputs": ["{projectRoot}/dist"]
    },
    "dev": {
      "executor": "nx:run-commands",
      "options": {
        "command": "uv run crane --help",
        "cwd": "apps/crane-cli"
      }
    },
    "test:quick": {
      "executor": "nx:run-commands",
      "options": {
        "commands": [
          "uv run coverage run -m pytest -m unit",
          "uv run coverage lcov -o coverage/lcov.info",
          "(cd ../../apps/rhino-cli && CGO_ENABLED=0 go run main.go test-coverage validate apps/crane-cli/coverage/lcov.info 85)"
        ],
        "parallel": false,
        "cwd": "apps/crane-cli"
      },
      "cache": true,
      "inputs": [
        "{projectRoot}/src/**/*.py",
        "{projectRoot}/tests/**/*.py",
        "{workspaceRoot}/specs/apps/crane/gherkin/**/*.feature"
      ],
      "outputs": ["{projectRoot}/coverage/lcov.info"]
    },
    "test:unit": {
      "executor": "nx:run-commands",
      "options": {
        "command": "uv run pytest -m unit",
        "cwd": "apps/crane-cli"
      },
      "cache": true,
      "inputs": [
        "{projectRoot}/src/**/*.py",
        "{projectRoot}/tests/**/*.py",
        "{workspaceRoot}/specs/apps/crane/gherkin/**/*.feature"
      ]
    },
    "test:integration": {
      "executor": "nx:run-commands",
      "options": {
        "command": "uv run pytest -m integration",
        "cwd": "apps/crane-cli"
      },
      "cache": false
    },
    "lint": {
      "executor": "nx:run-commands",
      "options": {
        "command": "uv run ruff check .",
        "cwd": "apps/crane-cli"
      }
    },
    "typecheck": {
      "executor": "nx:run-commands",
      "options": {
        "command": "uv run pyright",
        "cwd": "apps/crane-cli"
      },
      "cache": true,
      "inputs": ["{projectRoot}/src/**/*.py", "{projectRoot}/tests/**/*.py"]
    },
    "spec-coverage": {
      "executor": "nx:run-commands",
      "options": {
        "command": "CGO_ENABLED=0 go run -C apps/rhino-cli main.go spec-coverage validate --shared-steps specs/apps/crane/gherkin apps/crane-cli"
      },
      "cache": true,
      "inputs": ["{workspaceRoot}/specs/apps/crane/gherkin/**/*.feature", "{projectRoot}/**/*.py"]
    }
  },
  "tags": ["type:app", "platform:cli", "lang:python", "domain:crane"],
  "implicitDependencies": ["rhino-cli"]
}
```

## Data Models

```python
# src/crane_cli/models/finding.py
from enum import Enum
from pydantic import BaseModel

class Criticality(str, Enum):
    CRITICAL = "CRITICAL"
    HIGH = "HIGH"
    MEDIUM = "MEDIUM"
    LOW = "LOW"

class Confidence(str, Enum):
    HIGH = "HIGH"
    MEDIUM = "MEDIUM"
    FALSE_POSITIVE = "FALSE_POSITIVE"

class Category(str, Enum):
    TEXT_COMPLETENESS = "text-completeness"
    TEXT_ACCURACY = "text-accuracy"
    HEADING_LEVEL = "heading-level-accuracy"
    CONTENT_NESTING = "content-nesting-accuracy"
    TABLE_INTEGRITY = "table-integrity"
    FIGURE_COVERAGE = "figure-coverage"
    MERMAID_SYNTAX = "mermaid-syntax"
    OCR_QUALITY = "ocr-quality"
    STRUCTURE = "structure"

class Finding(BaseModel):
    category: Category
    criticality: Criticality
    confidence: Confidence = Confidence.HIGH
    location_pdf: str | None = None   # e.g. "Page 12, section heading"
    location_md: str | None = None    # e.g. "nist.md:line_445"
    description: str
    pdf_text: str | None = None       # first 80 chars of affected PDF passage
    fix_suggestion: str | None = None
    auto_fixable: bool = False
```

## Key Algorithms

### Text Normalization and Fuzzy Matching

```python
# src/crane_cli/core/text_checker.py
import re
from difflib import SequenceMatcher

FUZZY_THRESHOLD = 0.85

def normalize(text: str) -> str:
    return re.sub(r"\s+", " ", text).strip()

def similarity(a: str, b: str) -> float:
    return SequenceMatcher(None, normalize(a).lower(), normalize(b).lower()).ratio()

def segment_is_present(segment: str, md_text: str) -> bool:
    norm_seg = normalize(segment).lower()
    # First: exact normalized match (fast path)
    if norm_seg in normalize(md_text).lower():
        return True
    # Fallback: fuzzy match on sliding windows of similar length
    seg_len = len(norm_seg)
    norm_md = normalize(md_text).lower()
    for i in range(0, max(1, len(norm_md) - seg_len + 1), max(1, seg_len // 4)):
        window = norm_md[i : i + seg_len + 20]
        if similarity(norm_seg, window) >= FUZZY_THRESHOLD:
            return True
    return False
```

### Heading Depth Inference

```python
# src/crane_cli/core/heading_checker.py
import re

def infer_depth_from_numbering(heading_text: str) -> tuple[int, str] | None:
    """
    Returns (depth, confidence) or None if no section numbering detected.
    "2.3.1 Title" -> (4, "HIGH")
    "A.1 Title"   -> (3, "HIGH")
    "1. Title"    -> (2, "HIGH")
    """
    stripped = heading_text.strip()
    match = re.match(r"^(\d+|\w)(\.\d+|\.\w)*\.?\s", stripped)
    if not match:
        return None
    number_part = match.group(0).rstrip()
    dots = number_part.count(".")
    # "1." = 1 dot → H2; "1.1" = 1 dot → H3; "1.1.1" = 2 dots → H4
    if number_part.endswith("."):
        depth = dots + 1   # "1." has 1 dot → depth 2
    else:
        depth = dots + 2   # "1.1" has 1 dot → depth 3
    return (min(depth, 5), "HIGH")
```

### Table Detection

```python
# src/crane_cli/core/table_checker.py
import re
from dataclasses import dataclass

@dataclass
class TableSpec:
    start_line: int
    header_text: str
    col_count: int
    row_count: int

def detect_tables(layout_text: str) -> list[TableSpec]:
    """
    Detect columnar tables by finding runs of lines with >= 3 columns of
    content separated by >= 2 whitespace characters.
    """
    lines = layout_text.splitlines()
    tables: list[TableSpec] = []
    i = 0
    while i < len(lines):
        cols = re.split(r"\s{2,}", lines[i].strip())
        if len(cols) >= 3 and all(c.strip() for c in cols):
            # Potential table header — scan forward for data rows
            header = lines[i]
            row_count = 0
            j = i + 1
            while j < len(lines):
                row_cols = re.split(r"\s{2,}", lines[j].strip())
                if len(row_cols) >= 2 and lines[j].strip():
                    row_count += 1
                    j += 1
                else:
                    break
            if row_count >= 2:
                tables.append(TableSpec(i, header.strip(), len(cols), row_count))
                i = j
                continue
        i += 1
    return tables
```

### Mermaid Validation

````python
# src/crane_cli/core/mermaid_validator.py
import re

VALID_TYPES: frozenset[str] = frozenset({
    "graph", "flowchart", "sequenceDiagram", "stateDiagram", "stateDiagram-v2",
    "classDiagram", "gantt", "pie", "erDiagram", "journey", "gitGraph",
    "mindmap", "timeline", "quadrantChart", "xychart-beta", "sankey-beta",
    "block-beta", "architecture-beta",
})

def validate_block(block_content: str) -> tuple[bool, str | None]:
    """Returns (is_valid, error_description)."""
    lines = block_content.strip().splitlines()
    if not lines:
        return False, "Empty Mermaid block"
    diagram_type = lines[0].split()[0] if lines[0].strip() else ""
    if diagram_type not in VALID_TYPES:
        return False, f"Unknown diagram type: '{diagram_type}'"
    if block_content.count("[") != block_content.count("]"):
        return False, "Unmatched brackets"
    if block_content.count("(") != block_content.count(")"):
        return False, "Unmatched parentheses"
    return True, None

def extract_blocks(md_text: str) -> list[tuple[int, str]]:
    """Return list of (start_line, block_content)."""
    pattern = re.compile(r"^```mermaid\n(.*?)^```", re.MULTILINE | re.DOTALL)
    results = []
    for match in pattern.finditer(md_text):
        line_no = md_text[: match.start()].count("\n") + 1
        results.append((line_no, match.group(1)))
    return results
````

### OCR Quality Assessment

```python
# src/crane_cli/core/ocr_assessor.py
import re

_ERROR_PATTERNS: list[str] = [
    r"[^\x00-\x7F]{3,}",     # 3+ consecutive non-ASCII (garbled multi-byte)
    r"\b[lI1]{5,}\b",         # 5+ l/I/1 confusion characters
    r"\b[0Oo]{5,}\b",         # 5+ 0/O/o confusion characters
    r"[a-zA-Z]{30,}",         # Impossibly long word (joined OCR tokens)
]

def estimate_error_rate(text: str) -> float:
    total = len(text.replace(" ", "").replace("\n", ""))
    if total == 0:
        return 0.0
    error_chars = sum(
        sum(len(m.group()) for m in re.finditer(p, text))
        for p in _ERROR_PATTERNS
    )
    return min(error_chars / total, 1.0)
```

### UUID Chain + UTC+7 Timestamp

```python
# src/crane_cli/core/report_manager.py
import time, uuid
from datetime import datetime, timedelta, timezone
from pathlib import Path

_UTC7 = timezone(timedelta(hours=7))
_CHAIN_WINDOW_SECONDS = 30

def get_or_extend_chain(scope: str) -> str:
    chain_file = Path(f".execution-chain-{scope}")
    new_id = uuid.uuid4().hex[:6]
    if chain_file.exists():
        age = time.time() - chain_file.stat().st_mtime
        if age < _CHAIN_WINDOW_SECONDS:
            existing = chain_file.read_text().strip()
            chain = f"{existing}__{new_id}"
            chain_file.write_text(chain)
            return chain
    chain_file.write_text(new_id)
    return new_id

def utc7_timestamp() -> str:
    return datetime.now(_UTC7).strftime("%Y-%m-%d--%H-%M")
```

## conftest.py Pattern (GHERKIN_ROOT)

Unit test conftest follows the ose-primer pattern exactly:

```python
# tests/unit/conftest.py
import os
import pathlib

_env_root = os.environ.get("GHERKIN_ROOT")
GHERKIN_ROOT = (
    pathlib.Path(_env_root)
    if _env_root
    else pathlib.Path(__file__).parents[4] / "specs" / "apps" / "crane" / "gherkin"
)
```

## Agent Integration Pattern

After Phase 5, agents call crane instead of writing bash analysis:

**pdf-to-md-checker Step 0 — Report Init**:

```bash
# Before (racy bash)
UUID=$(openssl rand -hex 3)
TIMESTAMP=$(TZ='Asia/Bangkok' date '+%Y-%m-%d--%H-%M')
REPORT="generated-reports/pdf-to-md__${UUID}__${TIMESTAMP}__audit.md"

# After (crane)
REPORT=$(crane report init --scope pdf-to-md --pdf "$PDF_FILE" --md "$MD_FILE" | jq -r .path)
```

**pdf-to-md-checker Step 2 — Text Completeness**:

```bash
# Before (fragile grep)
grep -F "$SEGMENT" "$MD_FILE" >/dev/null 2>&1 || echo "MISSING: $SEGMENT"

# After (crane, structured findings)
crane text check "$PDF_FILE" "$MD_FILE" --chunk-size 50 > /tmp/text-findings.json
```

**pdf-to-md-checker Step 7 — Mermaid Validation**:

````bash
# Before (ad-hoc keyword checks)
grep -n '```mermaid' "$MD_FILE"
# ... manual block extraction and keyword check

# After
crane mermaid validate "$MD_FILE" > /tmp/mermaid-findings.json
````

**pdf-to-md-fixer — False Positive Persistence**:

```bash
# Before (undeduped append)
echo "- [text-completeness] | $MD_FILE | $DESC" >> "$SKIPLIST"

# After (deduplicating)
crane skiplist add "$MD_BASENAME" text-completeness "$DESC"
```
