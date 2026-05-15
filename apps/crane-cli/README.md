# crane-cli

Content Retrieval And Normalization Engine — an F# CLI that provides reliable, deterministic
operations for the PDF-to-Markdown conversion pipeline.

## Usage

```bash
dotnet run --project crane-cli.fsproj -- --help
```

## Subcommands

- `crane pdf` — PDF operations (info, type, extract)
- `crane text` — Text completeness checking
- `crane heading` — Heading depth inference and checking
- `crane nesting` — List nesting analysis
- `crane table` — Table detection and checking
- `crane figure` — Figure coverage checking
- `crane mermaid` — Mermaid diagram validation
- `crane ocr` — OCR quality assessment
- `crane report` — Audit report management
- `crane skiplist` — Skip list management
