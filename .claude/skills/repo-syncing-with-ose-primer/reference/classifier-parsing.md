# Classifier Parsing

Procedure for locating and parsing the authoritative classifier table in [`governance/conventions/structure/ose-primer-sync.md`](../../../../governance/conventions/structure/ose-primer-sync.md).

## Locate the table

1. Read the convention file.
2. Find the heading `## Classifier table` (H2).
3. Read forward until the first markdown table (pipe-delimited, header row + alignment row + data rows).

## Parse each row

Each row has four columns:

| Column       | Semantics                                                                                                                                                           |
| ------------ | ------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| Path pattern | Glob pattern relative to `ose-public/` root. Backtick-wrapped in the table; strip backticks before matching.                                                        |
| Direction    | One of `propagate`, `adopt`, `bidirectional`, `neither`. The post-extraction qualifier `(post-extraction)` is metadata only and does not change matching behaviour. |
| Transform    | One of `identity`, `strip-product-sections`, or `—` (dash; used for `neither` rows where transform is inapplicable).                                                |
| Rationale    | Human-readable justification; not consumed programmatically.                                                                                                        |

Rows whose path pattern is comma-separated (for example `LICENSE, LICENSING-NOTICE.md`) are treated as multiple patterns sharing the same direction/transform/rationale.

## Glob resolution

Patterns follow standard shell-glob semantics with one extension:

- `*` matches any single path segment (no slashes).
- `**` matches zero or more path segments.
- Suffixes like `(excluding *-e2e)` are commentary in the Rationale column; the pattern in column 1 is authoritative.

Resolution: for each path under inspection, iterate rows in **table order** and return the first match. Earlier rows take precedence over later rows — more specific patterns precede more general patterns by convention, but the matcher does not reorder for specificity.

## Orphan-path default

If no row matches a path, the default direction is `neither` and the default transform is `—`. This satisfies the Orphan-path rule documented in the convention. The audit rule catches stale rows (patterns that match zero actual paths) and orphan paths (paths not covered by any row) as distinct findings, not silent defaults.

## Zero-match whitelist

After Phase 8 extraction, several rows intentionally match zero paths. The whitelist lives in the convention doc's "Intentional zero-match whitelist" section. Parsers MUST read the whitelist at the same time as the table so that the audit rule can distinguish intentional zero-matchers from genuinely stale rows.

## Invariants

- Table rows are the sole source of truth; docstrings or rationale text never override a row's direction.
- A path may match at most one row; the first match wins. If the operator notices two equally specific rows that would both match, that is a convention defect — raise it as a governance finding.
- The classifier is whole-file, not range-based; per-section rules live in transforms, not in classifier rows.
