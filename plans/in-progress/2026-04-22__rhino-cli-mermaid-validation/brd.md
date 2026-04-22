# BRD: Mermaid Diagram Validation

## Problem Statement

Mermaid flowchart diagrams in repository markdown files degrade silently:

1. **Label overflow**: Long node labels render truncated or overflow the box in GitHub
   preview, VS Code, and Mermaid Live Editor — the diagram becomes unreadable without
   the viewer knowing text was clipped. This repo has already experienced this: production
   C4 diagrams in `governance/conventions/formatting/diagrams.md` show labels clipping at
   ~22 characters in Hugo/Hextra rendering. Mermaid's `wrappingWidth` config defaults to
   200 px; at the default 16 px font that accommodates ~28–30 characters before wrapping
   or overflow begins. Setting the default warning at **30 characters** is grounded in
   this official Mermaid configuration value (not an arbitrary pick).

2. **Diagram too wide (parallel axis)**: Flowcharts with many parallel nodes at the same
   rank (e.g., 6 nodes at rank 2 in a TB diagram) compress each node so small that the
   diagram is illegible at normal viewing sizes. A maximum of 3 parallel nodes per rank
   keeps diagrams readable at standard document widths. **The sequential depth (chain
   length along the primary flow axis) is not limited** — authors can have as many
   sequential steps as needed.

3. **Accidental multi-diagram blocks**: A second `flowchart`/`graph` keyword inside the
   same fenced code block produces undefined parser behavior (some renderers silently
   drop the second diagram; others show a parse error). Authors rarely do this
   intentionally — when it happens it is always a mistake.

## Business Value

| Stakeholder           | Benefit                                                                  |
| --------------------- | ------------------------------------------------------------------------ |
| Documentation authors | Immediate feedback on diagrams that will render badly                    |
| Reviewers             | Catch diagram issues in CI before merge — no manual visual review needed |
| Readers               | Consistently readable, correctly rendered diagrams across all docs       |

## Success Criteria

- Zero truncated-label or overcrowded-width diagrams reach `main` after the validator
  is in the pre-push hook.
- False-positive rate: non-flowchart mermaid blocks (sequence, class, gantt) never
  flagged.
- Performance: full-repo scan completes in under 2 seconds on a typical laptop
  (all `.md` files in `ose-public` ≈ 400 files).
- `--changed-only` mode enables pre-push gate to complete in < 500 ms for a typical
  PR (1–10 changed `.md` files).

## Out of Scope

- Fixing diagram violations automatically (validator reports only; no auto-fix).
- Validating non-flowchart Mermaid diagram types (sequenceDiagram, classDiagram, etc.).
- Validating Mermaid syntax correctness beyond the three structural rules.
- Edge-weight labels / link text length validation.
