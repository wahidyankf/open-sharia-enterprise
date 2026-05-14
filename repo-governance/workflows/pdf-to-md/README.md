---
title: "PDF-to-Markdown Workflows"
description: Workflows for converting PDF files to verbatim Markdown and validating conversion fidelity
category: explanation
subcategory: workflows
tags:
  - index
  - pdf
  - markdown
  - conversion
  - quality-gate
created: 2026-05-14
---

# PDF-to-Markdown Workflows

Workflows for converting PDF source documents to verbatim Markdown and validating that the conversion is complete and faithful. These workflows underpin cross-referencing workflows that treat the Markdown as a source-of-truth proxy for the original PDF.

## Workflows in This Family

| Workflow | Purpose | Agents | Complexity |
|---|---|---|---|
| [pdf-to-md-quality-gate](pdf-to-md-quality-gate.md) | Convert PDF → verbatim Markdown, then validate fidelity iteratively until ZERO findings | pdf-to-md-maker, pdf-to-md-checker, pdf-to-md-fixer | Medium |

## When to Use These Workflows

- After receiving a new PDF source document that needs Markdown archival
- To verify an existing PDF-to-Markdown conversion for completeness
- Before using a Markdown file for cross-referencing (quality gate)
- When a PDF has been updated and the Markdown needs revalidation

## Agents Used

- **[pdf-to-md-maker](../../../.claude/agents/pdf-to-md-maker.md)** — Converts PDF to verbatim Markdown (text-based and image-only via OCR)
- **[pdf-to-md-checker](../../../.claude/agents/pdf-to-md-checker.md)** — Validates Markdown fidelity against source PDF
- **[pdf-to-md-fixer](../../../.claude/agents/pdf-to-md-fixer.md)** — Applies validated fixes from checker audit

## Default Behavior

By default, the PDF and Markdown file share the same directory and filename, differing only in extension:

```
docs/reference/security/frameworks/nist-sp-800-53-rev5.pdf
docs/reference/security/frameworks/nist-sp-800-53-rev5.md  ← output
```

## Related Workflows

- [docs-quality-gate](../docs/docs-quality-gate.md) — Validate documentation quality after Markdown is created
