# Ubiquitous Language — regulatory-source

**Bounded context**: `regulatory-source`
**Maintainer**: ose-grc-be team
**Last reviewed**: 2026-05-13

## Responsibility

Ingests and stores regulator-published rule documents (PDFs, circulars, guidelines) with
full provenance metadata (issuer, issue date, jurisdiction, document type). Acts as the
authoritative source for all regulatory text consumed by the `gap-analysis` context.

## Term index

| Term | Code identifier(s) | Used in features |
| ---- | ------------------ | ---------------- |

_Terms defined in feature plans. Canonical terms include: regulatory document, regulatory
source, provenance metadata, jurisdiction, document type._

## Out of scope

- Parsing or extracting text from document binary formats (PDF OCR)
- Gap computation or policy comparison — belongs to `gap-analysis`
- LLM calls — belongs to `ai-orchestration`
