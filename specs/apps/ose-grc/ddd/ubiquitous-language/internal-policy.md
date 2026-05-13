# Ubiquitous Language — internal-policy

**Bounded context**: `internal-policy`
**Maintainer**: ose-grc-be team
**Last reviewed**: 2026-05-13

## Responsibility

Ingests and stores company-internal documents (SOPs, manuals, procedures, work instructions)
with version and scope metadata. Acts as the authoritative source for all internal policy
text consumed by the `gap-analysis` context.

## Term index

| Term | Code identifier(s) | Used in features |
| ---- | ------------------ | ---------------- |

_Terms defined in feature plans. Canonical terms include: internal policy, SOP, policy
version, policy scope, document owner._

## Out of scope

- Parsing or extracting text from document binary formats
- Gap computation or regulatory comparison — belongs to `gap-analysis`
- LLM calls — belongs to `ai-orchestration`
