# Ubiquitous Language — ai-orchestration

**Bounded context**: `ai-orchestration`
**Maintainer**: ose-grc-be team
**Last reviewed**: 2026-05-13

## Responsibility

Wraps LLM calls (OpenRouter), prompt management, retry/backoff logic, and token-budget
accounting. Stateless from the BE side — does not persist LLM outputs. Supplies
AI capabilities to `gap-analysis` on request.

## Term index

| Term | Code identifier(s) | Used in features |
| ---- | ------------------ | ---------------- |

_Terms defined in feature plans. Canonical terms include: OpenRouter, prompt template,
token budget, LLM response, retry policy._

## Out of scope

- Persisting LLM responses — belongs to `gap-analysis`
- Document ingestion — belongs to `regulatory-source` and `internal-policy`
- Selecting which documents to compare — belongs to `gap-analysis`
