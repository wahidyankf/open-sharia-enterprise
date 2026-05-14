# Ubiquitous Language — gap-analysis

**Bounded context**: `gap-analysis`
**Maintainer**: ose-app-be team
**Last reviewed**: 2026-05-13

## Responsibility

Compares a regulatory-source corpus against an internal-policy corpus and emits structured
`GapItem` records. Orchestrates calls to `ai-orchestration` for LLM-assisted comparison.
Owns the gap report lifecycle: creation, update, and presentation.

## Term index

| Term | Code identifier(s) | Used in features |
| ---- | ------------------ | ---------------- |

_Terms defined in feature plans. Canonical terms include: gap item, gap report, gap
severity, gap status, compliance gap._

## Out of scope

- Document storage — belongs to `regulatory-source` and `internal-policy`
- LLM prompt engineering — belongs to `ai-orchestration`
- User authentication and RBAC
