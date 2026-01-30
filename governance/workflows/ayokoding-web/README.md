# AyoKoding Web Workflows

Orchestrated workflows for ayokoding-web content quality validation and management.

## Purpose

These workflows define **WHEN and HOW to validate ayokoding-web content**, orchestrating multiple agents in sequence to ensure Hugo content quality, factual accuracy, structure consistency, and link validity.

## Scope

**✅ Workflows Here:**

- General content quality validation (Hugo conventions, facts, structure, links)
- By-example tutorial quality validation
- Multi-agent orchestration for ayokoding-web
- Iterative check-fix-verify cycles

**❌ Not Included:**

- Single-agent operations (use agents directly)
- Other Hugo sites (ose-platform-web has separate workflows)
- Non-workflow documentation (that's conventions/)

## Workflows

- [AyoKoding Web By-Example Quality Gate](./ayokoding-web-by-example-quality-gate.md) - Validate by-example tutorial quality (95% coverage through 75-90 examples) and apply fixes iteratively until EXCELLENT status
- [AyoKoding Web General Quality Gate](./ayokoding-web-general-quality-gate.md) - Validate all ayokoding-web content quality (Hugo conventions, factual accuracy, structure, links), apply fixes iteratively until ZERO findings, then regenerate titles and navigation

## Related Documentation

- [Workflows Index](../README.md) - All orchestrated workflows
- [AyoKoding Web Conventions](../../conventions/hugo/ayokoding.md) - Content conventions these workflows enforce
- [By Example Tutorial Convention](../../conventions/content/tutorial/by-example.md) - By-example standards
- [Maker-Checker-Fixer Pattern](../../development/pattern/maker-checker-fixer.md) - Core workflow pattern

---

**Last Updated**: 2026-01-01
