# AyoKoding Web Workflows

Orchestrated workflows for ayokoding-fs content quality validation and management.

## Purpose

These workflows define **WHEN and HOW to validate ayokoding-fs content**, orchestrating multiple agents in sequence to ensure content quality, factual accuracy, and link validity.

## Scope

**✅ Workflows Here:**

- General content quality validation (facts, links)
- By-example tutorial quality validation
- Multi-agent orchestration for ayokoding-fs
- Iterative check-fix-verify cycles

**❌ Not Included:**

- Single-agent operations (use agents directly)
- Other Hugo sites (oseplatform-fs has separate workflows)
- Non-workflow documentation (that's conventions/)

## Workflows

- [AyoKoding Web By-Example Quality Gate](./ayokoding-fs-by-example-quality-gate.md) - Validate by-example tutorial quality (95% coverage through 75-90 examples) and apply fixes iteratively until EXCELLENT status
- [AyoKoding Web General Quality Gate](./ayokoding-fs-general-quality-gate.md) - Validate all ayokoding-fs content quality (factual accuracy, links), apply fixes iteratively until ZERO findings
- [AyoKoding Web In-the-Field Quality Gate](./ayokoding-fs-in-the-field-quality-gate.md) - Validate in-the-field production guide quality and apply fixes iteratively until EXCELLENT status

## Related Documentation

- [Workflows Index](../README.md) - All orchestrated workflows
- [AyoKoding Web Conventions](../../conventions/hugo/ayokoding.md) - Content conventions these workflows enforce
- [By Example Tutorial Convention](../../conventions/tutorials/by-example.md) - By-example standards
- [Maker-Checker-Fixer Pattern](../../development/pattern/maker-checker-fixer.md) - Core workflow pattern

---

**Last Updated**: 2026-01-01
