# Plan Workflows

Orchestrated workflows for project planning quality validation and systematic execution.

## Purpose

These workflows define **WHEN and HOW to validate and execute plans**, orchestrating plan-checker, plan-fixer, plan-executor, and plan-execution-checker agents in sequence to ensure plan quality and systematic implementation.

## Scope

**✅ Workflows Here:**

- Plan quality validation
- Plan execution tracking
- Iterative plan improvement
- Multi-agent orchestration for plans/
- Check-fix-verify and execution cycles

**❌ Not Included:**

- Content quality validation (that's docs/)
- Hugo content validation (that's ayokoding-web/)
- Single-agent operations (use agents directly)

## Workflows

- [Plan Execution](./plan-execution.md) - Execute plan tasks systematically with validation and completion tracking using plan**executor and plan**execution-checker
- [Plan Quality Gate](../docs/quality-gate.md) - Validate plan completeness and accuracy, apply fixes iteratively until ZERO findings using plan**checker and plan**fixer

## Related Documentation

- [Workflows Index](../README.md) - All orchestrated workflows
- [Plans Organization Convention](../../conventions/content/project/plans-organization.md) - Plan structure standards
- [Maker-Checker-Fixer Pattern](../../development/pattern/maker-checker-fixer.md) - Core workflow pattern
- [Repository Architecture](../../repository-governance-architecture.md) - Six-layer governance model

---

**Last Updated**: 2026-01-01
