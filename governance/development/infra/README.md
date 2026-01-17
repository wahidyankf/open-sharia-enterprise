# Infrastructure Development

Development infrastructure standards covering temporary files, build artifacts, and acceptance criteria.

## Purpose

These standards define **HOW to manage development infrastructure**, including where AI agents should create temporary files, how build artifacts are organized, and how to write testable acceptance criteria using Gherkin format.

## Scope

**✅ Belongs Here:**

- Temporary file organization
- Build artifact management
- Acceptance criteria standards
- Development infrastructure patterns

**❌ Does NOT Belong:**

- Production infrastructure (deployment, hosting)
- Content organization (that's conventions/)
- Development workflow (that's workflow/)

## Documents

- [Acceptance Criteria Convention](./acceptance-criteria.md) - Writing testable acceptance criteria using Gherkin format for clarity and automation
- [Temporary Files Convention](./temporary-files.md) - Guidelines for AI agents creating temporary uncommitted files and folders

## Related Documentation

- [Development Index](./README.md) - All development practices
- [Explicit Over Implicit Principle](../../principles/software-engineering/explicit-over-implicit.md) - Why clear organization matters
- [AI Agents Convention](../agents/ai-agents.md) - Agent development standards
- [Repository Architecture](../../repository-governance-architecture.md) - Six-layer governance model

## Principles Implemented/Respected

This set of development practices implements/respects the following core principles:

- **[Explicit Over Implicit](../../principles/software-engineering/explicit-over-implicit.md)**: Temporary Files Convention defines explicit locations for agent-generated files, making it clear where temporary artifacts should be stored.

- **[Automation Over Manual](../../principles/software-engineering/automation-over-manual.md)**: Gherkin acceptance criteria enable automated testing and validation, ensuring requirements are met through executable specifications.

## Conventions Implemented/Respected

This set of development practices respects the following conventions:

- **[Plans Organization Convention](../../conventions/project/plans-organization.md)**: Acceptance criteria format aligns with Gherkin standard used for project planning and requirement specification.

- **[File Naming Convention](../../conventions/meta/file-naming.md)**: Temporary files follow naming patterns with UUID chains and timestamps for traceability and collision prevention.

---

**Last Updated**: 2026-01-01
