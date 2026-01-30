# Content Conventions

Content quality standards, validation methodology, and writing guidelines for all repository documentation.

## Purpose

These conventions define **WHAT content quality standards apply** to documentation, covering writing style, validation methodology, and quality principles. All content must follow these standards regardless of location (docs/, Hugo sites, plans/, root files).

## Scope

**✅ Belongs Here:**

- Content quality standards and checklists
- Validation methodology for factual correctness
- Writing guidelines and style rules
- README and OSS documentation standards
- Meta-convention for writing conventions
- Documentation type-specific standards (tutorials, how-to guides, reference, explanation)
- Project planning content standards (plans organization)

**❌ Does NOT Belong:**

- Markdown formatting syntax (that's formatting/)
- Hugo-specific content rules (that's hugo/)

## Structure

This directory contains both **universal standards** (apply to all content) and **type-specific standards** (apply to specific documentation types):

**Universal Standards** (root-level files):

- `quality.md` - Applies to ALL content types
- `convention-writing.md` - Applies to ALL conventions
- `factual-validation.md` - Applies to ALL content types
- `oss-documentation.md` - Applies to repository documentation
- `readme-quality.md` - Applies to README files

**Type-Specific Standards** (subdirectories):

- `tutorial/` - Standards specific to learning-oriented tutorials (Diátaxis framework)
- `project/` - Standards specific to project planning documents (plans/ folder organization)
- [Future: `how-to/` - Task-oriented guide standards]
- [Future: `reference/` - Information-oriented documentation standards]
- [Future: `explanation/` - Understanding-oriented content standards]

Type-specific standards **build upon and extend** the universal standards.

## Conventions

### Universal Standards

- [Content Quality Principles](./quality.md) - Universal markdown content quality standards (writing style, heading hierarchy, accessibility, formatting)
- [Convention Writing](./convention-writing.md) - **Meta-convention** defining how to write and organize convention documents
- [Factual Validation](./factual-validation.md) - Universal methodology for validating factual correctness using web verification
- [OSS Documentation](./oss-documentation.md) - Standards for repository documentation files (README, CONTRIBUTING, ADRs, security)
- [README Quality](./readme-quality.md) - Quality standards for README.md files (engagement, accessibility, scannability)

### Type-Specific Standards

- [Tutorial Conventions](./tutorial/README.md) - Standards for creating learning-oriented tutorials (structure, pedagogy, Full Set Tutorial Package)
- [Project Planning Conventions](./project/README.md) - Standards for organizing project planning documents (plans/ folder structure, lifecycle stages)

## Related Documentation

- [Conventions Index](../README.md) - All documentation conventions
- [Content Principles](../../principles/content/README.md) - Foundational values these conventions implement
- [Formatting Conventions](../formatting/README.md) - Markdown syntax and visual elements
- [Repository Architecture](../../repository-governance-architecture.md) - Six-layer governance model

## Principles Implemented/Respected

This set of conventions implements/respects the following core principles:

- **[Accessibility First](../../principles/content/accessibility-first.md)**: Content quality standards include active voice, proper heading hierarchy, and WCAG AA compliance to ensure documentation is accessible to all users.

- **[Documentation First](../../principles/content/documentation-first.md)**: Conventions establish mandatory documentation standards, treating documentation as an integral part of development rather than an optional afterthought.

- **[Explicit Over Implicit](../../principles/software-engineering/explicit-over-implicit.md)**: Convention Writing Convention defines explicit structure for documentation, making rules and standards transparent rather than implicit or magical.

---

**Last Updated**: 2026-01-01
