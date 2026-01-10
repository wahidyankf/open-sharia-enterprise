# Quality Development

Quality standards and methodologies for code quality, validation, and content preservation.

## Purpose

These standards define **HOW to maintain and validate quality**, covering automated code quality tools, repository validation methodologies, criticality and confidence level systems, and content preservation principles.

## Scope

**✅ Belongs Here:**

- Code quality automation (Prettier, Husky)
- Validation methodologies and patterns
- Criticality and confidence level systems
- Content preservation principles
- Quality gate standards

**❌ Does NOT Belong:**

- Why quality matters (that's a principle)
- Specific validation implementations (that's agents/)
- Content writing standards (that's conventions/)

## Documents

- [Code Quality Convention](./ex-ru-de-qu__code.md) - Automated code quality tools and git hooks (Prettier, Husky, lint-staged) for consistent formatting and commit validation
- [Content Preservation Convention](./ex-ru-de-qu__content-preservation.md) - Principles and processes for preserving knowledge when condensing files and extracting duplications
- [Criticality Levels Convention](./ex-ru-de-qu__criticality-levels.md) - Universal criticality level system for categorizing validation findings (CRITICAL/HIGH/MEDIUM/LOW)
- [Fixer Confidence Levels Convention](./ex-ru-de-qu__fixer-confidence-levels.md) - Universal confidence level system for fixer agents to assess and apply validated fixes (HIGH/MEDIUM/FALSE_POSITIVE)
- [Repository Validation Methodology Convention](./ex-ru-de-qu__repository-validation.md) - Standard validation methods and patterns for repository consistency checking

## Related Documentation

- [Development Index](../README.md) - All development practices
- [Automation Over Manual Principle](../../principles/software-engineering/ex-ru-pr-se__automation-over-manual.md) - Why automated quality matters
- [Maker-Checker-Fixer Pattern](../pattern/ex-ru-de-pa__maker-checker-fixer.md) - Quality workflow pattern
- [Repository Architecture](../../ex-ru__repository-governance-architecture.md) - Six-layer governance model

## Principles Implemented/Respected

This set of development practices implements/respects the following core principles:

- **[Automation Over Manual](../../principles/software-engineering/ex-ru-pr-se__automation-over-manual.md)**: Code quality automation uses git hooks (Husky, Prettier, lint-staged) to enforce standards automatically before commits.

- **[Documentation First](../../principles/content/ex-ru-pr-co__documentation-first.md)**: Content preservation conventions ensure knowledge is preserved during file condensation and restructuring, treating documentation as essential.

## Conventions Implemented/Respected

This set of development practices respects the following conventions:

- **[Content Quality Principles](../../conventions/content/ex-ru-co-co__quality.md)**: Quality validation standards align with documentation quality requirements for active voice, accessibility, and proper structure.

- **[Timestamp Format](../../conventions/formatting/ex-ru-co-fo__timestamp.md)**: Quality reports and validation artifacts use standard UTC+7 timestamps for consistency and traceability.

---

**Last Updated**: 2026-01-01
