# Development Patterns

Reusable software development patterns and practices for consistent, high-quality code.

## Purpose

These patterns define **HOW to structure development workflows and code**, covering the Maker-Checker-Fixer quality workflow and functional programming practices. These are proven patterns that solve common development challenges.

## Scope

**✅ Belongs Here:**

- Reusable development patterns
- Workflow patterns (maker-checker-fixer)
- Programming paradigm practices (functional programming)
- Code organization patterns

**❌ Does NOT Belong:**

- Why we value patterns (that's a principle)
- Specific tool configuration (that's workflow/)
- Language-specific syntax (that's reference docs)

## Documents

- [Functional Programming Practices](./ex-ru-de-pa__functional-programming.md) - Guidelines for applying functional programming principles in TypeScript/JavaScript (immutability patterns, pure functions, function composition)
- [Maker-Checker-Fixer Pattern](./ex-ru-de-pa__maker-checker-fixer.md) - Three-stage quality workflow for content creation and validation with user review gates and confidence level integration

## Related Documentation

- [Development Index](../README.md) - All development practices
- [Pure Functions Over Side Effects Principle](../../principles/software-engineering/ex-ru-pr-se__pure-functions.md) - Why functional programming matters
- [Immutability Over Mutability Principle](../../principles/software-engineering/ex-ru-pr-se__immutability.md) - Why immutability matters
- [Repository Architecture](../../ex-ru__repository-governance-architecture.md) - Six-layer governance model

## Principles Implemented/Respected

This set of development practices implements/respects the following core principles:

- **[Immutability Over Mutability](../../principles/software-engineering/ex-ru-pr-se__immutability.md)**: Functional programming practices favor immutable data structures and pure functions, reducing side effects and improving code predictability.

- **[Pure Functions Over Side Effects](../../principles/software-engineering/ex-ru-pr-se__pure-functions.md)**: Functional programming guidelines emphasize pure functions for deterministic, testable, and composable code.

## Conventions Implemented/Respected

This set of development practices respects the following conventions:

- **[Content Quality Principles](../../conventions/content/ex-ru-co-co__quality.md)**: Pattern documentation follows active voice, clear structure, and proper formatting standards.

- **[Criticality Levels Convention](../quality/ex-ru-de-qu__criticality-levels.md)**: Maker-Checker-Fixer pattern integrates with criticality assessment to prioritize and validate fixes systematically.

---

**Last Updated**: 2026-01-01
