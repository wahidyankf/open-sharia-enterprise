# Software Engineering Principles

Software engineering-specific foundational principles governing development practices, configuration, automation, and code organization.

## Purpose

These principles define **WHY we value specific approaches to software development**, covering configuration philosophy, automation strategies, code organization, and technical decision-making. All development practices implement these principles.

## Scope

**✅ Belongs Here:**

- Foundational values about software development
- Philosophical stances on automation and configuration
- Beliefs about code quality and organization
- Reasons behind technical standards

**❌ Does NOT Belong:**

- Specific coding standards (that's a development practice)
- How-to implement features (that's a guide)
- Technical tool configurations (that's a development practice)

## Principles

- [Automation Over Manual](./automation-over-manual.md) - Automate repetitive tasks to ensure consistency and reduce human error
- [Explicit Over Implicit](./explicit-over-implicit.md) - Choose explicit composition and configuration over magic, convenience, and hidden behavior
- [Immutability Over Mutability](./immutability.md) - Prefer immutable data structures over mutable state
- [Pure Functions Over Side Effects](./pure-functions.md) - Prefer pure functions (deterministic, no side effects) over functions with side effects
- [Reproducibility First](./reproducibility.md) - Development environments and builds should be reproducible from the start

## Examples from Platform

Each principle is demonstrated across the platform's technology stack:

### Java Examples

- **[Java Idioms](../../../docs/explanation/software-engineering/programming-languages/java/ex-soen-prla-ja__idioms.md)** - Records (Immutability), Pattern Matching (Explicit), Try-with-resources (Automation)
- **[Java Functional Programming](../../../docs/explanation/software-engineering/programming-languages/java/ex-soen-prla-ja__functional-programming.md)** - Pure Functions, Immutability, Functional Core architecture
- **[Java Type Safety](../../../docs/explanation/software-engineering/programming-languages/java/ex-soen-prla-ja__type-safety.md)** - Explicit nullable types, Static analysis (Automation)
- **[Java Concurrency](../../../docs/explanation/software-engineering/programming-languages/java/ex-soen-prla-ja__concurrency-and-parallelism.md)** - Immutability for thread-safety, Virtual threads
- **[Java Domain-Driven Design](../../../docs/explanation/software-engineering/programming-languages/java/ex-soen-prla-ja__domain-driven-design.md)** - Value Objects (Immutability), Domain Services (Pure Functions)
- **[Java Best Practices](../../../docs/explanation/software-engineering/programming-languages/java/ex-soen-prla-ja__best-practices.md)** - Build automation, Reproducible development

### Principle-Specific Examples

- **Automation Over Manual**: Java records auto-generate boilerplate, static analysis catches bugs, TestContainers automate infrastructure
- **Explicit Over Implicit**: Sealed classes define explicit type hierarchies, pattern matching ensures exhaustive handling
- **Immutability Over Mutability**: Records, final fields, immutable collections enable thread-safe concurrent code
- **Pure Functions Over Side Effects**: Functional core/imperative shell separates domain logic from I/O
- **Reproducibility First**: SDKMAN! pins Java versions, Maven wrapper pins build tools, dependency management pins versions

## Related Documentation

- [Core Principles Index](../README.md) - All foundational principles
- [Development Practices Index](../../development/README.md) - Development practices implementing these principles
- [Repository Architecture](../../repository-governance-architecture.md) - Six-layer governance model

---

**Last Updated**: 2026-01-21
