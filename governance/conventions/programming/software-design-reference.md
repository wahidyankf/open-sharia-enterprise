---
title: Software Design Reference
description: Cross-reference to authoritative software design and coding standards documentation
category: convention
subcategory: programming
layer: 2
tags:
  - programming
  - software-design
  - coding-standards
  - cross-reference
principles_implemented:
  - explicit-over-implicit
  - documentation-first
created: 2026-01-25
updated: 2026-01-25
---

# Software Design Reference

**Convention for locating authoritative software design and coding standards documentation.**

## Purpose

This convention establishes the separation between:

- **Governance conventions** (this directory) - Cross-language, repository-wide rules
- **Software design documentation** (docs/explanation/software/) - Language-specific, framework-specific, architecture-specific guidance

## Authoritative Sources

### Architecture Patterns

**Location**: [docs/explanation/software/architecture/](../../../docs/explanation/software/architecture/README.md)

- **[C4 Architecture Model](../../../docs/explanation/software/architecture/c4-architecture-model/README.md)** - System visualization
- **[Domain-Driven Design](../../../docs/explanation/software/architecture/domain-driven-design-ddd/README.md)** - Strategic and tactical patterns
- **[Finite State Machines](../../../docs/explanation/software/architecture/finite-state-machine-fsm/README.md)** - State management

### Development Practices

**Location**: [docs/explanation/software/development/](../../../docs/explanation/software/development/README.md)

- **[Test-Driven Development](../../../docs/explanation/software/development/test-driven-development-tdd/README.md)** - TDD methodology
- **[Behavior-Driven Development](../../../docs/explanation/software/development/behavior-driven-development-bdd/README.md)** - BDD with Gherkin

### Language-Specific Coding Standards

**Location**: [docs/explanation/software/stack-lang/](../../../docs/explanation/software/stack-lang/README.md)

Each language directory contains:

- **Idioms** - Language-specific patterns and conventions
- **Best Practices** - Clean code standards
- **Anti-Patterns** - Common mistakes to avoid

Languages covered:

- **[Java](../../../docs/explanation/software/stack-lang/java/README.md)** - Modern Java (17+)
- **[TypeScript](../../../docs/explanation/software/stack-lang/typescript/README.md)** - Frontend and Node.js
- **[Go](../../../docs/explanation/software/stack-lang/golang/README.md)** - CLI tools and infrastructure
- **[Python](../../../docs/explanation/software/stack-lang/python/README.md)** - Data processing and AI/ML
- **[Elixir](../../../docs/explanation/software/stack-lang/elixir/README.md)** - Real-time systems

### Framework-Specific Standards

**Location**: [docs/explanation/software/stack-libs/](../../../docs/explanation/software/stack-libs/README.md)

Frameworks covered:

- **[Spring Boot (JVM)](../../../docs/explanation/software/stack-libs/jvm-spring-boot/README.md)** - REST APIs and microservices
- **[Phoenix (Elixir)](../../../docs/explanation/software/stack-libs/elixir-phoenix/README.md)** - Real-time applications
- **[React (TypeScript)](../../../docs/explanation/software/stack-libs/ts-react/README.md)** - Interactive UIs

## Separation of Concerns

### Governance Conventions (This Directory)

**What**: Repository-wide rules that apply across all languages and contexts

**Examples**:

- File naming patterns
- Linking standards
- Diagram accessibility
- Emoji usage
- Documentation organization (Diátaxis)

**Authority**: Layer 2 of six-layer governance hierarchy

### Software Design Documentation

**What**: Language-specific, framework-specific, architecture-specific technical guidance

**Examples**:

- Java record usage vs traditional classes
- TypeScript type narrowing patterns
- Spring Boot auto-configuration best practices
- React hook usage patterns
- DDD aggregate boundaries

**Authority**: Authoritative technical reference, cited by agents and developers

## For AI Agents

When writing code or making architectural decisions:

1. **Follow language-specific standards** from docs/explanation/software/stack-lang/[language]/
2. **Apply framework patterns** from docs/explanation/software/stack-libs/[framework]/
3. **Use architecture models** from docs/explanation/software/architecture/
4. **Apply development practices** from docs/explanation/software/development/
5. **Comply with repository conventions** from governance/conventions/

Skills available for quick reference:

- `programming-java` - Java coding standards
- `programming-typescript` - TypeScript coding standards
- `programming-golang` - Go coding standards
- `programming-python` - Python coding standards
- `programming-elixir` - Elixir coding standards

## Validation

The `repo-governance-checker` agent validates:

- Cross-references between governance and software docs
- Principle alignment in software documentation
- File naming convention adherence
- Document structure consistency

## Related Documentation

- **[Programming Languages Overview](../../../docs/explanation/software/stack-lang/README.md)** - Language comparison and selection
- **[Architecture Overview](../../../docs/explanation/software/architecture/README.md)** - Architecture patterns
- **[Functional Programming Principles](../../development/pattern/functional-programming.md)** - Cross-language FP guidance

---

**Last Updated**: 2026-01-25
