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

**Location**: [docs/explanation/software/prog-lang/](../../../docs/explanation/software/prog-lang/README.md)

Each language directory contains:

- **Idioms** - Language-specific patterns and conventions
- **Best Practices** - Clean code standards
- **Anti-Patterns** - Common mistakes to avoid

Languages covered:

- **[Java](../../../docs/explanation/software/prog-lang/java/README.md)** - Modern Java (17+)
- **[TypeScript](../../../docs/explanation/software/prog-lang/typescript/README.md)** - Frontend and Node.js
- **[Go](../../../docs/explanation/software/prog-lang/golang/README.md)** - CLI tools and infrastructure
- **[Python](../../../docs/explanation/software/prog-lang/python/README.md)** - Data processing and AI/ML
- **[Elixir](../../../docs/explanation/software/prog-lang/elixir/README.md)** - Real-time systems

### Framework-Specific Standards

**Location**: [docs/explanation/software/platform-web/](../../../docs/explanation/software/platform-web/README.md)

Frameworks covered:

- **[Spring Boot (JVM)](../../../docs/explanation/software/platform-web/jvm-spring-boot/README.md)** - REST APIs and microservices
- **[Phoenix (Elixir)](../../../docs/explanation/software/platform-web/elixir-phoenix/README.md)** - Real-time applications
- **[React (TypeScript)](../../../docs/explanation/software/platform-web/ts-react/README.md)** - Interactive UIs

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

1. **Follow language-specific standards** from docs/explanation/software/prog-lang/[language]/
2. **Apply framework patterns** from docs/explanation/software/platform-web/[framework]/
3. **Use architecture models** from docs/explanation/software/architecture/
4. **Apply development practices** from docs/explanation/software/development/
5. **Comply with repository conventions** from governance/conventions/

Skills available for quick reference:

- `swe-programming-java` - Java coding standards
- `swe-programming-typescript` - TypeScript coding standards
- `swe-programming-golang` - Go coding standards
- `swe-programming-python` - Python coding standards
- `swe-programming-elixir` - Elixir coding standards

## Validation

The `repo-governance-checker` agent validates:

- Cross-references between governance and software docs
- Principle alignment in software documentation
- File naming convention adherence
- Document structure consistency

## Principles Implemented/Respected

This convention implements/respects the following core principles:

- **[Explicit Over Implicit](../../principles/software-engineering/explicit-over-implicit.md)**: By establishing clear separation between governance conventions and software design documentation, this convention makes it explicit where to find authoritative guidance. No guessing whether standards live in governance/ or docs/explanation/software/ - the boundary is defined.

- **[Documentation First](../../principles/content/documentation-first.md)**: By creating a clear reference structure pointing to authoritative software design documentation, this convention ensures documentation exists and is discoverable. AI agents and developers have explicit paths to language-specific standards, architecture patterns, and framework guidance.

## Related Documentation

- **[Programming Languages Overview](../../../docs/explanation/software/prog-lang/README.md)** - Language comparison and selection
- **[Architecture Overview](../../../docs/explanation/software/architecture/README.md)** - Architecture patterns
- **[Functional Programming Principles](../../development/pattern/functional-programming.md)** - Cross-language FP guidance

---

**Last Updated**: 2026-01-25
