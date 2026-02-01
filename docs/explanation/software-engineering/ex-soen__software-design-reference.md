---
title: Software Design Reference
description: Cross-reference to authoritative software design and coding standards documentation
category: explanation
subcategory: software
tags:
  - programming
  - software-design
  - coding-standards
  - cross-reference
principles_implemented:
  - explicit-over-implicit
  - documentation-first
created: 2026-01-25
updated: 2026-01-30
---

# Software Design Reference

**Cross-reference to authoritative software design and coding standards documentation.**

## Purpose

This document establishes the separation between:

- **Governance conventions** (governance/conventions/) - Cross-language, repository-wide rules
- **Software design documentation** (docs/explanation/software-engineering/) - Language-specific, framework-specific, architecture-specific guidance

## Authoritative Sources

### Architecture Patterns

**Location**: [docs/explanation/software-engineering/architecture/](./architecture/README.md)

- **[C4 Architecture Model](./architecture/c4-architecture-model/README.md)** - System visualization
- **[Domain-Driven Design](./architecture/domain-driven-design-ddd/README.md)** - Strategic and tactical patterns
- **[Finite State Machines](./architecture/finite-state-machine-fsm/README.md)** - State management

### Development Practices

**Location**: [docs/explanation/software-engineering/development/](./development/README.md)

- **[Test-Driven Development](./development/test-driven-development-tdd/README.md)** - TDD methodology
- **[Behavior-Driven Development](./development/behavior-driven-development-bdd/README.md)** - BDD with Gherkin

### Language-Specific Coding Standards

**Location**: [docs/explanation/software-engineering/programming-languages/](./programming-languages/README.md)

Each language directory contains:

- **Idioms** - Language-specific patterns and conventions
- **Best Practices** - Clean code standards
- **Anti-Patterns** - Common mistakes to avoid

Languages covered:

- **[Java](./programming-languages/java/README.md)** - Modern Java (17+)
- **[TypeScript](./programming-languages/typescript/README.md)** - Frontend and Node.js
- **[Go](./programming-languages/golang/README.md)** - CLI tools and infrastructure
- **[Python](./programming-languages/python/README.md)** - Data processing and AI/ML
- **[Elixir](./programming-languages/elixir/README.md)** - Real-time systems

### Framework-Specific Standards

**Location**: [docs/explanation/software-engineering/platform-web/](./platform-web/README.md)

Frameworks covered:

- **[Spring Boot (JVM)](./platform-web/jvm-spring-boot/README.md)** - REST APIs and microservices
- **[Phoenix (Elixir)](./platform-web/elixir-phoenix/README.md)** - Real-time applications
- **[React (TypeScript)](./platform-web/fe-react/README.md)** - Interactive UIs

## Separation of Concerns

### Governance Conventions

**What**: Repository-wide rules that apply across all languages and contexts

**Examples**:

- File naming patterns
- Linking standards
- Diagram accessibility
- Emoji usage
- Documentation organization (Diataxis)

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

1. **Follow language-specific standards** from docs/explanation/software-engineering/programming-languages/[language]/
2. **Apply framework patterns** from docs/explanation/software-engineering/platform-web/[framework]/
3. **Use architecture models** from docs/explanation/software-engineering/architecture/
4. **Apply development practices** from docs/explanation/software-engineering/development/
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

This document implements/respects the following core principles:

- **[Explicit Over Implicit](../../../governance/principles/software-engineering/explicit-over-implicit.md)**: By establishing clear separation between governance conventions and software design documentation, this document makes it explicit where to find authoritative guidance. No guessing whether standards live in governance/ or docs/explanation/software-engineering/ - the boundary is defined.

- **[Documentation First](../../../governance/principles/content/documentation-first.md)**: By creating a clear reference structure pointing to authoritative software design documentation, this document ensures documentation exists and is discoverable. AI agents and developers have explicit paths to language-specific standards, architecture patterns, and framework guidance.

## Related Documentation

- **[Programming Languages Overview](./programming-languages/README.md)** - Language comparison and selection
- **[Architecture Overview](./architecture/README.md)** - Architecture patterns
- **[Functional Programming Principles](../../../governance/development/pattern/functional-programming.md)** - Cross-language FP guidance

---

**Last Updated**: 2026-01-30
