---
title: Programming Languages
description: Language-specific idioms, best practices, and antipatterns
category: explanation
subcategory: stack-lang
tags:
  - programming-languages
  - idioms
  - best-practices
  - antipatterns
  - index
created: 2026-01-20
updated: 2026-01-20
---

# Programming Languages

**Understanding-oriented documentation** on language-specific idioms, best practices, and antipatterns for programming languages used in the open-sharia-enterprise platform.

## Overview

**The Polyglot Confusion**: Every language has its own idioms, conventions, and gotchas. Developers switch between Java streams, TypeScript promises, Go goroutines, and Python list comprehensions. What's idiomatic in one language is an antipattern in another. Copy-pasting patterns across languages leads to awkward, non-idiomatic code.

**Curated Language Guidance**: We provide language-specific documentation that captures idioms, best practices, and antipatterns for each language in the platform. Learn how to write code that feels native to the language, not awkwardly translated from another one.

This directory contains comprehensive documentation on programming languages used throughout the platform. Each language has three core documents:

1. **Idioms** - Language-specific patterns, conventions, and idiomatic code styles
2. **Best Practices** - Proven approaches for writing clean, maintainable code
3. **Antipatterns** - Common mistakes and pitfalls to avoid

## Quick Decision: Which Language for My Task?

| Task                                     | Recommended Language   | Start With                              |
| ---------------------------------------- | ---------------------- | --------------------------------------- |
| Complex domain logic with DDD            | Java                   | [Java Idioms](./java/README.md)         |
| REST API with business rules             | Java                   | [Java Best Practices](./java/README.md) |
| Frontend web application                 | TypeScript             | TypeScript docs (planned)               |
| CLI tool for repository automation       | Go                     | See rhino-cli, ayokoding-cli            |
| Data processing and analytics            | Python                 | Python docs (planned)                   |
| Microservice with high concurrency       | Java (Virtual Threads) | [Java Concurrency](./java/README.md)    |
| Infrastructure tooling                   | Go                     | See existing CLI tools                  |
| Real-time updates and WebSocket handling | TypeScript             | TypeScript docs (planned)               |

**Platform Guidance**:

- **Java**: Primary language for domain models, aggregates, and business logic
- **TypeScript**: Future frontend applications and Node.js services
- **Go**: Active for CLI tools (rhino-cli, ayokoding-cli)
- **Python**: Planned for data processing and AI/ML integration

## Purpose

Understanding language-specific idioms and patterns helps developers:

- Write code that follows established conventions
- Leverage language features effectively
- Avoid common pitfalls and mistakes
- Maintain consistency across the codebase
- Onboard new team members efficiently

## Documentation Structure

### Language Coverage

Each language directory contains:

```
[language-name]/
├── README.md                           # Language overview and version info
├── ex-so-stla-[la]__idioms.md    # Language-specific idioms
├── ex-so-stla-[la]__best-practices.md  # Best practices
└── ex-so-stla-[la]__antipatterns.md    # Common antipatterns
```

### Document Categories

**Idioms** focus on:

- Language-specific patterns and conventions
- Effective use of language features
- Standard library usage patterns
- Ecosystem conventions
- Community-established norms

**Best Practices** cover:

- Code organization and structure
- Naming conventions
- Error handling approaches
- Testing strategies
- Performance considerations
- Security practices

**Antipatterns** identify:

- Common mistakes and misuses
- Performance pitfalls
- Security vulnerabilities
- Maintainability issues
- Anti-idiomatic code patterns

## Languages Documented

### ☕ [Java](./java/README.md)

**Modern Java development with records, pattern matching, and virtual threads**

Java is a primary language for backend services, particularly for domain-driven design and enterprise features. The platform uses modern Java (17+) with emphasis on functional programming, immutability, and structured concurrency.

**Key Documentation:**

- [Java Idioms](./java/ex-so-stla-ja__idioms.md) - Records, pattern matching, Optional, Stream API
- [Java Best Practices](./java/ex-so-stla-ja__best-practices.md) - Modern Java standards (2025-2026)
- [Java Antipatterns](./java/ex-so-stla-ja__anti-patterns.md) - Common mistakes to avoid

**Use Java when you need:**

- Strong type safety and compile-time guarantees
- Enterprise integration (Jakarta EE, Spring ecosystem)
- High-performance concurrent processing
- Complex domain models (DDD tactical patterns)
- Mature tooling and ecosystem

## How Languages Fit into the Platform

### Language Selection Criteria

Languages in this documentation are chosen based on:

**Technical Fit**:

- Type safety and correctness guarantees
- Performance characteristics
- Ecosystem maturity
- Tooling support

**Development Practices**:

- Alignment with functional programming principles
- Support for immutability and pure functions
- Testing and maintainability
- Community best practices

**Platform Integration**:

- Nx monorepo compatibility
- CI/CD pipeline integration
- Deployment and containerization
- Observability and monitoring

### Current Language Usage

| Language       | Primary Use Cases                               | Status                               |
| -------------- | ----------------------------------------------- | ------------------------------------ |
| **Java** ☕    | Backend services, domain models, business logic | ✅ Active - In production            |
| **TypeScript** | Frontend applications, Node.js services         | 📋 Planned - Documentation ready     |
| **Go**         | CLI tools, infrastructure services              | ✅ Active - rhino-cli, ayokoding-cli |
| **Python**     | Data processing, AI/ML integration              | 📋 Planned - Future integration      |

**Legend**: ✅ Active (in use) | 📋 Planned (documentation ready, not yet implemented)

## Learning Paths

### For Backend Developers

1. **Start with Java fundamentals** - Read [Java Idioms](./java/ex-so-stla-ja__idioms.md)
2. **Apply modern practices** - Read [Java Best Practices](./java/ex-so-stla-ja__best-practices.md)
3. **Avoid common pitfalls** - Read [Java Antipatterns](./java/ex-so-stla-ja__anti-patterns.md)
4. **Integrate with DDD** - Read [DDD and Java](../architecture/domain-driven-design-ddd/README.md)

### For Full-Stack Developers

1. Learn both backend (Java) and frontend (TypeScript) idioms
2. Understand language-specific testing approaches
3. Apply consistent patterns across languages
4. Practice polyglot development

### For New Team Members

1. Read idioms document for your primary language
2. Review best practices for code standards
3. Study antipatterns to avoid common mistakes
4. Cross-reference with repository conventions

## Complementary Documentation

This language documentation complements other areas:

- **[Development Practices](../development/README.md)** - TDD, BDD, testing strategies
- **[Architecture](../architecture/README.md)** - C4 model, DDD patterns
- **[Functional Programming](../../../../governance/development/pattern/functional-programming.md)** - Cross-language FP principles
- **[Code Quality Standards](../../../../governance/development/quality/code.md)** - Quality requirements
- **[Monorepo Structure](../../../reference/re__monorepo-structure.md)** - Project organization

## Principles Reflected in Language Documentation

All language documentation follows the repository's core principles:

**Simplicity Over Complexity**:

- Prefer simple, clear code over clever solutions
- Use language features appropriately, not excessively
- Favor readability over premature optimization

**Explicit Over Implicit**:

- Make dependencies and behavior explicit
- Avoid magic and hidden complexity
- Use clear, descriptive naming

**Immutability First**:

- Prefer immutable data structures
- Use functional programming patterns
- Minimize mutable state

**Security by Design**:

- Follow language-specific security best practices
- Validate inputs at system boundaries
- Apply principle of least privilege

## Contributing Language Documentation

### Adding a New Language

To document a new language:

1. Create directory: `docs/explanation/software/stack-lang/[language-name]/`
2. Create README.md with language overview
3. Create three core documents:
   - `ex-so-stlag-[abbr]__idioms.md`
   - `ex-so-stlag-[abbr]__best-practices.md`
   - `ex-so-stlag-[abbr]__antipatterns.md`
4. Update this README.md with language section
5. Cross-reference with relevant documentation

### Updating Existing Documentation

- Keep content current with language evolution
- Cite authoritative sources (official docs, style guides)
- Include code examples from the platform when possible
- Mark deprecated patterns and suggest modern alternatives
- Update "Last Updated" date in frontmatter

## Related Documentation

- **[Software Design Index](../README.md)** - Parent software design documentation
- **[Architecture](../architecture/README.md)** - C4 and DDD documentation
- **[Development Practices](../development/README.md)** - TDD and BDD documentation
- **[Explanation Documentation Index](../../README.md)** - All conceptual documentation
- **[Monorepo Structure](../../../reference/re__monorepo-structure.md)** - Project organization
- **[Code Quality Standards](../../../../governance/development/quality/code.md)** - Quality requirements

---

**Last Updated**: 2026-01-20
