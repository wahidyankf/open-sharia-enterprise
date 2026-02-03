---
title: "Overview"
date: 2025-12-12T00:00:00+07:00
draft: false
description: Practical guidance for applying Java concepts in real-world development
weight: 10000000
tags: ["java", "in-practice", "overview"]
---

## What is In-Practice Content?

In-practice content provides **conceptual guidance** for applying Java knowledge in real-world development scenarios. This section bridges the gap between learning syntax and building robust, maintainable applications.

## Pedagogical Approach: Standard Library First

In-practice content follows a **progression from fundamentals to production frameworks**:

1. **Standard Library** - Learn built-in Java capabilities first (assert keyword, manual args[], System streams, JDBC)
2. **Understand Limitations** - See why standard approaches become unmaintainable for complex applications
3. **Production Frameworks** - Adopt industry-standard libraries with clear understanding of problems they solve

**Why this approach?**

- **Foundation first**: Understanding fundamentals makes framework features comprehensible
- **Informed decisions**: Knowing trade-offs enables appropriate tool selection
- **Problem awareness**: Seeing manual implementations reveals value of frameworks
- **Framework independence**: Standard library knowledge remains relevant across tools

**Topics teaching this progression**:

- JSON processing (manual → javax.json → Jackson)
- Testing (assert keyword → manual runners → JUnit 5)
- CLI apps (raw args[] → System streams → picocli)
- BDD (manual Given-When-Then → Cucumber)
- SQL (JDBC → HikariCP → JPA/Hibernate)

## Focus Areas

### Anti-Patterns

Common mistakes and pitfalls to avoid in Java development. Learn to recognize problematic patterns before they become ingrained in your codebase.

**Topics covered:**

- Concurrency anti-patterns (thread leakage, race conditions, deadlocks)
- Resource management mistakes (unclosed resources, connection leaks)
- Design anti-patterns (god classes, primitive obsession, shotgun surgery)
- Performance anti-patterns (N+1 queries, premature optimization)
- Security anti-patterns (input validation, credential management)

### Integration Patterns

Working with external systems and data formats in production applications. Content progresses from standard library fundamentals to production frameworks.

**Topics covered:**

- JSON processing: Manual StringBuilder → javax.json → Jackson (production standard)
- SQL database integration: JDBC fundamentals → HikariCP pooling → JPA/Hibernate ORM
- REST API client patterns with standard HttpClient
- Transaction management and data integrity with JDBC and JPA

### Application Development

Building complete applications with Java. Content teaches fundamentals before introducing frameworks.

**Topics covered:**

- Command-line applications: Manual args[] parsing → System streams → picocli framework
- Testing: assert keyword → manual test runners → JUnit 5 + Mockito + AssertJ
- BDD: Manual Given-When-Then structure → Cucumber + Gherkin
- Configuration management and exit codes
- Native compilation with GraalVM

### Future Topics

Additional in-practice content will cover:

- Best practices and idioms
- Design patterns in Java
- Performance optimization approaches

## How This Differs from By-Example

**In-practice content:**

- Focuses on **conceptual understanding** and **why** patterns emerge
- Uses illustrative code examples to show problems and solutions
- Emphasizes problem/solution format and comparison tables
- More narrative and explanatory

**By-example tutorials:**

- Focuses on **code-first learning** with heavy annotations
- Uses 1-2.25 comment density per code line
- Emphasizes incremental skill building through examples
- More code-centric and practical

## Getting the Most from In-Practice Content

1. **Read actively**: Consider how anti-patterns might appear in your code
2. **Compare examples**: Study both problematic and corrected versions
3. **Apply to your work**: Identify potential issues in existing codebases
4. **Cross-reference**: Link to by-example tutorials for syntax details
5. **Practice recognition**: Learn to spot patterns during code review

## Related Resources

- [By Example](/en/learn/software-engineering/programming-languages/java/by-example) - Code-first tutorials
- [Overview](/en/learn/software-engineering/programming-languages/java/overview) - Complete Java learning path
- [Quick Start](/en/learn/software-engineering/programming-languages/java/quick-start) - Get started quickly
