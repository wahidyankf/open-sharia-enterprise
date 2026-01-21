---
title: Java
description: Modern Java idioms, best practices, and antipatterns (Java 17+)
category: explanation
subcategory: stack-lang
tags:
  - java
  - programming-languages
  - idioms
  - best-practices
  - antipatterns
  - java-17
  - java-21
  - java-25
created: 2026-01-20
updated: 2026-01-20
---

# Java

**Understanding-oriented documentation** for modern Java development in the open-sharia-enterprise platform.

## Overview

Java is a primary language for backend services, domain-driven design implementations, and enterprise features in the platform. This documentation covers modern Java (17+) with emphasis on:

- Records and pattern matching
- Virtual threads and structured concurrency
- Functional programming with streams
- Immutability and type safety
- Modern frameworks (Spring Boot 4, Jakarta EE 11)

## Java Version Strategy

### Current Baseline: Java 17+ (LTS)

**Platform Standard**: Java 17 is the minimum required version for all Java projects in the platform.

**Rationale**:

- Long-term support (LTS) release
- Modern language features (sealed classes, pattern matching foundations, records)
- Framework requirement (Spring Boot 3+, Jakarta EE 10+, Gradle 9, Maven 4)
- Industry standard as of 2025-2026

### Target Version: Java 21+ (LTS)

**Migration Path**: Projects are encouraged to migrate to Java 21 for enhanced features:

- Virtual threads (Project Loom)
- Scoped values
- Pattern matching for switch (finalized)
- Sequenced collections
- Record patterns

### Latest: Java 25 (LTS)

**Released**: September 2025 as the latest LTS version

**Finalized Features**:

- Stream gatherers (finalized in JDK 24)
- Compact source files
- Instance main methods
- Flexible constructor bodies
- Scoped values

**Preview Features**:

- Pattern matching for primitive types

## Documentation Structure

### [Java Idioms](./ex-so-stla-ja__idioms.md)

Language-specific patterns and conventions for writing idiomatic Java code.

**Covers**:

- Records and immutable data structures
- Pattern matching (switch, instanceof)
- Optional for null safety
- Stream API for collections
- Virtual threads and structured concurrency
- Try-with-resources for resource management
- Text blocks for multi-line strings
- Sealed classes for domain modeling

### [Java Best Practices](./ex-so-stla-ja__best-practices.md)

Proven approaches for writing clean, maintainable Java code based on 2025-2026 standards.

**Covers**:

- Code organization and structure
- Naming conventions and clarity
- Error handling and exceptions
- Testing strategies
- Dependency management
- Performance optimization
- Security practices
- Modern framework usage

### [Java Antipatterns](./ex-so-stla-ja__antipatterns.md)

Common mistakes, pitfalls, and problematic patterns to avoid in Java development.

**Covers**:

- Thread safety issues
- Resource management mistakes
- Performance pitfalls
- Security vulnerabilities
- Over-engineering and complexity
- Misuse of frameworks and libraries
- Legacy patterns that are now obsolete

## Java in the Platform

### Primary Use Cases

**Backend Services**:

- RESTful APIs for business operations
- GraphQL endpoints for complex queries
- gRPC services for internal communication
- Event-driven microservices

**Domain-Driven Design**:

- Aggregates for business domains
- Value Objects for Money, thresholds
- Domain Events for business process tracking
- Repositories and specifications

**Business Logic**:

- Compliant calculation engines
- Complex validation rules
- Financial transaction processing
- Compliance and audit trail

### Framework Stack

**Spring Boot 4** (Primary Framework):

- Spring Web for REST APIs
- Spring Data JPA for persistence
- Spring Security for authentication/authorization
- Spring AI for AI/ML integration

**Jakarta EE 11** (Enterprise Features):

- CDI for dependency injection
- JPA for object-relational mapping
- Bean Validation for constraints
- Jakarta RESTful Web Services

**Testing Frameworks**:

- JUnit 5 for unit testing
- Mockito for test doubles
- TestContainers for integration testing
- Cucumber JVM for BDD scenarios

### Architectural Patterns

**Hexagonal Architecture** (Ports and Adapters):

- Domain core in pure Java
- Application services orchestrating use cases
- Infrastructure adapters for persistence, messaging
- REST/GraphQL adapters for external interface

**Event Sourcing** (Selected Domains):

- Event store for audit trail
- Projections for read models
- CQRS for command/query separation

**Functional Core, Imperative Shell**:

- Pure functions for domain logic
- Side effects isolated at boundaries
- Immutable domain models
- Functional programming principles

## Learning Path

### 1. Start with Idioms

Read [Java Idioms](./ex-so-stla-ja__idioms.md) to understand modern Java patterns:

- Records for data classes
- Pattern matching for type checks
- Optional for null safety
- Streams for collection processing

### 2. Apply Best Practices

Read [Java Best Practices](./ex-so-stla-ja__best-practices.md) to write clean code:

- Keep methods and classes focused (10-20 lines per method)
- Name with intention and clarity
- Test comprehensively with JUnit 5
- Use immutability and final fields

### 3. Avoid Antipatterns

Read [Java Antipatterns](./ex-so-stla-ja__antipatterns.md) to prevent common mistakes:

- Thread safety issues (SimpleDateFormat, shared state)
- Resource leaks (unclosed streams, connections)
- Performance pitfalls (StringBuffer in local variables)
- Over-engineering (excessive abstraction)

### 4. Integrate with DDD

Read complementary documentation:

- [Domain-Driven Design](../../architecture/domain-driven-design-ddd/README.md)
- [TDD with Java](../../development/test-driven-development-tdd/ex-so-de-tedrdeve__11-tdd-and-functional-programming.md)
- [Functional Programming](../../../../../governance/development/pattern/functional-programming.md)

## Code Examples from Platform

### Domain Model Example

```java
// Using records for immutable value objects
public record Money(BigDecimal amount, Currency currency) {
    public Money {
        if (amount == null || amount.compareTo(BigDecimal.ZERO) < 0) {
            throw new IllegalArgumentException("Amount must be non-negative");
        }
        if (currency == null) {
            throw new IllegalArgumentException("Currency is required");
        }
    }

    public Money add(Money other) {
        if (!this.currency.equals(other.currency)) {
            throw new IllegalArgumentException("Cannot add different currencies");
        }
        return new Money(this.amount.add(other.amount), this.currency);
    }
}

// Aggregate root with business rules
public sealed interface TaxCalculation permits IncomeTax, SalesTax, PropertyTax {
    Money calculateTax();
    boolean meetsThreshold();
    LocalDate fiscalYearEnd();
}

// Pattern matching for different tax types
public Money processTax(TaxCalculation calculation) {
    return switch (calculation) {
        case IncomeTax income -> income.calculateTax();
        case SalesTax sales -> sales.calculateTax();
        case PropertyTax property -> property.calculateTax();
    };
}
```

### Service Layer Example

```java
@Service
@Transactional
public class TaxService {
    private final TaxRepository repository;
    private final DomainEventPublisher eventPublisher;

    public TaxService(TaxRepository repository, DomainEventPublisher eventPublisher) {
        this.repository = repository;
        this.eventPublisher = eventPublisher;
    }

    public TaxCalculationResult calculate(TaxRequest request) {
        // Functional core: pure domain logic
        var calculation = TaxCalculation.create(
            request.income(),
            request.threshold(),
            request.fiscalYearEnd()
        );

        // Validate business rules
        var validation = calculation.validate();
        if (validation.hasErrors()) {
            throw new ValidationException(validation.errors());
        }

        // Imperative shell: side effects
        var savedCalculation = repository.save(calculation);
        eventPublisher.publish(new TaxCalculatedEvent(savedCalculation.id()));

        return TaxCalculationResult.from(savedCalculation);
    }
}
```

## Integration with Other Documentation

### Development Practices

- **[Test-Driven Development](../../development/test-driven-development-tdd/README.md)** - TDD with JUnit 5
- **[Behavior-Driven Development](../../development/behavior-driven-development-bdd/README.md)** - BDD with Cucumber JVM
- **[Functional Programming](../../../../../governance/development/pattern/functional-programming.md)** - FP principles in Java

### Architecture

- **[Domain-Driven Design](../../architecture/domain-driven-design-ddd/README.md)** - DDD tactical patterns in Java
- **[C4 Architecture Model](../../architecture/c4-architecture-model/README.md)** - System documentation

### Code Quality

- **[Code Quality Standards](../../../../../governance/development/quality/code.md)** - Quality requirements
- **[Commit Messages](../../../../../governance/development/workflow/commit-messages.md)** - Conventional Commits
- **[Implementation Workflow](../../../../../governance/development/workflow/implementation.md)** - Development process

## Tools and Ecosystem

### Build Tools

**Gradle 9** (Primary):

- Kotlin DSL for build scripts
- Dependency version catalogs
- Nx Gradle plugin for monorepo integration

**Maven 4** (Alternative):

- POM-based configuration
- Central dependency management

### Code Quality

**Checkstyle**: Validates coding standards
**SpotBugs**: Static analysis for bug detection
**JaCoCo**: Code coverage measurement
**SonarQube**: Continuous code quality inspection

### Development Environment

**IntelliJ IDEA**: Primary IDE
**Eclipse**: Alternative IDE
**VS Code**: Lightweight option with Java extensions

## Resources and References

### Official Documentation

- [Java Language Specification](https://docs.oracle.com/javase/specs/)
- [Java API Documentation](https://docs.oracle.com/en/java/javase/21/docs/api/)
- [OpenJDK Documentation](https://openjdk.org/)

### Style Guides

- [Google Java Style Guide](https://google.github.io/styleguide/javaguide.html)
- [Oracle Code Conventions](https://www.oracle.com/java/technologies/javase/codeconventions-programmingpractices.html)

### Modern Java Resources

- [InfoQ Java Trends Report 2025](https://www.infoq.com/articles/java-trends-report-2025/)
- [JetBrains Java Best Practices](https://blog.jetbrains.com/idea/2024/02/java-best-practices/)
- [Java Annotated Monthly](https://blog.jetbrains.com/idea/tag/java-annotated/)

## Related Documentation

- **[Tech Stack Languages Index](../README.md)** - Parent language documentation
- **[Software Design Index](../../README.md)** - Software documentation root
- **[Explanation Documentation Index](../../../README.md)** - All conceptual docs
- **[Monorepo Structure](../../../../reference/re__monorepo-structure.md)** - Project organization

---

**Last Updated**: 2026-01-20
**Java Version**: 17+ (baseline), 21+ (recommended), 25 (emerging)
