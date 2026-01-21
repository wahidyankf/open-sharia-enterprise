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

## Software Engineering Principles

Java development in this platform follows the five software engineering principles from [governance/principles/software-engineering/](../../../../../governance/principles/software-engineering/README.md):

1. **[Automation Over Manual](../../../../../governance/principles/software-engineering/automation-over-manual.md)** - Java automates through records, try-with-resources, static analysis, and build tools
2. **[Explicit Over Implicit](../../../../../governance/principles/software-engineering/explicit-over-implicit.md)** - Java enforces through sealed types, explicit configuration, module system
3. **[Immutability Over Mutability](../../../../../governance/principles/software-engineering/immutability.md)** - Java provides records, final fields, immutable collections for thread-safe code
4. **[Pure Functions Over Side Effects](../../../../../governance/principles/software-engineering/pure-functions.md)** - Java supports through functional interfaces, streams, and functional core architecture
5. **[Reproducibility First](../../../../../governance/principles/software-engineering/reproducibility.md)** - Java enables through version pinning, Maven wrapper, dependency management

**See Also**: [Functional Programming](./ex-so-stla-ja__functional-programming.md) for pure functions and immutability patterns, [Best Practices](./ex-so-stla-ja__best-practices.md) for explicit coding standards.

## Java Version Strategy

### Current Baseline: Java 17+ (LTS)

**Platform Standard**: Java 17 is the minimum required version for all Java projects in the platform.

**Rationale**:

- Long-term support (LTS) release
- Modern language features (sealed classes, pattern matching foundations, records)
- Framework requirement (Spring Boot 3+, Jakarta EE 10+, Maven 3.9+)
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

### [Java Anti-Patterns](./ex-so-stla-ja__anti-patterns.md)

Common mistakes, pitfalls, and problematic patterns to avoid in Java development.

**Covers**:

- Thread safety issues
- Resource management mistakes
- Performance pitfalls
- Security vulnerabilities
- Over-engineering and complexity
- Misuse of frameworks and libraries
- Legacy patterns that are now obsolete

### Release Documentation

Comprehensive guides to major Java LTS releases, documenting important features and changes:

#### [Java 17 LTS Release](./ex-so-stla-ja__release-17.md)

Released September 2021. Minimum required version for the platform.

**Major Features**:

- Sealed classes and interfaces (finalized)
- Pattern matching for switch (preview)
- Strict floating-point semantics
- Enhanced pseudo-random number generators
- macOS/AArch64 support (Apple Silicon)

#### [Java 21 LTS Release](./ex-so-stla-ja__release-21.md)

Released September 2023. Recommended target version.

**Major Features**:

- Virtual threads (finalized) - revolutionary concurrency model
- Record patterns (finalized)
- Pattern matching for switch (finalized)
- Sequenced collections
- String templates (preview)
- Scoped values (preview)
- Structured concurrency (preview)

#### [Java 25 LTS Release](./ex-so-stla-ja__release-25.md)

Released September 2025. Latest LTS version with focus on performance.

**Major Features**:

- Compact source files and instance main methods (finalized)
- Flexible constructor bodies (finalized)
- Scoped values (finalized)
- Module import declarations (finalized)
- Ahead-of-time method profiling (finalized)
- Compact object headers (20% memory reduction)
- Primitive types in patterns (preview)
- Generational Shenandoah GC

### Specialized Topics

Deep-dive documentation on critical Java development areas:

#### [Java Security](./ex-so-stla-ja__security.md)

Comprehensive guide to building secure Java applications aligned with OWASP guidelines.

**Covers**:

- Input validation and sanitization (allowlist approach)
- Injection prevention (SQL, XSS, command injection)
- Authentication and authorization (RBAC, MFA)
- Cryptography (AES-GCM, hybrid encryption, key management)
- Secure coding practices (error handling, resource management)
- Dependency management (vulnerability scanning)
- Audit logging (financial compliance)
- Modern Java security features (sealed classes, pattern matching)

#### [Java Performance](./ex-so-stla-ja__performance.md)

Comprehensive guide to JVM tuning, optimization, and performance engineering.

**Covers**:

- JVM architecture and memory management
- Garbage collection tuning (G1GC, ZGC, Shenandoah)
- Memory optimization (object allocation, compact headers)
- CPU optimization (JIT compilation, AOT profiling)
- I/O optimization (database queries, connection pooling)
- Caching strategies (Caffeine, Redis)
- Profiling tools (JFR, async-profiler, JMH)
- Modern Java performance features (virtual threads impact, generational GC)

#### [Java Concurrency and Parallelism](./ex-so-stla-ja__concurrency-and-parallelism.md)

Comprehensive guide to concurrent and parallel programming in modern Java.

**Covers**:

- Virtual threads (lightweight concurrency, Java 21+)
- Structured concurrency (hierarchical task management, Java 21+ preview)
- Thread safety (immutability, volatile, synchronization)
- Synchronization mechanisms (ReentrantLock, ReadWriteLock, semaphores)
- Concurrent collections (ConcurrentHashMap, BlockingQueue)
- Atomic operations (AtomicInteger, LongAdder)
- Parallel streams and Fork/Join framework
- Common concurrency problems (deadlocks, race conditions)
- Modern patterns (Scoped Values, CompletableFuture)

#### [Java Type Safety](./ex-so-stla-ja__type-safety.md)

Comprehensive guide to achieving type safety and minimizing runtime errors in Java.

**Covers**:

- Null safety with JSpecify and NullAway (compile-time null checking)
- Static analysis with Checker Framework (tainting, units, regex checkers)
- Error Prone for bug prevention (Google's static analyzer)
- Sealed classes for exhaustive handling
- Optional for null-safe APIs
- Pattern matching for type-safe decomposition
- Records and immutability for compile-time guarantees
- Phantom types and type-safe builders

#### [Java Functional Programming](./ex-so-stla-ja__functional-programming.md)

Comprehensive guide to functional programming patterns and practices in Java.

**Covers**:

- Pure functions and referential transparency
- Immutability with records and persistent data structures
- Vavr library (immutable collections, monads, pattern matching)
- Streams API for declarative collection processing
- Function composition and higher-order functions
- Monads (Optional, CompletableFuture, Vavr Try/Either)
- Functional core, imperative shell architecture
- Property-based testing for functional code

#### [Java Domain-Driven Design](./ex-so-stla-ja__domain-driven-design.md)

Comprehensive guide to implementing DDD tactical patterns with Java frameworks.

**Covers**:

- Value objects with records (Money, Email, immutable domain primitives)
- Entities with identity and lifecycle
- Aggregates and consistency boundaries
- Domain events for significant occurrences
- Repositories for persistence abstraction
- Domain services for cross-aggregate logic
- Axon Framework for CQRS and event sourcing
- Spring Boot integration for DDD applications

#### [Java Test-Driven Development](./ex-so-stla-ja__test-driven-development.md)

Comprehensive guide to TDD practices with JUnit 5, Mockito, and AssertJ.

**Covers**:

- Red-Green-Refactor cycle and TDD workflow
- JUnit 5 fundamentals (tests, assertions, parameterized tests)
- AssertJ for fluent assertions
- Mockito for test doubles and mocking
- Testing strategies (unit, integration, E2E)
- TestContainers for database testing
- Testing domain models (value objects, entities, aggregates)
- TDD best practices and patterns

#### [Java Behavior-Driven Development](./ex-so-stla-ja__behaviour-driven-development.md)

Comprehensive guide to BDD with Cucumber, Gherkin, and collaborative testing.

**Covers**:

- BDD core concepts (discovery, formulation, automation)
- Gherkin syntax (Given-When-Then, scenarios, scenario outlines)
- Cucumber JVM for Java BDD
- Step definitions connecting Gherkin to code
- Data tables and parameter types for domain objects
- BDD patterns and anti-patterns
- Three Amigos and Example Mapping collaboration
- BDD vs TDD complementary practices

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

### 3. Avoid Anti-Patterns

Read [Java Anti-Patterns](./ex-so-stla-ja__anti-patterns.md) to prevent common mistakes:

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

**Maven 3.9+** (Primary):

- POM XML for build configuration
- Dependency management and version control
- Plugin-based extensibility

**Maven 4** (Future):

- Enhanced performance and build caching
- Improved dependency resolution

### Code Quality

**Checkstyle**: Validates coding standards
**SpotBugs**: Static analysis for bug detection
**JaCoCo**: Code coverage measurement
**SonarQube**: Continuous code quality inspection

### Development Environment

**IntelliJ IDEA**: Primary IDE
**Eclipse**: Alternative IDE
**VS Code**: Lightweight option with Java extensions

### Reproducible Java Development

**Version Management**:

- Recommended: [SDKMAN!](https://sdkman.io/) for automatic Java version switching
- Alternative: [jEnv](https://www.jenv.be/), [MISE](https://mise.jdx.dev/), [Asdf](https://asdf-vm.com/)
- Create `.sdkmanrc` or `.tool-versions` to pin Java version

**Example .sdkmanrc**:

```bash
java=21.0.1-tem
maven=3.9.12
```

**Build Reproducibility**:

- **Maven Wrapper** (`mvnw`) pins Maven version - committed to git
- **Dependency Management** (`<dependencyManagement>` section) pins dependency versions
- **Maven Enforcer Plugin** enforces version consistency and prevents SNAPSHOT dependencies

**Setup Script**:

```bash
#!/bin/bash
# setup-java.sh

# Install SDKMAN! if not present
if ! command -v sdk &> /dev/null; then
    curl -s "https://get.sdkman.io" | bash
fi

# Use versions from .sdkmanrc
sdk env install

# Install dependencies and build
./mvnw clean install
```

**Docker Development Container** (Optional):

```dockerfile
FROM eclipse-temurin:21-jdk
WORKDIR /app
COPY . .
RUN ./mvnw clean install
```

**See**: [Reproducibility First principle](../../../../../governance/principles/software-engineering/reproducibility.md)

### Build Automation

**Maven Plugins** (Automation Over Manual):

- [Spotless Maven Plugin](https://github.com/diffplug/spotless/tree/main/plugin-maven) - Code formatting automation
- [Error Prone](https://errorprone.info/) - Compile-time bug detection (via maven-compiler-plugin)
- [NullAway](https://github.com/uber/NullAway) - Null safety automation (via annotation processor)
- [JaCoCo Maven Plugin](https://www.jacoco.org/jacoco/trunk/doc/maven.html) - Code coverage reporting
- [Maven Checkstyle Plugin](https://maven.apache.org/plugins/maven-checkstyle-plugin/) - Style checking automation

**Code Generation**:

- Records (Java 17+) - Auto-generate getters, equals, hashCode, toString
- Lombok - Annotation-based code generation (consider records first)
- JPA Metamodel Generator - Type-safe JPA queries
- Annotation Processing - Custom code generation

**Testing Automation**:

- JUnit 5 - Automated unit testing
- TestContainers - Automated integration testing with Docker
- Mockito - Automated mock creation
- Property-based testing (jqwik) - Automated test case generation

**See**: [Automation Over Manual principle](../../../../../governance/principles/software-engineering/automation-over-manual.md)

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

### Core Java Documentation

- **[Java Idioms](./ex-so-stla-ja__idioms.md)** - Modern Java patterns and conventions
- **[Java Best Practices](./ex-so-stla-ja__best-practices.md)** - Clean code guidelines
- **[Java Anti-Patterns](./ex-so-stla-ja__anti-patterns.md)** - Common mistakes to avoid

### Java Release Documentation

- **[Java 17 LTS Release](./ex-so-stla-ja__release-17.md)** - Baseline version features
- **[Java 21 LTS Release](./ex-so-stla-ja__release-21.md)** - Recommended version features
- **[Java 25 LTS Release](./ex-so-stla-ja__release-25.md)** - Latest LTS features

### Specialized Topics

- **[Java Security](./ex-so-stla-ja__security.md)** - Secure application development
- **[Java Performance](./ex-so-stla-ja__performance.md)** - JVM tuning and optimization
- **[Java Concurrency and Parallelism](./ex-so-stla-ja__concurrency-and-parallelism.md)** - Concurrent programming
- **[Java Type Safety](./ex-so-stla-ja__type-safety.md)** - Null safety, static analysis, compile-time guarantees
- **[Java Functional Programming](./ex-so-stla-ja__functional-programming.md)** - Pure functions, immutability, Vavr, monads
- **[Java Domain-Driven Design](./ex-so-stla-ja__domain-driven-design.md)** - DDD tactical patterns, Axon, Spring Boot
- **[Java Test-Driven Development](./ex-so-stla-ja__test-driven-development.md)** - TDD with JUnit 5, Mockito, AssertJ
- **[Java Behavior-Driven Development](./ex-so-stla-ja__behaviour-driven-development.md)** - BDD with Cucumber, Gherkin

### Platform Documentation

- **[Tech Stack Languages Index](../README.md)** - Parent language documentation
- **[Software Design Index](../../README.md)** - Software documentation root
- **[Explanation Documentation Index](../../../README.md)** - All conceptual docs
- **[Monorepo Structure](../../../../reference/re__monorepo-structure.md)** - Project organization

---

**Last Updated**: 2026-01-21
**Java Version**: 17+ (baseline), 21+ (recommended), 25 (latest LTS)
